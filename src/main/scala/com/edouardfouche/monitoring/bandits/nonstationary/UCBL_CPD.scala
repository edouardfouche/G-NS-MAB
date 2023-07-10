/*
 * Copyright (C) 2021 Edouard Fouché
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package com.edouardfouche.monitoring.bandits.nonstationary

import com.edouardfouche.monitoring.bandits.BanditUCB
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * UCBL-CPD (UCB Laplace-Changepoint Detector), as in "Distribution-dependent and Time-uniform Bounds for Piecewise i.i.d Bandits" (Mukherjee & Maillard, 2019)
  *
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  *
  *                        Note: It makes the assumption of global abrupt change points, no forced exploration
  *                        Note: Does not need the knowledge of time horizon
  *                        Note: Parameter delta is tuned internal (set to 1/t at runtime) as the authors suggested
  */
case class UCBL_CPD(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditUCB {
  override val initializationvalue = 0.0 // Does not do optimistic optimization but pull each arm once instead.
  val name = s"UCBL-CPD"
  t = 1
  (0 until narms).foreach(x => sums(x) = 0.0)
  (0 until narms).foreach(x => counts(x) = 0.0)
  var cumulative_history: scala.collection.mutable.Map[Int, Array[(Int, Double)]] =
    collection.mutable.Map((0 until narms).map(x => x -> Array[(Int, Double)]()).toMap.toSeq: _*)

  // val horizon: Int = stream.nbatches
  // var changedetected: Boolean = false // Just to flag whether there was a change in the iteration or not

  override def reset: Unit = {
    super.reset
    var cumulative_history: scala.collection.mutable.Map[Int, Array[(Int, Double)]] =
      collection.mutable.Map((0 until narms).map(x => x -> Array[(Int, Double)]()).toMap.toSeq: _*)
    t = 1
    (0 until narms).foreach(x => sums(x) = 0.0)
    (0 until narms).foreach(x => counts(x) = 0.0)
  }

  // return a vector a 2-tuples (arms) and a gain
  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val (arms, gains) = if (t < narms + 1) { // Pull each arm once
      val indexes: Array[Int] = Array(((t - 1) % narms).toInt)
      val arms = indexes.map(combinations(_))
      val newValues = stream.nextAndCompute(indexes)

      // If true then the bandit has finished, stream is exhausted
      if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)
      // If some gain is negative then "discard" the round by assigning -1 reward
      val gainscheck: Array[Double] = (indexes zip newValues).map(x => reward.getReward(x._2, currentMatrix(x._1)))
      if (gainscheck.exists(x => x < 0)) return (arms, gainscheck, -1)

      // Update the current Matrix, compute the gains and update the weights at the same time
      val gains = (indexes zip newValues).map(x => {
        val d = reward.getReward(x._2, currentMatrix(x._1))
        currentMatrix(x._1) = x._2 // replace
        counts(x._1) += 1.0
        sums(x._1) += d
        val lastelement: (Int, Double) = if (cumulative_history(x._1).isEmpty) (0, 0.0) else cumulative_history(x._1).last
        cumulative_history(x._1) = cumulative_history(x._1) :+ (lastelement._1 + 1, lastelement._2 + d)
        d
      })
      t = t + 1
      k = scalingstrategy.scale(gains, indexes, sums, counts, t)
      (arms, gains)
    } else {
      val delta: Double = 1.0 / t
      val confidences = counts.map(x => math.sqrt((1.0 + 1.0 / x) * (math.log(math.sqrt(x + 1) / delta) / (2 * x)))) // As Equation 1 in paper
      val upperconfidences = sums.zip(counts).zip(confidences).map(x => (x._1._1 / x._1._2) + x._2) //.min(1.0))
      val sortedupperconfidences = upperconfidences.zipWithIndex.sortBy(-_._1).map(_._2)
      val indexes: Array[Int] = sortedupperconfidences.take(1)
      val arms = indexes.map(combinations(_))
      val newValues = stream.nextAndCompute(indexes)

      // If true then the bandit has finished, stream is exhausted
      if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)
      // If some gain is negative then "discard" the round by assigning -1 reward
      val gainscheck: Array[Double] = (indexes zip newValues).map(x => reward.getReward(x._2, currentMatrix(x._1)))
      if (gainscheck.exists(x => x < 0)) return (arms, gainscheck, -1)

      // Update the current Matrix, compute the gains and update the weights at the same time
      val gains = (indexes zip newValues).map(x => {
        val d = reward.getReward(x._2, currentMatrix(x._1))
        currentMatrix(x._1) = x._2 // replace
        counts(x._1) += 1.0
        sums(x._1) += d
        val lastelement: (Int, Double) = if (cumulative_history(x._1).isEmpty) (0, 0.0) else cumulative_history(x._1).last
        cumulative_history(x._1) = cumulative_history(x._1) :+ (lastelement._1 + 1, lastelement._2 + d)
        d
      })
      t = t + 1

      k = scalingstrategy.scale(gains, indexes, sums, counts, t)
      (arms, gains)
    }

    // Change detection time
    def ChangePointDetection: Boolean = {
      (0 until narms).foreach { x =>
        if (cumulative_history(x).length >= 30) { // Making sure we have enough observation for that arm before we try to detect changes, might not be necessary
          cumulative_history(x).indices.foreach { y =>
            val ldelta = 1 / t //(y + 1) //TODO: The two deltas are the same (seems to work)
            val rdelta = 1 / t //(cumulative_history(x).length - y)
            val le = cumulative_history(x)(y)
            val ri = (cumulative_history(x).last._1 - le._1, cumulative_history(x).last._2 - le._2)
            val lconf = math.sqrt((1.0 + 1.0 / le._1) * (math.log(math.sqrt(le._1 + 1) / ldelta) / (2 * le._1)))
            val rconf = math.sqrt((1.0 + 1.0 / ri._1) * (math.log(math.sqrt(ri._1 + 1) / rdelta) / (2 * ri._1)))
            if ((le._2 / le._1 + lconf) < (ri._2 / ri._1 - rconf)) {
              //  println(s"UCBL-CPD Change detected at time $t on arm $x")
              return true
            }
            if ((le._2 / le._1 - lconf) > (ri._2 / ri._1 + rconf)) {
              //  println(s"UCBL-CPD Change detected at time $t on arm $x")
              return true
            }
          }
        }
      }
      false
    }

    if (ChangePointDetection) {
      (0 until narms).foreach { x => // reinitialize all the arms
        sums(x) = 0.0
        counts(x) = 0.0
        cumulative_history(x) = Array[(Int, Double)]() //reset entire memory for this arm
      }
      t = 1
    }

    //if(t%100 == 0) println(s"UCBL-CPD: $t, played ${arms.head}, gained ${gains.head}")

    val gain = gains.sum
    (arms, gains, gain)
  }
}
