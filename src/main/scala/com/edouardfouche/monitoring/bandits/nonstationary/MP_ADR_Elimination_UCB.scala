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

import breeze.stats.distributions.Gaussian
import com.edouardfouche.monitoring.bandits.{BanditAdwin, BanditUCB}
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * Resetting Adaptive Window with Elimination UCB
  *
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k the initial number of pull per round
  */
case class MP_ADR_Elimination_UCB(delta: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditUCB with BanditAdwin {
  val name = s"MP-ADR-Elimination-UCB-ADWIN1; d=$delta"

  var cumulative_history: scala.collection.mutable.Map[Int, Array[(Int, Double)]] =
    collection.mutable.Map((0 until narms).map(x => x -> Array[(Int, Double)]()).toMap.toSeq: _*)

  def epsilon(n: Int, m: Int): Double = math.sqrt(math.log(1.0 / delta) / (2.0 * n)) + math.sqrt(math.log(1.0 / delta) / (2.0 * m))

  var istar: Array[Int] = (0 until narms).map(x => x).toArray // Candidates of the best arms
  var toremove = Array[Int]()

  var sums_e: Array[Double] = (0 until narms).map(_ => initializationvalue).toArray // Initialization the weights to maximal gain forces to exploration at the early phase
  var counts_e: Array[Double] = sums.map(_ => initializationvalue)
  val horizon: Int = stream.nbatches

  override def reset: Unit = {
    super.reset
    istar = (0 until narms).map(x => x).toArray // Candidates of the best arms
    sums_e = (0 until narms).map(_ => initializationvalue).toArray
    counts_e = sums.map(_ => initializationvalue)
  }

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val toexplore = (t-1).toInt % narms.toInt // k

    val confidences = counts.map(x =>
      if(t==0.0 | x == 0.0) (0+Gaussian(0, 1).draw()*0.000001).max(0)
      else math.sqrt(math.log(math.pow(horizon,4))/(2*x))+Gaussian(0, 1).draw()*0.000001)

    val confidences_e = counts_e.map(x =>
      if(t==0.0 | x == 0.0) (0+Gaussian(0, 1).draw()*0.000001).max(0)
      else math.sqrt(math.log(math.pow(horizon,4))/(2*x))+Gaussian(0, 1).draw()*0.000001)

    val upperconfidences = sums.zip(counts).zip(confidences).map(x =>
      if(x._1._2 == 1) math.pow(horizon,4)+Gaussian(0, 1).draw()*0.000001 + x._2  // if never drawn since last forgetting then put a huge value
      else (x._1._1 / x._1._2) + x._2)//.min(1.0))
    val upperconfidences_e = sums_e.zip(counts_e).zip(confidences_e).map(x =>
      if(x._1._2 == 1) math.pow(horizon,4)+Gaussian(0, 1).draw()*0.000001 + x._2  // if never drawn since last forgetting then put a huge value
      else (x._1._1 / x._1._2) + x._2)//.min(1.0))

    val sortedindices = upperconfidences.zipWithIndex.sortBy(-_._1).map(_._2)
    val indexes: Array[Int] =  if(istar.contains(toexplore)){ // draw toexplore and the top k-1 arms
      Array(toexplore) ++ sortedindices.filter(_ != toexplore).take(k-1)
    } else {
      sortedindices.take(k)
    }

    if(istar.length > 1) { // elimination
      val lowerconfidences_e = sums_e.zip(counts_e).zip(confidences_e).map(x =>
        if(x._1._2 == 1) math.pow(horizon,4)+Gaussian(0, 1).draw()*0.000001 - x._2 // if never drawn since last forgetting then put a huge value
        else (x._1._1 / x._1._2) - x._2)//.min(1.0))
      toremove = Array[Int]()
      for(x <- istar) if(lowerconfidences_e.exists(y => y > upperconfidences_e(x))) toremove = toremove ++ Array(x)
      for(x <- toremove) if(istar.length > 1) istar = istar.filter(y => y == x)
    }

    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)

    // If true then the bandit has finished, stream is exhausted
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)
    // If some gain is negative then "discard" the round by assigning -1 reward
    val gainscheck: Array[Double] = (indexes zip newValues).map(x => reward.getReward(x._2, currentMatrix(x._1)))
    if(gainscheck.exists(x => x < 0)) return (arms, gainscheck, -1)

    val updates = scala.collection.mutable.Map[Int, Double]()

    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2
      counts(x._1) += 1.0
      sums(x._1) += d
      if(x._1 == toexplore) {
        counts_e(x._1) += 1.0
        sums_e(x._1) += d
      }
      // Add into adwin and add the update into the map
      val lastelement: (Int, Double) = if(cumulative_history(x._1).isEmpty) (0,0.0) else cumulative_history(x._1).last
      cumulative_history(x._1) = cumulative_history(x._1) :+ (lastelement._1 + 1, lastelement._2 + d)
      updates(x._1) = d
      d
    })
    history = history :+ updates
    t += 1

    k = scalingstrategy.scale(gains, indexes, sums, counts, t)

    // ADWIN1 change detection
    (0 until narms).foreach { x =>
      if(cumulative_history(x).length > 10) {
        if(t % 10 == 0) {
          val (nt, st): (Int, Double) = cumulative_history(x).last
          var changedetected = false
          var i = 1
          while((!changedetected) && i < (cumulative_history(x).length-2)) {
            changedetected = false
            val y: (Int, Double) = cumulative_history(x)(i)
            if (math.abs((y._2 / y._1.toDouble) - (st - y._2) / (nt.toDouble - y._1.toDouble)) > epsilon(y._1, nt - y._1)) {
              changedetected = true
              (0 until narms).foreach { z =>
                cumulative_history(z) = Array[(Int, Double)]()
                counts(z) = initializationvalue
                sums(z) = initializationvalue
                counts_e(z) = initializationvalue
                sums_e(z) = initializationvalue
              } //reset entire memory
              history = List()
              istar = (0 until narms).map(x => x).toArray
            }
            i += 3 //3 is for speeding up //
          }
        }
      }
    }
    t = history.length + 1 // The time context is the same as the history, which is the same as the smallest window
    (arms, gains, gains.sum)
  }

}
