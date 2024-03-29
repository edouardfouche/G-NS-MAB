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
import com.edouardfouche.monitoring.bandits.BanditUCB
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * Sliding-Window UCB # (SW-UCB#) with Multiple Plays -- For gradual environment
  * As in "On  abruptly-changing and  slowly-varying  multiarmed bandit problems" (Wei and Srivastava, 2018)
  *
  * @param windowsize size of the sliding window
  * @param kappa a value  in [0,1) that quantifies how much the stream changes in gradual environment
  * @param lambda a real value that quantifies how much the stream changes
  * @param stream a stream simulator on which we let this bandit run
  * @param reward the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k the initial number of pull per round
  *
  * The authors selected lambda = 12.3 and lambda = 4.3 for abruptly-changing and slowly-varying environments, respectively
  */
case class MP_SW_UCB_SHARP_G(kappa: Double, lambda: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditUCB {
  val alpha: Double = (1.0).min(3.0*kappa/4.0)
  var windowsize: Double = t.min(lambda * math.pow(t,alpha)).max(1)

  override val logfactor: Double = (1+alpha) // as in the paper

  val name = s"MP-SW-UCB#-G; ka=$kappa; l=$lambda"

  var history: List[scala.collection.mutable.Map[Int,Double]] = List() // first el in the update for count, and last in the update for weight

  override def reset: Unit = {
    super.reset
    history = List()
  }

  // return a vector a 2-tuples (arms) and a gain
  def next: (Array[(Int, Int)], Array[Double], Double) = {
    windowsize = t.min(lambda * math.pow(t,alpha)).max(1) // compute the new windowsize

    val confidences = counts.map(x =>
      if(t==0.0 | x == 0.0) (0+Gaussian(0, 1).draw()*0.000001).max(0)
      else (math.sqrt((logfactor*math.log(windowsize))/x)+Gaussian(0, 1).draw()*0.000001).max(0))

    val upperconfidences = sums.zip(counts).zip(confidences).map(x => (x._1._1/x._1._2)+ x._2)//.min(1.0))

    val sortedupperconfidences = upperconfidences.zipWithIndex.sortBy(-_._1).map(_._2)

    val indexes: Array[Int] = if((k==1) & (t <= narms)) { // The first forced exploration round
      Array(t.toInt-1)
    } else {
      if(t <= narms) sortedupperconfidences.filter(_ != t-1).take(k-1) ++ Array(t.toInt-1)
      else sortedupperconfidences.take(k)
    }

    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)

    // If true then the bandit has finished, stream is exhausted
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)
    // If some gain is negative then "discard" the round by assigning -1 reward
    val gainscheck: Array[Double] = (indexes zip newValues).map(x => reward.getReward(x._2, currentMatrix(x._1)))
    if(gainscheck.exists(x => x < 0)) return (arms, gainscheck, -1)

    val updates = scala.collection.mutable.Map[Int, Double]()

    // Update the current Matrix, compute the gains and update the weights at the same time
    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2 // replace
      counts(x._1) += 1.0
      sums(x._1) += d
      updates(x._1) = d
      d
    })
    history = history :+ updates
    t = t + 1

    k = scalingstrategy.scale(gains, indexes, sums, counts, t.min(windowsize))

    // Rolling back
    if(windowsize < history.length) {
      for{
        x <- windowsize.toInt until history.length
      } {
        val rollback = history.head
        history = history.tail
        for((key,value) <- rollback) {
          sums(key) = sums(key) - value
          counts(key) = counts(key) - 1
        }
      }
    }

    val gain = gains.sum
    (arms, gains, gain)
  }

}
