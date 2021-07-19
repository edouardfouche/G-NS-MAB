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

import breeze.stats.distributions.Beta
import com.edouardfouche.monitoring.bandits.{BanditAdwin, BanditTS}
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * Multiple Play Thompson Sampling, combined with ADWIN1
  * The idea of MP-TS comes from "Optimal Regret Analysis of Thompson Sampling in Stochastic Multi-armed BanditK Problem with Multiple Plays" (Komiyama 2016)
  * In "Scaling Multi-Armed Bandit Algorithms" (Fouché 2019), therein this is  referred to as S-TS-ADWIN (but it used ADWIN2)
  * ADS-TS drops the oldest window, while ADR-TS drops both windows
  *
  * @param delta           the parameter for ADWIN (upper bound for the false positive rate)
  * @param ADR             If true, reset the ADWIN window whenever a change is detected (instead of shrinking)
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  */
case class MP_ADS_TS_ADWIN1(delta: Double, ADR: Boolean = false)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditTS with BanditAdwin {
  val name = s"MP-ADS-TS-ADWIN1; d=$delta; r=$ADR"

  var cumulative_history: scala.collection.mutable.Map[Int,Array[(Int,Double)]] =
    collection.mutable.Map((0 until narms).map(x => x -> Array[(Int,Double)]()).toMap.toSeq: _*)
  def epsilon(n:Int,m:Int): Double = math.sqrt(math.log(1.0/delta)/(2.0*n)) + math.sqrt(math.log(1.0/delta)/(2.0*m))

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val draws = beta_params.zipWithIndex.map(x => (x._2, new Beta(x._1._1,x._1._2).draw())).sortBy(- _._2).take(k)
    val indexes = draws.map(_._1)
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
      beta_params(x._1) = (beta_params(x._1)._1+d, beta_params(x._1)._2+(1.0-d))
      counts(x._1) += 1.0
      sums(x._1) += d

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
              if(ADR) { //ADR
                (0 until narms).foreach { z => cumulative_history(z) = Array[(Int, Double)]() } // Reset entire memory
                history = List()
                beta_params = (0 until narms).map(x => (1.0, 1.0)).toArray
                sums = (0 until narms).map(_ => initializationvalue).toArray // Initialization the weights to maximal gain forces to exploration at the early phase
                counts = sums.map(_ => initializationvalue)
              }else { //ADS
                (0 until narms).foreach { z => cumulative_history(z) = cumulative_history(z).drop(i + 1) } // Remove first i elements
                val torollback = history.take(i + 1)
                history = history.drop(i + 1)
                for (rollback <- torollback) {
                  for ((key, value) <- rollback) {
                    sums(key) = sums(key) - value
                    counts(key) = counts(key) - 1 //- value._2
                    beta_params(key) = (beta_params(key)._1 - value, beta_params(key)._2 - (1.0 - value))
                  }
                }
              }
            }
            i += 3 //3 is for speeding up
          }
        }
      }
    }
    t = history.length + 1 // The time context is the same as the history, which is the same as the smallest window
    (arms, gains, gains.sum)
  }
}