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
package com.edouardfouche.monitoring.bandits.stationary

import com.edouardfouche.monitoring.bandits.BanditKLUCB
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * KL-UCB with multiple plays
  * This is an extension of KL-UCB "Kullback-leibler upper confidence bounds for optimal sequential allocation" (Cappé2013)
  * as described in "Optimal Regret Analysis of Thompson Sampling in Stochastic Multi-armed Bandit Problem with Multiple Plays" (Komiyama2016)
  *
  * @param stream a stream simulator on which we let this bandit run
  * @param reward the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k the initial number of pull per round
  *
  * @note As proposed in Garivier2011, the constant c is set to 0. (better empirical results)
  * @note This is basically the same as C-KL-UCB in "Thompson Sampling for Combinatorial Semi-Bandits" (Wang2018)
  * @note The implementation is based on https://github.com/jkomiyama/multiplaybanditlib/blob/master/policy/policy_klucb.hpp
  */
case class MPKLUCBPLUS(stream: Simulator, reward: Reward, scalingstrategy: ScalingStrategy, var k: Int) extends BanditKLUCB {
  val name = "MP-KLUCBPLUS"

  // KLUCBPLUS is a slight modification of the upper bound in KLUCB
  override def getKLUCBupper(arm: Int, t: Double): Double = {
    val logndn = scala.math.log(t/ counts(arm)) / counts(arm) // alternative: KL-UCB+ scala.math.log(t/ counts(arm)) / counts(arm)
    val p: Double = (sums(arm)/counts(arm)).max(Ndelta)
    if(p >= 1.0) return 1.0

    var q = p + Ndelta
    for(i <- 1 to maxiter) {
      val f = logndn - kl(p,q)
      val df = -dkl(p,q)
      if(f*f < eps) return q // newton's method has converged
      q = (1.0-Ndelta).min((q - f/df).max(p+Ndelta))
    }
    q
  }

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val klindices:Array[(Int,Double)] = (0 until narms).map(x => if(t==0 | counts(x) == 0.0) (x,1.0) else (x,getKLUCBupper(x,t))).toArray

    val indexes = klindices.sortBy(-_._2).map(_._1).take(k)

    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)

    // If true then the bandit has finished, stream is exhausted
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)
    // If some gain is negative then "discard" the round by assigning -1 reward
    val gainscheck: Array[Double] = (indexes zip newValues).map(x => reward.getReward(x._2, currentMatrix(x._1)))
    if(gainscheck.exists(x => x < 0)) return (arms, gainscheck, -1)

    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2 // replace
      counts(x._1) += 1
      sums(x._1) += d
      d
    })
    t = t + 1

    k = scalingstrategy.scale(gains, indexes, sums, counts, t)

    val gain = gains.sum
    (arms, gains, gain)
  }

}
