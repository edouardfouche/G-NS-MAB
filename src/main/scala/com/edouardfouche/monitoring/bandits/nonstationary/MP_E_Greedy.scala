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

import com.edouardfouche.monitoring.bandits.Bandit
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * Epsilon-Greedy with Multiple Plays
  * Espilon-Greedy strategies are described in "Reinforcement learning: An introduction" (Sutton1998)
  *
  * @param epsilon         the parameter controlling the greediness. At each round, with probability epsilon, we are greedy, otherwise random
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  * @note This approach is non-static because the epsilon does not vanish, however, weights are not discounted
  */
case class MP_E_Greedy(epsilon: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends Bandit {
  require((0 <= epsilon) & (epsilon <= 1)) // epsilon = 0 is like a random oracle while epsilon = 1 is completely greedy

  val name = s"MP-E-G; e=$epsilon"

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    var sortedindexes = sums.zip(counts).zipWithIndex.sortBy(x => - x._1._1 / x._1._2).map(_._2)

    // MP-EpsilonGreedyBandit: Take the head of sortedindexes or randomly
    val indexes = (0 until k).map(x => {
      if(math.random <= epsilon) {
        val arm = sortedindexes.head
        sortedindexes = sortedindexes.tail
        arm
      }
      else {
        val randomindex = scala.util.Random.nextInt(sortedindexes.length)
        val arm = sortedindexes(randomindex)
        sortedindexes  = sortedindexes.take(randomindex) ++ sortedindexes.drop(randomindex+1)
        arm
      }
    }).toArray

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
      counts(x._1) += 1.0
      sums(x._1) += d
      d
    })

    t += 1
    k = scalingstrategy.scale(gains, indexes, sums, counts, t)

    // Sum up the gain of the top arms / top indexes
    val gain = gains.sum
    (arms, gains, gain)
  }

}
