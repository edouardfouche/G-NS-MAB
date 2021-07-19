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
package com.edouardfouche.monitoring.bandits.oracles

import com.edouardfouche.monitoring.bandits.Bandit
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * A Dynamic Oracle with Multiple Plays, which always choose the top-k arms (not "in expectation") at each round
  * This is like the "dynamic oracle" described in "Optimal Exploration-Exploitation in a Multi-Armed-Bandit Problem with Non-stationary Rewards" (Besbes14)
  *
  * @param stream a stream simulator on which we let this bandit run
  * @param reward the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k the initial number of pull per round
  */
case class OracleDynamic(stream: Simulator, reward: Reward, scalingstrategy: ScalingStrategy, var k: Int) extends Bandit {
  val name: String = "OD"

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val newMatrix = stream.nextAndCompute(combinations.zipWithIndex.map(_._2))
    if (newMatrix.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    // Find the top-k arms
    val diffMatrix = newMatrix.zip(currentMatrix.toArray).map(x => reward.getReward(x._1, x._2))

    // Update the current Matrix
    val topindexes = diffMatrix.zipWithIndex.sortBy(-_._1).map(_._2).take(k)
    val topgains = diffMatrix.zipWithIndex.sortBy(-_._1).map(_._1).take(k)
    val toparms = topindexes.map(combinations(_))

    // If some gain is negative then "discard" the round by assigning -1 reward
    if(topgains.exists(x => x < 0)) return (toparms, topgains, -1)

    topindexes.foreach(x => {
      currentMatrix(x) = newMatrix(x)
      counts(x) += 1.0
      sums(x) += diffMatrix(x)
    })

    // Sum up the gain of the top arms / top indexes
    val gains = topindexes.map(diffMatrix(_))

    t += 1
    k = scalingstrategy.scale(gains, topindexes, sums, counts, t)

    (toparms, gains, gains.sum)
  }

}
