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
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  */
case class MP_ADR_TS_ADWIN1_v5(delta: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditTS with BanditAdwin {
  val name = s"MP-ADR-TS-ADWIN1-v5; d=$delta"
  // Monitoring parameters (new)
  val D: Int = narms
  var cumulative_history: scala.collection.mutable.Map[Int, Array[(Int, Double)]] =
    collection.mutable.Map((0 until narms).map(x => x -> Array[(Int, Double)]()).toMap.toSeq: _*)
  var horizon: Int = stream.nbatches
  // var L: Int = narms // Block length //TODO: How to set???
  // var M: Int = math.ceil(horizon/L).toInt // Number of blocks
  // var monitoredArms: Array[Int] = (0 until narms).map(x => x).toArray // Set of monitored arms (mathcal L)
  var m: Array[Int] = (0 until narms).map(x => 0).toArray // Counter for each arm
  var N: Int = math.ceil(math.log(horizon) / math.log(2)).toInt
  var l: Int = 1 // block index (1 to math.log(T/DN + 1)/math.log(2))
  var monitored_now: Int = 0
  // var tt: Int = 1 // subindex for each block
  var monitored_pre: Int = 0
  var monitored: Boolean = false

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val (arms, gains) = {
      val indexes: Array[Int] = if ((l >= 2) & (t % D == 0)) {
        monitored = true
        Array(monitored_pre) // play monitored arm of previous block
      } else if ((l >= 2) & (t % D == 1) & (t >= ((math.pow(2, l - 1) - 2) * D * N + 1))) {
        monitored = true
        Array(monitored_now) // play monitored arm of current block
      } else { // The normal procedure
        monitored = false
        val draws = beta_params.zipWithIndex.map(x => (x._2, new Beta(x._1._1, x._1._2).draw())).sortBy(-_._2).take(k)
        draws.map(_._1)
      }

      val arms = indexes.map(combinations(_))
      val newValues = stream.nextAndCompute(indexes)

      // Usual update of base bandit
      // If true then the bandit has finished, stream is exhausted
      if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)
      // If some gain is negative then "discard" the round by assigning -1 reward
      val gainscheck: Array[Double] = (indexes zip newValues).map(x => reward.getReward(x._2, currentMatrix(x._1)))
      if (gainscheck.exists(x => x < 0)) return (arms, gainscheck, -1)

      val updates = scala.collection.mutable.Map[Int, (Double, Boolean)]()

      val gains = (indexes zip newValues).map(x => {
        val d = reward.getReward(x._2, currentMatrix(x._1))
        currentMatrix(x._1) = x._2
        beta_params(x._1) = (beta_params(x._1)._1 + d, beta_params(x._1)._2 + (1.0 - d))
        counts(x._1) += 1.0
        sums(x._1) += d

        if (!monitored) { // The counter of most drawnarms excluding monitoring rounds
          m(x._1) += 1
        }

        // Add into adwin and add the update into the map
        val lastelement: (Int, Double) = if (cumulative_history(x._1).isEmpty) (0, 0.0) else cumulative_history(x._1).last
        cumulative_history(x._1) = cumulative_history(x._1) :+ (lastelement._1 + 1, lastelement._2 + d)
        updates(x._1) = (d, monitored)
        d
      })

      // Update monitored arm
      if (t == (D * N + 1)) { // The end of the first block
        monitored_pre = m.zipWithIndex.sortBy(-_._1).map(_._2).head
      } else if ((l >= 2) & (t == (math.pow(2, l - 1) - 2) * D * N)) { // Before the beginning of the last sub-block of block l
        monitored_pre = monitored_now
        monitored_now = m.zipWithIndex.sortBy(-_._1).map(_._2).head
      }

      // The end of the block
      if (t == ((math.pow(2, l) - 1) * D * N)) {
        l += 1
        monitored_pre = monitored_now
      }

      // Advancement
      historymonitored = historymonitored :+ updates
      t += 1
      k = scalingstrategy.scale(gains, indexes, sums, counts, t)

      // val reverse_L_history_arms: List[Int] = history.reverse.take(L).flatMap(x => x.keys)
      // m.map(x => if (reverse_L_history_arms.contains(x)) (x + 1) else x)
      // monitoredArms.filter(x => m(x) >= M.min(t / L))
      (arms, gains)
    }

    // ADWIN1 change detection
    (0 until narms).foreach { x =>
      if (cumulative_history(x).length > 10) {
        if (t % 10 == 0) {
          val (nt, st): (Int, Double) = cumulative_history(x).last
          var changedetected = false
          var i = 1
          while ((!changedetected) && i < (cumulative_history(x).length - 2)) {
            changedetected = false
            val y: (Int, Double) = cumulative_history(x)(i)
            if (math.abs((y._2 / y._1.toDouble) - (st - y._2) / (nt.toDouble - y._1.toDouble)) > epsilon(y._1, nt - y._1)) {
              changedetected = true
              //println(s"TS-ADWIN Change detected at time $t on arm $x")

              // Reset monitoring variables
              // var monitoredArms: Array[Int] = (0 until narms).map(x => x).toArray // Set of monitored arms (mathcal L)
              // var m: Array[Int] = (0 until narms).map(x => 0).toArray // Counter for each arm

              // Not distinction between ADS/ADR in this alg
              (0 until narms).foreach { z => cumulative_history(z) = Array[(Int, Double)]() } // Reset entire memory
              historymonitored = List()
              beta_params = (0 until narms).map(x => (1.0, 1.0)).toArray
              sums = (0 until narms).map(_ => initializationvalue).toArray // Initialization the weights to maximal gain forces to exploration at the early phase
              counts = sums.map(_ => initializationvalue)

              m = (0 until narms).map(x => 0).toArray // Counter for each arm
              horizon = horizon - t.toInt
              N = math.ceil(math.log(horizon) / math.log(2)).toInt
              l = 1 // block index (1 to math.log(T/DN + 1)/math.log(2))
              monitored_now = 0
              monitored_pre = 0
              monitored = false
            }
            i += 3 //3 is for speeding up
          }
        }
      }
    }
    t = historymonitored.length + 1 // The time context is the same as the history, which is the same as the smallest window
    (arms, gains, gains.sum)
  }

  def epsilon(n: Int, m: Int): Double = math.sqrt(math.log(1.0 / delta) / (2.0 * n)) + math.sqrt(math.log(1.0 / delta) / (2.0 * m))
}