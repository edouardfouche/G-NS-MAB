package com.edouardfouche.monitoring.bandits.oracles

import breeze.stats.distributions.{Bernoulli, RandBasis, ThreadLocalRandomGenerator}
import com.edouardfouche.monitoring.bandits.Bandit
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator
import org.apache.commons.math3.random.MersenneTwister

import scala.util.Random

/**
  * A Random Oracle with Multiple Plays, simply choosing K arms at random for every round
  *
  * @param stream a stream simulator on which we let this bandit run
  * @param reward the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k the initial number of pull per round
  */
case class OracleAbruptGlobal(stream: Simulator, reward: Reward, scalingstrategy: ScalingStrategy, var k: Int) extends Bandit {
  val name: String = "CustomOracle"

  val n = 10000 // 100000
  val d = 100

  val a = (1 to d).map(_/d.toDouble).toArray
  val means = a.reverse
  val cols: Array[Array[Double]] = means.zipWithIndex.map{x =>
    val partA: Array[Double] = (0 until n/3).toArray.map(y => x._1)
    val partB: Array[Double] = (0 until n/3).toArray.map(y => 1 - x._1)
    val partC: Array[Double] = (0 until (n/3+n%3)).toArray.map(y => x._1)
    partA ++ partB ++ partC
  }
  val all_arms = cols.transpose
  val all_arms_sorted: Array[Array[Int]] = all_arms.map(x => x.zipWithIndex.sortBy(x => -x._1).map(x => x._2))

  var position = 0
  val horizon = stream.nbatches

  override def reset: Unit = {
    super.reset
    position = 0
  }

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    // Random BanditK: Just draw arms at random
    val indexes = all_arms_sorted(position % horizon).take(k)
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
    position += 1
    k = scalingstrategy.scale(gains, indexes, sums, counts, t)

    // Sum up the gain of the top arms / top indexes
    val gain = gains.sum
    (arms, gains, gain)
  }
}
