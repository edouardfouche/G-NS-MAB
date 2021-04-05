package com.edouardfouche.monitoring.bandits.nonstationary

import breeze.stats.distributions.Beta
import com.edouardfouche.monitoring.bandits.BanditTS
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * Sliding-Window TS with Multiple Plays
  * As in Trovò, F., Restelli, M., and Gatti, N. (2020). Sliding-window thompson sampling for non-stationary settings
  * But possibly with multiple-plays
  *
  * @param windowsize size of the sliding window
  * @param stream a stream simulator on which we let this bandit run
  * @param reward the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k the initial number of pull per round
  *
  */
case class MP_SW_TS(windowsize: Int)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditTS {
  require(windowsize > 1)

  val name = s"MP-SW-TS; w=$windowsize"

  var sumsbuffer: Array[Array[Double]] = (0 until narms).map(x => (0 to windowsize).toArray.map(y => 0.0)).toArray
  var countsbuffer: Array[Array[Double]] = (0 until narms).map(x => (0 to windowsize).toArray.map(y => 0.0)).toArray

  override def reset: Unit = {
    super.reset
    sumsbuffer = (0 until narms).map(x => (0 to windowsize).toArray.map(y => 0.0)).toArray
    countsbuffer = (0 until narms).map(x => (0 to windowsize).toArray.map(y => 0.0)).toArray
  }

  // return a vector a 2-tuples (arms) and a gain
  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val sorteddraws = beta_params.zipWithIndex.map(x => (x._2, new Beta(x._1._1,x._1._2).draw())).sortBy(- _._2)
    val indexes = sorteddraws.take(k).map(_._1)
    val notindexes = sorteddraws.drop(k).map(_._1)

    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)

    // If true then the bandit has finished, stream is exhausted
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)
    // If some gain is negative then "discard" the round by assigning -1 reward
    val gainscheck: Array[Double] = (indexes zip newValues).map(x => reward.getReward(x._2, currentMatrix(x._1)))
    if(gainscheck.exists(x => x < 0)) return (arms, gainscheck, -1)

    val bufferposition = ((t-1) % windowsize).toInt
    // forget past values
    if(t-1 >= windowsize) {
      counts.indices.foreach { x =>
        counts(x) -= countsbuffer(x)(bufferposition)
        sums(x) -= sumsbuffer(x)(bufferposition)
        //beta_params(x) = ((beta_params(x)._1 - sumsbuffer(x)(bufferposition)).max(0.001), (beta_params(x)._2-(1.0-sumsbuffer(x)(bufferposition))).max(0.001))
        beta_params(x) = ((1.0+sums(x)).max(0.001), (1.0+counts(x)-sums(x)).max(0.001))
      }
    }

    notindexes.indices.foreach{x =>
      countsbuffer(x)(bufferposition) = 0
      sumsbuffer(x)(bufferposition) = 0.0
    }

    // Update the current Matrix, compute the gains and update the weights at the same time
    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2 // replace
      beta_params(x._1) = ((beta_params(x._1)._1+d).max(0.001), (beta_params(x._1)._2+(1.0-d)).max(0.001))
      counts(x._1) += 1.0
      sums(x._1) += d
      countsbuffer(x._1)(bufferposition) = 1
      sumsbuffer(x._1)(bufferposition) = d
      d
    })
    t = t + 1

    k = scalingstrategy.scale(gains, indexes, sums, counts, t.min(windowsize))

    val gain = gains.sum
    (arms, gains, gain)
  }

}
