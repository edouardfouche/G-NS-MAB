package com.edouardfouche.monitoring.bandits.nonstationary

import breeze.stats.distributions.Gaussian
import com.edouardfouche.monitoring.bandits.{BanditAdwin, BanditUCB}
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * CUCB as described in "Combinatorial Multi-Armed BanditK: General Framework, Results and Applications" (Chen2013)
  * This version is combined with ADWIN
  *
  * @param delta the parameter for ADWIN (upper bound for the false positive rate)
  * @param stream a stream simulator on which we let this bandit run
  * @param reward the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k the initial number of pull per round
  */
case class MP_ADS_UCB(delta: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditAdwin with BanditUCB {
  val name = s"MP-ADS-UCB; d=$delta"

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val confidences = counts.map(x =>
      if(t==0.0 | x == 0.0) (0+Gaussian(0, 1).draw()*0.000001).max(0)
      else math.sqrt((logfactor*math.log(t))/x)+ Gaussian(0, 1).draw()*0.000001)

    val upperconfidences = sums.zip(counts).zip(confidences).map(x => (x._1._1 / x._1._2) + x._2)//.min(1.0))
    val indexes = upperconfidences.zipWithIndex.sortBy(-_._1).map(_._2).take(k)
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
      currentMatrix(x._1) = x._2 // replace
      counts(x._1) += 1.0
      sums(x._1) += d

      // Add into adwin and add the update into the map
      sharedAdwin.addElement(x._1, d)
      updates(x._1) = d
      d
    })
    history = history :+ updates
    t = t + 1

    k = scalingstrategy.scale(gains, indexes, sums, counts, t)

    // Here we, add up the size of the adwin (those are the number of pulls) and the number of unpulls, to get the
    // actual size of each pulled arm.
    val windows = (0 until narms).map(x => (x, sharedAdwin.getSingleSize(x) + (history.length-counts(x))))

    val smallest_window = windows.minBy(_._2) // this is the smallest window

    // Rolling back on the ADWIN knowledge
    for {
      x <-  (0 until narms)
    } {
      if(windows(x)._2.toInt > smallest_window._2.toInt) {
        for{
          y <- smallest_window._2.toInt until windows(x)._2.toInt
        } {
          sharedAdwin.deleteElement(x)
        }
      }
    }

    // Rolling back
    if(smallest_window._2.toInt < history.length-1) {
      for{
        x <- smallest_window._2.toInt until history.length
      } {
        val rollback = history.head
        history = history.tail
        for((key,value) <- rollback) {
          sums(key) = sums(key) - value
          counts(key) = counts(key) - 1
        }
      }
    }
    t = history.length + 1 // The time context is the same as the history, which is the same as the smallest window

    (arms, gains, gains.sum)
  }


}
