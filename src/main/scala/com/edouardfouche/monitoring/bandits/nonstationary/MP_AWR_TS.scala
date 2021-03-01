package com.edouardfouche.monitoring.bandits.nonstationary

import breeze.stats.distributions.Beta
import com.edouardfouche.monitoring.bandits.{BanditAdwin, BanditTS}
import com.edouardfouche.monitoring.resetstrategies.SharedAdwin
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * Multiple Play Thompson Sampling, combined with ADWIN
  * The idea of MP-TS comes from "Optimal Regret Analysis of Thompson Sampling in Stochastic Multi-armed BanditK Problem with Multiple Plays" (Komiyama 2016)
  * In "Scaling Multi-Armed Bandit Algorithms" (Fouché 2019), this is referred to as S-TS-ADWIN
  *
  * @param delta the parameter for ADWIN (upper bound for the false positive rate)
  * @param stream a stream simulator on which we let this bandit run
  * @param reward the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k the initial number of pull per round
  */
case class MP_AWR_TS(delta: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditTS with BanditAdwin {
  val name = s"MP-AWR-TS; d=$delta"

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val draws = beta_params.zipWithIndex.map(x => (x._2, new Beta(x._1._1,x._1._2).draw())).sortBy(- _._2).take(k)
    val indexes = draws.map(_._1)
    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    val updates = scala.collection.mutable.Map[Int, Double]()

    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2
      beta_params(x._1) = (beta_params(x._1)._1+d, beta_params(x._1)._2+(1.0-d))
      counts(x._1) += 1.0
      sums(x._1) += d

      // Add into adwin and add the update into the map
      sharedAdwin.addElement(x._1, d)
      updates(x._1) = d
      d
    })
    history = history :+ updates
    t += 1

    k = scalingstrategy.scale(gains, indexes, sums, counts, t)

    // Here we, add up the size of the adwin (those are the number of pulls) and the number of unpulls, to get the
    // actual size of window each arm.
    val windows = (0 until narms).map(x => (x, sharedAdwin.getSingleSize(x) + (history.length-counts(x))))

    val smallest_window = windows.minBy(_._2) // this is the smallest window

    // Then some ADWIN instance has shrinked and we must reset.
    if(smallest_window._2.toInt < history.length-1) {
      //println(s"${name} resetting at time t=$t, smallestw=${smallest_window._2.toInt}, history.length=${history.length}")
      sharedAdwin = new SharedAdwin(stream.npairs, delta)
      history = List()
      beta_params = (0 until narms).map(x => (1.0,1.0)).toArray
      sums = (0 until narms).map(_ => initializationvalue).toArray // Initialization the weights to maximal gain forces to exploration at the early phase
      counts = sums.map(_ => initializationvalue)
    }
    //t = history.length + 1 // The time context is the same as the history, which is the same as the smallest window

    (arms, gains, gains.sum)
  }

}
