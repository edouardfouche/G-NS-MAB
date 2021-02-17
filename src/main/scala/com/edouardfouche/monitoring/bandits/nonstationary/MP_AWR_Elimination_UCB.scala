package com.edouardfouche.monitoring.bandits.nonstationary

import breeze.stats.distributions.Beta
import com.edouardfouche.monitoring.bandits.{BanditAdwin, BanditTS, BanditUCB}
import com.edouardfouche.monitoring.resetstrategies.SharedAdwin
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * Resetting Adaptive Window with Elimination UCB
  *
  * @param stream a stream simulator on which we let this bandit run
  * @param reward the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k the initial number of pull per round
  */
case class MP_AWR_Elimination_UCB(val delta: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditUCB with BanditAdwin {
  val name = s"MP-AWR-Elimination-UCB; d=$delta"

  var istar: Array[Int] = (0 until narms).map(x => x).toArray // candidates of the best arms
  var sums_e: Array[Double] = (0 until narms).map(_ => initializationvalue).toArray // Initialization the weights to maximal gain forces to exploration at the early phase
  var counts_e: Array[Double] = sums.map(_ => initializationvalue)

  override def reset: Unit = {
    super.reset
    istar = (0 until narms).map(x => x).toArray // candidates of the best arms
    sums_e = (0 until narms).map(_ => initializationvalue).toArray
    counts_e = sums.map(_ => initializationvalue)
  }

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val toexplore = (t-1).toInt % narms.toInt // k

    val confidences = counts.map(x => if(t==0.0 | x == 0.0) 0 else math.sqrt((logfactor*math.log(2*math.pow(t,3)))/(2*x)))
    val confidences_e = counts_e.map(x => if(t==0.0 | x == 0.0) 0 else math.sqrt((logfactor*math.log(2*math.pow(t,3)))/(2*x)))

    val upperconfidences = sums.zip(counts).zip(confidences).map(x => (x._1._1 / x._1._2) + x._2)//.min(1.0))
    val upperconfidences_e = sums_e.zip(counts_e).zip(confidences_e).map(x => (x._1._1 / x._1._2) + x._2)//.min(1.0))

    val sortedindices = upperconfidences.zipWithIndex.sortBy(-_._1).map(_._2)
    val indexes: Array[Int] =  if(istar.contains(toexplore)){ // draw toexplore and the top k-1 arms
      Array(toexplore) ++ sortedindices.filter(_ != toexplore).take(k-1)
    } else {
      sortedindices.take(k)
    }

    if(istar.length > 1) { // elimination
      val lowerconfidences_e = sums_e.zip(counts_e).zip(confidences_e).map(x => (x._1._1 / x._1._2) - x._2)//.min(1.0))
      var toremove = List[Int]()
      for(x <- istar) {
        if(lowerconfidences_e.exists(y => y > confidences(x))) {
          toremove :+ x
        }
      }
      istar = istar.filter(x => !toremove.contains(x))
    }

    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    val updates = scala.collection.mutable.Map[Int, Double]()

    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2
      counts(x._1) += 1.0
      sums(x._1) += d
      if(x._1 == toexplore) {
        counts_e(x._1) += 1.0
        sums_e(x._1) += d
      }
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
    if(smallest_window._2.toInt < history.length) {
      sharedAdwin = new SharedAdwin(stream.npairs, delta)
      history = List()
      (0 until narms).foreach { x => // reinitialize all the arms and the candidates
        counts(x) = initializationvalue
        sums(x) = initializationvalue
        counts_e(x) = initializationvalue
        sums_e(x) = initializationvalue
        istar = (0 until narms).map(x => x).toArray
      }
    }
    t = history.length + 1 // The time context is the same as the history, which is the same as the smallest window

    (arms, gains, gains.sum)
  }

}
