package com.edouardfouche.monitoring.bandits.nonstationary

import breeze.stats.distributions.Gaussian
import com.edouardfouche.monitoring.bandits.BanditKLUCB
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

import scala.util.Random

/**
  * KL-UCB with multiple plays and Bernoulli Generalized Likelihood Ratio Test, local
  * See Efficient Change-Point Detection for Tackling Piecewise-Stationary Bandits (Besson 2020 et al.)
  *
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  *
  * There is an exploration parameter alpha=\sqrt{k*A*ln(T)/T}
  * Also, tuning of delta is recommended as follows: δ = 1/\sqrt{T}
  * GLR depends on it, as the criterion is β(n, δ) = ln(math.pow(n,(3/2))/δ)
  * There is also some massive downsampling to make the GLR test computationally OK.
  * A difference between local and global restart
  */
case class MP_GLR_KL_UCB_L(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditKLUCB {
  val name = "MP-GLR-KL-UCB-L"

  var historyarm: Array[List[Double]] = (0 until narms).map(_ => List[Double]()).toArray

  val deltas = 5 // only check changes for every 5 observations
  val deltat = 10 // check for change every 10 time steps
  val horizon: Int = stream.nbatches
  var nepisodes: Int = 1 // number of episodes (restarts/changes)

  val delta: Double = 1.0/math.sqrt(horizon) // maximum error probability for the test (in (0,1))
  var alpha: Double = math.sqrt(nepisodes*narms* math.log(horizon)/horizon) // needs to be updated for each iteration (because of nepisodes)
  var changedetected: Boolean = false // Just to flag whether there was a change in the iteration or not

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    // Note that it is different from standard KL-UCB as the time is individual for each arm (tarms)
    val klindices: Array[(Int, Double)] = (0 until narms).map(x =>
      if (tarms(x) == 0 | counts(x) == 0.0) (x, 1.0+Gaussian(0, 1).draw()*0.000001)
      else (x, getKLUCBupper(x, tarms(x))+Gaussian(0, 1).draw()*0.000001)).toArray

    alpha = math.sqrt(nepisodes*narms* math.log(horizon)/horizon)

    val forcedexploration: Int = t.toInt % math.floor(narms/alpha).toInt
    val sortedindices = klindices.sortBy(-_._2).map(_._1)
    val indexes: Array[Int] =  if(forcedexploration < narms-1) {
      Array(forcedexploration) ++ sortedindices.filter(_ != forcedexploration).take(k-1)
    } else {
      sortedindices.take(k)
    }

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
      historyarm(x._1) = historyarm(x._1) :+ d // Keep history of rewards for each arm
      d
    })
    t = t + 1

    // increment personal counter of each arm
    (0 until narms).foreach{x =>
      tarms(x) += 1
    }

    k = scalingstrategy.scale(gains, indexes, sums, counts, t)


      (0 until narms).foreach { x =>
        // if we don't change anything (no observation), there will be no change too
        if(((historyarm(x).length-1) % deltat == 0) && (historyarm(x).length >= deltat)) { // Try to detect changes only every deltat time steps
          val ncheck = math.floor(historyarm(x).length / deltas).toInt-1 // This is the number of window pairs we are going to check
          val beta: Double = math.log(math.pow(historyarm(x).length,(3/2))/delta)
          var glr:Double = 0.0

          val tocheck = if(ncheck > 100) {
            //println(s"GLR-klUCB window at time $t on arm $x is too big, limiting to 1000 (random)")
            (1 to ncheck).toList
          } else Random.shuffle((1 to ncheck).toList).take(100)
          for(y <- tocheck) {
            val s = y*deltas // number of points in first window
            val mu1 = historyarm(x).slice(0, s).sum / s
            val mu2 = historyarm(x).slice(s, historyarm(x).length).sum / (historyarm(x).length-s)
            val mu = historyarm(x).sum / historyarm(x).length
            glr = glr.max(s*kl_safe(mu1, mu) + (historyarm(x).length-s)*kl_safe(mu2, mu))
          }
          if(glr > beta) { // reinitialize all data for this arm
            //println(s"Change detected at time $t at arm $x, history of this arm: ${historyarm(x).length}")
            tarms(x) = initializationvalue
            counts(x) = initializationvalue
            sums(x) = initializationvalue
            historyarm(x) = List[Double]()
            changedetected = true // flag that a change was detected
          }
      }
    }

    if(changedetected) { // there was at least one change!
      nepisodes += 1 // increase number of episodes
      changedetected = false // reset the change detected flag
    }

    val gain = gains.sum
    (arms, gains, gain)
  }

}
