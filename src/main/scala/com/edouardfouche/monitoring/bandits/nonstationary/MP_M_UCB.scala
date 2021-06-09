package com.edouardfouche.monitoring.bandits.nonstationary

import breeze.stats.distributions.Gaussian
import com.edouardfouche.monitoring.bandits.BanditUCB
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * M-UCB
  *
  * @param windowsize size of the sliding window (must be even)
  * @param nchanges Number of changes (M in the paper)
  * @param stream a stream simulator on which we let this bandit run
  * @param reward the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k the initial number of pull per round
  *
  */
case class MP_M_UCB(windowsize: Int, nchanges: Int)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditUCB {
  require(windowsize > 1)
  require(windowsize % 2 == 0)
  require(nchanges >= 0)

  override val logfactor = 2
  val name = s"MP-M-UCB; w=$windowsize; m=$nchanges"

  //var historyarm: Array[List[Double]] = (0 until narms).map(_ => List[Double]()).toArray
  var cumulative_history: scala.collection.mutable.Map[Int,Array[(Int,Double)]] =
    collection.mutable.Map((0 until narms).map(x => x -> Array[(Int,Double)]()).toMap.toSeq: _*)

  val horizon: Int = stream.nbatches
  val b = math.sqrt((windowsize/2)*math.log(2*narms*math.pow(horizon,2)))
  val gamma = if(nchanges == 0) 1/horizon else math.sqrt((nchanges-1)*narms*(2*b+3*math.sqrt(windowsize))/(2*horizon)).max(1/horizon).min(1)
  var tau = t
  var changedetected: Boolean = false // Just to flag whether there was a change in the iteration or not

  override def reset: Unit = {
    super.reset
    //historyarm = (0 until narms).map(_ => List[Double]()).toArray
    var cumulative_history: scala.collection.mutable.Map[Int,Array[(Int,Double)]] =
      collection.mutable.Map((0 until narms).map(x => x -> Array[(Int,Double)]()).toMap.toSeq: _*)
  }

  // return a vector a 2-tuples (arms) and a gain
  def next: (Array[(Int, Int)], Array[Double], Double) = {
    val forcedexploration: Int = (t-tau).toInt % math.floor(narms/gamma).toInt

    val confidences = counts.map(x =>
      if(t==0.0 | x == 0.0) (0+Gaussian(0, 1).draw()*0.000001).max(0)
      else (math.sqrt((logfactor*math.log(t-tau))/x)+Gaussian(0, 1).draw()*0.000001).max(0))

    val upperconfidences = sums.zip(counts).zip(confidences).map(x => (x._1._1/x._1._2)+ x._2)//.min(1.0))
    val sortedupperconfidences = upperconfidences.zipWithIndex.sortBy(-_._1).map(_._2)

    val indexes: Array[Int] =  if(forcedexploration < narms-1) {
      Array(forcedexploration) ++ sortedupperconfidences.filter(_ != forcedexploration).take(k-1)
    } else {
      sortedupperconfidences.take(k)
    }

    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)

    // If true then the bandit has finished, stream is exhausted
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)
    // If some gain is negative then "discard" the round by assigning -1 reward
    val gainscheck: Array[Double] = (indexes zip newValues).map(x => reward.getReward(x._2, currentMatrix(x._1)))
    if(gainscheck.exists(x => x < 0)) return (arms, gainscheck, -1)

    // Update the current Matrix, compute the gains and update the weights at the same time
    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2 // replace
      counts(x._1) += 1.0
      sums(x._1) += d
      //historyarm(x._1) = historyarm(x._1) :+ d // Keep history of rewards for each arm
      val lastelement: (Int, Double) = if(cumulative_history(x._1).isEmpty) (0,0.0) else cumulative_history(x._1).last
      cumulative_history(x._1) = cumulative_history(x._1) :+ (lastelement._1 + 1, lastelement._2 + d)
      d
    })
    t = t + 1

    k = scalingstrategy.scale(gains, indexes, sums, counts, t)

    (0 until narms).foreach { x =>
      if(counts(x) > windowsize) {
        val w1 = cumulative_history(x).last._2 - cumulative_history(x)(cumulative_history(x).length-(windowsize/2)-1)._2
        val w2 = cumulative_history(x)(cumulative_history(x).length-(windowsize/2)-1)._2 - cumulative_history(x)(cumulative_history(x).length-windowsize-1)._2
        //val w1 = historyarm(x).slice(historyarm(x).length-(windowsize/2+1)+1,historyarm(x).length).sum
        //val w2 = historyarm(x).slice(historyarm(x).length-windowsize,historyarm(x).length-(windowsize/2+1)+1).sum
        if(math.abs(w1-w2) > b) { // Detect a change
          changedetected = true
          //println(s"Change detected at time $t on arm $x")
        }
      }
    }

    // Definitelety global changes
    if(changedetected) { // there was at least one change!
      tau = t
      changedetected = false // reset the change detected flag
      (0 until narms).foreach { x => // reinitialize all the arms
        counts(x) = initializationvalue
        sums(x) = initializationvalue
        //historyarm(x) = List[Double]()
        cumulative_history(x) = Array[(Int, Double)]() //reset entire memory for this arm
      }
    }

    val gain = gains.sum
    (arms, gains, gain)
  }

}
