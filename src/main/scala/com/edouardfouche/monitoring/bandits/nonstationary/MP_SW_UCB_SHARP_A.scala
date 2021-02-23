package com.edouardfouche.monitoring.bandits.nonstationary

import breeze.stats.distributions.Gaussian
import com.edouardfouche.monitoring.bandits.BanditUCB
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

import scala.collection.mutable.ArrayBuffer

/**
  * Sliding-Window UCB # (SW-UCB#) with Multiple Plays -- For abrupt environment
  * The idea of SW-UCB comes from
  *
  * @param nu a value in [0,1) that quantifies how much the stream changes in abrupt environment
  * @param lambda a real value that quantifies how much the stream changes
  * @param stream a stream simulator on which we let this bandit run
  * @param reward the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k the initial number of pull per round
  *
  * The authors selected lambda = 12.3 and lambda = 4.3 for abruptly-changing and slowly-varying environments, respectively
  */
case class MP_SW_UCB_SHARP_A(nu: Double, lambda: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditUCB {
  val alpha: Double = (1.0-nu)/2.0
  var windowsize: Double = t.min(lambda * math.pow(t,alpha)).max(1.0)

  override val logfactor: Double = (1+alpha) // as in the paper

  val name = s"MP-SW-UCB#-A; nu=$nu; l=$lambda"

  var history: List[scala.collection.mutable.Map[Int,Double]] = List() // first el in the update for count, and last in the update for weight

  override def reset: Unit = {
    super.reset
    history = List()
  }

  // return a vector a 2-tuples (arms) and a gain
  def next: (Array[(Int, Int)], Array[Double], Double) = {
    windowsize = t.min(lambda * math.pow(t,alpha)).max(1) // compute the new windowsize

    val confidences = counts.map(x =>
      if(t==0.0 | x == 0.0) (0+Gaussian(0, 1).draw()*0.000001).max(0)
      else (math.sqrt((logfactor*math.log(windowsize))/x)+Gaussian(0, 1).draw()*0.000001).max(0))

    val upperconfidences = sums.zip(counts).zip(confidences).map(x => (x._1._1/x._1._2)+ x._2)//.min(1.0))

    val sortedupperconfidences = upperconfidences.zipWithIndex.sortBy(-_._1).map(_._2)

    val indexes: Array[Int] = if((k==1) & (t <= narms)) { // The first forced exploration round
      Array(t.toInt-1)
    } else {
      if(t <= narms) sortedupperconfidences.filter(_ != t-1).take(k-1) ++ Array(t.toInt-1)
      else sortedupperconfidences.take(k)
    }

    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    val updates = scala.collection.mutable.Map[Int, Double]()

    // Update the current Matrix, compute the gains and update the weights at the same time
    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2 // replace

      counts(x._1) += 1.0
      sums(x._1) += d
      updates(x._1) = d
      d
    })
    history = history :+ updates
    t = t + 1

    k = scalingstrategy.scale(gains, indexes, sums, counts, t.min(windowsize))

    // Rolling back
    if(windowsize < history.length) {
      for{
        x <- windowsize.toInt until history.length
      } {
        val rollback = history.head
        history = history.tail
        for((key,value) <- rollback) {
          sums(key) = sums(key) - value // if(counts(key) == 1.0) 1.0 else weights(key) - (1.0/(counts(key)-1.0))*(value._1 - weights(key))
          counts(key) = counts(key) - 1 //- value._2
        }
      }
    }

    val gain = gains.sum
    (arms, gains, gain)
  }

}
