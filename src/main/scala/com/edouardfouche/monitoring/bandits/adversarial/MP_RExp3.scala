package com.edouardfouche.monitoring.bandits.adversarial

import breeze.stats.distributions.Uniform
import com.edouardfouche.monitoring.bandits.Bandit
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.Random

/**
  * RExp3, as in Optimal Exploration-Exploitation in a Multi-Armed-Bandit Problem with Non-stationary Rewards (Besbes et al., 2014)
  *
  * @param stream a stream simulator on which we let this bandit run
  * @param reward the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k the initial number of pull per round
  *
  * In principle, tuning batchsize requires the knowledge of horizon and a variational budget V (impractical)
  * The variation budget is in [1/narms, horizon/arms]
  *
  */
case class MP_RExp3(batchsize: Double)(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends Bandit {
  val name: String = s"MP-RExp3; b=$batchsize"

  // Gamma is tuned as in (Besbes et al., 2014)
  val gamma = 1.0.min(math.sqrt((narms*math.log(narms))/((2.718281828459045-1)*batchsize)))

  // As the weights of RExp3 can grow very large, does it need its own weight array made of BigDecimal?
  var Exp3weights: Array[Double] = (0 until narms).map(x => 1.0).toArray // initialize weights to 1
  var batchindex: Int = 1
  var tau = (batchindex - 1)*batchsize

  override def reset: Unit = {
    super.reset
    Exp3weights = (0 until narms).map(x => 1.0).toArray // initialize weights to 1
  }

  def next: (Array[(Int, Int)], Array[Double], Double) = {
    if(t > tau+batchindex) {
      tau = (batchindex - 1)*batchsize
      Exp3weights = (0 until narms).map(x => 1.0).toArray // reset weights to 1
    }

    val p = (0 until narms).map(x => ((1-gamma)*Exp3weights(x))/Exp3weights.sum + (gamma/narms)).toArray

    @tailrec // This is a method to draw multiple arms without replacement from the probability distribution
    def draw(probs: Array[Double], possibleindexes: Array[Int], result: Array[Int], n:Int): Array[Int] = {
      @tailrec
      def cumulative(i: Int, acc: Double, x: Double): Int = {
        if((i >= probs.length-1) || (acc > x)) i
        else cumulative(i+1, acc + probs(i), x)
      }
      val x = Uniform(0,probs.sum).draw()
      val i = cumulative(0,0.0,x)
      if(n == 1) result :+ possibleindexes(i)
      else draw(probs.take(i) ++ probs.drop(i+1),
          possibleindexes.take(i) ++ possibleindexes.drop(i+1),
          result :+ possibleindexes(i), n-1)
    }

    val indexes = draw(p, p.indices.toArray, Array[Int](), k)
    val arms = indexes.map(combinations(_))

    val newValues = stream.nextAndCompute(indexes)
    if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)

    // Update the current Matrix and compute the diff at the same time
    val gains = (indexes zip newValues).map(x => {
      val d = reward.getReward(x._2, currentMatrix(x._1))
      currentMatrix(x._1) = x._2 // replace
      counts(x._1) += 1.0
      sums(x._1) += d
      d
    })

    t += 1
    k = scalingstrategy.scale(gains, indexes, sums, counts, t)

    // create the full reward vector (0 reward for the arm not drawn)
    val rewards: Array[Double] = (0 until narms).map(x => 0.0).toArray
    (gains zip indexes).foreach{
      x => rewards(x._2) = x._1
    }
    // update the weights
    (0 until narms).foreach{x =>
      Exp3weights(x) = Exp3weights(x)*math.exp(gamma*rewards(x)/narms)
    }

    (arms, gains, gains.sum)
  }
}
