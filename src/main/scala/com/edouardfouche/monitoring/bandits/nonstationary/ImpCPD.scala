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

import com.edouardfouche.monitoring.bandits.BanditUCB
import com.edouardfouche.monitoring.rewards.Reward
import com.edouardfouche.monitoring.scalingstrategies.ScalingStrategy
import com.edouardfouche.streamsimulator.Simulator

/**
  * ImpCPD (Improved Changepoint Detector), as in "Distribution-dependent and Time-uniform Bounds for Piecewise i.i.d Bandits" (Mukherjee & Maillard, 2019)
  *
  * @param stream          a stream simulator on which we let this bandit run
  * @param reward          the reward function which derives the gains for each action
  * @param scalingstrategy the scaling strategy, which decides how many arms to pull for the next step
  * @param k               the initial number of pull per round
  *
  *                        Note: The horizon must be known for this method.
  *                        Note: It makes the assumption of global abrupt change points, no forced exploration
  */
case class ImpCPD(val stream: Simulator, val reward: Reward, val scalingstrategy: ScalingStrategy, var k: Int) extends BanditUCB {
  override val initializationvalue = 0.0 // Does not do optimistic optimization but pull each arm once instead.
  //override val logfactor = 2
  val name = s"ImpCPD" //TODO: This is the implementation that is closest to my first guess
  val gamma = 0.05 // Set parameter as in Corollary 1. This parameter controls how often the changepoint detection routine is called
  var horizon: Int = stream.nbatches
  var B: Set[Int] = (0 until narms).toSet
  var m: Int = 0
  var epsilon: Double = 1.0
  var phi: Double = math.pow(horizon, 2) / (math.pow(narms, 2) * math.log(narms)) //TODO: Reference code does not adapt horizon, OK
  var alpha: Double = 3.0 / 2.0
  var M: Int = math.floor((1.0 / 2.0) * math.log(horizon / epsilon) / math.log(1 + gamma)).toInt // TODO Not sure if e = epsilon or euler's constant
  var l: Int = math.ceil(math.log(phi * math.pow(epsilon, 2)) / (2 * epsilon)).toInt
  var L: Int = B.size * l
  var tp = 1
  t = 1
  (0 until narms).foreach(x => sums(x) = 0.0)
  (0 until narms).foreach(x => counts(x) = 0.0)
  var cumulative_history: scala.collection.mutable.Map[Int, Array[(Int, Double)]] =
    collection.mutable.Map((0 until narms).map(x => x -> Array[(Int, Double)]()).toMap.toSeq: _*)

  //var epsilon_L_history: Array[(Double, Int)] = Array[(Double,Int)]((epsilon, L))
  var epsilon_L_history: scala.collection.mutable.Map[Int, Array[(Double, Int)]] =
    collection.mutable.Map((0 until narms).map(x => x -> Array[(Double, Int)]()).toMap.toSeq: _*)

  //var changedetected: Boolean = false // Just to flag whether there was a change in the iteration or not

  override def reset: Unit = {
    super.reset
    var cumulative_history: scala.collection.mutable.Map[Int, Array[(Int, Double)]] =
      collection.mutable.Map((0 until narms).map(x => x -> Array[(Int, Double)]()).toMap.toSeq: _*)
    t = 1
    tp = 1
    (0 until narms).foreach(x => sums(x) = 0.0)
    (0 until narms).foreach(x => counts(x) = 0.0)
  }

  //println(s"Initializing...")
  //println(s"We have B.size =${B.size}, M=$M, m=$m, l=$l, L=$L, t=$t")

  // return a vector a 2-tuples (arms) and a gain
  def next: (Array[(Int, Int)], Array[Double], Double) = {
    // The Algorithm 3 does not say whether they pull each arm once, but I assume it does
    val (arms, gains) = if (t < narms + 1) { // Pull each arm once
      val indexes: Array[Int] = Array(((t - 1) % narms).toInt)
      val arms = indexes.map(combinations(_))
      val newValues = stream.nextAndCompute(indexes)

      // If true then the bandit has finished, stream is exhausted
      if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)
      // If some gain is negative then "discard" the round by assigning -1 reward
      val gainscheck: Array[Double] = (indexes zip newValues).map(x => reward.getReward(x._2, currentMatrix(x._1)))
      if (gainscheck.exists(x => x < 0)) return (arms, gainscheck, -1)

      // Update the current Matrix, compute the gains and update the weights at the same time
      val gains = (indexes zip newValues).map(x => {
        val d = reward.getReward(x._2, currentMatrix(x._1))
        currentMatrix(x._1) = x._2 // replace
        counts(x._1) += 1.0
        sums(x._1) += d
        val lastelement: (Int, Double) = if (cumulative_history(x._1).isEmpty) (0, 0.0) else cumulative_history(x._1).last
        cumulative_history(x._1) = cumulative_history(x._1) :+ (lastelement._1 + 1, lastelement._2 + d)
        epsilon_L_history(x._1) = epsilon_L_history(x._1) :+ (epsilon, L)
        d
      })
      // Repeat the last known value for the arms we did not play
      //(0 until narms).toSet.diff(indexes.toSet).foreach {x =>
      //  val lastelement: (Int, Double) = if (cumulative_history(x).isEmpty) (0, 0.0) else cumulative_history(x).last
      //  cumulative_history(x) = cumulative_history(x) :+ (lastelement._1, lastelement._2)
      //}
      t = t + 1
      tp = tp + 1

      k = scalingstrategy.scale(gains, indexes, sums, counts, t)
      (arms, gains)
    } else {
      //if(t == narms+1) {
      //  println(s"Played all arms...")
      //  println(s"We have B.size =${B.size}, M=$M, m=$m, l=$l, L=$L, t=$t")
      //}
      val confidences = counts.map(x => math.sqrt((alpha * math.log(phi * math.pow(epsilon, 2))) / (2 * x))) // As line 3, Alg 3
      val upperconfidences = sums.zip(counts).zip(confidences).map(x => (x._1._1 / x._1._2) + x._2) //.min(1.0))
      val sortedupperconfidences = upperconfidences.zipWithIndex.sortBy(-_._1).map(_._2)
      val indexes: Array[Int] = sortedupperconfidences.take(1)
      val arms = indexes.map(combinations(_))
      val newValues = stream.nextAndCompute(indexes)

      // If true then the bandit has finished, stream is exhausted
      if (newValues.isEmpty) return (Array[(Int, Int)](), Array[Double](), 0)
      // If some gain is negative then "discard" the round by assigning -1 reward
      val gainscheck: Array[Double] = (indexes zip newValues).map(x => reward.getReward(x._2, currentMatrix(x._1)))
      if (gainscheck.exists(x => x < 0)) return (arms, gainscheck, -1)

      // Update the current Matrix, compute the gains and update the weights at the same time
      val gains = (indexes zip newValues).map(x => {
        val d = reward.getReward(x._2, currentMatrix(x._1))
        currentMatrix(x._1) = x._2 // replace
        counts(x._1) += 1.0
        sums(x._1) += d
        val lastelement: (Int, Double) = if (cumulative_history(x._1).isEmpty) (0, 0.0) else cumulative_history(x._1).last
        cumulative_history(x._1) = cumulative_history(x._1) :+ (lastelement._1 + 1, lastelement._2 + d)
        epsilon_L_history(x._1) = epsilon_L_history(x._1) :+ (epsilon, L)
        d
      })
      // Repeat the last known value for the arms we did not play
      // (0 until narms).toSet.diff(indexes.toSet).foreach { x =>
      //  val lastelement: (Int, Double) = if (cumulative_history(x).isEmpty) (0, 0.0) else cumulative_history(x).last
      //  cumulative_history(x) = cumulative_history(x) :+ (lastelement._1, lastelement._2)
      //}
      t = t + 1
      tp = tp + 1

      k = scalingstrategy.scale(gains, indexes, sums, counts, t)
      (arms, gains)
    }

    // Change detection time
    // TODO: The version that works best: Adapting with epsilonprime, adapting phi for Bsize, not adapting horizon, not distinguishing two epsilons
    // TODO: Results on global bandit is OK but a little bit approximate, I am not sure that the confidence interval are correct.
    // TODO: When distinguishing the epsilons (but not adapting horizon), results are a little bit worse (more delay) but more consistent somehow.
    def ChangePointDetectionImproved: Boolean = {
      (0 until narms).foreach { x =>
        //(1 to m).foreach{mprime =>
        if (cumulative_history(x).length >= 60) { // Making sure we have enough observation for that arm before we try to detect changes
          cumulative_history(x).indices.slice(15, cumulative_history(x).length - 15).filter(x => x % 10 == 0).foreach { y => // Avoid extremities and only use the multilples of 10
            //val (epsilonprime: Double, lprime: Int) = epsilon_L_history(x)(y)
            //val (lepsilonprime: Double, llprime: Int) = epsilon_L_history(x)(y) //TODO: Maybe epsilon prime should be different on left and right. Code: No
            //val (repsilonprime: Double, rlprime: Int) = epsilon_L_history(x)(cumulative_history(x).length-1-y)
            //val le = cumulative_history(x)((lprime.toInt-1).max(0)) // ArrayIndexOutOfBound avoided
            val le = cumulative_history(x)(y)
            val ri = (cumulative_history(x).last._1 - le._1, cumulative_history(x).last._2 - le._2)
            val lconf = math.sqrt((alpha * math.log(phi * math.pow(epsilon, 2))) / (2 * le._1)) // TODO: Unsure whether they actually use epsilonprime or epsilon. Code: Epsilon
            val rconf = math.sqrt((alpha * math.log(phi * math.pow(epsilon, 2))) / (2 * ri._1)) // TODO: Unsure whether they actually use epsilonprime or epsilon. Code: Epsilon
            if ((le._2 / le._1 + lconf) < (ri._2 / ri._1 - rconf)) {
              //println(Console.BLUE + s"Option1: ImpCPD Change detected at time $t ($tp) on arm $x at $y over ${cumulative_history(x).length}, $m <= $M")
              //println(s"We have B.size =${B.size}, M=$M, m=$m, l=$l, L=$L, t=$t, y=$y")
              //println(s"alpha: $alpha, phi: $phi, epsilon: $epsilon, horizon: $horizon ")
              //println {s"Means are ${le._2 / le._1} + $lconf < ${ri._2 / ri._1} - $rconf"}
              val head: Array[Double] = cumulative_history(x).map(_._2).init
              val tail: Array[Double] = cumulative_history(x).tail.map(_._2)
              //println {s"History: ${head.zip(tail).map(x => x._2 - x._1).mkString("Array(", ", ", ")")}"}
              return true
            }
            if ((le._2 / le._1 - lconf) > (ri._2 / ri._1 + rconf)) { //TODO: So far mostly option 2 gets triggered (why?) Maybe that depends on the change
              //println(Console.RED +s"Option2: ImpCPD Change detected at time $t ($tp) on arm $x at $y over ${cumulative_history(x).length}, $m <= $M")
              //println(s"We have B.size =${B.size}, M=$M, m=$m, l=$l, L=$L, t=$t, y=$y")
              //println(s"alpha: $alpha, phi: $phi, epsilon: $epsilon, horizon: $horizon ")
              //println {s"Means are ${le._2 / le._1} - $lconf > ${ri._2 / ri._1} + $rconf"}
              val head: Array[Double] = cumulative_history(x).map(_._2).init
              val tail: Array[Double] = cumulative_history(x).tail.map(_._2)
              //println {s"History: ${head.zip(tail).map(x => x._2 - x._1).mkString("Array(", ", ", ")")}"}
              return true
            }
            //if(x == 0) {
            //  println(s"We have B.size =${B.size}, M=$M, m=$m, l=$l, L=$L, t=$t, y=$y")
            //  println(s"alpha: $alpha, phi: $phi, epsilon: $epsilon, horizon: $horizon ")
            //  println {
            //    s"Means are ${le._2 / le._1} + $lconf < ${ri._2 / ri._1} - $rconf"
            //  }
            //}
          }
        }
      }
      false
    }

    // && (m <= M) //TODO: Removing this clause because somehow if no change is detected after a certain number of times, we will stop trying
    if ((t > L)) { // (t > L)  instead of (t >= L) for avoiding some array out of bound problem
      // println(s"ImpCPD: $t > $L and $m <= $M : Yes")
      if (ChangePointDetectionImproved) { // Restart
        (0 until narms).foreach { x => // reinitialize all the arms
          sums(x) = 0.0
          counts(x) = 0.0
          cumulative_history(x) = Array[(Int, Double)]() //reset entire memory for this arm
        }
        //horizon = horizon - (t.toInt-1) // TODO: Not adapting horizon
        t = 1 // They actually never reset t in the original implementation, which is weird a bit. Let's not reset tp
        B = (0 until narms).toSet
        m = 0 // Because we reset m, I assume it follows that we reset epsilon, M, l, L,
        // One question is maybe to reset the remaining horizon.
        epsilon = 1 //TODO: Maybe we don't reset epsilon. Code: Yes we do
        M = math.floor((1.0 / 2.0) * math.log(horizon / epsilon) / math.log(1 + gamma)).toInt // Not sure if e = epsilon
        l = math.ceil(math.log(phi * math.pow(epsilon, 2)) / (2 * epsilon)).toInt
        L = B.size * l
        phi = math.pow(horizon, 2) / (math.pow(narms, 2) * math.log(narms)) //TODO: Is adapting phi OK? Code: No, adapting actually it seems.
        epsilon_L_history = collection.mutable.Map((0 until narms).map(x => x -> Array[(Double, Int)]()).toMap.toSeq: _*)
        //println(s"Resetting...")
        //println(s"We have B.size =${B.size}, M=$M, m=$m, l=$l, L=$L, t=$t")
      } else {
        val confidences = counts.map(x => math.sqrt((alpha * math.log(phi * math.pow(epsilon, 2))) / (2 * x))) // As line 3, Alg 3
        val maxLCB: Double = sums.zip(counts).zip(confidences).map(x => (x._1._1 / x._1._2) - x._2).max
        val minUCB: Double = sums.zip(counts).zip(confidences).map(x => (x._1._1 / x._1._2) + x._2).min
        // println(Console.YELLOW + s"maxLCB: $maxLCB, minUCB: $minUCB, horizon: $horizon, epsilon: $epsilon")
        val oldbsize = B.size
        (0 until narms).foreach { x =>
          val ucb: Double = (sums(x) / counts(x)) + confidences(x)
          if (ucb < maxLCB) {
            B = B.diff(Set(x))
          }
        }
        // if(B.size < oldbsize) println(Console.YELLOW + s"some arms removed, B.size is now ${B.size} from $oldbsize")
        epsilon = (epsilon / (1 + gamma)).max(1 / math.pow((1 + gamma), M))
        M = math.floor((1.0 / 2.0) * math.log10(horizon / 2.718281828459045) / math.log(1 + gamma)).toInt // Not sure if e = epsilon //TODO: Code: Modifying slightly (math.e, log10)
        l = math.ceil(math.log(phi * math.pow(epsilon, 2)) / (2 * epsilon)).toInt
        L = B.size * l
        phi = math.pow(horizon, 2) / (math.pow(B.size, 2) * math.log(B.size)) //TODO: Is adapting phi OK? Code: It seems not (no adapting then) EDIT: Adapting anyway now
        m = m + 1
        //epsilon_L_history = epsilon_L_history :+ (epsilon, L)
      }
    } //else {if(t%100 == 0)  println(s"ImpCPD: $t > $L and $m <= $M : No")}

    val gain = gains.sum
    (arms, gains, gain)
  }
}
