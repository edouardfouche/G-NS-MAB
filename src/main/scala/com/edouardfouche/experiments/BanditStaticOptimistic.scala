/*
 * Copyright (C) 2018 Edouard Fouché
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
package com.edouardfouche.experiments

import breeze.linalg
import breeze.stats.distributions.{RandBasis, ThreadLocalRandomGenerator}
import com.edouardfouche.monitoring.bandits.adversarial._
import com.edouardfouche.monitoring.bandits.nonstationary.MP_AW_TS
import com.edouardfouche.monitoring.bandits.oracles.OracleStatic
import com.edouardfouche.monitoring.bandits.stationary._
import com.edouardfouche.monitoring.scalingstrategies._
import com.edouardfouche.preprocess._
import com.edouardfouche.streamsimulator.CachedStreamSimulator
import org.apache.commons.math3.random.MersenneTwister

/**
  * Created by fouchee on 12.07.17.
  * This experiment compares the behavior of various bandits in the static setting
  */
object BanditStaticOptimistic extends BanditSyntheticExperiment {
  override val attributes = List("bandit","optimistic","dataset","scalingstrategy","k","gain","cputime", "iteration")
  val d = 100
  val lmin = 1
  val lmax = d

  val generator= StaticGenerator(d)

  val scalingstrategies: Array[ScalingStrategy] = Array(
    KLBasedScaling(lmin, lmax, 0.9),
    //KLBasedScaling(lmin, lmax, 0.8),
    KLBasedScaling(lmin, lmax, 0.7))
    //KLBasedScaling(lmin, lmax, 0.6))

  val nRep = 100

  override val banditConstructors = Vector(
    //OracleDynamic,
    OracleStatic,
    //OracleRandom,
    CUCB, CUCBm,
    MPKLUCB, MPKLUCBPLUS,
    Exp3M,
    MPTS, IMPTS, MPOTS,
    MP_AW_TS(0.1)(_,_,_,_)
    /*
    MP_D_TS(0.7)(_,_,_,_), MP_D_TS(0.8)(_,_,_,_), MP_D_TS(0.9)(_,_,_,_), MP_D_TS(0.99)(_,_,_,_),
    MP_E_Greedy(0.7)(_, _, _, _), MP_E_Greedy(0.8)(_, _, _, _), MP_E_Greedy(0.9)(_, _, _, _), MP_E_Greedy(0.99)(_, _, _, _),
    MP_SW_UCB(50)(_, _, _, _), MP_SW_UCB(100)(_, _, _, _), MP_SW_UCB(500)(_, _, _, _), MP_SW_UCB(1000)(_, _, _, _),

    OracleStatic_ADWIN(0.1)(_,_,_,_), OracleDynamic_ADWIN(0.1)(_,_,_,_), OracleSequential_ADWIN(0.1)(_,_,_,_),

    MP_AW_UCB(0.1)(_,_,_,_),
    MP_AW_KL_UCB(0.1)(_,_,_,_),
    OracleRandom_ADWIN(0.1)(_,_,_,_),
    Exp3Mauto_ADWIN(0.1)(_,_,_,_),
    MP_AW_TS(0.1)(_,_,_,_),

    MP_AW_TS(0.5)(_,_,_,_),
    MP_AW_TS(0.3)(_,_,_,_),
    MP_AW_TS(1)(_,_,_,_),
    MP_AW_TS(0.01)(_,_,_,_),
    MP_AW_TS(0.001)(_,_,_,_)
    */
  )

  /*
  override val banditConstructors = Vector(
    CUCB, //CUCBm,
    MPKLUCB,
    Exp3Mauto,
    MPTS,// IMPTS, MPOTS,
    MP_D_TS(0.8)(_,_,_,_), MP_D_TS(0.9)(_,_,_,_), MP_D_TS(0.99)(_,_,_,_),
    MP_E_Greedy(0.8)(_, _, _, _), MP_E_Greedy(0.9)(_, _, _, _), MP_E_Greedy(0.99)(_, _, _, _),
    MP_SW_UCB(100)(_, _, _, _), MP_SW_UCB(1000)(_, _, _, _), MP_SW_UCB(10000)(_, _, _, _),

    MP_AW_UCB(0.1)(_,_,_,_),
    MP_AW_KL_UCB(0.1)(_,_,_,_),
    Exp3Mauto_ADWIN(0.1)(_,_,_,_),
    MP_AW_TS(0.1)(_,_,_,_)
  )
  */

  override def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting com.edouardfouche.experiments - ${this.getClass.getSimpleName}")
    // display parameters
    info(s"Parameters:")
    info(s"lmin:$lmin, lmax: $lmax")
    info(s"d:$d")
    info(s"generator: ${generator.id}")
    info(s"scalingstrategies: ${scalingstrategies.map(_.name) mkString ","}")
    info(s"reward: ${reward.name}")
    info(s"nRep: ${nRep}")

    val id= generator.id

    info(s"Computing simulators...")
    val simulators = (0 until nRep).par.map{x =>
      val rand = new RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister(x)))
      CachedStreamSimulator(InternalDataRef(id, generator.generate(rand), "cache"))
    }.toArray


    for {
      scalingstrategy <- scalingstrategies.par
    } {
      // Pessimistic phase
      for{
        banditConstructor <- banditConstructors.zipWithIndex.par
      } {
        var allgains: linalg.Vector[Double] = linalg.Vector((1 to simulators(0).nbatches).map(x => 0.0).toArray)
        var allks: linalg.Vector[Double] = linalg.Vector((1 to simulators(0).nbatches).map(x => 0.0).toArray)
        var allcpu: linalg.Vector[Double] = linalg.Vector((1 to simulators(0).nbatches).map(x => 0.0).toArray)

        for {
          rep <- 0 until nRep
        } {
          val bandit = banditConstructor._1(simulators(rep).copy(), reward, scalingstrategy, lmax)
          bandit.disable_optimistic
          val (gains, ks, cpu) = fullrunnerGainsKsCPU(bandit, Array[Double](), Array[Int](), Array[Double]())
          if (rep % 10 == 0) info(s"Reached rep $rep with bandit ${bandit.name}, ${scalingstrategy.name}")
          allgains = allgains +:+ (breeze.linalg.Vector(gains) *:* (1.0 / nRep))
          allks = allks +:+ (breeze.linalg.Vector(ks.map(_.toDouble)) *:* (1.0 / nRep))
          allcpu = allcpu +:+ (breeze.linalg.Vector(cpu.map(_.toDouble)) *:* (1.0 / nRep))
        }

        val bandit = banditConstructor._1(simulators(0), reward, scalingstrategy, lmax)
        for{
          step <- 0 until allgains.length
        }{
          val summary = ExperimentSummary(attributes)
          // this is the list of all the data possible we can record, "attributes" is usually a subset of it
          // val attributes = List("bandit","dataset","action","reward","scaling","windowSize","stepSize",
          // "delta","gamma","k","banditk","narms","gain","matrixdiff","cpuTime","wallTime","iteration","nrep")
          summary.add("bandit", bandit.name)
          summary.add("optimistic", false)
          summary.add("dataset", bandit.stream.dataset.id)
          summary.add("scalingstrategy", bandit.scalingstrategy.name)
          summary.add("k",  "%.2f".format(allks(step)))
          summary.add("gain",  "%.2f".format(allgains(step)))
          //summary.add("confidence", allconfs(step))
          //summary.add("gamma", bandit.scalingstrategy.gamma)
          //summary.add("delta", bandit.scalingstrategy.delta)
          summary.add("cputime", "%.4f".format(allcpu(step)))
          summary.add("iteration", step)
          summary.write(summaryPath)
        }
      }

      // Optimistic phase
      for{
        banditConstructor <- banditConstructors.zipWithIndex.par
      } {
        var allgains: linalg.Vector[Double] = linalg.Vector((1 to simulators(0).nbatches).map(x => 0.0).toArray)
        var allks: linalg.Vector[Double] = linalg.Vector((1 to simulators(0).nbatches).map(x => 0.0).toArray)
        var allcpu: linalg.Vector[Double] = linalg.Vector((1 to simulators(0).nbatches).map(x => 0.0).toArray)

        for {
          rep <- 0 until nRep
        } {
          val bandit = banditConstructor._1(simulators(rep).copy(), reward, scalingstrategy, lmax)
          val (gains, ks, cpu) = fullrunnerGainsKsCPU(bandit, Array[Double](), Array[Int](), Array[Double]())
          if (rep % 10 == 0) info(s"Reached rep $rep with bandit ${bandit.name}, ${scalingstrategy.name}")
          allgains = allgains +:+ (breeze.linalg.Vector(gains) *:* (1.0 / nRep))
          allks = allks +:+ (breeze.linalg.Vector(ks.map(_.toDouble)) *:* (1.0 / nRep))
          allcpu = allcpu +:+ (breeze.linalg.Vector(cpu.map(_.toDouble)) *:* (1.0 / nRep))
        }

        val bandit = banditConstructor._1(simulators(0), reward, scalingstrategy, lmax)
        for{
          step <- 0 until allgains.length
        }{
          val summary = ExperimentSummary(attributes)
          // this is the list of all the data possible we can record, "attributes" is usually a subset of it
          // val attributes = List("bandit","dataset","action","reward","scaling","windowSize","stepSize",
          // "delta","gamma","k","banditk","narms","gain","matrixdiff","cpuTime","wallTime","iteration","nrep")
          summary.add("bandit", bandit.name)
          summary.add("optimistic", true)
          summary.add("dataset", bandit.stream.dataset.id)
          summary.add("scalingstrategy", bandit.scalingstrategy.name)
          summary.add("k",  "%.2f".format(allks(step)))
          summary.add("gain",  "%.2f".format(allgains(step)))
          //summary.add("confidence", allconfs(step))
          //summary.add("gamma", bandit.scalingstrategy.gamma)
          //summary.add("delta", bandit.scalingstrategy.delta)
          summary.add("cputime", "%.4f".format(allcpu(step)))
          summary.add("iteration", step)
          summary.write(summaryPath)
        }
      }
    }

    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}