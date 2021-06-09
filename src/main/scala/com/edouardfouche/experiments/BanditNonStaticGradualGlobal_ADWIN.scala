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
import com.edouardfouche.monitoring.bandits.nonstationary._
import com.edouardfouche.monitoring.bandits.oracles.{OracleAbruptGlobal, OracleDynamic, OracleGradualGlobal, OracleRandom, OracleStatic}
import com.edouardfouche.monitoring.rewards.AbsoluteThreshold
import com.edouardfouche.monitoring.scalingstrategies._
import com.edouardfouche.preprocess._
import com.edouardfouche.streamsimulator.CachedStreamSimulator
import org.apache.commons.math3.random.MersenneTwister

/**
  * Created by fouchee on 12.07.17.
  * This experiment compares the behavior of various bandits in the face of a "shutdown" change (see ShutdownGenerator)
  */
object BanditNonStaticGradualGlobal_ADWIN extends BanditExperiment {
  val d = 100
  val lmin = 1
  val lmax = d

  val attributes = List("bandit","dataset","scalingstrategy","k","gain","cputime", "historylength", "iteration")
  val reward = AbsoluteThreshold(1)
  val generator = GradualGlobalGenerator(d)
  val nRep = 5

  val scalingstrategies: Array[ScalingStrategy] = Array(
    //NoScaling(10),
    NoScaling(5),
    NoScaling(2),
    NoScaling(1)
  )

  val banditConstructors = Vector(
    OracleDynamic,
    OracleStatic,
    OracleRandom,
    OracleGradualGlobal,
    // Ours
    MP_ADS_TS_ADWIN1(0.1)(_,_,_,_),
    MP_ADS_TS_ADWIN1(0.01)(_,_,_,_),
    MP_ADS_TS_ADWIN1(0.001)(_,_,_,_),
    MP_ADS_TS_ADWIN1(0.0001)(_,_,_,_),
    MP_ADS_TS_ADWIN1(0.00001)(_,_,_,_),
    MP_ADS_TS_ADWIN1(0.1,ADR=true)(_,_,_,_),
    MP_ADS_TS_ADWIN1(0.01,ADR=true)(_,_,_,_),
    MP_ADS_TS_ADWIN1(0.001,ADR=true)(_,_,_,_),
    MP_ADS_TS_ADWIN1(0.0001,ADR=true)(_,_,_,_),
    MP_ADS_TS_ADWIN1(0.00001,ADR=true)(_,_,_,_),
    MP_ADR_Elimination_UCB(0.1)(_,_,_,_),
    MP_ADR_Elimination_UCB(0.01)(_,_,_,_),
    MP_ADR_Elimination_UCB(0.001)(_,_,_,_),
    MP_ADR_Elimination_UCB(0.0001)(_,_,_,_),
    MP_ADR_Elimination_UCB(0.00001)(_,_,_,_),
  )

  override def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting com.edouardfouche.experiments - ${this.getClass.getSimpleName}")
    // display parameters
    info(s"Parameters:")
    info(s"lmin:$lmin, lmax: $lmax")
    info(s"d:$d")
    //info(s"generator: ${generator.id}")
    info(s"scalingstrategies: ${scalingstrategies.map(_.name) mkString ","}")
    //info(s"reward: ${reward.name}")
    info(s"nRep: ${nRep}")

    val id= generator.id

    info(s"Computing simulator for... $generator")
    val simulators = (0 until nRep).par.map{x =>
      val rand = new RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister(x)))
      CachedStreamSimulator(InternalDataRef(id, generator.generate(rand), "cache"))
    }.toArray

    for {
      scalingstrategy <- scalingstrategies.par
    } {
      for{
        banditConstructor <- banditConstructors.zipWithIndex.par
      } {
        var allgains: linalg.Vector[Double] = linalg.Vector((1 to simulators(0).nbatches).map(x => 0.0).toArray)
        var allks: linalg.Vector[Double] = linalg.Vector((1 to simulators(0).nbatches).map(x => 0.0).toArray)
        var allcpu: linalg.Vector[Double] = linalg.Vector((1 to simulators(0).nbatches).map(x => 0.0).toArray)
        var allhistorylengths: linalg.Vector[Double] = linalg.Vector((1 to simulators(0).nbatches).map(x => 0.0).toArray)

        for {
          rep <- 0 until nRep
        } {
          //info(s"Starting com.edouardfouche.experiments with data: ${d.id}, configuration k: ${kratio}, rep=$rep")
          //val bandit = banditConstructor(streamsimulator.copy(), reward, scalingstrategy, scalingstrategy.k)
          val bandit = banditConstructor._1(simulators(rep).copy(), reward, scalingstrategy, lmax)
          if (rep % 1 == 0) info(s"Reached rep $rep with bandit ${bandit.name}, ${scalingstrategy.name}")
          val (gains, ks, cpu, historylengths) = fullrunnerGainsKsCPUW(bandit, Array[Double](), Array[Int](), Array[Double](), Array[Double]())
          allgains = allgains +:+ (breeze.linalg.Vector(gains) *:* (1.0/nRep))
          allks = allks +:+ (breeze.linalg.Vector(ks.map(_.toDouble)) *:* (1.0/nRep))
          allcpu = allcpu +:+ (breeze.linalg.Vector(cpu.map(_.toDouble)) *:* (1.0/nRep))
          allhistorylengths = allhistorylengths +:+ (breeze.linalg.Vector(historylengths.map(_.toDouble)) *:* (1.0/nRep))
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
          summary.add("dataset", bandit.stream.dataset.id)
          summary.add("scalingstrategy", bandit.scalingstrategy.name)
          summary.add("k",  "%.2f".format(allks(step)))
          summary.add("gain",  "%.2f".format(allgains(step)))
          //summary.add("confidence", allconfs(step))
          //summary.add("gamma", bandit.scalingstrategy.gamma)
          summary.add("cputime", "%.4f".format(allcpu(step)))
          summary.add("historylength", "%.4f".format(allhistorylengths(step)))
          summary.add("iteration", step)
          summary.write(summaryPath)
        }
      }
    }
    info(s"End of experiment")// ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}
