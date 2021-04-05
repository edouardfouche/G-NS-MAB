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
import com.edouardfouche.experiments.Data._
import com.edouardfouche.monitoring.bandits.adversarial._
import com.edouardfouche.monitoring.bandits.nonstationary._
import com.edouardfouche.monitoring.bandits.oracles._
import com.edouardfouche.monitoring.bandits.stationary._
import com.edouardfouche.monitoring.rewards.CurrentScore
import com.edouardfouche.monitoring.scalingstrategies.{NoScaling, ScalingStrategy}
import com.edouardfouche.preprocess.DataRef
import com.edouardfouche.streamsimulator.CachedStreamSimulator

/**
  * Created by fouchee on 12.07.17.
  * This experiment compares the behavior of various bandits against real-world data (see Paper)
  */
object BanditRealWorld_Zozo_minus extends BanditExperiment {
  val attributes = List("bandit","dataset","scalingstrategy","k","gain","cputime", "iteration")

  val data: DataRef = zozo_bts_all_minus
  val streamsimulator = CachedStreamSimulator(data)

  val reward = CurrentScore()

  val lmin = 1
  val lmax = streamsimulator.npairs

  val nRep = 100
  //val nRep = 1

  val scalingstrategies: Array[ScalingStrategy] = Array(
    NoScaling(10),
    NoScaling(5),
    NoScaling(2),
    NoScaling(1)
  )

  val banditConstructors = Vector(
    // Oracles
    OracleDynamic,
    OracleStatic,
    OracleRandom,
    //OracleSequential,
    //CUCB, CUCBm,
    //MPKLUCB, MPKLUCBPLUS,
    //Exp3M,

    // Static
    MPTS, MPKLUCB,//, IMPTS, MPOTS,
    MP_E_Greedy(0.7)(_, _, _, _), MP_E_Greedy(0.8)(_, _, _, _), MP_E_Greedy(0.9)(_, _, _, _), MP_E_Greedy(0.99)(_, _, _, _),

    // Passive approaches
    MP_D_TS(0.7)(_,_,_,_), MP_D_TS(0.8)(_,_,_,_), MP_D_TS(0.9)(_,_,_,_), MP_D_TS(0.99)(_,_,_,_),
    MP_D_UCB(0.7)(_,_,_,_), MP_D_UCB(0.8)(_,_,_,_), MP_D_UCB(0.9)(_,_,_,_), MP_D_UCB(0.99)(_,_,_,_),
    MP_SW_UCB(100)(_, _, _, _), MP_SW_UCB(500)(_, _, _, _), MP_SW_UCB(1000)(_, _, _, _), MP_SW_UCB(5000)(_, _, _, _),
    MP_SW_TS(100)(_, _, _, _), MP_SW_TS(500)(_, _, _, _), MP_SW_TS(1000)(_, _, _, _), MP_SW_TS(5000)(_, _, _, _),
    MP_SW_UCB_SHARP_A(0.1, 12.3)(_,_,_,_),
    MP_SW_UCB_SHARP_G(0.1, 4.3)(_,_,_,_),
    MP_SW_UCB_SHARP_A(0.2, 12.3)(_,_,_,_),
    MP_SW_UCB_SHARP_G(0.2, 4.3)(_,_,_,_),
    MP_RExp3(100)(_,_,_,_),
    MP_RExp3(500)(_,_,_,_),
    MP_RExp3(1000)(_,_,_,_),
    MP_RExp3(5000)(_,_,_,_),

    // Active
    MP_GLR_KL_UCB_G(_,_,_,_),
    MP_GLR_KL_UCB_L(_,_,_,_),
    MP_GLR_KL_UCB_G_F(_,_,_,_),
    MP_GLR_KL_UCB_L_F(_,_,_,_),
    MP_M_UCB(1000, 10)(_,_,_,_), MP_M_UCB(5000, 10)(_,_,_,_),
    MP_M_UCB(1000, 100)(_,_,_,_), MP_M_UCB(5000, 100)(_,_,_,_),

    //OracleStatic_ADWIN(0.1)(_,_,_,_), OracleDynamic_ADWIN(0.1)(_,_,_,_), OracleRandom_ADWIN(0.1)(_,_,_,_),
    //OracleSequential_ADWIN(0.1)(_,_,_,_),

    //MP_ADS_UCB(0.1)(_,_,_,_),
    //MP_ADS_KL_UCB(0.1)(_,_,_,_),
    //Exp3M_ADWIN(0.1)(_,_,_,_),

    // Ours
    MP_ADS_TS(0.1)(_,_,_,_),
    MP_ADS_TS(0.01)(_,_,_,_),
    MP_ADS_TS(0.001)(_,_,_,_),
    MP_ADR_TS(0.1)(_,_,_,_),
    MP_ADR_TS(0.01)(_,_,_,_),
    MP_ADR_TS(0.001)(_,_,_,_),
    MP_ADR_Elimination_UCB(0.1)(_,_,_,_),
    MP_ADR_Elimination_UCB(0.01)(_,_,_,_),
    MP_ADR_Elimination_UCB(0.001)(_,_,_,_),
  )

  def run(): Unit = {
    info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting com.edouardfouche.experiments - ${this.getClass.getSimpleName}")
    // display parameters
    info(s"Parameters:")
    info(s"lmin:$lmin, lmax: $lmax")
    info(s"data:${data.id}")
    info(s"scalingstrategies: ${scalingstrategies.map(_.name) mkString ", "}")
    info(s"reward: ${reward.name}")
    info(s"nRep: ${nRep}")

    for {
      scalingstrategy <- scalingstrategies.par
    } {
      for{
        banditConstructor <- banditConstructors.par
      } {
        var allgains: linalg.Vector[Double] = linalg.Vector((1 to streamsimulator.nbatches).map(x => 0.0).toArray)
        var allks: linalg.Vector[Double] = linalg.Vector((1 to streamsimulator.nbatches).map(x => 0.0).toArray)
        var allcpu: linalg.Vector[Double] = linalg.Vector((1 to streamsimulator.nbatches).map(x => 0.0).toArray)
        var allinfocounts:  linalg.Vector[Double] = linalg.Vector((1 to streamsimulator.nbatches).map(x => 0.0).toArray)


        for {
          rep <- 0 until nRep
        } {
          //info(s"Starting com.edouardfouche.experiments with data: ${d.id}, configuration k: ${kratio}, rep=$rep")
          val bandit = banditConstructor(streamsimulator.copy(), reward, scalingstrategy, scalingstrategy.k)
          if (rep % 10 == 0) info(s"Reached rep $rep with bandit ${bandit.name}, ${scalingstrategy.name}")
          val (gains, ks, cpu) = fullrunnerGainsKsCPU(bandit, Array[Double](), Array[Int](), Array[Double]())

          // This is how we handle the -1 gains, i.e., the absence of feedback.
          val infovector: Array[Double] = gains.map(x => if(x < 0) 0.0 else 1.0)
          val correctedgain = gains.map(x => if(x < 0) 0 else x)

          allinfocounts = allinfocounts +:+ breeze.linalg.Vector(infovector)

          allgains = allgains +:+ breeze.linalg.Vector(correctedgain)
          allks = allks +:+ breeze.linalg.Vector(ks.map(_.toDouble))
          allcpu = allcpu +:+ breeze.linalg.Vector(cpu.map(_.toDouble))
        }

        allinfocounts = allinfocounts.map(x => if(x == 0) 1.0 else x) // to avoid dividing by 0
        allgains = allgains /:/ allinfocounts
        allks = allks /:/ allinfocounts
        allcpu = allcpu /:/ allinfocounts

        val bandit = banditConstructor(streamsimulator.copy(), reward, scalingstrategy, scalingstrategy.k)
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
          summary.add("iteration", step)
          summary.write(summaryPath)
        }
      }
    }
    info(s"End of experiment ${this.getClass.getSimpleName} - ${formatter.format(java.util.Calendar.getInstance().getTime)}")
  }
}
