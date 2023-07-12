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
package com.edouardfouche.experiments

import com.edouardfouche.monitoring.bandits._
import com.edouardfouche.monitoring.bandits.adversarial.MP_RExp3
import com.edouardfouche.monitoring.bandits.nonstationary._
import com.edouardfouche.monitoring.bandits.oracles._
import com.edouardfouche.monitoring.bandits.stationary.{MPKLUCB, MPTS}
import com.edouardfouche.utils
import com.edouardfouche.utils.StopWatch
import com.typesafe.scalalogging.LazyLogging
import org.slf4j.MDC

import java.io.{File, FileWriter}
import scala.annotation.tailrec

/**
  * Master trait for every bandit experiment. Define basic variables and common functions.
  */
trait BanditExperiment extends LazyLogging {
  // Output formatting
  val output_folder: String = System.getProperty("user.dir")
  val master_experiment_folder: String = output_folder concat "/" concat "experiments"
  utils.createFolderIfNotExisting(master_experiment_folder)
  val formatter = new java.text.SimpleDateFormat("yyy-MM-dd-HH-mm")
  val dirname: String = s"${formatter.format(java.util.Calendar.getInstance().getTime)}_${this.getClass.getSimpleName.init}_"
  val experiment_folder: String = master_experiment_folder concat "/" concat dirname
  val summaryPath = experiment_folder + "/" + this.getClass.getSimpleName.init + ".csv"

  MDC.put("path", s"$experiment_folder/${this.getClass.getSimpleName.init}")

  info(s"${formatter.format(java.util.Calendar.getInstance().getTime)} - Starting the experiment ${this.getClass.getSimpleName.init}\n")
  utils.createFolderIfNotExisting(experiment_folder)

  info(s"Started on: ${java.net.InetAddress.getLocalHost.getHostName}")

  val banditConstructors = Vector(
    // Oracles
    OracleDynamic,
    OracleStatic,
    OracleRandom,
    OracleAbrupt,
    OracleGradualGlobal,
    OracleAbruptGlobal,

    // Static
    MPTS, MPKLUCB,
    MP_E_Greedy(0.7)(_, _, _, _), MP_E_Greedy(0.8)(_, _, _, _), MP_E_Greedy(0.9)(_, _, _, _), MP_E_Greedy(0.99)(_, _, _, _),

    // Passive approaches
    MP_D_TS(0.7)(_, _, _, _), MP_D_TS(0.8)(_, _, _, _), MP_D_TS(0.9)(_, _, _, _), MP_D_TS(0.99)(_, _, _, _), MP_D_TS(0.999)(_, _, _, _),
    MP_D_UCB(0.7)(_, _, _, _), MP_D_UCB(0.8)(_, _, _, _), MP_D_UCB(0.9)(_, _, _, _), MP_D_UCB(0.99)(_, _, _, _), MP_D_UCB(0.999)(_, _, _, _),
    MP_SW_UCB(100)(_, _, _, _), MP_SW_UCB(500)(_, _, _, _), MP_SW_UCB(1000)(_, _, _, _), MP_SW_UCB(5000)(_, _, _, _),
    MP_SW_TS(100)(_, _, _, _), MP_SW_TS(500)(_, _, _, _), MP_SW_TS(1000)(_, _, _, _), MP_SW_TS(5000)(_, _, _, _),
    MP_SW_UCB_SHARP_A(0.1, 12.3)(_, _, _, _),
    MP_SW_UCB_SHARP_G(0.1, 4.3)(_, _, _, _),
    MP_SW_UCB_SHARP_A(0.2, 12.3)(_, _, _, _),
    MP_SW_UCB_SHARP_G(0.2, 4.3)(_, _, _, _),
    MP_RExp3(100)(_, _, _, _),
    MP_RExp3(500)(_, _, _, _),
    MP_RExp3(1000)(_, _, _, _),
    MP_RExp3(5000)(_, _, _, _),

    // Active
    MP_GLR_KL_UCB_G(_, _, _, _),
    MP_GLR_KL_UCB_L(_, _, _, _),
    MP_M_UCB(1000, 10)(_, _, _, _), MP_M_UCB(5000, 10)(_, _, _, _),
    MP_M_UCB(1000, 100)(_, _, _, _), MP_M_UCB(5000, 100)(_, _, _, _),

    // Mukherjee
    ImpCPD,
    ImpCPD2,
    ImpCPD3,
    UCBL_CPD,

    // Ours
    MP_ADS_TS_ADWIN1(0.1)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.01)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.0001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.00001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.0000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.00000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.000000000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.000000000000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.1)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.01)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.001)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.0001)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.00001)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.0000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.00000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.000000000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.000000000000001)(_, _, _, _),

    MP_ADR_TS_ADWIN1_v5(0.1)(_, _, _, _),
    MP_ADR_TS_ADWIN1_v5(0.01)(_, _, _, _),
    MP_ADR_TS_ADWIN1_v5(0.001)(_, _, _, _),
    MP_ADR_TS_ADWIN1_v5(0.0001)(_, _, _, _),
    MP_ADR_TS_ADWIN1_v5(0.00001)(_, _, _, _),
    MP_ADR_TS_ADWIN1_v5(0.000001)(_, _, _, _),
    MP_ADR_TS_ADWIN1_v5(0.0000001)(_, _, _, _),
    MP_ADR_TS_ADWIN1_v5(0.00000001)(_, _, _, _),
    MP_ADR_TS_ADWIN1_v5(0.000000000001)(_, _, _, _),
    MP_ADR_TS_ADWIN1_v5(0.000000000000001)(_, _, _, _),

    MP_ADS_TS_ADWIN1(0.1, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.01, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.0001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.00001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.0000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.00000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.000000000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.000000000000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.1, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.01, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.0001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.00001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.0000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.00000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.000000000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1_v2(0.000000000000001, ADR = true)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.1)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.01)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.0001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.00001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.000001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.0000001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.00000001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.000000000001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.000000000000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.1)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.01)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.0001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.00001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.0000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.00000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.000000000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.000000000000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.1)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.01)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.0001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.00001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.0000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.00000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.000000000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.000000000000001)(_, _, _, _),

    MP_ADR_KL_UCB_ADWIN1_v5(0.1)(_, _, _, _),
    MP_ADR_KL_UCB_ADWIN1_v5(0.01)(_, _, _, _),
    MP_ADR_KL_UCB_ADWIN1_v5(0.001)(_, _, _, _),
    MP_ADR_KL_UCB_ADWIN1_v5(0.0001)(_, _, _, _),
    MP_ADR_KL_UCB_ADWIN1_v5(0.00001)(_, _, _, _),
    MP_ADR_KL_UCB_ADWIN1_v5(0.000001)(_, _, _, _),
    MP_ADR_KL_UCB_ADWIN1_v5(0.0000001)(_, _, _, _),
    MP_ADR_KL_UCB_ADWIN1_v5(0.00000001)(_, _, _, _),
    MP_ADR_KL_UCB_ADWIN1_v5(0.000000000001)(_, _, _, _),
    MP_ADR_KL_UCB_ADWIN1_v5(0.000000000000001)(_, _, _, _),

    MP_ADS_KL_UCB_ADWIN1(0.1, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.01, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.0001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.00001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.0000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.00000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.000000000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.000000000000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.1, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.01, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.0001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.00001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.0000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.00000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.000000000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1_v2(0.000000000000001, ADR = true)(_, _, _, _),
  )

  val banditConstructors_ADWIN = Vector(
    OracleDynamic,
    OracleStatic,
    OracleRandom,
    OracleAbruptGlobal,
    OracleGradualGlobal,
    // Ours
    MP_ADS_TS_ADWIN1(0.1)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.01)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.0001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.00001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.0000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.00000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.000000000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.000000000000001)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.1, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.01, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.0001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.00001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.0000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.00000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.000000000001, ADR = true)(_, _, _, _),
    MP_ADS_TS_ADWIN1(0.000000000000001, ADR = true)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.1)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.01)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.0001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.00001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.000001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.0000001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.00000001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.000000000001)(_, _, _, _),
    MP_ADR_Elimination_UCB(0.000000000000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.1)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.01)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.0001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.00001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.0000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.00000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.000000000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.000000000000001)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.1, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.01, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.0001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.00001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.0000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.00000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.000000000001, ADR = true)(_, _, _, _),
    MP_ADS_KL_UCB_ADWIN1(0.000000000000001, ADR = true)(_, _, _, _),
  )

  def run(): Unit

  def info(s: String): Unit = logger.info(s)

  @tailrec
  final def runner(bandit: Bandit, iteration: Int, gain: Double, matrixdiff: Double, rep: Int): (Double, Double) = {
    // if (iteration % 200 == 0) info(s"Reached iteration $iteration with bandit ${bandit.name}")
    val ref = bandit.stream.cache

    val nextresult_watch = StopWatch.measureTime(bandit.next)
    val nextresult_cpu = nextresult_watch._1
    val nextresult_wall = nextresult_watch._2
    val nextresult = nextresult_watch._3

    if(nextresult._1.isEmpty) {
      //info(s"Bandit ${bandit.name} has finished")
      return (gain,matrixdiff)
    }
    else {
      // Be careful: Not meaningful in every case maybe
      val diff = breeze.linalg.sum(breeze.numerics.abs(bandit.currentMatrix - breeze.linalg.Vector(ref(iteration))))
      //val diff = breeze.linalg.sum(bandit.currentMatrix.toArray.zip(ref(iteration).toArray).map(x => bandit.reward.getReward(x._1, x._2)))

      val attributes = List("bandit","dataset","action","reward","scaling","windowSize","stepSize",
         "delta","gamma","k","banditk","narms","gain","matrixdiff","cpuTime","wallTime","iteration","nrep")
      val summary = ExperimentSummary(attributes)
      // This is the list of all the data possible we can record, "attributes" is usually a subset of it
      // val attributes = List("bandit","dataset","action","reward","scaling","windowSize","stepSize",
      // "delta","gamma","k","banditk","narms","gain","matrixdiff","cpuTime","wallTime","iteration","nrep")
      summary.add("bandit", bandit.name)
      summary.add("dataset", bandit.stream.dataset.id)
      summary.add("action", bandit.stream.action_name)
      summary.add("reward", bandit.reward.name)
      summary.add("scaling", bandit.scalingstrategy.name)
      summary.add("windowSize", bandit.stream.windowSize)
      summary.add("stepSize", bandit.stream.stepSize)
      summary.add("delta", bandit.scalingstrategy.delta)
      summary.add("gamma", bandit.scalingstrategy.gamma)
      summary.add("k", nextresult._2.length)
      summary.add("banditk", bandit.k)
      summary.add("narms", bandit.narms)
      summary.add("gain", nextresult._3)
      summary.add("matrixdiff", diff)
      summary.add("cpuTime", nextresult_cpu)
      summary.add("wallTime", nextresult_wall)
      summary.add("iteration", iteration)
      summary.add("nrep", rep)

      summary.write(summaryPath)
      runner(bandit, iteration+1, gain + nextresult._3, matrixdiff + diff, rep)
    }
  }

  @tailrec
  final def fullrunner(bandit: Bandit, gains: Array[Double]): Array[Double] = {
    val nextresult = bandit.next

    if(nextresult._1.isEmpty) {
      return gains
    } else {
      fullrunner(bandit, gains :+ nextresult._3)
    }
  }

  @tailrec
  final def fullrunnerGainsKs(bandit: Bandit, gains: Array[Double], ks: Array[Int]): (Array[Double], Array[Int]) = {
    val k = bandit.k
    val nextresult = bandit.next

    if(nextresult._1.isEmpty) {
      return (gains, ks)
    } else {
      fullrunnerGainsKs(bandit, gains :+ nextresult._3, ks :+ k)
    }
  }

  @tailrec
  final def fullrunnerGainsKsCPU(bandit: Bandit, gains: Array[Double], ks: Array[Int], cpu: Array[Double]): (Array[Double], Array[Int], Array[Double]) = {
    val k = bandit.k
    val next = StopWatch.measureCPUTime(bandit.next)
    val time = next._1
    val nextresult = next._2

    if(nextresult._1.isEmpty) {
      return (gains, ks, cpu)
    } else {
      fullrunnerGainsKsCPU(bandit, gains :+ nextresult._3, ks :+ k, cpu :+ time)
    }
  }

  @tailrec
  final def fullrunnerGainsKsCPUW(bandit: Bandit, gains: Array[Double], ks: Array[Int], cpu: Array[Double], historylengths: Array[Double]): (Array[Double], Array[Int], Array[Double], Array[Double]) = {
    val k = bandit.k
    val next = StopWatch.measureCPUTime(bandit.next)
    val time = next._1
    val nextresult = next._2

    if(nextresult._1.isEmpty) {
      return (gains, ks, cpu, historylengths)
    } else {
      bandit match {
        case bandit: BanditAdwin => fullrunnerGainsKsCPUW(bandit, gains :+ nextresult._3, ks :+ k, cpu :+ time, historylengths :+ bandit.history.length.toDouble)
        case _ => {
          val lasthistory = if(historylengths.isEmpty) 0.0 else historylengths.last
          fullrunnerGainsKsCPUW(bandit, gains :+ nextresult._3, ks :+ k, cpu :+ time, historylengths :+ lasthistory + 1.0)
        }
      }
    }
  }

  @tailrec
  final def fullrunnerGainsKsConfs(bandit: Bandit, gains: Array[Double], ks: Array[Int], confs: Array[Double]): (Array[Double], Array[Int], Array[Double]) = {
    val k = bandit.k
    val nextresult = bandit.next

    if(nextresult._1.isEmpty) {
      return (gains, ks, confs)
    } else {
      fullrunnerGainsKsConfs(bandit, gains :+ nextresult._3, ks :+ k, confs :+ bandit.scalingstrategy.confidence)
    }
  }



  case class ExperimentSummary(attributes: List[String]) {
    //var results: List[(String, Any)] = List()
    val results: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()

    //def add(name: String, v: Any): Unit = results = results :+ (name, v)
    def add(name: String, v: Any): Unit = {
      results(name) = v
    }

    def write(path: String): Unit = {
      synchronized {
        if(!new File(path).exists) { // write the header
          val fileA = new File(path)
          val fwA = new FileWriter(fileA, true)
          fwA.write(getHeader)
          fwA.flush()
          fwA.close()
        }
        val fileA = new File(path)
        val fwA = new FileWriter(fileA, true) // append set to true
        fwA.write(this.toString) // this is the string
        fwA.flush()
        fwA.close()
      }
    }

    override def toString: String = {
      (attributes.map(x => results.getOrElse(x, "NULL").toString) mkString ",") + "\n"
    }

    def getHeader: String = (attributes mkString ",") + "\n"
  }


  def dump(path:String, towrite:String) : Unit = {
    synchronized {
      val fileA = new File(path)
      val fwA = new FileWriter(fileA, true)
      fwA.write(towrite) // this is the string
      fwA.flush()
      fwA.close()
    }
  }
}
