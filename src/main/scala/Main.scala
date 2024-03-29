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
import com.edouardfouche.experiments._
import com.edouardfouche.utils.StopWatch
import com.typesafe.scalalogging.LazyLogging

object Main extends LazyLogging {
  def main(args: Array[String]): Unit = {
    val unit = "ms"

    info("Working directory: " + System.getProperty("user.dir"))
    info("Raw parameters given: " + args.map(s => "\"" + s + "\"").mkString("[", ", ", "]"))

    require(args.length > 0, "No arguments given. Please see README.md")

    StopWatch.start
    val result = startJob(experimentFactory(args(0)))
    val (cpu, wall) = StopWatch.stop(unit)

    println(s"Computation time: \t ${result._1} $unit (cpu), ${result._2} $unit (wall)")
    println(s"Total elapsed time: \t $cpu $unit (cpu), $wall $unit (wall)")
    System.exit(0)
  }

  def info(s: String): Unit = logger.info(s)

  def startJob[R](block: => R, unit: String = "ms"): (Double, Double, R) = {
    val res = StopWatch.measureTime(block, unit)
    //pprint.pprintln(res._3)
    res._3 match {
      case a:Double => println(a)
      case a:Array[Array[Double]] => print_matrix(a)
      case _ => println("Unknown type")
    }
    def print_matrix(a: Array[Array[Double]]): Unit = {
      val matrix = a.map{x =>
        if(x.length > 10) (x.take(10).map(y => f"$y%1.2f") mkString "\t") ++ "\t ... (truncated)"
        else x.map(y => f"$y%1.2f") mkString "\t"
      }
      val toprint = if(matrix.length > 10)
        (matrix.take(10) ++ Array((1 to 10).map(x => "...") mkString "\t")) ++ Array("(truncated)")
      else matrix
      toprint.foreach{x => println(x)}
    }
    res
  }

  def experimentFactory(arg: String): Unit = arg match {
    case "G-NS-MAB" => { // Run the full experiment suite
      BanditNonStaticAbruptGlobal.run()
      BanditNonStaticGradualGlobal.run()
      BanditNonStaticAbrupt.run()
      BanditStatic.run()
      BanditNonStaticAbruptGlobal_ADWIN.run()
      BanditNonStaticGradualGlobal_ADWIN.run()
      BanditRealWorld_Bioliq.run()
      BanditRealWorld_Zozo_unif.run()
    }
    // Individual handles
    case "BanditAbrupt" => BanditNonStaticAbrupt.run() // ~30min, 120MB
    case "BanditStatic" => BanditStatic.run() // ~30min, 115MB
    case "BanditNonStaticGradualGlobal" => BanditNonStaticGradualGlobal.run() // ~30min, 130MB
    case "BanditNonStaticAbruptGlobal" => BanditNonStaticAbruptGlobal.run() // ~30min, 130MB
    case "BanditRealWorld_Zozo_unif" => BanditRealWorld_Zozo_unif.run() // ~40min, 180MB
    case "BanditRealWorld_Bioliq" => BanditRealWorld_Bioliq.run() // ~1.5hours, 100MB
    case "BanditNonStaticGradualGlobal_ADWIN" => BanditNonStaticGradualGlobal_ADWIN.run() // ~10min, 55MB
    case "BanditNonStaticAbruptGlobal_ADWIN" => BanditNonStaticAbruptGlobal_ADWIN.run() // ~2hours, 55MB
    case "BanditNonStaticAbruptGlobal_Mukherjee" => BanditNonStaticAbruptGlobal_Mukherjee.run() // ~2hours, 55MB
    case "BanditNonStaticAbruptGlobal_ADR_v5" => BanditNonStaticAbruptGlobal_ADR_v5.run() // ~1hour local laptop, 16.8MB
    // Extra experiments
    case "BanditRealWorld_Bioliq_ADWIN" => BanditRealWorld_Bioliq_ADWIN.run()
    case "BanditRealWorld_Zozo_bts" => BanditRealWorld_Zozo_bts.run() // ~30min, 158MB
    case "BanditGradual" => BanditNonStaticGradual.run()

    case _ => throw new Error(s"Unknown experiment $arg")
  }
  def warn(s: String): Unit = logger.warn(s)
}
