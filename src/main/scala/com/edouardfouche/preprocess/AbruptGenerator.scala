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
package com.edouardfouche.preprocess

import breeze.stats.distributions.{Bernoulli, RandBasis, ThreadLocalRandomGenerator}
import org.apache.commons.math3.random.MersenneTwister

/**
  * Simulates a setting where the means of the tops arms at "abruptly" (i.e., at the same time) set to 0 and back
  * As in "Scaling Multi-Armed Bandit Algorithms" (Fouché 2019)
  *
  * @param d the number of arms
  */
case class AbruptGenerator(d: Int = 100) extends Scenario{
  val id = s"AbruptGenerator-$d"
  val n = 10000
  /**
    * generate data
    * @return A 2-D Array of Double containing the values from the csv. (row oriented)
    */
  def generate(rand: RandBasis =
               new RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister(scala.util.Random.nextInt)))): Array[Array[Double]] = {
    val a = (1 to d).map(_/d.toDouble).toArray
    val means = a.reverse

    val cols: Array[Array[Double]] = means.zipWithIndex.map{x =>
      val b = new Bernoulli(x._1)(rand)
      val index = x._2
      val partA: Array[Double] = (0 until n/3).toArray.map(y => if(b.draw()) 1.0 else 0.0)
      val partB: Array[Double] = (0 until n/3).toArray.map{y =>
        if(x._2 < 10){
          val bhalf = new Bernoulli(0.5)(rand)
          if( bhalf.draw() ) 1.0 else 0.0
        }
        else {
          if(b.draw()) 1.0 else 0.0
        }
      }
      val partC: Array[Double] = (0 until (n/3+n%3)).toArray.map(y => if(b.draw()) 1.0 else 0.0)
      partA ++ partB ++ partC
    }
    cols.transpose
  }
}