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

import scala.annotation.tailrec

/**
  * Simulates a setting where the means of the tops arms are "abruptly" (i.e., at the same time) set to 0 and back
  * Can do multiple changes
  *
  * @param nchanges the number of changes
  * @param d        the number of arms
  */
case class AbruptChangesGenerator(nchanges: Int = 2, d: Int = 100) extends Scenario{
  val id = s"AbruptChangesGenerator-$nchanges-$d" //val J = 90 // few high-reward arms
  val n = 100000
  val nphases = nchanges+1
  /**
    * generate data
    * @return A 2-D Array of Double containing the values from the csv. (row oriented)
    */
  def generate(rand: RandBasis =
                 new RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister(scala.util.Random.nextInt)))): Array[Array[Double]] = {
    val a = (1 to d).map(_/d.toDouble).toArray
    val means = a.reverse

    @tailrec
    def nextphase(i: Int, index: Int, b: Bernoulli, data: Array[Double]): Array[Double] = {
      if(i==nphases) data
      else{
        val nelements = if(i == nphases-1) n/nphases + +n%nphases else n/nphases // consider if this is the last phase
        val newpart = if(i%2==0) {
          (0 until nelements).toArray.map(y => if(b.draw()) 1.0 else 0.0)
        } else {
          (0 until nelements).toArray.map{y =>
            if(index < 30) 0.0
            else {
              if(b.draw()) 1.0 else 0.0
            }
          }
        }
        nextphase(i+1, index, b, data ++ newpart)
      }
    }

    val cols: Array[Array[Double]] = means.zipWithIndex.map{x =>
      val b: Bernoulli = new Bernoulli(x._1)(rand)
      val index = x._2
      nextphase(0, index, b, Array[Double]())
    }

    cols.transpose
  }
}
