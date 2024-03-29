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
package com.edouardfouche.monitoring.scalingstrategies

/**
  * Basic helper with which no scaling happens
  * @param k the fixed number of arms to play
  */
case class NoScaling(var k: Int) extends ScalingStrategy {
  val lmin = k
  val lmax = k
  val name=s"No-$k"
  val delta=0.0
  val gamma=0.0
  var confidence=0.0
  def scale(rewards: Array[Double], indexes: Array[Int], sums: Array[Double], counts: Array[Double], t:Double): Int = {
    k
  }
}
