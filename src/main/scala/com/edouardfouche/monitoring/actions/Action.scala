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
package com.edouardfouche.monitoring.actions

/**
  * An action in our case corresponds to a score one can compute between two arrays
 */
trait Action {
  val name: String
  /**
    * Compute a score between array "a" and array "b"
    * @param a the first array of doubles, i.e., column 1
    * @param b the second array of double, i.e., column 2
    * @return the result of the action (i.e., dependency score) between the two array
    */
  def compute(a: Array[Double], b: Array[Double]): Double
}
