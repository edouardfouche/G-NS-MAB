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

/**
  * Reference to an external data set, i.e., the data is stored in some external file
  * @param id name to give to this data set
  * @param path location in the system
  * @param header number of header lines (will be deleted)
  * @param separator character used to separate each value
  * @param category "category" to which this data set belongs (free text)
  */
case class ExternalDataRef(id: String, path: String, header: Int, separator: String, excludeIndex: Boolean, category: String) extends DataRef {

  /**
    * Open the data ref
    * @return A 2-D Array of Double containing the values from the csv. (row oriented)
    */
  def open(): Array[Array[Double]] = {
    try {
      Preprocess.open(path, header, separator, excludeIndex, dropClass = true, sample1000 = false)
    } catch {
      case e: Exception => println(s"Exception caught open $path" + e); null
    }
  }
}