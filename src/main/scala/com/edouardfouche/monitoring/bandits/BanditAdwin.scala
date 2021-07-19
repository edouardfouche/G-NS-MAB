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
package com.edouardfouche.monitoring.bandits

/**
  * General trait for bandits that use adwin
  */
trait BanditAdwin extends Bandit {
  val delta: Double // Parameter for adwin

  // Initialize  the history of updates
  var history: List[scala.collection.mutable.Map[Int,Double]] = List() // first el in the update for count, and last in the update for weight

  override def reset: Unit = {
    super.reset
    history = List()
  }
}
