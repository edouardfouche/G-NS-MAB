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
  * General trait for bandits based on Thompson Sampling
  */
trait BanditTS extends Bandit {
  var beta_params: Array[(Double, Double)] = (0 until narms).map(x => (1.0,1.0)).toArray // initialize beta parameters to 1

  override def reset: Unit = {
    super.reset
    beta_params = (0 until narms).map(x => (1.0,1.0)).toArray
  }
}
