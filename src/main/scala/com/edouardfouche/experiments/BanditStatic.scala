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

import com.edouardfouche.monitoring.scalingstrategies._
import com.edouardfouche.preprocess._

/**
  * Created by fouchee on 12.07.17.
  * This experiment compares the behavior of various bandits in the static setting
  */
object BanditStatic extends BanditSyntheticExperiment {
  val d = 100
  val lmin = 1
  // val lmax = d

  val generator= StaticGenerator(d)

  val nRep = 100

  val scalingstrategies: Array[ScalingStrategy] = Array(
    //NoScaling(10),
    //NoScaling(5),
    //NoScaling(2),
    NoScaling(1)
  )
}
