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

import com.edouardfouche.monitoring.bandits.nonstationary._
import com.edouardfouche.monitoring.scalingstrategies._
import com.edouardfouche.preprocess._

/**
  * Created by fouchee on 12.07.17.
  * This experiment compares the behavior of various bandits in the face of an abrupt global change
  */
object BanditNonStaticAbruptGlobal_ADR_v5 extends BanditSyntheticExperiment {
  override val banditConstructors = Vector(
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
  )
  val d = 100
  //val lmax: Int = d
  val lmin = 1
  //val generator: StaticGenerator = StaticGenerator(d)
  val generator: AbruptGlobalGenerator = AbruptGlobalGenerator(d)
  val nRep = 100
  val scalingstrategies: Array[ScalingStrategy] = Array(
    //NoScaling(10),
    //NoScaling(5),
    //NoScaling(2),
    NoScaling(1)
  )
}
