/*
 * Copyright (C) 2018 Edouard Fouché
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
import com.edouardfouche.monitoring.bandits.adversarial._
import com.edouardfouche.monitoring.bandits.oracles.{OracleDynamic, OracleRandom, OracleStatic}
import com.edouardfouche.monitoring.bandits.stationary._
import com.edouardfouche.monitoring.scalingstrategies._
import com.edouardfouche.preprocess._

/**
  * Created by fouchee on 12.07.17.
  * This experiment compares the behavior of various bandits in the face of a "shutdown" change (see ShutdownGenerator)
  */
object BanditNonStaticAbrupt extends BanditSyntheticExperiment {
  val d = 100
  val lmin = 1
  val lmax = d

  val generator= AbruptGenerator(d)

  val nRep = 5

  val scalingstrategies: Array[ScalingStrategy] = Array(
    //NoScaling(10),
    NoScaling(5),
    NoScaling(2),
    NoScaling(1)
  )

  override val banditConstructors = Vector(
    // Oracles
    OracleDynamic,
    OracleStatic,
    OracleRandom,
    //OracleSequential,
    //CUCB, CUCBm,
    //MPKLUCB, MPKLUCBPLUS,
    //Exp3M,

    // Static
    MPTS, MPKLUCB,//, IMPTS, MPOTS,
    MP_E_Greedy(0.7)(_, _, _, _), MP_E_Greedy(0.8)(_, _, _, _),
    MP_E_Greedy(0.9)(_, _, _, _), MP_E_Greedy(0.99)(_, _, _, _),

    // Passive approaches
    MP_D_TS(0.7)(_,_,_,_), MP_D_TS(0.8)(_,_,_,_), MP_D_TS(0.9)(_,_,_,_), MP_D_TS(0.99)(_,_,_,_),
    MP_D_UCB(0.7)(_,_,_,_), MP_D_UCB(0.8)(_,_,_,_), MP_D_UCB(0.9)(_,_,_,_), MP_D_UCB(0.99)(_,_,_,_),
    MP_SW_UCB(100)(_, _, _, _), MP_SW_UCB(500)(_, _, _, _), MP_SW_UCB(1000)(_, _, _, _), MP_SW_UCB(5000)(_, _, _, _),
    MP_SW_TS(100)(_, _, _, _), MP_SW_TS(500)(_, _, _, _), MP_SW_TS(1000)(_, _, _, _), MP_SW_TS(5000)(_, _, _, _),
    MP_SW_UCB_SHARP_A(0.1, 12.3)(_,_,_,_),
    MP_SW_UCB_SHARP_G(0.1, 4.3)(_,_,_,_),
    MP_SW_UCB_SHARP_A(0.2, 12.3)(_,_,_,_),
    MP_SW_UCB_SHARP_G(0.2, 4.3)(_,_,_,_),
    MP_RExp3(100)(_,_,_,_),
    MP_RExp3(500)(_,_,_,_),
    MP_RExp3(1000)(_,_,_,_),
    MP_RExp3(5000)(_,_,_,_),

    // Active
    MP_GLR_KL_UCB_G(_,_,_,_),
    MP_GLR_KL_UCB_L(_,_,_,_),
    MP_GLR_KL_UCB_G_F(_,_,_,_),
    MP_GLR_KL_UCB_L_F(_,_,_,_),
    MP_M_UCB(1000, 10)(_,_,_,_), MP_M_UCB(5000, 10)(_,_,_,_),
    MP_M_UCB(1000, 100)(_,_,_,_), MP_M_UCB(5000, 100)(_,_,_,_),

    //OracleStatic_ADWIN(0.1)(_,_,_,_), OracleDynamic_ADWIN(0.1)(_,_,_,_), OracleRandom_ADWIN(0.1)(_,_,_,_),
    //OracleSequential_ADWIN(0.1)(_,_,_,_),

    //MP_ADS_UCB(0.1)(_,_,_,_),
    //MP_ADS_KL_UCB(0.1)(_,_,_,_),
    //Exp3M_ADWIN(0.1)(_,_,_,_),

    // Ours
    MP_ADS_TS(0.1)(_,_,_,_),
    MP_ADS_TS(0.01)(_,_,_,_),
    MP_ADS_TS(0.001)(_,_,_,_),
    MP_ADR_TS(0.1)(_,_,_,_),
    MP_ADR_TS(0.01)(_,_,_,_),
    MP_ADR_TS(0.001)(_,_,_,_),
    MP_ADR_Elimination_UCB(0.1)(_,_,_,_),
    MP_ADR_Elimination_UCB(0.01)(_,_,_,_),
    MP_ADR_Elimination_UCB(0.001)(_,_,_,_),
  )
}
