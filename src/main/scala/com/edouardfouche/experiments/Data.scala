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

import com.edouardfouche.preprocess._

/**
  * Created by fouchee on 26.07.17.
  * Here, we define the reference to the data we need.
  */
object Data {
  val home: String = System.getProperty("user.home")
  val currentdir: String = System.getProperty("user.dir")

  // Reference to the pre-computed data for the use case
  lazy val bioliq_1wx20_MI_1000_100 = ExternalDataRef("bioliq_1wx20_MI_1000_100", currentdir + "/data/Bioliq_S-MAB_1wx20_MI_1000_100.csv", 0, ",", excludeIndex = false, "cache")
  lazy val zozo_bts_all = ExternalDataRef("zozo_bts_all", currentdir + "/data/zozo_bts_all.csv", 0, ",", excludeIndex = false, "cache")
  lazy val zozo_bts_all_minus = ExternalDataRef("zozo_bts_all_minus", currentdir + "/data/zozo_bts_all_minus.csv", 0, ",", excludeIndex = false, "cache")
  lazy val zozo_random_all_minus_small = ExternalDataRef("zozo_random_all_minus_small", currentdir + "/data/zozo_random_all_minus_small.csv", 0, ",", excludeIndex = false, "cache")
 }
