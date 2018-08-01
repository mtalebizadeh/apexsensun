#' @title genParmRange
#' @description Creates an empty dataframe for setting lower and upper limits of uncertain model parameters inside the PARM.dat file
#'
#' @param parmRangeFile File name associated to parameter range file containing parameters inside the PATM.dat file.
#'
#' @return Void
#' @export
#'
#' @examples
#' \dontrun{
#'   genParmRange("parmRangeFile.txt") #Note: Used only if the inputs are to be read through text file.
#'   }
genParmRange <- function(parmRangeFile) {
  watershedParamDataframe <- as.data.frame(array(data = -1, dim = c(2,103)))
  watershedParamDataframeNames <- c("Crop_canopy_PET", "Root_growth_soil", "Water_stress_harvest",
                                    "Water_storage_N", "Soil_water_limit", "Winter_dormancy",
                                    "N_fixation", "Soluble_P_runoff", "Pest_damage_moisture",
                                    "Pest_damage_cover", "Moisture_req_seed_germ", "Soil_evap_coeff",
                                    "Wind_erod_coeff", "Nitrate_leac_ratio", "Runoff_CN_Adj_parm",
                                    "Expand_CN_ret_parm", "Soil_evap_plant_cover", "Sedim_rout_exponent",
                                    "Sedim_rout_coeff", "Runoff_CN_int_abs", "Soluble_C_adsorp_coeff",
                                    "CN_retention_frozen_soil", "Harg_equation_parm", "Pest_leach_ratio",
                                    "Expo_coeff_rainfall", "Matur_frac_spring", "CEC_effect_nitrification",
                                    "N_fixation_limit", "Biological_mix_efficiency", "Soluble_P_exponent",
                                    "Max_depth_bio_mixing", "OrgP_loss_exponent", "MUST_coeff",
                                    "Harg_PET_exponent", "Denit_soil_threshold", "Daily_denit_limit",
                                    "SWAT_delivery_ratio_exponent", "Water_stress_coeff", "Puddling_sat_conduct",
                                    "Groundwater_stor_threshold", "Root_temp_stress_exponent", "SCS_index_coeff",
                                    "Plow_depth", "CN_retention_param", "sediment_rout_travel_coeff",
                                    "RUSLE_c_factor_res", "RUSLE_c_factor_height", "Climate_stress_factor",
                                    "Max_rain_intercept", "Rain_intercept_coeff", "Water_stor_residue_coeff",
                                    "Tillage_residue_decay_rate_coeff", "Microbial_soil_depth_coeff", "N_enrich_coeff",
                                    "N_enrich_rout_exponent", "Fraction_destroyed_burn", "P_enrich_rout_coeff",
                                    "P_enrich_rout_exponent", "P_move_evap_coeff", "Max_days_grazed_rotation",
                                    "Soil_water_up_flow_limit", "Manure_erosion_equation_coeff", "N_enrich_ratio_delivery",
                                    "Dust_distribution_coeff", "RUSLE2_trans_capacity", "RUSLE2_trans_capacity_threshold",
                                    "Dust_distribution_exponent", "Manure_erosion_exponent", "Microbial_top_soil_coeff",
                                    "Microbial_decay_coeff", "Manure_erosion_coeff", "Volt_nitrification_partition_coeff",
                                    "Hydrograph_dev_param", "Partition_N_flow_groundwater", "P_enrich_ratio_deliver_SWAT",
                                    "Stand_dead_fall_rate_coeff", "Runoff_2_delay_pest", "Soil_water_2_delay_tillage",
                                    "Auto_mov_lower_limit", "Nitrification_vol_upper_limit", "Tech_coeff",
                                    "Drainage_lateral_conduct", "P_flux_labile_active_coeff", "P_flux_active_stable_coeff",
                                    "N_salt_evap_coeff", "Water_table_recession_coeff", "Water_table_move_limit",
                                    "Water_table_recession_exponent", "Subsurface_flow_factor", "Flood_evap_limit",
                                    "Runoff_adj_link", "Water_erosion_threshold", "Wind_erosion_threshold",
                                    "Crop_stress_temp_exponent", "Soluble_P_leach_KD", "Unknown1",
                                    "Unknown2", "Unknown3", "Irrigation_cost",
                                    "Lime_cost", "Fuel_cost", "Labor_cost", "Unknown4")
  names(watershedParamDataframe) <- watershedParamDataframeNames





  if (hasArg(parmRangeFile)) {
  write.table(watershedParamDataframe, file = paste(parmRangeFile, sep=""), sep = "\t",
              row.names = FALSE)
  }

  watershedParamDataframe
}
