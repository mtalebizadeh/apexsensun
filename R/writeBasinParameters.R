

########################### Author: Mansour Talebizadeh ####################################
#
#
#
#Description: Writing with APEX input parameter format...
#Inputs: 1) uncertainWatershedParam:  A dataframe containing uncertain APEX parameters in Line 31 to 41
#        2) "paramFile.dat": File name containing parameter values with APEX format
#Outputs:
#        Void
#
#Required Functions: 1) watershed_param_dataframe.r
# source("watershed_param_dataframe.r")
# uncertainWatershedParam <- watershed_param_dataframe()
# uncertainWatershedParam$Crop_canopy_PET=6.6
# uncertainWatershedParam$Water_stress_harvest=0.66
# uncertainWatershedParam <- cbind(uncertainWatershedParam['Crop_canopy_PET'],uncertainWatershedParam['Water_stress_harvest'])
# paramFile.dat <- "PARM_Yah.dat"
# backUpParamFile.dat <- "PARM.dat"
# Tt <- write_param_basin(uncertainWatershedParam,paramFile.dat, backUpParamFile.dat)
############################################################################################
writeBasinParameters <- function (uncertainWatershedParam,paramFile.dat, backUpParamFile.dat) {
#
#
#Loading required functions...
  #source("watershed_param_dataframe.r")

#Setting up connections...
  connSource <- file(description = backUpParamFile.dat,open = "r+",blocking = TRUE)
  connTarget <- file(description = paramFile.dat,open ="r+",blocking = TRUE )

#Reading from back up file into a readLines object...
  readlinesObj <- readLines(con = connSource,n = -1)


####Sub-function
    parm2loc <- function(PARM) {
      switch(names(PARM),
            "Crop_canopy_PET"=return(c(31,1,8,"%8.3f")),
            "Root_growth_soil"=return(c(31,9,16,"%8.3f")),
            "Water_stress_harvest"=return(c(31,17,24,"%8.3f")),
            "Water_storage_N"=return(c(31,25,32,"%8.3f")),
            "Soil_water_limit"=return(c(31,33,40,"%8.3f")),
            "Winter_dormancy"=return(c(31,41,48,"%8.3f")),
            "N_fixation"=return(c(31,49,56,"%8.3f")),
            "Soluble_P_runoff"=return(c(31,57,64,"%8.3f")),
            "Pest_damage_moisture"=return(c(31,65,72,"%8.3f")),
            "Pest_damage_cover"=return(c(31,73,80,"%8.3f")),

      #Line 32....
            "Moisture_req_seed_germ"=return(c(32,1,8,"%8.3f")),
            "Soil_evap_coeff"=return(c(32,9,16,"%8.3f")),
            "Wind_erod_coeff"=return(c(32,17,24,"%8.3f")),
            "Nitrate_leac_ratio"=return(c(32,25,32,"%8.3f")),
            "Runoff_CN_Adj_parm"=return(c(32,33,40,"%8.3f")),
            "Expand_CN_ret_parm"=return(c(32,41,48,"%8.3f")),
            "Soil_evap_plant_cover"=return(c(32,49,56,"%8.3f")),
            "Sedim_rout_exponent"=return(c(32,57,64,"%8.3f")),
            "Sedim_rout_coeff"=return(c(32,65,72,"%8.3f")),
            "Runoff_CN_int_abs"=return(c(32,73,80,"%8.3f")),
      #Line 33...
            "Soluble_C_adsorp_coeff"= return(c(33,1,8,"%8.3f")),
            "CN_retention_frozen_soil"=return(c(33,9,16,"%8.3f")),
            "Harg_equation_parm"=return(c(33,17,24,"%8.3f")),
            "Pest_leach_ratio"=return(c(33,25,32,"%8.3f")),
            "Expo_coeff_rainfall"=return(c(33,33,40,"%8.3f")),
            "Matur_frac_spring"=return(c(33,41,48,"%8.3f")),
            "CEC_effect_nitrification"=return(c(33,49,56,"%8.3f")),
            "N_fixation_limit"=return(c(33,57,64,"%8.3f")),
            "Biological_mix_efficiency"=return(c(33,65,72,"%8.3f")),
            "Soluble_P_exponent"=return(c(33,73,80,"%8.3f")),
      #Line 34...
            "Max_depth_bio_mixing"= return(c(34,1,8,"%8.3f")),
            "OrgP_loss_exponent"=return(c(34,9,16,"%8.3f")),
            "MUST_coeff"=return(c(34,17,24,"%8.3f")),
            "Harg_PET_exponent"=return(c(34,25,32,"%8.3f")),
            "Denit_soil_threshold"=return(c(34,33,40,"%8.3f")),
            "Daily_denit_limit"=return(c(34,41,48,"%8.3f")),
            "SWAT_delivery_ratio_exponent"=return(c(34,49,56,"%8.3f")),
            "Water_stress_coeff"=return(c(34,57,64,"%8.3f")),
            "Puddling_sat_conduct"=return(c(34,65,72,"%8.3f")),
            "Groundwater_stor_threshold"=return(c(34,73,80,"%8.3f")),
      #Line 35...
            "Root_temp_stress_exponent" = return(c(35,1,8,"%8.3f")),
            "SCS_index_coeff"=return(c(35,9,16,"%8.3f")),
            "Plow_depth"=return(c(35,17,24,"%8.3f")),
            "CN_retention_param"=return(c(35,25,32,"%8.3f")),
            "sediment_rout_travel_coeff"=return(c(35,33,40,"%8.3f")),
            "RUSLE_c_factor_res"=return(c(35,41,48,"%8.3f")),
            "RUSLE_c_factor_height"=return(c(35,49,56,"%8.3f")),
            "Climate_stress_factor"=return(c(35,57,64,"%8.3f")),
            "Max_rain_intercept"=return(c(35,65,72,"%8.3f")),
            "Rain_intercept_coeff"=return(c(35,73,80,"%8.3f")),
      #Line 36...
            "Water_stor_residue_coeff"=return(c(36,1,8,"%8.3f")),
            "Tillage_residue_decay_rate_coeff"=return(c(36,9,16,"%8.3f")),
            "Microbial_soil_depth_coeff"=return(c(36,17,24,"%8.3f")),
            "N_enrich_coeff"=return(c(36,25,32,"%8.3f")),
            "N_enrich_rout_exponent"=return(c(36,33,40,"%8.3f")),
            "Fraction_destroyed_burn"=return(c(36,41,48,"%8.3f")),
            "P_enrich_rout_coeff"=return(c(36,49,56,"%8.3f")),
            "P_enrich_rout_exponent"=return(c(36,57,64,"%8.3f")),
            "P_move_evap_coeff"=return(c(36,65,72,"%8.3f")),
            "Max_days_grazed_rotation"=return(c(36,73,80,"%8.3f")),
      #Line 37...
            "Soil_water_up_flow_limit"=return(c(37,1,8,"%8.3f")),
            "Manure_erosion_equation_coeff"=return(c(37,9,16,"%8.3f")),
            "N_enrich_ratio_delivery"=return(c(37,17,24,"%8.3f")),
            "Dust_distribution_coeff"=return(c(37,25,32,"%8.3f")),
            "RUSLE2_trans_capacity"=return(c(37,33,40,"%8.3f")),
            "RUSLE2_trans_capacity_threshold"=return(c(37,41,48,"%8.3f")),
            "Dust_distribution_exponent"=return(c(37,49,56,"%8.3f")),
            "Manure_erosion_exponent"=return(c(37,57,64,"%8.3f")),
            "Microbial_top_soil_coeff"=return(c(37,65,72,"%8.3f")),
            'Microbial_decay_coeff'=return(c(37,73,80,"%8.3f")),
      #Line 38...
            "Manure_erosion_coeff"=return(c(38,1,8,"%8.3f")),
            "Volt_nitrification_partition_coeff"=return(c(38,9,16,"%8.3f")),
            "Hydrograph_dev_param"=return(c(38,17,24,"%8.3f")),
            "Partition_N_flow_groundwater"=return(c(38,25,32,"%8.3f")),
            "P_enrich_ratio_deliver_SWAT"=return(c(38,33,40,"%8.3f")),
            "Stand_dead_fall_rate_coeff"=return(c(38,41,48,"%8.3f")),
            "Runoff_2_delay_pest"=return(c(38,49,56,"%8.3f")),
            "Soil_water_2_delay_tillage"=return(c(38,57,64,"%8.3f")),
            "Auto_mov_lower_limit"=return(c(38,65,72,"%8.3f")),
            "Nitrification_vol_upper_limit"=return(c(38,73,80,"%8.3f")),
      #Line 39...
            "Tech_coeff"=return(c(39,1,8,"%8.3f")),

            "Drainage_lateral_conduct"=return(c(39,17,24,"%8.3f")),
            "P_flux_labile_active_coeff"=return(c(39,25,32,"%8.3f")),
            "P_flux_active_stable_coeff"=return(c(39,33,40,"%8.3f")),
            "N_salt_evap_coeff"=return(c(39,41,48,"%8.3f")),
            "Water_table_recession_coeff"=return(c(39,49,56,"%8.3f")),
            "Water_table_move_limit"=return(c(39,57,64,"%8.3f")),
            "Water_table_recession_exponent"=return(c(39,65,72,"%8.3f")),
            "Subsurface_flow_factor"=return(c(39,73,80,"%8.3f")),
      #Line 40...
            "Flood_evap_limit"=return(c(40,1,8,"%8.3f")),
            "Runoff_adj_link"=return(c(40,9,16,"%8.3f")),
            "Water_erosion_threshold"=return(c(40,17,24,"%8.3f")),
            "Wind_erosion_threshold"=return(c(40,25,32,"%8.3f")),
            "Crop_stress_temp_exponent"=return(c(40,33,40,"%8.3f")),
            "Soluble_P_leach_KD"=return(c(40,41,48,"%8.3f")),

            "Unknown1"=return(c(40,49,56,"%8.3f")),
            "Unknown2"=return(c(40,57,64,"%8.3f")),
            "Unknown3"=return(c(40,65,72,"%8.3f")),
      #Line 41...
            "Irrigation_cost"=return(c(41,1,8,"%8.3f")),
            "Lime_cost"=return(c(41,9,16,"%8.3f")),
            "Fuel_cost"=return(c(41,17,24,"%8.3f")),
            "Labor_cost"=return(c(41,25,32,"%8.3f")),
            "Unknown4"=return(c(41,33,40,"%8.3f"))
      )


    }
####Sub-Function....
    parm2replace <- function(readlinesObj,PARM) {
    loc_vec <- parm2loc(PARM)
    substr(x = readlinesObj[strtoi(loc_vec[1])],start = strtoi(loc_vec[2]),
           stop = strtoi(loc_vec[3])) <- sprintf(fmt = loc_vec[4],PARM[[1]])
    return(readlinesObj)

  }
################
    i=1
    col_numbers <-ncol(uncertainWatershedParam)
    while(i<=col_numbers) {
      readlinesObj <- parm2replace(readlinesObj,uncertainWatershedParam[i])
      i=i+1

    }
###Writing the final readLine object...
    writeLines(text = readlinesObj,con = connTarget)

close(connSource)
close(connTarget)

}

