###########################
##### Input variable ######
###########################
#### T_soil, T_air:      soil temperature, air temperature      (Kelvin)
#### delta_L, delta_air: isotopic composition                   (permil)
#### RH_air 0~100:       relative humidity percent from 1-100   (%)
#### theta_soil:         soil moisture in  decimal (0-1)        (decimal, <=1)

#### data from VP-4 Humidity/Temp: 
#### celsus degree for Temp and kPa for pressure

#### data from from the weather station:
####  "airTemp","soilT" 
####  WS15 for Juniper, and WS18 for grassland

#### Soil properties based on 15cm in depth

#### Grassland

###########################
CG_model_advanced<- 
  function(delta_L_2H, delta_L_18O, T_soil, T_air, RH_air,  
           theta_soil, delta_air_2H, delta_air_18O){
    
    ###_____1_______
    ## Compute equilibrium factionation alpha_L_V for 2H and 18O (>1)
    #### alpha_L_v for 2H 
    alpha_equilibrium_2H <- 
      exp(0.001*(24.844*10^6/(T_soil^2) - 76.248 * 10^3/T_soil + 52.612)) 
    epsilon_equilibrium_2H <- alpha_equilibrium_2H - 1
    
    #### alpha_L_v for 18O
    alpha_equilibrium_18O <- 
      exp(0.001*(1.137*10^6/(T_soil^2) - 0.4156*10^3/T_soil - 2.0667)) 
    
    epsilon_equilibrium_18O <- alpha_equilibrium_18O - 1
    ####_____2______
    ### compute "saturation_WVP", saturation water vapor pressure. 
    ## x_temp is stored as in Kelvin degree in data, and here should convert to celsus degree 
    ## pressure_atm is the atmopsheric pressure in Pascal
    saturation_pressure_component <<- function(soil_temp, air_temp){
      ### temperature conversion 
      air_temp_convert    <- air_temp - 273.15
      soil_temp_convert   <- soil_temp - 273.15
      air_temp_component  <- 17.502*air_temp_convert/(240.97 + air_temp_convert)
      soil_temp_component <- 17.502*soil_temp_convert/(240.97 + soil_temp_convert)
      exp(air_temp_component - soil_temp_component)
    }
    saturation_pressure_parameter <- 
      saturation_pressure_component(soil_temp = T_soil, air_temp = T_air)
    
    ###___3___###  compute relative humidity
    
    RH_normal = 0.01 * RH_air * saturation_pressure_parameter
    
    ###___4___#### compute "kinetic fractionation factor (epsilon_k)"
    theta_res <- 0.053
    theta_sat <- 0.423
    soil_water_normalize <- (theta_soil - theta_res)/(theta_sat - theta_res)
    n <- 1 - 0.5*soil_water_normalize
    Diff_ratio_2H <- 0.9755
    Diff_ratio_18O <-  0.9723
    
    epsilon_k_2H <-  (1 - RH_normal)*(1 - Diff_ratio_2H^n)
    epsilon_k_18O <- (1 - RH_normal)*(1 - Diff_ratio_18O^n)
    
    ###_____5_____### get the final computation result for the evapiration flux
    
    ## for 2H
    numerator_2H <- 
      0.001 * delta_L_2H/alpha_equilibrium_2H  - RH_normal * 0.001 * delta_air_2H  - epsilon_k_2H  - epsilon_equilibrium_2H
    
    denominator_2H <- 
      1 - RH_normal + epsilon_k_2H
    
    delta_E_2H_advanced <- 
      numerator_2H/denominator_2H
    
    ### for 18O
    numerator_18O <- 
      0.001 * delta_L_18O/alpha_equilibrium_18O - RH_normal * 0.001 * delta_air_18O - epsilon_k_18O - epsilon_equilibrium_18O
    
    denominator_18O <- 
      1 - RH_normal + epsilon_k_18O
    
    delta_E_18O_advanced <- 
      numerator_18O/denominator_18O
    
    
    #### return values
    return(dplyr::lst(alpha_equilibrium_2H,  1000 * epsilon_equilibrium_2H,
                      alpha_equilibrium_18O, 1000 * epsilon_equilibrium_18O,
                      RH_normal, n,
                      1000 * epsilon_k_2H, 1000 * epsilon_k_18O,
                      1000 * delta_E_2H_advanced, 1000 * delta_E_18O_advanced))
  }

########################################
############## END #####################
########################################
