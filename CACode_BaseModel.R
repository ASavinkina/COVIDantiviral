# General inputs

library(ggplot2)

#setwd('~/COVIDantivial')


#Set baseline parameter values for parameters of interest
COVID_highrisk_hosp_prob = 0.063
COVID_lowrisk_hosp_prob = COVID_highrisk_hosp_prob/5
Paxlovid_hosp_multiplier_noVax_HR = 1-0.873#1-0.873#1-0.21#1-0.873
Paxlovid_hosp_multiplier_noVax_LR = 1-0.873#Paxlovid_hosp_multiplier_noVax_HR
Paxlovid_hosp_multiplier_Vax_HR =  1-0.873#Paxlovid_hosp_multiplier_noVax_HR
Paxlovid_hosp_multiplier_Vax_LR =  1-0.873#Paxlovid_hosp_multiplier_noVax_LR
Vaccine_hosp_multiplier = 1-0.57

Death_hosp_prob = 0.07

US_highrisk_p = 0.6
US_vaccinated_p = 0.656
Cost_Paxlovid = 530
Cost_Hosp_US = 20000


#Set up output tables

Strategies <- c("Strategy0", "Strategy1", "Strategy2", "Strategy3", "Strategy4")
CE_Table <- data.frame(matrix(NA, nrow = 5, ncol = 5))

row.names(CE_Table) <- Strategies
colnames(CE_Table) <- c("Cost","Hospitalizations","Deaths","ICER Hospitalizations","ICER Deaths")
  
#Equations to calculate number of hospitalizations under each scenario (vaccination status, risk status)
  
  HR_UV_Hosp <- US_highrisk_p*(1-US_vaccinated_p)*COVID_highrisk_hosp_prob #high risk unvaccinated, hospitalized
  HR_V_Hosp <- US_highrisk_p*US_vaccinated_p*Vaccine_hosp_multiplier*COVID_highrisk_hosp_prob #high risk vaccinated, hospitalized
  LR_UV_Hosp <- (1-US_highrisk_p)*(1-US_vaccinated_p)*COVID_lowrisk_hosp_prob #low risk unvaccinated, hospitalized
  LR_V_Hosp <- (1-US_highrisk_p)*US_vaccinated_p*Vaccine_hosp_multiplier*COVID_lowrisk_hosp_prob #low risk vaccinated, hospitalized
  
  HR_UV_unHosp <- US_highrisk_p*(1-US_vaccinated_p)*(1-COVID_highrisk_hosp_prob) #high risk unvaccinated, unhospitalized
  HR_V_unHosp <- US_highrisk_p*US_vaccinated_p*(1-COVID_highrisk_hosp_prob) #high risk vaccinated, unhospitalized
  LR_UV_unHosp <- (1-US_highrisk_p)*(1-US_vaccinated_p)*(1-COVID_lowrisk_hosp_prob) #low risk unvaccinated, hospitalized
  LR_V_unHosp <- (1-US_highrisk_p)*US_vaccinated_p*(1-COVID_lowrisk_hosp_prob) #low risk vaccinated, hospitalized
  
  #Equations to calculate deaths among the hospitalized
  
  HR_UV_Death <- HR_UV_Hosp * Death_hosp_prob
  HR_V_Death <- HR_V_Hosp * Death_hosp_prob
  LR_UV_Death <- LR_UV_Hosp * Death_hosp_prob
  LR_V_Death <- LR_V_Hosp * Death_hosp_prob
  
  # Equations for the varying scenarios, calculating hospitalizations, deaths, and costs based on number hospitalized and dead
  # By vaccination status and risk status
  
  #Scenario 0
  
  Scenario0_Hospitalizations <- sum(HR_V_Hosp, HR_UV_Hosp,LR_V_Hosp, LR_UV_Hosp)
  Scenario0_Deaths <- sum(HR_V_Death, HR_UV_Death,LR_V_Death, LR_UV_Death)
  Scenario0_Costs <- sum(HR_V_Hosp*Cost_Hosp_US, HR_UV_Hosp*Cost_Hosp_US,LR_V_Hosp*Cost_Hosp_US, LR_UV_Hosp*Cost_Hosp_US)
  
  #Scenario 1
  
  Scenario1_Hospitalizations <- sum(HR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_HR, HR_V_Hosp, LR_UV_Hosp, LR_V_Hosp)
  Scenario1_Deaths <- sum(HR_UV_Hosp*Death_hosp_prob*Paxlovid_hosp_multiplier_noVax_HR, HR_V_Hosp*Death_hosp_prob, LR_UV_Hosp*Death_hosp_prob, LR_V_Hosp*Death_hosp_prob)
  Scenario1_Costs <- sum(HR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_HR*(Cost_Hosp_US+Cost_Paxlovid), HR_UV_unHosp*Cost_Paxlovid,  HR_V_Hosp*Cost_Hosp_US, LR_UV_Hosp*Cost_Hosp_US, LR_V_Hosp*Cost_Hosp_US)
  
  #Scenario 2
  
  Scenario2_Hospitalizations <- sum(HR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_HR, HR_V_Hosp*Paxlovid_hosp_multiplier_Vax_HR, LR_UV_Hosp, LR_V_Hosp)
  Scenario2_Deaths <- sum(HR_UV_Hosp*Death_hosp_prob*Paxlovid_hosp_multiplier_noVax_HR, HR_V_Hosp*Paxlovid_hosp_multiplier_Vax_HR*Death_hosp_prob, LR_UV_Hosp*Death_hosp_prob, LR_V_Hosp*Death_hosp_prob)
  Scenario2_Costs <- sum(HR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_HR*(Cost_Hosp_US+Cost_Paxlovid), HR_UV_unHosp*Cost_Paxlovid,  HR_V_Hosp*Paxlovid_hosp_multiplier_noVax_HR*(Cost_Hosp_US+Cost_Paxlovid), HR_V_unHosp*Cost_Paxlovid, LR_UV_Hosp*Cost_Hosp_US, LR_V_Hosp*Cost_Hosp_US)
  
  #Scenario 3
  
  Scenario3_Hospitalizations <- sum(HR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_HR, HR_V_Hosp*Paxlovid_hosp_multiplier_Vax_HR, LR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_LR, LR_V_Hosp)
  Scenario3_Deaths <- sum(HR_UV_Hosp*Death_hosp_prob*Paxlovid_hosp_multiplier_noVax_HR, HR_V_Hosp*Paxlovid_hosp_multiplier_Vax_HR*Death_hosp_prob, LR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_LR*Death_hosp_prob, LR_V_Hosp*Death_hosp_prob)
  Scenario3_Costs <- sum(HR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_HR*(Cost_Hosp_US+Cost_Paxlovid), HR_UV_unHosp*Cost_Paxlovid,  HR_V_Hosp*Paxlovid_hosp_multiplier_noVax_HR*(Cost_Hosp_US+Cost_Paxlovid), HR_V_unHosp*Cost_Paxlovid, LR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_LR*(Cost_Hosp_US+Cost_Paxlovid), LR_UV_unHosp*Cost_Paxlovid, LR_V_Hosp*Cost_Hosp_US)
  
  #Scenario 4
  
  Scenario4_Hospitalizations <- sum(HR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_HR, HR_V_Hosp*Paxlovid_hosp_multiplier_Vax_HR, LR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_LR, LR_V_Hosp*Paxlovid_hosp_multiplier_Vax_LR)
  Scenario4_Deaths <- sum(HR_UV_Hosp*Death_hosp_prob*Paxlovid_hosp_multiplier_noVax_HR, HR_V_Hosp*Paxlovid_hosp_multiplier_Vax_HR*Death_hosp_prob, LR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_LR*Death_hosp_prob, LR_V_Hosp*Paxlovid_hosp_multiplier_Vax_LR*Death_hosp_prob)
  Scenario4_Costs <- sum(HR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_HR*(Cost_Hosp_US+Cost_Paxlovid), 
                         HR_UV_unHosp*Cost_Paxlovid,  HR_V_Hosp*Paxlovid_hosp_multiplier_Vax_HR*(Cost_Hosp_US+Cost_Paxlovid), 
                         HR_V_unHosp*Cost_Paxlovid, LR_UV_Hosp*Paxlovid_hosp_multiplier_noVax_LR*(Cost_Hosp_US+Cost_Paxlovid), 
                         LR_UV_unHosp*Cost_Paxlovid, LR_V_Hosp*Paxlovid_hosp_multiplier_Vax_LR*(Cost_Hosp_US+Cost_Paxlovid),
                         LR_V_unHosp*Cost_Paxlovid)
  
  # Build cost effectiveness table
  
  CE_Table[1,1] <- Scenario0_Costs
  CE_Table[2,1] <- Scenario1_Costs
  CE_Table[3,1] <- Scenario2_Costs
  CE_Table[4,1] <- Scenario3_Costs
  CE_Table[5,1] <- Scenario4_Costs
  
  CE_Table[1,2] <- Scenario0_Hospitalizations
  CE_Table[2,2] <- Scenario1_Hospitalizations
  CE_Table[3,2] <- Scenario2_Hospitalizations
  CE_Table[4,2] <- Scenario3_Hospitalizations
  CE_Table[5,2] <- Scenario4_Hospitalizations
  
  CE_Table[1,3] <- Scenario0_Deaths
  CE_Table[2,3] <- Scenario1_Deaths
  CE_Table[3,3] <- Scenario2_Deaths
  CE_Table[4,3] <- Scenario3_Deaths
  CE_Table[5,3] <- Scenario4_Deaths
  
  # Calculate ICERs
  
  CE_Table[1,4] <- 0
  CE_Table[2,4] <- (CE_Table[2,1]-CE_Table[1,1])/(CE_Table[1,2]-CE_Table[2,2])
  CE_Table[3,4] <- (CE_Table[3,1]-CE_Table[2,1])/(CE_Table[2,2]-CE_Table[3,2])
  CE_Table[4,4] <- (CE_Table[4,1]-CE_Table[3,1])/(CE_Table[3,2]-CE_Table[4,2])
  CE_Table[5,4] <- (CE_Table[5,1]-CE_Table[4,1])/(CE_Table[4,2]-CE_Table[5,2])
  
  CE_Table[1,5] <- 0
  CE_Table[2,5] <- (CE_Table[2,1]-CE_Table[1,1])/(CE_Table[1,3]-CE_Table[2,3])
  CE_Table[3,5] <- (CE_Table[3,1]-CE_Table[2,1])/(CE_Table[2,3]-CE_Table[3,3])
  CE_Table[4,5] <- (CE_Table[4,1]-CE_Table[3,1])/(CE_Table[3,3]-CE_Table[4,3])
  CE_Table[5,5] <- (CE_Table[5,1]-CE_Table[4,1])/(CE_Table[4,3]-CE_Table[5,3])
  

# Print CE table
  
CE_Table$Strategy <- row.names(CE_Table)



