library(tidyverse)

# Code to recrete Figure 2 from the manuscript. Similar to base code, except includes a two-way sensitivity analysis
# on antiviral treatment cost and effectiveness

#Set baseline parameter values for parameters of interest

COVID_highrisk_hosp_prob = 0.063
COVID_lowrisk_hosp_prob = COVID_highrisk_hosp_prob/5
Vaccine_hosp_multiplier = 1-0.57

Death_hosp_prob = 0.07

US_highrisk_p = 0.6
US_vaccinated_p = 0.656
Cost_Hosp_US = 20000

#Set up output tables

Strategies <- c("Strategy0", "Strategy1", "Strategy2", "Strategy3", "Strategy4")
CE_Table <- data.frame(matrix(NA, nrow = 5, ncol = 5))

row.names(CE_Table) <- Strategies
colnames(CE_Table) <- c("Cost","Hospitalizations","Deaths","ICER Hospitalizations","ICER Deaths",
                        "Inc Cost","Inc Benefit Death","Inc Benefit Hosp")

# Create sensitivity analysis table

PSA <- 1000

PSA_table <- data.frame(matrix(NA, nrow = PSA, ncol = 17))
colnames(PSA_table) <- c("PaxEffect", "PaxCost", "Strategy0_Cost","Strategy1_Cost"
                         , "Strategy2_Cost","Strategy3_Cost", "Strategy4_Cost","Strategy0_Death"
                         , "Strategy1_Death","Strategy2_Death", "Strategy3_Death","Strategy4_Death",
                         "Strategy0_Hosp", "Strategy1_Hosp","Strategy2_Hosp", "Strategy3_Hosp","Strategy4_Hosp")


# Create sensitivity analysis parameters- 
  # antiviral treatment effectiveness will range from 10% to 100% by 10% 
  # antiviral cost will range from $10 to $1,000 by $10 

PSA_table$PaxEffect <-rep(seq(0.1,1, by=0.1),100)
PSA_table$PaxCost <- rep(seq(10,1000, by=10), each=10)


# For-loop using possible values of effectiveness and cost and calculating hospitalizations and deaths for each scenario

for (i in 1:1000) {

    #Hospitalizations

HR_UV_Hosp <- US_highrisk_p*(1-US_vaccinated_p)*COVID_highrisk_hosp_prob
HR_V_Hosp <- US_highrisk_p*US_vaccinated_p*Vaccine_hosp_multiplier*COVID_highrisk_hosp_prob
LR_UV_Hosp <- (1-US_highrisk_p)*(1-US_vaccinated_p)*COVID_lowrisk_hosp_prob
LR_V_Hosp <- (1-US_highrisk_p)*US_vaccinated_p*Vaccine_hosp_multiplier*COVID_lowrisk_hosp_prob

HR_UV_unHosp <- US_highrisk_p*(1-US_vaccinated_p)*(1-COVID_highrisk_hosp_prob)
HR_V_unHosp <- US_highrisk_p*US_vaccinated_p*(1-COVID_highrisk_hosp_prob)
LR_UV_unHosp <- (1-US_highrisk_p)*(1-US_vaccinated_p)*(1-COVID_lowrisk_hosp_prob)
LR_V_unHosp <- (1-US_highrisk_p)*US_vaccinated_p*(1-COVID_lowrisk_hosp_prob)

    #Deaths

HR_UV_Death <- HR_UV_Hosp * Death_hosp_prob
HR_V_Death <- HR_V_Hosp * Death_hosp_prob
LR_UV_Death <- LR_UV_Hosp * Death_hosp_prob
LR_V_Death <- LR_V_Hosp * Death_hosp_prob

#Scenario 0

Scenario0_Hospitalizations <- sum(HR_V_Hosp, HR_UV_Hosp,LR_V_Hosp, LR_UV_Hosp)
Scenario0_Deaths <- sum(HR_V_Death, HR_UV_Death,LR_V_Death, LR_UV_Death)
Scenario0_Costs <- sum(HR_V_Hosp*Cost_Hosp_US, HR_UV_Hosp*Cost_Hosp_US,LR_V_Hosp*Cost_Hosp_US, LR_UV_Hosp*Cost_Hosp_US)

#Scenario 1

Scenario1_Hospitalizations <- sum(HR_UV_Hosp*PSA_table[i,1], HR_V_Hosp, LR_UV_Hosp, LR_V_Hosp)
Scenario1_Deaths <- sum(HR_UV_Hosp*Death_hosp_prob*PSA_table[i,1], HR_V_Hosp*Death_hosp_prob, LR_UV_Hosp*Death_hosp_prob, LR_V_Hosp*Death_hosp_prob)
Scenario1_Costs <- sum(HR_UV_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), HR_UV_unHosp*PSA_table[i,2],  HR_V_Hosp*Cost_Hosp_US, LR_UV_Hosp*Cost_Hosp_US, LR_V_Hosp*Cost_Hosp_US)

#Scenario 2

Scenario2_Hospitalizations <- sum(HR_UV_Hosp*PSA_table[i,1], HR_V_Hosp*PSA_table[i,1], LR_UV_Hosp, LR_V_Hosp)
Scenario2_Deaths <- sum(HR_UV_Hosp*Death_hosp_prob*PSA_table[i,1], HR_V_Hosp*PSA_table[i,1]*Death_hosp_prob, LR_UV_Hosp*Death_hosp_prob, LR_V_Hosp*Death_hosp_prob)
Scenario2_Costs <- sum(HR_UV_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), HR_UV_unHosp*PSA_table[i,2],  HR_V_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), HR_V_unHosp*PSA_table[i,2], LR_UV_Hosp*Cost_Hosp_US, LR_V_Hosp*Cost_Hosp_US)

#Scenario 3

Scenario3_Hospitalizations <- sum(HR_UV_Hosp*PSA_table[i,1], HR_V_Hosp*PSA_table[i,1], LR_UV_Hosp*PSA_table[i,1], LR_V_Hosp)
Scenario3_Deaths <- sum(HR_UV_Hosp*Death_hosp_prob*PSA_table[i,1], HR_V_Hosp*PSA_table[i,1]*Death_hosp_prob, LR_UV_Hosp*PSA_table[i,1]*Death_hosp_prob, LR_V_Hosp*Death_hosp_prob)
Scenario3_Costs <- sum(HR_UV_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), HR_UV_unHosp*PSA_table[i,2],  HR_V_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), HR_V_unHosp*PSA_table[i,2], LR_UV_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), LR_UV_unHosp*PSA_table[i,2], LR_V_Hosp*Cost_Hosp_US)

#Scenario 4

Scenario4_Hospitalizations <- sum(HR_UV_Hosp*PSA_table[i,1], HR_V_Hosp*PSA_table[i,1], LR_UV_Hosp*PSA_table[i,1], LR_V_Hosp*PSA_table[i,1])
Scenario4_Deaths <- sum(HR_UV_Hosp*Death_hosp_prob*PSA_table[i,1], HR_V_Hosp*PSA_table[i,1]*Death_hosp_prob, LR_UV_Hosp*PSA_table[i,1]*Death_hosp_prob, LR_V_Hosp*PSA_table[i,1]*Death_hosp_prob)
Scenario4_Costs <- sum(HR_UV_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), 
                       HR_UV_unHosp*PSA_table[i,2],  HR_V_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), 
                       HR_V_unHosp*PSA_table[i,2], LR_UV_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), 
                       LR_UV_unHosp*PSA_table[i,2], LR_V_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]),
                       LR_V_unHosp*PSA_table[i,2])

# Create cost-effectiveness table

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

# Calculate ICERS

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

# Calculate differences in cost and effect

CE_Table[1,6] <- 0
CE_Table[2,6] <- (CE_Table[2,1]-CE_Table[1,1])
CE_Table[3,6] <- (CE_Table[3,1]-CE_Table[1,1])
CE_Table[4,6] <- (CE_Table[4,1]-CE_Table[1,1])
CE_Table[5,6] <- (CE_Table[5,1]-CE_Table[1,1])

CE_Table[1,7] <- 0
CE_Table[2,7] <- (CE_Table[1,3]-CE_Table[2,3])
CE_Table[3,7] <- (CE_Table[1,3]-CE_Table[3,3])
CE_Table[4,7] <- (CE_Table[1,3]-CE_Table[4,3])
CE_Table[5,7] <- (CE_Table[1,3]-CE_Table[5,3])

CE_Table[1,8] <- 0
CE_Table[2,8] <- (CE_Table[1,2]-CE_Table[2,2])
CE_Table[3,8] <- (CE_Table[1,2]-CE_Table[3,2])
CE_Table[4,8] <- (CE_Table[1,2]-CE_Table[4,2])
CE_Table[5,8] <- (CE_Table[1,2]-CE_Table[5,2])


PSA_table[i,3] <- CE_Table[1,6]
PSA_table[i,4] <- CE_Table[2,6]
PSA_table[i,5] <- CE_Table[3,6]
PSA_table[i,6] <- CE_Table[4,6]
PSA_table[i,7] <- CE_Table[5,6]


PSA_table[i,8] <- CE_Table[1,7]
PSA_table[i,9] <- CE_Table[2,7]
PSA_table[i,10] <- CE_Table[3,7]
PSA_table[i,11] <- CE_Table[4,7]
PSA_table[i,12] <- CE_Table[5,7]

PSA_table[i,13] <- CE_Table[1,8]
PSA_table[i,14] <- CE_Table[2,8]
PSA_table[i,15] <- CE_Table[3,8]
PSA_table[i,16] <- CE_Table[4,8]
PSA_table[i,17] <- CE_Table[5,8]


}

PSA_table$PaxEffectRecip <- 1-PSA_table$PaxEffect

# Calculate net monetary benefit using various threshholds for hospitalization willingness to pay (from $1,000 to $20,000)

#$1,000 threshhold

PSA_table_1k <- PSA_table

PSA_table_1k$Strat1IncCost <- PSA_table_1k$Strat

PSA_table_1k$Strat0NMB <- 1000*PSA_table_1k$Strategy0_Hosp - PSA_table_1k$Strategy0_Cost
PSA_table_1k$Strat1NMB <- 1000*PSA_table_1k$Strategy1_Hosp - PSA_table_1k$Strategy1_Cost
PSA_table_1k$Strat2NMB <- 1000*PSA_table_1k$Strategy1_Hosp - PSA_table_1k$Strategy2_Cost
PSA_table_1k$Strat3NMB <- 1000*PSA_table_1k$Strategy2_Hosp - PSA_table_1k$Strategy3_Cost
PSA_table_1k$Strat4NMB <- 1000*PSA_table_1k$Strategy3_Hosp - PSA_table_1k$Strategy4_Cost

PSA_table_1k$BestNMB <- ifelse(PSA_table_1k$Strat0NMB>PSA_table_1k$Strat1NMB & PSA_table_1k$Strat0NMB>PSA_table_1k$Strat2NMB &
                                   PSA_table_1k$Strat0NMB>PSA_table_1k$Strat3NMB & PSA_table_1k$Strat0NMB>PSA_table_1k$Strat4NMB, "No nirmatrelvir/ritonavir",
                                 ifelse(PSA_table_1k$Strat1NMB>PSA_table_1k$Strat0NMB & PSA_table_1k$Strat1NMB>PSA_table_1k$Strat2NMB &
                                          PSA_table_1k$Strat1NMB>PSA_table_1k$Strat3NMB & PSA_table_1k$Strat1NMB>PSA_table_1k$Strat4NMB, "High-risk unvaccinated only",
                                        ifelse(PSA_table_1k$Strat2NMB>PSA_table_1k$Strat1NMB & PSA_table_1k$Strat2NMB>PSA_table_1k$Strat0NMB &
                                                 PSA_table_1k$Strat2NMB>PSA_table_1k$Strat3NMB & PSA_table_1k$Strat2NMB>PSA_table_1k$Strat4NMB, "All high-risk",
                                               ifelse(PSA_table_1k$Strat3NMB>PSA_table_1k$Strat1NMB & PSA_table_1k$Strat3NMB>PSA_table_1k$Strat2NMB &
                                                        PSA_table_1k$Strat3NMB>PSA_table_1k$Strat0NMB & PSA_table_1k$Strat3NMB>PSA_table_1k$Strat4NMB, "All high-risk and low-risk unvaccinated",
                                                      ifelse(PSA_table_1k$Strat4NMB>PSA_table_1k$Strat1NMB & PSA_table_1k$Strat4NMB>PSA_table_1k$Strat2NMB &
                                                               PSA_table_1k$Strat4NMB>PSA_table_1k$Strat3NMB & PSA_table_1k$Strat4NMB>PSA_table_1k$Strat0NMB, "All","None")))))




#$5,000 threhshold

PSA_table_5k <- PSA_table

PSA_table_5k$Strat1IncCost <- PSA_table_5k$Strat

PSA_table_5k$Strat0NMB <- 5000*PSA_table_5k$Strategy0_Hosp - PSA_table_5k$Strategy0_Cost
PSA_table_5k$Strat1NMB <- 5000*PSA_table_5k$Strategy1_Hosp - PSA_table_5k$Strategy1_Cost
PSA_table_5k$Strat2NMB <- 5000*PSA_table_5k$Strategy1_Hosp - PSA_table_5k$Strategy2_Cost
PSA_table_5k$Strat3NMB <- 5000*PSA_table_5k$Strategy2_Hosp - PSA_table_5k$Strategy3_Cost
PSA_table_5k$Strat4NMB <- 5000*PSA_table_5k$Strategy3_Hosp - PSA_table_5k$Strategy4_Cost

PSA_table_5k$BestNMB <- ifelse(PSA_table_5k$Strat0NMB>PSA_table_5k$Strat1NMB & PSA_table_5k$Strat0NMB>PSA_table_5k$Strat2NMB &
                                   PSA_table_5k$Strat0NMB>PSA_table_5k$Strat3NMB & PSA_table_5k$Strat0NMB>PSA_table_5k$Strat4NMB, "No nirmatrelvir/ritonavir",
                                 ifelse(PSA_table_5k$Strat1NMB>PSA_table_5k$Strat0NMB & PSA_table_5k$Strat1NMB>PSA_table_5k$Strat2NMB &
                                          PSA_table_5k$Strat1NMB>PSA_table_5k$Strat3NMB & PSA_table_5k$Strat1NMB>PSA_table_5k$Strat4NMB, "High-risk unvaccinated only",
                                        ifelse(PSA_table_5k$Strat2NMB>PSA_table_5k$Strat1NMB & PSA_table_5k$Strat2NMB>PSA_table_5k$Strat0NMB &
                                                 PSA_table_5k$Strat2NMB>PSA_table_5k$Strat3NMB & PSA_table_5k$Strat2NMB>PSA_table_5k$Strat4NMB, "All high-risk",
                                               ifelse(PSA_table_5k$Strat3NMB>PSA_table_5k$Strat1NMB & PSA_table_5k$Strat3NMB>PSA_table_5k$Strat2NMB &
                                                        PSA_table_5k$Strat3NMB>PSA_table_5k$Strat0NMB & PSA_table_5k$Strat3NMB>PSA_table_5k$Strat4NMB, "All high-risk and low-risk unvaccinated",
                                                      ifelse(PSA_table_5k$Strat4NMB>PSA_table_5k$Strat1NMB & PSA_table_5k$Strat4NMB>PSA_table_5k$Strat2NMB &
                                                               PSA_table_5k$Strat4NMB>PSA_table_5k$Strat3NMB & PSA_table_5k$Strat4NMB>PSA_table_5k$Strat0NMB, "All","None")))))



#$10,000 threshhold

PSA_table_10k <- PSA_table

PSA_table_10k$Strat0NMB <- 10000*PSA_table_10k$Strategy0_Hosp - PSA_table_10k$Strategy0_Cost
PSA_table_10k$Strat1NMB <- 10000*PSA_table_10k$Strategy1_Hosp - PSA_table_10k$Strategy1_Cost
PSA_table_10k$Strat2NMB <- 10000*PSA_table_10k$Strategy1_Hosp - PSA_table_10k$Strategy2_Cost
PSA_table_10k$Strat3NMB <- 10000*PSA_table_10k$Strategy2_Hosp - PSA_table_10k$Strategy3_Cost
PSA_table_10k$Strat4NMB <- 10000*PSA_table_10k$Strategy3_Hosp - PSA_table_10k$Strategy4_Cost

PSA_table_10k$BestNMB <- ifelse(PSA_table_10k$Strat0NMB>PSA_table_10k$Strat1NMB & PSA_table_10k$Strat0NMB>PSA_table_10k$Strat2NMB &
                                 PSA_table_10k$Strat0NMB>PSA_table_10k$Strat3NMB & PSA_table_10k$Strat0NMB>PSA_table_10k$Strat4NMB, "No nirmatrelvir/ritonavir",
                               ifelse(PSA_table_10k$Strat1NMB>PSA_table_10k$Strat0NMB & PSA_table_10k$Strat1NMB>PSA_table_10k$Strat2NMB &
                                        PSA_table_10k$Strat1NMB>PSA_table_10k$Strat3NMB & PSA_table_10k$Strat1NMB>PSA_table_10k$Strat4NMB, "High-risk unvaccinated only",
                                      ifelse(PSA_table_10k$Strat2NMB>PSA_table_10k$Strat1NMB & PSA_table_10k$Strat2NMB>PSA_table_10k$Strat0NMB &
                                               PSA_table_10k$Strat2NMB>PSA_table_10k$Strat3NMB & PSA_table_10k$Strat2NMB>PSA_table_10k$Strat4NMB, "All high-risk",
                                             ifelse(PSA_table_10k$Strat3NMB>PSA_table_10k$Strat1NMB & PSA_table_10k$Strat3NMB>PSA_table_10k$Strat2NMB &
                                                      PSA_table_10k$Strat3NMB>PSA_table_10k$Strat0NMB & PSA_table_10k$Strat3NMB>PSA_table_10k$Strat4NMB, "All high-risk and low-risk unvaccinated",
                                                    ifelse(PSA_table_10k$Strat4NMB>PSA_table_10k$Strat1NMB & PSA_table_10k$Strat4NMB>PSA_table_10k$Strat2NMB &
                                                             PSA_table_10k$Strat4NMB>PSA_table_10k$Strat3NMB & PSA_table_10k$Strat4NMB>PSA_table_10k$Strat0NMB, "All","None")))))

#$20,000 threshhold

PSA_table_20k <- PSA_table

PSA_table_20k$Strat0NMB <- 20000*PSA_table_20k$Strategy0_Hosp - PSA_table_20k$Strategy0_Cost
PSA_table_20k$Strat1NMB <- 20000*PSA_table_20k$Strategy1_Hosp - PSA_table_20k$Strategy1_Cost
PSA_table_20k$Strat2NMB <- 20000*PSA_table_20k$Strategy1_Hosp - PSA_table_20k$Strategy2_Cost
PSA_table_20k$Strat3NMB <- 20000*PSA_table_20k$Strategy2_Hosp - PSA_table_20k$Strategy3_Cost
PSA_table_20k$Strat4NMB <- 20000*PSA_table_20k$Strategy3_Hosp - PSA_table_20k$Strategy4_Cost

PSA_table_20k$BestNMB <- ifelse(PSA_table_20k$Strat0NMB>PSA_table_20k$Strat1NMB & PSA_table_20k$Strat0NMB>PSA_table_20k$Strat2NMB &
                                 PSA_table_20k$Strat0NMB>PSA_table_20k$Strat3NMB & PSA_table_20k$Strat0NMB>PSA_table_20k$Strat4NMB, "No nirmatrelvir/ritonavir",
                               ifelse(PSA_table_20k$Strat1NMB>PSA_table_20k$Strat0NMB & PSA_table_20k$Strat1NMB>PSA_table_20k$Strat2NMB &
                                        PSA_table_20k$Strat1NMB>PSA_table_20k$Strat3NMB & PSA_table_20k$Strat1NMB>PSA_table_20k$Strat4NMB, "High-risk unvaccinated only",
                                      ifelse(PSA_table_20k$Strat2NMB>PSA_table_20k$Strat1NMB & PSA_table_20k$Strat2NMB>PSA_table_20k$Strat0NMB &
                                               PSA_table_20k$Strat2NMB>PSA_table_20k$Strat3NMB & PSA_table_20k$Strat2NMB>PSA_table_20k$Strat4NMB, "All high-risk",
                                             ifelse(PSA_table_20k$Strat3NMB>PSA_table_20k$Strat1NMB & PSA_table_20k$Strat3NMB>PSA_table_20k$Strat2NMB &
                                                      PSA_table_20k$Strat3NMB>PSA_table_20k$Strat0NMB & PSA_table_20k$Strat3NMB>PSA_table_20k$Strat4NMB, "All high-risk and low-risk unvaccinated",
                                                    ifelse(PSA_table_20k$Strat4NMB>PSA_table_20k$Strat1NMB & PSA_table_20k$Strat4NMB>PSA_table_20k$Strat2NMB &
                                                             PSA_table_20k$Strat4NMB>PSA_table_20k$Strat3NMB & PSA_table_20k$Strat4NMB>PSA_table_20k$Strat0NMB, "All","None")))))


# Create plots of net monetary benefit



#Plots

pdf(file = "Figures/Figure2.Hosps_7.28.22.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 4) # The height of the plot in inches

plot_1k <- 
  ggplot(data=PSA_table_1k, aes(x=PSA_table_1k$PaxCost, y=PSA_table_1k$PaxEffectRecip, fill=PSA_table_1k$BestNMB)) + geom_tile()
plot_1k+  xlab("Cost of nirmatrelvir/ritonavir treatment, $") + ylab("Effectiveness of nirmatrelvir/ritonavir to \nprevent hospitalization")+
  theme_classic() + scale_fill_manual(name="Best strategy according to $1k willingness to pay per \nhospitalization averted",
                                      values=c("No nirmatrelvir/ritonavir" = "#edf8fb",
                                               "High-risk unvaccinated only"= "#b2e2e2",
                                               "All high-risk"= "#66c2a4",
                                               "All high-risk and low-risk unvaccinated"= "#2ca25f",
                                               "All" = "#006d2c"))

plot_5k <- 
  ggplot(data=PSA_table_5k, aes(x=PSA_table_5k$PaxCost, y=PSA_table_5k$PaxEffectRecip, fill=PSA_table_5k$BestNMB)) + geom_tile()
plot_5k+ xlab("Cost of nirmatrelvir/ritonavir treatment, $") + ylab("Effectiveness of nirmatrelvir/ritonavir to \nprevent hospitalization") +
  theme_classic()+ scale_fill_manual(name="Best strategy according to $5k willingness to pay per \nhospitalization averted",
                                                       values=c("No nirmatrelvir/ritonavir" = "#edf8fb",
                                                                "High-risk unvaccinated only"= "#b2e2e2",
                                                                "All high-risk"= "#66c2a4",
                                                                "All high-risk and low-risk unvaccinated"= "#2ca25f",
                                                                "All" = "#006d2c"))



plot_10k <- ggplot(data=PSA_table_10k, aes(x=PSA_table_10k$PaxCost, y=PSA_table_10k$PaxEffectRecip, fill=PSA_table_10k$BestNMB)) + geom_tile()
plot_10k+xlab("Cost of nirmatrelvir/ritonavir treatment, $") + ylab("Effectiveness of nirmatrelvir/ritonavir to \nprevent hospitalization")+
  theme_classic()+ scale_fill_manual(name="Best strategy according to $10k willingness to pay per \nhospitalization averted",
                                     values=c("No nirmatrelvir/ritonavir" = "#edf8fb",
                                              "High-risk unvaccinated only"= "#b2e2e2",
                                              "All high-risk"= "#66c2a4",
                                              "All high-risk and low-risk unvaccinated"= "#2ca25f",
                                              "All" = "#006d2c"))


plot_20k <-ggplot(data=PSA_table_20k, aes(x=PSA_table_20k$PaxCost, y=PSA_table_20k$PaxEffectRecip, fill=PSA_table_20k$BestNMB)) + geom_tile()
plot_20k+ xlab("Cost of nirmatrelvir/ritonavir treatment, $") + ylab("Effectiveness of nirmatrelvir/ritonavir to \nprevent hospitalization")+
  theme_classic()+ scale_fill_manual(name="Best strategy according to $20k willingness to pay per \nhospitalization averted",
                                     values=c("No nirmatrelvir/ritonavir" = "#edf8fb",
                                              "High-risk unvaccinated only"= "#b2e2e2",
                                              "All high-risk"= "#66c2a4",
                                              "All high-risk and low-risk unvaccinated"= "#2ca25f",
                                              "All" = "#006d2c"))

dev.off()