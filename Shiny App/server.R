library(shiny)
library(deSolve)
library(ggplot2)
library(reshape2)
library(tidyverse)



shinyServer(function(input,output) {
    
    output$I_plot <- renderPlot({

        ############# Multiple plot function ###################
        ## http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ ##

        multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
            library(grid)

            # Make a list from the ... arguments and plotlist
            plots <- c(list(...), plotlist)

            numPlots = length(plots)

            # If layout is NULL, then use 'cols' to determine layout
            if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
            }

            if (numPlots==1) {
                print(plots[[1]])

            } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                    # Get the i,j matrix positions of the regions that contain this subplot
                    matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

                    print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                    layout.pos.col = matchidx$col))
                }
            } 
        }
        
# Paxlovid Model with PSA 
        
        # Inputs
        
        
        COVID_highrisk_hosp_prob = input$hospprob #0.063
        COVID_lowrisk_hosp_prob = input$hospprob_lr #COVID_highrisk_hosp_prob/5
        #Paxlovid_hosp_multiplier = 1-0.873
        Vaccine_hosp_multiplier = 1-input$vacceff
        Pax_vax_multi <- input$pax_vax_multi
        
        
        Death_hosp_prob = input$deathprob
        
        US_highrisk_p = input$highriskP #0.6
        US_vaccinated_p = input$vaccinated_p #0.656
        #PSA_table[i,2] = 530
        Cost_Hosp_US = input$hospcost
        
        Strategies <- c("Strategy0", "Strategy1", "Strategy2", "Strategy3", "Strategy4")
        CE_Table <- data.frame(matrix(NA, nrow = 5, ncol = 8))
        
        row.names(CE_Table) <- Strategies
        colnames(CE_Table) <- c("Cost","Hospitalizations","Deaths","ICER Hospitalizations","ICER Deaths",
                                "Inc Cost","Inc Benefit Death","Inc Benefit Hosp")
        
        PSA <- 1000
        
        PSA_table <- data.frame(matrix(NA, nrow = PSA, ncol = 18))
        colnames(PSA_table) <- c("PaxEffect", "PaxCost","Strategy0_Cost","Strategy1_Cost"
                                 , "Strategy2_Cost","Strategy3_Cost", "Strategy4_Cost","Strategy0_Death"
                                 , "Strategy1_Death","Strategy2_Death", "Strategy3_Death","Strategy4_Death","PaxEffectVaccinated","Strategy0_Hosp"
                                 , "Strategy1_Hosp","Strategy2_Hosp", "Strategy3_Hosp","Strategy4_Hosp")
        
        
        PSA_table$PaxEffect <- rep(seq(0.1,1, by=0.1),100)
        PSA_table$PaxCost <- rep(seq(20,2000, by=20), each=10)
        PSA_table$PaxEffectVaccinated <- 1-exp(log(1-PSA_table$PaxEffect)* input$pax_vax_multi)
  
# Model
        
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
          
          Scenario2_Hospitalizations <- sum(HR_UV_Hosp*PSA_table[i,1], HR_V_Hosp*PSA_table[i,13], LR_UV_Hosp, LR_V_Hosp)
          Scenario2_Deaths <- sum(HR_UV_Hosp*Death_hosp_prob*PSA_table[i,1], HR_V_Hosp*PSA_table[i,13]*Death_hosp_prob, LR_UV_Hosp*Death_hosp_prob, LR_V_Hosp*Death_hosp_prob)
          Scenario2_Costs <- sum(HR_UV_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), HR_UV_unHosp*PSA_table[i,2],  HR_V_Hosp*PSA_table[i,13]*(Cost_Hosp_US+PSA_table[i,2]), HR_V_unHosp*PSA_table[i,2], LR_UV_Hosp*Cost_Hosp_US, LR_V_Hosp*Cost_Hosp_US)
          
          #Scenario 3
          
          Scenario3_Hospitalizations <- sum(HR_UV_Hosp*PSA_table[i,1], HR_V_Hosp*PSA_table[i,13], LR_UV_Hosp*PSA_table[i,1], LR_V_Hosp)
          Scenario3_Deaths <- sum(HR_UV_Hosp*Death_hosp_prob*PSA_table[i,1], HR_V_Hosp*PSA_table[i,13]*Death_hosp_prob, LR_UV_Hosp*PSA_table[i,1]*Death_hosp_prob, LR_V_Hosp*Death_hosp_prob)
          Scenario3_Costs <- sum(HR_UV_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), HR_UV_unHosp*PSA_table[i,2],  HR_V_Hosp*PSA_table[i,13]*(Cost_Hosp_US+PSA_table[i,2]), HR_V_unHosp*PSA_table[i,2], LR_UV_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), LR_UV_unHosp*PSA_table[i,2], LR_V_Hosp*Cost_Hosp_US)
          
          #Scenario 4
          
          Scenario4_Hospitalizations <- sum(HR_UV_Hosp*PSA_table[i,1], HR_V_Hosp*PSA_table[i,13], LR_UV_Hosp*PSA_table[i,1], LR_V_Hosp*PSA_table[i,13])
          Scenario4_Deaths <- sum(HR_UV_Hosp*Death_hosp_prob*PSA_table[i,1], HR_V_Hosp*PSA_table[i,13]*Death_hosp_prob, LR_UV_Hosp*PSA_table[i,1]*Death_hosp_prob, LR_V_Hosp*PSA_table[i,13]*Death_hosp_prob)
          Scenario4_Costs <- sum(HR_UV_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), 
                                 HR_UV_unHosp*PSA_table[i,2],  HR_V_Hosp*PSA_table[i,13]*(Cost_Hosp_US+PSA_table[i,2]), 
                                 HR_V_unHosp*PSA_table[i,2], LR_UV_Hosp*PSA_table[i,1]*(Cost_Hosp_US+PSA_table[i,2]), 
                                 LR_UV_unHosp*PSA_table[i,2], LR_V_Hosp*PSA_table[i,13]*(Cost_Hosp_US+PSA_table[i,2]),
                                 LR_V_unHosp*PSA_table[i,2])
          
          
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
            
            PSA_table[i,14] <- CE_Table[1,8]
            PSA_table[i,15] <- CE_Table[2,8]
            PSA_table[i,16] <- CE_Table[3,8]
            PSA_table[i,17] <- CE_Table[4,8]
            PSA_table[i,18] <- CE_Table[5,8]
            
            
        }
        
        PSA_table$PaxEffectRecip <- 1-PSA_table$PaxEffect
        
        #With threshhold, hospitalizations
        
        threshhold_hosp <-as.numeric(input$threshhold_hosp)
        
        PSA_table$Strat1IncCost <- PSA_table$Strat
        
        PSA_table$Strat0NMB_hosp <- threshhold_hosp*PSA_table$Strategy0_Hosp - PSA_table$Strategy0_Cost
        PSA_table$Strat1NMB_hosp <- threshhold_hosp*PSA_table$Strategy1_Hosp - PSA_table$Strategy1_Cost
        PSA_table$Strat2NMB_hosp <- threshhold_hosp*PSA_table$Strategy1_Hosp - PSA_table$Strategy2_Cost
        PSA_table$Strat3NMB_hosp <- threshhold_hosp*PSA_table$Strategy2_Hosp - PSA_table$Strategy3_Cost
        PSA_table$Strat4NMB_hosp <- threshhold_hosp*PSA_table$Strategy3_Hosp - PSA_table$Strategy4_Cost
        
        PSA_table$BestNMB_hosp <- ifelse(PSA_table$Strat0NMB_hosp>PSA_table$Strat1NMB_hosp & PSA_table$Strat0NMB_hosp>PSA_table$Strat2NMB_hosp &
                                      PSA_table$Strat0NMB_hosp>PSA_table$Strat3NMB_hosp & PSA_table$Strat0NMB_hosp>PSA_table$Strat4NMB_hosp, "No antiviral treatment",
                                    ifelse(PSA_table$Strat1NMB_hosp>PSA_table$Strat0NMB_hosp & PSA_table$Strat1NMB_hosp>PSA_table$Strat2NMB_hosp &
                                             PSA_table$Strat1NMB_hosp>PSA_table$Strat3NMB_hosp & PSA_table$Strat1NMB_hosp>PSA_table$Strat4NMB_hosp, "High-risk unvaccinated only",
                                           ifelse(PSA_table$Strat2NMB_hosp>PSA_table$Strat1NMB_hosp & PSA_table$Strat2NMB_hosp>PSA_table$Strat0NMB_hosp &
                                                    PSA_table$Strat2NMB_hosp>PSA_table$Strat3NMB_hosp & PSA_table$Strat2NMB_hosp>PSA_table$Strat4NMB_hosp, "All high-risk",
                                                  ifelse(PSA_table$Strat3NMB_hosp>PSA_table$Strat1NMB_hosp & PSA_table$Strat3NMB_hosp>PSA_table$Strat2NMB_hosp &
                                                           PSA_table$Strat3NMB_hosp>PSA_table$Strat0NMB_hosp & PSA_table$Strat3NMB_hosp>PSA_table$Strat4NMB_hosp, "All high-risk and low-risk unvaccinated",
                                                         ifelse(PSA_table$Strat4NMB_hosp>PSA_table$Strat1NMB_hosp & PSA_table$Strat4NMB_hosp>PSA_table$Strat2NMB_hosp &
                                                                  PSA_table$Strat4NMB_hosp>PSA_table$Strat3NMB_hosp & PSA_table$Strat4NMB_hosp>PSA_table$Strat0NMB_hosp, "All","None")))))
        
        
        
        
        plot_hosp<- 
          ggplot(data=PSA_table, aes(x=PSA_table$PaxCost, y=PSA_table$PaxEffectRecip, fill=PSA_table$BestNMB_hosp)) + geom_tile() +xlab("Cost of antiviral treatment, $") + 
          ylab("Effectiveness of antiviral treatment to \nprevent hospitalization \n(unvaccinated)")+
          theme_classic() + scale_fill_manual(name="Best strategy according to willingness to pay per \nhospitalization averted",
                                              values=c("No antiviral treatment" = "#edf8fb",
                                                       "High-risk unvaccinated only"= "#b2e2e2",
                                                       "All high-risk"= "#66c2a4",
                                                       "All high-risk and low-risk unvaccinated"= "#2ca25f",
                                                       "All" = "#006d2c"))  +
          scale_x_continuous(labels=scales::dollar_format())+ theme(text = element_text(size = 14))
        
        #With threshhold, deaths
        
        threshhold <-as.numeric(input$threshhold)
        
        PSA_table$Strat1IncCost <- PSA_table$Strat
        
        PSA_table$Strat0NMB <- threshhold*PSA_table$Strategy0_Death - PSA_table$Strategy0_Cost
        PSA_table$Strat1NMB <- threshhold*PSA_table$Strategy1_Death - PSA_table$Strategy1_Cost
        PSA_table$Strat2NMB <- threshhold*PSA_table$Strategy1_Death - PSA_table$Strategy2_Cost
        PSA_table$Strat3NMB <- threshhold*PSA_table$Strategy2_Death - PSA_table$Strategy3_Cost
        PSA_table$Strat4NMB <- threshhold*PSA_table$Strategy3_Death - PSA_table$Strategy4_Cost
        
        PSA_table$BestNMB <- ifelse(PSA_table$Strat0NMB>PSA_table$Strat1NMB & PSA_table$Strat0NMB>PSA_table$Strat2NMB &
                                            PSA_table$Strat0NMB>PSA_table$Strat3NMB & PSA_table$Strat0NMB>PSA_table$Strat4NMB, "No antiviral treatment",
                                        ifelse(PSA_table$Strat1NMB>PSA_table$Strat0NMB & PSA_table$Strat1NMB>PSA_table$Strat2NMB &
                                                   PSA_table$Strat1NMB>PSA_table$Strat3NMB & PSA_table$Strat1NMB>PSA_table$Strat4NMB, "High-risk unvaccinated only",
                                               ifelse(PSA_table$Strat2NMB>PSA_table$Strat1NMB & PSA_table$Strat2NMB>PSA_table$Strat0NMB &
                                                          PSA_table$Strat2NMB>PSA_table$Strat3NMB & PSA_table$Strat2NMB>PSA_table$Strat4NMB, "All high-risk",
                                                      ifelse(PSA_table$Strat3NMB>PSA_table$Strat1NMB & PSA_table$Strat3NMB>PSA_table$Strat2NMB &
                                                                 PSA_table$Strat3NMB>PSA_table$Strat0NMB & PSA_table$Strat3NMB>PSA_table$Strat4NMB, "All high-risk and low-risk unvaccinated",
                                                             ifelse(PSA_table$Strat4NMB>PSA_table$Strat1NMB & PSA_table$Strat4NMB>PSA_table$Strat2NMB &
                                                                        PSA_table$Strat4NMB>PSA_table$Strat3NMB & PSA_table$Strat4NMB>PSA_table$Strat0NMB, "All","None")))))
        
        
        
        
        plot<- 
            ggplot(data=PSA_table, aes(x=PSA_table$PaxCost, y=PSA_table$PaxEffectRecip, fill=PSA_table$BestNMB)) + geom_tile() +xlab("Cost of antiviral treatment, $") + 
          ylab("Effectiveness of antiviral treatment to \nprevent hospitalization \n(unvaccinated)")+
            theme_classic() + scale_fill_manual(name="Best strategy according to willingness to pay per \ndeath averted",
                                                values=c("No antiviral treatment" = "#edf8fb",
                                                         "High-risk unvaccinated only"= "#b2e2e2",
                                                         "All high-risk"= "#66c2a4",
                                                         "All high-risk and low-risk unvaccinated"= "#2ca25f",
                                                         "All" = "#006d2c"))  +
          scale_x_continuous(labels=scales::dollar_format())+ theme(text = element_text(size = 14))
        
        multiplot(plot_hosp,plot,cols=1)
        
    })
    
})
