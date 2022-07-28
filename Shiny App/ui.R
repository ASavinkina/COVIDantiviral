library(shiny)

shinyUI(fluidPage(
	titlePanel("Determination of most cost-effective strategy for COVID-19 antiviral treatment 
	           allocation for deaths averted and hospitalizations averted, according to given willingness-to-pay threshhold"),
	  sidebarLayout(
		sidebarPanel(
			helpText("Select COVID-19 and treatment parameters for scenario analysis:"),
			#sliderInput("threshhold",label="Willingness to pay threshhold, per death averted",min=10000, max=5000000, value=100000, step=10000),
		  radioButtons("threshhold_hosp",label="Willingness to pay threshhold, per hospitalization averted",choices = list("$1,000"=1000,
		                                                                                                 "$5,000"=5000,
		                                                                                                 "$10,000"=10000,
		                                                                                                 "$15,000"=15000,
		                                                                                                 "$20,000"=20000,
		                                                                                                 "$50,000"=50000)),
		  radioButtons("threshhold",label="Willingness to pay threshhold, per death averted",choices = list("$1,000"=1000,
		                                                                                                    "$10,000"=10000,
		                                                                                                    "$100,000"=100000,
		                                                                                                    "$1 million" =1000000,
		                                                                                                    "$5 million" =5000000)),
			sliderInput("hospcost",label="Average cost of a COVID hospitalization, US",min=1000, max=90000, value=20000),
			sliderInput("highriskP",label="Proportion of population high-risk for severe COVID",min=0, max=1, value=0.6),
			sliderInput("vaccinated_p",label="Proportion of population vaccinated for COVID",min=0, max=1, value=0.656),
			sliderInput("hospprob",label="Probability of hospitalization from COVID, high-risk",min=0.01, max=0.20, value=0.063),
			sliderInput("hospprob_lr",label="Probability of hospitalization from COVID, low-risk",min=0.005, max=0.10, value=0.063/5),
			sliderInput("deathprob",label="Probability of death from COVID, given hospitalization",min=0.01, max=0.20, value=0.07),
			sliderInput("vacceff",label="Vaccine effectiveness against hospitalization",min=0.1, max=0.9, value=0.57),
			sliderInput("pax_vax_multi",label="Multiplyer for how much less effective antiviral treatment \nis in unvaccinated (1x meaning equally effective, 5x meaning 5 times less effective)",min=1, max=5, value=1)
			),
	
		mainPanel(
			plotOutput("I_plot", width = "100%", height="800px")
		  )
		)	
					
))