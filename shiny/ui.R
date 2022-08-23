# 
# Shiny app for MSIS model
#
# Created by Zhe Zheng
#

library("shiny")
library("deSolve")
library("cowplot")
library("ggplot2")
library("tidyverse")
library("ggrepel")
library("shinydashboard")
library("shinyWidgets")
library("plotly")
#
# Define UI 
#

ui <- fluidPage(
  titlePanel("Modeling Reemergent RSV"),
  hr(),
  p(div(HTML("Disclaimer: This simulation is for research and educational purposes only and is not intended to be a tool for epidemic prediction. There are many uncertainties and debates about the details of RSV infection and transmission and there are many limitations to this simple model. This work is licensed under a <a href=https://creativecommons.org/licenses/by-sa/4.0/> Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) License </a>"))),
  
  
  sidebarLayout(
    sidebarPanel(
      
      fluidRow(
        column(width=6,
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               h4(div(HTML("<em>Set fixed parameters...</em>"))),
               sliderInput("Mu_m",
                           "Per capita death/emmigration rate (months):",
                           min = -0.001, max = 0.001, value = -0.0003, step=0.0001
               ),
               # radioButtons("freqDepend", "Frequency dependent transmission",
               #              choices = list("Yes" = 1,"No" = 0),inline=TRUE, selected="Yes"),
               sliderInput("infdur1",
                           "Infectious duration of the 1st infection (days):",
                           min = 1, max = 30, value = 10
               ),
               sliderInput("infdur2",
                           "Infectious duration of the 2st infection (days):",
                           min = 1, max = 30, value = 7
               ),
               sliderInput("infdur3",
                           "Infectious duration of the 3st infection (days):",
                           min = 1, max = 30, value = 5
               ),
               sliderInput("rr1",
                           "Relative risk of infection following 1st infection:",
                           min = 0, max = 1, value = 0.76
               ),
               sliderInput("rr2",
                           "Relative risk of infection following 2st infection:",
                           min = 0, max = 1, value = 0.6
               ),
               sliderInput("rr3",
                           "Relative risk of infection following 3rd infection:",
                           min = 0, max = 1, value = 0.4
               ),
               sliderInput("rho1",
                           "Relative infectiousness for the 2nd infection:",
                           min = 0, max = 1, value = 0.75
               ),
               sliderInput("rho2",
                           "Relative infectiousness for the subsequent infections:",
                           min = 0, max = 1, value = 0.51
               ),
               
               br(),
               
        ),
        column(width=6,
               
               h4(div(HTML("<em>Set transmission parameters...</em>"))),
               sliderInput("baseline.txn.rate",
                           "Approximate R0:",
                           min = 6, max = 15, value = 8.5, step=0.1
               ),
               sliderInput("amp",
                           "Amplitude of transmission seasonality:",
                           0, 0.5, 0.13, step=0.01
               ),
               sliderInput("phase",
                           "Peak timing of epidemics:",
                           0, 2*pi, 3.56, step=0.01
               ),
               radioButtons("AllowWaning", "Existing immunity wanes during lockdown?",
                            choices = list("Yes" = "Yes","No" = "No"),inline=TRUE, selected="Yes"),
                conditionalPanel(
                  condition="input.AllowWaning == 'Yes'",
              sliderInput("durimmunity1", "Duration of boosted immunity (days)", 0, 400, 180, step=1, post="days"),
               sliderInput("durimmunity2",
                           "Duration of partially boosted immunity (days):",
                           min = 0, max = 400, value = 200, step=1, post="days"
               )),
               sliderInput("DurationMatImmunityDays",
                           "Duration of maternal immunity (days):",
                           min = 0, max = 150, value = 112, step=1
               ),
               hr(),
               
        )
      ),
      h4(div(HTML("<em>Set simulation values...</em>"))),
      fluidRow(
        column(width=6,
               numericInput("popsize", div(HTML("Initial population size (Millions):")), value=16, max=300, min=1, step=1)
        ),
        column(width=6,
               sliderInput("constant_strict","Percentage decrease in transmission rate:",value = 25, min = 1, max=30,step = 1,post="%")
        ),
        sliderInput("NPI_durUnittime_1",
                    "Duration of constant NPIs:",
                    min = 0, max = 100, value = 11, step=1
        ),
        sliderInput("NPI_durUnittime_2",
                    "Duration of gradual NPIs relaxation:",
                    min = 0, max = 100, value = 3, step=1
        ),
        sliderInput("I_ex",
                    "Amount of virus importation (per 1 million travelers per month):",
                    min = 0, max = 100, value = 0, step=1
        )),
      #br(),
      width=4
    ),
    
    mainPanel(
      
      navbarPage("Output:",
                 
                 tabPanel("Observed hospitalizations",
                          fluidPage(
                            fluidRow(
                              h3("Projected RSV hospitalizations"),
                              p("Simulate the observed RSV epidemic in the Dutch population after the interuption of COVID-19 mitigation measures."),
                              plotlyOutput("Plot_flex"),
                              br(),
                              br(),
                              wellPanel(
                                h4(div(HTML("<em>Set clinical parameters...</em>"))),
                               sliderInput("report_fraction",
                                            "reporting fraction:",
                                            min = 0, max = 1, value = 0.5, step=0.01
                                ),
                               sliderInput("Catchmentarea",
                                            "Catchment area:",
                                            min = 0, max = 1, value = 0.1, step=0.01
                                )
                                #p(HTML("<b>Reference: Epidemiology of respiratory syncytial virus in a community birth cohort of infants in the first 2 years of life, 2021, Mari D. Takashima at al.")),
                                # 
                                # p(HTML("<b>User instructions:</b> The graph shows the expected incidence of RSV hospitalization over time without an covid-19 pandemic. A more detailed description of the model is provided in the Model Description tab. The population size, initial condition, and parameter values used to simulate the spread of infection can be specified through the sliders located in the left-hand panel. Default slider values are equal to estimates taken from the literature (see Sources tab). The strength and timing of the intervention is controlled by the sliders below the plot."))
                              ),
                            )
                          )
                  ),
                 tabPanel("Transmission dynamics",
                          fluidPage(
                            fluidRow(
                              h3("Simulated RSV transmission dynamics"),
                              p("Simulate the change of RSV susceptible populations and infectious population in the time course before and after COVID-19 pandemic"),
                              plotlyOutput("Plot_transmission"),
                              br(),
                              br(),
                              wellPanel(
                             p("The graph shows the expected numbers of individuals who are susceptible or infected over time. A more detailed description of the model is provided in the Model Description tab. The population size, initial condition, and parameter values used to simulate the spread of infection can be specified through the sliders located in the left-hand panel. Default slider values are equal to estimates taken from the literature and manuscript XXX. The strength and timing of the intervention is controlled by the sliders left to the plot.")
                             ),
                            )
                         )),
                 tabPanel("Population dynamics",
                          fluidPage(
                            fluidRow(
                              h3("Simulated population growth"),
                              p("Simulate the growth of total population"),
                              plotlyOutput("Plot_population"),
                              br(),
                              br(),
                              wellPanel(
                                p("The graph shows the expected size of population. It has seasonal fluctation because the input birth rate consider seasonal birth rate while the death rate is constant. User can change the birth rate as User define by changing the input birth rate dataset in Server.R file")
                              ),
                            )
                          )),
                 tabPanel("Model Structure", br(),
                          fluidRow(column(12,
                                          withMathJax(),
                                          h2("Model Description"),
                                          plotOutput("plot4", height=500),
                                          #includeMarkdown("MSIS.Rmd"),
                                          #h3("Equations"),
                                          br(),
                          ))),
                 br(),
                 br()
      )
    )) 
)