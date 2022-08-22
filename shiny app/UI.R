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
  titlePanel("Modeling Re-emergent RSV"),
  hr(),
  p(div(HTML("Disclaimer: This simulation is for research and educational purposes only and is not intended to be a tool for epidemic prediction. There are many uncertainties and debates about the details of RSV infection and transmission and there are many limitations to this simple model. This work is licensed under a <a href=https://creativecommons.org/licenses/by-sa/4.0/> Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0) License </a>"))),
  
  
  sidebarLayout(
    sidebarPanel(
      
      fluidRow(
        column(width=6,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               h4(div(HTML("<em>Set fixed parameters...</em>"))),
               sliderInput("Mu_m",
                           "Per capita death rate (months):",
                           min = -0.001, max = 0.001, value = 0.0002, step=0.0001
               ),
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
               sliderInput("tau",
                           "Probability of infection given contact between a susceptible and infected individual:",
                           min = 0, max = 1, value = 0.6, step=0.01
               ),
               sliderInput("contactrate",
                           "Average rate of contact between susceptible and infected individuals (per week):",
                           min = .5, max = 30, value = 10
               ),
               sliderInput("amp",
                           "Amplitude of transmission seasonality:",
                           0, 100, 25, step=1, post="%"
               ),
               radioButtons("AllowReset", "Existing immunity wanes during lockdown?",
                            choices = list("Yes" = "Yes","No" = "No"),inline=TRUE, selected="No"),
               # conditionalPanel(
               #   condition="input.AllowSeason == 'Yes'",
               #   sliderInput("seas.amp", "Amplitude of seasonality", 0, 100, 0, step=1, post="%")),
               sliderInput("phase",
                           "Peak timing of transmission seasonality:",
                           min = 0, max = 2*pi, value = 1.96, step=0.01
               ),
               sliderInput("DurationMatImmunityDays",
                           "Duration of maternal immunity (days):",
                           min = 0, max = 150, value = 90, step=1
               ),
               hr(),
               
        )
      ),
      h4(div(HTML("<em>Set simulation values...</em>"))),
      fluidRow(
        column(width=6,
               numericInput("popsize", div(HTML("Initial population size (Millions):")), value=10, max=300, min=1, step=10)
        ),
        column(width=6,
               numericInput("pinf","Initial # infected:",value = 1, min = 1, step = 1)
        ),
        sliderInput("I_ex",
                    "Amount of virus importation (per 1 million people per week):",
                    min = 0, max = 100, value = 10, step=1
        )),
      #br(),
      
      sliderInput("timeframe", div(HTML("Maximum time")),0, 600, 318, step=10, post=" weeks"),
      width=4
    ),
    
    mainPanel(
      
      navbarPage("Output:",
                 
                 tabPanel("Seasonal Epidemics",
                          fluidPage(
                            fluidRow(
                              h3("Seasonal RSV Epidemics without interuption"),
                              p(HTML("Simulate the natural course of RSV epidemic in Dutch population without any interventions.")),
                              plotlyOutput("Plot_flex"),
                              br(),
                              br(),
                              wellPanel(
                                h4(div(HTML("<em>Set clinical parameters...</em>"))),
                                sliderInput(
                                  "hosp1","Risk of hospitalization of the first time infection:",value = 0.14, min = 0,max=0.2, step = 0.001
                                ),
                                # p(HTML("<b>Reference: The Natural History of Respiratory Syncytial Virus in a Birth Cohort: The Influence of Age and Previous Infection on Reinfection and Disease, 2012, E. O. Ohuma at al.")),
                                sliderInput("hosp2",
                                            "Risk of hospitalization of the second time infection:",value = 0.048, min = 0,max=0.1, step=0.001),
                                sliderInput("hosp3",
                                            "Risk of hospitalization of subsequent infection:",
                                            value = 0.01, min = 0,max=0.1, step=0.001),
                                sliderInput("Report",
                                            "Reporting fraction:",
                                            min = 0, max = 1, value = 0.8, step=0.05
                                )
                                #p(HTML("<b>Reference: Epidemiology of respiratory syncytial virus in a community birth cohort of infants in the first 2 years of life, 2021, Mari D. Takashima at al.")),
                                # 
                                # p(HTML("<b>User instructions:</b> The graph shows the expected incidence of RSV hospitalization over time without an covid-19 pandemic. A more detailed description of the model is provided in the Model Description tab. The population size, initial condition, and parameter values used to simulate the spread of infection can be specified through the sliders located in the left-hand panel. Default slider values are equal to estimates taken from the literature (see Sources tab). The strength and timing of the intervention is controlled by the sliders below the plot."))
                              ),
                         )
                            )
                            ),
                 tabPanel("Re-emergent Epidemics",
                          fluidPage(
                            fluidRow(
                              h3("Simulated RSV activites after re-introduction"),
                              p(HTML("Simulate the change in the time course of RSV cases after intervention and virus re-introduction")),
                              plotlyOutput("Plot_Intervention"),
                              br(),
                              br(),
                              wellPanel(
                                h4(div(HTML("<em>Set intervention parameters...</em>"))),
                                sliderInput(
                                       "Tint","Duration of Intervention (weeks):",value = 52, min = 0,max=100, step = 1
                                ),
                                p(HTML("<b>Intervention type: reducing transmission, </b> for example via social distancing or travel restriction.")),
                                sliderInput("TD", "Reduction in transmission from infections ", min = 0, max = 100, value = 30, post="%",step=1, animate=TRUE),
                                p(HTML("<b>User instructions:</b> The graph shows the expected numbers of individuals over time who are infected over time, with and without an intervention, with and without virus importation. A more detailed description of the model is provided in the Model Description tab. The population size, initial condition, and parameter values used to simulate the spread of infection can be specified through the sliders located in the left-hand panel. Default slider values are equal to estimates taken from the literature (see Sources tab). The strength and timing of the intervention is controlled by the sliders below the plot."))
                              )
                            )
                          )),
                 br(),
                 br()
      )
    )) 
)