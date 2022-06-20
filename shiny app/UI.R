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
               sliderInput("B_m",
                           "Per capita births rate (months):",
                           min = 0, max = 0.001, value = 0.0008, step=0.0001
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
               sliderInput("R0",
                           "Basic reproductive number (R0, # persons):",
                           min = 0, max = 20, value = 10.035, step=0.01
               ),
               sliderInput("contactrate",
                           "Average rate of contact between susceptible and infected individuals (per week):",
                           min = .5, max = 20, value = 10
               ),
               sliderInput("amp",
                           "Amplitude of transmission seasonality:",
                           min = 0, max = 2, value = 0.35, step=0.01
               ),
               sliderInput("phase",
                           "Peak timing of transmission seasonality:",
                           min = 0, max = 3.14, value = 1.53, step=0.01
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
               numericInput("popsize", div(HTML("Population size (millions):")), value=10, max=300, min=1, step=1)
        ),
        column(width=6,
               numericInput("pinf","Initial # infected:",value = 100, min = 1, step = 1)
        )),
      #br(),
      
      sliderInput("timeframe", div(HTML("Maximum time")),0, 600, 318, step=10, post=" weeks"),
      checkboxInput("lockdown", "Lockdown", FALSE), 
      width=4
    ),
    
    mainPanel(
      
      navbarPage("Output:",
                 
                 tabPanel("Seasonal Epidemics",
                          fluidPage(
                            fluidRow(
                              h3("Seasonal RSV Epidemics without interuption"),
                              p(HTML("Simulate the natural course of RSV epidemic in Dutch population without any interventions.")),
                              plotlyOutput("Plot_Normal")
                            ),
                            br())),
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
                                column(width=6,
                                       numericInput("Tint","Duration of Intervention (weeks):",value = 52, min = 0, step = 1)
                                ),
                                column(width=6,
                                       numericInput("virus_import",
                                                    "Time at virus importation (weeks):",value = 53, min = 0, step=1)),
                                sliderInput("I_ex",
                                            "Amount of virus importation (per 1 million people per week):",
                                            min = 0, max = 100, value = 0, step=5
                                ),
                                p(HTML("<b>Intervention type: reducing transmission, </b> for example via social distancing or travel restriction.")),
                                sliderInput("s1", "Reduction in transmission from infections ", 0, 100, 30, pre="%",step=1, animate=TRUE),
                                p(HTML("<b>User instructions:</b> The graph shows the expected numbers of individuals over time who are infected over time, with and without an intervention, with and without virus importation. A more detailed description of the model is provided in the Model Description tab. The population size, initial condition, and parameter values used to simulate the spread of infection can be specified through the sliders located in the left-hand panel. Default slider values are equal to estimates taken from the literature (see Sources tab). The strength and timing of the intervention is controlled by the sliders below the plot."))
                              )
                            )
                          )),
                 br(),
                 br()
      )
    )) 
)