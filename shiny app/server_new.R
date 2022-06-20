# 
# Shiny app for SIRS model
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

# Define server 
#
server <- function(input, output) {
  
  # Create reactive input
  simMSIS_Normal <- reactive({
    init       <-
      c(M=0,
        S0 =  input$popsize*1000000-input$pinf,
        I1 = input$pinf, 
        S1 = 0,
        I2=0,
        S2= 0,
        I3=0,
        S3=0,
        I4=0
      )
    ## beta: infection parameter; gamma: duration of infectiousness
    length.step=7 # weekly
    
    parameters_Normal <-
      c(beta = input$R0/(input$infdur1/length.step)/input$contactrate,
        b1=input$amp,
        phi=input$phase,
        gamma1 = 1 / (input$infdur1/length.step),
        gamma2 = 1 / (input$infdur2/length.step),
        gamma3 = 1 / (input$infdur3/length.step),
        gamma4 = 1 / (input$infdur3/length.step),
        rho1=input$rho1,
        rho2=input$rho2,
        sigma1=input$rr1,
        sigma2=input$rr2,
        sigma3=input$rr3,
        period=53,
        omega=1/(input$DurationMatImmunityDays/length.step),
        B_m=input$B_m,
        death.rate=0.00018,
        lockdown=F,
        virus_import=0,
        I_ex=0)
    ## Time frame
    times      <- seq(1, input$timeframe, by = 1)
    
    ## Solve using ode (General Solver for Ordinary Differential Equations)
    out_Normal <- ode(
      y = init,
      times = times,
      func = msis,
      parms = parameters_Normal
    )   
    
    #    out
    as.data.frame(out_Normal)
  })
  
  
  # Create reactive input
  simMSIS_Intervention <- reactive({
    init       <-
      c(M=0,
        S0 =  input$popsize*1000000-input$pinf,
        I1 = input$pinf, 
        S1 = 0,
        I2=0,
        S2= 0,
        I3=0,
        S3=0,
        I4=0
      )
    ## beta: infection parameter; gamma: duration of infectiousness
    length.step=7 # weekly
    
    parameters_intervention <-
      c(beta = input$R0/(input$infdur1/length.step)/input$contactrate,
        b1=input$amp,
        phi=input$phase,
        gamma1 = 1 / (input$infdur1/length.step),
        gamma2 = 1 / (input$infdur2/length.step),
        gamma3 = 1 / (input$infdur3/length.step),
        gamma4 = 1 / (input$infdur3/length.step),
        rho1=input$rho1,
        rho2=input$rho2,
        sigma1=input$rr1,
        sigma2=input$rr2,
        sigma3=input$rr3,
        period=53,
        omega=1/(input$DurationMatImmunityDays/length.step),
        B_m=input$B_m,
        death.rate=0.00018,
        lockdown=input$lockdown,
        virus_import=input$virus_import,
        I_ex=input$I_ex)
    ## Time frame
    times      <- seq(1, input$timeframe, by = 1)
    
    ## Solve using ode (General Solver for Ordinary Differential Equations)
    out_intervention <- ode(
      y = init,
      times = times,
      func = msis,
      parms = parameters_intervention
    )   
    
    #    out
    as.data.frame(out_intervention)
  })
  
  output$Plot_Normal <- renderPlotly({
    out_Normal <-simMSIS_Normal() %>%
      select(I1,I2,I3,I4) %>% 
      rowSums()
    InfectionPlot_Normal <- data.frame("Total_I"=round(out_Normal),
                                date=dates)
    
    plot_ly(InfectionPlot_Normal, type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~date, y = ~Total_I, name = 'Total infected individuals')%>%
      layout(showlegend = F)%>%
      layout(legend=list(title=list(text='variable')),
             yaxis = list(title = 'Total Infected Inviduals',showgrid = FALSE,showline= T),
             xaxis = list(dtick = "M1",
                          ticklabelmode="period",showgrid = FALSE,showline= T))
  })
  
  output$Plot_Intervention <- renderPlotly({
    out_Intervention <-simMSIS_Intervention() %>%
      select(I1,I2,I3,I4) %>% 
      rowSums()
    InfectionPlot_Intervention <- data.frame("Total_I"=round(out_Intervention),
                                       date=dates)
    
    plot_ly(InfectionPlot_Intervention, type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~date, y = ~Total_I, name = 'Total infected individuals')%>%
      layout(showlegend = F)%>%
      layout(legend=list(title=list(text='variable')),
             yaxis = list(title = 'Total Infected Inviduals',showgrid = FALSE,showline= T),
             xaxis = list(dtick = "M1",
                          ticklabelmode="period",showgrid = FALSE,showline= T))
  })
  
  # output$progressBox <- renderValueBox({
  #   valueBox(
  #     dataInput() %>% filter(time == max(time)) %>% select(S3) %>% mutate(S3 = round(100 * S3, 2)) %>% paste0("%"),
  #     "Proportion of full population that got the disease by end of time frame",
  #     icon = icon("thumbs-up", lib = "glyphicon"),
  #     color = "black"
  #   )
  # })
  
  # output$approvalBox <- renderValueBox({
  #   valueBox(
  #     paste0(round(
  #       100 * (dataInput() %>% filter(row_number() == n()) %>% mutate(res = (M+S1 + I1) / (M+S0 + I1 + S1)) %>% pull("res")), 2), "%"),
  #     "Proportion of susceptibles that will get the disease by end of time frame",
  #     icon = icon("thermometer-full"),
  #     color = "black"
  #   )
  # })
  
  # output$BRRBox <- renderValueBox({
  #   valueBox(
  #     paste0(round(input$connum* dataInput$S0/(dataInput$M+dataInput$S0 + dataInput$I1 + dataInput$S1), 2), ""),
  #     "Effective R0 (for populationen at outbreak, when immunity is taken into account)",
  #     icon = icon("arrows-alt"),
  #     color = "red"
  #   )
  # })
  
}

# Run the application
shinyApp(ui = ui, server = server)