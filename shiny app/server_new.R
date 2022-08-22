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
  simMSIS_flex <- reactive({
    
    init       <-
      c(M=0,
        S0 =  input$popsize*1000-input$pinf,
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
    
    parameters_flex <-
      list(tau = input$tau,
           contact=input$contactrate,
        b1=input$amp/100,
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
        period=52,
        omega=1/(input$DurationMatImmunityDays/length.step),
        B_m=birth_month,
        death.rate=input$Mu_m,
        airtravel=as.numeric(travel/4.33),
        I_ex=input$I_ex)
    ## Time frame
    times      <- seq(1, input$timeframe+52*20, by = 1)
    
    ## Solve using ode (General Solver for Ordinary Differential Equations)
    out_flex <- ode(
      y = init,
      times = times,
      func = msis,
      parms = parameters_flex
    )   
    
    #    out
    as.data.frame(tail(out_flex,input$timeframe))
  })
  
  
  # Create reactive input
  simMSIS_Intervention <- reactive({
    if(input$AllowReset=="No"){
    init       <-
      c(M=0,
        S0 =  input$popsize*1000-input$pinf,
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
    
    parameters_Intervention <-
      list(tau_t = c(rep(input$tau,52*21+16),rep(input$tau*(100-input$TD)/100,input$Tint),
                     rep(input$tau,input$timeframe-(52+16+3)-input$Tint)),
           contact=input$contactrate,
           b1=input$amp/100,
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
           period=52,
           omega=1/(input$DurationMatImmunityDays/length.step),
           B_m=birth_month,
           death.rate=input$Mu_m,
           airtravel=as.numeric(travel/4.33),
           I_ex=input$I_ex)
    ## Time frame
    times      <- seq(1, input$timeframe+52*20, by = 1)
    
    ## Solve using ode (General Solver for Ordinary Differential Equations)
    out_Intervention <- ode(
      y = init,
      times = times,
      func = msis_intervention,
      parms = parameters_Intervention
    )   } else if(input$AllowReset=="Yes"){
      init       <-
        c(M=0,
          S0 =  input$popsize*1000-input$pinf,
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
      
      parameters_first <-
        list(tau_t = c(rep(input$tau,52*21+16),rep(input$tau*(100-input$TD)/100,input$Tint)),
             contact=input$contactrate,
             b1=input$amp/100,
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
             period=52,
             omega=1/(input$DurationMatImmunityDays/length.step),
             B_m=birth_month,
             death.rate=input$Mu_m,
             airtravel=as.numeric(travel/4.33),
             I_ex=input$I_ex)
      ## Time frame
      init.times      <- seq(1, 52*21+16+input$Tint, by = 1)
      
      ## Solve using ode (General Solver for Ordinary Differential Equations)
      out_first <- ode(
        y = init,
        times = init.times,
        func = msis_intervention,
        parms = parameters_first)
      
      reset.times      <- seq(1, input$timeframe+52*20-(52*21+16+input$Tint), by = 1)
      
      reset.init <- c(M=0,
                      S0 = sum(tail(out_first[,"S0"],1),tail(out_first[,"M"],1)),
                      I1 = tail(out_first[,"I1"],1), 
                      S1 = sum(tail(out_first[,"S1"],1)),
                      I2= tail(out_first[,"I2"],1),
                      S2= sum(tail(out_first[,"S2"],1),0.8*tail(out_first[,"S3"],1),tail(out_first[,"I3"],1),tail(out_first[,"I4"],1)),
                      I3=0,
                      S3=0.2*tail(out_first[,"S3"],1),
                      I4=0)
      
      parameters_later <-
        list(tau_t = rep(input$tau,input$timeframe),
             contact=input$contactrate,
             b1=input$amp/100,
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
             period=52,
             omega=1/(input$DurationMatImmunityDays/length.step),
             B_m=tail(birth_month,input$timeframe+52*20-(52*21+16+input$Tint)),
             death.rate=input$Mu_m,
             airtravel=tail(travel,input$timeframe+52*20-(52*21+16+input$Tint)),
             I_ex=input$I_ex)
      
      out_later <- ode(
        y = reset.init,
        times = reset.times,
        func = msis_intervention,
        parms = parameters_later)
      
      out_Intervention <- rbind(out_first,out_later)
      
    }
    
    #    out
    as.data.frame(tail(out_Intervention,input$timeframe))
  })
  
<<<<<<< HEAD
  output$Plot_flex <- renderPlotly({
    library(xts)
    dates <- seq(as.Date("2018-12-07"), length=input$timeframe, by="weeks")
    
    I <- simMSIS_flex() %>%
      select(I1,I2,I3,I4) 
    S <- simMSIS_flex() %>%
      select(S0,S1,S2,S3)
    lambda1=rep(0,length=input$timeframe)#Force of infection
    I_incidence=rep(0,length=input$timeframe)#Incidence of infection
    total_Pop <- simMSIS_flex() %>%
      select(-time)%>%
=======
  output$Plot_Normal <- renderPlotly({
    dates <- seq(as.Date("2018-12-07"), length=input$timeframe, by="weeks")
    out_Normal <-simMSIS_Normal() %>%
      select(I1,I2,I3,I4) %>% 
>>>>>>> 51124db400ef274946e55d586390b7a1993cb19e
      rowSums()
    for (t in 1:input$timeframe)
    {

      lambda1[t] <- (1+input$amp/100*cos(2*pi*(t-input$phase*52)/52))*(I$I1[t]+input$rho1*I$I2[t]+input$rho2*I$I3[t]+input$rho2*I$I4[t]+input$rho2*input$I_ex*travel[t+52*20]/1000000)*input$tau*input$contactrate/total_Pop[t]
      I_incidence[t]=input$Report*0.03*(input$hosp1*S$S0[t]*lambda1[t]+input$hosp2*input$rr1*S$S1[t]*lambda1[t]+
                        input$hosp3*input$rr2*S$S2[t]*lambda1[t]+input$hosp3*input$rr3*S$S3[t]*lambda1[t])

    }

    InfectionPlot_flex <- data.frame("Total_I"=round(I_incidence),
                                date=dates,I)

    
      plot_ly(InfectionPlot_flex, type = 'scatter', mode = 'lines')%>%
        add_trace(x = ~date, y = ~Total_I, name = 'Total Incidence')%>%
      # add_trace(x = ~dates, y = ~I1, name = 'Total Prevalence of I1')%>%
      # add_trace(x = ~dates, y = ~I2, name = 'Total Prevalence of I2')%>%
      # add_trace(x = ~dates, y = ~I3, name = 'Total Prevalence of I3')%>%
      # add_trace(x = ~dates, y = ~I4, name = 'Total Prevalence of I4')%>%
      layout(legend=list(title=list(text='variable')),
             yaxis = list(title = 'Total hospitalizations in children under 2 years',showgrid = FALSE,showline= T),
             xaxis = list(dtick = "M1",
                          ticklabelmode="period",showgrid = FALSE,showline= T))
   
  })
  
  output$Plot_Intervention <- renderPlotly({
<<<<<<< HEAD
    library(xts)
    dates <- seq(as.Date("2018-12-07"), length=input$timeframe, by="weeks")
    
    I <- simMSIS_Intervention() %>%
      select(I1,I2,I3,I4) 
    S <- simMSIS_Intervention() %>%
      select(S0,S1,S2,S3)
    lambda1=rep(0,length=input$timeframe)#Force of infection
    H_incidence=rep(0,length=input$timeframe)#Incidence of infection
    H_incidence_1=rep(0,length=input$timeframe)#Incidence of infection
    H_incidence_2=rep(0,length=input$timeframe)#Incidence of infection
    H_incidence_3=rep(0,length=input$timeframe)#Incidence of infection
    H_incidence_4=rep(0,length=input$timeframe)#Incidence of infection
=======
    dates <- seq(as.Date("2018-12-07"), length=input$timeframe, by="weeks")
    out_Intervention <-simMSIS_Intervention() %>%
      select(I1,I2,I3,I4) %>% 
      rowSums()
    InfectionPlot_Intervention <- data.frame("Total_I"=round(out_Intervention),
                                       date=dates,Intervention="Lock Down")
>>>>>>> 51124db400ef274946e55d586390b7a1993cb19e
    
    total_Pop <- simMSIS_Intervention() %>%
      select(-time)%>%
      rowSums()
    tau_t = c(rep(input$tau,52+16),rep(input$tau*(100-input$TD)/100,input$Tint),
              rep(input$tau,input$timeframe-(52+16+3)-input$Tint))
    
    for (t in 1:input$timeframe)
    {
      lambda1[t] <- (1+input$amp/100*cos(2*pi*(t-input$phase*52)/52))*(I$I1[t]+input$rho1*I$I2[t]+input$rho2*I$I3[t]+input$rho2*I$I4[t]+input$rho2*input$I_ex*travel[t+52*20]/1000000)*tau_t[t]*input$contactrate/total_Pop[t]
      H_incidence[t]=input$Report*0.03*(input$hosp1*S$S0[t]*lambda1[t]+input$hosp2*input$rr1*S$S1[t]*lambda1[t]+
                                          input$hosp3*input$rr2*S$S2[t]*lambda1[t]+input$hosp3*input$rr3*S$S3[t]*lambda1[t])
      
          
    H_incidence_1[t]=input$Report*0.03*(input$hosp1*S$S0[t]*lambda1[t])
    H_incidence_2[t]=input$Report*0.03*(input$hosp2*input$rr1*S$S1[t]*lambda1[t])
    H_incidence_3[t]=input$Report*0.03*(input$hosp3*input$rr2*S$S2[t]*lambda1[t])
    H_incidence_4[t]=input$Report*0.03*(input$hosp3*input$rr3*S$S3[t]*lambda1[t])
    }
    
    InfectionPlot_Intervention <- data.frame("Total"=round(H_incidence),
                                     date=dates,H_incidence_1=round(H_incidence_1),
                                     H_incidence_2=round(H_incidence_2),
                                     H_incidence_3=round(H_incidence_3),
                                     H_incidence_4=round(H_incidence_4))
  
    plot_ly(InfectionPlot_Intervention, type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~date, y = ~Total, name = 'Total Hospitalizations')%>%
      add_trace(x = ~date, y = ~H_incidence_1,name="1st Hospitalizations")%>%
      add_trace(x = ~date, y = ~H_incidence_2,name="2st Hospitalizations")%>%
      add_trace(x = ~date, y = ~H_incidence_3,name="3st Hospitalizations")%>%
      add_trace(x = ~date, y = ~H_incidence_4,name="Subsequent Hospitalizations")%>%
      layout(showlegend = F)%>%
      layout(legend=list(title=list(text='variable')),
             yaxis = list(title = 'Number of RSV Hospitalizations',showgrid = FALSE,showline= T),
             xaxis = list(dtick = "M1",
                          ticklabelmode="period",showgrid = FALSE,showline= T))
  })
  }

# Run the application
shinyApp(ui = ui, server = server)
