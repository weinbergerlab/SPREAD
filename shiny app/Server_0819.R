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

# can change to location specific age distributions and annual per capita birth rate
agedist <- readRDS('./data_and_parms/agedist.rds')
# currently, we have 21 age groups
#The <12 month olds, were divided into monthly age classes.
#The 1-4 year olds, were divided into yearly age classes.
#The remaining population was divided into 5 classes: 5–9 years, 10–19 years, 20–39 years, 40–59-years, and 60+ years old. 
N_ages <- 21
PerCapitaBirthsYear2 <- readRDS('./data_and_parms/PerCapitaBirthsYear.rds')
c2 <- readRDS( './data_and_parms/c2.rds') # contact matrix
travel_volumn_NL <- readRDS( './data_and_parms/travel_volumn_NL.rds')

# adjust WidthAgeClassMonth based on the age distribution
WidthAgeClassMonth <- c(rep(1,times=12), rep(12,times=4),  60, 120, 240, 240, 240 )  
#Aging rate=1/width age class (months) Vector of long N_age
hosp1=c(.18*rep(.40,3),0.08*rep(.39,3),0.07*rep(.21,3),0.06*rep(.20,3),0.06*0.16,0.05*rep(.14,3),0.02*rep(0.05,N_ages-16))#proportion of first infections that are hospitalized
hosp2=.4*hosp1#proportion of second infections that are hospitalized
hosp3=c(rep(0,N_ages-2),0.00001,0.00004)#proportion of third infections that are hospitalized
agenames <- paste0('Agegrp', 1:N_ages) #Could replace this with vector of actual 
source("./shiny app/waning_immunity.R")  
# Define server 
#
server <- function(input, output) {
 
  # Create reactive input
  simMSIS_flex <- reactive({ 
  # Create reactive input
  Pop1 <- input$popsize*1000000*agedist
  StateNames <- c('M','S0','I1','S1','I2','S2','I3','S3','I4','W1','W2','W3')
  States <- array(NA, dim=c(N_ages, length(StateNames) )) #  N age groups xN parameters
  dimnames(States)[[1]] <- agenames
  dimnames(States)[[2]] <- StateNames
  
  yinit.matrix <- array(NA, dim=c(N_ages, length(StateNames) ))
  
  dimnames(yinit.matrix)[[1]] <- agenames
  dimnames(yinit.matrix)[[2]] <- StateNames
  
  yinit.matrix[,c('S1','I2','S2','I3','S3','I4','W1','W2','W3')]  = 0
  yinit.matrix[,'M'] = c(Pop1[1:6], rep(0,N_ages-6))
  yinit.matrix[,'S0'] = c(rep(0,6),Pop1[7:N_ages]-rep(N_ages-6)) 
  yinit.matrix[,'I1'] = c(rep(0,6), rep(1,N_ages-6))  #initializes with 1 infected person per age group 
  
  yinit.vector <- as.vector(yinit.matrix) #Vectorize the ynit matrix
  
  # Create array that has the labels by age, State and use this to name the yinit.vector
  name.array <- array(NA, dim=dim(yinit.matrix))
  for(i in 1:dim(name.array)[1]){
    for(j in 1:dim(name.array)[2]){
      name.array[i,j] <- paste(dimnames(yinit.matrix)[[1]][i],dimnames(yinit.matrix)[[2]][j]  )
    }
  }
  
  name.vector <- as.vector(name.array)
  names(yinit.vector) <- name.vector
  
  parameters_flex <-
    list(PerCapitaBirthsYear=as.matrix(PerCapitaBirthsYear2),
         DurationMatImmunityDays=input$DurationMatImmunityDays,
         WidthAgeClassMonth=WidthAgeClassMonth,
         um=input$Mu_m, # adjust for population increase/decrease
         b1=input$amp,
         phi=input$phase,
         rho1=input$rho1,
         rho2=input$rho2,
         sigma1=input$rr1,
         sigma2=input$rr2,
         sigma3=input$rr3,
         AllowWaning=input$AllowWaning,
         dur.days1=input$infdur1,
         dur.days2=input$infdur2,
         dur.days3=input$infdur3,
         dur.immunity1=input$durimmunity1,
         dur.immunity2=input$durimmunity2,
         yinit.matrix=yinit.matrix,
         R0 = input$baseline.txn.rate,
         # input$constant_strict/100=0-0.3 corrsponds to 70%-100% of pre-pandemic level transmission
         # ref: Baker RE, PNAS, 2020
         NPIs=c(rep(1,312+9),rep(1-input$constant_strict/100,input$NPI_durUnittime_1),
                seq((1-input$constant_strict/100),1,input$constant_strict/100/input$NPI_durUnittime_2),rep(1,70)),
         q=1,
         c=c2,#contact matrix, can change this to country/location specific
         time.step='month',
         I_ex=input$I_ex/1000000,
         # travel volumn can change to ountry/location specific
         airtravel=travel_volumn_NL)
  
  ## Time frame
  times      <- seq(1, nrow(PerCapitaBirthsYear2), by = 1)
  
  ## Solve using ode (General Solver for Ordinary Differential Equations)
  out_flex <- ode(
    y = yinit.vector,
    times = times,
    func = waning_immunity_model,
    parms = parameters_flex
  )   
  St <- out_flex [-c(1:300),-1]
  
  I1 <- St[,grep('I1', colnames(St))]
  I2 <- St[,grep('I2', colnames(St))]
  I3 <- St[,grep('I3', colnames(St))]
  I4 <- St[,grep('I4', colnames(St))]
  S1 <- St[,grep('S1', colnames(St))]
  S2 <- St[,grep('S2', colnames(St))]
  S3 <- St[,grep('S3', colnames(St))]
  W1 <- St[,grep('W1', colnames(St))]
  W2 <- St[,grep('W2', colnames(St))]
  W3 <- St[,grep('W3', colnames(St))]
  S0 <- St[,grep('S0', colnames(St))]
  
  I_S_W <- data.frame("I"=rowSums(I1+I2+I3+I4),"S"=rowSums(S1+S2+S3+S0),"W"=rowSums(W1+W2+W3))
  
  timemax=106
  lambda1=matrix(0,nrow=timemax,ncol=N_ages)#Force of infection
  total_Pop <- rowSums(St)
  I_ex_t <- c()
  baseline.txn.rate <- c()
  b <- c()
  beta=array(0,dim=c(timemax,N_ages,N_ages))
  for (t in 1:timemax)
  {
    baseline.txn.rate[t]=parameters_flex$R0*parameters_flex$NPIs[t+300] # change based on NPIs
    I_ex_t[t]=parameters_flex$I_ex*parameters_flex$airtravel[t+300] # virus importation
    b[t]= baseline.txn.rate[t]/(parameters_flex$dur.days1/30.44) # transmission per contact per unit time
    beta[t,,]=(b[t]/100)/(sum(parameters_flex$yinit.matrix)^(1-parameters_flex$q))*parameters_flex$c # transmission * contact pattern
    lambda1[t,] <-  as.vector((1+parameters_flex$b1*cos(2*pi*(t-parameters_flex$phi*12)/12))*((I1[t,]+
parameters_flex$rho1*I2[t,]+parameters_flex$rho2*I3[t,]+parameters_flex$rho2*I4[t,]+
  parameters_flex$rho2*I4[t,]+parameters_flex$rho2*I_ex_t[t])%*%beta[t,,])/total_Pop[t])
}
    Htotal=matrix(0,nrow=timemax,ncol=N_ages)#Number of hospitalizations by age
    for (i in 1:N_ages){
      Htotal[,i]=input$Catchmentarea*input$report_fraction*
        (hosp1[i]*S0[,i]*lambda1[,i]+
         hosp2[i]*input$rr1*S1[,i]*lambda1[,i]+
         hosp3[i]*input$rr2*S2[,i]*lambda1[,i]+
         hosp3[i]*input$rr3*S3[,i]*lambda1[,i])+
         hosp2[i]*input$rr1*W1[,i]*lambda1[,i]+
         hosp3[i]*input$rr2*W2[,i]*lambda1[,i]+
         hosp3[i]*input$rr3*W3[,i]*lambda1[,i]
    }
  
  return(list("Htotal"=as.data.frame(Htotal),"Totalpop"=total_Pop,"I_S_W"=I_S_W))
})
  

output$Plot_flex <- renderPlotly({

  library(xts)
  timemax=106
  dates <- seq(as.Date("2018-07-07"), length=timemax, by="months")
  
  Htotal <- simMSIS_flex()$Htotal %>%
    select(1:13) %>% # RSV hospitalizations in children under 2 years of age
    rowSums()
  Hyoung <- simMSIS_flex()$Htotal %>%
    select(1:6) %>% # RSV hospitalizations in infants under 6 months of age
    rowSums()
  H1y <- simMSIS_flex()$Htotal %>%
    select(13) %>% # RSV hospitalizations in children 1 year of age
    rowSums()
  
  # total hospitalizations in children under 2 years of age
  HospPlot_flex <- data.frame("Total_H"=round(Htotal),"Young"=round(Hyoung),
                                   "H1y"=round(H1y),
                                   date=dates)

  #adjust the margin
  margin <- list(autoexpand = T,
                 l = 100,
                 r = 105,
                 t = 105)
  
  
  plot_ly(HospPlot_flex, type = 'scatter', mode = 'lines')%>%
    add_trace(x = ~date, y = ~Total_H, name = 'Total Hospitalizations', line = list(color = '#66c2a5'))%>%
     add_trace(x = ~date, y = ~Young, name = 'Hospitalizations in infants < 6 mo', line = list(color = '#fc8d62'))%>%
     add_trace(x = ~date, y = ~H1y, name = 'Hospitalizations in children 1 year old', line = list(color = '#8da0cb'))%>%
    
      layout(margin = margin,
        legend=list(orientation = "h",   # show entries horizontally
                    xanchor = "center",  # use center of legend as anchor
                    x = 0.5,y=1.2),
           yaxis = list(title = 'Total hospitalizations in children under 2 years of age',showgrid = FALSE,showline= T),
           xaxis = list(dtick = "M3",
                        ticklabelmode="period",showgrid = FALSE,showline= T))
  
})

output$Plot_transmission <- renderPlotly({
  
  library(xts)
  timemax=106
  dates <- seq(as.Date("2018-07-07"), length=timemax, by="months")
  
  Total_I <- simMSIS_flex()$I_S_W$I
  
  Total_S <- simMSIS_flex()$I_S_W$S
  
  Total_W <- simMSIS_flex()$I_S_W$W
  
  InfectionPlot_flex <- data.frame("Total_I"=round(Total_I),"Total_S"=round(Total_S)+round(Total_W),
                                   "Total_W"=round(Total_W),
                                   date=dates)
  
  #adjust the margin
  # margin <- list(autoexpand = T,
  #                l = 100,
  #                r = 105,
  #                t = 105)
  
  
  plot_ly(InfectionPlot_flex, type = 'scatter', mode = 'lines')%>%
    add_trace(x = ~date, y = ~Total_I, name = 'Infected (I)', line = list(color = '#66c2a5'))%>%
    add_trace(x = ~date, y = ~Total_S, name = 'Suscepitible (S)', line = list(color = '#fc8d62'))%>%
    add_trace(x = ~date, y = ~Total_W, name = 'Waning Immunity (W)', line = list(color = '#8da0cb'))%>%
    layout(#margin = margin,
           legend=list(orientation = "h",   # show entries horizontally
                       xanchor = "center",  # use center of legend as anchor
                       x = 0.5,y=1.2),
           yaxis = list(title = 'Total number of people',showgrid = FALSE,showline= T),
           xaxis = list(dtick = "M3",
                        ticklabelmode="period",showgrid = FALSE,showline= T))
  
})

output$Plot_population <- renderPlotly({
  
  library(xts)
  timemax=106
  dates <- seq(as.Date("2018-07-07"), length=timemax, by="months")
  
  totalpopulation<- data.frame("Total_pop"=round(simMSIS_flex()$Totalpop),
                                   date=dates)
  
  plot_ly(totalpopulation, type = 'scatter', mode = 'lines')%>%
    add_trace(x = ~date, y = ~Total_pop, name = 'Population size', line = list(color = 'black'))%>%
    layout(margin = margin,
           legend=list(orientation = "h",   # show entries horizontally
                       xanchor = "center",  # use center of legend as anchor
                       x = 0.5,y=1.2),
           yaxis = list(title = 'Total number of people',showgrid = FALSE,showline= T),
           xaxis = list(dtick = "M3",
                        ticklabelmode="period",showgrid = FALSE,showline= T))
  
})

# Display the model diagram
output$plot4 <- renderImage({
  filename <- normalizePath(file.path('./shiny app',"model_diagram.png"))
  
  list(src = filename, height=500, width=900)
  
}, deleteFile = FALSE)

}
# Run the application
shinyApp(ui = ui, server = server)  