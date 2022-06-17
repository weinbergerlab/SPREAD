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
  dataInput <- reactive({
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
    parameters <-
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
    times      <- seq(0, input$timeframe, by = 1)
    
    ## Solve using ode (General Solver for Ordinary Differential Equations)
    out <- ode(
      y = init,
      times = times,
      func = msis,
      parms = parameters
    )   
    #    out
    as.data.frame(out)
  })
  
  output$distPlot <- renderPlot({
    out <-
      dataInput() %>%
      select(time,I1,I2,I3,I4) %>% 
      mutate(Total_I = rowSums(select(.,-time)))%>%
      select(time,Total_I) %>%
      gather(key, value, -time) %>%
      mutate(
        id = row_number(),
        key2 = recode(
          key,
          Total_I = "Total infections"
        )
      )
    
    ggplot(data = out, aes(
      x = time,
      y = value,
      color=key2,
      label = key2,
      data_id = id)) + 
      ylab("Number of Infectious Individuals") + xlab("Time (Weeks)") +
      geom_line(size = 1) +
      geom_text_repel(
        data = subset(out, time == max(time)),
        aes(label = key2),
        size = 6,
        segment.size  = 0.2,
        segment.color = "grey50",
        nudge_x = 0,
        hjust = -1,
        vjust= -1.5,
        direction = "y"
      )  +
      theme(legend.position = "none") +
      scale_colour_brewer(palette = "Set1") +
      theme(
        rect=element_rect(size=0),
        legend.position="none",
        panel.background=element_rect(fill="transparent", colour=NA),
        plot.background=element_rect(fill="transparent", colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
      )
    
    p2 <- ggplot(data = out[out$key=="Total_I"&out$time>159&out$time<159+18,], aes(
      x = time,
      y = value,
      label = key2,
      data_id = id)) + 
      ylab("Number of Infectious Individuals") + xlab("Time (Weeks)") +
      geom_line(size = 1)+theme_classic() 

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