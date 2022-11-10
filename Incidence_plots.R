spread_data <- read.csv("data/prospective_data_SPREAD.csv")
colnames(spread_data) <- c(names(spread_data)[1:3],
                           paste0("Week",rep(c(paste0(18:52,"year2021"),paste0(1:35,"year2022")),each=4),rep(c("Bronchiolitis","Test","RSV","comment"),70)))
spread_data <- spread_data[-c(1:2),]
spread_data <- spread_data[-c(48:49),]
RSV <- spread_data[,c(3,grep("RSV",colnames(spread_data)))]

library(dplyr)
library(tidyr)
RSV <- RSV %>%
  mutate_at(vars(matches("RSV")),as.numeric) %>% 
  group_by(Province) %>%
  summarise(across(everything(),~sum(.,na.rm = T))) 
RSV <- RSV[-1,]

RSV_NL <- data.frame(RSV=colSums(RSV[,-1]),date=seq.Date(as.Date("2021-05-02"),as.Date("2022-09-02"),by="week"))

library(ggplot2)

plot_NL_line <- 
  ggplot(data=RSV_NL,aes(x=date, y=RSV)) +
  geom_line()+
  theme_classic()+
  labs(x="Date",y="Weekly number of RSV hospitalizations")+
  geom_vline(xintercept=as.Date("2021-06-05"),colour="red", linetype = "longdash")+
  annotate("text", x = as.Date("2021-06-05"), y = 240, label = "end of lock down",color="red")+
  annotate("rect", xmin = as.Date("2021-10-16"), xmax = as.Date("2021-10-31"), ymin = 0, ymax = 250,
           alpha = .2)+
  annotate("rect", xmin = as.Date("2021-12-19"), xmax = as.Date("2022-01-10"), ymin = 0, ymax = 250,
           alpha = .2,fill = "blue")+
  geom_vline(xintercept=as.Date("2022-01-10"),colour="red", linetype = "longdash")+
  annotate("text", x = as.Date("2022-02-20"), y = 240, label = "school reopen",color="red")+
  annotate("text", x = as.Date("2021-12-10"), y = 200, label = "lock down",color="blue")+
  annotate("rect", xmin = as.Date("2022-02-19"), xmax = as.Date("2022-03-06"), ymin = 0, ymax = 250,
           alpha = .2)+
  annotate("rect", xmin = as.Date("2022-04-30"), xmax = as.Date("2022-05-08"), ymin = 0, ymax = 250,
           alpha = .2)+
  annotate("rect", xmin = as.Date("2022-07-09"), xmax = as.Date("2022-09-04"), ymin = 0, ymax = 250,
           alpha = .2)+
  scale_x_date(date_breaks = "1 month",date_minor_breaks = "1 week",
               date_labels = "%b %Y")+
  annotate("text", x = as.Date("2022-04-30"), y = 200, label = "school holidays")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

library(readr)
population_region <- read_csv("data/population_region.csv", 
                              skip = 4)

names(population_region) <- c("Province","Periods","0y",'1y','total')

library(stringr)
population_region$Province <- str_remove_all(population_region$Province,"[ (PV)]")

RSV_incidence <- merge(RSV,population_region[,c(1,5)])  
RSV_incidence[,2:71] <- RSV_incidence[,2:71]/RSV_incidence[,72]*100000
RSV_incidence <-RSV_incidence[,1:71]

RSV_incidence_summer <-RSV_incidence[,1:19]
colnames(RSV_incidence_summer) <- c("Province",paste0("Week",18:35))

library(sf)
# Retrieve data with municipal boundaries from PDOK
provinceBoundaries <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_provincie_2017_gegeneraliseerd&outputFormat=json")

RSV_incidence_summer_long <- RSV_incidence_summer%>%
  pivot_longer(!Province, names_to = "Week", values_to = "Incidence")

RSV_incidence_summer_long$Week <- factor(RSV_incidence_summer_long$Week,
                                  levels=paste0("Week",18:35),
                                  ordered = T)

RSV_incidence_summer_long <- merge(provinceBoundaries,RSV_incidence_summer_long,by.x="statnaam",by.y="Province")

library(ggplot2)
plot_NL <- RSV_incidence_summer_long%>%
  ggplot() +
  geom_sf(aes(fill = Incidence)) +
  scale_fill_distiller(trans = "reverse") +
  labs(title = "Summer outbreak 2021", fill = "Incidence") +
  theme_void() +
  facet_wrap(~Week,nrow = 3)


png("Incidence_NL_map.png", width = 3200, height = 1600,res=300)
plot_NL
# Close device
dev.off()

colnames(RSV_incidence) <- c("Province",paste0("Week",18:52,"Year2021"),paste0("Week",1:35,"Year2022"))

RSV_incidence_long <- RSV_incidence%>%
  pivot_longer(!Province, names_to = "Week", values_to = "Incidence")

RSV_incidence_long$Week <- factor(RSV_incidence_long$Week,
                                         levels=c(paste0("Week",18:52,"Year2021"),paste0("Week",1:35,"Year2022")),
                                         ordered = T)

library(geofacet)
plot_NL_line <- RSV_incidence_long %>%
  ggplot(aes(x=Week, y=Incidence, group=Province, color=Province)) +
  geom_line()+
  theme_classic()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_geo(~Province,grid="nl_prov_grid1")

png("Incidence_NL_map_line.png", width = 2200, height = 1600,res=300)
plot_NL_line
# Close device
dev.off()

png("Incidence_NL_map_line.png", width = 2200, height = 1200,res=300)
plot_NL_line
# Close device
dev.off()
