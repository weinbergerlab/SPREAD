library(haven)
SPREAD_age <- read_sav("data/SPREAD_age.sav")

# To select RSV+ cases you will need to select “klin_RSV_getest=1”
RSV_positive <- SPREAD_age[SPREAD_age$klin_RSV_getest==1,]

# To distinguish the subgroups, you need “groups_opname_datum” (0 = pre-covid; 1 = summer outbreak; 2 = endemic period)
Pre_covid_RSV <- RSV_positive[RSV_positive$groups_opname_datum==0,]

# To select date of admission, you will need to select “klin_opname_datum”
Pre_covid_RSV <- Pre_covid_RSV[complete.cases(Pre_covid_RSV$klin_opname_datum),]

library(lubridate)
Pre_covid_RSV$year_month <- floor_date(Pre_covid_RSV$klin_opname_datum, "month")

# Variable “klin_leeftijd_dagen” shows age at admission in days
Pre_covid_RSV$age_by_month <- floor(Pre_covid_RSV$klin_leeftijd_dagen/30.44)

table(Pre_covid_RSV$age_by_month)
library(dplyr)

# since all hospitalizations are for children under 2.
# we aggregate children under 1 to monthly age group and leave 1 year old 
# as a single age group
Change_age <-Pre_covid_RSV %>% 
  group_by(year_month) %>% 
  mutate(age_agg=ifelse(age_by_month<=11,paste0(age_by_month," m"),"1 y"))
  
total_case <- Change_age %>% 
  group_by(year_month,age_agg) %>% 
  summarise(mn_case = sum(klin_RSV_getest))

Median_age_RSV <- Pre_covid_RSV %>%
group_by(year_month) %>%
summarise(mn_median_age = median(klin_leeftijd_dagen))

complete_age_series <- data.frame(year_month=rep(unique(total_case$year_month),each=13),age_agg=rep(c(paste0(0:11," m"),"1 y"),12),
                                  pop=rep(c(rep(1980,12),23755),12))

complete_age_series <- merge(complete_age_series,total_case,all.x = T)
complete_age_series$mn_case <- ifelse(is.na(complete_age_series$mn_case),0,complete_age_series$mn_case)
complete_age_series$incidence <- complete_age_series$mn_case/complete_age_series$pop*100000
complete_age_series_2019 <- complete_age_series[complete_age_series$year_month<="2019-05-01",]

complete_age_series_2019$age_agg <- factor(complete_age_series_2019$age_agg,levels=c(paste0(0:11," m"),"1 y"))

Median_age_RSV_2019 <- Median_age_RSV[Median_age_RSV$year_month<="2019-05-01",] 
Median_age_RSV_2019$mn_median_age_month <- Median_age_RSV_2019$mn_median_age/30.44

epi2018_2019 <- ggplot() + 
  geom_tile(data=complete_age_series_2019, aes(year_month, age_agg, fill= incidence))+
  scale_fill_gradient(low = "yellow", high = "red",limits=c(0,1500))+
  theme_bw()+
  labs(fill="Monthly\nincidence",
       x = "Date",
       y = "Age group")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b")+
  geom_line(data=Median_age_RSV_2019,aes(x=year_month,y=mn_median_age_month))+
  geom_text(data = Median_age_RSV_2019, aes(x = year_month, y = mn_median_age_month, label = round(mn_median_age)), 
            size = 3, vjust = -2.5, hjust = -1.5)+
  annotate("text", x = as.Date("2019-01-01"), y = 7, label = "median age by days",color="black")



complete_age_series_2020 <- complete_age_series[complete_age_series$year_month>="2019-05-01",]

complete_age_series_2020$age_agg <- factor(complete_age_series_2020$age_agg,levels=c(paste0(0:11," m"),"1 y"))

Median_age_RSV_2020 <- Median_age_RSV[Median_age_RSV$year_month>="2019-05-01",] 
Median_age_RSV_2020$mn_median_age_month <- Median_age_RSV_2020$mn_median_age/30.44



epi2019_2020 <- ggplot() + 
  geom_tile(data=complete_age_series_2020, aes(year_month, age_agg, fill= incidence))+
  scale_fill_gradient(low = "yellow", high = "red",limits=c(0,1500))+
  theme_bw()+
  labs(fill="Monthly\nincidence",
       x = "Date",
       y = "Age group")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b")+
  geom_line(data=Median_age_RSV_2020,aes(x=year_month,y=mn_median_age_month))+
  geom_text(data = Median_age_RSV_2020, aes(x = year_month, y = mn_median_age_month, label = round(mn_median_age)), 
            size = 3, vjust = -2.5, hjust = -0.5)+
  annotate("text", x = as.Date("2020-01-01"), y = 7, label = "median age by days",color="black")


# To distinguish the subgroups, you need “groups_opname_datum” (0 = pre-covid; 1 = summer outbreak; 2 = endemic period)
Summer_covid_RSV <- RSV_positive[RSV_positive$groups_opname_datum!=0,]

# To select date of admission, you will need to select “klin_opname_datum”
Summer_covid_RSV <- Summer_covid_RSV[complete.cases(Summer_covid_RSV$klin_opname_datum),]

Summer_covid_RSV$year_month <- floor_date(Summer_covid_RSV$klin_opname_datum, "month")

# Variable “klin_leeftijd_dagen” shows age at admission in days
Summer_covid_RSV$age_by_month <- floor(Summer_covid_RSV$klin_leeftijd_dagen/30.44)

# since all hospitalizations are for children under 2.
# we aggregate children under 1 to monthly age group and leave 1 year old 
# as a single age group
Change_age_summer <-Summer_covid_RSV %>% 
  group_by(year_month) %>% 
  mutate(age_agg=ifelse(age_by_month<=11,paste0(age_by_month," m"),"1 y"))

total_case_summer <- Change_age_summer %>% 
  group_by(year_month,age_agg) %>% 
  summarise(mn_case = sum(klin_RSV_getest))

complete_age_series_summer <- data.frame(year_month=rep(unique(total_case_summer$year_month),each=13),age_agg=rep(c(paste0(0:11," m"),"1 y"),length(unique(total_case_summer$year_month))),
                                  pop=rep(c(rep(1968,12),23615),length(unique(total_case_summer$year_month))))

complete_age_series_summer <- merge(complete_age_series_summer,total_case_summer,all.x = T)
complete_age_series_summer$mn_case <- ifelse(is.na(complete_age_series_summer$mn_case),0,complete_age_series_summer$mn_case)
complete_age_series_summer$incidence <- complete_age_series_summer$mn_case/complete_age_series_summer$pop*100000
complete_age_series_summer$age_agg <- factor(complete_age_series_summer$age_agg,levels=c(paste0(0:11," m"),"1 y"))

Median_age_RSV_summer <- Summer_covid_RSV %>%
  group_by(year_month) %>%
  summarise(mn_median_age_month = median(klin_leeftijd_dagen,na.rm=T)/30.44,mn_median_age= median(klin_leeftijd_dagen,na.rm=T))

epi_summer <- ggplot() + 
  geom_tile(data=complete_age_series_summer, aes(year_month, age_agg, fill= incidence))+
  scale_fill_gradient(low = "yellow", high = "red",limits=c(0,1500))+
  theme_bw()+
  labs(fill="Monthly\nincidence",
       x = "Date",
       y = "Age group")+
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b")+
  geom_line(data=Median_age_RSV_summer,aes(x=year_month,y=mn_median_age_month))+
  geom_text(data = Median_age_RSV_summer, aes(x = year_month, y = mn_median_age_month, label = round(mn_median_age)), 
            size = 3, vjust = -2.5, hjust = 0)+
  annotate("text", x = as.Date("2021-11-30"), y = 7, label = "median age by days",color="black")

Median_age_RSV_2019$year <- "2018-2019"
Median_age_RSV_2020$year <- "2019-2020"
Median_age_RSV_summer$year <- "2021-2022"
total_age <- rbind(Median_age_RSV_2019,Median_age_RSV_2020)
total_age <- rbind(total_age,Median_age_RSV_summer)


epi_line <- ggplot(data=total_age,aes(x=year_month,y=mn_median_age)) + 
  labs(fill="Monthly\nincidence",
       x = "Date",
       y = "Median age by days")+
  ylim(0,300)+
  scale_x_date(date_breaks = "1 month",date_minor_breaks = "1 week",
               date_labels = "%b")+
   geom_line()+
  geom_text(aes(x = year_month, y = mn_median_age, label = round(mn_median_age)), 
            size = 3, vjust = -1.5, hjust = 0)+
    theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  facet_grid(~year, scales = "free_x", space = "free_x")



library(cowplot)

top_row <- plot_grid(epi2018_2019, epi2019_2020, labels = c('(a) 2018-2019 RSV hospitalizations', '(b) 2019-2020 RSV hospitalizations'), label_size = 10, scale = 0.95)
Figure_heatmap <- plot_grid(top_row, epi_summer, labels = c('','(c) Post-COVID-19 RSV hospitalizations'), label_size = 10, ncol = 1, scale = 0.95)

png("./age_hosp.png",res=300,width = 3000, height = 2200)
Figure_heatmap
dev.off()

png("./median_age_hosp.png",res=300,width = 3000, height = 1200)
epi_line
dev.off()

