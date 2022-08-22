spread_data <- read.csv("~/Library/CloudStorage/OneDrive-YaleUniversity/SPREAD study/prospective_SPREAD.csv")
colnames(spread_data) <- c(names(spread_data)[1:3],
                           paste0("week",rep(1:48,each=4),rep(c("Bronchiolitis","Test","RSV","comment"),48)))
spread_data <- spread_data[-c(1:2),]
spread_test <- spread_data[48,]
spread_RSVposrate <- spread_data[49,]
spread_data <- spread_data[-c(48:49),]

Bronchiolitis <- as.numeric(spread_test[,grep("Bronchiolitis",colnames(spread_test))])
RSV <- as.numeric(spread_test[,grep("RSV",colnames(spread_test))])
Test <- as.numeric(spread_test[,grep("Test",colnames(spread_test))])
Posrate <- as.numeric(spread_RSVposrate[,grep("RSV",colnames(spread_RSVposrate))])

cor.test(Bronchiolitis,RSV) # 0.9729342 P<0.001

plot(Bronchiolitis,type="l",xlab="week",bty="l",xaxt="n")
lines(RSV,col="red")
lines(Posrate*Bronchiolitis/100,col="blue",lty=2)
legend(30,300, bg="transparent",legend=c("Bronchiolitis","RSV","Pos*Bro"),
       col=c("black", "red","blue"),lty=c(1,1,2),cex=0.8,box.lty=0)
legend(15,300, bg="transparent",legend=c("correlation=0.97", "P<0.0001"),
       col=c("black"),cex=0.8,box.lty=0)
axis(1, at=seq(3,48,4.3), labels=c("May","Jun","Jul","Aug",
                                 "Sep","Oct","Nov","Dec",
                                 "Jan","Feb","Mar"))

plot(Test/Bronchiolitis,type="l",col="blue",lty=2,xlab="week",ylim=c(0,1))
lines(Posrate/100,col="purple",lty=2)
legend(20,0.36, legend=c("Test proportion", "Positive rate"),
       col=c("blue", "purple"),lty=2,cex=0.5,box.lty=0)

library(dplyr)
library(tidyr)
province <- spread_data %>%
          select(Province,ends_with("Bronchiolitis")) %>%
           mutate_at(vars(matches("Bronchiolitis")),as.numeric) %>% 
            group_by(Province) %>%
            summarise(across(everything(),~sum(.,na.rm = T))) %>%
            pivot_longer(!Province, names_to = "week", values_to = "count")


province <- province[province$Province!="",]

province <- province %>%
  group_by(Province) %>% 
  mutate(standardize_count = as.numeric(scale(count)))

library(readr)
province$week_num <- parse_number(province$week)
  
library(ggplot2)

province %>%
  filter(Province %in% c("Zuid-Holland","Noord-Holland","Noord-Brabant")) %>%
ggplot(aes(x=week_num, y=standardize_count, group=Province, color=Province)) +
  geom_line(linetype = "dashed")+
  theme_classic()

library(geofacet)
province %>%
  ggplot(aes(x=week_num, y=standardize_count, group=Province, color=Province)) +
  geom_line()+
  theme_classic()+
  facet_geo(~Province,grid="nl_prov_grid1")

province %>%
  filter(Province %in% c("Gelderland","Utrecht","Flevoland")) %>%
  ggplot(aes(x=week_num, y=count, group=Province, color=Province)) +
  geom_line(linetype = "dashed")+
  theme_classic()


Gelderland <- spread_data[spread_data$Province=="Gelderland",]

city_based <- Gelderland %>%
  select(City,ends_with("Bronchiolitis")) %>%
  mutate_at(vars(matches("Bronchiolitis")),as.numeric)%>%
  pivot_longer(!City, names_to = "week", values_to = "count")
city_based$week_num <- parse_number(city_based$week)

city_based %>%
  filter(City %in% c("Arnhem","Tiel","Doetinchem")) %>%
ggplot(aes(x=week_num, y=count, group=City, color=City)) +
  geom_line(linetype = "solid",size=0.5)+
  theme_classic()

peaktimedifference <- province %>%
  filter(Province %in% c("Zuid-Holland","Noord-Holland","Noord-Brabant"))%>%
  group_by(Province) %>% 
  summarise(timing=week_num[which.max(count)])

after <- province %>%
  group_by(Province) %>% 
  summarize(after20w=sum(count[week_num>20]))

before <- province %>%
    group_by(Province) %>% 
    summarize(before20w=sum(count[week_num<=20]))

ratio <- merge(before,after)
ratio$ratio <- ratio$before20w/ratio$after20w
ratio$ratio_cat <- cut(ratio$ratio,
                       breaks=c(0.5,0.7,0.9,1.11,1.4,1.8),
                       labels=c('<0.7', '0.7-0.9', '0.91-1.11', '1.2-1.4',">1.4"))

library(sf)
# Retrieve data with municipal boundaries from PDOK
provinceBoundaries <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_provincie_2017_gegeneraliseerd&outputFormat=json")

ratio <- merge(provinceBoundaries,ratio,by.x="statnaam",by.y="Province")

ratio%>%
  ggplot() +
  geom_sf(aes(fill = ratio)) +
  scale_fill_distiller(palette = "Spectral") +
  labs(title = "ratio before 20w and after 20w", fill = "") +
  theme_void() # popden/commute


ratio%>%
  ggplot() +
  geom_sf(aes(fill = ratio_cat)) +
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "ratio before 20w and after 20w", fill = "") +
  theme_void() # popden/commute

hist(ratio$ratio)

spread_pre <- read.csv("~/Library/CloudStorage/OneDrive-YaleUniversity/SPREAD study/SPREAD/SPREAD_study_excel_export.csv")
spread_pre$date <- as.Date(spread_pre$klin_opname_datum,"%d-%m-%Y")
RSV_pre <- spread_pre %>%
  group_by(group = ceiling_date(date,"1 week")) %>%
  summarise(RSV=sum(Test.for.RSV,na.rm=T))

