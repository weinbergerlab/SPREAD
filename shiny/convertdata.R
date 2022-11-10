library(readr)
Birth_Month_CBS <- read_csv("./data_and_parms/Birth_Month_CBS.csv")
Birth_Month_CBS <- Birth_Month_CBS[complete.cases(Birth_Month_CBS$birthrate_year),]
Birth_Wholepop <- matrix(c(Birth_Month_CBS$birthrate_year,rep(0,6240)),nrow=312,ncol=21)
saveRDS(Birth_Wholepop,'./data_and_parms/PerCapitaBirthsCBS.rds')

Flight_Month_CBS <- read_csv("./data_and_parms/Flights_Month_CBS.csv")
Flight_Month_CBS <- Flight_Month_CBS[complete.cases(Flight_Month_CBS$`Commercial air traffic/Passengers/Passenger arrivals/Total arrivals passengers (number)`),]
saveRDS(Flight_Month_CBS$`Commercial air traffic/Passengers/Passenger arrivals/Total arrivals passengers (number)`,'./data_and_parms/travel_volumn_NL_CBS.rds')