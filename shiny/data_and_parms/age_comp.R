agecomp <- read.csv("./data_and_parms/Age_composition_Nederlands_2022.csv")
#The <12 month olds, were divided into monthly age classes.
#The 1-4 year olds, were divided into yearly age classes.
#The remaining population was divided into 5 classes: 5–9 years, 10–19 years, 20–39 years, 40–59-years, and 60+ years old. 

agedist <- c(rep(agecomp$Percentage[agecomp$Age=="0 yrs"]/12,12),agecomp$Percentage[agecomp$Age=="1 yrs"],
             agecomp$Percentage[agecomp$Age=="2 yrs"],agecomp$Percentage[agecomp$Age=="3 yrs"],
             agecomp$Percentage[agecomp$Age=="4 yrs"],
             sum(agecomp$Percentage[agecomp$Age=="5 yrs"|agecomp$Age=="6 yrs"|agecomp$Age=="7 yrs"|agecomp$Age=="8 yrs"|agecomp$Age=="9 yrs"]),
             sum(agecomp$Percentage[agecomp$Age=="10 yrs"|agecomp$Age=="11 yrs"|agecomp$Age=="12 yrs"|agecomp$Age=="13 yrs"|agecomp$Age=="14 yrs"
                                    |agecomp$Age=="15 yrs"|agecomp$Age=="16 yrs"|agecomp$Age=="17 yrs"|agecomp$Age=="18 yrs"|agecomp$Age=="19 yrs"]),
                 sum(agecomp$Percentage[agecomp$Age%in%paste0(20:39," yrs")]),
             sum(agecomp$Percentage[agecomp$Age%in%paste0(40:59," yrs")]),
             sum(agecomp$Percentage[agecomp$Age%in%paste0(60:101," yrs")])
                 )

saveRDS(agedist,"./data_and_parms/agedist.rds")
