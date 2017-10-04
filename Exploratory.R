####Look at the juvenile cruise data for gelatinous things and jellyfish
#Haul data pre-filtered in SQL (excludes faulty tow  - could do differently if just want presence/absence)


library(dplyr)
library(tidyr)

setwd('M:/Juvenile_rockfish_cruise/')
##Read in catch data
Catch = read.csv('M:/Juvenile_rockfish_cruise/Gelatinous_catch.csv')
colnames(Catch)[1] = 'CRUISE'

##Need to sum heteropods, carinaria, and pyerotrachea catches
Catch = Catch %>% mutate(SPECIES = SPECIES %>% replace(SPECIES==2050 | SPECIES==2853, 1869))
Catch = Catch %>% mutate(COMMON_NAME = COMMON_NAME %>% replace(COMMON_NAME=='CARINARIA' | COMMON_NAME=='PTEROTRACHEA', 'HETEROPOD'))
Catch = Catch %>% group_by(CRUISE, HAUL_NO, SPECIES, COMMON_NAME) %>% summarise(TOTAL_NO = sum(TOTAL_NO))


##Read in all trawls
Hauls = read.csv('M:/Juvenile_rockfish_cruise/Haul_data.csv')
colnames(Hauls)[1] = 'CRUISE'
##CTD data


##Mutate the Catch data from long to wide
Catch2 = spread(Catch[,c(1,2,4,5)], COMMON_NAME, TOTAL_NO)

##Merge in haul data
Catch_haul = right_join(Hauls, Catch2)

#Remove anything before 1990
Catch_haul = subset(Catch_haul, YEAR>1989)


##Check when sampling of some species stopped/startes
names(Catch_haul) = gsub(" ","_", names(Catch_haul))
names(Catch_haul) = gsub("-","_", names(Catch_haul))

Catch_haul1 = Catch_haul[,c(5,14:20)]

##Get the sum total by year
test = Catch_haul1 %>% group_by(YEAR) %>% 
  summarise_each(funs(sum(.,na.rm=T)))

##Get the number of positive hauls by year

##Get the number of positive hauls by year and region or station





#######Cruise summaries
Hauls = subset(Hauls, YEAR>1989 & STATION!='NULL')
##Number of times a station sampled by year

N_hauls = Hauls %>% 
          count(YEAR, STATION) %>%
          spread(STATION, n, fill=0)

write.csv(N_hauls, "N_hauls.csv")


##Remove stations sampled in fewer than 5 years







###Look at presence absence matrix of stations to see if merging is appropriate






