####Look at the juvenile cruise data for gelatinous things and jellyfish
#Haul data pre-filtered in SQL (excludes faulty tow  - could do differently if just want presence/absence)


library(dplyr)
library(tidyr)

setwd('C:/JuvRockfish/')
##Read in catch data
Catch = read.csv('All_juv_catch.csv')
colnames(Catch)[1] = 'CRUISE'

##Need to sum heteropods, carinaria, and pyerotrachea catches
Catch = Catch %>% mutate(SPECIES = SPECIES %>% replace(SPECIES==2050 | SPECIES==2853, 1869))
Catch = Catch %>% mutate(COMMON_NAME = COMMON_NAME %>% replace(COMMON_NAME=='CARINARIA' | COMMON_NAME=='PTEROTRACHEA', 'HETEROPOD'))
Catch = Catch %>% group_by(CRUISE, HAUL_NO, SPECIES, COMMON_NAME) %>% summarise(TOTAL_NO = sum(TOTAL_NO))


##Read in catch groups from John
Spp_groups = read.csv('JuvRockfish_species_groups.csv')




##Read in all trawls
Hauls = read.csv('Haul_data.csv')
colnames(Hauls)[1] = 'CRUISE'





##Station data
Station = read.csv('STANDARD_STATIONS.csv')
colnames(Station)[1] = 'STATION'

##Merge Haul and station data
Hauls = inner_join(Hauls, Station)

##Subset hauls to only those in the core area
Hauls = subset(Hauls, STRATA=='C')



##Mutate the Catch data from long to wide
Catch2 = spread(Catch[,c(1,2,4,5)], COMMON_NAME, TOTAL_NO)



##Merge catch and  haul data
Catch_haul = right_join(Hauls, Catch2)

#Remove anything before 1990
Catch_haul = subset(Catch_haul, YEAR>1989)



#New group 1: time 1990-2001, 2012-2017 - core area only, but 2nd option to 
#consider would be all areas, 2012-2017 only …


#New group 2: Time 2005-2017 only, but let's try both core AND if possible 
#whole survey area (exclude far north, only sampled since 2013)_





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

N_hauls = as.data.frame(N_hauls)
##Remove stations sampled in fewer than 5 years

N_hauls1 = N_hauls[colSums(N_hauls==0)<=4,]




###Look at presence absence matrix of stations to see if merging is appropriate






