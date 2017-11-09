####Look at the juvenile cruise data for gelatinous things and jellyfish
#Haul data pre-filtered in SQL (excludes faulty tow  - could do differently if just want presence/absence)

##Clear the workspace
rm(list=ls(all=TRUE))

library(dplyr)
library(tidyr)
library(vegan)
library(cluster)


setwd('C:/JuvRockfish/')
##Read in catch data
Catch = read.csv('All_juv_catch.csv')
colnames(Catch)[1] = 'CRUISE'


##Need to sum heteropods, carinaria, and pyerotrachea catches
#Catch = Catch %>% mutate(SPECIES = SPECIES %>% replace(SPECIES==2050 | SPECIES==2853, 1869))
#Catch = Catch %>% mutate(COMMON_NAME = COMMON_NAME %>% replace(COMMON_NAME=='CARINARIA' | COMMON_NAME=='PTEROTRACHEA', 'HETEROPOD'))
#Catch = Catch %>% group_by(CRUISE, HAUL_NO, SPECIES, COMMON_NAME) %>% summarise(TOTAL_NO = sum(TOTAL_NO))


##Read in catch groups from John
  #New group 1: time 1990-2001, 2012-2017 core area only
  #2nd option consider would be all areas, 2012-2017 only

  #New group 2: Time 2005-2017 only, but let's try both core AND if possible 
  #whole survey area (exclude far north, only sampled since 2013)_

Spp_groups = read.csv('JuvRockfish_species_groups.csv')

#Use species group 1 and remove anything that is Ignore or NULL
Spp_group1 = subset(Spp_groups, Group_1 !='Ignore'& Group_1 !='NULL')
Spp_group1 = Spp_group1[,c(1:2)]

##Merge in the species group data with the catch data
Catch_grp1 = right_join(Catch, Spp_group1)
Catch_grp1 = subset(Catch_grp1, !is.na(CRUISE))

##Sum catch over species now assigned to the same group
Catch_grp1 = Catch_grp1 %>% group_by(CRUISE, HAUL_NO, Group_1) %>% summarise(TOT = sum(TOTAL_NO))

##Read in all trawls
Hauls = read.csv('Haul_data.csv')
colnames(Hauls)[1] = 'CRUISE'

##Station data
Station = read.csv('STANDARD_STATIONS.csv')
colnames(Station)[1] = 'STATION'

##Merge Haul and station data
Hauls = inner_join(Hauls, Station)

##Subset hauls to only those in the core area and after 1989
Hauls = subset(Hauls, STRATA=='C' & ((YEAR>1989 & YEAR<2002) | (YEAR>2011)) & STATION!='NULL')


##Mutate the Catch data from long to wide
Catch2 = spread(Catch_grp1, Group_1, TOT)


##Merge catch and  haul data
Catch_haul = left_join(Hauls, Catch2)


#######Cruise summaries
##Number of times a station sampled by year
  N_hauls = Hauls %>% 
           count(YEAR, STATION) %>%
           spread(STATION, n, fill=0)

  write.csv(N_hauls, "N_hauls.csv")

  N_hauls = as.data.frame(N_hauls)
  
##Remove stations that were sampled only 3 years or less  
  
##Remove stations sampled in fewer than 5 years
 Catch_haul = subset(Catch_haul, STATION %in% colnames(N_hauls)[colSums(N_hauls==0)<4])



##Get the row sums to see if each tow caught something
 ##Make sure to check the column numbers if you add/remove any 
Catch_haul$rowsums = rowSums(Catch_haul[,22:37], na.rm = T)
  
#Remove tows with no catch of species of interest
Catch_haul = subset(Catch_haul, rowsums>0) 

#Replace all NA's with 0
Catch_haul[is.na(Catch_haul)] = 0

##Limit to 1990-2001
Catch_haul_core = subset(Catch_haul, YEAR<2002)

##Count stations by year again
N_hauls_core = Catch_haul_core %>% 
  count(YEAR, STATION) %>%
  spread(STATION, n, fill=0)

N_hauls_core = as.data.frame(N_hauls_core)


##Now get the average of each species within a year/station combo




##Scale species to the largest catch











###########################################################################
## Analyses
##
##########################################################################
##Data for the analyses - exclude Euphausids
Catch_haul_early = mutate(Catch_haul_core,
                    Year_station = paste(YEAR, STATION, sep="."))
One_clust_early = Catch_haul_early[,c(39,22,24:37)]

##On big cluster analysis for each year, by stations
#average the catches within station/year
One_clust_early = as.data.frame(One_clust_early %>% group_by(Year_station) %>% summarise_all(funs(mean)))

rownames(One_clust_early) = One_clust_early$Year_station
One_clust_early = One_clust_early[,-1]

## Change the metric, method, and stand
clust1 = agnes(One_clust_early,diss=FALSE, metric = "euclidean", method = "ward", stand = FALSE)
x11(); plot(clust1)

##do MDS for this

mds1 = metaMDS(One_clust_early, dist="euclidean")
plot(mds1)






##One cluster analysis for each station over all years
Stations = unique(Catch_haul$STATION)
New_plot = c(7,13,19,25,31)
x11(); par(mfrow = c(3,2))

for(i in 1:length(Stations)) {
  if(i %in% New_plot){ x11(); par(mfrow = c(3,2))}
  
  Station = subset(Catch_haul, STATION == Stations[i])
  Station = as.data.frame(Station[,c(5,22,24:37)] %>% group_by(YEAR) %>% summarise_all(funs(mean)))
  rownames(Station) = Station$YEAR
  Station = Station[,-1]

   clust = agnes(Station, diss=FALSE, metric = "euclidean", method = "ward", stand = FALSE)
       plot(clust,  main = Stations[i], which.plots = 2)

           
}    








##One cluster analysis for each year, using all stations
Years = unique(Catch_haul$YEAR)
New_plot = c(3,5)#c(7,13)
x11(); par(mfrow = c(2,2))

for(i in 1:4){#length(Years)) {
  if(i %in% New_plot){ x11(); par(mfrow = c(2,2))}
  
  Year_dat = subset(Catch_haul, YEAR == Years[i])
  Year_dat = as.data.frame(Year_dat[,c(8,22,24:37)] %>% group_by(STATION) %>% summarise_all(funs(mean)))
  rownames(Year_dat) = Year_dat$STATION
  Year_dat = Year_dat[,-1]
  
  Year_dat[Year_dat>0]=1
  Year_dat_jac = vegdist(Year_dat, dist="jaccard")
  clust = agnes(Year_dat_jac, diss=T, metric = "euclidian", stand = TRUE)
  plot(clust,  main = Years[i], which.plots = 2)
  
  ##do MDS for this
  mds1 = metaMDS(Year_dat_jac)
  plot(mds1)
 # reg.arrow <- scores(mds1,choices=1:3,display="sp",scaling=2)
  points(mds1,display="sites",pch=19)
 # arrows(0,0,reg.arrow[,1],reg.arrow[,2],length=0,lty=1,cex=3,lwd=2.5,col="black")
  text(mds1,display="sites") 
  
}    

##do MDS for this



  
  

  
  
###Look at presence absence matrix of stations to see if merging is appropriate
##Change anything >0 to 1

Catch_haul_pa = One_clust_early
Catch_haul_pa[Catch_haul_pa>0]=1
 
Test_jac = vegdist(Catch_haul_pa, dist="jaccard")
##do MDS for this
mds_All1 = metaMDS(Test_jac, distance="euclidian", trymax = 100, k=8) #, previous.best = mds_All)
plot(mds_All1, display='sites',type='t', cex=.4)
#points(mds_All1,display="sites",pch=19)
#text(mds_All1,display="sites", cex=.6)




