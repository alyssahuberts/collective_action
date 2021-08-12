#### Script for sample design
# Created 6/24
# Last edited 7/5
######
library(sf)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(viridis)

# load in time series of crime data 
load("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/crime/time_series.Rdata")
colonias <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.shp")
load("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/crime/cuadrantes.Rdata")

# add date info
ts_all$year <- year(as.Date(paste(ts_all$rows.date, "-01",sep =""), format = "%Y-%m-%d"))
ts_all$month <- month(as.Date(paste(ts_all$rows.date, "-01",sep =""), format = "%Y-%m-%d"))
ts_all$date <- as.Date(paste(ts_all$rows.date, "-01",sep =""), format = "%Y-%m-%d")

# create violent crime variables
# these are population weighted
#Multiply everything by 1000 to make it more interpretable 

ts_violent <- ts_all %>%
  filter((rows.crime=="HOMICIDIO DOLOSO"|rows.crime == "SECUESTRO"|rows.crime=="VIOLACION"|rows.crime=="LESIONES POR ARMA DE FUEGO")) %>% 
  group_by(rows.cuadrante, year) %>% 
  mutate(rate = rows.count/rows.population*10000) %>% 
  summarize(violent_crime =sum(rate))  %>%
  rename(cuadrante=rows.cuadrante) %>% pivot_wider(id_cols="cuadrante", values_from="violent_crime",names_from="year",names_prefix = "violent_crime_")

# weight the different cuadrantes by land area 
cuadrantes <- st_make_valid(cuadrantes)
cuadrantes$cuadrante_area <- as.numeric(st_area(cuadrantes))

colonias <- st_make_valid(colonias)
colonias$colonia_area <- st_area(colonias)
colonias$colonia_area <- as.numeric(colonias$colonia_area)
colonia_area <- as_tibble(colonias[,c("cve_col", "colonia_area")])
colonia_area$geometry <- NULL

int <- st_intersection(colonias, cuadrantes)
int$area <- as.numeric(st_area(int))
int <- left_join(int, colonia_area)
int$share_area <- as.numeric(int$area)/as.numeric(int$colonia_area)
int <- int[,c("cve_col","cuadrante","share_area")]
int$geometry <- NULL


cuadrantes <- left_join(cuadrantes, ts_violent, by ="cuadrante")
c_c <- st_join(colonias, cuadrantes)
c_c <-left_join(c_c, int, by = c("cve_col", "cuadrante"))

violent_crime_colonia <- c_c %>% 
  group_by(cve_col) %>% 
  summarize(violent_crime_2016=sum(violent_crime_2016*share_area),
            violent_crime_2017=sum(violent_crime_2017*share_area),
            violent_crime_2018=sum(violent_crime_2018*share_area),
            violent_crime_2019=sum(violent_crime_2019*share_area),
            violent_crime_2020=sum(violent_crime_2020*share_area))

median(violent_crime_colonia$violent_crime_2016,na.rm=TRUE)

pdf(file="/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/a1.pdf",height=4,width=4)
ggplot(data = violent_crime_colonia[violent_crime_colonia$violent_crime_2016<25,])+geom_histogram(aes(x=violent_crime_2016),bins =35) + 
  labs(title = "Distribution of Violent Crime \n at the Colonia Level in 2016*", 
       x= "Violent Crimes Per 10,000 People ") +
  theme_classic() + 
  geom_text(aes(x= 15, y = 250), label = "*5 outlier \n colonias \n dropped") + 
  geom_vline(aes(xintercept= mean(violent_crime_colonia$violent_crime_2016, na.rm=TRUE)),color = "red")+
  geom_vline(aes(xintercept= median(violent_crime_colonia$violent_crime_2016, na.rm=TRUE)),color = "blue")+
  geom_text(aes(x= mean(violent_crime_colonia$violent_crime_2016, na.rm=TRUE)-2.5, y=220), color = "red", label = "mean")+
  geom_text(aes(x= median(violent_crime_colonia$violent_crime_2016, na.rm=TRUE)+2.5, y =140), color = "blue", label = "median")
dev.off()


# What's the distribution of the population crime rates?

pdf(file="/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/a2.pdf",height=4,width=8)
p1 <- ggplot(data=violent_crime_colonia[violent_crime_colonia$violent_crime_2016<20,]) + geom_sf(aes(fill=violent_crime_2016),lwd=.02) + 
  scale_fill_viridis()+ 
  labs(title = "2016")+
  theme(legend.position = "top")
p2 <- ggplot(data=violent_crime_colonia[violent_crime_colonia$violent_crime_2019<20,]) + geom_sf(aes(fill=violent_crime_2019),lwd=.02) + 
  scale_fill_viridis()+ 
  labs(title = "2019")+
  theme(legend.position = "top")
grid.arrange(p1,p2,ncol=2,top = "Violent Crimes Per 10,000 Inhabitants")
dev.off()


violent_crime_colonia$low_1.5 <- ifelse(violent_crime_colonia$violent_crime_2016 <1.5,1,0)

p1 <- ggplot(data=violent_crime_colonia[!is.na(violent_crime_colonia$low_1.5),]) + geom_sf(aes(fill=factor(low_1.5)),lwd=.02) + 
  labs(title = "Less than 1.5 per 10,000 \n 475 colonias", fill = "Crime Rates")+
  theme(legend.position = "bottom")+ 
  scale_fill_manual(values = c("white", "black"), labels = c("Moderate/high", "Low"))

violent_crime_colonia$low_2 <- ifelse(violent_crime_colonia$violent_crime_2016 <2,1,0)
p2 <- ggplot(data=violent_crime_colonia[!is.na(violent_crime_colonia$low_1),]) + geom_sf(aes(fill=factor(low_2)),lwd=.02) + 
  labs(title = "Less than 2 per 10,000 \n 945 colonias")+
  theme(legend.position = "none")+ 
  scale_fill_manual(values = c("white", "black"), labels = element_blank())

pdf(file="/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/a3.pdf",height=4,width=4)
ggplot(data=violent_crime_colonia[!is.na(violent_crime_colonia$low_1.5),]) + geom_sf(aes(fill=factor(low_1.5)),lwd=.02) + 
  labs(title = "Less than 1.5 Violent Crimes\n per 10,000 \n 683 colonias", fill = "Crime Rates")+
  theme(legend.position = "bottom")+ 
  scale_fill_manual(values = c("white", "black"), labels = c("Moderate/high", "Low"))
dev.off()


# Define crime shock as a sudden spike in crime
violent_crime_colonia <- violent_crime_colonia %>% mutate(change_16_17= violent_crime_2017-violent_crime_2016,
                                 change_17_18= violent_crime_2018-violent_crime_2017,
                                 change_18_19= violent_crime_2019-violent_crime_2018,
                                 change_19_20= violent_crime_2020-violent_crime_2019,
                                 change_16_19= violent_crime_2019-violent_crime_2016)

violent_crime_colonia <-violent_crime_colonia %>% mutate(largest_change = pmax(change_16_17, change_17_18, change_18_19,change_19_20, na.rm = TRUE))


mean(violent_crime_colonia[violent_crime_colonia$low_1==1,]$largest_change, na.rm=TRUE)

median(violent_crime_colonia[violent_crime_colonia$low_1==1,]$largest_change, na.rm=TRUE)

violent_crime_colonia$shock <-ifelse(violent_crime_colonia$largest_change>10,1,0)
  table(violent_crime_colonia[violent_crime_colonia$low_1==1,]$shock)
violent_crime_colonia$shock <-ifelse(violent_crime_colonia$largest_change>8,1,0)
  table(violent_crime_colonia[violent_crime_colonia$low_1==1,]$shock)
violent_crime_colonia$shock <-ifelse(violent_crime_colonia$largest_change>5,1,0)
  table(violent_crime_colonia[violent_crime_colonia$low_1.5==1,]$shock)

  violent_crime_colonia$shock_longterm <-ifelse(violent_crime_colonia$change_16_19>10,1,0)
  table(violent_crime_colonia[violent_crime_colonia$low_1.5==1,]$shock)
  
  violent_crime_colonia$shock_10 <-ifelse(violent_crime_colonia$largest_change>10,1,0)
  table(violent_crime_colonia[violent_crime_colonia$low_2==1,]$shock_10)
  violent_crime_colonia$shock_8 <-ifelse(violent_crime_colonia$largest_change>8,1,0)
  table(violent_crime_colonia[violent_crime_colonia$low_2==1,]$shock_8)
  violent_crime_colonia$shock_5 <-ifelse(violent_crime_colonia$largest_change>5,1,0)
  table(violent_crime_colonia[violent_crime_colonia$low_2==1,]$shock_5)
  
  violent_crime_colonia$shock_5_status <-case_when(
    violent_crime_colonia$low_2==1& violent_crime_colonia$shock_5==1 ~ "Low -> Shock",
    violent_crime_colonia$low_2==1& violent_crime_colonia$shock_5==0 ~ "Low -> No Shock",
    violent_crime_colonia$low_2==0 ~ "Moderate/\nHigh",
    TRUE~ NA_character_
  )
 p1 <-  ggplot(violent_crime_colonia[!is.na(violent_crime_colonia$shock_5_status),])+ geom_sf(aes(fill = factor(shock_5_status)), lwd=.2)+
   labs(title = "Defining Shock by an Increase \n of 5+ Violent Crimes \n Per 10,000 Inhabitants \n (73 colonias)", fill="")+
   theme_classic()+
   theme(legend.position = "bottom")
 
  
# What about places where the crime rate at least doubles in a given year?
  violent_crime_colonia <- violent_crime_colonia %>% mutate(pct_change_16_17= (violent_crime_2017-violent_crime_2016)/violent_crime_2016,
                                                            pct_change_17_18= (violent_crime_2018-violent_crime_2017)/violent_crime_2017,
                                                            pct_change_18_19= (violent_crime_2019-violent_crime_2018)/violent_crime_2018,
                                                            pct_change_19_20= (violent_crime_2020-violent_crime_2019)/violent_crime_2019,
                                                            pct_change_16_19= (violent_crime_2019-violent_crime_2016)/violent_crime_2016)
                                                            
  violent_crime_colonia <-violent_crime_colonia %>% mutate(largest_pct_change = pmax(pct_change_16_17, pct_change_17_18, pct_change_18_19,pct_change_19_20, na.rm = TRUE))
  violent_crime_colonia$largest_pct_change <-ifelse(violent_crime_colonia$largest_pct_change=="Inf", NA, violent_crime_colonia$largest_pct_change)
  violent_crime_colonia$doubles <- ifelse(violent_crime_colonia$largest_pct_change>100,1,0)
  table(violent_crime_colonia[violent_crime_colonia$low_1.5==1,]$doubles)
  table(violent_crime_colonia[violent_crime_colonia$low_1.5==1,]$doubles_longterm)
  
ggplot(violent_crime_colonia[violent_crime_colonia$low_2==1& violent_crime_colonia$largest_pct_change<1000,])+ 
  geom_histogram(aes(x=largest_pct_change), position ="dodge", bins=100)

violent_crime_colonia$doubles <- ifelse(violent_crime_colonia$largest_pct_change>100,1,0)
table(violent_crime_colonia[violent_crime_colonia$low_2==1,]$doubles)

mean(violent_crime_colonia[violent_crime_colonia$low_2==0,]$violent_crime_2016, na.rm=TRUE)

violent_crime_colonia$doubles_status <-case_when(
  violent_crime_colonia$low_2==1& violent_crime_colonia$doubles==1 ~ "Low -> Shock",
  violent_crime_colonia$low_2==1& violent_crime_colonia$doubles==0 ~ "Low -> No Shock",
  violent_crime_colonia$low_2==0 ~ "Moderate/ \nHigh",
  TRUE~ NA_character_
)

p2 <-  ggplot(violent_crime_colonia)+ geom_sf(aes(fill = factor(doubles_status)), lwd=.2)+
labs(title = "Defining Shock by a \n Doubling of Violent Crime Rate \n (83 colonias)")+
  theme_classic()+
  theme(legend.position = "none")

pdf(file="/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/a4.pdf",height=4,width=8)
grid.arrange(p1,p2, ncol=2)
dev.off()


violent_crime_colonia$low_2_whole_period <- ifelse(violent_crime_colonia$violent_crime_2016<2&
                                                  violent_crime_colonia$violent_crime_2017<2&
                                                  violent_crime_colonia$violent_crime_2018<2&
                                                  violent_crime_colonia$violent_crime_2019<2&
                                                  violent_crime_colonia$violent_crime_2020<2,1,0)

table(violent_crime_colonia$low_2_whole_period)
