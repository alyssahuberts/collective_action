#### Script for summary stats and first stage tests on crime data
# Created 2/18/21
# last edited 6/24/2021
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
c_c <- st_join(colonias, cuadrantes)

# add date info
ts_all$year <- year(as.Date(paste(ts_all$rows.date, "-01",sep =""), format = "%Y-%m-%d"))
ts_all$month <- month(as.Date(paste(ts_all$rows.date, "-01",sep =""), format = "%Y-%m-%d"))
ts_all$date <- as.Date(paste(ts_all$rows.date, "-01",sep =""), format = "%Y-%m-%d")

# just violent crime
violent_crime <-ts_all %>% filter(rows.crime=="HOMICIDIO DOLOSO"|rows.crime == "SECUESTRO"|rows.crime=="VIOLACION"|rows.crime=="LESIONES POR ARMA DE FUEGO")


# Plots and summary stats
# What is the distribution of violent crime across time?
pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p1.pdf")
violent_crime %>% filter(month == 12 & year == 2020) %>% 
  ggplot() + geom_histogram(aes(x=rows.count, group = rows.crime, fill = rows.crime),position = "dodge") +
  theme_bw()+
  labs(title = "Distribution of Violent Crime by Cuadrante \n December 2020")
dev.off()

pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p2.pdf")
violent_crime %>%
  group_by(rows.sector, date, month, year, rows.crime) %>% 
  summarize(count = sum(rows.count)) %>% 
  filter(month == 12 & year == 2020) %>% 
  ggplot() + geom_histogram(aes(x=count, group = rows.crime, fill = rows.crime), position = "dodge") + theme_bw()+
  labs(title = "Distribution of Violent Crime by Sector \n December 2020")
dev.off()

robbery_non_transit = list("ROBO A CASA HABITACION C.V.", "ROBO A CUENTAHABIENTE C.V.",
                           "ROBO A NEGOCIO C.V.", "ROBO A TRANSEUNTE C.V.", 
                           "ROBO A TRANSEUNTE S.V.", "ROBO DE VEHICULO AUTOMOTOR S.V.")

pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p3.pdf")
ts_all %>% filter(month == 12 & year == 2020) %>% 
  filter(rows.crime %in% robbery_non_transit) %>% 
  ggplot() + geom_histogram(aes(x=rows.count, group = rows.crime, fill = rows.crime),position = "dodge") +
  theme_bw()+
  labs(title = "Distribution of Non-Transit Robbery by Cuadrante \n December 2020") + theme(legend.position = "bottom")
dev.off()

pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p4.pdf")
ts_all %>%
  filter(month == 12 & year == 2020) %>% 
  filter(rows.crime %in% robbery_non_transit) %>% 
  group_by(rows.sector, date, month, year, rows.crime) %>% 
  summarize(count = sum(rows.count)) %>% 
  filter(month == 12 & year == 2020) %>% 
  ggplot() + geom_histogram(aes(x=count, group = rows.crime, fill = rows.crime), position = "dodge") + theme_bw()+
  labs(title = "Distribution of Non-Transit Robbery  by Sector \n December 2020")
dev.off()

pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p5.pdf")
ggplot(data = cuadrantes) +
  geom_sf(data = cuadrantes, color = "red", fill = NA) + geom_sf(data = sectores, color = "blue", fill = NA) + theme_classic() + 
  labs(title = "Cuadrantes (Red) and Sectores (Blue)")
dev.off()

pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p6.pdf")
violent_crime %>% group_by(date, rows.crime) %>% 
  summarize(count =sum(rows.count)) %>% 
  ggplot()+ geom_line(aes(x=date, y = count, group = rows.crime, color = rows.crime))+ 
  theme_classic()+ 
  theme(legend.position = "bottom")+
  labs(title = "Change over time in violent crime \n All of Mexico City ")
dev.off()

# Divide into six month periods 
ts_all$yearhalf <- ifelse(ts_all$month <7, "Jan-June","July-Dec")
ts_all$period <- paste(ts_all$year, ts_all$yearhalf, sep = " ")

periods_cuadrantes <- ts_all %>% group_by(rows.cuadrante, period, rows.crime) %>% 
  summarize(count = sum(rows.count))

periods_cuadrantes <- periods_cuadrantes %>% group_by(rows.cuadrante, rows.crime) %>% 
  mutate(last_period_count = lag(count, order_by=period),
         change = count - last_period_count)

pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p7.pdf")
periods_cuadrantes <- periods_cuadrantes %>% rename(cuadrante = rows.cuadrante)
cuadrantes_x <- left_join(cuadrantes, periods_cuadrantes[periods_cuadrantes$rows.crime == "HOMICIDIO DOLOSO",], by = "cuadrante")
ggplot(cuadrantes_x) + geom_sf(aes(fill = change),lwd=.05) + facet_wrap(~period) + theme_classic()  + scale_fill_viridis(discrete = FALSE)
dev.off()

# my gut is that the most important is a place that has gone from 0 to not zero in the last six months. Can we codify that?

homicides <- periods_cuadrantes %>% filter(rows.crime == "HOMICIDIO DOLOSO")
homicides_wide <- homicides %>% pivot_wider(id_cols = cuadrante, names_from = period, names_prefix = "homicide_", values_from = count)
homicides_wide$zero_in_first_half_2020 <- ifelse(homicides_wide$`homicide_2020 Jan-June` ==0,1,0)
homicides_wide$nonzero_in_second_half_2020 <- ifelse(homicides_wide$`homicide_2020 July-Dec` ==0,0,1)
homicides_wide$zero_to_nonzero <- ifelse(homicides_wide$zero_in_first_half_2020==1 & homicides_wide$nonzero_in_second_half_2020==1,1,0)
cuadrantes_y <-left_join(cuadrantes, homicides_wide, by = "cuadrante")

pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p8.pdf")
ggplot(data = cuadrantes_y) + geom_sf(aes(fill = factor(zero_to_nonzero))) +
  labs(title = "Cuadrantes that went from zero homicides \n to nonzero homicides between the first \n and second half of 2020")+theme_bw()
dev.off()

# If we think crime is correlated, maybe we want to look at places that experienced an increase in crime in at least 2 violent crime categories
twentytwenty <- periods_cuadrantes %>% filter(period == "2020 July-Dec"|period == "2020 Jan-June")
twentytwenty <- twentytwenty %>% group_by(cuadrante, rows.crime) %>% 
  mutate(last_period_count = lag(count, order_by=period),
         change = count - last_period_count) %>% 
  filter(period =="2020 July-Dec")

cuadrantes_z <- left_join(cuadrantes, twentytwenty, by = "cuadrante")
pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p9.pdf")
ggplot(cuadrantes_z[cuadrantes_z$rows.crime == "HOMICIDIO DOLOSO",]) + geom_sf(aes(fill = change)) + scale_fill_viridis()+
  labs(title = "Change in Homicides between the first and second half of 2020")
dev.off()

pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p10.pdf")
ggplot(cuadrantes_z[(cuadrantes_z$rows.crime == "HOMICIDIO DOLOSO"|
                       cuadrantes_z$rows.crime == "LESIONES POR ARMA DE FUEGO"|
                       cuadrantes_z$rows.crime == "SECUESTRO"| 
                       cuadrantes_z$rows.crime == "VIOLACION"),]) + geom_sf(aes(fill = change)) + scale_fill_viridis()+ facet_wrap(~rows.crime) + theme_classic()+
  labs(title = "Changes in Violent Crime Between the First and Second Half of 2020")
dev.off()

yearly_totals <- ts_all %>% filter(rows.crime == "HOMICIDIO DOLOSO"|
                  rows.crime == "LESIONES POR ARMA DE FUEGO"|
                  rows.crime == "SECUESTRO"|
                  rows.crime == "VIOLACION") %>% 
  group_by(year, rows.cuadrante) %>% 
  summarize(total_violent_crime =sum(rows.count))%>% 
  rename(cuadrante = rows.cuadrante)

yearly_totals_wide <- yearly_totals %>%
  pivot_wider(id_cols = c("cuadrante"), names_from = year, 
                                                    values_from = total_violent_crime,
              names_prefix = "violent_crime_") 
yearly_totals_wide$violent_crime_2016_2017 <- yearly_totals_wide$violent_crime_2016 + yearly_totals_wide$violent_crime_2017
yearly_totals_wide$violent_crime_2018_2019 <- yearly_totals_wide$violent_crime_2018 + yearly_totals_wide$violent_crime_2019
yearly_totals_wide$violent_crime_diff <- yearly_totals_wide$violent_crime_2018_2019 - yearly_totals_wide$violent_crime_2016_2017

yearly_totals_wide$zero_to_nonzero <- ifelse(yearly_totals_wide$violent_crime_2016_2017 == 0  & yearly_totals_wide$violent_crime_2018_2019 !=0,1,0)

cuadrantes_a <- left_join(cuadrantes, yearly_totals_wide, by = "cuadrante")


pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p11.pdf")
 ggplot(cuadrantes_a) + 
  geom_sf(aes(fill = violent_crime_2018_2019 )) + 
  theme_classic() + 
  scale_fill_gradient2() + 
  labs(title = "Violent Crime 2018 and 2019",
       fill = "")
dev.off()

pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p12.pdf")
ggplot(cuadrantes_a) + 
  geom_sf(aes(fill = violent_crime_diff )) + 
  theme_classic() + 
  scale_fill_gradient2() + 
  labs(title = "Difference in violent crime \n 2018 & 2019 as compared to 2016 & 2017",
       fill = "")
dev.off()

pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p13.pdf")
ggplot(cuadrantes_a) + 
  geom_sf(aes(fill = factor(zero_to_nonzero) )) + 
  theme_classic() + 
  labs(title = "Places that started experiencing violent crime \n 2018/2019",
       fill = "")
dev.off()




# places that experienced an increase in violent crime in 2018/2019 as compared to 2016/2017




###### Testing potential first stage
# get colonias and cuadrantes together:
load("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/crime/cuadrantes.Rdata")
load("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/crime/time_series.Rdata")
load("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/groups/accounts_all.Rdata")
colonias <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.shp")

adm <- st_join(colonias, cuadrantes)

# we want to test a number of different specs to see what predicts having a
# "neighbors" account. We limited the "neighbors" account to ones that formed more
# than  a year ago. So we want things in wide format so we can compare them
# (although we could technically do sophisticated spatial stuff with the date
# the account was created)
  # total violent crime, 2018 and 2019
  # difference in violent 2016/2017 to 2018/2019
  # nonzero homicides in 2018/2019
  

# what i'm doing here is for each colonia, summing crime in all the cuadrantes
# it intersects (this means that crimes will be attributed to more than one
# colonia and that some crimes that weren't technically in the colonia will
# count. i think both of these are ok to live with)

adm <- left_join()

crimes_by_cuadrante <- adm %>%
  group_by(cve_col) %>% 
  summarize()

##########
# June 24
##########

# Identify all cuadrants that, as of 2016, had zero or basically zero crime 
baseline <- ts_all %>%
  filter(year ==2016 & (rows.crime=="HOMICIDIO DOLOSO"|rows.crime == "SECUESTRO"|rows.crime=="VIOLACION"|rows.crime=="LESIONES POR ARMA DE FUEGO")) %>% 
  group_by(rows.cuadrante) %>% 
  summarize(violent_crime =sum(rows.count))  %>%
  rename(cuadrante=rows.cuadrante)
table(baseline$violent_crime)

map_2016 <- left_join(cuadrantes, baseline, by = "cuadrante")

ggplot(map_2016) + geom_sf(aes(fill=violent_crime)) + scale_fill_viridis(direction=-1)+ 
  labs(title="Violent Crime as of 2016")

# restrict to places with 0 incidents of violent crime in 2016
map_2016$noviolent <-ifelse(map_2016$violent_crime==0,1,0)


ggplot(map_2016) + geom_sf(aes(fill=factor(noviolent))) +
  labs(title="Places with No Violent Crime as of 2016")

# restrict to places with 0 or one incident of violent crime in 2016
map_2016$almostnoviolent <-ifelse(map_2016$violent_crime==0|map_2016$violent_crime==1,1,0)
ggplot(map_2016) + geom_sf(aes(fill=factor(almostnoviolent))) +
  labs(title="Places with 0 or 1 Violent Crime as of 2016")


ts_violent <- ts_all %>%
  filter((rows.crime=="HOMICIDIO DOLOSO"|rows.crime == "SECUESTRO"|rows.crime=="VIOLACION"|rows.crime=="LESIONES POR ARMA DE FUEGO")) %>% 
  group_by(rows.cuadrante, year) %>% 
  summarize(violent_crime =sum(rows.count))  %>%
  rename(cuadrante=rows.cuadrante) %>% pivot_wider(id_cols="cuadrante", values_from="violent_crime",names_from="year",names_prefix = "violent_crime_")

ts_violent$nonviolent_at_baseline <-ifelse(ts_violent$violent_crime_2016==0|ts_violent$violent_crime_2016==1,1,0)
ts_violent$violent_in_2017 <- ifelse(ts_violent$nonviolent_at_baseline==1& ts_violent$violent_crime_2017>1,1,0)
ts_violent$violent_in_2018 <- ifelse(ts_violent$nonviolent_at_baseline==1& ts_violent$violent_in_2017==0& ts_violent$violent_crime_2018>1,1,0)
ts_violent$violent_in_2019 <- ifelse(ts_violent$nonviolent_at_baseline==1& ts_violent$violent_in_2017==0&ts_violent$violent_in_2018==0& ts_violent$violent_crime_2019>1,1,0)
ts_violent$violent_in_2020 <- ifelse(ts_violent$nonviolent_at_baseline==1& ts_violent$violent_in_2017==0&ts_violent$violent_in_2018==0&ts_violent$violent_in_2019==0& ts_violent$violent_crime_2020>1,1,0)
ts_violent$nonviolent_in_sample <- ifelse(ts_violent$nonviolent_at_baseline==1& ts_violent$violent_in_2017==0&ts_violent$violent_in_2018==0&ts_violent$violent_in_2019==0& ts_violent$violent_crime_2020 ==0,1,0)


ts_violent$neverviolent<- ifelse(ts_violent$violent_crime_2016<2& ts_violent$violent_crime_2017<2&
                                 ts_violent$violent_crime_2018<2& ts_violent$violent_crime_2019<2&  ts_violent$violent_crime_2020<2,1,0)

ts_violent$nonviolent_at_baseline <-ifelse(ts_violent$violent_crime_2016==0|ts_violent$violent_crime_2016==1,1,0)
ts_violent$really_violent_in_2017 <- ifelse(ts_violent$nonviolent_at_baseline==1& ts_violent$violent_crime_2017>5,1,0)
ts_violent$really_violent_in_2018 <- ifelse(ts_violent$nonviolent_at_baseline==1& ts_violent$violent_in_2017==0& ts_violent$violent_crime_2018>5,1,0)
ts_violent$really_violent_in_2019 <- ifelse(ts_violent$nonviolent_at_baseline==1& ts_violent$violent_in_2017==0&ts_violent$violent_in_2018==0& ts_violent$violent_crime_2019>5,1,0)
ts_violent$really_violent_in_2020 <- ifelse(ts_violent$nonviolent_at_baseline==1& ts_violent$violent_in_2017==0&ts_violent$violent_in_2018==0&ts_violent$violent_in_2019==0& ts_violent$violent_crime_2020>5,1,0)
ts_violent$nonviolent_in_sample <- ifelse(ts_violent$nonviolent_at_baseline==1& ts_violent$violent_in_2017==0&ts_violent$violent_in_2018==0&ts_violent$violent_in_2019==0& ts_violent$violent_crime_2020 ==0,1,0)


