#### Script for sample design
# Created 6/24
# Last edited 7/15
######
library(sf)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(viridis)
library(cobalt)
library(stargazer)
library(rstatix)
library(factoextra)
library(magrittr)
library(cem)
library(MatchIt)
library(ggmap)

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
#Multiply everything by 10000 to make it more interpretable 

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
violent_crime_colonia <- st_drop_geometry(violent_crime_colonia)
rm(list=setdiff(ls(), "violent_crime_colonia"))

# restrict the pool to places where crime rate was less than 2 per 10,000
violent_crime_colonia$low_2 <- ifelse(violent_crime_colonia$violent_crime_2016 <2,1,0)
low <- violent_crime_colonia %>% filter(low_2 == TRUE)

# create variables for the change in crime rate
#absolute
low <- low %>% mutate(change_16_17= violent_crime_2017-violent_crime_2016,
                                                          change_17_18= violent_crime_2018-violent_crime_2017,
                                                          change_18_19= violent_crime_2019-violent_crime_2018,
                                                          change_19_20= violent_crime_2020-violent_crime_2019,
                                                          change_16_19= violent_crime_2019-violent_crime_2016)
low <-low %>% mutate(largest_change = pmax(change_16_17, change_17_18, change_18_19,change_19_20, na.rm = TRUE))

# relative
low <- low %>% mutate(pct_change_16_17= (violent_crime_2017-violent_crime_2016)/violent_crime_2016,
                                                          pct_change_17_18= (violent_crime_2018-violent_crime_2017)/violent_crime_2017,
                                                          pct_change_18_19= (violent_crime_2019-violent_crime_2018)/violent_crime_2018,
                                                          pct_change_19_20= (violent_crime_2020-violent_crime_2019)/violent_crime_2019,
                                                          pct_change_16_19= (violent_crime_2019-violent_crime_2016)/violent_crime_2016)

low <-low %>% mutate(largest_pct_change = pmax(pct_change_16_17, pct_change_17_18, pct_change_18_19,pct_change_19_20, na.rm = TRUE))
low$largest_pct_change <-ifelse(low$largest_pct_change=="Inf", NA, low$largest_pct_change)

# Among these colonias, we identify the “shocked” pool as including colonias which meet any of the following conditions: 
  #	Colonias whose crime rate doubled in one of the years between 2016 and 2020
  #	Colonias which increased by more than 5 violent crimes per 10,000 people in a single year between 2016 and 2020 
  #	Colonias whose crime rate in 2019  was double that in 2016 
  #	Colonias whose crime rate in 2019  increased over the 2016 rate by more than 10 violent crimes per 10,000 
#	In our analyses, we will test these definitions of shock separately, but for the purpose of matching, we consider a colonia “shocked” if it meets any of these conditions.

low$shock <- ifelse((
  low$largest_change>5|
  low$largest_pct_change>100|
  low$pct_change_16_19 >100|
  low$change_16_19>10),
  1,0
)


#######################
# Census Data 
#######################  
# read in the shapefile at the ageb level. Note that we're only using the urban agebs, which I think is ok
# 2020 ageb - note that this database contains both manzanas and agebs but we'll stick to ageb
# from https://en.www.inegi.org.mx/programas/ccpv/2020/#Microdata
census <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/2020_census_ageb.csv",
                   col_types = cols_only(
                     MUN = col_character(), 
                     NOM_MUN = col_character(),
                     LOC = col_character(),
                     NOM_LOC = col_character(), 
                     AGEB = col_character(),
                     POBTOT = col_character(),
                     PROM_OCUP = col_character(),
                     TVIVHAB = col_character(),
                     TVIVPAR = col_character(), 
                     VIVPAR_HAB = col_character(),
                     GRAPROES=col_character(),
                     VPH_REFRI =col_character(),
                     VPH_LAVAD = col_character(),
                     VPH_AUTOM = col_character(),
                     VPH_MOTO = col_character(),
                     VPH_BICI = col_character(),
                     VPH_RADIO = col_character(),
                     VPH_TV = col_character(),
                     VPH_PC = col_character(),
                     VPH_TELEF = col_character(),
                     VPH_CEL = col_character(),
                     VPH_INTER = col_character(),
                     PNACENT = col_character(),
                     PDER_SS=col_character()
                   )
)
# drop totals and manzana level data
census_ageb <- census[census$NOM_LOC == "Total AGEB urbana",] 
rm(census)

census_ageb <- census_ageb %>% select(
  MUN, NOM_MUN, LOC,NOM_LOC,AGEB,POBTOT,GRAPROES,
  TVIVHAB, TVIVPAR, PROM_OCUP, VIVPAR_HAB, PDER_SS, PNACENT,
  VPH_REFRI,VPH_LAVAD,VPH_AUTOM, VPH_MOTO,VPH_BICI, VPH_RADIO,
  VPH_TV, VPH_PC, VPH_TELEF, VPH_CEL,VPH_INTER
)

census_ageb[,6:24] <- lapply(census_ageb[,6:24],as.numeric)
percents <- census_ageb[,12:24]
colnames(percents) <- paste("p_", colnames(percents), sep = "")
census_ageb <- cbind(census_ageb, percents)
rm(percents)
census_ageb[,-(1:26)] %<>% sapply(`/`, census_ageb[,11])
census_ageb[,c(25:26)] %<>% sapply(`/`, census_ageb[,6])


# create wealth index using pca and asset variables 
census_percents_ageb_complete <- na.omit(census_ageb[,c(27:37)]) 
assets <- prcomp(census_percents_ageb_complete, center = TRUE,scale. = TRUE)
census_percents_ageb_complete <- na.omit(census_ageb[,c(1:5, 27:37)]) 
census_percents_ageb_complete$pca_1 <-  get_pca_ind(assets)$coord[,1]
loadings <-  get_pca_var(assets)
head(loadings$contrib)

census_ageb <- left_join(census_ageb, census_percents_ageb_complete[,c("MUN", "NOM_MUN", "LOC", "AGEB","pca_1")], by = c("MUN", "NOM_MUN", "AGEB", "LOC"))

# merge the values onto the colonia-ageb level data 
# For urban areas (the areas I have here) the CVEGEO can be broken into: 
# Urban Areas
# EE+MMM+LLLL+AAA-A+NNN
# EE: State (2 digits)
# MMM: Municipality (3 digits)
# LLLL: Locality (four digits)
# AAA-A: AGEB (3 digits, dash, 1 digit) 
# NNN: Manzana/block (3 digits) 

census_ageb$CVEGEO <- paste("09", census_ageb$MUN, census_ageb$LOC, census_ageb$AGEB, sep = "")

# read in ageb shapefile 
ageb_shp <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/scince_2010/shps/df/df_ageb_urb.shp")
ageb_shp <- st_set_crs(ageb_shp, 4326)

# for every colonia, identify which agebs it intersects and the area for each (so I can do weighted averages)
colonias_shp <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.shp")
colonias_shp <- st_set_crs(colonias_shp, 4326)
colonias_shp$colonia_area <- st_area(colonias_shp)
colonias_ageb <- st_intersection(colonias_shp,ageb_shp[c("CVEGEO", "OID", "geometry")])
colonias_ageb$int_area <- st_area(colonias_ageb)
colonias_ageb$int_perc_colonia_area <- as.numeric(colonias_ageb$int_area/colonias_ageb$colonia_area)
# get rid of places where it's just map areas/boundaries (i'm defining this as less than 5% of colonia is in the ageb)
colonias_ageb <- colonias_ageb %>% filter(int_perc_colonia_area >.05)
x <- colonias_ageb %>% group_by(alcaldia, cve_col) %>% summarize(sum_area = sum(int_perc_colonia_area))


colonias_ageb <- left_join(colonias_ageb, census_ageb[,c("CVEGEO", "POBTOT","GRAPROES", "p_PNACENT", "p_PDER_SS", "pca_1")], by = "CVEGEO")
rm(census_percents_ageb_complete, assets)

# now group the ageb's by colonia and take weighted average based on land area
census_colonia <- colonias_ageb %>% 
  group_by(cve_col) %>% 
  summarize(POBTOT = sum(POBTOT*int_perc_colonia_area),
            PNACENT= mean(p_PNACENT*int_perc_colonia_area),
            pca_1=mean(pca_1*int_perc_colonia_area),
            GRAPROES = mean(GRAPROES*int_perc_colonia_area),
            PDER_SS=mean(p_PDER_SS*int_perc_colonia_area)
            )
census_colonia <- st_drop_geometry(census_colonia)

################################################################
# Look at balance between treatment and control before matching
################################################################
# join to the crime data 

low <- left_join(low, census_colonia, by = "cve_col")%>% 
  filter(!is.na(shock))

low$PNACENT <- ifelse(low$PNACENT==Inf, NA, low$PNACENT)
low$PDER_SS <- ifelse(low$PDER_SS==Inf, NA, low$PDER_SS)

# Note- the alternative pruning strategies that I tried (CEM and doing PSM
# without alcaldia) can be found at the end of this script
# what about doing psm including alcaldia?

# distributions actually look fairly similar
ggplot() +geom_histogram(aes(x=pscore.treat.alc), alpha = .3, fill = "red") + geom_histogram(aes(x=pscore.control.alc), alpha = .3, fill = "blue")+theme_classic()

################################################################
# # Propensity score matching
# this gets us 190 of each 
################################################################
pscores.logit.alc <- glm(shock ~ PNACENT+ pca_1+GRAPROES+PDER_SS +cve_alc, data = lowdf)
fittedvalues.alc <- pscores.logit.alc$fitted
pscore.treat.alc <- fittedvalues.alc[lowdf$shock==1]
pscore.control.alc <-fittedvalues.alc[lowdf$shock==0]
propensity.alc <- as.data.frame(fittedvalues.alc)
propensity.alc <- cbind(lowdf, propensity.alc)

nearest.match.alc <- matchit(formula = shock ~ PNACENT+ pca_1+GRAPROES+PDER_SS +cve_alc, data = lowdf, method = "nearest", distance = "logit", discard = "control")
data.matched.alc <- match.data(nearest.match.alc)
data.matched.alc <- left_join(data.matched.alc, propensity.alc[,c("cve_col", "fittedvalues.alc")], by = "cve_col")

ggplot() +geom_histogram(aes(x=data.matched.alc[data.matched.alc$shock==0,]$fittedvalues.alc), alpha = .3, fill = "red") + geom_histogram(aes(x=data.matched.alc[data.matched.alc$shock==1,]$fittedvalues.alc), alpha = .3, fill = "blue")+theme_classic()

# with alcaldia, we get the same number of matches but the balance looks a little worse 
table(data.matched.alc$shock, data.matched.alc$cve_alc)
test1 <- t.test(data.matched.alc$PNACENT[data.matched.alc$shock==1], data.matched.alc$PNACENT[data.matched.alc$shock==0])
test2 <- t.test(data.matched.alc$pca_1[data.matched.alc$shock==1], data.matched.alc$pca_1[data.matched.alc$shock==0])
test3 <- t.test(data.matched.alc$GRAPROES[data.matched.alc$shock==1], data.matched.alc$GRAPROES[data.matched.alc$shock==0])
test4 <- t.test(data.matched.alc$PDER_SS[data.matched.alc$shock==1], data.matched.alc$PDER_SS[data.matched.alc$shock==0])


rm(list=setdiff(ls(), c("lowdf", "data.matched.alc")))
colonias <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.shp")
tr <- data.matched.alc %>% filter(shock ==1)
co <- data.matched.alc %>% filter(shock ==0)

colonias$matched_shock <- ifelse(colonias$cve_col %in% tr$cve_col,1,0)
colonias$matched_noshock <- ifelse(colonias$cve_col %in% co$cve_col,1,0)
colonias$treatment <-NA
colonias[colonias$matched_noshock==1,"treatment"] <-0
colonias[colonias$matched_shock==1,"treatment"] <-1

pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/3_Output/Plots/p14.pdf")
g <- ggplot(colonias) + geom_sf(aes(fill=treatment), lwd=.1) + 
  scale_fill_manual(values = c("red", "blue"),
   labels = c("No Shock", "Shock")) + theme_classic()+
  theme(legend.position="bottom")+ 
  labs(fill="",
       title = "Sample: Low Crime Colonias* that \n Did and Did not Experience Crime Shocks")
 grid.arrange(g, bottom = textGrob("*as of 2016 \n Shocked and non-shocked colonias matched on propensity scores, \n using alcaldia, average education levels, Mexico City-born population,\n formal sector,  and asset index", x = 1, 
                                    hjust = 1, gp = gpar(fontface = 3L, fontsize = 9)))
dev.off()

# Save the ids for the target colonias:

sample <- data.matched.alc[,c("cve_col", "shock")]
colonias <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.shp")

# get centroids of colonias for target colonias
centroids <- colonias %>% 
  st_centroid()

sample <- left_join(sample, centroids[, c("cve_col", "geometry")])

# reverse geocode locations for the centroids for target colonias 
# load centroids of secciones electorales 
sample$coords <- as.character(sample$geometry) 
sample$X <- str_split_fixed(sample$coords,",", 2)[,1]
sample$X <- as.numeric(str_replace(sample$X, "c\\(", ""))
sample$Y <- str_split_fixed(sample$coords,",", 2)[,2]
sample$Y <- as.numeric(str_replace(sample$Y, "\\)", ""))

seccion_centroids_crd <- list()
for (i in 1:length(sample$cve_col)[1]) {
  lon <- sample$X[i]
  lat <- sample$Y[i]
  seccion_centroids_crd[[i]] <- c(lon, lat)
}

# geocode the centroids 
register_google(key = Sys.getenv("ggmap_key"))

# reverse geocode the coordinates and save them to the list
seccion_centroids_address <- list()
for (i in 1:length(seccion_centroids_crd)) {
  cve_col <- sample$cve_col[i]
  crd <- seccion_centroids_crd[[i]]
  address <- revgeocode(location = crd, output = "address")
  seccion_centroids_address[[i]] <- address
}
sample$address <- unlist(seccion_centroids_address)
write_csv(sample, path = "/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/sample.csv")
sample <- read_csv("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/sample.csv")
sample <- left_join(sample, colonias[,c("cve_col", "nombre", "alcaldia")], by = "cve_col")
write_csv(sample, path = "/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/sample_with_names.csv")
sample <- read_csv("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/sample_with_names.csv")

#sample<- data.frame(dat = 1:380)
# Divide sample into 10 initial groups of 38
for(i in 0:9){
  group <- sample[(i*38+1):((i+1)*38+1),]
  group_name = paste("group_", i+1, sep = "")
  assign(group_name, group, envir=.GlobalEnv)
  write_csv(group, path = paste("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/sample_groups/", group_name, ".csv", sep = ""))
  write_delim(as.data.frame(group$address), path = paste("/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/sample_groups/", group_name, "addresses.txt", sep = ""))
  }





#####
# appendix: alternative approaches to matching
####

##############################
# Pruning the Sample using CEM
##############################
# note that this process follows the vignette in the cem package

# start by removing missing data
lowdf <- data.frame(na.omit(low[,c("cve_col", "shock", "POBTOT", "PNACENT", "pca_1", "GRAPROES", "PDER_SS")]))

tr <- which(lowdf$shock==1)
ct <- which(lowdf$shock==0)
ntr <- length(tr)
nct <- length(ct)

vars <- c( "PNACENT", "pca_1", "GRAPROES", "PDER_SS")
imbalance(group=lowdf$shock, data = lowdf[,vars])
# note that this "imbalance" metric doesn't give t tests, it just gives the crude
# difference in means. better to use the t tests
test1 <- t.test(lowdf$PNACENT[lowdf$shock==1], lowdf$PNACENT[lowdf$shock==0])
test2 <- t.test(lowdf$pca_1[lowdf$shock==1], lowdf$pca_1[lowdf$shock==0])
test3 <- t.test(lowdf$GRAPROES[lowdf$shock==1], lowdf$GRAPROES[lowdf$shock==0])
test4 <- t.test(lowdf$PDER_SS[lowdf$shock==1], lowdf$PDER_SS[lowdf$shock==0])

# automatic coarsening
mat <- cem(treatment="shock", data = lowdf, keep.all=TRUE)
mat_sample <- lowdf[mat$matched,]

# note that the automatic coarsening doesn't necessarily get us balance
test1 <- t.test(mat_sample$PNACENT[mat_sample$shock==1], mat_sample$PNACENT[mat_sample$shock==0])
test2 <- t.test(mat_sample$pca_1[mat_sample$shock==1], mat_sample$pca_1[mat_sample$shock==0])
test3 <- t.test(mat_sample$GRAPROES[mat_sample$shock==1], mat_sample$GRAPROES[mat_sample$shock==0])
test4 <- t.test(mat_sample$PDER_SS[mat_sample$shock==1], mat_sample$PDER_SS[mat_sample$shock==0])

# coarsening by explicit user choice 
educut <- c(0, 6.5, 8.5, 12.5,17)

mat1 <- cem(treatment = "shock", data = lowdf, cutpoints = list(GRAPROES=educut))
mat1_sample <- lowdf[mat1$matched,]

test1 <- t.test(mat1_sample$PNACENT[mat1_sample$shock==1], mat1_sample$PNACENT[mat1_sample$shock==0])
test2 <- t.test(mat1_sample$pca_1[mat1_sample$shock==1], mat1_sample$pca_1[mat1_sample$shock==0])
test3 <- t.test(mat1_sample$GRAPROES[mat1_sample$shock==1], mat1_sample$GRAPROES[mat1_sample$shock==0])
test4 <- t.test(mat1_sample$PDER_SS[mat1_sample$shock==1], mat1_sample$PDER_SS[mat1_sample$shock==0])


# now add alcaldia and see how much it restricts things
lowdf$cve_alc <- substr(lowdf$cve_col, 1,2)
mat2 <- cem(treatment = "shock", data = lowdf, cutpoints = list(GRAPROES=educut))
mat2_sample <- lowdf[mat2$matched,]

# the balance looks better once we match on alcaldia, but we're reduced to 197 neighborhoods
test1 <- t.test(mat2_sample$PNACENT[mat2_sample$shock==1], mat2_sample$PNACENT[mat2_sample$shock==0])
test2 <- t.test(mat2_sample$pca_1[mat2_sample$shock==1], mat2_sample$pca_1[mat2_sample$shock==0])
test3 <- t.test(mat2_sample$GRAPROES[mat2_sample$shock==1], mat2_sample$GRAPROES[mat2_sample$shock==0])
test4 <- t.test(mat2_sample$PDER_SS[mat2_sample$shock==1], mat2_sample$PDER_SS[mat2_sample$shock==0])


#psample <- pair(mat, data = lowdf)


# Propensity score pruning
pscores.logit <- glm(shock ~ PNACENT+ pca_1+GRAPROES+PDER_SS, data = lowdf)
fittedvalues <- pscores.logit$fitted
pscore.treat <- fittedvalues[lowdf$shock==1]
pscore.control <-fittedvalues[lowdf$shock==0]
propensity <- as.data.frame(fittedvalues)
propensity <- cbind(lowdf, propensity)

# distributions actually look fairly similar
ggplot() +geom_histogram(aes(x=pscore.treat), alpha = .3, fill = "red") + geom_histogram(aes(x=pscore.control), alpha = .3, fill = "blue")+theme_classic()

# Propensity score matching
# this gets us 190 of each 
nearest.match <- matchit(formula = shock ~ PNACENT+ pca_1+GRAPROES+PDER_SS, data = lowdf, method = "nearest", distance = "logit", discard = "control")
data.matched <- match.data(nearest.match)
data.matched <- left_join(data.matched, propensity[,c("cve_col", "fittedvalues")], by = "cve_col")

ggplot() +geom_histogram(aes(x=data.matched[data.matched$shock==0,]$fittedvalues), alpha = .3, fill = "red") + geom_histogram(aes(x=data.matched[data.matched$shock==1,]$fittedvalues), alpha = .3, fill = "blue")+theme_classic()

# under propensity score matching the balance looks really good                      
test1 <- t.test(data.matched$PNACENT[data.matched$shock==1], data.matched$PNACENT[data.matched$shock==0])
test2 <- t.test(data.matched$pca_1[data.matched$shock==1], data.matched$pca_1[data.matched$shock==0])
test3 <- t.test(data.matched$GRAPROES[data.matched$shock==1], data.matched$GRAPROES[data.matched$shock==0])
test4 <- t.test(data.matched$PDER_SS[data.matched$shock==1], data.matched$PDER_SS[data.matched$shock==0])

data.matched$cve_alc <- substr(data.matched$cve_col, 1,2)
table(data.matched$shock, data.matched$cve_alc)

lowdf$cve_alc <- substr(lowdf$cve_col, 1,2)

