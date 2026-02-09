library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(plotly)
library(tidyverse)
library(synthdid)

data <- fread("~/data/S2_Dhis2Data.csv")
data <- as.data.frame(data)

# facility data 
facility <- fread("~/data/S1_master_facility_update_11.csv")

# Processing
data%>%
  group_by(name1,productID)%>%
  mutate(normAvg=mean(consumption,na.rm=TRUE))%>%
  mutate(normStd=sd(consumption,na.rm=TRUE))%>%
  filter(!is.na(consumption))->data
data <- as.data.table(data)
alldata<- data[,.(stockout=sum(stockout,na.rm=TRUE),consumption=sum(consumption,na.rm=TRUE)),by=.(quarter,hf_pk,name1,productID,normAvg,normStd)]
alldata$normConsump <- (alldata$consumption-alldata$normAvg)/alldata$normStd
alldata%>%
  left_join(facility)%>%
  mutate(treat=ifelse(district%in%c("Kono","Karene","Tonkolili","Pujehun","Falaba"),1,0))%>%
  mutate(time=ifelse(quarter>="2023Q2",1,0))%>%
  mutate(type = as.numeric(factor(facility_type)))%>%
  mutate(productIDFac=paste0(productID,"-",hf_pk))%>%
  mutate(quarterID = as.numeric(factor(quarter)))-> alldata2

alldataDiD <- alldata2[,.(stockout=sum(stockout,na.rm=TRUE),consumption=mean(normConsump,na.rm=TRUE)),by=.(quarter,quarterID,type,time,treat,hf_pk,district)]


# DiD
setup = panel.matrices(mainData2)
did = did_estimate(setup$Y, setup$N0, setup$T0)
did #0.128

se = sqrt(vcov(sdid, method='jackknife'))
se #0.04560403
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (0.03, 0.21)

a <- synthdid_plot(did)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.128/0.5954063 #0.2149793 


#### SynthDiD Alternative Control (Staggered) ####
# data with alternative controls
all2 <- fread("~/S4_AlternativeData.csv")

all2%>%
  group_by(name1,productID)%>%
  mutate(normAvg=mean(consumption,na.rm=TRUE))%>%
  mutate(normStd=sd(consumption,na.rm=TRUE))%>%
  filter(!is.na(consumption))->all22
all22 <- as.data.table(all22)

allstockout<- all22[,.(stockout=sum(stockout,na.rm=TRUE),consumption=sum(consumption,na.rm=TRUE)),by=.(quarter,hf_pk,name1,productID,normAvg,normStd)]
allstockout$normConsump <- (allstockout$consumption-allstockout$normAvg)/allstockout$normStd

allstockout%>%
  left_join(facility)%>%
  mutate(treat=ifelse(productID%in%c(38,16,5,28,6,33,47,23,1,24,19,34,44,56,51,9,42,21,39,46,50,52,18,7,22,2,3,4,20,43,32,29,17,54,15,35),1,0))%>%
  mutate(treatD=ifelse(district%in%c("Tonkolili","Falaba","Karene","Pujehun","Kono")&quarter=="2023Q2",1,treat))%>%
  mutate(time=ifelse(quarter>="2023Q2",1,0))%>%
  mutate(hf_pk=ifelse(treat==1,hf_pk,paste0(hf_pk,"_",hf_pk)))%>%
  filter(!facility_type=="Clinic")%>%
  mutate(type = as.numeric(factor(facility_type)))%>%
  mutate(quarterID = as.numeric(factor(quarter)))-> dataAlt

dataDiD <- dataAlt[,.(stockout=sum(stockout,na.rm=TRUE),consumption=mean(normConsump,na.rm=TRUE)),by=.(quarter,quarterID,treat,treatD,time,hf_pk,district)]

dataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=treat*time*treatD)%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated)%>%
  drop_na()->staggeredData

staggeredData%>%
  mutate(c=1)%>%
  group_by(hf_pk)%>%
  mutate(Sumc=sum(c))%>%
  dplyr::select(hf_pk,Sumc)%>%
  unique()%>%
  filter(Sumc<5)-> check
nobal <- check$hf_pk

dataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=treat*time*treatD)%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated)%>%
  dplyr::filter(!hf_pk%in%nobal)-> staggeredData2

setup = panel.matrices(staggeredData2)

sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #0.095

se = sqrt(vcov(sdid, method='jackknife'))
se #0.02225102
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (0.05, 0.14)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.095/0.5212862 #0.1822415 --> 18%


#### Substitution Analysis #### 
data%>%
  mutate(consumption=ifelse(productID==54,consumption/3,consumption))%>% #Assume ID=54 substitute ID=35
  mutate(consumption=ifelse(productID==46,consumption/2, consumption))%>%#Assume ID=46 substitute ID=50 
  filter(!is.na(consumption))%>%
  mutate(productID=ifelse(productID%in%c(2,3,4),100,
                          ifelse(productID%in%c(21,47,52),101,
                                 ifelse(productID%in%c(29,56),102,
                                        ifelse(productID%in%c(6,33),103,
                                               ifelse(productID%in%c(35,54),104,
                                                      ifelse(productID%in%c(46,50),105, productID)))))))%>%
  #ifelse(productID%in%c(15,32),106,productID))))))))%>% #alternative categorization
  group_by(productID)%>%
  mutate(normAvg=mean(consumption,na.rm=TRUE))%>%
  mutate(normStd=sd(consumption,na.rm=TRUE))->dataS
dataS <- as.data.table(dataS)
alldata<- dataS[,.(stockout=sum(stockout,na.rm=TRUE),consumption=sum(consumption,na.rm=TRUE)),by=.(quarter,hf_pk,productID,normAvg,normStd)]
alldata$normConsump <- (alldata$consumption-alldata$normAvg)/alldata$normStd
alldata%>%
  left_join(facility)%>%
  mutate(treat=ifelse(district%in%c("Kono","Karene","Tonkolili","Pujehun","Falaba"),1,0))%>%
  mutate(time=ifelse(quarter>="2023Q2",1,0))%>%
  mutate(type = as.numeric(factor(facility_type)))%>%
  mutate(productIDFac=paste0(productID,"-",hf_pk))%>%
  mutate(quarterID = as.numeric(factor(quarter)))-> alldata2

alldataDiD <- alldata2[,.(stockout=sum(stockout,na.rm=TRUE),consumption=mean(normConsump,na.rm=TRUE)),by=.(quarter,quarterID,type,time,treat,hf_pk,district)]

alldataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated,district,treat,type)-> mainData

mainData%>%
  mutate(c=1)%>%
  group_by(hf_pk)%>%
  mutate(Sumc=sum(c))%>%
  dplyr::select(hf_pk,Sumc)%>%
  unique()%>%
  filter(Sumc<5)-> check
nobal <- check$hf_pk 

alldataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated,district,treat,type)%>%
  dplyr::filter(!hf_pk%in%nobal)-> mainData2

setup = panel.matrices(mainData2)
sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #0.119  #0.132

se = sqrt(vcov(sdid, method='jackknife'))
se #0.04817968 #0.04847752
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (0.03, 0.21) #95% CI (0.04, 0.23)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.119/0.672 #0.1770833 --> 18%
0.132/0.65 #0.2030769 --> 20%


#### Missing data analysis ####
data <- fread("~/data/S2_Dhis2Data.csv")
data <- as.data.frame(data)
facility <- fread("~/data/S1_master_facility_update_11.csv")

data%>%
  group_by(name1,productID)%>%
  mutate(normAvg=mean(consumption,na.rm=TRUE))%>%
  mutate(normStd=sd(consumption,na.rm=TRUE))%>%
  filter(!is.na(consumption))->data
data <- as.data.table(data)
alldata<- data[,.(stockout=sum(stockout,na.rm=TRUE),consumption=sum(consumption,na.rm=TRUE)),by=.(quarter,hf_pk,name1,productID,normAvg,normStd)]
alldata$normConsump <- (alldata$consumption-alldata$normAvg)/alldata$normStd
alldata%>%
  left_join(facility)%>%
  mutate(treat=ifelse(district%in%c("Kono","Karene","Tonkolili","Pujehun","Falaba"),1,0))%>%
  mutate(time=ifelse(quarter>="2023Q2",1,0))%>%
  mutate(type = as.numeric(factor(facility_type)))%>%
  mutate(productIDFac=paste0(productID,"-",hf_pk))%>%
  mutate(quarterID = as.numeric(factor(quarter)))-> alldata2
df <- alldata2
df %>% filter(quarter >= "2022Q3" & quarter <= "2023Q3") -> df

# Define treated time as 2023Q2 (keeping for reference if needed)
treated_time <- "2023Q2"

# Step 1: Create unique combinations with all quarters
unique_medicines <- df %>% dplyr::select(name1) %>% distinct()
unique_quarters <- df %>% select(quarter, quarterID) %>% distinct() %>% arrange(quarter)
unique_hf_treat_district <- df %>% select(hf_pk, treat, district) %>% distinct()

all_combinations <- expand_grid(
  name1 = unique_medicines$name1,
  hf_pk = unique_hf_treat_district$hf_pk,
  quarter = unique_quarters$quarter
) %>%
  left_join(unique_hf_treat_district, by = "hf_pk") %>%
  left_join(unique_quarters, by = "quarter")

df_for_join <- df %>% select(-treat, -district, -quarterID)

df_complete <- all_combinations %>%
  left_join(df_for_join, by = c("name1", "hf_pk", "quarter"))

df_complete <- df_complete %>%
  mutate(
    Missing = ifelse(is.na(consumption), 1, 0),
    time=ifelse(quarter>="2023Q2",1,0))

df_complete <- as.data.table(df_complete)

missdataDiD <- df_complete[,.(missing=mean(Missing)),by=.(quarter,quarterID,time,treat,hf_pk)]

missdataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,missing,treated)-> missData

missData%>%
  mutate(c=1)%>%
  group_by(hf_pk)%>%
  mutate(Sumc=sum(c))%>%
  dplyr::select(hf_pk,Sumc)%>%
  unique()%>%
  filter(Sumc<5)-> check
nobal <- check$hf_pk 

missdataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,missing,treated)%>%
  dplyr::filter(!hf_pk%in%nobal)-> missData2

missData2 %>%
  filter(hf_pk%in%mainData2$hf_pk) -> missData2

setup = panel.matrices(missData2)
sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #-0.008

se = sqrt(vcov(sdid, method='jackknife'))
se #0.005800473
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (-0.02, 0.00)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]
-0.008/0.5775366 #-0.01385194 --> -1.4%, p=0.168

## Only retain products without significant different missing (product without imbalanced missing)
alldata %>%
  select(productID,name1)%>%
  unique() -> pd
alldata%>%
  left_join(facility)%>%
  filter(productID%in%c(38,15,47,24,56,51,42,17,54,21,39,35,46,18,7,22,20,43))%>% #post treatment is not significant
  mutate(treat=ifelse(district%in%c("Kono","Karene","Tonkolili","Pujehun","Falaba"),1,0))%>%
  mutate(time=ifelse(quarter>="2023Q2",1,0))%>%
  mutate(type = as.numeric(factor(facility_type)))%>%
  mutate(productIDFac=paste0(productID,"-",hf_pk))%>%
  mutate(quarterID = as.numeric(factor(quarter)))-> alldata2Null

alldataDiDNull <- alldata2Null[,.(stockout=sum(stockout,na.rm=TRUE),consumption=mean(normConsump,na.rm=TRUE)),by=.(quarter,quarterID,type,time,treat,hf_pk,district)]
alldataDiDNull%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated,district,treat,type)-> mainDataNull

mainDataNull%>%
  mutate(c=1)%>%
  group_by(hf_pk)%>%
  mutate(Sumc=sum(c))%>%
  dplyr::select(hf_pk,Sumc)%>%
  unique()%>%
  filter(Sumc<5)-> check
nobal <- check$hf_pk 

alldataDiDNull%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated,district,treat,type)%>%
  dplyr::filter(!hf_pk%in%nobal)-> mainDataNull2

setup = panel.matrices(mainDataNull2)
sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #0.132

se = sqrt(vcov(sdid, method='jackknife'))
se #0.0540
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (0.02, 0.24)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.132/0.7479105 #0.1764917 -> 18%


#### SynthDiD Stockout as outcome ####
alldataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,stockout,treated)->StockoutData

StockoutData%>%
  mutate(c=1)%>%
  group_by(hf_pk)%>%
  mutate(Sumc=sum(c))%>%
  dplyr::select(hf_pk,Sumc)%>%
  unique()%>%
  filter(Sumc<5)-> check
nobal <- check$hf_pk 

alldataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,stockout,treated)%>%
  dplyr::filter(!hf_pk%in%nobal)-> StockoutData2


setup = panel.matrices(StockoutData2)
sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #-0.280 

se = sqrt(vcov(sdid, method='jackknife'))
se #0.1798671
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #"95% CI (-0.63, 0.07)"

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

-0.28/6.157056 # -0.04547628


# Treatment impact by facility type 
# Hospital & CHC (serve at district and Chiefdom level)

alldataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated,district,treat,type)-> mainData

mainData%>%
  mutate(c=1)%>%
  group_by(hf_pk)%>%
  mutate(Sumc=sum(c))%>%
  dplyr::select(hf_pk,Sumc)%>%
  unique()%>%
  filter(Sumc<5)-> check
nobal <- check$hf_pk 

alldataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated,district,treat,type)%>%
  dplyr::filter(!hf_pk%in%nobal)-> mainData2


mainData2 %>% 
  left_join(facility)%>%
  filter(facility_type%in%c("CHC","Hospital"))%>%
  select(hf_pk,quarterID,consumption,treated,district,treat)-> mainLargeF 

setup = panel.matrices(mainLargeF)
sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #0.368

se = sqrt(vcov(sdid, method='jackknife'))
se #0.1827058
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (0.01, 0.73)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.368/1.034301 #0.3557958 --> 36%

# CHP & MCHP (serve at town and smaller level)
mainData2 %>% 
  left_join(facility)%>%
  filter(facility_type%in%c("CHP","MCHP"))%>%
  select(hf_pk,quarterID,consumption,treated,district,treat)-> mainSmallF 

setup = panel.matrices(mainSmallF)
sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #0.052

se = sqrt(vcov(sdid, method='jackknife'))
se #0.03689728
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (-0.02, 0.12)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.052/0.5263290 #0.09879752 --> 10%


#### Matching Analysis ####
library(sf)
library(dplyr)
library(readr)

# Load district boundaries (SHP)
# get data from here: https://data.humdata.org/dataset/geoboundaries-admin-boundaries-for-sierra-leone/resource/c669688a-9e56-44bd-8908-bfa55b8e6f16
districts <- st_read("~/geoBoundaries-SLE-ADM2-all/geoBoundaries-SLE-ADM2.shp")
facility <- fread("~/data/S1_master_facility_update_11.csv")

facilities_df <- facility %>%
  mutate(
    treatment = ifelse(district %in% c("Kono","Karene","Tonkolili","Pujehun","Falaba"), 1, 0),
    lat  = as.numeric(lat),
    long = as.numeric(long)
  ) %>%
  select(hf_pk, lat, long, treatment)

facilities_sf <- st_as_sf(facilities_df, coords = c("long","lat"), crs = 4326) %>%
  st_transform(32629)


districts <- st_transform(districts, 32629) %>%
  st_make_valid()

district_borders <- districts %>%
  st_boundary() %>%
  st_cast("MULTILINESTRING") %>%
  st_union()

# Distance to nearest border (km)
dist_m <- st_distance(facilities_sf, district_borders)  # units object
facilities_sf$dist_to_border_km <- as.numeric(st_distance(facilities_sf, district_borders)) / 1000

# Flag near-border facilities (threshold in km)
border_km_thresh <- 16  # (change here if you want 5/10/15 km)
facilities_sf$near_border <- facilities_sf$dist_to_border_km <= border_km_thresh

# Keep near-border and bring back plain columns if needed
fac_use <- facilities_sf %>%
  filter(near_border) %>%
  left_join(st_drop_geometry(facilities_df), by = "hf_pk")

# Split treated vs control
treated <- fac_use %>% filter(treatment.x == 1)
control <- fac_use %>% filter(treatment.x == 0)

if (nrow(treated) > 0 && nrow(control) > 0) {
  # treated → nearest control
  nc_idx <- st_nearest_feature(treated, control)
  d_tc   <- st_distance(treated, control[nc_idx, ], by_element = TRUE)
  treated$nearest_opposite_km <- as.numeric(d_tc) / 1000
  
  # control → nearest treated
  nt_idx <- st_nearest_feature(control, treated)
  d_ct   <- st_distance(control, treated[nt_idx, ], by_element = TRUE)
  control$nearest_opposite_km <- as.numeric(d_ct) / 1000
} else {
  stop("Either treated or control pool is empty — cannot compute cross-distances.")
}

# Combine & keep pairs within 30 km
fac_keep <- rbind(treated, control) %>%
  filter(nearest_opposite_km <= 30)

# Final output based on defined near-border distance (5/10/15km)
output15 <- fac_keep %>%
  st_drop_geometry() %>%
  select(hf_pk, treatment.x, dist_to_border_km, nearest_opposite_km, near_border)

output10 <- fac_keep %>%
  st_drop_geometry() %>%
  select(hf_pk, treatment.x, dist_to_border_km, nearest_opposite_km, near_border)

output5 <- fac_keep %>%
  st_drop_geometry() %>%
  select(hf_pk, treatment.x, dist_to_border_km, nearest_opposite_km, near_border)

# filter data to border facilities: change the distance threshold as you want
mainBorder <- mainData2 %>% filter(hf_pk %in% output5$hf_pk)

setup = panel.matrices(mainBorder)
did = did_estimate(setup$Y, setup$N0, setup$T0)
did #0.154 (5km), 0.166(10km), 0.121 (15km)

se = sqrt(vcov(did, method='jackknife'))
se #0.06629853 (5km), 0.06388191(10km),  0.05435303 (15km)

CI <- sprintf('95%% CI (%1.2f, %1.2f)', did - 1.96 * se, did + 1.96 * se)
CI #5km: 95% CI (0.02, 0.28), 10km:95% CI (0.04, 0.29), 15km: 95% CI (0.01, 0.23)

a <- synthdid_plot(did)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.154/0.6268761 #5km: 0.2456626 -> 25%

0.166/0.5458606 #10km: 0.3041069 -> 30%
0.121/0.5795374 #15km: 0.2087872 -> 21%

#### Rural/Urban ####
# purchase Pro dataset from here: https://simplemaps.com/data/world-cities
# We only include it for review process. Please don't distribute it, thank you! 
city <- fread("~/data/worldcities.csv")
city %>%
  filter(country=="Sierra Leone") -> cities
table(cities$ranking)

sf1 <- st_as_sf(facility, coords = c("lat", "long"), crs = 4326)
sf2 <- st_as_sf(cities, coords = c("lat", "lng"), crs = 4326)
nearest_indices <- st_nearest_feature(sf1, sf2)
distances <- st_distance(sf1, sf2[nearest_indices, ], by_element = TRUE)

facility$nearest_location <- sf2$city[nearest_indices]
facility$distance_meters <- as.numeric(distances) 
facility$distance_miles <- facility$distance_meters * 0.000621371
facility$urban <- ifelse(facility$distance_miles<=0.25,1,0)
facility$urban <- ifelse(facility$distance_miles<=1,1,0)
facility %>%
  select(hf_pk,urban) -> ruralurban

alldataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  left_join(ruralurban)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated,district,treat,urban)-> mainData

mainData%>%
  mutate(c=1)%>%
  group_by(hf_pk)%>%
  mutate(Sumc=sum(c))%>%
  dplyr::select(hf_pk,Sumc)%>%
  unique()%>%
  filter(Sumc<5)-> check
nobal <- check$hf_pk 

alldataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  left_join(ruralurban)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated,district,treat,urban)%>%
  dplyr::filter(!hf_pk%in%nobal)-> mainData2

# urban
mainData2 %>% 
  filter(urban==1)%>%
  select(hf_pk,quarterID,consumption,treated,district,treat)-> mainurban
table(mainurban$type)
setup = panel.matrices(mainurban)
sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #0.291 #0.334 (1mile) #0.257 (2miles)

se = sqrt(vcov(sdid, method='jackknife'))
se #0.3947961 #0.3150689 (1mile) #0.233001 (2mile)
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (-0.48, 1.06) #95% CI (-0.28, 0.95) (1mile) #95% CI (-0.20, 0.71) (2mile)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.291/1.192345 #0.25mile: 0.2440569 ->24%
0.334/1.056385 #1mile: 0.3161726 -> 32%


# Rural
mainData2 %>% 
  filter(urban==0)%>%
  select(hf_pk,quarterID,consumption,treated,district,treat)-> mainrural

setup = panel.matrices(mainrural)
sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #0.107 #0.09 (1mile) #0.093 (2mile)

se = sqrt(vcov(sdid, method='jackknife'))
se #0.04266099 #0.04398454 (1mile) #0.04450709 (2miles)
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (0.02, 0.19) #95% CI (0.00, 0.18) (1mile) #95% CI (0.01, 0.18) (2mile)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.107/0.5750321 #0.25mile: 0.1860766 -> 19%
0.09/0.5774403 #1mile:0.1558603 -> 16%


#### SynthDiD Imputation (Avg Consumption) ####
df <- alldata2
unique_medicines <- df %>% select(name1) %>% distinct()
unique_quarters <- df %>% select(quarterID) %>% distinct() %>% arrange(quarterID)
unique_hf_treat <- df %>% select(hf_pk, treat) %>% distinct()

# Create all possible combinations
all_combinations <- expand_grid(
  name1 = unique_medicines$name1,
  hf_pk = unique_hf_treat$hf_pk,
  quarterID = unique_quarters$quarterID
) %>%
  left_join(unique_hf_treat, by = "hf_pk")

# Merge with original data to identify missing observations
df_without_treat <- df %>% select(-treat) %>% select(-time)
df_complete <- all_combinations %>%
  left_join(df_without_treat, by = c("name1", "hf_pk", "quarterID"))
df_complete %>%
  group_by(name1, hf_pk) %>%
  mutate(avg_consumption = mean(consumption, na.rm = TRUE)) %>%
  ungroup()%>%
  mutate(consumption=ifelse(is.na(consumption) & treat == 0,avg_consumption,
                            ifelse(is.na(consumption) & quarterID<=14,avg_consumption,
                                   ifelse(is.na(consumption) & quarterID>14,avg_consumption,consumption)))) -> df_complete
df_complete%>%
  group_by(name1,hf_pk)%>%
  mutate(normAvg=mean(consumption,na.rm=TRUE))%>%
  mutate(normStd=sd(consumption,na.rm=TRUE))->df_complete

df_complete$normConsump <- (df_complete$consumption-df_complete$normAvg)/df_complete$normStd

df_complete <- df_complete %>%
  mutate(time = ifelse(quarterID > 14, 1, 0))
df_complete <- as.data.table(df_complete)
df_complete %>% select(quarterID, time, treat, hf_pk, normConsump) %>% unique() -> df_complete

dataDiD <- df_complete[,.(consumption=mean(normConsump,na.rm=TRUE)),by=.(quarterID,time,treat,hf_pk)]
dataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated)->ImputeAvgData

ImputeAvgData%>%
  mutate(c=1)%>%
  group_by(hf_pk)%>%
  mutate(Sumc=sum(c))%>%
  dplyr::select(hf_pk,Sumc)%>%
  unique()%>%
  filter(Sumc<5)-> check
nobal <- check$hf_pk 

dataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated)%>%
  dplyr::filter(!hf_pk%in%nobal)-> ImputeAvgData2

setup = panel.matrices(ImputeAvgData2)
sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #0.067

se = sqrt(vcov(sdid, method='jackknife'))
se #0.03102956
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (0.01, 0.13)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.067/0.3119449 #0.2147815 --> 21%

#### SynthDiD Imputation (Population based) ####
## processing data for population based data
df <- alldata2
unique_medicines <- df %>% select(productID) %>% distinct()
unique_quarters <- df %>% select(quarterID) %>% distinct() %>% arrange(quarterID)
unique_hf_treat <- df %>% select(hf_pk, treat) %>% distinct()

all_combinations <- expand_grid(
  productID = unique_medicines$productID,
  hf_pk = unique_hf_treat$hf_pk,
  quarterID = unique_quarters$quarterID
) %>%
  left_join(unique_hf_treat, by = "hf_pk")

df_without_treat <- df %>% select(-treat) %>% select(-time)

df_complete <- all_combinations %>%
  left_join(df_without_treat, by = c("productID", "hf_pk", "quarterID"))
treatpost_NA <- df_complete %>%
  filter(treat==1 & quarterID>14 & is.na(consumption))%>%
  select(productID, hf_pk, quarterID)%>%
  mutate(Missing=1)



# Use pop data to run main analysis
dfImp <- fread("~/data/S5_dfImp_popbased.csv")

imputed_df <- df_complete %>%
  left_join(dfImp)%>% # data include Excel allocation, our allocation, and population demand 
  mutate(time=ifelse(quarterID>=15,1,0))%>%
  group_by(hf_pk,productID)%>%
  mutate(AvgConsump=mean(consumption,na.rm=T))%>%
  ungroup()%>%
  mutate(popImp=ifelse(time==0 & is.na(consumption),min(ExcelAlloc,popD),
                       ifelse(quarterID==15&is.na(consumption),min(Q2AIalloc,popDQ3),
                              ifelse(quarterID==16&is.na(consumption),min(Q2AIalloc,popDQ3),AvgConsump))))%>%
  mutate(consumption=ifelse(is.na(consumption),popImp,consumption))

df_complete <- as.data.table(imputed_df)
df_complete%>%
  group_by(productID,hf_pk)%>%
  mutate(normAvg=mean(consumption,na.rm=TRUE))%>%
  mutate(normStd=sd(consumption,na.rm=TRUE))->df_complete

df_complete$normConsump <- (df_complete$consumption-df_complete$normAvg)/df_complete$normStd
df_complete <- as.data.table(df_complete)
ImputePop <-df_complete[,.(consumptionFinal=mean(normConsump,na.rm=TRUE)),by=.(quarterID,time,treat,hf_pk)]

ImputePop%>%
  mutate(quarterID=as.numeric(quarterID))%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumptionFinal,treated)%>%
  drop_na()->WorstData

WorstData%>%
  mutate(c=1)%>%
  group_by(hf_pk)%>%
  mutate(Sumc=sum(c))%>%
  dplyr::select(hf_pk,Sumc)%>%
  unique()%>%
  filter(Sumc<5)-> check
nobal <- check$hf_pk 

ImputePop%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumptionFinal,treated)%>%
  dplyr::filter(!hf_pk%in%nobal)-> WorstData2

setup = panel.matrices(WorstData2)
sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #0.076
se = sqrt(vcov(sdid, method='jackknife'))
se #0.028
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (0.02, 0.13)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]] 

0.076/0.2769140 #27%


#### SynthDiD Imputation (Low Rank) ####
library(softImpute)
library(dplyr)
library(tidyr)

# Imputation process 
#df <-alldata2
#unique_medicines <- df %>% select(name1) %>% distinct()
#unique_quarters <- df %>% select(quarterID) %>% distinct() %>% arrange(quarterID)
#unique_hf_treat <- df %>% select(hf_pk, treat) %>% distinct()

## Create all possible combinations
#all_combinations <- expand_grid(
#  name1 = unique_medicines$name1,
#  hf_pk = unique_hf_treat$hf_pk,
#  quarterID = unique_quarters$quarterID
#) %>%
#  left_join(unique_hf_treat, by = "hf_pk")

## Merge with original data to identify missing observations
#df_without_treat <- df %>% select(-treat) %>% select(-time)
#df_complete <- all_combinations %>%
#  left_join(df_without_treat, by = c("name1", "hf_pk", "quarterID"))

#treatpost_NA <- df_complete %>%
#  filter(treat==1 & quarterID>14 & is.na(consumption))%>%
#  select(productID, hf_pk, quarterID)%>%
#  mutate(Missing=1)

## Pivot data into a matrix (Product-Facility as rows, Quarter as columns)
#df_complete <- df_complete[!duplicated(df_complete),]
#matrix_data <- df_complete %>%
#  select(name1, hf_pk,quarterID, consumption)%>%
#  unite("product_facility", name1, hf_pk, sep = "_") %>%
#  pivot_wider(names_from = quarterID, values_from = consumption) %>%
#  as.data.frame()

## Assign row names and remove row identifier column
#rownames(matrix_data) <- matrix_data$product_facility
#matrix_data$product_facility <- NULL
#matrix_data <- as.matrix(matrix_data)

## Apply softImpute for low-rank imputation
#fit <- softImpute(matrix_data, rank.max = 2, lambda = 0.1)

## Impute missing values
#imputed_matrix <- complete(matrix_data, fit)

## Convert the imputed matrix back to a long format dataframe
#imputed_df <- as.data.frame(imputed_matrix)
#imputed_df$product_facility <- rownames(imputed_matrix)
#rownames(imputed_df) <- NULL

#imputed_df <- imputed_df %>%
#  separate(product_facility, into = c("name1", "hf_pk"), sep = "_") %>%
#  pivot_longer(cols = -c(name1, hf_pk), names_to = "quarterID", values_to = "consumption")%>%
#  mutate(hf_pk=as.numeric(hf_pk))%>%
#  mutate(quarterID=as.numeric(quarterID))%>%
#  left_join(facility)%>%
#  mutate(treat=ifelse(district%in%c("Kono","Karene","Tonkolili","Pujehun","Falaba"),1,0))%>%
#  mutate(time=ifelse(quarterID>=15,1,0))%>%
#  mutate(consumption=ifelse(consumption<0,0,consumption))

#df_complete <- as.data.table(imputed_df)
#df_complete%>%
#  group_by(name1,hf_pk)%>%
#  mutate(normAvg=mean(consumption,na.rm=TRUE))%>%
#  mutate(normStd=sd(consumption,na.rm=TRUE))->df_complete

#df_complete$normConsump <- (df_complete$consumption-df_complete$normAvg)/df_complete$normStd

#df_complete <- df_complete %>%
#  left_join(treatpost_NA)%>%
#  mutate(Missing=ifelse(is.na(Missing),0,Missing))%>%
#  mutate(consumptionImp=ifelse(Missing==1, consumption,consumption))

#df_complete <- as.data.table(df_complete)

#dataDiD <-df_complete[,.(consumption=mean(normConsump,na.rm=TRUE)),by=.(quarterID,time,treat,hf_pk)]
#dataDiD %>%
#  filter(quarterID>11)%>%
#  mutate(treated=ifelse(quarterID>14,1,treat))%>%
#  mutate(treated=treat*time)%>%
#  dplyr::select(hf_pk,quarterID,consumption,treated)->ImputeLRData

#ImputeLRData%>%
#  mutate(c=1)%>%
#  group_by(hf_pk)%>%
#  mutate(Sumc=sum(c))%>%
#  dplyr::select(hf_pk,Sumc)%>%
#  unique()%>%
#  filter(Sumc<5)-> check
#nobal <- check$hf_pk 

#dataDiD%>%
#  filter(quarterID>11)%>%
#  mutate(treated=ifelse(quarterID>14,1,treat))%>%
#  mutate(treated=treat*time)%>%
#  dplyr::select(hf_pk,quarterID,consumption,treated)%>%
#  dplyr::filter(!hf_pk%in%nobal)->ImputeLRData2

#ImputeLRData2 <- fread("/Users/angelchung/LRImputedData0.01.csv")

# Use imputed data to run main analysis (Run the imputation above first. Note that it requires hyperparameter tuning to replicate exact same results.)
ImputeLRData2 <- fread("~/data/LRImputedData0.01.csv")
setup = panel.matrices(ImputeLRData2)
sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #0.037

se = sqrt(vcov(sdid, method='jackknife'))
se #0.01400726
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (0.01, 0.06)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]] 

0.037/0.2465343 #0.1500805 --> 15%


#### Underserved ####
data <- fread("~/data/S2_Dhis2Data.csv")
data <- as.data.frame(data)

# facility data 
facility <- fread("~/data/S1_master_facility_update_11.csv")

# Processing
data%>%
  group_by(name1,productID)%>%
  mutate(normAvg=mean(consumption,na.rm=TRUE))%>%
  mutate(normStd=sd(consumption,na.rm=TRUE))%>%
  filter(!is.na(consumption))->data
data <- as.data.table(data)
alldata<- data[,.(stockout=sum(stockout,na.rm=TRUE),consumption=sum(consumption,na.rm=TRUE)),by=.(quarter,hf_pk,name1,productID,normAvg,normStd)]
alldata$normConsump <- (alldata$consumption-alldata$normAvg)/alldata$normStd

alldata%>%
  left_join(facility)%>%
  filter(stockout>=1)%>% #underserved
  mutate(treat=ifelse(district%in%c("Kono","Karene","Tonkolili","Pujehun","Falaba"),1,0))%>%
  mutate(time=ifelse(quarter>="2023Q2",1,0))%>%
  mutate(type = as.numeric(factor(facility_type)))%>%
  mutate(productIDFac=paste0(productID,"-",hf_pk))%>%
  mutate(quarterID = as.numeric(factor(quarter)))-> alldata2

alldataDiD <- alldata2[,.(stockout=sum(stockout,na.rm=TRUE),consumption=mean(normConsump,na.rm=TRUE)),by=.(quarter,quarterID,type,time,treat,hf_pk,district)]

alldataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated,district,treat,type)-> mainData

# Only keep balanced data
mainData%>%
  mutate(c=1)%>%
  group_by(hf_pk)%>%
  mutate(Sumc=sum(c))%>%
  dplyr::select(hf_pk,Sumc)%>%
  unique()%>%
  filter(Sumc<5)-> check
nobal <- check$hf_pk 

alldataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarterID,consumption,treated,district,treat,type)%>%
  dplyr::filter(!hf_pk%in%nobal)-> mainData2

fwrite(mainData2, file="~/data/mainData.csv") #use this data to run event study in STATA

setup = panel.matrices(mainData2)
sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sdid #underserved: 0.182

se = sqrt(vcov(sdid, method='jackknife'))
se #underserved:  0.06878537
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #underserved: 95% CI (0.05, 0.32)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.182/0.5604557 #0.3247357 -> 32% underserved p = 0.0071


