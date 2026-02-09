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


#### SynthDiD Main Analysis ####
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
sdid #0.116

se = sqrt(vcov(sdid, method='jackknife'))
se #0.04560403
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (0.03, 0.21)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.116/0.607 #0.1911038 --> 19%


#### Get MainData to run event study in STATA ####
# Run main analysis code above first!

fwrite(mainData2, file="~/data/mainData.csv")


#### Get IV dataset ####
data <- fread("~/data/S2_Dhis2Data.csv")
data <- as.data.frame(data)

# facility data 
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
  filter(productID%in%c(38,16,5,28,6,33,47,23,1,24,19,34,44,56,51,9,42,37,21,39,46,50,52,18,7,22,2,3,4,20,43,32,29,17,54,15,35))%>%
  mutate(treat=ifelse(district%in%c("Tonkolili","Falaba","Karene","Pujehun","Kono"),1,0))%>%
  mutate(complier=ifelse(district%in%c("Karene","Tonkolili","Falaba"),1,0))%>%
  mutate(time=ifelse(quarter>="2023Q2",1,0))%>%
  mutate(z=treat*time)%>%
  mutate(type = as.numeric(factor(facility_type)))%>%
  mutate(quarterID = as.numeric(factor(quarter)))%>%
  filter(quarterID>11)%>%
  select(productID,district,treat,time,z,complier,facility_type,consumption,quarterID,normConsump,hf_pk)-> dataIV

dataIV <- as.data.table(dataIV)
dataIVfinal <- dataIV[,.(normConsump=mean(normConsump,na.rm=TRUE)),by=.(quarterID,time,treat,hf_pk,z,complier,facility_type,district)]

dataIVfinal%>%
  mutate(c=1)%>%
  group_by(hf_pk)%>%
  mutate(Sumc=sum(c))%>%
  dplyr::select(hf_pk,Sumc)%>%
  unique()%>%
  filter(Sumc<5)-> check
nobal <- check$hf_pk

dataIVfinal%>%
  dplyr::filter(!hf_pk%in%nobal)-> dataIVfinal2


dataIVfinal%>%
  mutate(c=1)%>%
  group_by(hf_pk)%>%
  mutate(Sumc=sum(c))%>%
  dplyr::select(hf_pk,Sumc)%>%
  unique()%>%
  filter(Sumc<5)-> check
nobal <- check$hf_pk

dataIVfinal%>%
  dplyr::filter(!hf_pk%in%nobal)-> dataIVfinal2

fwrite(dataIVfinal2,file="~/data/IVData.csv")



