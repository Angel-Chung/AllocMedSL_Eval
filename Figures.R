library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(plotly)
library(tidyverse)
library(synthdid)



# Input data
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
  #filter(stockout>=1)%>% #underserved
  mutate(treat=ifelse(district%in%c("Kono","Karene","Tonkolili","Pujehun","Falaba"),1,0))%>%
  mutate(time=ifelse(quarter>="2023Q2",1,0))%>%
  mutate(type = as.numeric(factor(facility_type)))%>%
  mutate(productIDFac=paste0(productID,"-",hf_pk))%>%
  mutate(quarterID = as.numeric(factor(quarter)))-> alldata2


#### Figure 2: Treatment Map ####
geo <- alldata2
geo %>%
  select(lat,long,district,facility_type,treat)%>%
  unique() -> geo
mapboxToken <- 'Reaplce with your own token'  #you can obtain it on mapbox website
Sys.setenv("MAPBOX_TOKEN" = mapboxToken)
fig1 <- geo %>%plot_mapbox(lat = ~lat, lon = ~long,size=0.5,color = ~treat,mode = 'scattermapbox') 
fig1 <- fig1 %>% layout(title = 'SL',
                        font = list(color='black'),
                        plot_bgcolor = 'white', paper_bgcolor = '#F0F0EF',
                        mapbox = list(style = 'light',zoom =3,center = list(lon = 8.7, lat = -11.9)),
                        legend = list(x = 0.1, y = 0.9, orientation = 'v', 
                                      font = list(size = 12),
                                      margin = list(l = 25, r = 25,
                                                    b = 25, t = 25,
                                                    pad = 2)))
fig1 <- fig1 %>% config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))
fig1



#### Figure 3: Main Result Plot ####

alldataDiD <- alldata2[,.(stockout=sum(stockout,na.rm=TRUE),consumption=mean(normConsump,na.rm=TRUE)),by=.(quarter,quarterID,type,time,treat,hf_pk,district)]

#SynthDiD Main Analysis
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
sdid #0.116, underserved: 0.182

se = sqrt(vcov(sdid, method='jackknife'))
se #0.04560403, underserved:  0.06878537
CI <- sprintf('95%% CI (%1.2f, %1.2f)', sdid - 1.96 * se, sdid + 1.96 * se)
CI #95% CI (0.03, 0.21), underserved: 95% CI (0.05, 0.32)

a <- synthdid_plot(sdid)
built_plot <- ggplot_build(a)
plot_data <- built_plot$data[[2]]

0.116/0.607 #0.1911038 --> 19%
0.182/0.5604557 #0.3247357 -> 32% underserved p = 0.0071

# Main result plot
Q <- c("2022Q3","2022Q4","2023Q1","2023Q2","2023Q3")
mainplot <- synthdid_plot(sdid, 
                          treated.name = "Treated",
                          control.name = "Control",
                          se.method = 'jackknife', 
                          lambda.plot.scale = 0,      
                          diagram.alpha = 0,         
                          trajectory.alpha = 1,   
                          effect.alpha = 0,      
                          ci.alpha = 0,             
                          onset.alpha = 0,point.size = 0,effect.curvature =0,spaghetti.line.width = 0,
                          spaghetti.label.size = 0,
                          spaghetti.line.alpha = 0,
                          spaghetti.label.alpha = 0,alpha.multiplier = NULL)+ 
  labs(x = '', y = 'Consumption',color='black')+scale_x_continuous(breaks=c(12,13,14,15,16),labels= Q)+
  geom_vline(xintercept = 14, linetype = "dashed", color = "black", size = 0.5) +
  scale_y_continuous(limits = c(0.35, NA))+
  theme(
    axis.text.x = element_text(size = 14,color='black'),  
    axis.text.y = element_text(size = 14,color='black'), 
    axis.title.y   = element_text(size = 14, color = 'black'),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.8),
    legend.position = c(0.01, 0.99), 
    legend.justification = c(0, 1),  
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 14),
    legend.direction = "vertical"
  ) +scale_alpha(range=c(0,1), guide='none')

mainplot


#### Figure 3: Raw Data Trend Plot ####
alldataDiD%>%
  filter(quarterID>11)%>%
  mutate(treated=ifelse(quarterID>14,1,treat))%>%
  mutate(treated=treat*time)%>%
  dplyr::select(hf_pk,quarter,quarterID,consumption,treated,district,treat)%>%
  dplyr::filter(!hf_pk%in%nobal)%>%
  group_by(quarterID, quarter, treat) %>%                       
  summarise(avg_consumption = mean(consumption, na.rm = TRUE), .groups = "drop") %>%
  
  ggplot(aes(x = quarter,
             y = avg_consumption,
             color = factor(treat),
             group = treat)) +
  geom_line(size = 0.5) +
  geom_point(size = 2) +
  scale_color_manual(values = c("0" = "#1b9e77", "1" = "#d95f02"),
                     labels = c("Control", "Treated")) +
  labs(x = "Quarter",
       y = "Average Consumption",
       color = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> a

Q <- c("2022Q3","2022Q4","2023Q1","2023Q2","2023Q3")

rawplot <- alldataDiD %>%
  filter(quarterID > 11) %>%
  dplyr::filter(!hf_pk %in% nobal) %>%
  group_by(quarterID, treat) %>%   
  summarise(avg_consumption = mean(consumption, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = quarterID,
             y = avg_consumption,
             color = factor(treat),
             group = treat)) +
  geom_line(size = 0.5) +
  geom_vline(xintercept = 14, linetype = "dashed", color = "black", size = 0.5)+
  scale_x_continuous(breaks = c(12,13,14,15,16),
                     labels = Q) +
  scale_y_continuous(limits = c(0.35, NA))+
  scale_color_manual(values = c("0" = "#F8766D", "1" = "#00BFC4"),
                     labels = c("Control", "Treated")) +
  labs(x = '',
       y = 'Consumption',
       color = 'black') +
  theme_minimal() +  
  theme(
    axis.text.x  = element_text(size = 14, color = 'black'),
    axis.text.y  = element_text(size = 14, color = 'black'),
    axis.title.y = element_text(size = 14, color = 'black'),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.8),
    legend.position       = c(0.01, 0.99),
    legend.justification  = c(0, 1),
    legend.title          = element_blank(),
    legend.key            = element_blank(),
    legend.text           = element_text(size = 14)
  )

rawplot



