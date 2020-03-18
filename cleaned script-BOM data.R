library(tidyverse)

Bom_data <- read_csv("data/Bom_data.csv")
view(Bom_data)

Bom_stations <- read.csv("data/Bom_stations.csv")
view(Bom_stations)

#Challenge 1:
Bom_data %>%
  separate(Temp_min_max,into=c("min_temp","max_temp"),sep = "/") %>% 
  filter(min_temp != "-", max_temp != "-", Rainfall >= 0) %>%
 group_by(Station_number) %>%  
 summarise(num_row=n())

#Challenge 2:
lowest_ave_temp <- Bom_data %>% 
separate(Temp_min_max,into=c("min_temp","max_temp"),sep="/")%>%
filter(min_temp>=0,max_temp>=0)%>%
mutate(min_temp=as.numeric(min_temp))%>%
mutate(max_temp=as.numeric(max_temp)) %>%
mutate(temp_diff=max_temp-min_temp) %>% 
group_by(Month) %>% 
summarise(average=mean(temp_diff)) 

lowest_ave_temp <- Bom_data %>% 
  separate(Temp_min_max,into=c("min_temp","max_temp"),sep="/")%>%
  filter(min_temp>=0,max_temp>=0)%>%
  mutate(min_temp=as.numeric(min_temp))%>%
  mutate(max_temp=as.numeric(max_temp)) %>%
  mutate(temp_diff=max_temp-min_temp)%>%
  group_by(Month) %>% 
  summarise(average=mean(temp_diff)) 

view(Bom_stations)

write_csv(lowest_ave_temp,"data/bom_data_diff.csv")

Bom_stations
#Challenge 3
Tidy_bom_station <- Bom_stations %>% 
  gather(key = Station_number, value = amount,-info) %>% 
  spread(key = info, value = amount) %>%
  mutate(Station_no=as.numeric(Station_number)) %>% 
  select(Station_no,state,start,end,elev,lat,lon,name) %>%
  group_by(state) %>% 
  write_csv("data/Tidy_bom_station.csv")

Station_df <- Bom_stations %>% 
  gather(key = Station_number, value = amount,-info) %>% 
  spread(key = info, value = amount) %>%
  mutate(Station_no=as.numeric(Station_number))


stations_meteo_merge <- full_join(Bom_data,Tidy_bom_station, by=c("Station_number"="Station_no")) %>% 
  write_csv("data/Bom_stations_merge.csv")

