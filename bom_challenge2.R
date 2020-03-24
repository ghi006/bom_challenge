library(tidyverse)

Bom_data <- read_csv("data/Bom_data.csv")
view(Bom_data)

Bom_stations <- read.csv("data/Bom_stations.csv")
view(Bom_stations)

#Challenge 1:
Bom_data %>%
  separate(Temp_min_max,into=c("min_temp","max_temp"),sep = "/") %>% 
  filter(min_temp != "-", max_temp != "-", Rainfall != "-") %>%
  group_by(Station_number) %>%  
  summarise()
# Answer:20 stations have those data recorded.

#Challenge 2:
lowest_ave_temp <- Bom_data %>% 
  separate(Temp_min_max,into=c("min_temp","max_temp"),sep="/")%>%
  filter(min_temp!="-",max_temp!="-")%>%
  mutate(min_temp=as.numeric(min_temp))%>%
  mutate(max_temp=as.numeric(max_temp)) %>%
  mutate(temp_diff=max_temp-min_temp) %>% 
  group_by(Month) %>% 
  summarise(average=mean(temp_diff)) %>% 
  arrange(average) %>% 
  slice(1)

view(Bom_stations)

write_csv(lowest_ave_temp,"data/bom_data_diff.csv")


#Challenge 3
Tidy_bom_station <- Bom_stations %>% 
  gather(key = Station_number, value = amount,-info) %>% 
  spread(key = info, value = amount) %>%
  mutate(Station_number=str_replace_all(string = Station_number,
                                        pattern = "X",
                                        replacement = "")) %>% 
  mutate(Station_number=as.numeric(Station_number))

state_ave_temp <- Bom_data %>% 
  separate(Temp_min_max,into=c("min_temp","max_temp"),sep = "/") %>%  
  mutate(min_temp=as.numeric(min_temp))%>%
  mutate(max_temp=as.numeric(max_temp)) %>%
  mutate(temp_diff=max_temp-min_temp)

Station_df <- Bom_stations %>% 
  gather(key = Station_number, value = amount,-info) %>%
  spread(key = info, value = amount)

stations_meteo_merge <- full_join(state_ave_temp,Tidy_bom_station, by=c("Station_number"="Station_number"))  

#Which state saw the lowest average daily temperature difference?  
answer3 <- stations_meteo_merge %>% 
  mutate(min_temp=as.numeric(min_temp))%>%
  mutate(max_temp=as.numeric(max_temp)) %>%
  #mutate(temp_diff=max_temp-min_temp) %>% 
  group_by(state) %>% 
  summarise(average=mean(temp_diff,na.rm = TRUE)) %>% 
  arrange(average) %>% 
  slice(1)

#Does the westmost (lowest longitude) or eastmost (highest longitude)
#weather station in our dataset have a higher average solar exposure?
Question4 <- stations_meteo_merge %>%
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>%
  mutate(lon=as.numeric(lon)) %>%
  group_by(Station_number,lon) %>% 
  summarise(average = mean(Solar_exposure, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(lon==min(lon)|lon==max(lon))










