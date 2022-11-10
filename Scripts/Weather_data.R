Stations_date <- read.xlsx("Data/Weather_data/Weather_station.xlsx") %>% 
  mutate(Start_Date = lubridate::as_date(Thinning, origin = "1899-12-30"),
         End_date = lubridate::as_date(Disc_coll, origin = "1899-12-30")) %>% 
  mutate(Year = year(Start_Date))

Read_in_csv_smhi <- function(Data_in, Data_type = "Air", Site = "" ) {
  read_delim(Data_in,
             delim = ";", skip = 11, col_select = c(1:3),
             col_names = c("Date", "Time", "Value")) %>% 
    mutate(Data_type = Data_type,
           Site = Site)
}

Hästveda_air_temp <- Read_in_csv_smhi("Data/Weather_data/Hästveda/smhi-opendata_1_63160_20221025_092802.csv",
                                      Data_type = "Air_temp", Site = "Hästveda") 
Hästveda_rain <- Read_in_csv_smhi("Data/Weather_data/Hästveda/smhi-opendata_7_63160_20221025_092813.csv",
                                  Data_type = "Precipitation", Site = "Hästveda")
Hästveda_rel <- Read_in_csv_smhi("Data/Weather_data/Hästveda/smhi-opendata_6_63160_20221025_092823.csv",
                                 Data_type = "Rel_hum", Site = "Hästveda")
Hagshult_air_temp <- Read_in_csv_smhi("Data/Weather_data/Hagshult/smhi-opendata_1_74180_20221025_093423.csv",
                                      Data_type = "Air_temp", Site = "Hagshult")
Hagshult_rain <- Read_in_csv_smhi("Data/Weather_data/Hagshult/smhi-opendata_7_74180_20221025_093435.csv",
                                  Data_type = "Precipitation", Site = "Hagshult")
Hagshult_rel <- Read_in_csv_smhi("Data/Weather_data/Hagshult/smhi-opendata_6_74180_20221025_093352.csv",
                                 Data_type = "Rel_hum", Site = "Hagshult")
Prec_data <- bind_rows(Hästveda_rain,  Hagshult_rain) %>% 
  group_by(Date, Data_type, Site) %>% 
  summarise(Value = sum(Value)) %>% ungroup() %>% 
  mutate(Year = year(Date),
          Month = month(Date), Day = day(Date)) %>% filter(Year %in% c(2014:2020))

Temp_rel_data <- bind_rows(Hästveda_air_temp, Hästveda_rel,
                           Hagshult_air_temp, Hagshult_rel) %>% 
  group_by(Date, Data_type, Site) %>% 
  summarise(Value = mean(Value)) %>% ungroup() %>% 
  mutate(Year = year(Date),
         Month = month(Date), Day = day(Date)) %>% filter(Year %in% c(2014:2020))

All_weather_data <- bind_rows(Prec_data, Temp_rel_data)

Dates <- Stations_date %>% group_by(Year) %>% 
  summarise(Min_date = min(Start_Date), Max_date = max(End_date))

Weather_mean_2017 <- All_weather_data %>% 
  mutate(Date = lubridate::ymd(paste(2017,Month, Day, sep ="-"))) %>% 
  filter(!is.na(Date)) %>% 
  filter(Year != 2017) %>% mutate(Year =2017) %>% 
  filter(Date >= Dates$Min_date[1] & Date <= Dates$Max_date[1]) %>% 
  mutate(Data = "2014-2020")

All_weather_data_2017 <- All_weather_data %>% 
  filter(Date >= Dates$Min_date[1] & Date <= Dates$Max_date[1]) %>% 
  mutate(Data = "2017") 

All_weather_data_2017_mean <- bind_rows(Weather_mean_2017, All_weather_data_2017)

Weather_mean_2018 <- All_weather_data %>% 
  mutate(Date = lubridate::ymd(paste(2018,Month, Day, sep ="-"))) %>% 
  filter(!is.na(Date)) %>% 
  filter(Year != 2018)  %>% mutate(Year =2018) %>% 
  filter(Date >= Dates$Min_date[2] & Date <= Dates$Max_date[2])  %>% 
  mutate(Data = "2014-2020")

All_weather_data_2018 <- All_weather_data %>% filter(Date >= Dates$Min_date[2] & Date <= Dates$Max_date[2]) %>% 
  mutate(Data = "2018") 

All_weather_data_2018_mean <- bind_rows(Weather_mean_2018, All_weather_data_2018)

Plot_rain_2017 <- All_weather_data_2017_mean %>% 
  filter(Data_type == "Precipitation") %>% 
  ggplot(aes(Date, Value, color = Data, linetype = Site)) +
  geom_smooth(se = F) +
#  geom_smooth(data = Weather_mean_2017, aes(Date2, Value), color = "red") +
  #geom_point() +
  #facet_wrap(Site~., scales = "free") +
  scale_y_continuous(limit=c(0,10),oob=scales::squish) +
  scale_color_manual("", values = Color_scale) +
  ggthemes::theme_tufte() +
  ylab("Precipitation mm²/day") +
  theme(axis.title.x = element_blank()) +
  scale_x_date(date_breaks = "months", date_labels = "%b")
Plot_temp_2017 <- All_weather_data_2017_mean %>% 
  filter(Data_type == "Air_temp") %>% 
  ggplot(aes(Date, Value, color = Data, linetype = Site)) +
  geom_smooth(se = F) +
  #  geom_smooth(data = Weather_mean_2017, aes(Date2, Value), color = "red") +
  #geom_point() +
  #facet_wrap(Site~., scales = "free") +
  scale_y_continuous(limit=c(0,25),oob=scales::squish) +
  scale_color_manual("", values = Color_scale) +
  ggthemes::theme_tufte() +
  ylab("Average temperature °C/day") +
  theme(axis.title.x = element_blank()) +
  scale_x_date(date_breaks = "months", date_labels = "%b")
Plot_rel_hum_2017 <- All_weather_data_2017_mean %>% 
  filter(Data_type == "Rel_hum") %>% 
  ggplot(aes(Date, Value, color = Data, linetype = Site)) +
  geom_smooth(se = F) +
  #  geom_smooth(data = Weather_mean_2017, aes(Date2, Value), color = "red") +
  #geom_point() +
  #facet_wrap(Site~., scales = "free") +
  scale_y_continuous(limit=c(50,100)) +
  scale_color_manual("", values = Color_scale) +
  ggthemes::theme_tufte() +
  ylab("Relative humidity %") +
  theme(axis.title.x = element_blank()) +
  scale_x_date(date_breaks = "months", date_labels = "%b")

Plot_weather_2017 <- egg::ggarrange(Plot_temp_2017 + guides(linetype = "none", color = "none"), 
               Plot_rain_2017 + guides(linetype = "none", color = "none"), 
               Plot_rel_hum_2017 + guides(linetype = guide_legend(order = 1, 
                                                                  override.aes = list(color = "black"))), nrow = 1)

Plot_rain_2018 <- All_weather_data_2018_mean %>% 
  filter(Data_type == "Precipitation") %>% 
  ggplot(aes(Date, Value, color = Data, linetype = Site)) +
  geom_smooth(se = F) +
  #  geom_smooth(data = Weather_mean_2017, aes(Date2, Value), color = "red") +
  #geom_point() +
  #facet_wrap(Site~., scales = "free") +
  scale_y_continuous(limit=c(0,10),oob=scales::squish) +
  scale_color_manual("", values = c(Color_scale[1], Color_scale[3])) +
  ggthemes::theme_tufte() +
  ylab("Precipitation mm²/day") +
  theme(axis.title.x = element_blank())
Plot_temp_2018 <- All_weather_data_2018_mean %>% 
  filter(Data_type == "Air_temp") %>% 
  ggplot(aes(Date, Value, color = Data, linetype = Site)) +
  geom_smooth(se = F) +
  #  geom_smooth(data = Weather_mean_2017, aes(Date2, Value), color = "red") +
  #geom_point() +
  #facet_wrap(Site~., scales = "free") +
  scale_y_continuous(limit=c(0,25),oob=scales::squish) +
  scale_color_manual("", values = c(Color_scale[1], Color_scale[3])) +
  ggthemes::theme_tufte() +
  ylab("Average temperature °C/day") +
  theme(axis.title.x = element_blank())
Plot_rel_hum_2018 <- All_weather_data_2018_mean %>% 
  filter(Data_type == "Rel_hum") %>% 
  ggplot(aes(Date, Value, color = Data, linetype = Site)) +
  geom_smooth(se = F,method = "loess") +
  #  geom_smooth(data = Weather_mean_2017, aes(Date2, Value), color = "red") +
  #geom_point() +
  #facet_wrap(Site~., scales = "free") +
  scale_y_continuous(limit=c(50,100)) +
  scale_color_manual("", values = c(Color_scale[1], Color_scale[3])) +
  ggthemes::theme_tufte() +
  ylab("Relative humidity %") +
  theme(axis.title.x = element_blank()) 

Plot_weather_2018 <- egg::ggarrange(Plot_temp_2018 + guides(linetype = "none", color = "none"), 
               Plot_rain_2018 + guides(linetype = "none", color = "none"), 
               Plot_rel_hum_2018 + guides(linetype = guide_legend(order = 1, 
                                                                  override.aes = list(color = "black"))), 
               nrow = 1)

Plot_both_years <- gridExtra::grid.arrange(Plot_weather_2017, 
               Plot_weather_2018)

