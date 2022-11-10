# Reads in the data file with stump data
Inf_data <- read.xlsx("Data/Stump_data.xlsx")

# Reads in PCR_data
PCR_data <- read.xlsx(xlsxFile = "Data/PCR_data.xlsx") %>% 
  mutate(PCR_result = if_else(Result == "+", "Positive", "Negative")) %>% 
  filter(Stump.count != "na") %>% 
  dplyr::select(Site, Stump, PCR_result)  

# Combined_data
All_data <- Inf_data %>% left_join(PCR_data)
