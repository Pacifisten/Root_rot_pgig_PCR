Site_values <- Calculated_data %>%  
  group_by(Site) %>% 
  summarise(Sum = n(),
            Mean_area  = mean(Area),
            SE_Area = sd(Area)/sqrt(n()),
            Mean_inf_area = mean(Relative_inf_area[No_colonies > 0]*100),
            SE_inf_area = sd(Relative_inf_area[No_colonies > 0]*100)/sqrt(sum(No_colonies > 0)),
            Mean_no_col = mean(Colonies_per_area[No_colonies > 0]*100),
            SE_no_col = sd(Colonies_per_area[No_colonies > 0]*100)/sqrt(sum(No_colonies > 0)),
            Infected = sum(Colonies_per_area > 0), 
            Mean_cov = mean(Coverage),
            SE_cov = sd(Coverage)/sqrt(n()),
            Sum_zero = sd(Colonies_per_area),
            Perc_infected = (Sum-Sum_zero)/Sum) %>% ungroup() %>% 
  mutate(Total = sum(Sum), Of_total = Sum/Total) %>% 
  left_join(PCR_and_Over85 %>%  mutate(Site = as.numeric(Site)) %>% 
              group_by(Site, Number_PCR_positive, Number_PCR_total) %>% 
              summarise())

Table <- Site_values %>% 
  group_by() %>% 
  summarise(Site, Sum, 
            Stump_area = paste0(round(Mean_area,0), "cm² ± ", round(SE_Area,0), "cm²"), 
            Percent_infected = paste0(round((Infected/Sum),2)*100, "%"),
            Mean_cov = paste0(round(Mean_cov,2), "% ± ", round(SE_cov,2), "%"),
            Mean_inf_area = paste0(round(Mean_inf_area,2), "% ± ", round(SE_inf_area,2), "%"),
            Mean_no_col_dm2 = paste0(round(Mean_no_col,2), " ± ", round(SE_no_col,2)),
            PCR_positive = paste0(as.character(round((Number_PCR_positive/Number_PCR_total),2)*100),"%"))

