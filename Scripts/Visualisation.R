# Calculate relative infected area and colonies per square cm and cleans dataset
Calculated_data <- All_data %>% filter(Coverage != "nd") %>% 
  mutate(Relative_inf_area = Infection_area/Area,
         Colonies_per_area = No_colonies/Area) %>% 
  mutate(Coverage = as.numeric(Coverage)) %>% 
  mutate(Group = if_else(Coverage < 85, "Under recommendation (85%)", if_else(Coverage == 100, "Full coverage", "Over 85% coverage"))) %>% 
  filter(!is.na(Colonies_per_area)) %>% 
  mutate(Group = as.factor(Group)) %>% 
  mutate(PCR_result = as.factor(if_else(is.na(PCR_result), "Not tested", PCR_result)),
         Coverage_int = as.integer(Coverage)) 

# Mean values

# Calculates percentage PCR positive for each stand
PCR_stand <- PCR_data %>%   group_by(Site) %>% 
  summarise(Positive_PCR_test = sum(PCR_result == "Positive")/n()*100,
            Number_PCR_positive = sum(PCR_result == "Positive"), 
            Number_PCR_total = n())

# Calculates averages for each stand
Stand_data <- Calculated_data %>% 
  group_by(Site) %>% 
  summarise(Number_trees = n(), 
            Infected_perc = sum(Colonies_per_area != 0)/n()*100, # Calculates percentage infected
            Mean_stump_size = mean(Area), # Calculates mean area of the stump
            Mean_cov = mean(Coverage), # Calculates mean coverage of P. gigantea
            Over_85_coverage = (sum(Coverage > 85)/Number_trees)*100, # Calculates number of trees that had a coverage above 85% 
            Full_coverage = (sum(Coverage == 100)/Number_trees)*100,  # Calculates number of trees that are fully covered
            Mean_no_col = mean(No_colonies[No_colonies > 0]), # Calculates mean number of colonies if the stump is infected
            Mean_rel_no_col_dm = mean(Colonies_per_area[No_colonies > 0])*100, # Calculates relative number of colonies per stump if the stump is infected
            Mean_inf_area = mean(Infection_area[No_colonies > 0]), # Calculates mean infection area if the stump is infected
            Mean_rel_inf_area = mean(Relative_inf_area[No_colonies > 0])*100, # Calculate mean relative infected area if the stump is infected
            SE_stump_size = sd(x = Area)/sqrt(n())
            )

# Plots all the data for visual look
Stand_data %>% bind_rows(PCR_stand) %>% 
  pivot_longer(cols = Number_trees:Positive_PCR_test, names_to = "Variable", values_to = "Values") %>% # Makes the data long
  ggplot(aes(Site, Values)) +
    geom_col() + # Plot bar chart
  facet_wrap(.~Variable, scales = "free") # Plots all variables 

#  Joins average stand data with PCR result for each stand
PCR_and_Over85 <- Stand_data %>% left_join(PCR_stand) %>% # Joins the PCR result for each result
  pivot_longer(cols = Number_trees:Positive_PCR_test, names_to = "Variable", values_to = "Values") %>% # Makes the data long
  filter(Variable %in% c("Positive_PCR_test", "Full_coverage", "Over_85_coverage")) %>% # Subsets the variables we are interested in
  mutate(Site = as.factor(Site), Variable2 = if_else(Variable == "Positive_PCR_test", "PCR", "Coverage")) # Makes a second group for coverage to have less bars in the plot

# Bar plot of coverage and positive PCR values
Barplot_coverage_pos_PCR <- PCR_and_Over85 %>%
  mutate(Variable = fct_rev(Variable)) %>%
  ggplot(aes(Site, Values, group = Variable2, fill = Variable)) +
  geom_col(position = "dodge", color = "Black", width = .7) +
  #geom_hline(yintercept = 90, linetype = 2) +
  ggthemes::theme_tufte() +
  scale_fill_manual("", values = Color_scale,
                    labels = c("Positive PCR test", "Over 85% coverage", "Full coverage" )
                    ) +
  ylab("Percentage of stumps (%)") +
  theme(legend.position = "top") +
  scale_y_continuous(breaks = c(0,25,50,75,100)) +
  guides(fill = guide_legend(reverse = T))


# Calculates infection variables
Data_values <- Calculated_data %>%  
  group_by(Group) %>% 
  summarise(Sum = n(), 
            Mean_inf_area = mean(Relative_inf_area[No_colonies > 0]*100),
            SE_inf_area = sd(Relative_inf_area[No_colonies > 0]*100)/sqrt(sum(No_colonies > 0)),
            Mean_no_col = mean(Colonies_per_area[No_colonies > 0]*100),
            SE_no_col = sd(Colonies_per_area[No_colonies > 0]*100)/sqrt(sum(No_colonies > 0)),
            Infected = sum(Colonies_per_area > 0), 
            Mean_cov = mean(Coverage),
            Sum_zero = sum(Colonies_per_area == 0),
            Perc_infected = (Sum-Sum_zero)/Sum) %>% ungroup() %>% 
  mutate(Total = sum(Sum), Of_total = Sum/Total) %>% 
  mutate(Group = fct_rev(Group)) 

# Linear mixed model for relative infected area
Model <- Calculated_data %>% filter(Colonies_per_area != 0) %>% 
  lme4::lmer(data = ., formula = Relative_inf_area~Group+(1|Site))

Result_model <- Model %>% emmeans::emmeans("Group") %>% pairs() %>% summary() 
Result_inf_area <- Result_model %>% as_tibble() %>% 
  mutate(To_group = as.factor(str_extract(contrast, "[:print:]+(?=[:space:]-)")),
         Group = as.factor(str_extract(contrast, "(?<=-[:space:])[:print:]+"))) %>% 
  mutate(p.value = round(p.value, 3)) %>% 
  mutate(Value = if_else(To_group == "Over 85% coverage", .25, .5))
Values <- Data_values %>% mutate(Number_group = as.numeric(Group)) %>% 
  dplyr::select(Group, Number_group)
Values2 <- Values %>% rename(To_group = Group, Number_to_group = Number_group) 
Result_inf_area <- Result_inf_area %>% left_join(Values) %>% left_join(Values2) %>% 
  left_join(Data_values)

# Linear mixed model on colonies per area
Model2 <- Calculated_data %>% filter(Colonies_per_area != 0) %>% 
  lme4::lmer(data = ., formula = Colonies_per_area~Group+(1|Site))
Model2 %>% emmeans::emmeans("Group") %>% pairs()
Result_model2 <- Model2 %>% emmeans::emmeans("Group") %>% pairs() %>% summary() 
Result_no_col <- Result_model2 %>% as_tibble() %>% 
  mutate(To_group = as.factor(str_extract(contrast, "[:print:]+(?=[:space:]-)")),
         Group = as.factor(str_extract(contrast, "(?<=-[:space:])[:print:]+"))) %>% 
  mutate(p.value = round(p.value, 3)) %>% 
  mutate(Value = if_else(To_group == "Over 85% coverage", .25, .5))
Result_no_col <- Result_no_col %>% left_join(Values) %>% left_join(Values2) %>% 
  left_join(Data_values)
# Percentage of totalt stumps infected
Barplot_inf_area <- Data_values %>% 
  #pivot_longer(values_to = "Values", names_to = "Variable",cols = Mean_inf_area:Mean_no_col) %>% 
  ggplot(aes(Group, Mean_inf_area, fill = Group)) +
  geom_col(color = "black") +
  geom_linerange(data = Result_inf_area, aes(xmin = Group, xmax = To_group, y = Mean_inf_area + SE_inf_area + Value*Mean_inf_area), inherit.aes = F) +
  geom_linerange(data = Result_inf_area, aes(x = Group, ymin = Mean_inf_area + SE_inf_area + Value*Mean_inf_area - .1, ymax = Mean_inf_area + SE_inf_area + Value*Mean_inf_area), inherit.aes = F) +
  geom_linerange(data = Result_inf_area, aes(x = To_group, ymin = Mean_inf_area + SE_inf_area + Value*Mean_inf_area - .1, ymax = Mean_inf_area + SE_inf_area + Value*Mean_inf_area), inherit.aes = F) +
  geom_text(data = Result_inf_area, aes(x = (Number_group+Number_to_group)/2, y = Mean_inf_area + SE_inf_area + Value*Mean_inf_area + .1, label = p.value), family = "serif") +
  geom_errorbar(aes(ymin = Mean_inf_area-SE_inf_area, ymax = Mean_inf_area+SE_inf_area), width = .15) +
  ggtitle("B) Relative infected area (%)") +
  ggthemes::theme_tufte() +
  theme(axis.title = element_blank(),legend.position = "top", axis.ticks.x = element_blank()) +
  scale_fill_brewer("", type = "qual", palette = 6) +
  guides(fill = "none")
Barplot_inf_area
# barplot of number of colonies
Barplot_no_colonies <- Data_values %>% 
  #pivot_longer(values_to = "Values", names_to = "Variable",cols = Mean_inf_area:Mean_no_col) %>% 
  ggplot(aes(Group, Mean_no_col, group = Group, fill = Group)) +
  geom_col(color = "black") +
  geom_linerange(data = Result_no_col, aes(xmin = Group, xmax = To_group, y = Mean_no_col + SE_no_col + Value*Mean_no_col), inherit.aes = F) +
  geom_linerange(data = Result_no_col, aes(x = Group, ymin = Mean_no_col + SE_no_col + Value*Mean_no_col - .175, ymax = Mean_no_col + SE_no_col + Value*Mean_no_col), inherit.aes = F) +
  geom_linerange(data = Result_no_col, aes(x = To_group, ymin = Mean_no_col + SE_no_col + Value*Mean_no_col - .175, ymax = Mean_no_col + SE_no_col + Value*Mean_no_col), inherit.aes = F) +
  geom_text(data = Result_no_col, aes(x = (Number_group+Number_to_group)/2, y = Mean_no_col + SE_no_col + Value*Mean_no_col + .175, label = p.value), family = "serif") +
  geom_errorbar(aes(ymin = Mean_no_col-SE_no_col, ymax = Mean_no_col+SE_no_col), width = .15) +
    ggtitle("A) Number of colonies per dm²") +
  ggthemes::theme_tufte() +
  theme(axis.title= element_blank(),legend.position = "top", axis.ticks.x = element_blank()) +
  scale_fill_brewer("", type = "qual", palette = 6) +
  guides(fill = "none")
Both_graphs <- gridExtra::grid.arrange(Barplot_no_colonies, Barplot_inf_area, ncol = 2)

# ggsave(plot = Barplot_inf_area, filename = "Output/Barplot_inf_area.jpg", height = 100, width = 160, units = "mm", dpi = 600, bg = "white")
# ggsave(plot = Barplot_no_colonies, filename = "Output/Barplot_no_col.jpg", height = 100, width = 160, units = "mm", dpi = 600, bg = "white")

# New graph
Spread_of_coverage <- Calculated_data %>% 
  mutate(Site = fct_relevel(as.factor(Site), as.character(c(1:15)))) %>% 
  ggplot(aes(Site, Coverage/100)) +
  geom_text(data = Stand_data, aes(as.factor(Site), 
                                  y = 1.07,label = paste0("n = ", Number_trees)),
           size = 2, family = "serif") +
  geom_hline(yintercept = .85, linetype = 2, color = "grey") +
  geom_jitter(width = .2, aes(color = fct_rev(Group)), size = rel(.8), alpha = .7) +
  geom_boxplot(aes(), fill = "transparent", outlier.shape = NA) + 
  ggthemes::theme_tufte() +
  scale_y_continuous(breaks = c(0,.25,.50,.75,.85,1.00), labels = scales::percent) +
  coord_trans(expand = F, ylim = c(0,1.10)) + 
  #scale_color_manual("", values = Color_scale) +
  scale_color_brewer("", type = "qual", palette = 6) +
  ylab("Stump coverage of *P. gigantea*") +
  xlab("Site") +
  theme(axis.title.y = ggtext::element_markdown()) +
  facet_grid(.~as.factor(Site), scales = "free") +
  theme(strip.text = element_blank(),
        strip.background = element_blank()) +
  theme(legend.position = "top") +
  guides(fill = guide_legend(reverse = T))

