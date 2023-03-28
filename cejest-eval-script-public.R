
################################
#### Analysis of CEJEST v1 ##### 
################################
## Created by Gabe Watson and Jack Ding for Environmental Policy Center CEJST evaluation 1.31.2023 ## 
## Results from this analysis are curated in this blog post: https://www.policyinnovation.org/blog/cejst-simple-map-big-implications ## 
## Contact Gabe Watson - gabe@policyinnovation.org for questions ## 

## NOTICE ## 
## This script is covered by a Creative Commons Licence and can be used for non commercial purposes only ##
## Publishing of visualizations and data generated in this script should be attributed to the Environmental Policy Innovation Center ## 

library(tidyverse)
library(aws.s3)
library(leaflet)
library(tigris)
library(ggplot2)
library(magrittr)
library(sf)
library(htmltools)
library(RColorBrewer)
library(ggformula)
library(reshape2)


######################
#### Data Import ##### 
######################
## CEJEST V1 Download - in our AWS folder ##
cejest_raw <- read_csv(get_object(object = "ej-data/cejest-v1/1.0-communities.csv", bucket = "tech-team-data"))
##########################
#### End Data Import ##### 
##########################


########################
#### Data Cleaning ##### 
########################

## This file can be used to answer multiple questions! ## 
cejest_cleaned_v1 <- cejest_raw %>%
  select(c(1,2,3,'Total threshold criteria exceeded','Total categories exceeded','Total population','Identified as disadvantaged'))%>%
  group_by(`State/Territory`)%>%
  mutate(state_pop = sum(`Total population`, na.rm = TRUE))%>%
  add_tally()%>%
  rename(state_tract_n = n)%>%
  mutate(dac = ifelse(`Identified as disadvantaged` == TRUE, 1,0))%>%
  mutate(state_tract_dac_n  = sum(dac, na.rm = TRUE))


## This file can be used to answer multiple questions! ## 
cejest_cleaned_v1 <- cejest_raw %>%
  select(c(1,2,3,'Total threshold criteria exceeded','Total categories exceeded','Total population','Identified as disadvantaged'))%>%
  group_by(`State/Territory`)%>%
  mutate(state_pop = sum(`Total population`, na.rm = TRUE))%>%
  add_tally()%>%
  rename(state_tract_n = n)%>%
  mutate(dac = ifelse(`Identified as disadvantaged` == TRUE, 1,0))%>%
  mutate(state_tract_dac_n  = sum(dac, na.rm = TRUE))

cejest_issue = cejest_raw %>% 
  select("Census tract 2010 ID", `State/Territory`, `Is low income?`,
         c(grep("Greater than or equal to the 90th percentile for",names(cejest_raw)))) 
names(cejest_issue)[1:2] = c("GEOID10","State")

cejest_issue_melt = melt(cejest_issue, id = c("GEOID10","State"))
cejest_issue_melt = cejest_issue_melt %>% mutate(variable = gsub("\\?", "", gsub("Greater than or equal to the 90th percentile for ","",  variable)))
cejest_issue_true = cejest_issue_melt %>% filter(value == TRUE) 

# filter out duplicated threshold to avoid double counting
duplicate_thresholds = c("is low income",
                         "share of properties at risk of flood in 30 years and is low income",
                         "share of properties at risk of fire in 30 years and is low income")
cejest_issue_true = cejest_issue_true %>% filter(!variable %in% duplicate_thresholds)
category = read.csv("category.csv")
cejest_issue_true = cejest_issue_true %>% left_join(category, by = "variable")
cejest_issue_true$category = factor(cejest_issue_true$category, levels = c("Climate Change",
                                                                           "Energy",
                                                                           "Health",
                                                                           "Housing",
                                                                           "Legacy Pollution",
                                                                           "Transportation",
                                                                           "Water/Wastewater",
                                                                           "Workforce Development"))
df_issues_count = cejest_issue_true %>% group_by(State) %>%count(category)


############################
#### End Data Cleaning ##### 
############################


############################
#### Anaylsis Questions ####
############################

## QUESTION 1: What % of a state’s tracts/population are disadvantaged? ## 
## Generating table
state_dac_summary <- cejest_cleaned_v1 %>%
  select(3,6,8,9,10,11)%>%
  mutate(dac_pop = ifelse(dac == 1,`Total population`,0))%>%
  mutate(state_dac_pop = sum(dac_pop, na.rm = TRUE))%>%
  select(1,3,4,6,8)%>%
  unique()%>%
  mutate(dac_pop_per = state_dac_pop / state_pop)%>%
  mutate(dac_tract_per = state_tract_dac_n / state_tract_n)%>%
  select(1,state_pop,state_dac_pop,state_tract_n,state_tract_dac_n,dac_pop_per,dac_tract_per)

## writing out as csv ## 
#write.csv(state_dac_summary,"results/state-dac-summary_v1.csv", row.names = FALSE)
## END QUESTION 1 ### 

## QUESTION 2: Count and distribution of tracts by indicator by state ## 
## Generate list of indicators
ind_list = names(cejest_raw)[grep("Greater than or equal to the 90th percentile for",names(cejest_raw))]

## Create a data frame containing counts of disadvantaged tracts
df_count_ind = data.frame(
  `State/Territory` = unique(cejest_raw$`State/Territory`)
) %>% rename("State/Territory" = "State.Territory")

tot_number_of_tracts_per_state = cejest_raw %>% group_by(`State/Territory`) %>% tally()
colnames(tot_number_of_tracts_per_state)[2] = "tot_num_tracts_per_state"

df_count_ind = df_count_ind %>% left_join(tot_number_of_tracts_per_state, by = 'State/Territory')


## Generating table
for (i in seq_along(ind_list)){
  # i = 1
  ind = ind_list[i]
  df = cejest_raw %>% filter(cejest_raw[,which(colnames(cejest_raw)==ind)] == TRUE)
  countdf = df %>% group_by(`State/Territory`) %>% tally(!!sym(ind) == TRUE)
  colnames(countdf)[2] = ind
  
  df_count_ind = df_count_ind %>% left_join(countdf, by = "State/Territory") %>% replace(is.na(.), 0)
  df_count_ind[[paste0("percent_",ind)]] = df_count_ind[,which(colnames(df_count_ind)==ind)]/df_count_ind$tot_num_tracts_per_state * 100
}

### create bar plots for percentage of disadvantaged tracts per state
pdf("results/plots/state_barplot_by_indicator.pdf", width = 8, height = 6)
for (i in seq_along(ind_list)){
  p = ggplot(data=df_count_ind, aes(x=`State/Territory`, y=!!sym(paste0("percent_",ind_list[i]))))+theme_bw() +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())+
    ylab(gsub("\\?", "", gsub("Greater than or equal to the 90th percentile for ","% DAC for ",  ind_list[i])))
  print(p)
}
dev.off()
## writing out as csv ## 
##write.csv(df_count_ind,"results/state-indicator_counts.csv", row.names = FALSE)
## END QUESTION 2 ### 


## QUESTION 3: Counts of indicators by tracts then summarize by state ##
# average number of threshold exceeded/per tract per state
df_sum_counts_per_state = cejest_raw %>%
  group_by(`State/Territory`)%>%
  tally(`Total threshold criteria exceeded`)
df_sum_counts_per_state = df_sum_counts_per_state %>% rename(thd_exceeded_per_state = n) %>% left_join(df_count_ind[,1:2], by = "State/Territory")

# average number of categories exceeded/per tract per state
temp = cejest_raw %>%
  group_by(`State/Territory`)%>%
  tally(`Total categories exceeded`) %>% rename(cat_exceeded_per_state = n)
df_sum_counts_per_state = df_sum_counts_per_state %>% left_join(temp)
n_disadvantaged_tracts = cejest_cleaned_v1 %>% select(`State/Territory`, "state_tract_dac_n") %>% unique()
df_sum_counts_per_state = df_sum_counts_per_state %>% left_join(n_disadvantaged_tracts)
df_sum_counts_per_state = df_sum_counts_per_state %>% mutate(n_thd_exceeded_per_dac = thd_exceeded_per_state/state_tract_dac_n,
                                                             n_cat_exceeded_per_dac = cat_exceeded_per_state/state_tract_dac_n)
# work on issues count
df_issues_count = df_issues_count %>% 
  left_join(unique(df_sum_counts_per_state %>% 
                     select(`State/Territory`,"state_tract_dac_n","tot_num_tracts_per_state")%>% 
                     dplyr::rename(State =`State/Territory`)))

df_issues_count = df_issues_count %>%
  mutate(cat_per_dac = n/state_tract_dac_n) %>%
  dplyr:: rename(cat_per_state = n)

# bar plot of stacked category plot
# pdf("results/plots/state_issue_barplot.pdf", width = 14, height = 6)
# ggplot(df_issues_count, aes(fill=category, x=State, y = cat_per_dac)) + 
#   geom_bar(position="stack", stat="identity") +theme_bw()+ 
#   scale_fill_manual(values = cols) + #workforce development speed yellow
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         legend.position = "bottom") 

## generating color columns
cols <- c("Climate Change"= "#009E73", 
          "Energy" = "#F98E1D", 
          "Health" = "#DC267F", 
          "Housing" = "#ffdd05", 
          "Legacy Pollution"  = "#C04000",
          "Transportation" = "#3A3B3C",
          "Water/WasteWater" = "#0F52BA",
          "Workforce Development" = "grey" )


State_issue <- ggplot(df_issues_count  %>% group_by(State) %>% top_n(1,cat_per_dac) , aes(fill=category, x=State, y = cat_per_dac)) + 
  geom_bar(position="stack", stat="identity") +theme_bw()+ 
  scale_fill_manual("Categories", values = cols) + #workforce development speed yellow
  ylab("Average Threshold Exceedence")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom"
  ) + ggtitle("Most Prominent CEJST Category by State")

plot(State_issue)
#ggsave("results/plots/state-issue-wide-v1.jpeg", plot = State_issue, width = 35, height = 15, units = c("cm"), dpi = 700)
#ggsave("results/plots/state-issue-v1.jpeg", plot = State_issue, width = 30, height = 20, units = c("cm"), dpi = 700)

State_issue_no_wf <- ggplot(df_issues_count %>% filter(!category == "Workforce Development") %>% group_by(State) %>% top_n(1,cat_per_dac) , aes(fill=category, x=State, y = cat_per_dac)) + 
  geom_bar(position="stack", stat="identity") +theme_bw()+ 
  scale_fill_manual("Categories",values = cols) + #workforce development speed yellow
  ylab("Average Threshold Exceedence")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom"
  ) + ggtitle("Most Prominent CEJST Category by State (Excluding Workforce Development)")

plot(State_issue_no_wf)

#ggsave("results/plots/state-issue_no-wf-wide-v1.jpeg", plot = State_issue_no_wf, width = 35, height = 15, units = c("cm"), dpi = 700)

#ggsave("results/plots/state-issue_no-wf-v1.jpeg", plot = State_issue_no_wf, width = 30, height = 20, units = c("cm"), dpi = 700)

dev.off()
## writing out as csv ## 
#write.csv(df_sum_counts_per_state,"results/state-avg_cat_threshold_exceeded_per_dac.csv", row.names = FALSE)
## END QUESTION 3 ### 


### Question 4 ### 
### Scatter plots of thresholds and categories against race/income variables ##  
Threshold <- cejest_raw %>%
  select(4:16,24,25)%>%
  group_by(`Total threshold criteria exceeded`)%>%
  summarize_all(mean, na.rm = TRUE)

Catagories <- cejest_raw %>%
  select(4:16,24,25)%>%
  group_by(`Total categories exceeded`)%>%
  summarize_all(mean, na.rm = TRUE)

Thresholds_sum <- cejest_raw %>%
  select(4:16,24,25,23)%>%
  group_by(`Total threshold criteria exceeded`)%>%
  summarize_all(sum, na.rm = TRUE)%>%
  dplyr::select(1,16)%>%
  left_join(Threshold)

#write.csv(Thresholds_sum ,"results/Thresholds_PopCharacteristics_v1.csv")

Thresholds_sum <- Thresholds_sum * Thresholds_sum$`Total population`


VariableMultiScatter <- Threshold %>% 
  select(1,2,7,15)%>%
  rename(`Below 200% of Federal Poverty Line` = `Adjusted percent of individuals below 200% Federal Poverty Line`)%>%
  pivot_longer(!`Total threshold criteria exceeded`,names_to="Demographics",values_to = "percent")

VariableMultiScatterPlot <- ggplot(VariableMultiScatter %>% filter(`Total threshold criteria exceeded` >0), aes(x = `Total threshold criteria exceeded`, y = percent *100, fill = `Demographics`))+
  #geom_point(fill = "#f45d00", color = "black", shape = 21, size = 6)+
  xlab("Total Threshold Criteria Exceeded")+
  ylab("% of Population")+
  ylim(0,90)+
  #geom_smooth(se = FALSE, aes(color = as.factor(PopulationVars)))+
  geom_point(shape = 21, size = 3.5, color = "black")+
  scale_colour_brewer(palette = "Dark2")+
  theme_classic()+
  labs(title = "", 
       color = "Demographics:")+
  theme(plot.title = element_text(face = "bold", size = 16))+
  theme(legend.title = element_text(face = "bold"))+
  theme(legend.text = element_text(size = 10), legend.position = "bottom")+
  theme(axis.title = element_text(size = 12),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
plot(VariableMultiScatterPlot)
#ggsave("results/plots/thresholds-race-pov_v2.png", plot = VariableMultiScatterPlot, units = "cm", width = 26, height = 16, dpi = 700, bg = "transparent")


#### DEMOG VARS AGAINST THRESHOLD COUNT - HOLY SMOKES #### 
ggplot(Threshold, aes(x = `Total threshold criteria exceeded`, y = `Percent Black or African American alone` *100))+
  geom_point(fill = "#f45d00", color = "black", shape = 21, size = 6)+
  xlab("Total Threshold Criteria Exceeded")+
  ylab("% Black or African American alone")+
  ylim(0,100)+
  theme_minimal()+
  labs(title = "CEJST Census Tract Threshold Count and Percent Black", 
       subtitle = "CEJST's binary catagorization hides significant racial inequity",
       caption = "CEJST Version 1 data accessed 02.01.2023, analyzed by Environmental Policy Innovation Center")

ggplot(Threshold, aes(x = `Total threshold criteria exceeded`, y = `Adjusted percent of individuals below 200% Federal Poverty Line`*100))+
  geom_point(fill = "#20841f", color = "black", shape = 21, size = 6)+
  theme_minimal()+
  xlab("Total Threshold Criteria Exceeded")+
  ylab("% Below Federal Poverty Line")+
  ylim(0,100)+
  labs(title = "CEJST Census Tract Threshold Count and Individuals 200% Below Federal Povery Rate", 
       subtitle = "CEJST's binary catagorization hides significant economic inequity",
       caption = "CEJST Version 1 data accessed 02.01.2023, analyzed by Environmental Policy Innovation Center")
## Chart of all race categories against thresholds ## 



## Histogram of Thresholds 
ggplot(cejest_raw %>% filter(`Total threshold criteria exceeded` > 0), aes(x=`Total threshold criteria exceeded`)) + 
  geom_histogram(colour="black", fill="white", bins = 200)+
  xlab("Total CEJST Threshold Criteria Exceeded")+
  ylab("Number of Census Tracts")+
  labs(title = "",
       subtitle = "")+
  #  geom_vline(aes(xintercept=1.65),
  #    color="red", linetype="dashed", size=.5)+ 
  theme_classic()




### Question 







