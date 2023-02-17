
################################
#### Analysis of CEJEST v1 ##### 
################################

## See here for documentation ## 
## https://docs.google.com/document/d/1hqYa-I8bqMoRbaJK-EBtRxJ9RX09ehGX6qPvjq0alA4/edit# ## 
## Created by Gabe Watson 1.31.2023 ## 

library(tidyverse)
library(aws.s3)
library(leaflet)
library(tigris)
library(ggplot2)
library(magrittr)
library(sf)
library(htmltools)
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

cejest_issue = cejest_raw %>% 
  select("Census tract 2010 ID", `State/Territory`, `Is low income?`,
         c(grep("Greater than or equal to the 90th percentile for",names(cejest_raw)))) 
names(cejest_issue)[1:2] = c("GEOID10","State")

cejest_issue_melt = melt(cejest_issue, id = c("GEOID10","State"))
cejest_issue_melt = cejest_issue_melt %>% mutate(variable = gsub("\\?", "", gsub("Greater than or equal to the 90th percentile for ","",  variable)))
cejest_issue_true = cejest_issue_melt %>% filter(value == TRUE) 
###
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
state_dac_summary <- cejest_q_1 %>%
                     select(3,6,8,9,10,11)%>%
                     mutate(dac_pop = ifelse(dac == 1,`Total population`,0))%>%
                     mutate(state_dac_pop = sum(dac_pop, na.rm = TRUE))%>%
                     select(1,3,4,6,8)%>%
                     unique()%>%
                     mutate(dac_pop_per = state_dac_pop / state_pop)%>%
                     mutate(dac_tract_per = state_tract_dac_n / state_tract_n)%>%
                     select(1,state_pop,state_dac_pop,state_tract_n,state_tract_dac_n,dac_pop_per,dac_tract_per)

## writing out as csv ## 
write.csv(state_dac_summary,"results/state-dac-summary_v1.csv", row.names = FALSE)
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
write.csv(df_count_ind,"results/state-indicator_counts.csv", row.names = FALSE)
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
df_issues_count = df_issues_count %>% left_join(unique(df_sum_counts_per_state %>% select(`State/Territory`,
                                                                                    "state_tract_dac_n",
                                                                                    "tot_num_tracts_per_state") %>% dplyr::rename(State =`State/Territory`)))
df_issues_count = df_issues_count %>% mutate(cat_per_dac = n/state_tract_dac_n) %>% dplyr:: rename(cat_per_state = n)
# bar plot of stacked category plot
pdf("results/plots/state_issue_barplot.pdf", width = 14, height = 6)
ggplot(df_issues_count, aes(fill=category, x=State, y = cat_per_dac)) + 
  geom_bar(position="stack", stat="identity") +theme_bw()+ 
  scale_fill_manual(values = c("#009E73", # climate change
                               "#F98E1D", # Energy papaya
                               "#DC267F", # health ruby stone
                               "grey", # housing
                               "#C04000", #legacy pollution (mahogany)
                               "#3A3B3C", # transportation (dark grey)
                               "#0F52BA", # water sapphire blue
                               "#ffdd05")) + #workforce development speed yellow
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom") 
ggplot(df_issues_count  %>% group_by(State) %>% top_n(1,cat_per_dac) , aes(fill=category, x=State, y = cat_per_dac)) + 
  geom_bar(position="stack", stat="identity") +theme_bw()+ 
  scale_fill_manual(values = c("#009E73", # climate change
                               "#F98E1D", # Energy papaya
                               "#DC267F", # health ruby stone
                               "grey", # housing
                               "#C04000", #legacy pollution (mahogany)
                               "#3A3B3C", # transportation (dark grey)
                               "#0F52BA", # water sapphire blue
                               "#ffdd05")) + #workforce development speed yellow
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom"
        ) + ggtitle("Most prominant EJ category of a state")
ggplot(df_issues_count %>% filter(!category == "Workforce Development") %>% group_by(State) %>% top_n(1,cat_per_dac) , aes(fill=category, x=State, y = cat_per_dac)) + 
  geom_bar(position="stack", stat="identity") +theme_bw()+ 
  scale_fill_manual(values = c("#009E73", # climate change
                               "#F98E1D", # Energy papaya
                               "#DC267F", # health ruby stone
                               "grey", # housing
                               "#C04000", #legacy pollution (mahogany)
                               "#3A3B3C", # transportation (dark grey)
                               "#0F52BA", # water sapphire blue
                               "#ffdd05")) + #workforce development speed yellow
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom"
  ) + ggtitle("Most prominant EJ category of a state (exclude workforce development)")
dev.off()
## writing out as csv ## 
write.csv(df_sum_counts_per_state,"results/state-avg_cat_threshold_exceeded_per_dac.csv", row.names = FALSE)

## END QUESTION 3 ### 


## QUESTION 4:  Census tract map of indicator count by census tract ##
state_list  = unique(cejest_cleaned_v1$`State/Territory`)
US_tracts = data.frame()
for ( i in seq_along(state_list)) {
  temp = tracts(state = state_list[i],year = 2010)
  US_tracts = rbind(US_tracts,temp)
}
US_tracts_backup = US_tracts
US_tracts_simp = US_tracts %>% st_simplify(dTolerance = 50000)
US_tracts = US_tracts %>% select("GEOID10","geometry") %>% right_join(cejest_raw %>% rename(GEOID10 = `Census tract 2010 ID`),by = "GEOID10")

i = 1
geo_data = US_tracts %>% filter(!!sym(ind_list[i]) == TRUE)
labels <-sprintf(
    "<strong>%s</strong><br/> %s %s",
    gsub("Greater than or equal to the 90th percentile for ","",  ind_list[i]), geo_data$`County Name`,geo_data$`State/Territory` )%>%lapply(htmltools::HTML)

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addPolygons(data = geo_data,smoothFactor = 2, weight = 0.2, color = "black", opacity = 1, fillOpacity = 1,
              fillColor = "grey",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              label = ~labels,
              labelOptions = labelOptions(noHide = F, textsize = "15px")
              ) %>%
  setView(lat = 41.62531, lng = -97.71755, zoom = 3)

## save US tracts data
st_write(US_tracts %>% select(GEOID10,geometry), "results/US_tracts_CejstV1_2010_boundary.shp")

## END QUESTION 4 ### 




