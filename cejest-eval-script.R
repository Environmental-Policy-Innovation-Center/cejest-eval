
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
library(RColorBrewer)
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
#write.csv(df_count_ind,"results/state-indicator_counts.csv", row.names = FALSE)
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

## writing out as csv ## 
#write.csv(df_sum_counts_per_state,"results/state-avg_cat_threshold_exceeded_per_dac.csv", row.names = FALSE)

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
#st_write(US_tracts %>% select(GEOID10,geometry), "results/US_tracts_CejstV1_2010_boundary.shp")

### Static version colored by threshold/catagory number ### 


CA_tracts <- US_tracts %>%
                         filter(`State/Territory` == "Maryland")%>%
                         filter(`Total population` > 0)
# %>%
#                          filter(`Total threshold criteria exceeded` > 0)

plot <- ggplot(CA_tracts)+
  geom_sf(aes(fill = `Total threshold criteria exceeded`), linewidth = .01)+
  scale_fill_gradient(low = "#02abe2", high = "#da222b", na.value = NA)+
  theme_minimal()

plot
## END QUESTION 4 ### 


### Question 5 ### 
### Scatter plots of thresholds and catagories against race/income variables ##  
Threshold <- cejest_raw %>%
  select(4:16,24,25)%>%
  group_by(`Total threshold criteria exceeded`)%>%
  summarize_all(mean, na.rm = TRUE)

Catagories <- cejest_raw %>%
  select(4:16,24,25)%>%
  group_by(`Total categories exceeded`)%>%
  summarize_all(mean, na.rm = TRUE)

#### HOLY SMOKES #### 
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


### Question 







