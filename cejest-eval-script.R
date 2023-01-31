
################################
#### Analysis of CEJEST v1 ##### 
################################

## See here for documentation ## 
## https://docs.google.com/document/d/1hqYa-I8bqMoRbaJK-EBtRxJ9RX09ehGX6qPvjq0alA4/edit# ## 
## Created by Gabe Watson 1.31.2023 ## 

library(tidyverse)
library(aws.s3)
library(leaflet)

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












