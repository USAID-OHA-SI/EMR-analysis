# PROJECT: Munging EMR data & indicator data into usable format
# AUTHOR:   N.Petrovic | USAID
# PURPOSE:  Analyzing relationship between EMR and patient outcomes
# LICENSE:  MIT
# DATE:     2022-9-6
# UPDATED:  2022-9-6

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(ggtext)
library(glue)
library(purrr)

# IMPORT ------------------------------------------------------------------

## Data downloaded from Genie using parameters: 
        #Country=Burundi, Eswatini, Malawi, Nigeria,
        #Years - 2017,2018,2019,2020,2021 (EMR is annual so no 2022 data yet)
        #Stddisagg - Total Numerator, Total Denominator, Service Delivery Area
        #Frozen
## Data set (1,010,760) is just barely under excel limit of 1,048,576. When it goes
## over could select a smaller subset of indicators

df_all<- si_path() %>% 
  return_latest("Genie_SITE_IM_MultipleOUs") %>% 
  read_msd() %>% 
  resolve_knownissues()

## Can use this code if the data sets are too big for excel and need to be stitched together
#ous <- list.files(path = si_path(), 
#                  pattern = "SITE_IM.*Burundi|Eswatini|Malawi|Nigeria",
#                  full.names = TRUE)

# Loops over the data sets of available countries and creates a single data frame
#df_all <- purrr::map_dfr(ous, ~read_msd(file = .x)) 

# GLOBALS -------------------------------------------------------------------------

data_out <- "Dataout"

## Pulling all TX_ML indicators and all HTS
TX_ML_all<-sort(str_subset(unique(df_all$indicator), "TX_ML"))
HTS_all<-sort(str_subset(unique(df_all$indicator), "HTS_"))
HTS_all<-HTS_all[HTS_all!="HTS_RECENT"]

## Initial indicator list
indicator_init_ls<-c("EMR_SITE", TX_ML_all,"TX_CURR", "TX_NEW", "TX_PVLS") 

## Standardized disaggregates -- Service Delivery Area is std disagg for EMR
## Total Num/Denom for all other indicators of interest
std_disagg_ls<-c("Service Delivery Area", "Total Numerator", "Total Denominator")

## Info needed for each unique piece of data
cols_ls<-c("orgunituid","sitename", "sitetype", "psnu", "psnuuid", "country",
           "prime_partner_name", "prime_partner_uei", "mech_name", "mech_code", "funding_agency", 
           "communityuid", "community", "facilityuid", "facility", 
           "fiscal_year")


df<-df_all %>% filter(indicator %in% indicator_init_ls, standardizeddisaggregate %in% std_disagg_ls, 
                      ## Remove data that is above site level
                      sitename!="Data reported above Site level", typemilitary!="Y", is.na(targets)) %>%
                      ## Create denominator for TX_PVLS_D
                      mutate(indicator=case_when(indicator=="TX_PVLS" & numeratordenom=="N"~"TX_PVLS_N",
                                       indicator=="TX_PVLS" & numeratordenom=="D"~"TX_PVLS_D",
                                       TRUE ~ indicator )) %>%
                      ## Pull information about EMR category into indicator name
                      mutate(indicator=case_when(indicator=="EMR_SITE" ~ paste("EMR_SITE", otherdisaggregate),
                                                 TRUE ~ indicator)) %>%
                      ## Drop numerator/denom -- note that assumption is that only indicator w/ denom is TX_PVLS
                      select(-numeratordenom) %>%
                      ## Pivot all selected indicators into columns. Any redundant values (due to dropping indicatortype)
                      ## are summed.  
                      select(c(cols_ls,"indicator","cumulative")) %>%
                      pivot_wider(names_from=indicator, values_from =cumulative, id_cols = cols_ls,
                                  values_fn=sum) %>%
                      arrange(sitename, fiscal_year) %>%
                      ## Create a total value of EMR that sums over all different types of EMR in a site
                      mutate(EMR_SITE_TOTAL=rowSums(across(starts_with("EMR_SITE")),na.rm=TRUE)) 
                      ## Clean up names
                      names(df)<-str_remove(names(df), " Service Delivery Area")
                      ## Reorder to preferred order for indicators
                      df <- df %>%
                      select(cols_ls,c("EMR_SITE_TOTAL", "EMR_SITE - ANC and/or Maternity", "EMR_SITE - Care and Treatment", 
                                        "EMR_SITE - Early Infant Diagnosis not Ped ART", "EMR_SITE - HIV Testing Services", 
                                        "EMR_SITE - HIV/TB"),                
                                        TX_ML_all, c("TX_CURR", "TX_NEW", "TX_PVLS_N", "TX_PVLS_D")) 

                      write.csv(df,file="Dataout/msd_emr_data.csv",row.names=FALSE)

## Codes that can be used to double-check/plot data
                    
# df %>% dplyr::group_by(orgunituid, sitename, sitetype, psnu, psnuuid, 
#                country, prime_partner_name, prime_partner_uei, mech_name, mech_code, funding_agency, 
#                communityuid, community, facilityuid, facility, 
#                fiscal_year) %>%
#  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#  dplyr::filter(n > 1L) %>%
#  View()   

#  group_by(indicator, standardizeddisaggregate) %>%
#group_by(df,indicator) %>%
#  tally() %>%
#  View()

#df %>% ggplot(aes(x=EMR_SITE_TOTAL, y=TX_ML)) + geom_point()
                      
#sum_indic <- function(df1) {
#  df1 %>% 
#    group_by(fiscal_year, indicator) %>% 
#    summarise(val = sum(cumulative, na.rm = TRUE)) %>% 
#    ungroup()
#}

