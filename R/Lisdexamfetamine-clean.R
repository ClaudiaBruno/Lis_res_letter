#################################################################################
# Project: 	Code for "Changes in lisdexamfetamine after PBS subsidy restriction 
#  change: Research Letter"				#
# Author:		Claudia Bruno (Code review by Malcolm Gillies)		            			#
#           School of Population Health                                      #
#           UNSW Sydney							                                    	#
print(paste0("Last updated ", Sys.Date()))			                                #
#################################################################################
#add libraries based on ARIMA code given in 
#"Interrupted time series analysis using Autoregressive #
#           Integrated Moving Average (ARIMA) models: A guide for evaluating 	#
#	          large-scale health interventions"
#required libraries for Andrea code
library(astsa)
library(forecast)
library(dplyr)
library(zoo)
#ARIMA libraries tidyvert
library(fable)
library(tsibble)
library(readxl)
#load libraries
library(magrittr)
library(ggplot2)
library(haven)
library(dplyr)
library(lubridate)
library(sessioninfo)
library(DBI)
library(data.table)
library(purrr)
library(flextable)

session_info()

# Create output


# log current git commit
system('git --no-pager log -1 --date=short --no-decorate --pretty=format:"%d @ %h %cd" HEAD')
message()

ADHD <- readRDS(file = "data/ADHD.rds")

setnames(ADHD, names(ADHD), tolower(names(ADHD)))

# suppress unreliable recent PBS data
ADHD %<>% filter(supp_date < as.Date("2022-07-01"))




#Prepare data for following analysis
#create new age group function for children and adults
agegrp <- function(a) {
  cut(a,
      breaks = c(-130,6,13,18,25,130), # randomised DOB -> neg age
      right=FALSE, ordered_result=TRUE,
      labels = c("0--5 years", "6--12 years", "13--17 years",
                 "18--24 years", "25+ years"))
}
agegrp_18 <- function(a) {
  cut(a,
      breaks = c(-130,18,25,130), # randomised DOB -> neg age
      right=FALSE, ordered_result=TRUE,
      labels = c("Under 18 years",
                 "18--24 years", "25+ years"))
}
##################################

#import PBS data


#create age_group, year, sex, and month of supply date - group total by monthly
#add class and medicine variable


ADHD[, ':=' (
  age_group_2 = agegrp(age), 
  age_group = agegrp_18(age), 
  year = lubridate::year(supp_date),
  sex = if_else(patient_sex == "M", "Male", "Female"), 
  mos = lubridate::ymd(paste(year(supp_date), month(supp_date), "01")), 
  
  class=fcase(
    atc_code == 'C02AC02', 'Nonstimulant',
    atc_code == 'N06BA02', 'Dexamfetamine',
    atc_code == 'N06BA04', 'Methylphenidate',
    atc_code == 'N06BA09', 'Nonstimulant',
    atc_code == 'N06BA12', 'Lisdexamfetamine',
    default = "Other"),  
  
  medicine = fcase(
    atc_code == 'C02AC02', 'Guanfacine',
    atc_code == 'N06BA02', 'Dexamfetamine',
    atc_code == 'N06BA04', 'Methylphenidate',
    atc_code == 'N06BA09', 'Atomoxetine',
    atc_code == 'N06BA12', 'Lisdexamfetamine',
    default = "Other"))] %>%  #other includes modafinil and armodafinil 
  .[, .(pat_id, supp_date, mos, year,
        item_code, 
        atc_code, class, medicine, age_group, age_group_2, sex, 
        rstr_num,
        stream_auth_code)]


#remove dispensing of other medicine (modafinal and armodafinil and dispensings for dexamfetamine with narcolepsy codes)
`%notin%` <- negate(`%in%`)


ADHD <- ADHD[medicine != "Other", ] %>% 
  .[rstr_num %notin% c(1236, 6227, 5983,6017, 6250, 6547, 8694, 10935, 10967, 10968, 10970) | stream_auth_code %notin% c(1236, 6227, 5983,6017, 6250, 6547, 8694, 10935, 10967, 10968, 10970),]


print(paste("Number of people with an ADHD dispensing between ", min(ADHD$supp_date), " and ", max(ADHD$supp_date), " is ", length(unique(ADHD$pat_id))))
