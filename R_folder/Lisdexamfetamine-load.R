####
# Load PBS ADHD data into RDS

#PBSDIR="/mnt/pearson/PBS 10% Sample/Source Data/"

#comment out Malcolm directory - unsure how to make this part more reproducible
PBSDIR="Y:/PBS 10% Sample/Source Data/"

########################################
#load libraries
library(haven)
library(dplyr)
library(lubridate)
library(sessioninfo)
library(data.table)

session_info()
time_start<-Sys.time()

# log current git commit
system('git --no-pager log -1 --date=short --no-decorate --pretty=format:"%d @ %h %cd" HEAD')
message()

pt <- read_sas(paste0(PBSDIR,"patient_ids.sas7bdat"))
setDT(pt)
setkey(pt, PAT_ID)

pt[, dob :=  as.Date(paste0(YEAR_BIRTH, '-07-01'))]
#pt[, c("YEAR_BIRTH", "YEAR_DEATH", "flag_100", "date_100") := NULL]

agec <- function(a, b) {
  age<-floor(time_length(interval(as.Date(a), as.Date(b)), "years"))
  ifelse(age<0,0,ifelse(age>100,100,age))
}

ADHD <- NULL

for(yr in c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")) {
  message(paste0("Reading PBS data: ", yr))
  pbs <- read_sas(paste0(PBSDIR, "y", yr, "supply.sas7bdat"),
                  paste0(PBSDIR, "formats.sas7bcat"),
                  col_select=c("SUPP_DATE", "SCRIPTS", "ATC_CODE",
                               "FORM_CAT", "PAT_ID", "PHARMACY_STATE",
                               "CONCESSIONAL_STATUS", "ITEM_CODE", "QTY",
                               "PRESC_ID", "AUTH_FORM_ID", "STREAM_AUTH_CODE", "PRESC_DATE", "DERIVED_SPECIALTY"
                                 ))
  message(paste0("    Modified: ",
                 file.info(paste0(PBSDIR, "y", yr, "supply.sas7bdat"))$mtime))
  message(paste0("    Rows: ", nrow(pbs)))
  
  setDT(pbs)
  
  # XXXX can we optimise the subset part?
  ##I have been using bread() R package to subset on ATC_code and select columns, but not sure if it works on SAS files - not necessarily quicker but I cant load such large datasets on my 16G RAM computer
  
  ##not sure if grep or grepl is any quicker -- I thought Bread uses grepl
  
  ##I have had some issues with bread if data is messy (i.e, if an ATC code was recorded as C02,AC02 it would move the second half to the next column); 
  
  ADHD_tmp <- pbs[substr(ATC_CODE,1,5)=='N06BA'|substr(ATC_CODE,1,7)=='C02AC02']
 
   rm(pbs)
 
   ADHD_tmp[pt, c("AGE", "PATIENT_SEX") := .(agec(dob, SUPP_DATE),
                                        PATIENT_SEX), on="PAT_ID"]
  
   ADHD <- rbind(ADHD, ADHD_tmp)
   rm(ADHD_tmp)
}

auth <- read_sas(paste0(PBSDIR, "authority.sas7bdat"))
setDT(auth)
setkey(auth, PAT_ID)

ADHD_auth<- merge(x=ADHD, y=auth,  by.x = c('PAT_ID', 'PRESC_DATE', 'ITEM_CODE'),
                                      by.y = c('PAT_ID', 'AUTH_APPR_DTE','AUTH_ITEM_CODE'), 
                  all.x = TRUE)



#check indication and medicine match - need historical restriction numbers:
#Noting current Authority codes for (can extract and compare with PBS RestrictionExtract text tables from PBS):
#narcolepsy 1236, 6227, 5983,6017, 6250, 6547, 8694, 10935, 10967, 10968, 10970)
#ADHD = 6226, 6249, 6279, 6298, 4578, 5252, 7876, 7890, 8544, 8585, 9031, 9034, 10717, 10719, 10792 -> age criteria)
#number of missing rstr number

 
saveRDS(ADHD_auth, file = "data/ADHD.Rds")

time_end<- Sys.time()

time_end - time_start
