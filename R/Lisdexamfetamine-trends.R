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

#load libraries
library(magrittr)
library(ggplot2)
library(haven)
library(dplyr)
library(lubridate)
library(sessioninfo)
library(DBI)
library(data.table)
library(purr)

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
agegrp<- function(age){
  cut(age, breaks = c(-130, 18,25,130),  # randomised DOB -> neg age
      right=FALSE, ordered_result=TRUE,
      labels = c("0--17 years",
                 "18--24 years", "25+ years"))
}
##################################

#import PBS data


#create age_group, year, sex, and month of supply date - group total by monthly
#add class and medicine variable


ADHD[, ':=' (
  age_group = agegrp(age), 
  year = lubridate::year(supp_date),
  sex = if_else(patient_sex == "M", "Male", "Female"), 
  mos = lubridate::ymd(paste(year(supp_date), month(supp_date), "01")), 
  
  class=fcase(
    atc_code == 'C02AC02', 'Nonstimulant',
    atc_code == 'N06BA02', 'Stimulant',
    atc_code == 'N06BA04', 'Stimulant',
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
        atc_code, class, medicine, age_group, sex, 
        rstr_num,
        stream_auth_code)]


#remove dispensing of other medicine (modafinal and armodafinil and dispensings for dexamfetamine with narcolepsy codes)
`%notin%` <- negate(`%in%`)


ADHD[medicine != "Other", ] %>% 
  .[rstr_num %notin% c(1236, 6227, 5983,6017, 6250, 6547, 8694, 10935, 10967, 10968, 10970) | stream_auth_code %notin% c(1236, 6227, 5983,6017, 6250, 6547, 8694, 10935, 10967, 10968, 10970) ,   ]


print(paste("Number of people with an ADHD dispensing between ", min(ADHD$supp_date), " and " max(ADHD$supp_date), " is " length(unique(ADHD$pat_id))))
      
      # Convert data to time series object - dispensings
      disp_medicine<- 
        ADHD[age_group != "0--17 years", .N, by = .(medicine, mos)] %>% 
        arrange( medicine, mos) 

#for now i will just convert and run analysis on lisdexamfetamine only

disp_ts <- disp_medicine %>% 
  filter(medicine == "Lisdexamfetamine") %>% 
  select(N) %>% 
  ts(frequency=12, start=c(2016,1))

# Step 1: Plot data to visualise time series
options(scipen=5)

maxy <- max(disp_medicine$N[disp_medicine$medicine == "Lisdexamfetamine"])
plot(disp_ts, ylim=c(0,maxy+1000), type='l', col="blue", xlab="Month", ylab="Dispensings")
# Add vertical line indicating date of intervention (Feb 1, 2021)
abline(v=2021.083, col="gray", lty="dashed", lwd=2)

# Step 2. Transform data to stabilise variance (if necessary). If the variance is changing over time, a log-transformation should be applied.


# Step 3. Model selection: While automated algorithms in several statistical packages can identify candidate p and q parameters, they can sometimes be estimated based on the ACF/PACF plots.
#a. Determine differencing order to induce stationary: If there is seasonality, a seasonal difference is required and D =1.
#b. Plot the ACF/PACF of stationarity data to determine potential AR/MA orders
#c. Estimate model and use information criteria to find the best model:

# Step 3a. View ACF/PACF plots of undifferenced data
acf2(disp_ts, max.lag=24)

# Step 3a. View ACF/PACF plots of differenced/seasonally differenced data
acf2(diff(diff(disp_ts,12)), max.lag=24)

intervention_date = 'Feb 2021'
# Create variable representing step change and view
step <- as.numeric(as.yearmon(time(disp_ts))>=intervention_date)
step

# Create variable representing ramp (change in slope) and view
#edited from rep(0, 36), seq(1, 12, 1) <- assume this ramp begins at intervention
ramp <- append(rep(0,length(step[step < 1])), seq(1,length(step[step == 1]),1))
ramp  

#c. Estimate model and use information criteria to find the best model:

# Use automated algorithm to identify p/q parameters
# Specify first difference = 1 and seasonal difference = 1
model1 <- auto.arima(disp_ts, seasonal=TRUE, xreg=cbind(step,ramp), max.d=1, max.D=1, stepwise=FALSE, trace=TRUE)

#Automated best model: Regression with ARIMA(0,1,4)(0,0,1)[12] errors
#results seemed not to include any models with seasonal difference =1

#Autocorrelation order of the model (P) is 0, the moving average order of the model q = 4;
#Autocorrelation order of the seasonal model (P) is 0, the moving average order of the seasonal model Q = 1;


#creating a model 2 - which adds Seasonal difference D = 1
model2 <- Arima(disp_ts, xreg=cbind(step,ramp),order = c(0, 1, 4),
                seasonal = list(order = c(0, 1, 1) ))

#Step 4. Check if residuals of chosen model are white noise.

# Check residuals
checkresiduals(model1)
checkresiduals(model2)
#AFC plot model 1 lag crosses - 0.2; There is no obvious pattern or significant autocorrelation in the residuals, and they are normally distributed. resdiuals look normally distributed
#Model 2 There is no obvious pattern or significant autocorrelation in the residuals, and they are normally distributed. resdiuals look normally distributed

Box.test(model1$residuals, lag = 24, type = "Ljung-Box")
Box.test(model2$residuals, lag = 24, type = "Ljung-Box")

#The p-value for the Ljung-Box test for white noise is 0.17 (model 1) and 0.69 (model2) at 24 lags. 
#As the null hypothesis for the Ljung-Box test is that there is no significant autocorrelation, we do not reject the null and our chosen model has a good fit.The p-value for the Ljung-Box test for white noise is 0.50 at 24 lags. As the null hypothesis for the Ljung-Box test is that there is no significant autocorrelation, we do not reject the null and our chosen model has a good fit. 

# Estimate parameters and confidence intervals
summary(model1)
confint(model1)
#AIC=971.29   AICc=973.27   BIC=990.55
summary(model2)
confint(model2)

#model2 has lowest information criterion so will choose that model
#AIC=818.48   AICc=820.85   BIC=836.47


# To forecast the counterfactual, model data excluding post-intervention time period
model3 <- Arima(window(disp_ts, end=c(2021,1)), order=c(0,1,4), seasonal=list(order=c(0,1,1), period=18))

# Forecast 12 months post-intervention and convert to time series object
fc <- forecast(model3, h=18)%>%
  autoplot()
fc.ts <- ts(as.numeric(fc$mean), start=c(2021,2), frequency=18)

# Combine with observed data
#Bind time series which have a common frequency. ts.union pads with NAs to the total time coverage, ts.intersect restricts to the time covered by all the series.
disp_ts.2 <- ts.union(disp_ts, fc.ts)
disp_ts.2

# Plot 
plot(disp_ts.2, type="l", plot.type="s", col=c('blue','red'), xlab="Month", ylab="Dispensings", 
     #linetype=c("solid","dashed"), #getting error here
     
     ylim=c(0,maxy+50), 
     lwd=2)
abline(v=2021.083, col="gray", lty="dashed", lwd=2)
##end of Andrea's papers R code

##Forecast uses autoplot which uses ggplot - needs updating - ideally also plot 

disp_lis <- disp_medicine %>% filter(medicine == "Lisdexamfetamine") %>% arrange(mos) %>% 
  mutate(time =time(disp_ts), series= "disp_ts")

autoplot(disp_ts.2)+
  geom_point(data = disp_lis, aes(x=time, y=N))+
  labs(x="Date (Month Year)", y= "Number of dispensings", title= "Lisdexamfetamine dispensings with ARIMA forcast among adults") +
  scale_y_continuous(labels = scales::comma)+
  #facet_grid(. ~ age_group, margins=FALSE, scales = "free_y") +
  theme_Publication() +
  #scale_x_date(expand = c(0.04,0.04), date_minor_breaks = "3 month", date_labels = "%b %y", date_breaks = "6 month",) +
  geom_vline(xintercept = 2021.083, linetype=4, size=1.1)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#########################################
#Number of new users by month and age group - 2016 to 2022
#prepare new users dataset

ADHD_new <- ADHD[order(pat_id, supp_date)] %>% .[rowid(pat_id) == 1,] %>% 
  .[ year >= 2016,] %>% 
  .[, .(pat_id,  sex, mos, medicine, class, age_group)]




##should we include - new use by medicine: then compare adults who initiated treatment before the change then intiated another medicine following subsidy?

