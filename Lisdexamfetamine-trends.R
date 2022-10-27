####
#intention - plot number of ADHD medicine dispensings and new users of ADHD medicines using data from 2015 to 2022 (march 2022)

#Focus to describe trends of lisdexamfetamine use during this period, particularly after subsidy changes on February 2021 among adults. Trends of children included as comparator
#stratify trends by age group (and also sex)

########################################
#load libraries
library(magrittr)
library(ggplot2)
library(haven)
library(dplyr)
library(lubridate)
library(sessioninfo)
library(DBI)
library(data.table)

session_info()

# log current git commit
system('git --no-pager log -1 --date=short --no-decorate --pretty=format:"%d @ %h %cd" HEAD')
message()

ADHD <- readRDS(file = "data/ADHD.rds")

setnames(ADHD, names(ADHD), tolower(names(ADHD)))

# suppress unreliable recent PBS data
ADHD <- ADHD %>% filter(supp_date < as.Date("2022-04-01"))

########################################

##functions and themes to be used
#themes for graphs
theme_Publication <- function(base_size=14, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.25), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line.x = element_line(colour="black"),
            axis.line.y = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.6, "cm"),
            #legend.margin = unit(0.2, "cm"),
            legend.title = element_text(),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}


#Prepare data for following analysis
#create new age group function for children and adults
agegrp<- function(age){
  case_when(age  %between%  c(-130,17) ~ "Children (0-17 years)",
            age >=18 ~ "Adults (\u2265 18 years)")
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
  
  Class=fcase(
    atc_code == 'C02AC02', 'Nonstimulant',
    atc_code == 'N06BA02', 'Stimulant',
    atc_code == 'N06BA04', 'Stimulant',
    atc_code == 'N06BA09', 'Nonstimulant',
    atc_code == 'N06BA12', 'Lisdexamfetamine',
    default = "Other"),  
  
  Medicine = fcase(
    atc_code == 'C02AC02', 'Guanfacine',
    atc_code == 'N06BA02', 'Dexamfetamine',
    atc_code == 'N06BA04', 'Methylphenidate',
    atc_code == 'N06BA09', 'Atomoxetine',
    atc_code == 'N06BA12', 'Lisdexamfetamine',
    default = "Other"))] %>% 
  .[, .(pat_id, supp_date, mos, year,
        item_code, 
        atc_code, Medicine, Class, age_group, sex)]


print("Monthly dispensings of ADHD medicine (not including clonidine) by age group, 2016 to 2022")

#plot monthly dispensings by age group and sex

disp_medicine<- 
  ADHD[year != 2015,] %>% 
  .[, .N, by = .(Medicine, mos, age_group)]

total<- 
  ADHD[year != 2015,] %>% 
  .[, .N, by = .(mos, age_group)] %>% 
  .[, Medicine := "Total"]

disp_medicine<- rbind(disp_medicine, total)

disp_medicine$Medicine<-factor(disp_medicine$Medicine, levels = c("Total", "Atomoxetine","Dexamfetamine", "Guanfacine", "Lisdexamfetamine",  "Methylphenidate" ))


ggplot(disp_medicine) +
  geom_line(aes(x=mos, y=N, color=Medicine), size=1.15) +
  labs(x="Date (Month Year)", y= "Number of dispensings by medicine") +
  #scale_y_continuous(labels = scales::comma) +
  facet_grid(. ~ age_group, margins=FALSE, scales = "free_y") +
  theme_Publication() +
  scale_x_date(expand = c(0.04,0.04), date_minor_breaks = "3 month", date_labels = "%b %y", date_breaks = "6 month",) +
  geom_vline(xintercept = as.numeric(as.Date("2021-02-01")), linetype=4, size=1.1)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#save plot
ggsave(
  filename="out/ADHD dispensings by medicine and age group (2016-2022).tiff",
  plot = last_plot(),
  device = NULL,
  scale = 1,
  width = 2500/300,
  height = 1800/300,
  units = "in",
  dpi = 300,
  limitsize = FALSE,
)



disp_class<- 
  ADHD[year != 2015,] %>% 
  .[, .N, by = .(Class, mos, age_group)]

total_class<- ADHD[year != 2015,] %>% 
  .[, .N, by = .(mos, age_group)] %>% 
  .[, Class := "Total"]

disp_class <- rbind(disp_class, total_class)


disp_class$Class<-factor(disp_class$Class, levels = c("Total", "Nonstimulant","Stimulant",  "Lisdexamfetamine"))

#plot

ggplot(disp_class) + 
  geom_line(aes(x=mos, y=N, color=Class), size=1.15) +
  labs(x="Date (Month Year)", y= "Number of dispensings by class") +
  scale_y_continuous(labels = scales::comma)+
  facet_grid(. ~ age_group, margins=FALSE, scales = "free_y") +
  theme_Publication() +
  scale_x_date(expand = c(0.04,0.04), date_minor_breaks = "3 month", date_labels = "%b %y", date_breaks = "6 month",) +
  geom_vline(xintercept = as.numeric(as.Date("2021-02-01")), linetype=4, size=1.1)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#save plot
ggsave(
  filename="out/ADHD dispensings by class and age group (2016-2022).tiff",
  plot = last_plot(),
  device = NULL,
  scale = 1,
  width = 2500/300,
  height = 1800/300,
  units = "in",
  dpi = 300,
  limitsize = FALSE,
)

#########################################
#Number of new users by month and age group - 2016 to 2022
#prepare new users dataset


ADHD_new <- ADHD[order(pat_id, supp_date)] %>% .[rowid(pat_id) == 1,] %>% 
  .[ year >= 2016,] %>% 
  .[, .(pat_id,  sex, mos, Medicine, Class, age_group)]


new_medicine<-  ADHD_new[, .N, by = .(Medicine, mos, age_group)]   

total<- ADHD_new[, .N, by = .(mos, age_group)] %>% 
  .[, Medicine := "Total"]

new_medicine<- rbind(new_medicine, total)

new_medicine$Medicine<-factor(new_medicine$Medicine, levels = c("Total", "Atomoxetine","Dexamfetamine", "Guanfacine", "Lisdexamfetamine",  "Methylphenidate" ))


ggplot(new_medicine) +
  geom_line(aes(x=mos, y=N, color=Medicine), size=1.15) +
  labs(x="Date (Month Year)", y= "Number of new users by medicine") +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(. ~ age_group, margins=FALSE, scales = "free_y") +
  theme_Publication() +
  scale_x_date(expand = c(0.04,0.04), date_minor_breaks = "3 month", date_labels = "%b %y", date_breaks = "6 month",) +
  
  geom_vline(xintercept = as.numeric(as.Date("2021-02-01")), linetype=4, size=1.1)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#save plot
ggsave(
  filename="out/New ADHD medicine users by medicine and age group (2016-2022).tiff",
  plot = last_plot(),
  device = NULL,
  scale = 1,
  width = 2500/300,
  height = 1800/300,
  units = "in",
  dpi = 300,
  limitsize = FALSE,
)


new_class<-  ADHD_new[, .N, by = .(Class, mos, age_group)] 

total_class<- ADHD_new[, .N, by = .(mos, age_group)] %>% 
  .[, Class := "Total"]

new_class <- rbind(new_class, total_class)


new_class$Class<-factor(new_class$Class, levels = c("Total", "Nonstimulant","Stimulant",  "Lisdexamfetamine" ))



ggplot(new_class) + 
  geom_line(aes(x=mos, y=N, color=Class), size=1.15) +
  labs(x="Date (Month Year)", y= "Total new users by class") +
  scale_y_continuous(labels = scales::comma)+
  facet_grid(. ~ age_group, margins=FALSE, scales = "free_y") +
  theme_Publication() +
  scale_x_date(expand = c(0.04,0.04), date_minor_breaks = "3 month", date_labels = "%b %y", date_breaks = "6 month",) +
  
  geom_vline(xintercept = as.numeric(as.Date("2021-02-01")), linetype=4, size=1.1)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#save plot
ggsave(
  filename="out/New ADHD medicine users by class and age group (2016-2022).tiff",
  plot = last_plot(),
  device = NULL,
  scale = 1,
  width = 2500/300,
  height = 1800/300,
  units = "in",
  dpi = 300,
  limitsize = FALSE,
)

#########################################
## Same as above but also stratified by sex

#plot monthly dispensings by age group and sex

disp_medicine<-  ADHD[, .N, by = .(Medicine, mos, age_group, sex)] 

total<- ADHD[, .N, by = .(mos, age_group, sex)] %>% 
  .[, Medicine := "Total"]

disp_medicine<- rbind(disp_medicine, total)

disp_medicine$Medicine<-factor(disp_medicine$Medicine, levels = c("Total", "Atomoxetine","Dexamfetamine", "Guanfacine", "Lisdexamfetamine",  "Methylphenidate" ))


ggplot(disp_medicine) +
  geom_line(aes(x=mos, y=N, color=Medicine), size=1.15) +
  labs(x="Date (Month Year)", y= "Number of dispensings by medicine") +
  #scale_y_continuous(labels = scales::comma) +
  facet_grid(sex ~ age_group, margins=FALSE, scales = "free_y") +
  theme_Publication() +
  scale_x_date(expand = c(0.04,0.04), date_minor_breaks = "3 month", date_labels = "%b %y", date_breaks = "6 month",) +
  
  geom_vline(xintercept = as.numeric(as.Date("2021-02-01")), linetype=4, size=1.1)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#save plot
ggsave(
  filename="out/ADHD dispensings by medicine, sex and age group (2016-2022).tiff",
  plot = last_plot(),
  device = NULL,
  scale = 1,
  width = 2500/300,
  height = 2200/300,
  units = "in",
  dpi = 300,
  limitsize = FALSE,
)



disp_class<-  ADHD[, .N, by = .(Class, mos, age_group, sex)] 

total<- ADHD[, .N, by = .(mos, age_group, sex)] %>% 
  .[, Class := "Total"]

disp_class <- rbind(disp_class, total)


disp_class$Class<-factor(disp_class$Class, levels = c("Total", "Nonstimulant","Stimulant",  "Lisdexamfetamine"))


ggplot(disp_class) + 
  geom_line(aes(x=mos, y=N, color=Class), size=1.15) +
  labs(x="Date (Month Year)", y= "Number of dispensings by class") +
  scale_y_continuous(labels = scales::comma)+
  facet_grid(sex ~ age_group, margins=FALSE, scales = "free_y") +
  theme_Publication() +
  scale_x_date(expand = c(0.04,0.04), date_minor_breaks = "3 month", date_labels = "%b %y", date_breaks = "6 month",) +
  geom_vline(xintercept = as.numeric(as.Date("2021-02-01")), linetype=4, size=1.1)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#save plot
ggsave(
  filename="out/ADHD dispensings by class, sex and age group (2016-2022).tiff",
  plot = last_plot(),
  device = NULL,
  scale = 1,
  width = 2500/300,
  height = 2200/300,
  units = "in",
  dpi = 300,
  limitsize = FALSE,
)

#########################################
#NEW USERS
new_medicine<-  ADHD_new[, .N, by = .(Medicine, mos, age_group, sex)] 

total<- ADHD_new[, .N, by = .(mos, age_group, sex)] %>% 
  .[, Medicine := "Total"]

new_medicine<- rbind(new_medicine, total)

new_medicine$Medicine<-factor(new_medicine$Medicine, levels = c("Total", "Atomoxetine","Dexamfetamine", "Guanfacine", "Lisdexamfetamine",  "Methylphenidate" ))


ggplot(new_medicine) +
  geom_line(aes(x=mos, y=N, color=Medicine), size=1.15) +
  labs(x="Date (Month Year)", y= "Number of new users by medicine") +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(sex ~ age_group, margins=FALSE, scales = "free_y") +
  theme_Publication() +
  scale_x_date(expand = c(0.04,0.04), date_minor_breaks = "3 month", date_labels = "%b %y", date_breaks = "6 month",) +
  
  geom_vline(xintercept = as.numeric(as.Date("2021-02-01")), linetype=4, size=1.1)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#save plot
ggsave(
  filename="out/New ADHD medicine users by medicine, sex and age group (2016-2022).tiff",
  plot = last_plot(),
  device = NULL,
  scale = 1,
  width = 2500/300,
  height = 2200/300,
  units = "in",
  dpi = 300,
  limitsize = FALSE,
)

new_class<-  ADHD_new[, .N, by = .(Class, mos, age_group, sex)] 

total_class<- ADHD_new[, .N, by = .(mos, age_group, sex)] %>% 
  .[, Class := "Total"]

new_class <- rbind(new_class, total_class)


new_class$Class<-factor(new_class$Class, levels = c("Total", "Nonstimulant","Stimulant",  "Lisdexamfetamine" ))



ggplot(new_class) + 
  geom_line(aes(x=mos, y=N, color=Class), size=1.15) +
  labs(x="Date (Month Year)", y= "Number of new users by class") +
  scale_y_continuous(labels = scales::comma)+
  facet_grid(sex ~ age_group, margins=FALSE, scales = "free_y") +
  theme_Publication() +
  scale_x_date(expand = c(0.04,0.04), date_minor_breaks = "3 month", date_labels = "%b %y", date_breaks = "6 month",) +
  
  geom_vline(xintercept = as.numeric(as.Date("2021-02-01")), linetype=4, size=1.1)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#save plot
ggsave(
  filename="out/New ADHD medicine users by class, sex and age group (2016-2022).tiff",
  plot = last_plot(),
  device = NULL,
  scale = 1,
  width = 2500/300,
  height = 2200/300,
  units = "in",
  dpi = 300,
  limitsize = FALSE,
)


