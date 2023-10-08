library(tidyverse)
library(collapse)
library(stargazer)
library(plm)
library(eventStudy)
library(fixest)
library(did)
library(ggplot2)
library(broom) 
library(data.table)
library(pwr)
library(car) 
library(DRDID)
library(tidysynth)


##DATA PREPARATION

#Setting Working Directory: 
#setwd("FILEPATH")
data <- read.csv("cpsgen.csv")
str(data)

data$Woman <- ifelse(data$sex == 2, 1, 0)
data$Man <- ifelse(data$sex == 1, 1, 0)
sum(data$Woman)
sum(data$Man)

data <- data %>% group_by(serial, year) %>% mutate(child = sum(relate == 301))

#Only private sector workers
data <- data %>% filter(empstat == 10) %>% filter(age >= 25 & age <= 55) %>% 
  filter(classwkr == 21) 

#To include civil servants for robustness check: 
#data <- data %>% filter(empstat == 10) %>% filter(age >= 25 & age <= 55) %>% 
 # filter(classwkr != 29 & classwkr != 10) 

data$FemWork <- ifelse(data$sex == 2, data$hrswork, 0)
sum(data$Woman)
sum(data$Man)
sum(is.na(data$realhrwage))
data$wageMale <- ifelse(data$sex == 1, data$realhrwage, 0)
data$wageFem <- ifelse(data$sex == 2, data$realhrwage, 0)
data$married <- ifelse(data$marst == 1, 1, 0)


#Collapsing by state and year
data_Collapse1 <- data %>% group_by(year, statefip) %>% summarize(mean_sex = mean(sex),                                                  , 
               mean_white = mean(white), mean_black = mean(black), mean_hisp = mean(hisp),
               mean_othrace = mean(othrace), mean_married = mean(married), 
               mean_hrswork = mean(hrswork), mean_FemWork = mean(FemWork), 
               median_age = median(age), median_sch = median(sch), mean_child = mean(child),
               median_wageMale = median(wageMale[wageMale!=0]), 
               median_wageFem = median(wageFem[wageFem!=0]))
sum(is.na(data_Collapse1$mean_othrace))

data_Collapse1$logwageMale <- log(data_Collapse1$median_wageMale)
data_Collapse1$logwageFem <- log(data_Collapse1$median_wageFem)

#GENDER WAGE GAP
data_Collapse1$wagegap <- data_Collapse1$logwageMale - data_Collapse1$logwageFem

#WOMEN WORK SHARE
data_Collapse1$FemWorkShare <- data_Collapse1$mean_FemWork/data_Collapse1$mean_hrswork


data_Collapse1$post <- ifelse(data_Collapse1$year >= 2004, 1, 0)
data_Collapse1$treated <- ifelse(data_Collapse1$statefip == 17, 1, 0)
data_Collapse1$did <- data_Collapse1$post * data_Collapse1$treated
sum(data$did)



data_Collapse1 %>% glimpse()

data_Collapse1$State <- data_Collapse1$statefip

data_Collapse1$State[data_Collapse1$statefip == 1] <- "Alabama"
data_Collapse1$State[data_Collapse1$statefip == 2] <- "Alaska"
data_Collapse1$State[data_Collapse1$statefip == 4] <- "Arizona"
data_Collapse1$State[data_Collapse1$statefip == 5] <- "Arkansas"
data_Collapse1$State[data_Collapse1$statefip == 6] <- "California"
data_Collapse1$State[data_Collapse1$statefip == 8] <- "Colorado"
data_Collapse1$State[data_Collapse1$statefip == 9] <- "Connecticut"
data_Collapse1$State[data_Collapse1$statefip == 10] <- "Delaware"
data_Collapse1$State[data_Collapse1$statefip == 11] <- "DC"
data_Collapse1$State[data_Collapse1$statefip == 12] <- "Florida"
data_Collapse1$State[data_Collapse1$statefip == 13] <- "Georgia"
data_Collapse1$State[data_Collapse1$statefip == 15] <- "Hawaii"
data_Collapse1$State[data_Collapse1$statefip == 16] <- "Idaho"
data_Collapse1$State[data_Collapse1$statefip == 17] <- "Illinois"
data_Collapse1$State[data_Collapse1$statefip == 18] <- "Indiana"
data_Collapse1$State[data_Collapse1$statefip == 19] <- "Iowa"
data_Collapse1$State[data_Collapse1$statefip == 20] <- "Kensas"
data_Collapse1$State[data_Collapse1$statefip == 21] <- "Kentucky"
data_Collapse1$State[data_Collapse1$statefip == 22] <- "Louisiana"
data_Collapse1$State[data_Collapse1$statefip == 23] <- "Maine"
data_Collapse1$State[data_Collapse1$statefip == 24] <- "Maryland"
data_Collapse1$State[data_Collapse1$statefip == 25] <- "Massachusetts"
data_Collapse1$State[data_Collapse1$statefip == 26] <- "Michigan"
data_Collapse1$State[data_Collapse1$statefip == 27] <- "Minnesota"
data_Collapse1$State[data_Collapse1$statefip == 28] <- "Mississippi"
data_Collapse1$State[data_Collapse1$statefip == 29] <- "Missouri"
data_Collapse1$State[data_Collapse1$statefip == 30] <- "Montana"
data_Collapse1$State[data_Collapse1$statefip == 31] <- "Nebraska"
data_Collapse1$State[data_Collapse1$statefip == 32] <- "Nevada"
data_Collapse1$State[data_Collapse1$statefip == 33] <- "New Hampshire"
data_Collapse1$State[data_Collapse1$statefip == 34] <- "New Jersey"
data_Collapse1$State[data_Collapse1$statefip == 35] <- "New Mexico"
data_Collapse1$State[data_Collapse1$statefip == 36] <- "New York"
data_Collapse1$State[data_Collapse1$statefip == 37] <- "N Carolina"
data_Collapse1$State[data_Collapse1$statefip == 38] <- "N Dakota"
data_Collapse1$State[data_Collapse1$statefip == 39] <- "Ohio"
data_Collapse1$State[data_Collapse1$statefip == 40] <- "Oklahoma"
data_Collapse1$State[data_Collapse1$statefip == 41] <- "Oregon"
data_Collapse1$State[data_Collapse1$statefip == 42] <- "Pennsylvania"
data_Collapse1$State[data_Collapse1$statefip == 44] <- "Rhode Island"
data_Collapse1$State[data_Collapse1$statefip == 45] <- "S Carolina"
data_Collapse1$State[data_Collapse1$statefip == 46] <- "S Dakota"
data_Collapse1$State[data_Collapse1$statefip == 47] <- "Tennessee"
data_Collapse1$State[data_Collapse1$statefip == 48] <- "Texas"
data_Collapse1$State[data_Collapse1$statefip == 49] <- "Utah"
data_Collapse1$State[data_Collapse1$statefip == 50] <- "Vermont"
data_Collapse1$State[data_Collapse1$statefip == 51] <- "Virginia"
data_Collapse1$State[data_Collapse1$statefip == 53] <- "Washington"
data_Collapse1$State[data_Collapse1$statefip == 54] <- "West Virginia"
data_Collapse1$State[data_Collapse1$statefip == 55] <- "Wisconsin"
data_Collapse1$State[data_Collapse1$statefip == 56] <- "Wyoming"

ggplot(data_Collapse1, aes(x = factor(year), y = wagegap, colour = State, group = statefip)) +
  geom_line()

data_SAVE <- data_Collapse1[c("year","statefip", "State", "wagegap", "FemWorkShare", "median_age",
                              "median_sch", "mean_married", "mean_white", 
                              "mean_black", "mean_hisp", "mean_child", "treated", "post", "did")]

data_SAVE$year <- as.double(data_SAVE$year)
data_SAVE <- data_SAVE %>% ungroup()
glimpse(data_SAVE)

data_SAVE <- data_SAVE %>% filter(State != "West Virginia")
data_SAVE <- data_SAVE %>% filter(State != "Virginia")
data_SAVE <- data_SAVE %>% filter(State != "Vermont")
data_SAVE <- data_SAVE %>% filter(State != "Alabama")
data_SAVE <- data_SAVE %>% filter(State != "Mississippi")
data_SAVE <- data_SAVE %>% filter(State != "Alaska")
data_SAVE <- data_SAVE %>% filter(State != "Hawaii")
data_SAVE <- data_SAVE %>% filter(State != "DC")
data_SAVE <- data_SAVE %>% filter(State != "Michigan")
data_SAVE <- data_SAVE %>% filter(State != "California")
data_SAVE <- data_SAVE %>% filter(State != "New Jersey")
data_SAVE <- data_SAVE %>% filter(State != "Indiana")




##DESCRIPTIVE STATSTICS
GWG_CG <- data_Collapse1 %>% filter(State != "Illinois") %>% group_by(year) %>% 
  summarise(meanGWG_CG=mean(wagegap))
GWG_Ill <- data_Collapse1 %>% filter(State == "Illinois") %>% group_by(year) %>% 
  summarise(meanGWG_Ill=mean(wagegap))
WS_CG <- data_Collapse1 %>% filter(State != "Illinois") %>% group_by(year) %>% 
  summarise(meanWS_CG=mean(FemWorkShare))
WS_Ill <- data_Collapse1 %>% filter(State == "Illinois") %>% group_by(year) %>% 
  summarise(meanWS_Ill=mean(FemWorkShare))
GWG <- merge(GWG_CG, GWG_Ill, by='year')
WS <- merge(WS_CG, WS_Ill, by='year')
stat_descri <- merge(GWG, WS, by="year")

ggplot(stat_descri, aes(x=year)) +
  geom_line(aes(y = meanGWG_CG), color = "darkred") +
  geom_line(aes(y = meanGWG_Ill), color="steelblue") +
  labs(x="Year", y="Gender Wage Gap", subtitle = "Time trends Wage Gap between female and male
employees in Illinois (blue line) and the average for the other US states (red line).", 
       colour = "Cylinders")

ggplot(stat_descri, aes(x=year)) +
  geom_line(aes(y = meanWS_CG), color = "darkred") +
  geom_line(aes(y = meanWS_Ill), color="steelblue") +
  labs(x="Year", y="Women's Work Share", subtitle = "Time trends of female labour market participation
for Illinois (blue line) and average for the other US states (red
line).", colour = "Cylinders")


#CONTROL GROUP
GWG_CG <- data_SAVE %>% filter(State != "Illinois") %>% group_by(year) %>% 
  summarise(meanGWG_CG=mean(wagegap))
GWG_Ill <- data_SAVE %>% filter(State == "Illinois") %>% group_by(year) %>% 
  summarise(meanGWG_Ill=mean(wagegap))
WS_CG <- data_SAVE %>% filter(State != "Illinois") %>% group_by(year) %>% 
  summarise(meanWS_CG=mean(FemWorkShare))
WS_Ill <- data_SAVE %>% filter(State == "Illinois") %>% group_by(year) %>% 
  summarise(meanWS_Ill=mean(FemWorkShare))
GWG <- merge(GWG_CG, GWG_Ill, by='year')
WS <- merge(WS_CG, WS_Ill, by='year')
stat_descri <- merge(GWG, WS, by="year")

ggplot(stat_descri, aes(x=year)) +
  geom_line(aes(y = meanGWG_CG), color = "darkred") +
  geom_line(aes(y = meanGWG_Ill), color="steelblue") +
  labs(x="Year", y="Gender Wage Gap", subtitle = "Time trends Wage Gap between female and male
employees in Illinois (blue line) and the average for the
control states (red line).", colour = "Cylinders")

ggplot(stat_descri, aes(x=year)) +
  geom_line(aes(y = meanWS_CG), color = "darkred") +
  geom_line(aes(y = meanWS_Ill), color="steelblue") +
  labs(x="Year", y="Women's Work Share", subtitle = "Time trends of female labour market participation
for Illinois (blue line) and average for the control states (red
line).", colour = "Cylinders")


#VERMONT
GWG_Ver <- data_Collapse1 %>% filter(State == "Vermont") %>% group_by(year) %>% 
  summarise(meanGWG_Ver=mean(wagegap))
WS_Ver <- data_Collapse1 %>% filter(State == "Vermont") %>% group_by(year) %>% 
  summarise(meanWS_Ver=mean(FemWorkShare))
GWG2 <- merge(GWG_Ver, GWG_Ill, by='year')
WS2 <- merge(WS_Ver, WS_Ill, by='year')
stat_descri2 <- merge(GWG2, WS2, by="year")

ggplot(stat_descri2, aes(x=year)) +
  geom_line(aes(y = meanGWG_Ver), color = "darkred") +
  geom_line(aes(y = meanGWG_Ill), color="steelblue") +
  labs(x="Year", y="Gender Wage Gap", colour = "Cylinders")

ggplot(stat_descri2, aes(x=year)) +
  geom_line(aes(y = meanWS_Ver), color = "darkred") +
  geom_line(aes(y = meanWS_Ill), color="steelblue") +
  labs(x="Year", y="Women's work share", colour = "Cylinders")

#TOTAL
data_Coco <-  select(data_Collapse1, -c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22))
data_long <- melt(data_Coco, id.vars = "year")    # Reshaping data to long format
head(data_long)  
ggplot(data_long,                            # Draw ggplot2 time series plot
       aes(x = year,
           y = value,
           col = variable)) +
  geom_line()

ggplot(data_Collapse1, aes(x = factor(year), y = wagegap, colour = statefip, group = statefip)) +
  geom_line()

data2003 <- data_Collapse1 %>% filter(year == 2003) 
which.max(data2003$wagegap)





##SYNTHETIC CONTROL (SCM)

#SCM GWG
SC1_GWG <- data_SAVE %>%
synthetic_control(outcome = wagegap, # outcome
                  unit = State, # unit index in the panel data
                  time = year, # time index in the panel data
                  i_unit = "Illinois", # unit where the intervention occurred
                  i_time = 2004, # time period when the intervention occurred
                  generate_placebos=TRUE # generate placebo synthetic controls (for inference)
) 

SC2_GWG <- SC1_GWG %>% generate_predictor(time_window = 2000:2004,
                         age = mean(median_age),
                         educ = mean(median_sch),
                         married = mean(mean_married),
                         white = mean(mean_white),
                         black = mean(mean_black),
                         hisp = mean(mean_hisp),
                         child = mean(mean_child)
                         ) 

SC3_GWG <- SC2_GWG %>% generate_weights(optimization_window = 2000:2004, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) 
  # Generate the synthetic control
SCM_GWG <- SC3_GWG %>% generate_control()

SCM_GWG %>% plot_trends()
SCM_GWG %>% plot_differences()
SCM_GWG %>% plot_weights()
SCM_GWG %>% grab_balance_table()



#SCM WORK SHARE
SC1_WS <- data_SAVE %>%
  synthetic_control(outcome = FemWorkShare, # outcome
                    unit = State, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "Illinois", # unit where the intervention occurred
                    i_time = 2004, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) 

SC2_WS <- SC1_WS %>% generate_predictor(time_window = 2000:2004,
                                  age = mean(median_age),
                                  educ = mean(median_sch),
                                  married = mean(mean_married),
                                  white = mean(mean_white),
                                  black = mean(mean_black),
                                  hisp = mean(mean_hisp),
                                  child = mean(mean_child)
) 

SC3_WS <- SC2_WS %>% generate_weights(optimization_window = 2000:2004, # time to use in the optimization task
                                margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
) 
# Generate the synthetic control
SCM_WS <- SC3_WS %>% generate_control()

SCM_WS %>% plot_trends()
SCM_WS %>% plot_differences()
SCM_WS %>% plot_weights()
SCM_WS %>% grab_balance_table()


#PLACEBO
SCM_GWG %>% plot_placebos()
SCM_WS %>% plot_placebos()

SCM_GWG %>% plot_mspe_ratio()
SCM_WS %>% plot_mspe_ratio()

SCM_GWG %>% grab_significance()
SCM_WS %>% grab_significance()





## OLS

#Separating by year
data_SAVE$yeardummy <- factor(data_SAVE$year) 
levels(data_SAVE$yeardummy)

data_SAVE2003 <- data_SAVE %>% filter(data_SAVE$year < 2004)
data_SAVE2004 <- data_SAVE %>% filter(data_SAVE$year < 2005)
data_SAVE2005 <- data_SAVE %>% filter(data_SAVE$year < 2006 & data_SAVE$year != 2004)
data_SAVE2006 <- data_SAVE %>% filter(data_SAVE$year != 2004 & data_SAVE$year != 2005)


## GWG DID

#REGRESSION ON EACH YEAR
didregW0_04 = lm(wagegap ~  treated + post + did, data = data_SAVE2004)
summary(didregW0_04)
didregW0_05 = lm(wagegap ~  treated + post + did, data = data_SAVE2005)
summary(didregW0_05)
didregW0_06 = lm(wagegap ~  treated + post + did, data = data_SAVE2006)
summary(didregW0_06)

didregW1_04 = lm(wagegap ~ treated +  post + did + median_age + median_sch +
                mean_married + mean_white + mean_black + mean_hisp + mean_child, data = data_SAVE2004) #CONTROLS
summary(didregW1_04)
confint(didregW1_04, 'did', level=0.95)
didregW1_05 = lm(wagegap ~ treated +  post + did + median_age + median_sch +
                mean_married + mean_white + mean_black + mean_hisp + mean_child, data = data_SAVE2005) #CONTROLS
summary(didregW1_05)
confint(didregW1_05, 'did', level=0.95)
didregW1_06 = lm(wagegap ~ treated +  post + did + median_age + median_sch +
                mean_married + mean_white + mean_black + mean_hisp + mean_child, data = data_SAVE2006) #CONTROLS
summary(didregW1_06)
confint(didregW1_06, 'did', level=0.95)

#TEST FOR EXPECTATIONS
didregW1_03 = lm(wagegap ~ treated + post + did + median_age + median_sch +
                   mean_married + mean_white + mean_black + mean_hisp + mean_child, data = data_SAVE2003) #CONTROLS
summary(didregW1_03)



## WORK SHARE DID

#REGRESSION ON EACH YEAR
didregS0_04 = lm(FemWorkShare ~ treated + post + did, data = data_SAVE2004)
summary(didregS0_04)
didregS0_05 = lm(FemWorkShare ~ treated + post + did, data = data_SAVE2005)
summary(didregS0_05)
didregS0_06 = lm(FemWorkShare ~ treated + post + did, data = data_SAVE2006)
summary(didregS0_06)


didregS1_04 = lm(FemWorkShare ~ treated + post + did + median_age + median_sch +
                mean_married + mean_white + mean_black + mean_hisp + mean_child, data = data_SAVE2004) #CONTROLS
summary(didregS1_04)
confint(didregS1_04, 'did', level=0.95)
didregS1_05 = lm(FemWorkShare ~ treated + post + did + median_age + median_sch +
                mean_married + mean_white + mean_black + mean_hisp + mean_child, data = data_SAVE2005) #CONTROLS
summary(didregS1_05)
confint(didregS1_05, 'did', level=0.95)
didregS1_06 = lm(FemWorkShare ~ treated + post + did + median_age + median_sch +
                   mean_married + mean_white + mean_black + mean_hisp + mean_child, data = data_SAVE2006) #CONTROLS
summary(didregS1_06)
confint(didregS1_06, 'did', level=0.95)


#TEST FOR EXPECTATIONS
didregS1_03 = lm(FemWorkShare ~ treated + post + did + median_age + median_sch +
                   mean_married + mean_white + mean_black + mean_hisp + mean_child, data = data_SAVE2003) #CONTROLS
summary(didregS1_03)


# TRENDS VERMONT ILLINOIS
wagegapIV <- data_Collapse1 %>% filter(statefip == 50 | statefip == 17)  
wagegapIV$State <- ifelse(wagegapIV$statefip == 50, "Vermont", "Illinois")

ggplot(wagegapIV, aes(year, wagegap, group = State, colour = State, theme_bw(base_size=16))) +
  geom_line() 




##TABLES

# FE-OLS BY YEAR
control <- c("Controls", "No", "Yes", "Yes", "No", "Yes", "Yes", "No", "Yes", "Yes")

stargazer(didregW0_04, didregW1_04, didregS1_04, didregW0_05, didregW1_05, didregS1_05,
          didregW0_06, didregW1_06, didregS1_06,
          title= "OLS model results",
          type = "latex", 
          dep.var.labels.include = FALSE,
          column.labels = c("GWG (2004)", "Work share (2004)", "GWG (2005)", "Work share (2005)", "GWG (2006)", "Work share (2006)"), 
          column.separate = c(2,1,2,1,2,1),
          intercept.bottom = FALSE,
          #omit = ,
          omit.stat = c("ser", "f", "adj.rsq"),   # si veut retirer R^2, F stat, residual SE, nb d'obs
          digits = 3,
          column.sep.width = "3pt",
          keep = c("Constant", "treated", "post", "did"),
          covariate.labels = c("Constant", "Illinois", "Post", "DiD"),
          add.lines = list(c(control)),
          float.env = "sidewaystable"
          )
