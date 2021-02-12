#Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. The built-in dataset esoph contains data from a case-control study in France comparing people with esophageal cancer (cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a variety of demographic and medical characteristics. The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases and controls grouped by age range (agegp).
#The dataset is available in base R and can be called with the variable name esoph:

data(esoph)
library(gtools)
library(tidyverse)
head(esoph)

#Each row contains one group of the experiment. Each group has a different combination of age, alcohol consumption, and tobacco consumption. The number of cancer cases and number of controls (individuals without cancer) are reported for each group.
nrow(esoph)
str(esoph)
all_cases<- sum(esoph$ncases)
all_cases
all_controls<- sum(esoph$ncontrols)
all_controls
tab_alcgp<- table(esoph$alcgp)
tab
prop.table(tab)
tab1_tobgp <- table(esoph$tobgp)
tab1_tobgp
prop.table(tab1)
#What is the probability that a subject in the highest alcohol consumption group is a cancer case?
highest_alcgp<- esoph %>% filter(esoph$alcgp=='120+')
highest_alcgp
highest_alcgp_total_cases <- sum(highest_alcgp$ncases)
highest_alcgp_total_cases
highest_alcgp %>%
  summarize(cases=sum(ncases), total= sum(ncontrols)+sum(ncases), probability=cases/total)
#probability that a subject in the highest alcohol consumption group is a cancer case is 40.17%

#What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
lowest_alcgps<- esoph %>% filter(esoph$alcgp=='0-39g/day')
lowest_alcgps
lowest_alcgps %>% summarize(cases_low=sum(ncases),total_low= sum(ncases)+sum(ncontrols),probability_low=cases_low/total_low)
#probability that a subject in the lowest alcohol consumption group is a cancer case is 6.53%

#Given that a person is a case, what is the probability that they smoke 10g or more a day?
all_cases
tob_cases<- esoph %>% filter(esoph$tobgp>'0-9g/day')%>% summarize(total_tob=sum(ncases))
tob_cases
pr_tob<- tob_cases/all_cases
pr_tob
#Given that a person is a case,the probability that they smoke 10g or more a day 61%

#Given that a person is a control, what is the probability that they smoke 10g or more a day?
all_controls
tob_con <- esoph %>% filter(esoph$tobgp>'0-9g/day')%>% summarize(total=sum(ncontrols))
tob_con
pr_con_tob<-tob_con/all_controls
pr_con_tob
# Given that a person is a control,the probability that they smoke 10g or more a day 46.15%

#For cases, what is the probability of being in the highest alcohol group?
pr_alcgp_total <- highest_alcgp_total_cases/all_cases
pr_alcgp_total
# For cases,the prob of being in the highest alcohol group is 22.5%

#For cases, what is the probability of being in the highest tobacco group?
table(esoph$tobgp)
highest_tob_cases<- esoph %>% filter(esoph$tobgp>='30+')%>% summarize(total_cases=sum(ncases))
highest_tob_cases
pr_tob_total<- highest_tob_cases/all_cases
pr_tob_total
#For cases, the probability of being in the highest tobacco group 15.5%

#For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
high_alc_tob<- highest_alcgp %>% filter(tobgp>='30+')
high_alc_tob <-high_alc_tob  %>% summarize(total_cases=sum(ncases))
pr_highest_alc_and_tob<-high_alc_tob/all_cases 
pr_highest_alc_and_tob
# prob of being in highest alcohol gp & highest tobacco group 5%

pr_alcgp_total ## For cases,probability of being in the highest tobacco group
pr_tob_total # For cases,probability of being in the highest alcohol group

#For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
Pr_highest_alc_or_tob_gp <-pr_alcgp_total+ pr_tob_total-pr_highest_alc_and_tob
Pr_highest_alc_or_tob_gp 
#For cases,the probability of being in the highest alcohol group or the highest tobacco group 33%

#For controls, what is the probability of being in the highest alcohol group?
highest_alcgp_total_controls <- sum(highest_alcgp$ncontrols)
highest_alcgp_total_controls
pr_alc_total_control<- highest_alcgp_total_controls/all_controls
pr_alc_total_controlhighest_alcgp_total_controls <- sum(highest_alcgp$ncontrols)
highest_alcgp_total_controls
pr_alc_total_control<- highest_alcgp_total_controls/all_controls
pr_alc_total_control
#For controls, the probability of being in the highest alcohol group is 6.87%

#How many times more likely are cases than controls to be in the highest alcohol group?
pr_alcgp_total  #cases to be in the highest alcgp
pr_alc_total #controls to be in the highest alcgp
pr_alcgp_total/pr_alc_total
#Cases are 3 times times more likely than controls to be in the highest alcohol group

#For controls, what is the probability of being in the highest tobacco group?
highest_tobgp <- sum(highest_tobgp$ncontrols)
highest_tobgp
pr_tob_total<- highest_tobgp/all_controls
pr_tob_total
#For controls, the probability of being in the highest tobacco group is 8.41%

#For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?
high_alc_tob<- highest_alcgp %>% filter(tobgp>='30+')
high_alc_tob
high_alc_tob_con <-high_alc_tob %>% summarize(total_controls=sum(ncontrols))
high_alc_tob_con
pr_high_alc_tob_con<-high_alc_tob_con/all_controls
pr_high_alc_tob_con
#For controls,the probability of being in the highest alc group and the highest tob group is 1.33%

#For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?
pr_high_alc_or_tob<- pr_alc_total_control+pr_tob_total-pr_high_alc_tob_con
pr_high_alc_or_tob
#For controls,the probability of being in the highest alc group or the highest tob group is 13.94%

#How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?
Pr_highest_alc_or_tob_gp /pr_high_alc_or_tob
#Cases are two times more likely than controls to be in the highest alcohol group or the highest tobacco group

