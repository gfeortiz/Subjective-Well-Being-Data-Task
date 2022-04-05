######################################################################
########################## DATA TASK #################################
######################################################################

###################################
#### LOADING THE REQUIRED PACKAGES
###################################

if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

if(!require(rsample)) install.packages("rsample")
library(rsample)

#################
#### Question 1
#################


###a.

ratings <- read.csv("ratings.csv")


###b. 

#unique respondents:
unique(ratings$worker) %>% length() #1,056 unique workers

#unique aspects:
unique(ratings$aspect) %>% length() #17 unique aspects


###c.

count(ratings, worker) %>% filter(n > 17) %>% nrow() #211 respondents have more than 17 answers.

#total observations:
nrow(ratings) #18,189 total observations 

#unique workers times unique workers:
1056*17 #17,952 

#need to drop:
18189-17952

#drop:
ratings_u <- ratings %>% group_by(worker, aspect) %>% filter(time==max(time)) %>% ungroup()

#dropped:
nrow(ratings)-nrow(ratings_u)

###d.

subjective_riches <- ratings %>% group_by(worker) %>% summarise(avg=mean(rating)) %>% .$avg
summary(subjective_riches)


#################
#### Question 2
#################

###a.

demographics <- read.csv("demographics.csv")

###b.

nrow(demographics) #1,056
nrow(demographics) == unique(ratings$worker) %>% length()

###c.

demographics_m <- demographics %>% left_join(
  ratings %>% group_by(worker) %>% 
    summarise(subjective_riches=mean(rating)),
  by="worker"
)


###d.

summary(lm(subjective_riches~income, data=demographics_m))

###e.

summary(lm(subjective_riches~income+age+c(age^2)+male+education+race, data=demographics_m))

###f.

#answer in report

#################
#### Question 3
#################

###a.

count(ratings_u, aspect)

ratings_h <- ratings_u %>% 
  filter(aspect%in%c("you not feeling anxious", "your health",
                     "your mental health", "your physical fitness")) %>% 
  group_by(worker) %>%
  summarise(health=mean(rating)) %>% ungroup()

demographics_h <- left_join(demographics_m, ratings_h,
          by="worker")

###b.

demographics_h %>% mutate(age_strata=factor(round(age,-1)),
                          income_strata=make_strata(income)) %>% 
  group_by(age_strata, income_strata) %>% summarise(avg_health=mean(health)) %>% 
  ggplot(aes(age_strata,avg_health, col=income_strata)) + geom_point() +
  scale_color_discrete(name = "Income", labels = c("Lower 25%", "Medium lower 25%", "Medium upper 25%", "Top 25%")) + 
  ggtitle("Relationship between subjective measures of health, income & age") +
  xlab("Age (rounded to the nearest 10)") + 
  ylab("Subjective measure of health (average)")


###c.

#answer in report
