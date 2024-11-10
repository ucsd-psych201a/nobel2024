#--------------------------------- Libraries -----------------------------####
# these are the packages that we need for the subsequent functions and codes:
library(tidyverse) # for data wrangling 
library(tidytext) # for text wrangling 
library(SnowballC) # for stemming text data 
library(pwr) # for power analysis 
library(gridExtra) # for plotting multiple graphs
library(powerAnalysis)
library(broom)
library(generics)
library(knitr)
library(forestmangr)
library(arsenal)
library(qwraps2)
library(generics)
library(grid)
library(psych)
library(apaTables)
library(skimr) # Compact and Flexible Summaries of Data
library(lme4)
library(emmeans)
library(car)
library(sjplot)

#### KILANDS ####

#--------------------------------- DATA UPLOAD -----------------------------####

#Set the working directory to a subfolder within the current working directory
setwd("~/On computer/1. Choice study data analysis/Data")

# select the respective raw data file and assign it to the R environment with a name:
kilands_raw <- read_csv("kilands_20201103.csv") 

# checking the data
head(kilands_raw)
summary(kilands_raw)

#### -------- OUTLIER DETECTION/REMOVAL --------####


## define bounds for exclusion: search session length

upper_bound_length_kilands <- mean(kilands_raw$`search session length (seconds)`) + 2.5 * sd(kilands_raw$`search session length (seconds)`)
upper_bound_length_kilands

length(which(kilands_raw$`search session length (seconds)` > upper_bound_length_kilands))

# There are 27 observations that are above the upper bounds = outliers for session length

## create a new data set where outliers observations are removed

kilands_no_out <-
  kilands_raw %>% 
  filter(`search session length (seconds)` <= upper_bound_length_kilands)

## define bounds for exclusion: clicks

upper_bound_clicks_kilands <- mean(kilands_raw$`number of clicks`) +
  2.5 * sd(kilands_raw$`number of clicks`)
upper_bound_clicks_kilands

length(which(kilands_no_out$`number of clicks` > upper_bound_clicks_kilands))

# There are 374 observations that are above the upper bounds = outliers for clicks

kilands_no_out1 <-
  kilands_no_out %>% 
  filter(kilands_no_out$`number of clicks` <= upper_bound_clicks_kilands)

## New dataset has 14431 observations

## Recode Segment: High Attractiveness (HA) and Low Attractiveness (LA)
kilands_no_out1$segment[kilands_no_out1$segment == "202004_gandalf_rel"] <- "HA"
kilands_no_out1$segment[kilands_no_out1$segment == "202004_kilands_shuffle7"] <- "LA"

## remove unnecessary columns
## keep: search ID (1), user (2), segment (4), platform (7), 15, 10, 11, 12, 13,
##, 14, 17, 20, 21, 22, 23, 25, 26, 18

kilands_no_out1 <- subset(kilands_no_out, select = 
    c(1, 2, 4, 7, 15, 10, 11, 17, 12, 13, 14, 20, 21, 23, 22, 25, 26, 18))

## Recode attention_click: Top and Bottom
kilands_no_out1$attention_click[kilands_no_out1$`first click rank`<=6] <- "Top"
kilands_no_out1$attention_click[kilands_no_out1$`first click rank`>6] <- "Bottom"

## Recode attention_purchase: Top and Bottom
kilands_no_out1$attention_purchase[kilands_no_out1$`first purchase rank`<=6] <- "Top"
kilands_no_out1$attention_purchase[kilands_no_out1$`first purchase rank`>6] <- "Bottom"

## Recode top_clicks and bottom_clicks and top_purch and bottom_purch 
## (contains clicks / purchases in top / bottom)
kilands_no_out1 <- kilands_no_out1 %>% 
  mutate(clicks_position = str_replace_all(`click positions`, ";", " ")) %>% 
  mutate(purchase_position = str_replace_all(`purchase positions`, ";", " ")) %>% 
  rowwise() %>% 
  mutate(top_clicks = sum(as.numeric(str_split(clicks_position, " ")[[1]])<=6, na.rm = T),
         bottom_clicks = sum(as.numeric(str_split(clicks_position, " ")[[1]])>6, na.rm = T)) %>% 
  mutate(top_purch = sum(as.numeric(str_split(purchase_position, " ")[[1]])<=6, na.rm = T),
         bottom_purch = sum(as.numeric(str_split(purchase_position, " ")[[1]])>6, na.rm = T)) %>% 
  ungroup()

kilands_no_out1$attention_click <- factor(kilands_no_out1$attention_click)
summary(kilands_no_out1$attention_click)

kilands_no_out1$attention_purchase <- factor(kilands_no_out1$attention_purchase)
summary(kilands_no_out1$attention_purchase)

skim(kilands_no_out1$attention_click)

## Create dummy binary variables carts / purch that captures whether an add-to-cart/purchase has happened
kilands_no_out1$carts[kilands_no_out1$`number of add-to-carts` >0] <- 1
kilands_no_out1$carts[kilands_no_out1$`number of add-to-carts` ==0] <- 0
kilands_no_out1$purch[kilands_no_out1$`number of purchases` >0] <- 1
kilands_no_out1$purch[kilands_no_out1$`number of purchases` == 0] <- 0

#### HYPOTHESIS TESTING ####

# * Hypotheses a: carts (Model 2) and b: purchase (Model 1) are now modeled as `Multilevel Logistic Regression`, i.e., logistic regression with random intercept per participant.
# * Hypothesis c: Products viewed (Model 3) is now modeled as `Multilevel Negative Binomial Regression`, i.e, negative binomial regression (count-model) with random intercept per participant.
# * Hypothesis d: Session Length (Model 4) is now modeled as `Multilevel Linear Regression`, i.e, just a regular linear mixed model with random intercept per participant.

#### Hypothesis a: carts ####

lmer_a <- glmer(data = kilands_no_out1, carts~segment + (1|user), family = binomial,
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))

#print regression table

summary(lmer_a)

sjPlot::tab_model(lmer_a, show.se = T, show.stat = T, show.df = T,  transform = "exp")

broom.mixed::tidy(lmer_a, conf.int = T)

#plot

emmip(lmer_a, formula = ~segment, cov.reduce = unique, CIs = T, type = "response") +
  cowplot::theme_cowplot() + labs(x = "Condition")

#### Hypothesis b: purchase ####
kilands_no_out1$num_segment <- ifelse(kilands_no_out1$segment=="HA", 1, 0)

lmer_b <- glmer(data = kilands_no_out1, purch~segment + (1|user), family = binomial,
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))

#print regression table

summary(lmer_b)

sjPlot::tab_model(lmer_b, show.se = T, show.stat = T, show.df = T, transform = "exp")

broom.mixed::tidy(lmer_b, conf.int = T)

#plot

emmip(lmer_b, formula = ~segment, cov.reduce = unique, CIs = T, type = "response") +
  cowplot::theme_cowplot() + labs(x = "Condition")


#### Hypothesis c: clicks ####

lmer_c <- glmer.nb(data = kilands_no_out1, `number of clicks`~segment + (1|user))

#print regression table

summary(lmer_c)

sjPlot::tab_model(lmer_c, show.se = T, show.stat = T, show.df = T,  transform = "exp")

broom.mixed::tidy(lmer_c, conf.int = T)

#plot

emmip(lmer_c, formula = ~segment, cov.reduce = unique, CIs = T, type = "response") +
  cowplot::theme_cowplot() + labs(x = "Condition")


#### Hypothesis d: session length ####

lmer_d <- lmer(data = kilands_no_out1, `search session length (seconds)`~segment + (1|user))

#print regression table

summary(lmer_d)

sjPlot::tab_model(lmer_d, show.se = T, show.stat = T, show.df = T)

broom.mixed::tidy(lmer_d, conf.int = T)

#plot

emmip(lmer_d, formula = ~segment, cov.reduce = unique, CIs = T, type = "response") +
  cowplot::theme_cowplot() + labs(x = "Condition")

#### CYBERPHOTO ####

#--------------------------------- DATA UPLOAD -----------------------------####

# select the respective raw data file and assign it to the R environment with a name:
cyber_raw <- read_csv("cyberfoto_20210121.csv") 

# checking the data
head(cyber_raw)
summary(cyber_raw)

#### -------- OUTLIER DETECTION/REMOVAL --------####


## define bounds for exclusion: search session length

upper_bound_length_cyber <- mean(cyber_raw$`search session length (seconds)`) + 2.5 * sd(cyber_raw$`search session length (seconds)`)
upper_bound_length_cyber

length(which(cyber_raw$`search session length (seconds)` > upper_bound_length_cyber))

# There are 949 observations that are above the upper bounds = outliers for session length

## create a new data set where outliers observations are removed

cyber_no_out <-
  cyber_raw %>% 
  filter(`search session length (seconds)` <= upper_bound_length_cyber)

## define bounds for exclusion: clicks

upper_bound_clicks_cyber <- mean(cyber_raw$`number of clicks`) +
  2.5 * sd(cyber_raw$`number of clicks`)
upper_bound_clicks_cyber

length(which(cyber_no_out$`number of clicks` > upper_bound_clicks_cyber))

# There are 406 observations that are above the upper bounds = outliers for clicks

cyber_no_out1 <-
  cyber_no_out %>% 
  filter(cyber_no_out$`number of clicks` <= upper_bound_clicks_cyber)

## New dataset has 36056 observations

## Recode Segment: High Attractiveness (HA) and Low Attractiveness (LA)
cyber_no_out1$segment[cyber_no_out1$segment == "202004_gandalf_rel"] <- "HA"
cyber_no_out1$segment[cyber_no_out1$segment == "202004_kilands_shuffle7"] <- "LA"

## remove unnecessary columns
## keep: search ID (1), user (2), segment (4), platform (7), 15, 10, 11, 12, 13,
##, 14, 17, 20, 21, 22, 23, 25, 26, 18

cyber_no_out1 <- subset(cyber_no_out, select = 
                            c(1, 2, 4, 7, 15, 11, 17, 12, 13, 14, 20, 21, 23, 22, 18))

## Recode attention_click: Top and Bottom
cyber_no_out1$attention_click[cyber_no_out1$`first click rank`<=6] <- "Top"
cyber_no_out1$attention_click[cyber_no_out1$`first click rank`>6] <- "Bottom"

## Recode attention_purchase: Top and Bottom
cyber_no_out1$attention_purchase[cyber_no_out1$`first purchase rank`<=6] <- "Top"
cyber_no_out1$attention_purchase[cyber_no_out1$`first purchase rank`>6] <- "Bottom"

## Recode top_clicks and bottom_clicks and top_purch and bottom_purch 
## (contains clicks / purchases in top / bottom)
cyber_no_out1 <- cyber_no_out1 %>% 
  mutate(clicks_position = str_replace_all(`click positions`, ";", " ")) %>% 
  mutate(purchase_position = str_replace_all(`purchase positions`, ";", " ")) %>% 
  rowwise() %>% 
  mutate(top_clicks = sum(as.numeric(str_split(clicks_position, " ")[[1]])<=6, na.rm = T),
         bottom_clicks = sum(as.numeric(str_split(clicks_position, " ")[[1]])>6, na.rm = T)) %>% 
  mutate(top_purch = sum(as.numeric(str_split(purchase_position, " ")[[1]])<=6, na.rm = T),
         bottom_purch = sum(as.numeric(str_split(purchase_position, " ")[[1]])>6, na.rm = T)) %>% 
  ungroup()

cyber_no_out1$attention_click <- factor(cyber_no_out1$attention_click)
summary(cyber_no_out1$attention_click)

cyber_no_out1$attention_purchase <- factor(cyber_no_out1$attention_purchase)
summary(cyber_no_out1$attention_purchase)

skim(cyber_no_out1$attention_click)

## Create dummy binary variables purch that captures whether an add-to-cart/purchase has happened
cyber_no_out1$purch[cyber_no_out1$`number of purchases` >0] <- 1
cyber_no_out1$purch[cyber_no_out1$`number of purchases` == 0] <- 0

#### HYPOTHESIS TESTING ####

# * Hypotheses b: purchase (Model 1) are now modeled as `Multilevel Logistic Regression`, i.e., logistic regression with random intercept per participant.
# * Hypothesis c: Products viewed (Model 3) is now modeled as `Multilevel Negative Binomial Regression`, i.e, negative binomial regression (count-model) with random intercept per participant.
# * Hypothesis d: Session Length (Model 4) is now modeled as `Multilevel Linear Regression`, i.e, just a regular linear mixed model with random intercept per participant.

#### Hypothesis b: purchase ####
cyber_no_out1$num_segment <- ifelse(cyber_no_out1$segment=="HA", 1, 0)

lmer_b2 <- glmer(data = cyber_no_out1, purch~segment + (1|user), family = binomial,
                control=glmerControl(optimizer="bobyqa",
                                     optCtrl=list(maxfun=2e5)))

#print regression table

summary(lmer_b2)

sjPlot::tab_model(lmer_b2, show.se = T, show.stat = T, show.df = T, transform = "exp")

broom.mixed::tidy(lmer_b2, conf.int = T)

#plot

emmip(lmer_b2, formula = ~segment, cov.reduce = unique, CIs = T, type = "response") +
  cowplot::theme_cowplot() + labs(x = "Condition")

#### Hypothesis c: clicks ####

lmer_c2 <- glmer.nb(data = cyber_no_out1, `number of clicks`~segment + (1|user))

#print regression table

summary(lmer_c2)

sjPlot::tab_model(lmer_c2, show.se = T, show.stat = T, show.df = T,  transform = "exp")

broom.mixed::tidy(lmer_c2, conf.int = T)

#plot

emmip(lmer_c2, formula = ~segment, cov.reduce = unique, CIs = T, type = "response") +
  cowplot::theme_cowplot() + labs(x = "Condition")


#### Hypothesis d: session length ####

lmer_d2 <- lmer(data = cyber_no_out1, `search session length (seconds)`~segment + (1|user))

#print regression table

summary(lmer_d2)

sjPlot::tab_model(lmer_d2, show.se = T, show.stat = T, show.df = T)

broom.mixed::tidy(lmer_d2, conf.int = T)

#plot

emmip(lmer_d2, formula = ~segment, cov.reduce = unique, CIs = T, type = "response") +
  cowplot::theme_cowplot() + labs(x = "Condition")