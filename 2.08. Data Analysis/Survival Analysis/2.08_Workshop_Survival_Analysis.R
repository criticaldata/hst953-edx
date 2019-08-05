# Load required packages
list.of.packages <- c("survival", "survminer", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(survival)
library(survminer)
library(dplyr)

# Import the ovarian cancer dataset and have a look at it
data(ovarian)
glimpse(ovarian)

# Dichotomize age and change data labels
ovarian$rx <- factor(ovarian$rx, 
                     levels = c("1", "2"), 
                     labels = c("A", "B"))
ovarian$resid.ds <- factor(ovarian$resid.ds, 
                           levels = c("1", "2"), 
                           labels = c("no", "yes"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, 
                          levels = c("1", "2"), 
                          labels = c("good", "bad"))

# Data seems to be bimodal
ggplot(ovarian,aes(x=age)) + geom_histogram(binwidth=4) + labs(title="Histogram of age variable", x="Age", y="Count/Frequency")

# Therefore, let us build a categorical variable based on the natural separator (age 50)
ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv_object

fit1 <- survfit(surv_object ~ rx, data = ovarian)
summary(fit1)

# Observe the results graphically
ggsurvplot(fit1, data = ovarian, pval = TRUE)

# Examine predictive value of residual disease status
fit2 <- survfit(surv_object ~ resid.ds, data = ovarian)
ggsurvplot(fit2, data = ovarian, pval = TRUE)

# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, 
                   data = ovarian)
# Investigate the results
ggforest(fit.coxph, data = ovarian)
