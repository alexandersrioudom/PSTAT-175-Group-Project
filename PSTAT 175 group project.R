#View(heart)

#external vs internal for transplant

#base model
#surv(start, stop, event)~ transplant + age + year

# See if can manipulate a recurrent event model using mscore 
#or mismatch
#(Gap model possibly, but not preferrable, doesn't make sense)

#1.) Model Fitting
#AIC Step

#2.) Chech PH assumption
#log -log plot
#coxzph
#3.) Hazard Ratios
#Conclusion(how did this change)
#95% CI



library(survival)
library(survminer)
library(dplyr)

heart[c(1:7),]
jasa[c(1:7),]
jasa1[c(1:7),]

heart.surv <- Surv(heart$start, heart$stop, heart$event)

heart.surv.m <- surv_fit(heart.surv~1, data = heart)

plot1 <- plot(heart.surv.m, conf.int = .95, col = "red")
title("Kaplan Meier Estimate")

#Base model
#Test the effects of the interesting covariates on transplant

heart.age <- heart[,4]

heart.age2 <- heart.age + 48

heart.surv2 <- 
  Surv(heart$start, heart$stop, heart$event)~heart$transplant 
+ heart.age2 + heart$year

heart.surv.m2 <- surv_fit(heart.surv2, data = heart)

plot2 <- plot(heart.surv.m2, conf.int = .95, col = "blue")
title("Kaplan Meier Estimate")

#AIC


heart.surv2.cox <- coxph(heart.surv2)

#Covariates of interests to check against base model?

#mscore, reject, surgery?


jasa$mscore

jasa.mscore1 <- jasa$mscore %>% c(rep(NA,69))

jasa.reject1 <- jasa$reject %>% c(rep(NA, 69))

jasa.surgery1 <- jasa$surgery %>% c(rep(NA,69))

model1 <- coxph(Surv(heart$start, heart$stop, heart$event)~heart$transplant 
+ heart.age2 + heart$year + jasa.mscore1)

model2 <- coxph(Surv(heart$start, heart$stop, heart$event)~heart$transplant 
+ heart.age2 + heart$year + jasa.reject1)

model3 <- coxph(Surv(heart$start, heart$stop, heart$event)~heart$transplant 
                + heart.age2 + heart$year + jasa.surgery1)

AIC(heart.surv2.cox)
AIC(model1)
AIC(model2)
AIC(model3)

#Lowest AIC is model1(230.2854)

model1.2 <- coxph(Surv(heart$start, heart$stop, heart$event)~heart$transplant 
                  + heart.age2 + heart$year + jasa.mscore1
                  + jasa.reject1)

  
model1.3 <- coxph(Surv(heart$start, heart$stop, heart$event)~heart$transplant 
                  + heart.age2 + heart$year + jasa.mscore1
                  + jasa.surgery1)

AIC(model1.2)
AIC(model1.3)

#AIC(model1.3) = 227.5847

model1.3.2 <- coxph(Surv(heart$start, heart$stop, heart$event)~heart$transplant 
                    + heart.age2 + heart$year + jasa.mscore1
                    + jasa.surgery1 + jasa.reject1)

AIC(model1.3.2)
#AIC(model1.3.2) = 228.4591

#Best fitting model with considered variables of interest
#is (model1.3)


summary(model1.3)


#CHeck proportional hazards assumption

model1.3.coxzph <- cox.zph(model1.3)

model1.3.coxzph

#plot3 <- plot(model1.3.coxzph)

#It is found that the proportional hazards assumption is not 
#violated, as the P-values are all above
#the 0.05 level.

#4.)

#Conclusion

summary(model1.3)

#Conclusion
#Original question:
#Testing survival against whether the subject
#had a transplant or not and then sub categorizing covariates
#into internal or external factors, with external being
#things like age and year and internal being surgery and
#mscore(mismatch score)

#From our summary function on our coxph model, we can see
#that in comparison to not getting the transplant,
#the regression coefficient for getting the transplant, being
#-0.80407, indicates that receiving a transplant has a 
#lower risk of death than not, for the data.





#5.)























