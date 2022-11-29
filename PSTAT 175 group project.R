
#You guys can take a look at the code and let me know if there are any changes or suggestions you think that will make the project better
#So for the base model  I decided to see the effect of age and years on transplant just as a place to start. And then continued the project from there.


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

#mscore, reject, surgery


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



#Check proportional hazards assumption

model1.3.coxzph <- cox.zph(model1.3)

model1.3.coxzph



#It is found that the proportional hazards assumption is not 
#violated, as the P-values are all above
#the 0.05 level, but to be sure, check schoenfeld residuals.

plot(model1.3.coxzph)

ggcoxzph(model1.3.coxzph)

#We can see that from each individual schoenfeld residual,
#there is no severe issue to be concerned about, as the lines
#show no extreme deviation from flat.


#4.)

summary(model1.3)

#Conclusion
#Original question:
#Testing survival against whether the subject
#had a transplant or not and then sub categorizing covariates
#into internal or external factors, with external being
#things like age and year and internal being surgery and
#mscore(mismatch score), and rejection(although not included
#in model)

#From our summary function on our coxph model, we can see
#that in comparison to not getting the transplant:

#Firstly, from the column marked z, gives a value
#of -1.652, which indicates
#that receiving a transplant is statistically significant.

#The regression coefficient for getting the transplant, being
#-0.80407, indicates that receiving a transplant has a 
#lower risk of death than not receiving one.

#The hazard ratio indicates that receiving 
#the transplant reduces the hazard by a factor of 
#0.446013, or 55.3987%, which is an indication 
#of being positive for receiving a transplant versus not.

#The confidence interval for this 
#hazard ratio is (.1712, 1.162)

#Then, in terms of whether internal or external factors
#are of more significance, we can see that:

#looking at the regression coefficients, the highest value
#is the mscore(0.25945), meaning that having a heart mismatch
#is a sign of a higher risk of death.
#The next highest value is age(.008034), which is as well
#associated with a higher risk of death, but not nearly
#as much as having a heart mismatch.
#Of the other covariates considered, their regression
#coefficients are negative, meaning that these values are 
#associated with having a lower risk of death,
#with year being at (-0.052584) and surgery being(-1.139818).
#This means that having prior bypass surgery is associated with
#the best prognosis for having a lower risk of death, in terms
#of the regression coefficients.
#In terms of our original question, external factors do not
#affect survivability as much as internal factors, as they have
#the highest and lowest values for the regression coefficients.

#Looking at the hazard ratios, the highest value is
#mscore(1.296217), which is followed by age(1.008067).
#Like before, this means that these covariates are 
#associated with an increased and higher risk of death, and
#mscore being more so.
#As opposed to year(0.948775) and surgery(0.31987), with both
#being associated with less and smaller risk of death,
#and like before, surgery being the lowest value meaning
#it is the most positively associated with a lower risk of 
#death.

#The hazard ratios for each of these covariates is as followed:
#mscore
#(.7608, 2.208)
#age
#(.9587, 1.060)
#year
#(.7000, 1.286)
#surgery
#(.1014, 1.010)




#5.)

#Let us now consider a gap model.

heart.gap <- coxph(Surv(heart$stop-heart$start,heart$event)~
                     heart$transplant)
summary(heart.gap)

#Lets now order the considered covariates
#in the order of highest to lowest association
#with risk of death

heart.gap1 <- coxph(Surv(heart$stop-heart$start,heart$event)~
                      heart$transplant+jasa.mscore1 + heart.age2
                    +heart$year+jasa.surgery1)
AIC(heart.gap1)

summary(heart.gap1)

#This model yields similar results.



Footer
© 2022 GitHub, Inc.
Footer navigation
