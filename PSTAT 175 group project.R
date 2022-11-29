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

View(heart)
View(jasa)
View(jasa1)

heart.surv <- Surv(heart$start, heart$stop, heart$event)

heart.surv.m <- surv_fit(heart.surv~1, data = heart)

plot1 <- plot(heart.surv.m, conf.int = .95, col = "red")
title("Kaplan Meier Estimate")


#heart.surv2 <- Surv(heart$start, heart$stop, heart$event)~heart$transplant

#heart.surv.m2 <- surv_fit(heart.surv2, data = heart)

#plot2 <- plot(heart.surv.m2)

heart.age <- heart[,4]

heart.age2 <- heart.age + 48

heart.surv2 <- 
  Surv(heart$start, heart$stop, heart$event)~heart$transplant 
+ heart.age2 + heart$year

heart.surv.m2 <- surv_fit(heart.surv2, data = heart)

plot2 <- plot(heart.surv.m2, conf.int = .95, col = "blue")
title("Kaplan Meier Estimate")

#AIC

model1 <- coxph(heart.surv~heart.age2, data = heart)

model2 <- coxph(heart.surv~heart$transplant, data = heart)

model3 <- coxph(heart.surv~heart$year, data = heart)




heart.surv2.cox <- coxph(heart.surv2)


AIC(model1)
AIC(model2)
AIC(model3)
AIC(heart.surv2.cox)










