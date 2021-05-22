library(survival)
library(KMsurv)
library(MASS)
data(bfeed)


############################## Selection de variables ##############################

cox_all=coxph(Surv(duration,delta)~race+smoke+ybirth+yschool+poverty+agemth+pc3mth+alcohol, data=bfeed, method='efron')
# Criteres AIC et BIC
fit_aic=stepAIC(cox_all, k=2)
n=sum(bfeed$delta)
fit_bic=stepAIC(cox_all, k=log(n))
# -> race, smoke, ybirth, yschool

# Methode descendante 

summary(cox_all)
# p-value la plus grande "pc3mth"
cox_sel_1=coxph(Surv(duration,delta)~race+smoke+ybirth+yschool+poverty+agemth+alcohol, data=bfeed, method='efron')
summary(cox_sel_1)

# "agemth"
cox_sel_2=coxph(Surv(duration,delta)~race+smoke+ybirth+yschool+poverty+alcohol, data=bfeed, method='efron')
coxsummary(cox_sel_2)

# "alcohol"
cox_sel_3=coxph(Surv(duration,delta)~race+smoke+ybirth+yschool+poverty, data=bfeed, method='efron')
summary(cox_sel_3)
# 0.05>p-value poverty>0.01 on retrouve AIC

# "poverty"
cox_sel_4=coxph(Surv(duration,delta)~race+smoke+ybirth+yschool, data=bfeed, method='efron')
summary(cox_sel_4)
# p-values toutes < 0.01 on retrouve BIC
# Donc on choisit les 4 restantes : race, smoke, ybirth et yschool

# Modeles retenus
cox_fit=coxph(Surv(duration, delta)~race+smoke+ybirth+yschool, data=bfeed, method="efron")
cox_fit2=coxph(Surv(duration, delta)~factor(race)+factor(smoke)+ybirth+yschool, data=bfeed, method="efron")


