library(survival)
library(survminer)
library(KMsurv)
library(ggplot2)
data(bfeed)


### Modèles retenus (cf selection_variables.R)

cox_fit=coxph(Surv(duration, delta)~race+smoke+ybirth+yschool, data=bfeed, method="efron")
cox_fit2=coxph(Surv(duration, delta)~factor(race)+factor(smoke)+ybirth+yschool, data=bfeed, method="efron")


############################### STRATIFICATITON ###############################

### Partitionnement en groupes de la covariable "yschool"
strata.yschool=cut(bfeed$yschool,breaks=c(2,14,19))
cox_strata_ysc=coxph(Surv(duration,delta)~factor(race)+factor(smoke)+ybirth+strata(strata.yschool), data=bfeed, method='efron')
summary(coxph(Surv(duration,delta)~factor(race)+factor(smoke)+ybirth+strata.yschool, data=bfeed, method='efron'))
# h0 propre à chaque strate
ggsurvplot(survfit(cox_strata_ysc, data=bfeed), data=bfeed, xlab="Durées d'allaitement (en semaines)", ylab="Probabilité de survie")

# Test du logrank
bfeed$strata.yschool=strata.yschool
survdiff(Surv(duration, delta)~strata.yschool, data=bfeed)
# H0 presque rejetee 

# Estimation du modele de Cox
cox_fit2
cox_strata_ysc


### Partitionnement du temps 
## Pour la covariable "smoke"
borne_1=quantile(bfeed$duration)["50%"]
b=seq(10,48,2)
loglik_max=NULL
for(i in seq(1,length(b))){
  bfeed.split = survSplit(Surv(duration, delta)~., data=bfeed, cut=c(borne_1,b[i]), episode="tgroup")
  cox_strata=coxph(Surv(duration,delta)~smoke:strata(tgroup)+factor(race)+ybirth+yschool, data=bfeed.split, method='efron')
  loglik_max[i]=cox_strata$loglik[2]
}
borne_2=b[which.max(loglik_max)]

bfeed.split = survSplit(Surv(duration, delta)~., data=bfeed, cut=c(borne_1, borne_2), episode="tgroup")
cox_strata_smoke=coxph(Surv(duration,delta)~smoke:strata(tgroup)+factor(race)+ybirth+yschool, data=bfeed.split, method='efron')
plot(cox.zph(cox_fit, transform="identity"), var="smoke", xlab="Temps (en semaines)",lwd=3, resid=T, ylim=c(-1,1))
x_smoke_st=c(0,borne_1,borne_2,192)
y_smoke_st=c(cox_strata_smoke[["coefficients"]][["smoke:strata(tgroup)tgroup=1"]], 
             cox_strata_smoke[["coefficients"]][["smoke:strata(tgroup)tgroup=2"]],
             cox_strata_smoke[["coefficients"]][["smoke:strata(tgroup)tgroup=3"]],
             cox_strata_smoke[["coefficients"]][["smoke:strata(tgroup)tgroup=3"]])
points(x_smoke_st,y_smoke_st, type='s', col='red')
abline(h=cox_fit$coefficients["smoke"], lwd=1, col="blue")
legend("bottomright", legend=c(expression(beta(t)), expression(hat(beta)~"stratifié"), expression(hat(beta))), 
       lty=c(1,1,1), col=c("black", "red", "blue"), bty="n")

summary(cox_strata_smoke)


## Pour la covariable "race"
borne_1_race=quantile(bfeed$duration)["50%"]
b=seq(10,48,2)
loglik_max=NULL
for(i in seq(1,length(b))){
  bfeed.split_race = survSplit(Surv(duration, delta)~., data=bfeed, cut=c(borne_1,b[i]), episode="tgroup")
  cox_strata=coxph(Surv(duration,delta)~race:strata(tgroup)+factor(smoke)+ybirth+yschool, data=bfeed.split_race, method='efron')
  loglik_max[i]=cox_strata$loglik[2]
}
borne_2_race=b[which.max(loglik_max)]

bfeed.split_race = survSplit(Surv(duration, delta)~., data=bfeed, cut=c(borne_1_race, borne_2_race), episode="tgroup")
cox_strata_race=coxph(Surv(duration,delta)~race:strata(tgroup)+factor(smoke)+ybirth+yschool, data=bfeed.split_race, method='efron')
plot(cox.zph(cox_fit, transform="identity"), var="race", xlab="Temps (en semaines)",lwd=3, resid=T, ylim=c(-1,1))
x_smoke_st=c(0,borne_1_race,borne_2_race,192)
y_smoke_st=c(cox_strata_race[["coefficients"]][["race:strata(tgroup)tgroup=1"]], 
             cox_strata_race[["coefficients"]][["race:strata(tgroup)tgroup=2"]],
             cox_strata_race[["coefficients"]][["race:strata(tgroup)tgroup=3"]],
             cox_strata_race[["coefficients"]][["race:strata(tgroup)tgroup=3"]])
points(x_smoke_st,y_smoke_st, type='s', col='red')
abline(h=cox_fit$coefficients["smoke"], lwd=1, col="blue")
legend("bottomright", legend=c(expression(beta(t)), expression(hat(beta)~"stratifié"), expression(hat(beta))), 
       lty=c(1,1,1), col=c("black", "red", "blue"), bty="n")

summary(cox_strata_race)



## Modèle final
cox_final = coxph(Surv(duration, delta)~ybirth+smoke:strata(tgroup)+strata.yschool+ factor(race), data=bfeed.split, method='efron' )
summary(cox_final)


