library(survival)
library(survminer)
library(KMsurv)
data(bfeed)


### Modèles retenus (cf selection_variables.R)

cox_fit=coxph(Surv(duration, delta)~race+smoke+ybirth+yschool, data=bfeed, method="efron")
cox_fit2=coxph(Surv(duration, delta)~factor(race)+factor(smoke)+ybirth+yschool, data=bfeed, method="efron")



############################## Verification des hypotheses ##############################

##### Validation de l'hypothese des hasards proportionnels

### Validation graphique. (fonctions de survie)

mean.bfeed = data.frame(ybirth=rep(mean(bfeed$ybirth),times=2),
                        yschool=rep(mean(bfeed$yschool),times=2),
                        race=rep(1,2),
                        smoke=c(0,1))
surv.bfeed=survfit(cox_fit2,newdata=mean.bfeed)

# Representation des durees d'allaitement des femmes en fonction de 
# leur consommation de tabac (+yschool +ybirth)
par(mfrow=c(1,2), xpd=TRUE)
plot(surv.bfeed, 
     col = c(1:2), 
     xlab = "Durées d'allaitement", 
     ylab = "S(t)")
# en echelle logarithmique
plot(surv.bfeed, 
     fun="cloglog", 
     col = c(1:2), 
     xlab = "Durée d'allaitement \n (en échelle logarithmique)", 
     ylab = "log(H(t))")


# Fonction de risque cumulee
plot(surv.bfeed, 
     fun="cumhaz", 
     col = c(1:2), 
     xlab = "Time", 
     ylab = "H(t)")
legend("topleft",legend=c("Non fumeuse","Fumeuse"), col=c(1:2), lty=c(1,1))


# Durees en fonction des annees d'etudes 
mean.bfeed2 = data.frame(ybirth=rep(mean(bfeed$ybirth),times=17),
                         race=rep(1,17),
                         smoke=rep(0,17),
                         yschool=c(3:19))
surv.bfeed2=survfit(cox_fit2, newdata=mean.bfeed2)
plot(surv.bfeed2, 
     fun="cloglog", 
     xlab = "Time (in log-scale)", 
     ylab = "log(H(t))", 
     col=c(1:17))
legend("bottomright",legend=c(3:19), col=c(1:17), lty=1)



### Test correlation des residus de Schoenfeld
# H0 : beta_j(t)=beta_j  contre  H1 : beta_j(t)=/=beta_j 
# en testant la nullite de a dans s* =at+ε ,i=1...n
r = cox.zph(cox_fit2, transform="identity")
r


### Representation des residus de Schoenfeld 

## Pour "yschool"
plot(cox.zph(cox_fit2, transform="identity"), 
     var="yschool", 
     xlab="Temps (en semaines)", 
     lwd=2, 
     resid=F, 
     ylim=c(-1,1))
abline(h=cox_fit2$coefficients["yschool"], lwd=3, col="red")
legend("topright", legend=c(expression(beta(t)), expression(hat(beta))), 
       lty=c(1,1), col=c("black", "red"), bty="n", cex=0.6)

# courbe noire : courbe de tendance lissee 
# courbe pointille : intervalle de confiance à 95% de la courbe lissee
# courbe rouge : effet estime du modele de cox
# si courbe noire est approx courbe rouge (courbe rouge dans l'intervalle de confiance) 
# alors on peut penser que le risque est constant et l'hyp est validee.

## Pour "ybirth"
plot(cox.zph(cox_fit2, transform="identity"), 
     var="ybirth", 
     xlab="Temps (en semaines)", 
     lwd=2, 
     resid=F, 
     ylim=c(-1,1))
abline(h=cox_fit2$coefficients["ybirth"], lwd=3, col="red")
legend("topright", legend=c(expression(beta(t)), expression(hat(beta))), 
       lty=c(1,1), col=c("black", "red"), bty="n", cex=0.6)

## Pour "race"
plot(cox.zph(cox_fit, transform="identity"), 
     var="race", 
     xlab="Temps", 
     lwd=3, 
     resid=F, 
     ylim=c(-1,1))
abline(h=cox_fit$coefficients["race"], lwd=3, col="red")
legend("topleft", legend=c(expression(beta(t)), expression(hat(beta))), 
       lty=c(1, 1), col=c("black", "red"), bty="n")


## Pour "smoke"
plot(cox.zph(cox_fit, transform="identity"), 
     var="smoke", 
     xlab="Temps", 
     lwd=3, 
     resid=F, 
     ylim=c(-1,1))
abline(h=cox_fit$coefficients["smoke"], lwd=3, col="red")
legend("topleft", legend=c(expression(beta(t)), expression(hat(beta))), 
       lty=c(1, 1), col=c("black", "red"), bty="n")




##### Valisation de l'hypothese de log-linearite 

### Residus de martingales 
# Modele nul
fit.bfeed0 <- coxph(Surv(duration, delta) ~ 1, data=bfeed) # method='efron' par defaut
resm <- residuals(fit.bfeed0, type="martingale") # Residus de martingale par defaut : type=c("martingale")

# Forme fonctionnelle de "yschool"
plot(bfeed$yschool, 
     resm, 
     xlab="Nombre d'années d'étude", 
     ylab="Résidus de martingale", 
     pch=1)
lines(lowess(bfeed$yschool, resm, iter=0), lwd=3, col="red")
legend("bottomleft", c("Tendance lissée des résidus \n de martingales"), col=c('red'), lty=1, cex=0.75)

# Forme fonctionnelle de "log(yschool)"
plot(log(bfeed$yschool), 
     resm, 
     xlab="log(yschool)", 
     ylab="Résidus de martingale", 
     pch=16)
lines(lowess(log(bfeed$yschool), resm, iter=0), lwd=3, col="red")


# Forme fonctionnelle de "smoke"
plot(bfeed$smoke, 
     resm,
     xlab="smoke", 
     ylab="Résidus de martingale", 
     pch=16)
lines(lowess(bfeed$smoke, resm, iter=0), lwd=3, col="red")
# INUTILE CAR SEULEMENT 2 CATEGORIES


# Forme fonctionnelle de "ybirth"
plot(bfeed$ybirth, 
     resm, 
     xlab="ybirth", 
     ylab="Résidus de martingale", 
     pch=16)
lines(lowess(bfeed$ybirth, resm, iter=0), lwd=3, col="red")


# Forme fonctionnelle de "race"
plot(bfeed$race, 
     resm, 
     xlab="race", 
     ylab="Résidus de martingale", 
     pch=16)
lines(lowess(bfeed$race, resm, iter=0), lwd=3, col="red")
# INUTILE CAR UNIQUEMENT 3 CATEGORIES

# Pour toutes les covariables le "smooth" des residus de martingales (par lowess) est une droite
# on peut alors conserver la variable sous forme continue



##### Smoothing splines

### La regression spline pour la covariable "yschool"

fit.splines_ysc = coxph(Surv(duration, delta)~pspline(yschool, df=4), data = bfeed)
summary(fit.splines_ysc)

temp_ysc = termplot(fit.splines_ysc, se = T, plot = F)
ysc = temp_ysc$yschool
centre_ysc = with(ysc, y[round(x, digits=1)==12])
ytemp_ysc = ysc$y + outer(ysc$se, c(0, -1.96, 1.96), '*')
matplot(ysc$x, 
        exp(ytemp_ysc - centre_ysc), 
        type='l', 
        lty=c(1,2,2), 
        col=2, 
        ylim=c(0,2), 
        xlab="Nombre d'années d'étude", 
        ylab="Risque relatif")
# regression lineaire
abline(lm(exp(ytemp_ysc - centre_ysc)[,1]~ysc$x), col='turquoise')
# droites pointillees pour le point (10,1)
abline(h=1, col='grey', lty=2)
abline(v=12, col='grey', lty=2)
legend('bottomright', legend=c("Spline", "Intervalle de confiance", "Droite de régression"), 
       col=c(2, 2, 'turquoise'), lty=c(1,2,1), cex=0.8)
title("Risque relatif lié au nombre \n d'années d'étude (référence : 12 ans)")


## Pour "ybirth"
fit.splines_yb = coxph(Surv(duration, delta)~pspline(ybirth, df=2), data = bfeed)
summary(fit.splines_yb)

temp_yb = termplot(fit.splines_yb, se = T, plot = F)
yb = temp_yb$ybirth
centre_yb = with(yb, y[round(x, digits=1)==78])
ytemp_yb = yb$y + outer(yb$se, c(0, -1.96, 1.96), '*')
matplot(yb$x, 
        exp(ytemp_yb - centre_yb), 
        type='l', 
        lty=c(1,2,2), 
        col=1, 
        ann=F, 
        ylim=c(-0.5,3))
abline(lm(exp(ytemp_yb - centre_yb)[,1]~yb$x), col='turquoise')





