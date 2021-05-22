library(KMsurv)
library(survival)
library(survminer)
library(ggplot2)
data(bfeed)


############################### Presentation des donnees ###############################

### Effectifs 

# "smoke"
table(bfeed$smoke)

# "race"
table(bfeed$race)

# "yschool"
table(bfeed$yschool)

# "ybirth"
table(bfeed$ybirth)



### Resume statistique 

# "yschool"
summary(bfeed$yschool)

# "ybirth"
summary(bfeed$ybirth)



### Histogrammes

par(mfrow=c(1,2))
# "yschool"
hist(bfeed$yschool, 
     breaks = 15, 
     col="#0099CC", 
     density=5, 
     xlab="Nombre d'années d'étude de la mère.", 
     ylab="Nombre de femmes.", 
     main="Répartition des femmes en fonction \n de leur nombre d'années d'étude.", 
     cex.main=0.9, 
     cex.lab=0.8)

# "ybirth"
hist(bfeed$ybirth, 
     breaks = 15, 
     col="#0099CC", 
     density=5, 
     xlab="Année de naissance de la mère.", 
     ylab="Nombre de femmes.", 
     main="Répartition des femmes en fonction \n de leur année de naissance.", 
     cex.main=0.9, 
     cex.lab=0.8)



### Diagrammes circulaires
library(lessR)

# "race"
race1 = bfeed$race[bfeed$race==1]
race_b = rep("Blanche", length(race1))
race2 = bfeed$race[bfeed$race==2]
race_n = rep("Noire", length(race2))
race3 = bfeed$race[bfeed$race==3]
race_a = rep("Autre", length(race3))
race_race = c(race_b,race_n,race_a)
bfeed$race_race = race_race
PieChart(race_race,
         hole=0, 
         values="%",
         values_color ="black",
         data=bfeed,
         fill=c("orange", "beige", "chocolate4"),
         main = "Proportion de femmes selon leur couleur de peau.")

# "smoke"
smoke1 = bfeed$smoke[bfeed$smoke==0]
smoke_n = rep("Non-fumeuses", length(smoke1))
smoke2 = bfeed$smoke[bfeed$smoke==1]
smoke_o = rep("Fumeuses", length(smoke2))
smoke_smoke = c(smoke_n,smoke_o)
bfeed$smoke_smoke = smoke_smoke
PieChart(smoke_smoke,
         hole=0, 
         values="%",
         values_color ="black",
         data=bfeed,
         fill=c("#FF9999", "#CCFF99"),
         main = "Proportion de femmes fumeuses ou non.")



### Estimation courbe survie (Kaplan-Meier)
KM=survfit(Surv(duration,delta)~1, data=bfeed)
ggsurvplot(KM,xlab="Durées d'allaitement (en semaines)", ylab="Probabilité de survie")



### Resume statistique de la variable de duree d'allaitement 

# Avec censures
summary(bfeed$duration)

# Sans censure
summary(bfeed$duration[bfeed$delta==1])





