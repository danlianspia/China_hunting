
#setwd("/Users/liangdan/Documents/Research/RStudio/ChinaHunting/EcoSoci1009")

library(ggplot2)
library(glmmTMB)
library(ggeffects)
library(MuMIn)
library(performance)
library(fitdistrplus)
library(bbmle)
library(ggpubr)
library(DHARMa)

Hunting = read.csv("data_eco_soci.csv", header = T, row.names = 1)


############################################################
#############Number of court verdicts#######################
############################################################

#Vertebrates: combining all four taxa together
#Choose the best distribution from six distributions (nb1, nb2, poisson, and with and without zero-inflation) according to AICc values

# Vertebrates
###Negative binomial mixed model woth glmmTMB
md.vert.nb1 <- glmmTMB(verdi.vert ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage was excluded because of its positive correlation with species richeness 
                         #scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was excluded because of its negative correlation with species richness
                         #scale(Latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_vert) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = nbinom1(link = "log"))

md.vert.zinb1 <- glmmTMB(verdi.vert ~ 
                           # law enforecement: number of forest police per unite of area
                           scale(forest_police_pa) + 
                           #GDP per capita 
                           scale(gdp_pc) +
                           #Population density
                           scale(pop_dens) + 
                           #Artificial habitat coverage
                           scale(artificial) + 
                           #Forest coverage was excluded because of its positive correlation with species richeness 
                           #scale(forest) + 
                           #Market inaccessibility: travel time to urban markets
                           scale(market_access)+ 
                           #Latitude was excluded because of its negative correlation with species richness
                           #scale(Latitude) + 
                           #Road density was excluded because of its positive correlation with artificial habitat cover
                           #scale(road_dens)+ 
                           #Elevation was excluded because of it negative correlation with market inaccessibility
                           #scale(Elevation)+
                           scale(div_vert) +
                           offset(log(area)) +
                           (1|region),
                         data = Hunting,
                         ziformula = ~1,
                         family = nbinom1(link = "log"))

md.vert.nb2 <- glmmTMB(verdi.vert ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage was excluded because of its positive correlation with species richeness 
                         #scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was excluded because of its negative correlation with species richness
                         #scale(Latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_vert) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = nbinom2(link = "log"))

md.vert.zinb2 <- glmmTMB(verdi.vert ~ 
                           # law enforecement: number of forest police per unite of area
                           scale(forest_police_pa) + 
                           #GDP per capita 
                           scale(gdp_pc) +
                           #Population density
                           scale(pop_dens) + 
                           #Artificial habitat coverage
                           scale(artificial) + 
                           #Forest coverage was excluded because of its positive correlation with species richeness 
                           #scale(forest) + 
                           #Market inaccessibility: travel time to urban markets
                           scale(market_access)+ 
                           #Latitude was excluded because of its negative correlation with species richness
                           #scale(Latitude) + 
                           #Road density was excluded because of its positive correlation with artificial habitat cover
                           #scale(road_dens)+ 
                           #Elevation was excluded because of it negative correlation with market inaccessibility
                           #scale(Elevation)+
                           scale(div_vert) +
                           offset(log(area)) +
                           (1|region),
                         data = Hunting,
                         ziformula = ~1,
                         family = nbinom2(link = "log"))

md.vert.poi <- glmmTMB(verdi.vert ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage was excluded because of its positive correlation with species richeness 
                         #scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was excluded because of its negative correlation with species richness
                         #scale(Latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_vert) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = poisson(link = "log"))

md.vert.zipoi <- glmmTMB(verdi.vert ~ 
                           # law enforecement: number of forest police per unite of area
                           scale(forest_police_pa) + 
                           #GDP per capita 
                           scale(gdp_pc) +
                           #Population density
                           scale(pop_dens) + 
                           #Artificial habitat coverage
                           scale(artificial) + 
                           #Forest coverage was excluded because of its positive correlation with species richeness 
                           #scale(forest) + 
                           #Market inaccessibility: travel time to urban markets
                           scale(market_access)+ 
                           #Latitude was excluded because of its negative correlation with species richness
                           #scale(Latitude) + 
                           #Road density was excluded because of its positive correlation with artificial habitat cover
                           #scale(road_dens)+ 
                           #Elevation was excluded because of it negative correlation with market inaccessibility
                           #scale(Elevation)+
                           scale(div_vert) +
                           offset(log(area)) +
                           (1|region),
                         data = Hunting,
                         ziformula = ~1,
                         family = poisson(link = "log"))

## use AICc 
AICctab(md.vert.nb1, md.vert.nb2, md.vert.poi,md.vert.zinb1,md.vert.zinb2, md.vert.zipoi, base=T, weights=T)
#md.vert.nb2 has the smallest AICc value.

summary(md.vert.nb2)


ms.vert.nb2 <- dredge(md.vert.nb2, rank = "AICc", fixed = "cond(offset(log(area)))")
ms.vert.nb2
ma.vert.nb2 <- model.avg(ms.vert.nb2, subset = delta<=2)
importance(ma.vert.nb2)
summary(ma.vert.nb2)

######## run model diagnostics using the DHARMa package
## global model
summary(md.vert.nb2)

#random effects
rr <-ranef(md.vert.nb2)
print(rr, simplify=T)

windows(8,4)
simoutDHARMa.md.vert.nb2 <- simulateResiduals(fittedModel = md.vert.nb2, n=10000, plot = T)


## top model ## 23456
md.vert.nb2.23456 <- glmmTMB(verdi.vert ~ 
                               scale(forest_police_pa) + #3
                               scale(pop_dens) + #5
                               scale(market_access)+ #4
                               scale(div_vert) + #2
                               offset(log(area)) + #6
                               (1|region),
                             data = Hunting,
                             family = nbinom2(link = "log"))


summary(md.vert.nb2.23456)						
windows(8,4)

#top
simoutDHARMa.md.vert.nb2.23456 <- simulateResiduals(fittedModel = md.vert.nb2.23456, n=10000, plot = T)

#random effects
rr <-ranef(md.vert.nb2.23456)
print(rr, simplify=T)


## amphibians

#model1 nb1 
md.amp.nb1 <- glmmTMB(verdi.amph ~ # law enforecement: number of forest police per unite of area
                        scale(forest_police_pa) + 
                        #GDP per capita 
                        scale(gdp_pc) +
                        #Population density
                        scale(pop_dens) + 
                        #Artificial habitat coverage
                        scale(artificial) + 
                        #Forest coverage was excluded because of its positive correlation with species richeness 
                        #scale(forest) + 
                        #Market inaccessibility: travel time to urban markets
                        scale(market_access)+ 
                        #Latitude was excluded because of its negative correlation with amphibians richness
                        #scale(Latitude) + 
                        #Road density was excluded because of its positive correlation with artificial habitat cover
                        #scale(road_dens)+ 
                        #Elevation was excluded because of it negative correlation with market inaccessibility
                        #scale(Elevation)+
                        scale(div_amph) +
                        offset(log(area)) +
                        (1|region),
                      data = Hunting,
                      family = nbinom1(link = "log"))



#model2 zero-inlation nb1

md.amp.zinb1 <- glmmTMB(verdi.amph ~ # law enforecement: number of forest police per unite of area
                          scale(forest_police_pa) + 
                          #GDP per capita 
                          scale(gdp_pc) +
                          #Population density
                          scale(pop_dens) + 
                          #Artificial habitat coverage
                          scale(artificial) + 
                          #Forest coverage was excluded because of its positive correlation with species richeness 
                          #scale(forest) + 
                          #Market inaccessibility: travel time to urban markets
                          scale(market_access)+ 
                          #Latitude was excluded because of its negative correlation with amphibians richness
                          #scale(Latitude) + 
                          #Road density was excluded because of its positive correlation with artificial habitat cover
                          #scale(road_dens)+ 
                          #Elevation was excluded because of it negative correlation with market inaccessibility
                          #scale(Elevation)+
                          scale(div_amph) +
                          offset(log(area)) +
                          (1|region),
                        data = Hunting,
                        ziformula = ~1,
                        family = nbinom1(link = "log"))

summary(md.amp.zinb1)

#model3 nb2
md.amp.nb2 <- glmmTMB(verdi.amph ~ # law enforecement: number of forest police per unite of area
                        scale(forest_police_pa) + 
                        #GDP per capita 
                        scale(gdp_pc) +
                        #Population density
                        scale(pop_dens) + 
                        #Artificial habitat coverage
                        scale(artificial) + 
                        #Forest coverage was excluded because of its positive correlation with species richeness 
                        #scale(forest) + 
                        #Market inaccessibility: travel time to urban markets
                        scale(market_access)+ 
                        #Latitude was excluded because of its negative correlation with amphibians richness
                        #scale(Latitude) + 
                        #Road density was excluded because of its positive correlation with artificial habitat cover
                        #scale(road_dens)+ 
                        #Elevation was excluded because of it negative correlation with market inaccessibility
                        #scale(Elevation)+
                        scale(div_amph) +
                        offset(log(area)) +
                        (1|region),
                      data = Hunting,
                      family = nbinom2(link = "log"))
summary(md.amp.nb2)


#model4 zero-inflation nb2

md.amp.zinb2 <- glmmTMB(verdi.amph ~ # law enforecement: number of forest police per unite of area
                          scale(forest_police_pa) + 
                          #GDP per capita 
                          scale(gdp_pc) +
                          #Population density
                          scale(pop_dens) + 
                          #Artificial habitat coverage
                          scale(artificial) + 
                          #Forest coverage was excluded because of its positive correlation with species richeness 
                          #scale(forest) + 
                          #Market inaccessibility: travel time to urban markets
                          scale(market_access)+ 
                          #Latitude was excluded because of its negative correlation with amphibians richness
                          #scale(Latitude) + 
                          #Road density was excluded because of its positive correlation with artificial habitat cover
                          #scale(road_dens)+ 
                          #Elevation was excluded because of it negative correlation with market inaccessibility
                          #scale(Elevation)+
                          scale(div_amph) +
                          offset(log(area)) +
                          (1|region),
                        data = Hunting,
                        ziformula = ~1,
                        family = nbinom2(link = "log"))

summary(md.amp.zinb2)


#model5 poisson
md.amp.poi <- glmmTMB(verdi.amph ~ # law enforecement: number of forest police per unite of area
                        scale(forest_police_pa) + 
                        #GDP per capita 
                        scale(gdp_pc) +
                        #Population density
                        scale(pop_dens) + 
                        #Artificial habitat coverage
                        scale(artificial) + 
                        #Forest coverage was excluded because of its positive correlation with species richeness 
                        #scale(forest) + 
                        #Market inaccessibility: travel time to urban markets
                        scale(market_access)+ 
                        #Latitude was excluded because of its negative correlation with amphibians richness
                        #scale(Latitude) + 
                        #Road density was excluded because of its positive correlation with artificial habitat cover
                        #scale(road_dens)+ 
                        #Elevation was excluded because of it negative correlation with market inaccessibility
                        #scale(Elevation)+
                        scale(div_amph) +
                        offset(log(area)) +
                        (1|region),
                      data = Hunting,
                      family = poisson(link = "log"))
summary(md.amp.poi)


#model6 zero-inflation poisson

md.amp.zipoi <- glmmTMB(verdi.amph ~ # law enforecement: number of forest police per unite of area
                          scale(forest_police_pa) + 
                          #GDP per capita 
                          scale(gdp_pc) +
                          #Population density
                          scale(pop_dens) + 
                          #Artificial habitat coverage
                          scale(artificial) + 
                          #Forest coverage was excluded because of its positive correlation with species richeness 
                          #scale(forest) + 
                          #Market inaccessibility: travel time to urban markets
                          scale(market_access)+ 
                          #Latitude was excluded because of its negative correlation with amphibians richness
                          #scale(Latitude) + 
                          #Road density was excluded because of its positive correlation with artificial habitat cover
                          #scale(road_dens)+ 
                          #Elevation was excluded because of it negative correlation with market inaccessibility
                          #scale(Elevation)+
                          scale(div_amph) +
                          offset(log(area)) +
                          (1|region),
                        data = Hunting,
                        ziformula = ~1,
                        family = poisson(link = "log"))

summary(md.amp.zipoi)

## use AICc 
AICctab(md.amp.nb1, md.amp.nb2, md.amp.poi,md.amp.zinb1,md.amp.zinb2, md.amp.zipoi, base=T, weights=T)
#md.amp.zinb2 has the smallest AIC value.

summary(md.amp.nb2)

ms.amp <- dredge(md.amp.nb2, rank = "AICc", fixed = "cond(offset(log(area)))")
print(ms.amp)
ma.amp <- model.avg(ms.amp, subset = delta<=2)
importance(ma.amp)
summary(ma.amp)

######## run model diagnostics using the DHARMa package
## global model is the top model
summary(md.amp.nb2)

#random effects
rr <-ranef(md.amp.nb2)
print(rr, simplify=T)

windows(8,4)
simoutDHARMa.md.amp.nb2 <- simulateResiduals(fittedModel = md.amp.nb2, n=10000, plot = T)


## top model ## 23456
## global model is the top model

## reptiles

#model1 nb1 
md.rept.nb1 <- glmmTMB(verdi.rept ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage 
                         scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was excluded because of its negative correlation with amphibians richness
                         #scale(Latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_rept) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = nbinom1(link = "log"))

md.rept.nb1

#model2 zero-inlation nb1

md.rept.zinb1 <- glmmTMB(verdi.rept ~ 
                           # law enforecement: number of forest police per unite of area
                           scale(forest_police_pa) + 
                           #GDP per capita 
                           scale(gdp_pc) +
                           #Population density
                           scale(pop_dens) + 
                           #Artificial habitat coverage
                           scale(artificial) + 
                           #Forest coverage 
                           scale(forest) + 
                           #Market inaccessibility: travel time to urban markets
                           scale(market_access)+ 
                           #Latitude was excluded because of its negative correlation with amphibians richness
                           #scale(Latitude) + 
                           #Road density was excluded because of its positive correlation with artificial habitat cover
                           #scale(road_dens)+ 
                           #Elevation was excluded because of it negative correlation with market inaccessibility
                           #scale(Elevation)+
                           scale(div_rept) +
                           offset(log(area)) +
                           (1|region),
                         data = Hunting,
                         ziformula = ~1,
                         family = nbinom1(link = "log"))

summary(md.rept.zinb1)

#model3 nb2
md.rept.nb2 <- glmmTMB(verdi.rept ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage 
                         scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was excluded because of its negative correlation with amphibians richness
                         #scale(Latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_rept) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = nbinom2(link = "log"))
summary(md.rept.nb2)


#model4 zero-inflation nb2

md.rept.zinb2 <- glmmTMB(verdi.rept ~ 
                           # law enforecement: number of forest police per unite of area
                           scale(forest_police_pa) + 
                           #GDP per capita 
                           scale(gdp_pc) +
                           #Population density
                           scale(pop_dens) + 
                           #Artificial habitat coverage
                           scale(artificial) + 
                           #Forest coverage 
                           scale(forest) + 
                           #Market inaccessibility: travel time to urban markets
                           scale(market_access)+ 
                           #Latitude was excluded because of its negative correlation with amphibians richness
                           #scale(Latitude) + 
                           #Road density was excluded because of its positive correlation with artificial habitat cover
                           #scale(road_dens)+ 
                           #Elevation was excluded because of it negative correlation with market inaccessibility
                           #scale(Elevation)+
                           scale(div_rept) +
                           offset(log(area)) +
                           (1|region),
                         data = Hunting,
                         ziformula = ~1,
                         family = nbinom2(link = "log"))

summary(md.rept.zinb2)


#model5 poisson
md.rept.poi <- glmmTMB(verdi.rept ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage 
                         scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was excluded because of its negative correlation with amphibians richness
                         #scale(Latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_rept) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = poisson(link = "log"))
summary(md.rept.poi)


#model6 zero-inflation poisson

md.rept.zipoi <- glmmTMB(verdi.rept ~ 
                           # law enforecement: number of forest police per unite of area
                           scale(forest_police_pa) + 
                           #GDP per capita 
                           scale(gdp_pc) +
                           #Population density
                           scale(pop_dens) + 
                           #Artificial habitat coverage
                           scale(artificial) + 
                           #Forest coverage 
                           scale(forest) + 
                           #Market inaccessibility: travel time to urban markets
                           scale(market_access)+ 
                           #Latitude was excluded because of its negative correlation with amphibians richness
                           #scale(Latitude) + 
                           #Road density was excluded because of its positive correlation with artificial habitat cover
                           #scale(road_dens)+ 
                           #Elevation was excluded because of it negative correlation with market inaccessibility
                           #scale(Elevation)+
                           scale(div_rept) +
                           offset(log(area)) +
                           (1|region),
                         data = Hunting,
                         ziformula = ~1,
                         family = poisson(link = "log"))

summary(md.rept.zipoi)

## use AICc 
AICctab(md.rept.nb1, md.rept.nb2, md.rept.poi,md.rept.zinb1,md.rept.zinb2, md.rept.zipoi, base=T, weights=T)
#md.amp.zinb2 has the smallest AIC value.
summary(md.rept.nb2)

ms.rep <- dredge(md.rept.nb2, rank = "AICc", fixed = "cond(offset(log(area)))")
ma.rep <- model.avg(ms.rep, subset = delta<=2)
importance(ma.rep)
summary(ma.rep)


######## run model diagnostics using the DHARMa package
## global model
summary(md.rept.nb2)
#random effects
rr <-ranef(md.rept.nb2)
print(rr, simplify=T)

windows(8,4)
simoutDHARMa.md.rept.nb2 <- simulateResiduals(fittedModel = md.rept.nb2, n=10000, plot = T)


print(ms.rep)
## top model ## 68
md.rept.nb2.68 <- glmmTMB(verdi.rept ~ 
                            scale(market_access)+ #6
                            offset(log(area)) + #8
                            (1|region),
                          data = Hunting,
                          family = nbinom2(link = "log"))
summary(md.rept.nb2.68)						
windows(8,4)
simoutDHARMa.md.rept.nb2.68 <- simulateResiduals(fittedModel = md.rept.nb2.68, n=10000, plot = T)
#random effects
rr <-ranef(md.rept.nb2.68)
print(rr, simplify=T)


#################

# Birds


#model1 nb1 
md.bird.nb1 <- glmmTMB(verdi.bird ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage 
                         scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was excluded because of its negative correlation with amphibians richness
                         #scale(Latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_bird) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = nbinom1(link = "log"))

md.bird.nb1

#model2 zero-inlation nb1

md.bird.zinb1 <- glmmTMB(verdi.bird ~ 
                           # law enforecement: number of forest police per unite of area
                           scale(forest_police_pa) + 
                           #GDP per capita 
                           scale(gdp_pc) +
                           #Population density
                           scale(pop_dens) + 
                           #Artificial habitat coverage
                           scale(artificial) + 
                           #Forest coverage 
                           scale(forest) + 
                           #Market inaccessibility: travel time to urban markets
                           scale(market_access)+ 
                           #Latitude was excluded because of its negative correlation with amphibians richness
                           #scale(Latitude) + 
                           #Road density was excluded because of its positive correlation with artificial habitat cover
                           #scale(road_dens)+ 
                           #Elevation was excluded because of it negative correlation with market inaccessibility
                           #scale(Elevation)+
                           scale(div_bird) +
                           offset(log(area)) +
                           (1|region),
                         data = Hunting,
                         ziformula = ~1,
                         family = nbinom1(link = "log"))

summary(md.bird.zinb1)

#model3 nb2
md.bird.nb2 <- glmmTMB(verdi.bird ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage 
                         scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was excluded because of its negative correlation with amphibians richness
                         #scale(Latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_bird) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = nbinom2(link = "log"))
summary(md.bird.nb2)


#model4 zero-inflation nb2

md.bird.zinb2 <- glmmTMB(verdi.bird ~ 
                           # law enforecement: number of forest police per unite of area
                           scale(forest_police_pa) + 
                           #GDP per capita 
                           scale(gdp_pc) +
                           #Population density
                           scale(pop_dens) + 
                           #Artificial habitat coverage
                           scale(artificial) + 
                           #Forest coverage 
                           scale(forest) + 
                           #Market inaccessibility: travel time to urban markets
                           scale(market_access)+ 
                           #Latitude was excluded because of its negative correlation with amphibians richness
                           #scale(Latitude) + 
                           #Road density was excluded because of its positive correlation with artificial habitat cover
                           #scale(road_dens)+ 
                           #Elevation was excluded because of it negative correlation with market inaccessibility
                           #scale(Elevation)+
                           scale(div_bird) +
                           offset(log(area)) +
                           (1|region),
                         data = Hunting,
                         ziformula = ~1,
                         family = nbinom2(link = "log"))

summary(md.bird.zinb2)


#model5 poisson
md.bird.poi <- glmmTMB(verdi.bird ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage 
                         scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was excluded because of its negative correlation with amphibians richness
                         #scale(Latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_bird) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = poisson(link = "log"))
summary(md.bird.poi)


#model6 zero-inflation poisson

md.bird.zipoi <- glmmTMB(verdi.bird ~ 
                           # law enforecement: number of forest police per unite of area
                           scale(forest_police_pa) + 
                           #GDP per capita 
                           scale(gdp_pc) +
                           #Population density
                           scale(pop_dens) + 
                           #Artificial habitat coverage
                           scale(artificial) + 
                           #Forest coverage 
                           scale(forest) + 
                           #Market inaccessibility: travel time to urban markets
                           scale(market_access)+ 
                           #Latitude was excluded because of its negative correlation with amphibians richness
                           #scale(Latitude) + 
                           #Road density was excluded because of its positive correlation with artificial habitat cover
                           #scale(road_dens)+ 
                           #Elevation was excluded because of it negative correlation with market inaccessibility
                           #scale(Elevation)+
                           scale(div_bird) +
                           offset(log(area)) +
                           (1|region),
                         data = Hunting,
                         ziformula = ~1,
                         family = poisson(link = "log"))

summary(md.bird.zipoi)


## use AICc 
AICctab(md.bird.nb1, md.bird.nb2, md.bird.poi,md.bird.zinb1,md.bird.zinb2, md.bird.zipoi, base=T, weights=T)
#md.amp.zinb2 has the smallest AIC value.

md.bird.nb2 <- glmmTMB(verdi.bird ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage 
                         scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was excluded because of its negative correlation with amphibians richness
                         #scale(Latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_bird) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = nbinom2(link = "log"))

summary(md.bird.nb2)

ms.bird <- dredge(md.bird.nb2, rank = "AICc", fixed = "cond(offset(log(area)))")
ma.bird <- model.avg(ms.bird, subset = delta<=2)
importance(ma.bird)
summary(ma.bird)

######## run model diagnostics using the DHARMa package
## global model
summary(md.bird.nb2)

#random effects
rr <-ranef(md.bird.nb2)
print(rr, simplify=T)

windows(8,4)
simoutDHARMa.md.bird.nb2 <- simulateResiduals(fittedModel = md.bird.nb2, n=10000, plot = T)

print(ms.bird)
## top model is global model


################3
#Mammals
#model1 nb1 
md.mamm.nb1 <- glmmTMB(verdi.mamm ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage 
                         scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was not correlated with mammal richness
                         scale(latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_mamm) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = nbinom1(link = "log"))

md.mamm.nb1

#model2 zero-inlation nb1

md.mamm.zinb1 <- glmmTMB(verdi.mamm ~ 
                           # law enforecement: number of forest police per unite of area
                           scale(forest_police_pa) + 
                           #GDP per capita 
                           scale(gdp_pc) +
                           #Population density
                           scale(pop_dens) + 
                           #Artificial habitat coverage
                           scale(artificial) + 
                           #Forest coverage 
                           scale(forest) + 
                           #Market inaccessibility: travel time to urban markets
                           scale(market_access)+ 
                           #Latitude was not correlated with mammal richness
                           scale(latitude) + 
                           #Road density was excluded because of its positive correlation with artificial habitat cover
                           #scale(road_dens)+ 
                           #Elevation was excluded because of it negative correlation with market inaccessibility
                           #scale(Elevation)+
                           scale(div_mamm) +
                           offset(log(area)) +
                           (1|region),
                         data = Hunting,
                         ziformula = ~1,
                         family = nbinom1(link = "log"))

summary(md.mamm.zinb1)

#model3 nb2
md.mamm.nb2 <- glmmTMB(verdi.mamm ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage 
                         scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was not correlated with mammal richness
                         scale(latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_mamm) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = nbinom2(link = "log"))
summary(md.mamm.nb2)


#model4 zero-inflation nb2

md.mamm.zinb2 <- glmmTMB(verdi.mamm ~ 
                           # law enforecement: number of forest police per unite of area
                           scale(forest_police_pa) + 
                           #GDP per capita 
                           scale(gdp_pc) +
                           #Population density
                           scale(pop_dens) + 
                           #Artificial habitat coverage
                           scale(artificial) + 
                           #Forest coverage 
                           scale(forest) + 
                           #Market inaccessibility: travel time to urban markets
                           scale(market_access)+ 
                           #Latitude was not correlated with mammal richness
                           scale(latitude) + 
                           #Road density was excluded because of its positive correlation with artificial habitat cover
                           #scale(road_dens)+ 
                           #Elevation was excluded because of it negative correlation with market inaccessibility
                           #scale(Elevation)+
                           scale(div_mamm) +
                           offset(log(area)) +
                           (1|region),
                         data = Hunting,
                         ziformula = ~1,
                         family = nbinom2(link = "log"))

summary(md.mamm.zinb2)


#model5 poisson
md.mamm.poi <- glmmTMB(verdi.mamm ~ 
                         # law enforecement: number of forest police per unite of area
                         scale(forest_police_pa) + 
                         #GDP per capita 
                         scale(gdp_pc) +
                         #Population density
                         scale(pop_dens) + 
                         #Artificial habitat coverage
                         scale(artificial) + 
                         #Forest coverage 
                         scale(forest) + 
                         #Market inaccessibility: travel time to urban markets
                         scale(market_access)+ 
                         #Latitude was not correlated with mammal richness
                         scale(latitude) + 
                         #Road density was excluded because of its positive correlation with artificial habitat cover
                         #scale(road_dens)+ 
                         #Elevation was excluded because of it negative correlation with market inaccessibility
                         #scale(Elevation)+
                         scale(div_mamm) +
                         offset(log(area)) +
                         (1|region),
                       data = Hunting,
                       family = poisson(link = "log"))
summary(md.mamm.poi)


#model6 zero-inflation poisson

md.mamm.zipoi <- glmmTMB(verdi.mamm ~ 
                           # law enforecement: number of forest police per unite of area
                           scale(forest_police_pa) + 
                           #GDP per capita 
                           scale(gdp_pc) +
                           #Population density
                           scale(pop_dens) + 
                           #Artificial habitat coverage
                           scale(artificial) + 
                           #Forest coverage 
                           scale(forest) + 
                           #Market inaccessibility: travel time to urban markets
                           scale(market_access)+ 
                           #Latitude was not correlated with mammal richness
                           scale(latitude) + 
                           #Road density was excluded because of its positive correlation with artificial habitat cover
                           #scale(road_dens)+ 
                           #Elevation was excluded because of it negative correlation with market inaccessibility
                           #scale(Elevation)+
                           scale(div_mamm) +
                           offset(log(area)) +
                           (1|region),
                         data = Hunting,
                         ziformula = ~1,
                         family = poisson(link = "log"))

summary(md.mamm.zipoi)


## use AICc 
AICctab(md.mamm.nb1, md.mamm.nb2, md.mamm.poi,md.mamm.zinb1,md.mamm.zinb2, md.mamm.zipoi, base=T, weights=T)
#md.amp.zinb2 has the smallest AIC value.

summary(md.mamm.nb2)

ms.mamm <- dredge(md.mamm.nb2, rank = "AICc", fixed = "cond(offset(log(area)))")
ma.mamm <- model.avg(ms.mamm, subset = delta<=2)
importance(ma.mamm)
summary(ma.mamm)

######## run model diagnostics using the DHARMa package
## global model
summary(md.mamm.nb2)

#random effects
rr <-ranef(md.mamm.nb2)
print(rr, simplify=T)

windows(8,4)
simoutDHARMa.md.mamm.nb2 <- simulateResiduals(fittedModel = md.mamm.nb2, n=10000, plot = T)

print(ms.mamm)
## top model is 123479
md.mamm.nb2.123479 <- glmmTMB(verdi.mamm ~ 
                                scale(forest_police_pa) + #4
                                scale(artificial) + #1
                                scale(forest) + #3
                                scale(market_access)+ #7
                                scale(div_mamm) + #2
                                offset(log(area)) + #9
                                (1|region),
                              data = Hunting,
                              family = nbinom2(link = "log"))
summary(md.mamm.nb2.123479)
windows(8,4)
simoutDHARMa.md.mamm.nb2.123479 <- simulateResiduals(fittedModel = md.mamm.nb2.123479, n=10000, plot = T)

#random effects
r <-ranef(md.mamm.nb2.123479)
print(r, simplify=T)




############################################################
#############Number of individuals being hunted#############
############################################################

#Vertebrates: combining all four taxa together
#Choose the best distribution from six distributions (nb1, nb2, poisson, and with and without zero-inflation) according to AIC values
#Vertebrates: nb2
#Amphibians: nb2 + zero inflation
#Reptiles: nb2 + zero inflation
#Birds: nb2
#Mammals: nb2

head(Hunting)
# Vertebrates

#model1 nb1 
md.vert.ind.nb1 <- glmmTMB(individual.vert ~ 
                             # law enforecement: number of forest police per unite of area
                             scale(forest_police_pa) + 
                             #GDP per capita 
                             scale(gdp_pc) +
                             #Population density
                             scale(pop_dens) + 
                             #Artificial habitat coverage
                             scale(artificial) + 
                             #Forest coverage was excluded because of its positive correlation with species richeness 
                             #scale(forest) + 
                             #Market inaccessibility: travel time to urban markets
                             scale(market_access)+ 
                             #Latitude was excluded because of its negative correlation with species richness
                             #scale(Latitude) + 
                             #Road density was excluded because of its positive correlation with artificial habitat cover
                             #scale(road_dens)+ 
                             #Elevation was excluded because of it negative correlation with market inaccessibility
                             #scale(Elevation)+
                             scale(div_vert) +
                             offset(log(area)) +
                             (1|region),
                           data = Hunting,
                           family = nbinom1(link = "log"))

md.vert.ind.nb1

#model2 zero-inlation nb1

md.vert.ind.zinb1 <- glmmTMB(individual.vert ~ 
                               # law enforecement: number of forest police per unite of area
                               scale(forest_police_pa) + 
                               #GDP per capita 
                               scale(gdp_pc) +
                               #Population density
                               scale(pop_dens) + 
                               #Artificial habitat coverage
                               scale(artificial) + 
                               #Forest coverage was excluded because of its positive correlation with species richeness 
                               #scale(forest) + 
                               #Market inaccessibility: travel time to urban markets
                               scale(market_access)+ 
                               #Latitude was excluded because of its negative correlation with species richness
                               #scale(Latitude) + 
                               #Road density was excluded because of its positive correlation with artificial habitat cover
                               #scale(road_dens)+ 
                               #Elevation was excluded because of it negative correlation with market inaccessibility
                               #scale(Elevation)+
                               scale(div_vert) +
                               offset(log(area)) +
                               (1|region),
                             data = Hunting,
                             ziformula = ~1,
                             family = nbinom1(link = "log"))

summary(md.vert.ind.zinb1)

#model3 nb2
md.vert.ind.nb2 <- glmmTMB(individual.vert ~ 
                             # law enforecement: number of forest police per unite of area
                             scale(forest_police_pa) + 
                             #GDP per capita 
                             scale(gdp_pc) +
                             #Population density
                             scale(pop_dens) + 
                             #Artificial habitat coverage
                             scale(artificial) + 
                             #Forest coverage was excluded because of its positive correlation with species richeness 
                             #scale(forest) + 
                             #Market inaccessibility: travel time to urban markets
                             scale(market_access)+ 
                             #Latitude was excluded because of its negative correlation with species richness
                             #scale(Latitude) + 
                             #Road density was excluded because of its positive correlation with artificial habitat cover
                             #scale(road_dens)+ 
                             #Elevation was excluded because of it negative correlation with market inaccessibility
                             #scale(Elevation)+
                             scale(div_vert) +
                             offset(log(area)) +
                             (1|region),
                           data = Hunting,
                           family = nbinom2(link = "log"))
summary(md.vert.ind.nb2)


#model4 zero-inflation nb2

md.vert.ind.zinb2 <- glmmTMB(individual.vert ~ 
                               # law enforecement: number of forest police per unite of area
                               scale(forest_police_pa) + 
                               #GDP per capita 
                               scale(gdp_pc) +
                               #Population density
                               scale(pop_dens) + 
                               #Artificial habitat coverage
                               scale(artificial) + 
                               #Forest coverage was excluded because of its positive correlation with species richeness 
                               #scale(forest) + 
                               #Market inaccessibility: travel time to urban markets
                               scale(market_access)+ 
                               #Latitude was excluded because of its negative correlation with species richness
                               #scale(Latitude) + 
                               #Road density was excluded because of its positive correlation with artificial habitat cover
                               #scale(road_dens)+ 
                               #Elevation was excluded because of it negative correlation with market inaccessibility
                               #scale(Elevation)+
                               scale(div_vert) +
                               offset(log(area)) +
                               (1|region),
                             data = Hunting,
                             ziformula = ~1,
                             family = nbinom2(link = "log"))

summary(md.vert.ind.zinb2)


#model5 poisson
md.vert.ind.poi <- glmmTMB(individual.vert ~ 
                             # law enforecement: number of forest police per unite of area
                             scale(forest_police_pa) + 
                             #GDP per capita 
                             scale(gdp_pc) +
                             #Population density
                             scale(pop_dens) + 
                             #Artificial habitat coverage
                             scale(artificial) + 
                             #Forest coverage was excluded because of its positive correlation with species richeness 
                             #scale(forest) + 
                             #Market inaccessibility: travel time to urban markets
                             scale(market_access)+ 
                             #Latitude was excluded because of its negative correlation with species richness
                             #scale(Latitude) + 
                             #Road density was excluded because of its positive correlation with artificial habitat cover
                             #scale(road_dens)+ 
                             #Elevation was excluded because of it negative correlation with market inaccessibility
                             #scale(Elevation)+
                             scale(div_vert) +
                             offset(log(area)) +
                             (1|region),
                           data = Hunting,
                           family = poisson(link = "log"))
summary(md.vert.ind.poi)


#model6 zero-inflation poisson
md.vert.ind.zipoi <- glmmTMB(individual.vert ~ 
                               # law enforecement: number of forest police per unite of area
                               scale(forest_police_pa) + 
                               #GDP per capita 
                               scale(gdp_pc) +
                               #Population density
                               scale(pop_dens) + 
                               #Artificial habitat coverage
                               scale(artificial) + 
                               #Forest coverage was excluded because of its positive correlation with species richeness 
                               #scale(forest) + 
                               #Market inaccessibility: travel time to urban markets
                               scale(market_access)+ 
                               #Latitude was excluded because of its negative correlation with species richness
                               #scale(Latitude) + 
                               #Road density was excluded because of its positive correlation with artificial habitat cover
                               #scale(road_dens)+ 
                               #Elevation was excluded because of it negative correlation with market inaccessibility
                               #scale(Elevation)+
                               scale(div_vert) +
                               offset(log(area)) +
                               (1|region),
                             data = Hunting,
                             ziformula = ~1,
                             family = poisson(link = "log"))

summary(md.vert.ind.zipoi)

## use AICc 
AICctab(md.vert.ind.nb1, md.vert.ind.nb2, md.vert.ind.poi,md.vert.ind.zinb1,md.vert.ind.zinb2, md.vert.ind.zipoi, base=T, weights=T)
#md.amp.zinb2 has the smallest AIC value.
summary(md.vert.ind.nb2)

ms.vert.ind.nb2 <- dredge(md.vert.ind.nb2, rank = "AICc", fixed = "cond(offset(log(area)))")
ma.vert.ind.nb2 <- model.avg(ms.vert.ind.nb2, subset = delta<=2)
importance(ma.vert.ind.nb2)
summary(ma.vert.ind.nb2)


######## run model diagnostics using the DHARMa package
## global model
summary(md.vert.ind.nb2)
#random effects
rr <-ranef(md.vert.ind.nb2)
print(rr, simplify=T)

windows(8,4)
simoutDHARMa.md.vert.ind.nb2 <- simulateResiduals(fittedModel = md.vert.ind.nb2, n=10000, plot = T)

print(ms.vert.ind.nb2)

## top model is 1457
md.vert.ind.nb2.1457 <- glmmTMB(individual.vert ~ 
                                  scale(gdp_pc) + #4
                                  scale(artificial) + #1
                                  scale(market_access)+ #5 
                                  offset(log(area)) + #7
                                  (1|region),
                                data = Hunting,
                                family = nbinom2(link = "log"))
summary(md.vert.ind.nb2.1457)
windows(8,4)
simoutDHARMa.md.vert.ind.nb2.1457 <- simulateResiduals(fittedModel = md.vert.ind.nb2.1457, n=10000, plot = T)

#random effects
rr <-ranef(md.vert.ind.nb2.1457)
print(rr, simplify=T)

#########################
#Aphibians

#model1 nb1 
md.amp.ind.nb1 <- glmmTMB(individual.amph ~ # law enforecement: number of forest police per unite of area
                            scale(forest_police_pa) + 
                            #GDP per capita 
                            scale(gdp_pc) +
                            #Population density
                            scale(pop_dens) + 
                            #Artificial habitat coverage
                            scale(artificial) + 
                            #Forest coverage was excluded because of its positive correlation with species richeness 
                            #scale(forest) + 
                            #Market inaccessibility: travel time to urban markets
                            scale(market_access)+ 
                            #Latitude was excluded because of its negative correlation with amphibians richness
                            #scale(Latitude) + 
                            #Road density was excluded because of its positive correlation with artificial habitat cover
                            #scale(road_dens)+ 
                            #Elevation was excluded because of it negative correlation with market inaccessibility
                            #scale(Elevation)+
                            scale(div_amph) +
                            offset(log(area)) +
                            (1|region),
                          data = Hunting,
                          family = nbinom1(link = "log"))

md.amp.ind.nb1

#model2 zero-inlation nb1

md.amp.ind.zinb1 <- glmmTMB(individual.amph ~ # law enforecement: number of forest police per unite of area
                              scale(forest_police_pa) + 
                              #GDP per capita 
                              scale(gdp_pc) +
                              #Population density
                              scale(pop_dens) + 
                              #Artificial habitat coverage
                              scale(artificial) + 
                              #Forest coverage was excluded because of its positive correlation with species richeness 
                              #scale(forest) + 
                              #Market inaccessibility: travel time to urban markets
                              scale(market_access)+ 
                              #Latitude was excluded because of its negative correlation with amphibians richness
                              #scale(Latitude) + 
                              #Road density was excluded because of its positive correlation with artificial habitat cover
                              #scale(road_dens)+ 
                              #Elevation was excluded because of it negative correlation with market inaccessibility
                              #scale(Elevation)+
                              scale(div_amph) +
                              offset(log(area)) +
                              (1|region),
                            data = Hunting,
                            ziformula = ~1,
                            family = nbinom1(link = "log"))

summary(md.amp.ind.zinb1)

#model3 nb2
md.amp.ind.nb2 <- glmmTMB(individual.amph ~ # law enforecement: number of forest police per unite of area
                            scale(forest_police_pa) + 
                            #GDP per capita 
                            scale(gdp_pc) +
                            #Population density
                            scale(pop_dens) + 
                            #Artificial habitat coverage
                            scale(artificial) + 
                            #Forest coverage was excluded because of its positive correlation with species richeness 
                            #scale(forest) + 
                            #Market inaccessibility: travel time to urban markets
                            scale(market_access)+ 
                            #Latitude was excluded because of its negative correlation with amphibians richness
                            #scale(Latitude) + 
                            #Road density was excluded because of its positive correlation with artificial habitat cover
                            #scale(road_dens)+ 
                            #Elevation was excluded because of it negative correlation with market inaccessibility
                            #scale(Elevation)+
                            scale(div_amph) +
                            offset(log(area)) +
                            (1|region),
                          data = Hunting,
                          family = nbinom2(link = "log"))
summary(md.amp.ind.nb2)


#model4 zero-inflation nb2

md.amp.ind.zinb2 <- glmmTMB(individual.amph ~ # law enforecement: number of forest police per unite of area
                              scale(forest_police_pa) + 
                              #GDP per capita 
                              scale(gdp_pc) +
                              #Population density
                              scale(pop_dens) + 
                              #Artificial habitat coverage
                              scale(artificial) + 
                              #Forest coverage was excluded because of its positive correlation with species richeness 
                              #scale(forest) + 
                              #Market inaccessibility: travel time to urban markets
                              scale(market_access)+ 
                              #Latitude was excluded because of its negative correlation with amphibians richness
                              #scale(Latitude) + 
                              #Road density was excluded because of its positive correlation with artificial habitat cover
                              #scale(road_dens)+ 
                              #Elevation was excluded because of it negative correlation with market inaccessibility
                              #scale(Elevation)+
                              scale(div_amph) +
                              offset(log(area)) +
                              (1|region),
                            data = Hunting,
                            ziformula = ~1,
                            family = nbinom2(link = "log"))

summary(md.amp.ind.zinb2)


#model5 poisson
md.amp.ind.poi <- glmmTMB(individual.amph ~ # law enforecement: number of forest police per unite of area
                            scale(forest_police_pa) + 
                            #GDP per capita 
                            scale(gdp_pc) +
                            #Population density
                            scale(pop_dens) + 
                            #Artificial habitat coverage
                            scale(artificial) + 
                            #Forest coverage was excluded because of its positive correlation with species richeness 
                            #scale(forest) + 
                            #Market inaccessibility: travel time to urban markets
                            scale(market_access)+ 
                            #Latitude was excluded because of its negative correlation with amphibians richness
                            #scale(Latitude) + 
                            #Road density was excluded because of its positive correlation with artificial habitat cover
                            #scale(road_dens)+ 
                            #Elevation was excluded because of it negative correlation with market inaccessibility
                            #scale(Elevation)+
                            scale(div_amph) +
                            offset(log(area)) +
                            (1|region),
                          data = Hunting,
                          family = poisson(link = "log"))
summary(md.amp.ind.poi)


#model6 zero-inflation poisson

md.amp.ind.zipoi <- glmmTMB(individual.amph ~ # law enforecement: number of forest police per unite of area
                              scale(forest_police_pa) + 
                              #GDP per capita 
                              scale(gdp_pc) +
                              #Population density
                              scale(pop_dens) + 
                              #Artificial habitat coverage
                              scale(artificial) + 
                              #Forest coverage was excluded because of its positive correlation with species richeness 
                              #scale(forest) + 
                              #Market inaccessibility: travel time to urban markets
                              scale(market_access)+ 
                              #Latitude was excluded because of its negative correlation with amphibians richness
                              #scale(Latitude) + 
                              #Road density was excluded because of its positive correlation with artificial habitat cover
                              #scale(road_dens)+ 
                              #Elevation was excluded because of it negative correlation with market inaccessibility
                              #scale(Elevation)+
                              scale(div_amph) +
                              offset(log(area)) +
                              (1|region),
                            data = Hunting,
                            ziformula = ~1,
                            family = poisson(link = "log"))

summary(md.amp.ind.zipoi)


## use AICc 
AICctab(md.amp.ind.nb1, md.amp.ind.nb2, md.amp.ind.poi,md.amp.ind.zinb1,md.amp.ind.zinb2, md.amp.ind.zipoi, base=T, weights=T)
#md.amp.zinb1 has the smallest AICc value.

summary(md.amp.ind.zinb1)

rr$cond$Subject
as.data.frame(rr)

ms.amp.ind <- dredge(md.amp.ind.zinb1, rank = "AICc", fixed = "cond(offset(log(area)))")
ma.amp.ind <- model.avg(ms.amp.ind, subset = delta<=2)
importance(ma.amp.ind)
summary(ma.amp.ind)

######## run model diagnostics using the DHARMa package
## global model
summary(md.amp.ind.zinb1)

#random effects
rr <-ranef(md.amp.ind.zinb1)
print(rr, simplify=T)

windows(8,4)
simoutDHARMa.md.amp.in.zinb2 <- simulateResiduals(fittedModel = md.amp.ind.zinb1, n=10000, plot = T)

print(ms.amp.ind)
## top model is 12357

md.amp.in.zinb1.12357 <- glmmTMB(individual.amph ~ 
                                   scale(forest_police_pa) + #3 
                                   scale(artificial) + #1
                                   scale(market_access)+ #5
                                   scale(div_amph) + #2
                                   offset(log(area)) + #7
                                   (1|region),
                                 data = Hunting,
                                 family = nbinom1(link = "log"),
                                 ziformula = ~1)
summary(md.amp.in.zinb1.12357)

windows(8,4)
simoutDHARMa.md.amp.in.zinb2.12357 <- simulateResiduals(fittedModel = md.amp.in.zinb1.12357, n=10000, plot = T)

#random effects
r <-ranef(md.amp.in.zinb1.12357)

print(r, simplify=T)


#Reptiles
#model1 nb1 
md.rept.ind.nb1 <- glmmTMB(individual.rept ~ 
                             # law enforecement: number of forest police per unite of area
                             scale(forest_police_pa) + 
                             #GDP per capita 
                             scale(gdp_pc) +
                             #Population density
                             scale(pop_dens) + 
                             #Artificial habitat coverage
                             scale(artificial) + 
                             #Forest coverage 
                             scale(forest) + 
                             #Market inaccessibility: travel time to urban markets
                             scale(market_access)+ 
                             #Latitude was excluded because of its negative correlation with repthibians richness
                             #scale(Latitude) + 
                             #Road density was excluded because of its positive correlation with artificial habitat cover
                             #scale(road_dens)+ 
                             #Elevation was excluded because of it negative correlation with market inaccessibility
                             #scale(Elevation)+
                             scale(div_rept) +
                             offset(log(area)) +
                             (1|region),
                           data = Hunting,
                           family = nbinom1(link = "log"))

md.rept.ind.nb1

#model2 zero-inlation nb1

md.rept.ind.zinb1 <- glmmTMB(individual.rept ~ 
                               # law enforecement: number of forest police per unite of area
                               scale(forest_police_pa) + 
                               #GDP per capita 
                               scale(gdp_pc) +
                               #Population density
                               scale(pop_dens) + 
                               #Artificial habitat coverage
                               scale(artificial) + 
                               #Forest coverage 
                               scale(forest) + 
                               #Market inaccessibility: travel time to urban markets
                               scale(market_access)+ 
                               #Latitude was excluded because of its negative correlation with repthibians richness
                               #scale(Latitude) + 
                               #Road density was excluded because of its positive correlation with artificial habitat cover
                               #scale(road_dens)+ 
                               #Elevation was excluded because of it negative correlation with market inaccessibility
                               #scale(Elevation)+
                               scale(div_rept) +
                               offset(log(area)) +
                               (1|region),
                             data = Hunting,
                             ziformula = ~1,
                             family = nbinom1(link = "log"))

summary(md.rept.ind.zinb1)

#model3 nb2
md.rept.ind.nb2 <- glmmTMB(individual.rept ~ 
                             # law enforecement: number of forest police per unite of area
                             scale(forest_police_pa) + 
                             #GDP per capita 
                             scale(gdp_pc) +
                             #Population density
                             scale(pop_dens) + 
                             #Artificial habitat coverage
                             scale(artificial) + 
                             #Forest coverage 
                             scale(forest) + 
                             #Market inaccessibility: travel time to urban markets
                             scale(market_access)+ 
                             #Latitude was excluded because of its negative correlation with repthibians richness
                             #scale(Latitude) + 
                             #Road density was excluded because of its positive correlation with artificial habitat cover
                             #scale(road_dens)+ 
                             #Elevation was excluded because of it negative correlation with market inaccessibility
                             #scale(Elevation)+
                             scale(div_rept) +
                             offset(log(area)) +
                             (1|region),
                           data = Hunting,
                           family = nbinom2(link = "log"))
summary(md.rept.ind.nb2)


#model4 zero-inflation nb2

md.rept.ind.zinb2 <- glmmTMB(individual.rept ~ 
                               # law enforecement: number of forest police per unite of area
                               scale(forest_police_pa) + 
                               #GDP per capita 
                               scale(gdp_pc) +
                               #Population density
                               scale(pop_dens) + 
                               #Artificial habitat coverage
                               scale(artificial) + 
                               #Forest coverage 
                               scale(forest) + 
                               #Market inaccessibility: travel time to urban markets
                               scale(market_access)+ 
                               #Latitude was excluded because of its negative correlation with repthibians richness
                               #scale(Latitude) + 
                               #Road density was excluded because of its positive correlation with artificial habitat cover
                               #scale(road_dens)+ 
                               #Elevation was excluded because of it negative correlation with market inaccessibility
                               #scale(Elevation)+
                               scale(div_rept) +
                               offset(log(area)) +
                               (1|region),
                             data = Hunting,
                             ziformula = ~1,
                             family = nbinom2(link = "log"))

summary(md.rept.ind.zinb2)


#model5 poisson
md.rept.ind.poi <- glmmTMB(individual.rept ~ 
                             # law enforecement: number of forest police per unite of area
                             scale(forest_police_pa) + 
                             #GDP per capita 
                             scale(gdp_pc) +
                             #Population density
                             scale(pop_dens) + 
                             #Artificial habitat coverage
                             scale(artificial) + 
                             #Forest coverage 
                             scale(forest) + 
                             #Market inaccessibility: travel time to urban markets
                             scale(market_access)+ 
                             #Latitude was excluded because of its negative correlation with repthibians richness
                             #scale(Latitude) + 
                             #Road density was excluded because of its positive correlation with artificial habitat cover
                             #scale(road_dens)+ 
                             #Elevation was excluded because of it negative correlation with market inaccessibility
                             #scale(Elevation)+
                             scale(div_rept) +
                             offset(log(area)) +
                             (1|region),
                           data = Hunting,
                           family = poisson(link = "log"))
summary(md.rept.ind.poi)


#model6 zero-inflation poisson

md.rept.ind.zipoi <- glmmTMB(individual.rept ~ 
                               # law enforecement: number of forest police per unite of area
                               scale(forest_police_pa) + 
                               #GDP per capita 
                               scale(gdp_pc) +
                               #Population density
                               scale(pop_dens) + 
                               #Artificial habitat coverage
                               scale(artificial) + 
                               #Forest coverage 
                               scale(forest) + 
                               #Market inaccessibility: travel time to urban markets
                               scale(market_access)+ 
                               #Latitude was excluded because of its negative correlation with repthibians richness
                               #scale(Latitude) + 
                               #Road density was excluded because of its positive correlation with artificial habitat cover
                               #scale(road_dens)+ 
                               #Elevation was excluded because of it negative correlation with market inaccessibility
                               #scale(Elevation)+
                               scale(div_rept) +
                               offset(log(area)) +
                               (1|region),
                             data = Hunting,
                             ziformula = ~1,
                             family = poisson(link = "log"))

summary(md.rept.ind.zipoi)
## use AICc 
AICctab(md.rept.ind.nb1, md.rept.ind.nb2, md.rept.ind.poi,md.rept.ind.zinb1,md.rept.ind.zinb2, md.rept.ind.zipoi, base=T, weights=T)
#md.rept.zinb1 has the smallest AICc value.

summary(md.rept.ind.zinb2)

#random effects
rr <-ranef(md.rept.ind.zinb2)

print(rr, simplify=T)

ms.rep <- dredge(md.rept.ind.zinb2, rank = "AICc", fixed = "cond(offset(log(area)))")
ma.rep <- model.avg(ms.rep, subset = delta<=2)
importance(ma.rep)
summary(ma.rep)


######## run model diagnostics using the DHARMa package
## global model
summary(md.rept.ind.zinb2)
windows(8,4)
simoutDHARMa.md.rept.in.zinb2 <- simulateResiduals(fittedModel = md.rept.ind.zinb2, n=10000, plot = T)

print(ms.rep)
## top model is d45
md.rept.ind.zinb2.245 <- glmmTMB(individual.rept ~ 
                                   scale(market_access)+ #4
                                   scale(div_rept) + #2
                                   offset(log(area)) + #5
                                   (1|region),
                                 data = Hunting,
                                 family = nbinom2(link = "log"),
                                 ziformula = ~1)
summary(md.rept.ind.zinb2.245)
windows(8,4)
#simoutDHARMa.md.rept.ind.zinb2.245 <- simulateResiduals(fittedModel = md.rept.ind.zinb2.245, n=10000, plot = T) ## one sim did not converge
simoutDHARMa.md.rept.in.zinb2.245 <- simulateResiduals(fittedModel = md.rept.ind.zinb2.245, n=10000, plot = T, seed=234) ## use this

#random effects
rr <-ranef(md.rept.ind.zinb2.245)

print(rr, simplify=T)

###############
# Birds

#model1 nb1 
md.bird.ind.nb1 <- glmmTMB(individual.bird ~ 
                             # law enforecement: number of forest police per unite of area
                             scale(forest_police_pa) + 
                             #GDP per capita 
                             scale(gdp_pc) +
                             #Population density
                             scale(pop_dens) + 
                             #Artificial habitat coverage
                             scale(artificial) + 
                             #Forest coverage 
                             scale(forest) + 
                             #Market inaccessibility: travel time to urban markets
                             scale(market_access)+ 
                             #Latitude was excluded because of its negative correlation with amphibians richness
                             #scale(Latitude) + 
                             #Road density was excluded because of its positive correlation with artificial habitat cover
                             #scale(road_dens)+ 
                             #Elevation was excluded because of it negative correlation with market inaccessibility
                             #scale(Elevation)+
                             scale(div_bird) +
                             offset(log(area)) +
                             (1|region),
                           data = Hunting,
                           family = nbinom1(link = "log"))

md.bird.ind.nb1

#model2 zero-inlation nb1

md.bird.ind.zinb1 <- glmmTMB(individual.bird ~ 
                               # law enforecement: number of forest police per unite of area
                               scale(forest_police_pa) + 
                               #GDP per capita 
                               scale(gdp_pc) +
                               #Population density
                               scale(pop_dens) + 
                               #Artificial habitat coverage
                               scale(artificial) + 
                               #Forest coverage 
                               scale(forest) + 
                               #Market inaccessibility: travel time to urban markets
                               scale(market_access)+ 
                               #Latitude was excluded because of its negative correlation with amphibians richness
                               #scale(Latitude) + 
                               #Road density was excluded because of its positive correlation with artificial habitat cover
                               #scale(road_dens)+ 
                               #Elevation was excluded because of it negative correlation with market inaccessibility
                               #scale(Elevation)+
                               scale(div_bird) +
                               offset(log(area)) +
                               (1|region),
                             data = Hunting,
                             ziformula = ~1,
                             family = nbinom1(link = "log"))

summary(md.bird.ind.zinb1)

#model3 nb2
md.bird.ind.nb2 <- glmmTMB(individual.bird ~ 
                             # law enforecement: number of forest police per unite of area
                             scale(forest_police_pa) + 
                             #GDP per capita 
                             scale(gdp_pc) +
                             #Population density
                             scale(pop_dens) + 
                             #Artificial habitat coverage
                             scale(artificial) + 
                             #Forest coverage 
                             scale(forest) + 
                             #Market inaccessibility: travel time to urban markets
                             scale(market_access)+ 
                             #Latitude was excluded because of its negative correlation with amphibians richness
                             #scale(Latitude) + 
                             #Road density was excluded because of its positive correlation with artificial habitat cover
                             #scale(road_dens)+ 
                             #Elevation was excluded because of it negative correlation with market inaccessibility
                             #scale(Elevation)+
                             scale(div_bird) +
                             offset(log(area)) +
                             (1|region),
                           data = Hunting,
                           family = nbinom2(link = "log"))
summary(md.bird.ind.nb2)


#model4 zero-inflation nb2

md.bird.ind.zinb2 <- glmmTMB(individual.bird ~ 
                               # law enforecement: number of forest police per unite of area
                               scale(forest_police_pa) + 
                               #GDP per capita 
                               scale(gdp_pc) +
                               #Population density
                               scale(pop_dens) + 
                               #Artificial habitat coverage
                               scale(artificial) + 
                               #Forest coverage 
                               scale(forest) + 
                               #Market inaccessibility: travel time to urban markets
                               scale(market_access)+ 
                               #Latitude was excluded because of its negative correlation with amphibians richness
                               #scale(Latitude) + 
                               #Road density was excluded because of its positive correlation with artificial habitat cover
                               #scale(road_dens)+ 
                               #Elevation was excluded because of it negative correlation with market inaccessibility
                               #scale(Elevation)+
                               scale(div_bird) +
                               offset(log(area)) +
                               (1|region),
                             data = Hunting,
                             ziformula = ~1,
                             family = nbinom2(link = "log"))

summary(md.bird.ind.zinb2)


#model5 poisson
md.bird.ind.poi <- glmmTMB(individual.bird ~ 
                             # law enforecement: number of forest police per unite of area
                             scale(forest_police_pa) + 
                             #GDP per capita 
                             scale(gdp_pc) +
                             #Population density
                             scale(pop_dens) + 
                             #Artificial habitat coverage
                             scale(artificial) + 
                             #Forest coverage 
                             scale(forest) + 
                             #Market inaccessibility: travel time to urban markets
                             scale(market_access)+ 
                             #Latitude was excluded because of its negative correlation with amphibians richness
                             #scale(Latitude) + 
                             #Road density was excluded because of its positive correlation with artificial habitat cover
                             #scale(road_dens)+ 
                             #Elevation was excluded because of it negative correlation with market inaccessibility
                             #scale(Elevation)+
                             scale(div_bird) +
                             offset(log(area)) +
                             (1|region),
                           data = Hunting,
                           family = poisson(link = "log"))
summary(md.bird.ind.poi)


#model6 zero-inflation poisson

md.bird.ind.zipoi <- glmmTMB(individual.bird ~ 
                               # law enforecement: number of forest police per unite of area
                               scale(forest_police_pa) + 
                               #GDP per capita 
                               scale(gdp_pc) +
                               #Population density
                               scale(pop_dens) + 
                               #Artificial habitat coverage
                               scale(artificial) + 
                               #Forest coverage 
                               scale(forest) + 
                               #Market inaccessibility: travel time to urban markets
                               scale(market_access)+ 
                               #Latitude was excluded because of its negative correlation with amphibians richness
                               #scale(Latitude) + 
                               #Road density was excluded because of its positive correlation with artificial habitat cover
                               #scale(road_dens)+ 
                               #Elevation was excluded because of it negative correlation with market inaccessibility
                               #scale(Elevation)+
                               scale(div_bird) +
                               offset(log(area)) +
                               (1|region),
                             data = Hunting,
                             ziformula = ~1,
                             family = poisson(link = "log"))

summary(md.bird.ind.zipoi)

## use AICc 
AICctab(md.bird.ind.nb1, md.bird.ind.nb2, md.bird.ind.poi,md.bird.ind.zinb1,md.bird.ind.zinb2, md.bird.ind.zipoi, base=T, weights=T)
#md.bird.ind.nb2 has the smallest AICc value.
summary(md.bird.ind.nb2)

ms.bird <- dredge(md.bird.ind.nb2, rank = "AICc", fixed = "cond(offset(log(area)))")
ma.bird <- model.avg(ms.bird, subset = delta<=2)
importance(ma.bird)
summary(ma.bird)

######## run model diagnostics using the DHARMa package
## global model
summary(md.bird.ind.nb2)
# random effects
rr <-ranef(md.bird.ind.nb2)
print(rr, simplify=T)

windows(8,4)
simoutDHARMa.md.bird.in.nb2 <- simulateResiduals(fittedModel = md.bird.ind.nb2, n=10000, plot = T)

print(ms.bird)

## top model is 123567
md.bird.in.nb2.123567 <- glmmTMB(individual.bird ~ 
                                   scale(forest_police_pa) + #3 
                                   scale(pop_dens) + #6
                                   scale(artificial) + #1
                                   scale(forest) + #2
                                   scale(market_access)+ #5
                                   offset(log(area)) + #7
                                   (1|region),
                                 data = Hunting,
                                 family = nbinom2(link = "log"))
summary(md.bird.in.nb2.123567)
windows(8,4)
simoutDHARMa.md.bird.in.nb2.123567 <- simulateResiduals(fittedModel = md.bird.in.nb2.123567, n=10000, plot = T)
# random effects
rr <-ranef(md.bird.in.nb2.123567)
print(rr, simplify=T)

###############

#Mammals

#model1 nb1 
md.mamm.ind.nb1 <- glmmTMB(individual.mamm ~ 
                             # law enforecement: number of forest police per unite of area
                             scale(forest_police_pa) + 
                             #GDP per capita 
                             scale(gdp_pc) +
                             #Population density
                             scale(pop_dens) + 
                             #Artificial habitat coverage
                             scale(artificial) + 
                             #Forest coverage 
                             scale(forest) + 
                             #Market inaccessibility: travel time to urban markets
                             scale(market_access)+ 
                             #Latitude was not correlated with mammal richness
                             scale(latitude) + 
                             #Road density was excluded because of its positive correlation with artificial habitat cover
                             #scale(road_dens)+ 
                             #Elevation was excluded because of it negative correlation with market inaccessibility
                             #scale(Elevation)+
                             scale(div_mamm) +
                             offset(log(area)) +
                             (1|region),
                           data = Hunting,
                           family = nbinom1(link = "log"))

md.mamm.ind.nb1

#model2 zero-inlation nb1

md.mamm.ind.zinb1 <- glmmTMB(individual.mamm ~ 
                               # law enforecement: number of forest police per unite of area
                               scale(forest_police_pa) + 
                               #GDP per capita 
                               scale(gdp_pc) +
                               #Population density
                               scale(pop_dens) + 
                               #Artificial habitat coverage
                               scale(artificial) + 
                               #Forest coverage 
                               scale(forest) + 
                               #Market inaccessibility: travel time to urban markets
                               scale(market_access)+ 
                               #Latitude was not correlated with mammal richness
                               scale(latitude) + 
                               #Road density was excluded because of its positive correlation with artificial habitat cover
                               #scale(road_dens)+ 
                               #Elevation was excluded because of it negative correlation with market inaccessibility
                               #scale(Elevation)+
                               scale(div_mamm) +
                               offset(log(area)) +
                               (1|region),
                             data = Hunting,
                             ziformula = ~1,
                             family = nbinom1(link = "log"))

summary(md.mamm.ind.zinb1)

#model3 nb2
md.mamm.ind.nb2 <- glmmTMB(individual.mamm ~ 
                             # law enforecement: number of forest police per unite of area
                             scale(forest_police_pa) + 
                             #GDP per capita 
                             scale(gdp_pc) +
                             #Population density
                             scale(pop_dens) + 
                             #Artificial habitat coverage
                             scale(artificial) + 
                             #Forest coverage 
                             scale(forest) + 
                             #Market inaccessibility: travel time to urban markets
                             scale(market_access)+ 
                             #Latitude was not correlated with mammal richness
                             scale(latitude) + 
                             #Road density was excluded because of its positive correlation with artificial habitat cover
                             #scale(road_dens)+ 
                             #Elevation was excluded because of it negative correlation with market inaccessibility
                             #scale(Elevation)+
                             scale(div_mamm) +
                             offset(log(area)) +
                             (1|region),
                           data = Hunting,
                           family = nbinom2(link = "log"))
summary(md.mamm.ind.nb2)


#model4 zero-inflation nb2

md.mamm.ind.zinb2 <- glmmTMB(individual.mamm ~ 
                               # law enforecement: number of forest police per unite of area
                               scale(forest_police_pa) + 
                               #GDP per capita 
                               scale(gdp_pc) +
                               #Population density
                               scale(pop_dens) + 
                               #Artificial habitat coverage
                               scale(artificial) + 
                               #Forest coverage 
                               scale(forest) + 
                               #Market inaccessibility: travel time to urban markets
                               scale(market_access)+ 
                               #Latitude was not correlated with mammal richness
                               scale(latitude) + 
                               #Road density was excluded because of its positive correlation with artificial habitat cover
                               #scale(road_dens)+ 
                               #Elevation was excluded because of it negative correlation with market inaccessibility
                               #scale(Elevation)+
                               scale(div_mamm) +
                               offset(log(area)) +
                               (1|region),
                             data = Hunting,
                             ziformula = ~1,
                             family = nbinom2(link = "log"))

summary(md.mamm.ind.zinb2)


#model5 poisson
md.mamm.ind.poi <- glmmTMB(individual.mamm ~ 
                             # law enforecement: number of forest police per unite of area
                             scale(forest_police_pa) + 
                             #GDP per capita 
                             scale(gdp_pc) +
                             #Population density
                             scale(pop_dens) + 
                             #Artificial habitat coverage
                             scale(artificial) + 
                             #Forest coverage 
                             scale(forest) + 
                             #Market inaccessibility: travel time to urban markets
                             scale(market_access)+ 
                             #Latitude was not correlated with mammal richness
                             scale(latitude) + 
                             #Road density was excluded because of its positive correlation with artificial habitat cover
                             #scale(road_dens)+ 
                             #Elevation was excluded because of it negative correlation with market inaccessibility
                             #scale(Elevation)+
                             scale(div_mamm) +
                             offset(log(area)) +
                             (1|region),
                           data = Hunting,
                           family = poisson(link = "log"))
summary(md.mamm.ind.poi)


#model6 zero-inflation poisson

md.mamm.ind.zipoi <- glmmTMB(individual.mamm ~ 
                               # law enforecement: number of forest police per unite of area
                               scale(forest_police_pa) + 
                               #GDP per capita 
                               scale(gdp_pc) +
                               #Population density
                               scale(pop_dens) + 
                               #Artificial habitat coverage
                               scale(artificial) + 
                               #Forest coverage 
                               scale(forest) + 
                               #Market inaccessibility: travel time to urban markets
                               scale(market_access)+ 
                               #Latitude was not correlated with mammal richness
                               scale(latitude) + 
                               #Road density was excluded because of its positive correlation with artificial habitat cover
                               #scale(road_dens)+ 
                               #Elevation was excluded because of it negative correlation with market inaccessibility
                               #scale(Elevation)+
                               scale(div_mamm) +
                               offset(log(area)) +
                               (1|region),
                             data = Hunting,
                             ziformula = ~1,
                             family = poisson(link = "log"))

summary(md.mamm.ind.zipoi)


## use AICc 
AICctab(md.mamm.ind.nb1, md.mamm.ind.nb2, md.mamm.ind.poi,md.mamm.ind.zinb1,md.mamm.ind.zinb2, md.mamm.ind.zipoi, base=T, weights=T)
#md.mamm.ind.nb2 has the smallest AICc value.

summary(md.mamm.ind.nb2)

ms.mamm <- dredge(md.mamm.ind.nb2, rank = "AICc", fixed = "cond(offset(log(area)))")
ma.mamm <- model.avg(ms.mamm, subset = delta<=2)
importance(ma.mamm)
summary(ma.mamm)

######## run model diagnostics using the DHARMa package
## global model
summary(md.mamm.ind.nb2)
# random effects
rr <-ranef(md.mamm.ind.nb2)
print(rr, simplify=T)

windows(8,4)
simoutDHARMa.md.mamm.ind.nb2 <- simulateResiduals(fittedModel = md.mamm.ind.nb2, n=10000, plot = T)

## top model is 568
md.mamm.in.nb2.568 <- glmmTMB(individual.mamm ~ 
                                scale(market_access)+ #6 
                                scale(latitude) +  #5
                                offset(log(area)) + #8
                                (1|region),
                              data = Hunting,
                              family = nbinom2(link = "log"))
summary(md.mamm.in.nb2.568)
windows(8,4)
simoutDHARMa.md.mamm.in.nb2.568 <- simulateResiduals(fittedModel = md.mamm.in.nb2.568, n=10000, plot = T)
# random effects
rr <-ranef(md.mamm.in.nb2.568)
print(rr, simplify=T)
