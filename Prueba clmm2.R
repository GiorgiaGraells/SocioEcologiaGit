#MCMCglmm package test

library(MuMIn)
library(ordinal)
data(wine)



# Cumulative Link Mixed Model fitted with the Laplace approximation
fm1 <- clmm2(rating ~ temp + contact, random=judge, data=wine, Hess = T)
fmm1 <- clmm(rating ~ temp + contact + (1|judge), data = wine)	

fm1

# Cumulative Link Mixed Model fitted with the adaptive Gauss-Hermite 
#quadrature approximation with 10 quadrature points
fm2 <- clmm2(rating ~ temp + contact, random=judge, data=wine, Hess=TRUE, nAGQ=10)
summary(fm2)

# Likelihood ratio tests of cumulative link models
fm3 <- clmm(rating ~ temp, random=judge, data=wine, nAGQ=10)
anova(fm3, fm2)

options(na.action = "na.fail")

Selected <- dredge(fmm1)
  


