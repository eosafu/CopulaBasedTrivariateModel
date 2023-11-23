#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Example code for modeling copula on west Africa map by Osafu
# Last modified  May 2023.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(GJRM)
library(BayesX)

# Read in map (available for download in this repository)
map <- read.bnd("wabnd.bnd")
# Read in data (Sample data is available for download in this repository)
Data <- read.csv("child_illness_data.raw",sep = "\t")


xt <- list(polys = map)

Data$region <- factor(Data$region)

eq1 <- diarrhea  ~ urban+ prim+ sec+ high+ water+ elect+ newsp+ s(child_age) + s(region, bs = "mrf", xt = xt)

eq2 <- fever   ~ urban+ prim+ sec+ high+ water+ elect+ newsp + s(child_age) + s(region, bs = "mrf", xt = xt)

eq3 <- ari   ~ urban+ prim+ sec+ high+ water+ elect+ newsp + s(child_age) +s(region, bs = "mrf", xt = xt)

eq4 <-       ~ urban+ prim+ sec+ high+ water+ elect+ newsp + s(child_age) + s(region, bs = "mrf", xt = xt)

eq5 <-       ~ urban+ prim+ sec+ high+ water+ elect+ newsp+ s(child_age)+
               s(region, bs = "mrf", xt = xt)

eq6 <-    ~    urban+ prim+ sec+ high+ water+ elect+ newsp + s(child_age) + s(region, bs = "mrf", xt = xt)

f.l <- list(eq1 ,eq2,eq3,eq4 ,eq5,eq6)


# Using Gaussian copula and probit marginal models (Note: May take time to estimate)
outDA <- gjrm(f.l,margins = c("probit","probit","probit"),copula="N", model="T", data=Data,Chol = TRUE)


summary(outDA)

# Plot non-linear and spatial effects. 
# Note: carefully navigate through the sequence plots

# diarrhea
plot(outJ270,eq=1,xlab="Child's Age", ylab="Effect") 
plot(outJ270,eq=1,xlab="Mother's Age", ylab="Effect")
# fever
plot(outJ270,eq=2,xlab="Child's Age", ylab="Effect") 
plot(outJ270,eq=2,xlab="Mother's Age", ylab="Effect")
# shared
plot(outJ270,eq=3,xlab="Child's Age", ylab="Effect") 
plot(outJ270,eq=3,xlab="Mother's Age", ylab="Effect")



# Alternatively, extract the estimates and make the plot yourself. 
# For example, To extract the spatial effect, follow the code below.

id=as.numeric((attr(map,"regions")))

# For first marginal
plt <- plot(outDA,eq=1)

# For second marginal
plt <- plot(outDA,eq=2)

# and so on...
loc=2 ##In this modeling, loc=2 extracts the spatial effect estimate. In general loc is the position
      ## of the "s(region)" among all the s() functions in the respective linear predictor used.
est=data.frame(id=id,effect=as.vector(plt[[loc]]$fit))

# Plot with own function
drawmap(est,map,"id","effect",cols="hsv",swapcolors = TRUE)

#################################
