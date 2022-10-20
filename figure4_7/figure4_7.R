## translate to R by André Suriane ECONS ECONOMIA UFJF 
#
# FIGURE4_7.M
#
# Kilian and Lutkepohl (2017), Structural VAR Analysis, Cambridge University Press.
# This file generates Figure 4.7

rm(list = ls())

# EIA MER, thousand barrels/day, 1973.1-2015.9
time<- seq(1973+1/12,2015+9/12, by = 1/12)
worldoilproduction<- read.table("worldoilproduction.txt")

# EIA Drilling Productivity Report
# Shale oil production in the Bakken, Eagle Ford, Haynesville, Marcellus, Niobrara, the Permian, Uttica, 
# These seven regions accounted for 92# of domestic oil production growth.
# Converted to thousands barrels/day
shaleoil_bakken <- read.table("shaleoil_bakken.txt")
shaleoil_eagleford <- read.table("shaleoil_eagleford.txt")
shaleoil_haynesville <- read.table("shaleoil_haynesville.txt")
shaleoil_marcellus <- read.table("shaleoil_marcellus.txt")
shaleoil_niobrara <- read.table("shaleoil_niobrara.txt")
shaleoil_permian <- read.table("shaleoil_permian.txt")
shaleoil_uttica <- read.table("shaleoil_uttica.txt")

shaleoil=shaleoil_bakken+shaleoil_eagleford+shaleoil_haynesville+shaleoil_marcellus+shaleoil_niobrara+shaleoil_permian+shaleoil_uttica;

rm(shaleoil_bakken, shaleoil_eagleford, shaleoil_haynesville, shaleoil_marcellus , shaleoil_niobrara, shaleoil_permian,shaleoil_uttica)

# Compute incremental shale oil production since November 2008 
dummy<-   c(rep(0,431), shaleoil[24:(nrow(shaleoil)-4),1]-shaleoil[23,1])/0.92;
shaleoil<-c(rep(NA,431),shaleoil[24:(nrow(shaleoil)-4),1]-shaleoil[23,1])/0.92

# Plot actual and counterfactual level of world oil production

plot(time,worldoilproduction$V1/1000, type = "l", col = 'blue',ylab= 'Million barrels/day') +
lines(tail(time, length(shaleoil)),(tail(worldoilproduction$V1, length(shaleoil))-shaleoil)/1000, lty =2 , col = "red") +
legend(1973, 80 
       , legend = c('Actual','Without Shale Oil')
       , col = c("blue","red")
       , lty= c(1,2)
       , cex = .7
)


# Define the counterfactual level of world oil production
 counterfactual<-(worldoilproduction-dummy)/1000;
 write.csv2(counterfactual,"counterfactual.csv")

