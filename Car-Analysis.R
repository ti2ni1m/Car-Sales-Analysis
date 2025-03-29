vehicles= read.csv("light_vehicles.csv")
head(vehicles)

#Question 1.1 Test if the mean number of cylinder is different between Mazda and Isuzu vehicles

#H0: No difference between number of cylinders of both Mazda and Isuzu vehicles
#HA: A difference between number of cylinders of both Mazda and Isuzu vehicles

vehicles.Isuzu <- subset(vehicles, Make=='Isuzu', Number.of.cylinders, drop=TRUE)
mean(vehicles.Isuzu)
vehicles.Mazda <- subset(vehicles, Make=='Mazda', Number.of.cylinders, drop=TRUE)
mean(vehicles.Mazda)

label<-c("Isuzu", "Mazda")
boxplot(vehicles.Isuzu, vehicles.Mazda, names=label, col="skyblue", main="Boxplot the number of cylinders between Mazda and Isuzu")
vehicles.t.test <- t.test(vehicles.Isuzu,vehicles.Mazda, var.equal=TRUE, alternative="two.sided")
vehicles.t.test$p.value
#P value is more than 5%,
# The null hypothesis test is  not rejected as there is a very slight difference between Mazda and Isuzu

sim= replicate(1000, {
  Isuzu.resamp= sample(vehicles.Isuzu, replace=TRUE)
  Mazda.resamp= sample(vehicles.Mazda, replace=TRUE)
  t.test(Isuzu.resamp, Mazda.resamp, var.equal=TRUE)$statistic
})

hist(sim, breaks=20, main= "Mean differences in number of Cylinders between Isuzu and Mazda")
quantile(sim, c(0.05, 0.95)) 
quantile (sim, 0.50)
abline(v=quantile(sim, c(0.05, 0.95)), col=6)

#There is a mean differences in a number of cylinders is  centralised in approximately -0.00232. This means there is barely any differences with the number of cylinders between Mazda and Isuzu
# The quantile between 5% and 95%, it is for sure that 95% confident that the mean difference between both vehicles is approximately between -1.7382 and 1.5443.

#Question 1.2 Test if the mean number of seats is different for each colour. If so, determine which colour has a statistically different mean

F.test <- oneway.test(vehicles$Number.of.seats~vehicles$Colour, data=vehicles)
F.test #p-value is small, therefore F.test is big

tu <- aov(vehicles$Number.of.seats~vehicles$Colour, data=vehicles)
tu

summary(tu)

tuke.test<-TukeyHSD(tu)
par(mar=c(5.1,10,4.1,2.1), cex=0.8)
plot(TukeyHSD(tu), las=1)

# there are not much differences in means of number of sets in each colour, using the Tukey test the most difference I can see with is Blue-Beige, Bronze-Beige, White-Beige, Red-Blue, Red-Bronze, Silver-Bronze, Silver-Grey, and Silver-Khaki.

#Question 1.3 
# Use Bootstrapping to compute a 88% confidence interval for the difference between GVM and TareWeights for Volkswagen vehicles?
# Compute a 88% CI for the difference between GVM and Tare Weights for Volkswagen vehicle by using approximation.
# Can we conclude that GVM weights are different than Tare Weights for Volkswagen vehicles (Don't do a hypothesis test)?
# Test the hypothesis that GVM weights are greater than Tare Weights for Volkswagen vehicles

#There is no record on Volkswagen, a simulation boot to approximate data is needed:

count.Volkswagen.GVM.weight= length(subset(vehicles, Make=="Volkswagen", GVM.weight, drop=TRUE))
count.Volkswagen.GVM.weight
count.Volkswagen.Tare.weight= length(subset(vehicles, Make=="Volkswagen", Tare.weight, drop=TRUE))
count.Volkswagen.Tare.weight

Volkswagen.GVM = subset(vehicles, Make=="Volkswagen", GVM.weight, drop=TRUE)
Volkswagen.GVM
Volkswagen.Tare= subset(vehicles, Make=="Volkswagen", Tare.weight, drop=TRUE)
Volkswagen.Tare
d0= mean(Volkswagen.GVM-Volkswagen.Tare)
d0


boot= replicate(10000, {
  Volkswagen.GVM.sampled= sample(Volkswagen.GVM, replace=TRUE)
  Volkswagen.Tare.sampled= sample(Volkswagen.Tare, replace=TRUE)
  mean(Volkswagen.GVM.sampled-Volkswagen.Tare.sampled)
})
hist(boot, breaks=20)
quantile(boot, c(0.06, 0.94))
abline(v=quantile(boot, c(0.06, 0.94)), col=6)

CI_88_approx=wilcox.test(Volkswagen.GVM,Volkswagen.Tare, conf.int= TRUE, conf.level=0.88)
CI_88_approx
CI_88_approx$conf.int
#The approximate conf level of 88%CI

# To conclude that the GVM weight are different than Tare weights of Volkswagen vehicles since 88% of data lied in between -268 and 1242 and a very small percentage lying on 0 which means the difference is zero between GVM and Tare weight for Volkswagen

#Using Wilcoxon-Mann Whitney test to find if  GVMweight are greater than TareWeights for Volkswagen

#H0: GVM.weight is not greater to Tare.Weights
#HA: GVM.weight is greater than Tare.weights
wilcox.test(Volkswagen.GVM, Volkswagen.Tare, alternative = "greater")
boxplot(Volkswagen.GVM, Volkswagen.Tare, names= c("GVM.weight", "Tare weight"), main="Boxplot of GVM and Tare weights of Volkswagen")
#Since the p-value is more than 5%, the null hypothesis is favoured


#Question 1.4 Test if there is a difference in proportions of the silver vehicles between Landrover and Mercedes

l=length(subset(vehicles, Make=="Landrover", Colour, drop=TRUE))
l
m=length(subset(vehicles,Make=="Mercedes", Colour, drop=TRUE))
m
Colour.vehicles.table=table(vehicles$Make[vehicles$Colour=="Silver"])
Colour.vehicles.table
Silver.Landrover=Colour.vehicles.table[7]
Silver.Landrover
Silver.Mercedes=Colour.vehicles.table[9]
Silver.Mercedes
diff.proportion=(Silver.Landrover/l) - (Silver.Mercedes/m)
diff.proportion

# The difference in proportion is -0.05411, meaning that the proportion of Silver Landrover is less than Silver Mercedes.

#Question 1.5 The recent trend shows that people tend to buy more powerful vehicles. We would like to investigate whether there is a linear relationship between the registration year and the mean of the number of the cylinders
# Decide if the mean numbers of cylinders and the registration year are linearly related?
# If so, compute the equation to predict mean number of cylinders by using the registration year and discuss the significance of this equation? What is your estimate of the population mean number of cylinders when the year is 1984?

min(vehicles$Year)
max(vehicles$Year)    

mean.calculate <- tapply(vehicles$Number.of.cylinders, vehicles$Year, mean)    
mean.calculate

year.register<- c(1985:2021)

cor(mean.calculate, year.register, method="pearson")

plot(y=mean.calculate, x=year.register, pch=5, main="Linear relationship between the number of cylinders and the registration years")
abline(lm(mean.calculate~year.register))
#There is a little to no downward trend but correlation coefficient is very weak.


fit= lm(mean.calculate~year.register)
fit
slope= fit[[1]][2]
slope
intercept = fit[[1]][1]
intercept

x = 1990
axis= slope*x + intercept
axis
