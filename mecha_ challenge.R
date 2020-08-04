library(tidyverse)
mecha_car <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(mpg ~ AWD, data = mecha_car)
summary(lm(mpg~AWD,mecha_car))
mecha_model <- lm(mpg ~ AWD, mecha_car)
yvals <- mecha_model$coefficients['hp']*mecha_car$mpg +
  +     mecha_model$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(mecha_model,aes(x=mpg,y=AWD))
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model
names(mecha_car) = str_replace_all(names(mecha_car), c(" " = "_"))
regression <- lm(mpg ~ AWD + vehicle_length + vehicle_weight + spoiler_angle + ground_clearance,data=mecha_car)
summary(regression)
## Mean, Median, Variance, Standard Deviation
suspension_coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
statistcal_table <- suspension_coil %>% group_by(PSI) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Standard_Deviation_PSI()=sd(PSI),Variance_PSI()=var(PSI)) 
summarize_PSI <- suspension_coil %>% summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), Variance=var(PSI),StandardDev_PSI=sd(PSI))#create summary table
## T-TEST
t.test(suspension_coil$PSI,mu=summarize_PSI$Mean_PSI) #compare sample versus population means
summarize_PSI$Mean_PSI
stat.desc(suspension_coil)

