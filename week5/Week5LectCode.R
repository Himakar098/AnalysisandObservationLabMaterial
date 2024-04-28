##STAT2402, 2022 S2. R code for week 5 F2F session. 

setwd("C:/Users/rnazi/Documents/Teaching/STAT2402/STAT2402-2022/LectureSlides")
bf <- read.table("Data/bodyfat.txt", header = T, sep = "\t", stringsAsFactors = T)
summary(bf)

#Q1
x <- c(958, 899)
n <- c(1000, 1000)
(pt <- prop.test(x = x, n =n, alternative = "two.sided"))
(pt1 <- prop.test(x = x, n =n, alternative = "greater"))

##Q2
x2 <- c(298, 81)
n2 <- c(3055, 744)
(pt2 <- prop.test(x = x2, n =n2, alternative = "less"))


##Q3
bf.lm <- lm(Pct.BF ~., data = bf)
summary(bf.lm)
plot(bf)
plot(bf$Waist ~ bf$Abdomen)

bf.lm1 <- lm(Pct.BF ~ .^2, data = bf)
summary(bf.lm1)

library(MASS)
stepAIC(bf.lm1)

bf.lm2 <- lm(Pct.BF ~ Density + Age + Weight + Height + Neck + 
               Chest + Abdomen + Hip + Thigh + Knee + Ankle + Bicep + Forearm + 
               Wrist + Density:Age + Density:Weight + Density:Neck + Density:Abdomen + 
               Density:Thigh + Density:Knee + Density:Ankle + Density:Bicep + 
               Density:Wrist + Age:Weight + Age:Height + Age:Abdomen + Age:Knee + 
               Age:Ankle + Age:Bicep + Age:Wrist + Weight:Neck + Weight:Hip + 
               Weight:Thigh + Weight:Knee + Weight:Ankle + Weight:Wrist + 
               Height:Chest + Height:Thigh + Height:Bicep + Neck:Abdomen + 
               Neck:Bicep + Chest:Thigh + Chest:Knee + Chest:Ankle + Chest:Bicep + 
               Abdomen:Thigh + Abdomen:Knee + Abdomen:Forearm + Hip:Knee + 
               Hip:Ankle + Hip:Forearm + Hip:Wrist + Thigh:Knee + Knee:Wrist + 
               Ankle:Wrist + Bicep:Forearm + Bicep:Wrist, data = bf)
summary(bf.lm2)

