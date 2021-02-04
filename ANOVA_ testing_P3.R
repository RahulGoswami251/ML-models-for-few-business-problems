

## Load "LabTAT" datset or import it in in global environment 
LabTAT <- read.csv("File Path")

# ANOVA for Turn Around Time (TAT) reports of 4 laboratories
# H0: mu1=mu2=mu3=m4 & Ha: two of them is different from other two. 

# Extracting data from 4 laboratories TAT reports data set

laboratories_1 <- c(LabTAT$Laboratory.1)
laboratories_1

laboratories_2 <- c(LabTAT$Laboratory.2)
laboratories_2

laboratories_3 <- c(LabTAT$Laboratory.3)
laboratories_3

laboratories_4 <- c(LabTAT$Laboratory.4)
laboratories_4

laboratories_TAT = c(laboratories_1,laboratories_2,laboratories_3,laboratories_4)
laboratories_TAT

avg_TAT_reports = c(rep('A',120), rep('B',120),rep('C',120),rep('D',120))
avg_TAT_reports

Lab_TAT=data.frame(laboratories_TAT,avg_TAT_reports)
Lab_TAT$avg_TAT_reports <- as.factor(Lab_TAT$avg_TAT_reports)


plot(laboratories_TAT~avg_TAT_reports, data=Lab_TAT)
results <- aov(laboratories_TAT~avg_TAT_reports, data=Lab_TAT)
summary(results)

# Some more visualizations for all laboratory average TAT reports

plot(laboratories_1)
barplot(laboratories_1)
hist(laboratories_1)
summary(laboratories_1)


plot(laboratories_2)
barplot(laboratories_2)
hist(laboratories_2)
summary(laboratories_2)


plot(laboratories_3)
barplot(laboratories_3)
hist(laboratories_3)
summary(laboratories_3)


plot(laboratories_4)
barplot(laboratories_4)
hist(laboratories_4)
summary(laboratories_4)




# As per received  P value <2e-16(2.2 X 10 ^-16) which is less than 0.05 
# so we reject the null hypothesis or we can say that two of them is different. 
# As per above visualization for all 4 laboratories average TAT reports. 

# we can say that laboratories 1 & Lab 2 average TAT reports are different from 
# laboratories 3 & laboratories 4. 
# Also all 4 laboratories average TAT reports are different from each other as per box plot
# observation at few points. 
# Laboratory 1 & 4 TAT average reports have outliers in upper & lower extreme 
# but laboratories 2 & 3 average TAT reports have no outliers.
# Laboratory 1 & 4 average TAT reports have positive & negative skweness. 
# Laboratory 2 have positive skewness.


# As per summary of all laboratories TAT reports data values show that 
# Laboratory 1 & 2 have similar median value but Laboratory 3 has higher & 
# 4 has lower median value. 




