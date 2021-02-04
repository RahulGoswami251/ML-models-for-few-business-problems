


## Load "BuyerRatio" dataset or import it in global environment 
BuyerRatio <- read.csv("File Path")

## ANOVA for finding buyer ratio in four different sales regions 
## H0: mu_e = mu_w = mu_n = mu_s & 
## Ha: mu_e != mu_w != mu_n != mu_s 

## Extract data from all four sales regions 

sales_reg.= c(BuyerRatio$East, BuyerRatio$West,BuyerRatio$North,BuyerRatio$South)
sales_reg.

sex_rt. = c(rep('A',2),rep('B',2),rep('C',2), rep('D',2)) 
sex_rt.

buy_rt <- data.frame(sales_reg.,sex_rt.) 
buy_rt$sex_rt. <- as.factor(buy_rt$sex_rt.)

plot(sales_reg.~sex_rt., data=buy_rt)
results <- aov(sales_reg.~sex_rt., data=buy_rt)
summary(results)


# As per box plot visualization west & north sales region has sex (Male & Female) ratio is more 
# compare to east & south region. 
# East sales region have lower sex ratio among all sales region. 
# As per received value of P 0.82 which is greater than 0.05 then we reject null hypothesis. 









