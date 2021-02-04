


## Load "CustomerOrderForm" dataset or import it in global environment 
CustomerOrderForm <- read.csv("File path")

## ANOVA for finding defectiveness % in customer order forms in four countries 

cust_ord_form <- c(CustomerOrderForm$Phillippines,CustomerOrderForm$Indonesia,CustomerOrderForm$Malta,CustomerOrderForm$India)
cust_ord_form

def_er_cof <- c(rep('A',300), rep('B',300), rep('C',300),rep('D',300))
def_er_cof

per_der_cof= data.frame(cust_ord_form,def_er_cof)
per_der_cof$def_er_cof <- as.factor(per_der_cof$def_er_cof)

plot(cust_ord_form ~ def_er_cof, data=per_der_cof)
results <- aov(cust_ord_form ~ def_er_cof, data=per_der_cof)
summary(results)

## As per received P value 0.278 value is greater than 0.05 then we reject null hypothesis
## or we can say that only few defective % vary in customer order form from its center 
## in all 4 tell call center around the globe. 
## But as per box plot visualization it's clear that customer order form has 
## defectiveness from its center due to each individual tell Call center customer order form 
## has one outlier in their data. 

## Few other visualization part we can perform here for analysis of data  

plot(CustomerOrderForm$Phillippines, main = 'phillippines tellcall center',ylab='customer order form')
summary(CustomerOrderForm$Phillippines)

plot(CustomerOrderForm$Indonesia, main = 'Indonesia tellcall center', ylab='customer order form')
summary(CustomerOrderForm$Indonesia)

plot(CustomerOrderForm$Malta, main= 'Malta tellcall center',ylab='customer order form')
summary(CustomerOrderForm$Malta)

plot(CustomerOrderForm$India, main='India tellcall center',ylab='cust_ord_form')
summary(CustomerOrderForm$India)


## As per above plot visualization we can say that only India TellCall canter has lowest 
## defectiveness in its customer order forms from its center as compare to other country 
## TellCall center customer order form defectiveness from its center.
## As per summary of all country tellcall center each center customer order forms have 
## few % of defectiveness from its center which details are following:
## phillippines customer order forms: 9.67%  defectiveness &  90.33% error free   
## Indonesia customer order forms:    11%    defectiveness &  89%    error free
## Malta customer order forms:        10.33% defectiveness &  89.67% error free
## India customer order forms:        6.67%  defectiveness &  93.33  error free

