

############ Prepare Association rules for grocery data ##########################

## Load "groceries" dataset
grocery <- read.csv("File path")
## Set environment for dataset algorithm 
install.packages("arules")
library(arules)

## Prepare apriori algorithm for getting association rules 
grocery_ass.rul. <- apriori(grocery) 
##Inspect the rules 
arules::inspect(grocery_ass.rul.)
## Sorted the rules by lift ratio & inspect them 
rules.sorted <- sort(grocery_ass.rul.,by="lift")
arules::inspect(rules.sorted)
## Remove the redundant rules 
grocery_ass.rul.1 <- grocery_ass.rul.[!is.redundant(grocery_ass.rul.)]
## Inspect the rules
arules:: inspect(grocery_ass.rul.1)


## Rules with rhs containing maragarine only
margarine_rules <- apriori(grocery,parameter = list(supp=0.1, conf=0.5)
                 ,appearance = list(rhs=c("margarine=")),control = list(verbose=F))
arules::inspect(margarine_rules)
## Rules with rhs containing  only with "ready.soups"
ready_soups_rules <- apriori(grocery,parameter = list(supp=0.2, conf=0.7),
                             appearance= list(rhs=c("ready.soups=")),
                             control = list(verbose=F))
arules::inspect(ready_soups_rules)



## Rules with different support & confidence
rules1 <- apriori(grocery,parameter = list(supp=0.3, conf=0.8),
                             control = list(verbose=F))
arules::inspect(rules1)


rules2 <- apriori(grocery,parameter = list(supp=0.4, conf=0.9),
                  control = list(verbose=F))
arules::inspect(rules2)


rules3 <- apriori(grocery,parameter = list(supp=0.3, conf=0.95),
                  control = list(verbose=F))
arules::inspect(rules3)


rules4 <- apriori(grocery,parameter = list(supp=0.1, conf=1),
                            control = list(verbose=F))
arules::inspect(rules4)


rules5 <- apriori(grocery,parameter = list(supp=0.39, conf=0.9),
                            control = list(verbose=F))
arules::inspect(rules5)


## The minimum length or miminum support and confirdence  in apriori algorithm 
min_rules <- apriori(grocery,parameter = list(supp=0.39, conf=0.39),
                     control = list(verbose=F))
arules::inspect(min_rules)


## Visualization of above built all rules
install.packages("arulesViz")
install.packages("grid")
library(arulesViz)
library(grid)
library(lattice)
plot(grocery_ass.rul.,main="All rules for grocery data")
plot(margarine_rules,main='lhs margraine rules')
plot(ready_soups_rules,main='ready.soups rules')
plot(rules.sorted, main='rules by lift ratio')
plot(rules1, main='rules1')
plot(rules3, main='rules3')
plot(rules4, main='rules4')
plot(rules5, main='rules5')
plot(min_rules, main= "min length of apriori algorithm")







