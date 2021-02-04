
############ Prepare Association rules for my movies data ##########################

## Load "my_movies" dataset 
mymovies <- read.csv("File path")

## Set environment for dataset algorithm 
install.packages("arules")
library(arules)

## Prepare apriori algorithm for getting asscoaition rules 
mymovies_ass_rules <- apriori(mymovies)
##Inspect the rules 
arules::inspect(mymovies_ass_rules)
## Sorted the rules by lift ratio & inspect them 
rules.sorted <- sort(mymovies_ass_rules,by="lift")
arules::inspect(rules.sorted)

## Remove the redundant rules 
mymovies_ass_rules1 <- mymovies_ass_rules[!is.redundant(mymovies_ass_rules)]
## Inspect the rules
arules:: inspect(mymovies_ass_rules1)


## Rules with rhs containing pipeline "V3" movies 
V3_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                    ,appearance = list(rhs=c("V3=")),control = list(verbose=F))
arules::inspect(V3_rules)


## Rules with rhs containing pipleline "V4" movies
V4_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                    ,appearance = list(rhs=c("V4=")),control = list(verbose=F))
arules::inspect(V4_rules)


## Rules with rhs pipleine "V3" & "V4" pipeline movies 
V3_V4_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                       ,appearance = list(rhs=c("V3=", "V4=")),control = list(verbose=F))
arules::inspect(V3_V4_rules)


## Rules with rhs contanining pipleline "V5" movies 
V5_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                    ,appearance = list(rhs=c("V5=")),control = list(verbose=F))
arules::inspect(V5_rules)


## Rules with rhs containing movie "	Braveheart"
Braveheart_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                            ,appearance = list(rhs=c("Braveheart=[0,1]")),control = list(verbose=F))
arules::inspect(Braveheart_rules)


## Rules with rhs containing movie "Gladiator"
Gladiator_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                           ,appearance = list(rhs=c("Gladiator=[0,1]")),control = list(verbose=F))
arules::inspect(Gladiator_rules)


## Rules with rhs containing movie "	Braveheart"
Green_Mile_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                            ,appearance = list(rhs=c("Green.Mile=[0,1]")),control = list(verbose=F))
arules::inspect(Green_Mile_rules)


## Rules with rhs containing movie "Harry Potter1"
Harry_Potter1_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                               ,appearance = list(rhs=c("Harry.Potter1=[0,1]")),control = list(verbose=F))
arules::inspect(Harry_Potter1_rules)


## Rules with rhs containing movie "Harry Potter2"
Harry_Potter2_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                               ,appearance = list(rhs=c("Harry.Potter2=[0,1]")),control = list(verbose=F))
arules::inspect(Harry_Potter1_rules)



## Rules with rhs containing movie "LOTR"
LOTR_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                      ,appearance = list(rhs=c("LOTR=[0,1]")),control = list(verbose=F))
arules::inspect(LOTR_rules)


## Rules with rhs containing movie "LOTR1"
LOTR1_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                       ,appearance = list(rhs=c("LOTR1=[0,1]")),control = list(verbose=F))
arules::inspect(LOTR1_rules)


## Rules with rhs containing movie "LOTR2"
LOTR2_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                       ,appearance = list(rhs=c("LOTR2=[0,1]")),control = list(verbose=F))
arules::inspect(LOTR2_rules)


## Rules with rhs containing movie "Patriot"
Patriot_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                         ,appearance = list(rhs=c("Patriot=[0,1]")),control = list(verbose=F))
arules::inspect(Patriot_rules)


## Rules with rhs containing movie "Sixth Sense"
Sixth_Sense_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                       ,appearance = list(rhs=c("Sixth.Sense=[0,1]")),control = list(verbose=F))
arules::inspect(Sixth_Sense_rules)


## Rules with rhs containing movies "Sixth Sense" & "Patriot"
Six_Sen__Pat_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                             ,appearance = list(rhs=c("Sixth.Sense=[0,1]","Patriot=[0,1]")),control = list(verbose=F))
arules::inspect(Six_Sen__Pat_rules)


## Rules with rhs containing movies "Sixth Sense","Patriot" & "Gladiator"
SSPG._rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                              ,appearance = list(rhs=c("Sixth.Sense=[0,1]","Patriot=[0,1]","Gladiator=[0,1]")),control = list(verbose=F))
arules::inspect(SSPG_rules)


## Rules with rhs containing movies "Sixth Sense","Patriot","Gladiator" & "Green Mile"
SSPGG_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5),
                 appearance = list(rhs=c("Sixth.Sense=[0,1]","Patriot=[0,1]",
                "Gladiator=[0,1]","Green.Mile=[0,1]")),control = list(verbose=F))
arules::inspect(SSPGG_rules)


## Rules with rhs containing movies "Sixth Sense","Patriot","Gladiator",
## "Green Mile","Harry Potter1"
SSPGGHP1_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5),
                appearance = list(rhs=c("Sixth.Sense=[0,1]","Patriot=[0,1]",
                "Gladiator=[0,1]","Green.Mile=[0,1]","Harry.Potter1=[0,1]")),
                control = list(verbose=F))
arules::inspect(SSPGGHP1_rules)


## Rules with rhs containing movies "Sixth Sense","Patriot","Gladiator",
## "Green Mile","Harry Potter1","Harry Potter2"
SSPGGHP1HP2_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5),
                appearance = list(rhs=c("Sixth.Sense=[0,1]","Patriot=[0,1]",
                "Gladiator=[0,1]","Green.Mile=[0,1]","Harry.Potter1=[0,1]",
                "Harry.Potter2=[0,1]")),control = list(verbose=F))
arules::inspect(SSPGGHP2_rules)


## Rules with rhs containing movies "Sixth Sense","Patriot","Gladiator",
## "Green Mile","Harry Potter1","Harry Potter2",	"LOTR"
SSPGGHP1HP2L_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5),
                  appearance = list(rhs=c("Sixth.Sense=[0,1]","Patriot=[0,1]",
                  "Gladiator=[0,1]","Green.Mile=[0,1]","Harry.Potter1=[0,1]",
                  "Harry.Potter2=[0,1]","LOTR=[0,1]")),control = list(verbose=F))
arules::inspect(SSPGGHP2L_rules)


## Rules with rhs containing movies "Sixth Sense","Patriot","Gladiator",
## "Green Mile","Harry Potter1","Harry Potter2",	"LOTR","LOTR1"
SSPGGHP1HP2LL1_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5),
                     appearance = list(rhs=c("Sixth.Sense=[0,1]","Patriot=[0,1]",
                     "Gladiator=[0,1]","Green.Mile=[0,1]","Harry.Potter1=[0,1]",
                     "Harry.Potter2=[0,1]","LOTR=[0,1]","LOTR1=[0,1]")),
                     control = list(verbose=F))
arules::inspect(SSPGGHP2LL1_rules)


## Rules with rhs containing movies "Sixth Sense","Patriot","Gladiator",
## "Green Mile","Harry Potter1","Harry Potter2",	"LOTR","LOTR1" & "LOTR1" 
SSPGGHP1HP2LL1L2_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5),
                          appearance = list(rhs=c("Sixth.Sense=[0,1]","Patriot=[0,1]",
                          "Gladiator=[0,1]","Green.Mile=[0,1]","Harry.Potter1=[0,1]",
                          "Harry.Potter2=[0,1]","LOTR=[0,1]","LOTR1=[0,1]","LOTR2=[0,1]")),
                          control = list(verbose=F))
arules::inspect(SSPGGHP2LL1L2_rules)



## Rules with rhs containing movies "Sixth Sense","Patriot","Gladiator",
## "Green Mile","Harry Potter1","Harry Potter2",	"LOTR","LOTR1","LOTR1" & "Braveheart" 
SSPGGHP1HP2LL1L2B_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5),
                          appearance = list(rhs=c("Sixth.Sense=[0,1]","Patriot=[0,1]",
                          "Gladiator=[0,1]","Green.Mile=[0,1]","Harry.Potter1=[0,1]",
                          "Harry.Potter2=[0,1]","LOTR=[0,1]","LOTR1=[0,1]","LOTR2=[0,1]",
                          "Braveheart=[0,1]" )),control = list(verbose=F))
arules::inspect(SSPGGHP2LL1L2B_rules)


## Rules with rhs containing movies "Harry Potter1" & "Harry Potter2"
H1H2_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                      ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                      "Harry.Potter2=[0,1]")),control = list(verbose=F))
arules::inspect(H1H2_rules)


## Rules with rhs containing movies "LOTR" & "LOTR1"
LL1_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                      ,appearance = list(rhs=c("LOTR=[0,1]",
                                               "LOTR1=[0,1]")),control = list(verbose=F))
arules::inspect(LL1_rules)


## Rules with rhs containing movies "LOTR","LOTR1" & "LOTR2"
LL1L2_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
              ,appearance = list(rhs=c("LOTR=[0,1]","LOTR1=[0,1]","LOTR2=[0,1]")),
              control = list(verbose=F))
arules::inspect(LL1L2_rules)


## Rules with rhs containing movies "Harry Potter1","Harry Potter2",
## "LOTR","LOTR1" & "LOTR2"
H1H2LL1L2_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                  ,appearance = list(rhs=c("Harry.Potter1=[0,1]","Harry.Potter2=[0,1]",
                  "LOTR=[0,1]","LOTR1=[0,1]","LOTR2=[0,1]")),
                  control = list(verbose=F))
arules::inspect(H1H2LL1L2_rules)


## Rules with rhs containing movies "Harry Potter1","Harry Potter2", "Sixth Sense",
## "LOTR" &"LOTR1" 
H1H2SLL1_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
              ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
              "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]",
              "LOTR1=[0,1]")),control = list(verbose=F))
arules::inspect(H1H2SLL1_rules)

## Rules with rhs containing movies "Harry Potter1","Harry Potter2", "Sixth Sense",
## "LOTR"  
H1H2SLO_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                          ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                          "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]")),
                          control = list(verbose=F))
arules::inspect(H1H2SLO_rules)

## Algorithms with Different values of support and confidence
rules1<- apriori(mymovies,parameter = list(supp=0.1, conf=0.5)
                         ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                                                  "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]")),
                         control = list(verbose=F))
arules::inspect(rules1)


rules2 <- apriori(mymovies,parameter = list(supp=0.2, conf=0.70)
                         ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                                                  "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]")),
                         control = list(verbose=F))
arules::inspect(rules2)


rules3 <- apriori(mymovies,parameter = list(supp=0.2, conf=0.90)
                         ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                                                  "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]")),
                         control = list(verbose=F))
arules::inspect(rules3)



rules5 <- apriori(mymovies,parameter = list(supp=0.2, conf=0.95)
                         ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                                                  "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]")),
                         control = list(verbose=F))
arules::inspect(rules5)


rules6 <- apriori(mymovies,parameter = list(supp=0.2, conf=0.97)
                  ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                                           "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]")),
                  control = list(verbose=F))
arules::inspect(rules6)


rules7 <- apriori(mymovies,parameter = list(supp=0.3, conf=0.95)
                  ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                                           "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]")),
                  control = list(verbose=F))
arules::inspect(rules7)



rules8 <- apriori(mymovies,parameter = list(supp=0.4, conf=0.95)
                  ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                                           "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]")),
                  control = list(verbose=F))
arules::inspect(rules8)


rules9 <- apriori(mymovies,parameter = list(supp=0.27, conf=0.97)
                  ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                                           "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]")),
                  control = list(verbose=F))
arules::inspect(rules9)


rules10 <- apriori(mymovies,parameter = list(supp=0.32, conf=0.97)
                  ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                                           "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]")),
                  control = list(verbose=F))
arules::inspect(rules10)

rules11 <- apriori(mymovies,parameter = list(supp=0.38, conf=0.97)
                   ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                                            "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]")),
                   control = list(verbose=F))
arules::inspect(rules11)


## The minimum length or miminum support and confirdence  in apriori algorithm 
min_rules <- apriori(mymovies,parameter = list(supp=0.1, conf=0.8)
                         ,appearance = list(rhs=c("Harry.Potter1=[0,1]",
                                                  "Harry.Potter2=[0,1]","Sixth.Sense=[0,1]","LOTR=[0,1]")),
                         control = list(verbose=F))
arules::inspect(min_rules)


## Visualization of above built all rules
install.packages("arulesViz")
install.packages("grid")
library(arulesViz)
plot(rules.sorted, main="rules sorted by lift ratio")
plot(V3_rules, main="V3 pipeline movies rules" )
plot(V4_rules, main="V4 pipeline movies rules")
plot(V5_rules, main= "V5 pipeline movies rules")
plot(V3_V4_rules, main="V3 & V4 pipeline movies rules")
plot(Braveheart_rules, main="Braveheart movie rules")
plot(Gladiator_rules, main="Gladiator movie rules")
plot(Green_Mile_rules, main="Green Mile movie rules")
plot(Harry_Potter1_rules, main="Harry Potter1 movie rules")
plot(Harry_Potter2_rules, main="Harry_Potter2 movie rules") 
plot(LOTR_rules, main="LOTR movie rules")
plot(LOTR1_rules, main="LOTR1 movie rules") 
plot(LOTR2_rules, main="LOTR2 movie rules") 
plot(Patriot_rules, main=" Patriot movie rules") 
plot(Sixth_Sense_rules, main="Sixth Sense rules")
plot(Six_Sen__Pat_rules, main="Sixth Sense & Patriot movies rules") 
plot(SSPG._rules, main="Sixth Sense,Patriot & Gladiator movies rules ")
plot(SSPGG_rules, main="Sixth Sense,Patriot,Gladiator & Green Mile movies rules")
plot(SSPGGHP1_rules, main="Sixth Sense,Patriot,Gladiator,Green Mile movie rules & Harry Potter1 movies rules")
plot(SSPGGHP1HP2_rules, main="Sixth Sense,Patriot,Gladiator,Green Mile movie rules,Harry Potter1 & Harry Potter2 movies rules") 
plot(SSPGGHP1HP2L_rules, main="Sixth Sense,Patriot,Gladiator,Green Mile movie rules,Harry Potter1,Harry Potter2 & LOTR movies rules") 
plot(SSPGGHP1HP2LL1_rules, main="Sixth Sense,Patriot,Gladiator,Green Mile movie rules,Harry Potter1, Harry Potter2,LOTR & LOTR1 movies rules")
plot(SSPGGHP1HP2LL1L2_rules, main="Sixth Sense,Patriot,Gladiator,Green Mile movie rules,Harry Potter1,Harry Potter2,LOTR,LOTR1 & LOTR2 movies rules") 
plot(SSPGGHP1HP2LL1L2B_rules, main="Sixth Sense,Patriot,Gladiator,Green Mile movie rules,Harry Potter1,Harry Potter2,LOTR,LOTR1,LOTR2 & Braveheart movies rules")
plot(H1H2_rules, main="Harry Potter1 & Harry Potter2 movies rules")
plot(LL1_rules, main="LOTR & LOTR1 movies rules") 
plot(LL1L2_rules, main="LOTR,LOTR1 & LOTR2 movies rules") 
plot(H1H2LL1L2_rules, main="Harry Potter1,Harry Potter2,LOTR,LOTR1 & LOTR2 movies rules")
plot(H1H2SLL1_rules, main="Harry Potter1,Harry Potter2, Sixth Sense,LOTR & LOTR1")
plot(H1H2SLO_rules ,main= "Harry Potter1,Harry Potter2, Sixth Sense,LOTR & LOTR1")

plot(rules1, main="rules1")
plot(rules2, main="rules2")
plot(rules3, main="rules3")
plot(rules5, main="rules5")
plot(rules6, main="rules6")
plot(rules7, main="rules7")
plot(rules8, main="rules8")
plot(rules9, main="rules9")
plot(rules10, main="rules10")
plot(rules11, main="rules11")
plot(min_rules, main="minimum length of apriori algorithm")



