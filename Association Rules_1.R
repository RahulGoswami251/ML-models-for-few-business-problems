
############Prepare Asssociation rules for books data ##########################

##Load the data 
Books <- read.csv("E:\\DATASCIENCE\\2nd May Data Science Class\\Assignment_2020\\Data Science Assignment_Rahul_ Goswami\\Association Rules\\book.csv")

##set environment for dataset algorithm 
install.packages("arules")
library(arules)
library(lattice)

#### Preapare apriori algorithm for getting asscoaition rules 
books_rules <- apriori(Books)
## Inspect the rules
arules:: inspect(books_rules)
## Sorted the rules by lift ratio & inspect them 
sorted_booksrules <- sort(books_rules,by="lift")
arules::inspect(sorted_booksrules)
## Remove the redundant rules 
books_rules1 <- books_rules[!is.redundant(books_rules)]
## Inspect the rules
arules:: inspect(books_rules1)


## Rules with rhs containing only "ChildBks" book
CB_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                    appearance=list(rhs=c("ChildBks=[0,1]")), 
                    control=list(verbose=F))
arules::inspect(CB_rules)


####Rules with rhs containing only "YouthBks" book
Youthbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                    appearance=list(rhs=c("YouthBks=[0,1]")), 
                    control=list(verbose=F))
arules::inspect(Youthbs_rules)


######Rules with rhs containing only "CookBks" book
Cookbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                         appearance=list(rhs=c("CookBks=[0,1]")), 
                         control=list(verbose=F))
arules::inspect(Cookbs_rules)


####Rules with rhs containing only "DoItYBks" book
DoItYbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                         appearance=list(rhs=c("DoItYBks=[0,1]")), 
                         control=list(verbose=F))
arules::inspect(DoItYbs_rules)


####Rules with rhs containing only "RefBks" book
Refbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                         appearance=list(rhs=c("RefBks=[0,1]")), 
                         control=list(verbose=F))
arules::inspect(Refbs_rules)


####Rules with rhs containing only "ArtBks" book
Artbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                         appearance=list(rhs=c("ArtBks=[0,1]")), 
                         control=list(verbose=F))
arules::inspect(Artbs_rules)


####Rules with rhs containing only "GeogBks" book
Geogbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                         appearance=list(rhs=c("GeogBks=[0,1]")), 
                         control=list(verbose=F))
arules::inspect(Geogbs_rules)


####Rules with rhs containing only "ItalCook" book
ItalCookbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                         appearance=list(rhs=c("ItalCook=[0,1]")), 
                         control=list(verbose=F))
arules::inspect(ItalCookbs_rules)


####Rules with rhs containing only "ItalAtlas" book
ItalAtlasbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                         appearance=list(rhs=c("ItalAtlas=[0,1]")), 
                         control=list(verbose=F))
arules::inspect(ItalAtlasbs_rules)



####Rules with rhs containing only "ItalArt" book
ItalArtbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                         appearance=list(rhs=c("ItalArt=[0,1]")), 
                         control=list(verbose=F))
arules::inspect(ItalArtbs_rules)



####Rules with rhs containing only "Florence" book
Florencebs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                         appearance=list(rhs=c("Florence=[0,1]")), 
                         control=list(verbose=F))
arules::inspect(Florencebs_rules)



##Rules with rhs containing only "ChildBks" & "YouthBks" books
CBYbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                    appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]")), 
                    control=list(verbose=F))
arules::inspect(CBYbs_rules)


##Rules with rhs containing only "ChildBks","YouthBks" & "CookBks" books
CBYCbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                       appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                       "CookBks=[0,1]")), 
                       control=list(verbose=F))
arules::inspect(CBYCbs_rules)


##Rules with rhs containing only "ChildBks","YouthBks","CookBks" & "DoItYBks" books
CBYCDoItYbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                        appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                              "CookBks=[0,1]","DoItYBks=[0,1]")), 
                        control=list(verbose=F))
arules::inspect(CBYCDoItYbs_rules)

##Rules with rhs containing only "ChildBks","YouthBks","CookBks","DoItYBks" & 
##"RefBks" books
CBYCDoItYRbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                             appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                                   "CookBks=[0,1]","DoItYBks=[0,1]",
                                                   "RefBks=[0,1]")), 
                             control=list(verbose=F))
arules::inspect(CBYCDoItYRbs_rules)


##Rules with rhs containing only "ChildBks","YouthBks","CookBks","DoItYBks", 
##"RefBks" & "ArtBks" books
CBYCDoItYRAbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                                appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                                      "CookBks=[0,1]","DoItYBks=[0,1]",
                                                      "RefBks=[0,1]","ArtBks=[0,1]")), 
                                control=list(verbose=F))
arules::inspect(CBYCDoItYRAbs_rules)



##Rules with rhs containing only "ChildBks","YouthBks","CookBks","DoItYBks", 
##"RefBks", "ArtBks" & "GeogBks" books
CBYCDoItYRAGbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                                   appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                                         "CookBks=[0,1]","DoItYBks=[0,1]",
                                                         "RefBks=[0,1]","ArtBks=[0,1]",
                                                         "GeogBks=[0,1]")), 
                                   control=list(verbose=F))
arules::inspect(CBYCDoItYRAGbs_rules)



##Rules with rhs containing only "ChildBks","YouthBks","CookBks","DoItYBks", 
##"RefBks", "ArtBks", "GeogBks" & "ItalCook" books
CBYCDoItYRAGICbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                                appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                                      "CookBks=[0,1]","DoItYBks=[0,1]",
                                                      "RefBks=[0,1]","ArtBks=[0,1]",
                                                      "GeogBks=[0,1]","ItalCook=[0,1]")), 
                                control=list(verbose=F))
arules::inspect(CBYCDoItYRAGICbs_rules)



##Rules with rhs containing only "ChildBks","YouthBks","CookBks","DoItYBks", 
##"RefBks", "ArtBks", "GeogBks","ItalCook" & "ItalAtlas"books

CBYCDoItYRAGICIAbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                                  appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                                        "CookBks=[0,1]","DoItYBks=[0,1]",
                                                        "RefBks=[0,1]","ArtBks=[0,1]",
                                                        "GeogBks=[0,1]","ItalCook=[0,1]",
                                                        "ItalAtlas=[0,1]")), 
                                  control=list(verbose=F))
arules::inspect(CBYCDoItYRAGICIAbs_rules)




##Rules with rhs containing only "ChildBks","YouthBks","CookBks","DoItYBks", 
##"RefBks", "ArtBks", "GeogBks","ItalCook","ItalAtlas" & ItalArt books

CBYCDoItYRAGICIAIArbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                                    appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                                          "CookBks=[0,1]","DoItYBks=[0,1]",
                                                          "RefBks=[0,1]","ArtBks=[0,1]",
                                                          "GeogBks=[0,1]","ItalCook=[0,1]",
                                                          "ItalAtlas=[0,1]","ItalArt=[0,1]")), 
                                    control=list(verbose=F))
arules::inspect(CBYCDoItYRAGICIAIArbs_rules)



##Rules with rhs containing only "ChildBks","YouthBks","CookBks","DoItYBks", 
##"RefBks", "ArtBks", "GeogBks","ItalCook","ItalAtlas",ItalArt & Florence books

CBYCDoItYRAGICIAIArFbs_rules <- apriori(Books,parameter =list(supp=0.1, conf=0.5), 
                                       appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                                             "CookBks=[0,1]","DoItYBks=[0,1]",
                                                             "RefBks=[0,1]","ArtBks=[0,1]",
                                                             "GeogBks=[0,1]","ItalCook=[0,1]",
                                                             "ItalAtlas=[0,1]","ItalArt=[0,1]",
                                                             "Florence=[0,1]")), 
                                       control=list(verbose=F))
arules::inspect(CBYCDoItYRAGICIAIArFbs_rules)


##Algorithms with Different values of support and confidence
rules1 <-  apriori(Books,parameter =list(supp=0.2, conf=0.70), 
                   appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                         "CookBks=[0,1]","DoItYBks=[0,1]",
                                         "RefBks=[0,1]","ArtBks=[0,1]",
                                         "GeogBks=[0,1]","ItalCook=[0,1]",
                                         "ItalAtlas=[0,1]","ItalArt=[0,1]",
                                         "Florence=[0,1]")), 
                   control=list(verbose=F))

arules::inspect(rules1)



rules2 <-  apriori(Books,parameter =list(supp=0.35, conf=0.90), 
                   appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                         "CookBks=[0,1]","DoItYBks=[0,1]",
                                         "RefBks=[0,1]","ArtBks=[0,1]",
                                         "GeogBks=[0,1]","ItalCook=[0,1]",
                                         "ItalAtlas=[0,1]","ItalArt=[0,1]",
                                         "Florence=[0,1]")), 
                   control=list(verbose=F))

arules::inspect(rules2)




rules3 <-  apriori(Books,parameter =list(supp=0.45, conf=0.95), 
                   appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                         "CookBks=[0,1]","DoItYBks=[0,1]",
                                         "RefBks=[0,1]","ArtBks=[0,1]",
                                         "GeogBks=[0,1]","ItalCook=[0,1]",
                                         "ItalAtlas=[0,1]","ItalArt=[0,1]",
                                         "Florence=[0,1]")), 
                   control=list(verbose=F))

arules::inspect(rules3)



rules4 <-  apriori(Books,parameter =list(supp=0.70, conf=0.97), 
                   appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                         "CookBks=[0,1]","DoItYBks=[0,1]",
                                         "RefBks=[0,1]","ArtBks=[0,1]",
                                         "GeogBks=[0,1]","ItalCook=[0,1]",
                                         "ItalAtlas=[0,1]","ItalArt=[0,1]",
                                         "Florence=[0,1]")), 
                   control=list(verbose=F))

arules::inspect(rules4)



rules5 <-  apriori(Books,parameter =list(supp=0.95, conf=0.97), 
                   appearance=list(rhs=c("ChildBks=[0,1]","YouthBks=[0,1]",
                                         "CookBks=[0,1]","DoItYBks=[0,1]",
                                         "RefBks=[0,1]","ArtBks=[0,1]",
                                         "GeogBks=[0,1]","ItalCook=[0,1]",
                                         "ItalAtlas=[0,1]","ItalArt=[0,1]",
                                         "Florence=[0,1]")), 
                   control=list(verbose=F))

arules::inspect(rules5)



##Visualization of above built all rules
install.packages("arulesViz")
install.packages("grid")
library(arulesViz)

plot(sorted_booksrules, main="Sorted rules by lift ratio")
plot(CB_rules,main="child books rules")
plot(Youthbs_rules, main="Youth books rules")
plot(Cookbs_rules, main="Cooks books rules" )
plot(DoItYbs_rules, main="DoItY books rules")
plot(Refbs_rules, main="Reference books rules")
plot(Artbs_rules, main="Art books rules")
plot(Geogbs_rules, main="Geogrophy books rules")
plot(ItalCookbs_rules,main="ItalCook books rules")
plot(ItalAtlasbs_rules, main="ItalAtlas books rules" )
plot(ItalArtbs_rules, main="ItalArt books rules" )
plot(Florencebs_rules,main="Florence books rules" )



plot(CBYbs_rules,main="ChildBks & YouthBks books rules")
plot(CBYCbs_rules,mmain ="ChildBks,YouthBks & CookBks books rules")
plot(CBYCDoItYbs_rules,main ="ChildBks,YouthBks,CookBks & DoItYBks books")
plot(CBYCDoItYRbs_rules,main ="ChildBks,YouthBks,CookBks,DoItYBks & RefBks books rules")
plot(CBYCDoItYRAbs_rules,main = "ChildBks,YouthBks,CookBks,DoItYBks,RefBks & ArtBks books rules")
plot(CBYCDoItYRAGbs_rules,main ="ChildBks,YouthBks,CookBks,DoItYBks,RefBks,ArtBks & GeogBks books rules" )
plot(CBYCDoItYRAGICbs_rules,main="ChildBks,YouthBks,CookBks,DoItYBks,RefBks,ArtBks,GeogBks & ItalCook books rules")
plot(CBYCDoItYRAGICIAbs_rules,main ="ChildBks,YouthBks,CookBks,DoItYBks,RefBks,ArtBks,GeogBks,ItalCook & ItalAtlas books rules" )
plot(CBYCDoItYRAGICIAIArbs_rules,main ="ChildBks,YouthBks,CookBks,DoItYBks,RefBks,ArtBks,GeogBks,ItalCook,ItalAtlas & ItalArts books rules")
plot(CBYCDoItYRAGICIAIArFbs_rules,main ="ChildBks,YouthBks,CookBks,DoItYBks,RefBks,ArtBks,GeogBks,ItalCook,ItalAtlas,ItalArts & Florence books rules" )

plot(rules1)
plot(rules2)
plot(rules3)
plot(rules4)
plot(rules5)



