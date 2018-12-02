setwd('Spring 18/Data Mining/project')
library(readr)
library(arules)
library(arulesViz)


dataset <- read_csv("processed/covtype_categorical_small_binned.csv")
summary(dataset)
data = dataset[,11:13]
data <- dataset 
tr <- data
data <- data.frame(sapply(data,as.factor))


#write(data,"transactions1")
#data <- as(data,"transactions")

rules1 <- apriori(data,parameter = list(supp = 0.02, conf = 0.75)) 
rules1 <- sort(rules1,by = 'lift')
inspect(rules1)

measures = interestMeasure(rules1, c("lift", "chiSquare","imbalance", "cosine","jaccard","kulczynski", "maxConfidence"), data)
quality(rules1) <- cbind(quality(rules1),measures)
inspect(rules1)
write(rules1,
      file = "Association Rules/Cover_Type_3/association_rules_binned_3.csv",
      sep = ",",
      quote = TRUE,
      row.names = FALSE)
plot(rules1, measure=c("support","confidence"), shading="confidence")
subrules = head(rules1,n=10,by = 'lift')
plot(subrules,method = 'Graph',control=list(type = 'itemsets',main = 'Top 10 Rules'))

# 
# tr$size <- 3
# tr <- tr[,c(1,4,2,3)]
# tr <- data.frame(sapply(tr,as.factor))
# tr <- as(tr,"transactions")
# write.table(tr,"transactions")
# tr1 <- as(tr,"data.frame")
# inspect(head(tr))
# tra <- read_baskets(con = "transactions",info = c("eventID","sequenceID","SIZE"))
# s1 <- cspade(tra,parameter = list(support = 0.4))
# inspect(head(tra))
