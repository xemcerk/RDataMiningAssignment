library(magrittr) 
library(arules)
library(arulesViz)

##load Mushroom dataset
url <- "./data/mushroom/agaricus-lepiota.data"
mushrooms <- read.csv(file = url, header = FALSE)
names(mushrooms) <- c("class", "cap-shape", "cap-surface",
                      "cap-color", "bruises", "odor", "gill-attachment", "gill-spacing",
                      "gill-size", "gill-color", "stalk-shape", "stalk-root",
                      "stalk-surface-above-ring", "stalk-surface-below-ring",
                      "stalk-color-above-ring", "stalk-color-below-ring",
                      "veil-type", "veil-color", "ring-number", "ring-type",
                      "spore-print-color", "population", "habitat")

##Data Exploration
dim(mushrooms)#check out the dimensionality of the dataset
names(mushrooms)#check out the column names of the dataset
table(mushrooms$class, useNA="ifany")#check out the split of the two lable
str(mushrooms)#check out the structure of the dataset
attributes(mushrooms)#check out the attributes of the dataset
head(mushrooms,2)#check out the first 2 row of the dataset

#plot a pie chart to show the split of the dataset
library(dplyr)
tab <- mushrooms$class %>% table()
precentages <- tab %>% prop.table() %>% round(3) * 100
txt <- paste0(names(tab), '\n', precentages, '%')
pie(tab, labels=txt)

#plot a heat map of the similarity between samples
#dist.matrix <- as.matrix(dist(mushrooms[,2:23]))
#heatmap(dist.matrix)

##Mining association rules on dataset
rules <- apriori(mushrooms, control = list(verbose=F),
                 parameter = list(minlen=4, maxlen=5, confidence=1),
                 appearance = list(rhs=c("class=p", "class=e"),
                                   default="lhs"))
quality(rules) <- round(quality(rules), digits=3)

rules.sorted.bylif <- sort(rules, by="lift")
rulse.sorted.bysup <- sort(rules, by="support")
inspect(head(rules.sorted.bylif, 20))

attributes(rules.sorted.bylif)
attributes(rules.sorted.bysup)

plot(head(rules.sorted,n=20))
head(rules.sorted,n=12) %>% plot(method="grouped")
head(rules.sorted,n=12) %>% plot(method="graph",
                    control=list(layout=igraph::with_fr()))

plot(rules, measure=c("support","lift"), shading = "confidence")
plot(rules, method="matrix", measure="lift");

#prune redundant rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix) >= 1
rules.pruned <- rules[!redundant]
rules.pruned %>% inspect() ## print rules

attributes(rules.pruned)

##use random forest to calculate feature importance