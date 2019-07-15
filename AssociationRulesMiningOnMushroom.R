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

##Mine association rules on dataset
#Mine asscociation rules whose length range from 2 to 4
rules <- apriori(mushrooms, control = list(verbose=F),
                 parameter = list(minlen=2, maxlen=5, confidence=1),
                 appearance = list(rhs=c("class=p", "class=e"),
                                   default="lhs"))
quality(rules) <- round(quality(rules), digits=3)
#check out how many rules we get
nrow(quality(rules))

#prune redundant rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix) >= 1
rules.pruned <- rules[!redundant]
#check out how many rules left after pruning
nrow(quality(rules.pruned))

#Sort rules by lift and support and inspect the first 8 ones.
rules.pruned.sorted <- sort(rules.pruned, by=c("lift","support"))
inspect(head(rules.pruned.sorted, 8))

#use a scatter plot to show how the rules are distributed
plot(rules.pruned.sorted, measure=c("support","lift"))
plot(rules.pruned.sorted, method="matrix", measure=c("support","lift"))

#plot the graph of the rules to the association between them
head(rules.pruned.sorted,n=8) %>% plot(method="graph",
                    control=list(layout=igraph::in_circle()))

#limit the rules' length to 2 and mine again
rules2 <- apriori(mushrooms, control = list(verbose=F),
                 parameter = list(minlen=2, maxlen=2),
                 appearance = list(rhs=c("class=p", "class=e"),
                                   default="lhs"))
quality(rules2) <- round(quality(rules2), digits=3)

#sort by confidence, lift and support 
rules2.sorted <- sort(rules2, by=c("confidence","lift","support"))
inspect(head(rules2.sorted, 8))
