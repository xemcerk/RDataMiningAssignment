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

##Mining association rules on dataset
rules <- apriori(mushrooms, control = list(verbose=F),
                 parameter = list(minlen=2, maxlen=5),
                 appearance = list(rhs=c("class=p", "class=e"),
                                   default="lhs"))
quality(rules) <- round(quality(rules), digits=3)
rules.sorted <- sort(rules, by="confidence")
inspect(head(rules.sorted, 12))

plot(head(rules.sorted,n=20))
head(rules.sorted,n=12) %>% plot(method="grouped")
head(rules.sorted,n=12) %>% plot(method="graph",
                    control=list(layout=igraph::with_fr()))
