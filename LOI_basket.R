# Set working directory
setwd("C:\\LOI CSV")
getwd() #Directory set OK!

#Install arules library
install.packages("arules")
library(arules)

#Reproducibility of results
set.seed(1234)

#Load dataset
groceries <- read.transactions('MARKET-BASKET.csv', sep=",")

#Print Summary (Records, variables, max, min, mean, etc.)
summary(groceries)

#Check for duplicates [If any we should see a message 'distribution of transactions with duplicates']
groceries <- read.transactions('MARKET-BASKET.csv', sep=",", rm.duplicates = TRUE)
summary(groceries)

#Inspect 500th transaction
inspect(groceries[500])

#Plot items that appear in more than % of transactions (plot) Support >= 0.1
transaction <- itemFrequencyPlot(groceries, support=0.1)
dev.copy(png,'10percent_transactions.png') #We save the plot to working directory!
dev.off()

#Table form
#10% transactions
support_10 <- eclat (groceries, parameter = list(supp = 0.1, maxlen = 1))
# Convert support_10 object to a data frame
support_10_df <- as(support_10, "data.frame")
# Sort the data frame alphabetically to match the same order as shown in the plot
sorted_df <- support_10_df[order(support_10_df[,1]), ]
#Display on screen
print(sorted_df)


#Plot 10 most purchased articles
top10 <- itemFrequencyPlot(groceries, topN=10)
dev.copy(png,'top10.png') #We save the plot to working directory!
dev.off()

#Table form
#10 most purchased
top10 <- itemFrequency(groceries, type = "absolute")
head(sort(top10, decreasing = TRUE), n = 10)

#Apriori algorithm initialization
groceryrules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.45, minlen = 2))

#Examining association rules
groceryrules
#Examining statistical parameters
summary(groceryrules)
inspect(groceryrules)

#Rules sorted by lift:
rules_by_lift <- inspect(sort(groceryrules, by="lift"))
#Rules sorted by lift>2:
rules_by_lift[rules_by_lift$lift>2,]

#List association rules that contain 'soda' with lift > 2
filtered <- subset(groceryrules, items %in% "soda" & lift > 2)
inspect(filtered)

#Formulate an assignment yourself (question 4), support = 0.7% and confidence 52%.
custom_groceryrules <- apriori(groceries, parameter = list(support = 0.007, confidence = 0.52, minlen = 2))
inspect(custom_groceryrules)

#We print and sort by lift.
sorted <- inspect(sort(custom_groceryrules, by = "lift", decreasing = FALSE))

#List association rules that contain 'bread' & 'milk' with lift > 3
bread_rules <- subset(custom_groceryrules, items %ain% c("brown bread", "whole milk") & lift > 2)
bread_rules
inspect(bread_rules)

#Save rules as CSV
write(custom_groceryrules, file = "custom_grocery_rules.csv", sep = ",", quote = TRUE, row.names = FALSE)


#Plots and Charts

set.seed(1234) #Set seed for reproducibility
#Visualize the rules
install.packages("arulesViz")
library(arulesViz)

#Examining association rules
groceryrules

#Simple scatter plot for 70 rules, support vs confidence
plot(groceryrules)
dev.copy(png,'simple_plot_SvsC.png') #We save the plot to working directory!
dev.off()

#Simple scatter plot support vs lift
plot(groceryrules, measure = c("support", "lift"), shading ="confidence")
dev.copy(png,'simple_plot_SvsL.png') #We save the plot to working directory!
dev.off()

#Filter by lift > 6
sub_rules <- groceryrules[quality(groceryrules)$lift > 6]
#Check the content of the variable!
sub_rules

#Start plotting the rule with highest lift (>6)
plot(sub_rules,measure = c("support", "lift"), shading ="confidence")
dev.copy(png,'filter_by_lift.png') #We save the plot to working directory!
dev.off()


#Two-key scatter plot w/ order
plot(groceryrules, method = "two-key plot")
dev.copy(png,'two_key_plot_SvsC.png') #We save the plot to working directory!
dev.off()

#Same plot interactive function (inspect,filter,zoom, end)
plot(groceryrules, measure=c("support", "confidence"),shading = "lift", engine = "interactive")

#Scatter plot w/ plotly engine
plot(groceryrules, engine = "plotly")

#Same plot w/ HTML widget
plot(groceryrules, method = "graph", shading = "confidence", engine = "interactive", jitter = 0)

#Graph plot w/ ggplot2 engine
plot(groceryrules, method = "graph", shading = "confidence", engine = "ggplot2")
dev.copy(png,'graph_ggplot2.png') #We save the plot to working directory!
dev.off()

#Graph plot w/ grouped method
plot(groceryrules, method = "grouped")
dev.copy(png,'graph_grouped.png') #We save the plot to working directory!
dev.off()

#Matrix visualization with 3D bars
plot(groceryrules, method = "matrix", engine = "3d", measure = "lift")
dev.copy(png,'matrix_3D.png') #We save the plot to working directory!
dev.off()

#Matrix visualization with colored squares
plot(groceryrules, method = "matrix", measure = "lift")
dev.copy(png,'matrix_squares.png') #We save the plot to working directory!
dev.off()

#Graph w/ HTML engine
plot(groceryrules, method = "graph", engine = "html")

#Parallel coordinates plot
set.seed(1) #Set seed for reproducibility
plot(groceryrules, method = "paracoord")
dev.copy(png,'pcoord.png') #We save the plot to working directory!
dev.off()

#Parallel coordinates plot with reorder
set.seed(1)
plot(groceryrules, method = "paracoord", control = list(reorder = TRUE), main = "Parallel coordinates plot for 70 rules (Reordered Plot)")
dev.copy(png,'pcoord_reorder.png') #We save the plot to working directory!
dev.off()

#Filter subset rules by lift > 2
lift_subset <- subset(groceryrules, items %in% "soda" & lift >2)
plot(lift_subset, method = "graph")
dev.copy(png,'soda_by_lift.png') #We save the plot to working directory!
dev.off()

#Double-decker plot
set.seed(344) #Set seed for reproducibility
#We choose a rule from the soda subset
fourth_rule <- sample(lift_subset, 1)
inspect(fourth_rule)
plot(fourth_rule, method = "doubledecker", data =groceries)
dev.copy(png,'doubledecker_soda.png') #We save the plot to working directory!
dev.off()