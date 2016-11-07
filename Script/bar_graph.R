library(ggplot2)
library(reshape2)
# load the CSV file from the local directory

dataset <- data.frame(
  meal = c("Breakfast", "Lunch","Dinner"),
  total_bill = c(10.5, 14.89, 17.23)
)


# Plot Model Accuracy
acimg <- ggplot(data=dataset, aes(x=meal, y=total_bill, colour=meal)) +
  geom_bar(stat="identity", width = 0.4) +
  ggtitle("Average bill for each meal a day") +     # Set title
  xlab("Meal") + ylab("Total Bill") + # Set axis labels
  theme(legend.position="top", plot.title = element_text(size = 10))

print(acimg)

