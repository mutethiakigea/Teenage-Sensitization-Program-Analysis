# importing data
data <- read.csv("C:/Users/Administrator/Downloads/TEST.csv")
View(data)


# Data cleaning
# 1) null values
summary (is.na(data))
data=na.omit(data)

# 2) duplicated rows
duplicated_rows=data[duplicated(data), ]
View(duplicated_rows)
data=unique(data)
View(data)


#Exploratory Data Analysis (EDA)
#Explore the structure of the dataset
# View the structure of the dataset
str(data)

#summary statistics
summary(data)

#histogram for age
hist(data$AGE)

#Scatterplot for ENG Vs MATHS Scores
plot(data$ENG, data$MATHS)


# Data Analytics
# Example: Mean scores for each subject
colMeans(data[, c("ENG", "MATHS", "KISW", "BIO", "CHEM", "PHY", "GEO", "AGRI")])

# Example: Correlation matrix
cor(data[, c("AGE", "Days.attended.School", "ENG", "MATHS", "KISW", "BIO", "CHEM", "PHY", "GEO", "AGRI")])

# Create a new column 'Total' with the sum of scores for specified subjects
data$Total <- rowSums(data[, c("ENG", "MATHS", "KISW", "BIO", "CHEM", "PHY", "GEO", "AGRI")])

# View the updated dataset with the new 'Total' column
head(data)
View(data)

#Data Visualization
library(ggplot2)
ggplot(data, aes(x = AREA, y = ENG)) + geom_boxplot()


# Create a histogram to visualize the distribution of total scores
ggplot(data, aes(x = Total)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") +
  labs(title = "Distribution of Total Scores",
       x = "Total Scores",
       y = "Frequency")

# Create a density plot for total scores
ggplot(data, aes(x = Total)) +
  geom_density(fill = "green", color = "black") +
  labs(title = "Density of Total Scores", x = "Total Scores")

# Create a bar plot showing average total scores by area
ggplot(data, aes(x = AREA, y = Total, fill = AREA)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Total Scores by Area", x = "Area", y = "Average Total Scores")
#A bar plot can display the average total scores for different groups AREA for categorical variables to compare.

# Adding a dummy x-axis variable for the violin plot
data$dummy <- 1  # Creating a dummy variable

# Create a violin plot for total scores
ggplot(data, aes(x = as.factor(dummy), y = Total)) +
  geom_violin(fill = "red") +
  labs(title = "Violin Plot of Total Scores", x = "", y = "Total Scores") +
  scale_x_discrete(labels = NULL)  # Removing x-axis labels

# FORECASTING
# Example: Linear regression to predict English score based on other subjects
model <- lm(ENG ~ MATHS + KISW + BIO + CHEM + PHY + GEO + AGRI, data = data)
summary(model)
# Make predictions
new_data <- data.frame(MATHS = 60, KISW = 70, BIO = 75, CHEM = 80, PHY = 85, GEO = 90, AGRI = 95) # Example data
predicted_scores <- predict(model, newdata = new_data)
View(predicted_scores)
