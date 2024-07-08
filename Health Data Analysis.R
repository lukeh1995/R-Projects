install.packages("caret")
install.packages("randomForest")
install.packages("e1071")
install.packages("pROC")


#Install required packages
library(tidyverse)
library(ggplot2)
library(Hmisc) # For descriptive statistics
library(skimr) # For detailed summaries of datasets
library(caret)
library(randomForest)
library(e1071)
library(pROC)

#Get dataset
data <- read.csv("fetal_health.csv")

#Explore summary of data
summary(data)
str(data)
head(data)
describe(data)

#Check for missing values and summary stats
skim(data)

#Check for missing values
colSums(is.na(data))

#Convert output variable to factor
data$fetal_health <- as.factor(data$fetal_health)

#Create new column for actual fetal health
data <- data %>% 
  mutate(fetal_health_actual = case_when(
    fetal_health == "1" ~ "Normal",
    fetal_health == "2" ~ "Suspect",
    fetal_health == "3" ~ "Pathological"
  ))

#Create percentages across categories
fetal_count <- data %>%
  count(fetal_health_actual) %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

#Visualise fetal health accross categories
plot <- ggplot(data = data, aes(x = fetal_health_actual)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Proportion of Fetal Health Outcomes", x = "Fetal Health", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
plot <- plot + 
  geom_text(data = fetal_count, aes(x = fetal_health_actual, y = n, label = paste0(n, " (", round(percentage, 1), "%)")),
            vjust = -0.5, color = "black", size = 3.5)


