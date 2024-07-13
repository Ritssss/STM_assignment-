data1<- read.csv("C:\\Users\\Sriza\\Downloads\\csv_folder\\Property.csv")
data2<- read.csv("C:\\Users\\Sriza\\Downloads\\csv_folder\\Property_with_Feature_Engineering.csv")
head(data1)
head(data2)

library(ggplot2)
library(dplyr)
library(corrplot)


# Display the column names and data types
str(data2)

# Ensure columns are numeric
data2$price <- as.numeric(data2$price)
data2$area <- as.numeric(data2$area)
data2$bedrooms <- as.numeric(data2$bedrooms)
data2$baths <- as.numeric(data2$baths)
str(data2)
summary(data2)


#visualization
#Histogram of Property Prices
ggplot(data2, aes(x = price)) +
  geom_histogram(binwidth = 100000, fill = "blue", color = "black") +
  labs(title = "Distribution of Property Prices", x = "Price", y = "Frequency")

#Bar Plot of Property Types
ggplot(data2, aes(x = property_type)) +
  geom_bar(fill = "green") +
  labs(title = "Property Types Distribution", x = "Type", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Box Plot of Prices by Property Type
ggplot(data2, aes(x = property_type, y = price)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of Prices by Property Type", x = "Property Type", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
#Scatter Plot of Price vs. Area
ggplot(data2, aes(x = area, y = price)) +
  geom_point(color = "red") +
  labs(title = "Price vs. Area", x = "Area", y = "Price")

#Scatter Plot of Price vs. Bedrooms
ggplot(data2, aes(x = bedrooms, y = price)) +
  geom_point(color = "blue") +
  labs(title = "Price vs. Number of Bedrooms", x = "Number of Bedrooms", y = "Price")

#Data Cleaning(Handling missing values)
# Check for missing values
sum(is.na(data2))

# Handling missing values by removing rows with NA
data2 <- na.omit(data2)


# Boxplot to detect outliers in Price
ggplot(data2, aes(y = price)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of Property Prices", y = "Price")


#EDA and Statistical Methods

# Calculate correlation matrix
cor_matrix <- cor(data[, sapply(data, is.numeric)])
print(cor_matrix)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle")

# Summary statistics for numeric columns
summary(data[, sapply(data, is.numeric)])

# Average price by property type
avg_price_by_type <- data %>%
  group_by(property_type) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))

#Price Distribution by City
ggplot(data2, aes(x = city, y = price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Price Distribution by City", x = "City", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Statistical Analysis
#Linear Regression
# Linear regression model to predict Price based on Area
# Create the linear regression model
model <- lm(price ~ area_marla, data = data2)
summary(model)

# Plot the regression line
ggplot(data, aes(x = area_marla, y = price)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Price vs. Area with Regression Line", x = "Area", y = "Price")






