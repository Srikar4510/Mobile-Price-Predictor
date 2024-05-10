train <-read_csv("~/IDA_Project/train.csv")
View(train)
head(train)

df <- train

dim_info <- dim(df) # dimention
rows <- dim_info[1]
col <- dim_info[2]

cat("Dimentions of the dataset: " , dim_info,"\n")
cat("Rows:", rows, "\n")
cat("Columns:", col, "\n") 

summary(df[, -ncol(df)]) # selects all columns except the target column

# our analysis :
# The above table (train) displays:
# Each feature contains 2000 data recorded.
# There are some numerical features in the dataset including 
# m_dep, px height, and sc_w that their min values don't make sense.


unique_counts <- sapply(df, function(col) length(unique(col))) # sapply =  applies function to each element of dataframe 
cat(unique_counts)

column_data_types <- sapply(df, class)
print(column_data_types)

column_data_types <- data.frame(Type = sapply(df, typeof))
print(column_data_types)

# our analysis

count97 <- length(df$m_dep)       
mean_value97 <- sum(df$m_dep) / count97  
std_dev97 <- sqrt(sum((df$m_dep - mean_value97)^2) / (count97 - 1)) 
min_value97 <- min(df$m_dep)
q1_97 <- quantile(df$m_dep, 0.25)
median_value97 <- quantile(df$m_dep, 0.5)
q3_97 <- quantile(df$m_dep, 0.75)
max_value97 <- max(df$m_dep)

cat("Count: ", count97, "\n")
cat("Mean: ", mean_value97, "\n")
cat("Standard Deviation: ", std_dev97, "\n")
cat("Minimum: ", min_value97, "\n")
cat("25th Percentile (Q1): ", q1_97, "\n")
cat("Median (50th Percentile): ", median_value97, "\n")
cat("75th Percentile (Q3): ", q3_97, "\n")
cat("Maximum: ", max_value97, "\n")


#Min,1st Qu,Median,Mean,3rd Qu,Max,NA's   

# outcome:
# According to the text as well as the reference below,
# I will consider the minimum Mobile Depth to be 0.5 centimeters.

count7 <- length(df$px_height)

mean_value7 <- sum(df$px_height) / count7

std_dev7 <- sqrt(sum((df$px_height - mean_value7)^2) / (count7 - 1))

min_value7 <- min(df$px_height)

q1_7 <- quantile(df$px_height, 0.25)

median_value7 <- quantile(df$px_height, 0.5)

q3_7 <- quantile(df$px_height, 0.75)

max_value7 <- max(df$px_height)

cat("Count: ", count7, "\n")
cat("Mean: ", mean_value7, "\n")
cat("Standard Deviation: ", std_dev7, "\n")
cat("Minimum: ", min_value7, "\n")
cat("25th Percentile (Q1): ", q1_7, "\n")
cat("Median (50th Percentile): ", median_value7, "\n")
cat("75th Percentile (Q3): ", q3_7, "\n")
cat("Maximum: ", max_value7, "\n")


# outcome:
# 

count17 <- length(df$sc_w)       

mean_value17 <- sum(df$sc_w) / count12  

# Calculate standard deviation
std_dev17 <- sqrt(sum((df$sc_w - mean_value12)^2) / (count12 - 1)) 

min_value17 <- min(df$sc_w)

q1_17 <- quantile(df$sc_w, 0.25)

median_value17 <- quantile(df$sc_w, 0.5)

q3_17 <- quantile(df$sc_w, 0.75)

max_value17 <- max(df$sc_w)

cat("Count: ", count17, "\n")
cat("Mean: ", mean_value17, "\n")
cat("Standard Deviation: ", std_dev17, "\n")
cat("Minimum: ", min_value17, "\n")
cat("25th Percentile (Q1): ", q1_17, "\n")
cat("Median (50th Percentile): ", median_value17, "\n")
cat("75th Percentile (Q3): ", q3_17, "\n")
cat("Maximum: ", max_value17, "\n")

levels(df$battery_power)

# DATA CLEANING



TARGET <- 'price_range'
FEATURES <- setdiff(names(df), c("df", TARGET))

cat_features <- names(df)[sapply(df, function(col) length(unique(col)) < 25)]
cont_features <- names(df)[sapply(df, function(col) length(unique(col)) >= 25)]

num_cat_features <- length(cat_features)
num_cont_features <- length(cont_features)

cat("Total number of features except for the target: ", length(FEATURES), "\n")
cat("Number of categorical (<25 Unique Values) features: ", num_cat_features, "\n")
cat("Number of continuous features: ", num_cont_features, "\n")

# Define labels and colors for the pie chart
labels <- c("Categorical (<25 Unique Values)", "Continuous")
colors <- c("#bbddcb", "#cdddbb")

# Create a pie chart
pie(c(num_cat_features, num_cont_features), labels = labels, col = colors, main = "Distribution of Categorical and Continuous Features", cex = 0.8)


# Data Imbalance

value_counts <- table(df$price_range)

# Define the label strings
labels <- c('Very high cost', 'High cost', 'Medium cost', 'Low cost')

# Define the colors for each pie slice
colors <- c('#4d9b68', '#538e8a', '#468e71', '#59ae8c')

# Create a pie chart
pie_data <- value_counts
pie_percent <- 100 * pie_data / sum(pie_data)
label_with_percent <- paste(labels, format(pie_percent, digits = 2), "%", sep = " - ")

# Calculate label positions
label_positions <- cumsum(pie_percent) - pie_percent / 2

# Create a pie chart with labels and count information
pie(pie_data, labels = label_with_percent, col = colors, main = "Balanced or Imbalanced?")

# Add count information under the percentage
text(label_positions, -1.5, value_counts, cex = 0.7)


# EDA ::::l:l:l::::-



# Univariate Analysis

# Exploring Categorical Features

# Assuming df is your data frame
library(ggplot2)

# Categorical columns
categorical_columns <- c('blue', 'dual_sim', 'four_g', 'n_cores', 'three_g', 'touch_screen', 'wifi')

# Create a long format of the data for plotting
df_long <- reshape2::melt(df[, categorical_columns])

# Create a count plot for each categorical variable
count_plot <- ggplot(df_long, aes(value, fill = variable)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Count Plot of Categorical Variables",
       x = "Category", y = "Count") +
  theme_minimal() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2)

# Print the combined count plot
print(count_plot)


# with price range :

install.packages("gridExtra")
library(ggplot2)

# Assuming 'df' is your data frame
# Assuming 'price_range' is a categorical variable

# Convert 'price_range' to a factor if it's not already
df$price_range <- as.factor(df$price_range)

# Specify the categorical columns
categorical_columns <- c('blue', 'dual_sim', 'four_g', 'n_cores', 'three_g', 'touch_screen', 'wifi')

# Create count plots for each specified categorical column
plots <- lapply(categorical_columns, function(col) {
  ggplot(df, aes(x = .data[[col]], fill = price_range)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Count Plot of", col), x = col, y = "Count") +
    theme_minimal()
})

# Arrange and display the plots
grid.arrange(grobs = plots, ncol = 3)




# Exploring Numerical Features

# boxplots :-

# Assuming df is your data frame
library(ggplot2)

# Numeric columns


library(gridExtra)

library(ggplot2)

# Assuming 'df' is your data frame
# 'numeric_columns' is a vector of numerical column names
numeric_columns <- c('battery_power', 'clock_speed', 'fc', 'int_memory', 'm_dep', 'mobile_wt', 'pc', 'px_height', 'px_width', 'ram', 'sc_h', 'sc_w', 'talk_time')

# Convert 'price_range' to a factor if it's not already
df$price_range <- as.factor(df$price_range)

# Create boxplots for each numerical column
plots <- lapply(numeric_columns, function(col) {
  ggplot(df, aes(x = price_range, y = .data[[col]], fill = price_range)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", col), x = "Price Range") +
    theme_minimal()
})

# Arrange and display the plots
do.call(grid.arrange, c(plots, ncol = 3))


# KDE PLOTS :-

library(ggplot2)

numeric_columns <- c('battery_power', 'clock_speed', 'fc', 'int_memory', 'm_dep', 'mobile_wt', 'pc', 'px_height', 'px_width', 'ram', 'sc_h', 'sc_w', 'talk_time')

df$price_range <- as.factor(df$price_range)

# Create KDE plots for each numerical column
plots <- lapply(numeric_columns, function(col) {
  ggplot(df, aes(x = .data[[col]], fill = price_range)) +
    geom_density(alpha = 0.7) +
    labs(title = paste("KDE Plot of", col), x = col, y = "Density") +
    theme_minimal()
})

# Arrange and display the plots
grid.arrange(grobs = plots, ncol = 3)





# 'result' will contain a long-format data frame with quartiles and mean for each numerical column by 'price_range'


# BIVARIATE ANALYSIS


count <- length(df$clock_speed)      

mean_value <- sum(df$clock_speed) / count


std_dev <- sqrt(sum((df$clock_speed - mean_value)^2) / (count - 1))

min_value <- min(df$clock_speed)

q1 <- quantile(df$clock_speed, 0.25)

median_value <- quantile(df$clock_speed, 0.5)

q3 <- quantile(df$clock_speed, 0.75)

max_value <- max(df$clock_speed)

cat("Count: ", count, "\n")
cat("Mean: ", mean_value, "\n")
cat("Standard Deviation: ", std_dev, "\n")
cat("Minimum: ", min_value, "\n")
cat("25th Percentile (Q1): ", q1, "\n")
cat("Median (50th Percentile): ", median_value, "\n")
cat("75th Percentile (Q3): ", q3, "\n")
cat("Maximum: ", max_value, "\n")

# clock_speed based on price_range

install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x = as.factor(clock_speed), fill = as.factor(price_range))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "Clock Speed", y = "Count") +
  ggtitle("Grouped Bar Plot of Clock Speed by Price Range") +
  scale_fill_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "purple")) +
  theme_minimal()

# Observations:
# The above chart illustrates that the phones with a clock_speed of 0.5 contains the highest 
# count among all mobile phones.



count1 <- length(df$fc)      
mean_value1 <- sum(df$fc) / count
std_dev1 <- sqrt(sum((df$fc - mean_value)^2) / (count - 1))
min_value1 <- min(df$fc)
q1_ <- quantile(df$fc, 0.25)
median_value1 <- quantile(df$fc, 0.5)
q3_ <- quantile(df$fc, 0.75)
max_value1 <- max(df$fc)

cat("Count: ", count1, "\n")
cat("Mean: ", mean_value1, "\n")
cat("Standard Deviation: ", std_dev1, "\n")
cat("Minimum: ", min_value1, "\n")
cat("25th Percentile (Q1): ", q1_, "\n")
cat("Median (50th Percentile): ", median_value1, "\n")
cat("75th Percentile (Q3): ", q3_, "\n")
cat("Maximum: ", max_value1, "\n")


ggplot(df, aes(x = as.factor(fc), fill = as.factor(price_range))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "fc", y = "Count") +
  ggtitle("Grouped count Plot of fc by Price Range") +
  scale_fill_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "purple")) +
  theme_minimal()

# Observations:
# The above chart illustrates that with the increase in the value of fc from 0-19, their count will decrease. This indicates that as the front camera becomes more powerful,
# the number of phones decreases.

# int_memory :

count11 <- length(df$int_memory)       

mean_value11 <- sum(df$int_memory) / count


std_dev11 <- sqrt(sum((df$int_memory - mean_value)^2) / (count - 1))

min_value11 <- min(df$int_memory)

q1__ <- quantile(df$int_memory, 0.25)

median_value11 <- quantile(df$int_memory, 0.5)

q3__ <- quantile(df$int_memory, 0.75)

max_value11 <- max(df$int_memory)

cat("Count: ", count11, "\n")
cat("Mean: ", mean_value11, "\n")
cat("Standard Deviation: ", std_dev11, "\n")
cat("Minimum: ", min_value11, "\n")
cat("25th Percentile (Q1): ", q1__, "\n")
cat("Median (50th Percentile): ", median_value11, "\n")
cat("75th Percentile (Q3): ", q3__, "\n")
cat("Maximum: ", max_value1, "\n")


library(ggplot2)

# Create a countplot
ggplot(df, aes(x = int_memory, fill = price_range)) +
  geom_bar(position = "dodge") +
  labs(x = "int_memory", y = "Count") +
  ggtitle("Countplot of int_memory ") +
  scale_fill_discrete(name = "Price Range")

# Observations:
# Mobile phones with 27 gigabytes of int_memory with the value of 64 have the highest count among all phones.


# m_dep:

count2 <- length(df$fc)    

mean_value2 <- sum(df$fc) / count


std_dev2 <- sqrt(sum((df$fc - mean_value)^2) / (count - 1))

min_value2 <- min(df$fc)

quantile1 <- quantile(df$fc, 0.25)

median_value2 <- quantile(df$fc, 0.5)

quantile3 <- quantile(df$fc, 0.75)

max_value2 <- max(df$fc)

cat("Count: ", count2, "\n")
cat("Mean: ", mean_value2, "\n")
cat("Standard Deviation: ", std_dev2, "\n")
cat("Minimum: ", min_value2, "\n")
cat("25th Percentile (Q1): ", quantile1, "\n")
cat("Median (50th Percentile): ", median_value2, "\n")
cat("75th Percentile (Q3): ", quantile3, "\n")
cat("Maximum: ", max_value2, "\n")


ggplot(df, aes(x = as.factor(m_dep), fill = as.factor(price_range))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "fc", y = "Count") +
  ggtitle("Grouped count Plot of m_dep by Price Range") +
  scale_fill_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "purple")) +
  theme_minimal()

# Observations:
# Mobile phones with 0.5 cm of m_dep have the highest count among all phones.

# screen_width :

count12 <- length(df$sc_w)       

mean_value12 <- sum(df$sc_w) / count12  

# Calculate standard deviation
std_dev12 <- sqrt(sum((df$sc_w - mean_value12)^2) / (count12 - 1)) 

min_value12 <- min(df$sc_w)

q1_12 <- quantile(df$sc_w, 0.25)

median_value12 <- quantile(df$sc_w, 0.5)

q3_12 <- quantile(df$sc_w, 0.75)

max_value12 <- max(df$sc_w)

cat("Count: ", count12, "\n")
cat("Mean: ", mean_value12, "\n")
cat("Standard Deviation: ", std_dev12, "\n")
cat("Minimum: ", min_value12, "\n")
cat("25th Percentile (Q1): ", q1_12, "\n")
cat("Median (50th Percentile): ", median_value12, "\n")
cat("75th Percentile (Q3): ", q3_12, "\n")
cat("Maximum: ", max_value12, "\n")

# Now, let's create the grouped bar plot
ggplot(df, aes(x = as.factor(sc_w), fill = as.factor(price_range))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "sc_w", y = "Count") +
  ggtitle("Grouped Count Plot of sc_w by Price Range") +
  scale_fill_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "purple")) +
  theme_minimal()

# Observations:
#  The above chart demonstrates that with the increase in the value of sc_w from 2.54-18, their count will decrease. This indicates that as the Screen Width becomes bigger, the number of phones decreases.
# In general, mobile phones with 2.54 cm or 1-inch of sc_w have the highest count among all phones.

# talk_time :
  
count123 <- length(df$talk_time)      

mean_value123 <- sum(df$talk_time) / count123  


std_dev123 <- sqrt(sum((df$talk_time - mean_value123)^2) / (count123 - 1))  # Fix the variable name

min_value123 <- min(df$talk_time)

q1_123 <- quantile(df$talk_time, 0.25)

median_value123 <- quantile(df$talk_time, 0.5)

q3_123 <- quantile(df$talk_time, 0.75)

max_value123 <- max(df$talk_time)

cat("Count: ", count123, "\n")
cat("Mean: ", mean_value123, "\n")
cat("Standard Deviation: ", std_dev123, "\n")
cat("Minimum: ", min_value123, "\n")
cat("25th Percentile (Q1): ", q1_123, "\n")
cat("Median (50th Percentile): ", median_value123, "\n")
cat("75th Percentile (Q3): ", q3_123, "\n")
cat("Maximum: ", max_value123, "\n")

# Now, let's create the grouped bar plot
ggplot(df, aes(x = as.factor(talk_time), fill = as.factor(price_range))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(x = "talk_time", y = "Count") +
  ggtitle("Grouped Count Plot of talk_time by Price Range") +
  scale_fill_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "purple")) +
  theme_minimal()

# Observations:
# The range of talk_time varies from 2 to 20.
# Mobile phones with talk_time 4 with a Low-cost price range have the highest count among all phones.

# RAM :

library(ggplot2)
ggplot(df, aes(x = ram, y = price_range, color = as.factor(price_range))) +
  geom_point() +
  labs(x = "Ram", y = "Price Range") +
  ggtitle("Scatter Plot of Ram vs Price Range") +
  scale_color_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "purple")) +
  theme_minimal()

# Observations:
# By increasing the value of ram from 256-4000 megabytes, the price range will increase.

# MULTIVARIATE


# Scatter Plot of Internal Memory vs Ram by Price Range

ggplot(df, aes(x = int_memory, y = ram, color = as.factor(price_range))) +
  geom_point() +
  labs(x = "Internal Memory (GB)", y = "Ram (MB)") +
  ggtitle("Scatter Plot of Internal Memory vs Ram by Price Range") +
  scale_color_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "purple")) +
  theme_minimal() +
  facet_wrap(~ as.factor(price_range), scales = "free_y", ncol = 1)

#

# Scatter Plot of Battery Power vs Ram by Price Range


ggplot(df, aes(x = battery_power, y = ram, color = as.factor(price_range))) +
  geom_point() +
  labs(x = "Battery Power", y = "Ram (MB)") +
  ggtitle("Scatter Plot of Battery Power vs Ram by Price Range") +
  scale_color_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "purple")) +
  theme_minimal() +
  facet_wrap(~ as.factor(price_range), scales = "free_y", ncol = 1)

# all colors in 1 plot
ggplot(df, aes(x = battery_power, y = ram, color = as.factor(price_range))) +
  geom_point() +
  labs(x = "Battery Power", y = "Ram (MB)") +
  ggtitle("Scatter Plot of Battery Power vs Ram by Price Range") +
  scale_color_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "purple")) +
  theme_minimal()

# Observations:
# Battery power does not necessarily improve by increasing the Ram of the mobile phones.

# Scatter Plot of Pixel Width vs Pixel Height by Price Range

ggplot(df, aes(x = px_width, y = px_height, color = as.factor(price_range))) +
  geom_point() +
  labs(x = "Pixel Width", y = "Pixel Height") +
  ggtitle("Scatter Plot of Pixel Width vs Pixel Height by Price Range") +
  scale_color_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "purple")) +
  theme_minimal()

# Observations:
# Generally, Display Resolution illustrates by increasing Pixel Height, the Pixel Width will increase


# Scatter Plot of Screen Width vs Screen Height by Price Range

ggplot(df, aes(x = sc_w, y = sc_h, color = as.factor(price_range))) +
  geom_point() +
  labs(x = "Screen Width", y = "Screen Height") +
  ggtitle("Scatter Plot of Screen Width vs Screen Height by Price Range") +
  scale_color_manual(values = c("0" = "red", "1" = "blue", "2" = "green", "3" = "purple")) +
  theme_minimal()

# Observations:
# Screen Size illustrates by increasing Screen Height, the Screen Width will increase.

# REVISING ON RAM :

# scatter

library(ggplot2)
library(viridis)
ggplot(df, aes(x = ram, y = four_g, color = as.factor(price_range))) +
  geom_point(position = "jitter", size = 3) +
  labs(x = "Ram", y = "Four G", color = "Price Range") +
  ggtitle("Strip Plot of Ram vs Four G by Price Range") +
  scale_color_manual(values = viridis(4)) +
  theme_minimal()

# boxplot

ggplot(df, aes(x = as.factor(price_range), y = ram, fill = as.factor(four_g))) +
  geom_boxplot() +
  labs(x = "Price Range", y = "Ram", fill = "Four G") +
  ggtitle("Box Plot of Ram by Price Range and Four G") +
  scale_fill_manual(values = viridis(4)) +
  theme_minimal()

# WIFI :

ggplot(df, aes(x = ram, y = wifi, color = as.factor(price_range))) +
  geom_point(position = position_jitter(width = 0.2), size = 3) +
  labs(x = "Ram", y = "Wifi", color = "Price Range") +
  ggtitle("Strip Plot of Wifi vs Ram by Price Range") +
  scale_color_manual(values = viridis(4)) +
  theme_minimal() +
  theme(legend.position="bottom")

# BOXPLOT

ggplot(df, aes(x = as.factor(price_range), y = ram, fill = as.factor(wifi))) +
  geom_boxplot() +
  labs(x = "Price Range", y = "Ram", fill = "Wifi") +
  ggtitle("Box Plot of Ram by Price Range and Wifi") +
  scale_fill_manual(values = viridis(4)) +
  theme_minimal()

# Observations:
# Overall, we observe that increasing the RAM in mobile phones leads to an increase in price range across all categorical features.





# Missing Values :-
missing_values <- colSums(is.na(df))
print("Number of missing values in each column:")
print(missing_values)

 df <- na.omit(df)

# Print the number of missing values after removal
print("Number of missing values after removal:")
print(colSums(is.na(df)))


# duplicated rows :-

duplicated_rows <- sum(duplicated(df))
print("Number of duplicated rows:")
print(duplicated_rows)

# Remove duplicated rows
df <- df[!duplicated(df), ]

# Print the number of duplicated rows after removal
print("Number of duplicated rows after removal:")
print(sum(duplicated(df)))


# redundancy


# Remove the 'id' column if it exists
if ("id" %in% names(df)) {
  df <- subset(df, select = -id)
} else {
  print("The 'id' column does not exist in the data frame.")
}


# outliers 

# Assuming df is your data frame
numerical_columns <- df[, c('battery_power', 'clock_speed', 'fc', 'int_memory', 'm_dep', 'mobile_wt', 'pc', 'px_height', 'px_width', 'ram', 'sc_h', 'sc_w', 'talk_time')]

list_1 <- df[, c('battery_power', 'clock_speed', 'fc', 'int_memory', 'm_dep', 'mobile_wt')]

par(mfrow=c(2, 3))
for (col in colnames(list_1)) {
  boxplot(list_1[[col]], main=col, col="skyblue", border="black", notch=TRUE)
}

# Assuming df is your dataframe
list_2 <- df[, c('pc', 'px_height', 'px_width', 'ram', 'sc_h', 'sc_w', 'talk_time')]

# Create boxplots for each column in list_2
par(mfrow=c(2, 4))
for (col in colnames(list_2)) {
  boxplot(list_2[[col]], main=col, col="skyblue", border="black", notch=TRUE)
}

# detecting outliers :-


# Assuming num_cols is your data frame with numerical columns
num_cols <- df[, c('battery_power', 'clock_speed', 'fc', 'int_memory', 'm_dep', 'mobile_wt', 'pc', 'px_height', 'px_width', 'ram', 'sc_h', 'sc_w', 'talk_time')]

# Function to detect outliers using IQR method

detect_outliers <- function(column) {
  Q1 <- quantile(column, 0.25)
  Q3 <- quantile(column, 0.75)
  IQR <- Q3 - Q1
  return(column < (Q1 - 1.5 * IQR) | column > (Q3 + 1.5 * IQR))
}
outliers <- apply(num_cols, 2, detect_outliers)
num_outliers <- colSums(outliers)
print("Number of outliers for each variable:")
print(num_outliers)


# Observation:

# While the boxplots in the table above indicate the presence of outliers in the fc and px_height features,
# we cannot justify removing them from the dataset without a strong rationale to do so. Therefore, we have decided to retain these outliers in our analysis.




num_cols <- df[, c('battery_power', 'clock_speed', 'fc', 'int_memory', 'm_dep', 'mobile_wt', 'pc', 'px_height', 'px_width', 'ram', 'sc_h', 'sc_w', 'talk_time')]
cat_cols <- df[, c('blue', 'dual_sim', 'four_g', 'n_cores', 'three_g', 'touch_screen', 'wifi')]

numeric_columns <- df[, c('battery_power', 'clock_speed', 'fc', 'int_memory', 'm_dep', 'mobile_wt', 'pc', 'px_height', 'px_width', 'ram', 'sc_h', 'sc_w', 'talk_time')]
categorical_columns <- df[, c('blue', 'dual_sim', 'four_g', 'n_cores', 'three_g', 'touch_screen', 'wifi')]


#  noises

numerical_columns <- c('battery_power', 'clock_speed', 'fc', 'int_memory', 'm_dep', 'mobile_wt', 'pc', 'px_height', 'px_width', 'ram', 'sc_h', 'sc_w', 'talk_time')

num_cols <- df[, numerical_columns]

pairs(num_cols, pch = ".", col = "red", cex = 0.5)

# Observation:-
# It seems that there isn't any Noisy data in the train dataset.


# Continuos and Categorical Data Distribution




# CORRELATION :


# Specify the numeric columns
numeric_columns <- c('battery_power', 'clock_speed', 'fc', 'int_memory', 'm_dep', 'mobile_wt', 'pc', 'px_height', 'px_width', 'ram', 'sc_h', 'sc_w', 'talk_time')

# Additional categorical columns to be converted to numeric
categorical_columns <- c('blue', 'dual_sim', 'four_g', 'n_cores', 'three_g', 'touch_screen', 'wifi')

# Convert categorical columns to numeric using as.numeric
df[, categorical_columns] <- lapply(df[, categorical_columns], as.numeric)

# Combine both numeric and converted categorical columns
all_numeric_columns <- c(numeric_columns, categorical_columns)

# Calculate correlation with 'price_range'
correlations <- cor(df[, all_numeric_columns], df$price_range)

# Extract correlation values for 'price_range'
cor_with_price_range <- correlations['price_range']

# Print the correlation values
print(cor_with_price_range)


#Interpretation:
#There is a strong correlation between ram and price_range.
#In addition, the heatmap above indicates a moderate correlation between 4G and 3G, fc and pc, px_height and px_width, and sc_h and sc_w.





install.packages(c("corrplot", "reshape2"))
library(corrplot)
library(reshape2)

correlation_matrix <- cor(df)

melted_corr <- melt(correlation_matrix)

par(mar=c(2,2,2,2))  

corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("blue", "white", "firebrick3"))(20),
         type = "full", order = "hclust", tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7)

title(main = "Correlation Between The Features", cex.main = 1.2)

cor_matrix <- cor(df)


# Assuming df is your data frame
# Selecting specific columns for correlation analysis
selected_columns <- c('battery_power', 'clock_speed', 'fc', 'int_memory', 'm_dep', 'mobile_wt', 'pc', 'px_height', 'px_width', 'ram', 'sc_h', 'sc_w', 'talk_time', 'blue', 'dual_sim', 'four_g', 'n_cores', 'three_g', 'touch_screen', 'wifi')

# Creating a subset of the dataframe
subset_df <- df[selected_columns]

# Calculate the correlation matrix
cor_matrix <- cor(subset_df)

# Print the correlation matrix
print(cor_matrix)







# main splitting into train , test :

install.packages("rpart")
library(rpart)

set.seed(123)  
sample_size <- floor((2/3) * nrow(df))
train_indices <- sample(seq_len(nrow(df)), size = sample_size)

train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

dimen<-dim(test_data)
cat(dimen)

dimen<-dim(train_data)
cat(dimen)
# Define predictors (X) and target variable (y)
X_train <- train_data[, names(train_data) != 'price_range']
y_train <- train_data$price_range

X_test <- test_data[, names(test_data) != 'price_range']
y_test <- test_data$price_range

# Build the CART model
cart_model <- rpart(price_range ~ ., data = train_data, method = "class")

# Make predictions on the test set
predictions <- predict(cart_model, newdata = test_data, type = "class")

# Evaluate the model
confusion_matrix <- table(predictions, test_data$price_range)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Plot the decision tree (optional)
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(cart_model, main = "CART Decision Tree")


# accuracy , precision , recall , metrics :-


# Install and load necessary packages
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
library(rpart)
library(rpart.plot)
library(pROC)

confusion_matrix <- predict(cart_model, newdata = test_data, type = "class")

# Confusion Matrix
# Assuming 'predictions' is a vector of predicted values
# Assuming 'test_data$price_range' is the true labels

# Create a confusion matrix


conf_matrix <- table(confusion_matrix, test_data$price_range)
print("Confusion Matrix:")
print(conf_matrix)


# Calculate Predictive Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Predictive Accuracy:", accuracy))

# Calculate Precision, Recall, and F1-score
precision <- diag(conf_matrix) / rowSums(conf_matrix)
recall <- diag(conf_matrix) / colSums(conf_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-Score:", f1_score))

# ROC Curve
roc_curve <- roc(test_data$price_range, as.numeric(predictions))
auc_value <- auc(roc_curve)
plot(roc_curve, main = paste("ROC Curve (AUC =", auc_value, ")"), col = "blue", lwd = 2)




























