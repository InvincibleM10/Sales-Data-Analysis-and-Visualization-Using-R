#  Install Required Packages 
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("psych")
install.packages("corrplot")
install.packages("writexl")

# Load Libraries 
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(psych)
library(corrplot)
library(writexl)

#  Load Data 
data <- read_excel("C:\\Users\\meran\\Downloads\\sample datafor r.xlsx")

# Data Cleaning and Preparation 
# Remove unwanted column
data <- data %>% select(-`\\/`)

# Rename columns for easier access
data <- data %>% rename(
  OrderID = `Order ID`,
  CustomerID = `Cust ID`,
  AgeGroup = `Age Group`,
  City = `ship-city`,
  State = `ship-state`,
  PostalCode = `ship-postal-code`
)

# Remove rows with missing values
data <- na.omit(data)

# Convert data types
data$Date <- as.Date(data$Date)
data$Month <- factor(data$Month, levels = month.name)
data$Age <- as.numeric(data$Age)
data$Qty <- as.numeric(data$Qty)
data$Amount <- as.numeric(data$Amount)
data$Gender <- as.factor(data$Gender)
data$Channel <- as.factor(data$Channel)
data$Category <- as.factor(data$Category)
data$B2B <- as.factor(data$B2B)

#  Descriptive Statistics 
summary(data)
str(data)
describe(data %>% select(Age, Qty, Amount))

# Group-wise stats: Gender
data %>%
  group_by(Gender) %>%
  summarise(
    Avg_Age = mean(Age),
    Avg_Qty = mean(Qty),
    Avg_Amount = mean(Amount),
    Count = n()
  )

#  Visualizations

# Daily sales data
daily_sales <- data %>%
  group_by(Date) %>%
  summarise(Total_Amount = sum(Amount))

# Channel distribution data
channel_data <- data %>%
  count(Channel)

# Average amount by category
avg_amount_cat <- data %>%
  group_by(Category) %>%
  summarise(Avg_Amount = mean(Amount))

# Bar Chart: Orders by Gender
ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  scale_fill_manual(values = c("Male" = "#AEC6CF", "Female" = "#FFB6C1")) +
  labs(title = "Number of Orders by Gender", x = "Gender", y = "Count") +
  theme_minimal()

# Histogram: Age Distribution
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#BFD8B8", color = "white") +
  labs(title = "Customer Age Distribution", x = "Age", y = "Frequency") +
  theme_minimal()

# Boxplot: Amount by Gender
ggplot(data, aes(x = Gender, y = Amount, fill = Gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Male" = "#ADD8E6", "Female" = "#FFDAB9")) +
  labs(title = "Amount Spent by Gender", x = "Gender", y = "Amount") +
  theme_minimal()

# Line Plot: Sales Over Time
ggplot(daily_sales, aes(x = Date, y = Total_Amount)) +
  geom_line(color = "#9AD0EC", size = 1) +
  labs(title = "Total Sales Over Time", x = "Date", y = "Total Sales") +
  theme_minimal()

# Pie Chart: Channel Distribution
ggplot(channel_data, aes(x = "", y = n, fill = Channel)) +
  geom_col(width = 1) +
  coord_polar("y") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Sales Channel Distribution") +
  theme_void()

# Bar Chart: Avg Amount by Category
ggplot(avg_amount_cat, aes(x = reorder(Category, -Avg_Amount), y = Avg_Amount, fill = Category)) +
  geom_col() +
  scale_fill_brewer(palette = "Pastel2") +
  labs(title = "Average Amount by Category", x = "Category", y = "Avg Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter Plot + Regression Line: Amount vs Qty
ggplot(data, aes(x = Qty, y = Amount)) +
  geom_point(color = "#B39EB5") +
  geom_smooth(method = "lm", color = "#77DD77") +
  labs(title = "Linear Relationship: Quantity vs Amount", x = "Quantity", y = "Amount") +
  theme_minimal()

# Statistical Tests 
# T-test: Male vs Female Spending
t.test(Amount ~ Gender, data = data)

# T-test: B2B vs non-B2B Quantity
t.test(Qty ~ B2B, data = data)

# Correlation matrix
num_data <- data %>% select(Age, Qty, Amount)
cor_matrix <- cor(num_data, use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper")

#  Linear Regression 
model <- lm(Amount ~ Qty, data = data)
summary(model)

#  Scaling 
data$Amount_scaled <- scale(data$Amount)
data$Qty_scaled <- scale(data$Qty)
head(data[, c("Amount", "Amount_scaled", "Qty", "Qty_scaled")])

# Improved Scatter Plot with Regression Line: Quantity vs Amount
ggplot(data, aes(x = Qty, y = Amount)) +
  geom_jitter(width = 0.3, height = 0, color = "lightcoral", alpha = 0.6, size = 2) +  # Jitter avoids overlapping
  geom_smooth(method = "lm", color = "steelblue", se = TRUE, size = 1.2) +  # Regression line with confidence interval
  scale_x_continuous(name = "Quantity Ordered") +
  scale_y_continuous(name = "Amount Spent") +
  labs(title = "Scatter Plot: Quantity vs Amount with Linear Regression") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )
