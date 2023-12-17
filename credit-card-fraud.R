
library(data.table)

credit_card_Data <- data.table::fread("~/Downloads/archive (3)/fraudTest.csv")

# Print the first few rows of the data to check if it loaded correctly
View(credit_card_Data)

# Check for missing values in the entire data.table
missing_values <- sapply(credit_card_Data, function(x) sum(is.na(x)))
print(missing_values)
str(credit_card_Data$trans_date_trans_time)
credit_card_Data[, category := as.factor(category)]

# Extract separate 'date' and 'time' columns
credit_card_Data[, c('date', 'time') := list(as.Date(trans_date_trans_time), format(trans_date_trans_time, "%H:%M:%S"))]

# Extract separate 'month' and 'year' columns
credit_card_Data[, month := format(date, "%m")]
credit_card_Data[, year := format(date, "%Y")]

# Remove 'trans_num' and 'unix_time' columns
credit_card_Data[, c('trans_num', 'unix_time') := NULL]

credit_card_Data <- unique(credit_card_Data)

# What is the overall fraud rate?   ---- 0.3859 (out of 555719 transactions 2145 are fraud)
fraud_rate <- mean(credit_card_Data$is_fraud)
print(paste("Overall fraud rate:", fraud_rate))

#How does the fraud rate vary by transaction category?
fraud_rate_by_category <- credit_card_Data[, .(Fraud_Rate = mean(is_fraud)), by = category]
print(fraud_rate_by_category)

# Visualize fraud rates by category
ggplot(fraud_rate_by_category, aes(x = category, y = Fraud_Rate)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = round(Fraud_Rate, 2)), vjust = -0.3, size = 3) +  # Add labels with rounding
  labs(title = "Fraud Rate by Transaction Category", x = "Category", y = "Fraud Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()


#Are there any patterns in the timing of fraudulent transactions?
# Visualize the timing of fraudulent transactions with larger binwidth

ggplot(credit_card_Data[is_fraud == 1], aes(x = trans_date_trans_time)) +
  geom_histogram(binwidth = 3600, fill = "red", color = "black") +
  labs(title = "Timing of Fraudulent Transactions", x = "Transaction Date & Time", y = "Count") +
  theme_minimal() +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y")

#Is there a correlation between transaction amount and fraud?
correlation <- cor(credit_card_Data$amt, credit_card_Data$is_fraud)
print(paste("Correlation between amount and fraud:", correlation))

#Are there any geographic patterns in fraudulent transactions?
ggplot(credit_card_Data[is_fraud == 1], aes(x = long, y = lat)) +
  geom_point(color = "red", alpha = 0.5) +
  labs(title = "Geographic Distribution of Fraudulent Transactions", x = "Longitude", y = "Latitude")


#Are there any geographic patterns in fraudulent transactions?
min_lat <- 30  # Minimum latitude
max_lat <- 47  # Maximum latitude
min_long <- -100  # Minimum longitude
max_long <- -75  # Maximum longitude

# Filter the dataset based on the latitude and longitude range
filtered_data <- credit_card_Data[is_fraud == 1 & 
                                    lat >= min_lat & lat <= max_lat 
                                  & long >= min_long & long <= max_long]

# Extract unique states and cities within the filtered range
unique_states <- unique(filtered_data$state)
unique_cities <- unique(filtered_data$city)

# Print the list of states and cities
print("States within the specified range:")
print(unique_states)

print("Cities within the specified range:")
print(unique_cities)



