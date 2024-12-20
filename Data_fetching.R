
##############################################################################################################
#
# This Code collects Daily log Returns from Denmark and the US 
# Combining it with Monetary policy shocks, as well as implied volatility Indices
#
# Made by Markus Roed Schoeler -  20/12/2024
#
###############################################################################################################


# We load the  necessary libraries
library(quantmod)    # For stock data
library(dplyr)       # For data manipulation
library(readr)       # For reading CSV files
library(haven)
library(xts)
library(fst)

###############################################################################################################
#
#  We start by getting Data for the USA and then merging it with the Shocks and VIX Index
#
###############################################################################################################

# Step 1: We load the CSV file containing the Tickers, Stock names, and the Sector for 5.949 American publicly listed stocks
stock_data_US <- read.csv("C:/Users/Markus/SyNologyDrive/Drive/Desktop/Speciale/data/tickers_US.csv", sep = ";")  # Set delimiter as semicolon


# Step 2: We initialize an empty list to store the stock returns data
stock_returns_list_US <- list()


# Step 3: We set the date range (2000 to 2023)
start_date <- as.Date("2000-01-03")
end_date <- as.Date("2023-10-26")

# Step 4: We make a  Loop that goes through each ticker and get daily stock data and calculate daily returns
for (i in 1:nrow(stock_data_US)) {
  Ticker <- stock_data_US$Ticker[i]  # Access Ticker_US column
  Stock_Name <- stock_data_US$Stock_Name[i]  # Access Stock_Name_US column
  Sector <- stock_data_US$Sector[i]  # Access Sector_US column
  
  # Print ticker being processed 
  print(paste("Processing:", Ticker))
  
  # Try to get stock data, skip ticker if not available
  try({
    # Get daily stock data from Yahoo Finance
    stock_data_symbol_US <- getSymbols(Ticker, src = "yahoo", 
                                       from = start_date, to = end_date, 
                                       auto.assign = FALSE)
    
    # Calculate daily returns (logarithmic returns)
    stock_returns_US <- dailyReturn(Ad(stock_data_symbol_US), type = "log")
    
    # Convert the returns to a data frame and add the American stock metadata
    stock_returns_df_US <- data.frame(Date = index(stock_returns_US), 
                                      Daily_Return_US = coredata(stock_returns_US)*100,
                                      Ticker = Ticker,
                                      Stock_Name = Stock_Name,
                                      Sector = Sector)
    
    # Append the data to the list
    stock_returns_list_US[[Ticker]] <- stock_returns_df_US
  }, silent = TRUE)  # Skip tickers with issues
}

# Step 5: We combine all American stock data into a single data frame
combined_stock_returns_US <- bind_rows(stock_returns_list_US)


# Step 6: We filter out tickers with fewer than 756 (3 years) observations
filtered_stock_returns_US <- combined_stock_returns_US %>%
  group_by(Ticker) %>%
  filter(n() >= 756) %>%
  ungroup()


# Step 7: We check how many tickers were removed (we removed 1137 tickers) 
tickers_removed <- setdiff(unique(combined_stock_returns_US$Ticker), unique(filtered_stock_returns_US$Ticker))
print(paste("Number of tickers removed:", length(tickers_removed)))


# Step 8: We sort the data by Date
sorted_data_US <- filtered_stock_returns_US %>%
  arrange(Date)


##############################################################################################################################
#
# In this section we merge the Fed monetary shocks and the VIX Index with the US Stock dataset
#
##############################################################################################################################

# Step 9: We load the CSV file containing VIX
VIX_data <- read.csv("X/X/X/VIX.csv", sep = ";")  # Set delimiter as semicolon


# Step 10: We make sure both tables has the same date format
VIX_data$Date <- format(VIX_data$Date, format = "%Y-%m-%d")
sorted_data_US$Date <- format(sorted_data_US$Date, format = "%Y-%m-%d")


# Step 11: We merge the datasets based on the Date column
sorted_data_US_VIX <- left_join(sorted_data_US, VIX_data, by = "Date")


# Step 12: We replace missing VIX values with 0
sorted_data_US_VIX$VIX[sorted_data_US_VIX$VIX=="."] <- 0


# Step 13: We load the CSV file containing monetary shocks
monetary_shocks_US <- read.csv("X/X/X/Shocks_fed.csv", sep= ";")


# Step 14: We make sure both tables has the same date format
sorted_data_US_VIX$Date <- format(sorted_data_US_VIX$Date, format = "%Y-%m-%d")
monetary_shocks_US$Date <- format(monetary_shocks_US$Date, format = "%Y-%m-%d")


# Step 15: We merge the datasets based on the Date column
merged_data_US <- left_join(sorted_data_US_VIX, monetary_shocks_US, by = "Date")


# Step 16: We replace missing monetary shocks (NA) with 0
merged_data_US$Shocks_FED[is.na(merged_data_US$Shocks_FED)] <- 0
merged_data_US$INFO_Shocks_FED[is.na(merged_data_US$INFO_Shocks_FED)] <- 0


# Step 17: Since we used returns, the first date is gonna be zero, so we remove it
merged_data_US <- merged_data_US %>%
  filter(Date != as.Date("2000-01-03"))


# Step 18: We save the dataset as a rds file
saveRDS(Finale_US, "X/X/X/Finale_US.rds")


# Step 19: We rename the return variable and save the dataset as a dta file
Finale_US <- Finale_US %>%
  rename(daily_returns = daily.returns)
write_dta(Finale_US, "X/X/X/Finale_US.dta")




##############################################################################################################################
#
# In this section we calculate some descriptive statistics
#
##############################################################################################################################

# Step 20:  We  count the total number of unique tickers and print it (4096) 
total_unique_tickers_US <- Finale_US %>%
  summarize(Unique_Tickers_US = n_distinct(Ticker))
print(paste("Total unique tickers (US):", total_unique_tickers_US$Unique_Tickers_US))


# Step 21: We count the number of tickers in each sector and print it 
tickers_per_sector_US <- Finale_US %>%
  group_by(Sector) %>%
  summarize(Number_of_Tickers_US = n_distinct(Ticker)) %>%
  arrange(desc(Number_of_Tickers_US))

print("Number of tickers per sector (US):")
print(tickers_per_sector_US)


# Step 22: We calculate the average number of observations across all tickers and print it (4042)
ticker_observations_US <- Finale_US %>%
  group_by(Ticker) %>%
  summarise(Num_Observations_US = n())

average_observations_US <- ticker_observations_US %>%
  summarise(Average_Observations_US = mean(Num_Observations_US))
print(paste("Average number of observations per ticker (US):", average_observations_US$Average_Observations_US))



##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
#
#  In the below section we do the same, but for Danish stocks.
#
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################


# Step 1: We load the tickers we want to fetch
stock_data_DK <- read.csv("X/X/X/tickers_DK.csv", sep = ";")  # Set delimiter as semicolon


# Step 2: We Initialize an empty list to store the stock returns data for Danish tickers
stock_returns_list_DK <- list()


# Step 3: We set the date range (2000 to 2023)
start_date <- as.Date("2000-01-03")
end_date <- as.Date("2023-10-26")


# Step 4: We loop through each Danish ticker and get daily stock data and calculate daily returns
for (i in 1:nrow(stock_data_DK)) {
  Ticker <- stock_data_DK$Ticker[i]  # Access Ticker column
  Stock_Name <- stock_data_DK$Stock_Name[i]  # Access Stock_Name column
  Sector <- stock_data_DK$Sector[i]  # Access Sector column
  
  # Print ticker being processed 
  print(paste("Processing:", Ticker))
  
  # Try to get stock data, skip ticker if not available
  try({
    # Get daily stock data from Yahoo Finance 
    stock_data_symbol <- getSymbols(Ticker, src = "yahoo", 
                                    from = start_date, to = end_date, 
                                    auto.assign = FALSE)
    
    # Calculate daily returns (logarithmic returns) 
    stock_returns <- dailyReturn(Ad(stock_data_symbol), type = "log")
    
    # Convert the returns to a data frame and multiply by 100 for percentages
    stock_returns_df <- data.frame(
      Date = index(stock_returns), 
      Daily_Return = coredata(stock_returns) * 100,  # Multiply by 100 for percentages
      Ticker = Ticker,
      Stock_Name = Stock_Name,
      Sector = Sector
    )
    
    # Append to the list of Danish stock returns
    stock_returns_list_DK[[Ticker]] <- stock_returns_df
  }, silent = TRUE)  # Skip tickers with issues
}


# Step 5: We combine all Danish stock data into a single data frame
combined_stock_returns_DK <- bind_rows(stock_returns_list_DK)


# Step 6: We filter out tickers with fewer than 756 (3 years) observations
filtered_stock_returns_DK <- combined_stock_returns_DK %>%
  group_by(Ticker) %>%
  filter(n() >= 756) %>%
  ungroup()


# Step 7: We check how many tickers were removed (we removed 30 Stocks) 
tickers_removed <- setdiff(unique(combined_stock_returns_DK$Ticker), unique(filtered_stock_returns_DK$Ticker))
print(paste("Number of tickers removed:", length(tickers_removed)))

# Step 8: We sort data by Date
sorted_data_DK <- filtered_stock_returns_DK %>%
  arrange(Date)


##############################################################################################################################
#
# In this section we merge both the ECB and Fed monetary shocks as well as the EURO Implied Volatility Index
#
##############################################################################################################################


# Step 9: We load the CSV file containing ECB monetary shocks
monetary_shocks_DK <- read.csv("X/X/X/Shocks_ecb.csv", sep= ";")


# Step 10: We make sure both tables has the same date format
sorted_data_DK$Date <- format(sorted_data_DK$Date, format = "%Y-%m-%d")
monetary_shocks_DK$Date <- format(monetary_shocks_DK$Date, format = "%Y-%m-%d")


# Step 11: We merge the datasets based on the Date column
merged_data_DK_Shock <- left_join(sorted_data_DK, monetary_shocks_DK, by = "Date")


# Step 12: We replace missing monetary shocks (NA) with 0
merged_data_DK_Shock$Shocks_ECB[is.na(merged_data_DK_Shock$Shocks_ECB)] <- 0
merged_data_DK_Shock$INFO_Shocks_ECB[is.na(merged_data_DK_Shock$INFO_Shocks_ECB)] <- 0


# Step 13: We load the CSV file containing Fed monetary shocks
monetary_shocks_US <- read.csv("X/X/X/Shocks_fed.csv", sep= ";")


# Step 14: We make sure both tables has the same date format
merged_data_DK_Shock$Date <- format(merged_data_DK_Shock$Date, format = "%Y-%m-%d")
monetary_shocks_US$Date <- format(monetary_shocks_US$Date, format = "%Y-%m-%d")


# Step 15: We merge the datasets based on the Date column
merged_data_DK_Shock_FED <- left_join(merged_data_DK_Shock, monetary_shocks_US, by = "Date")


# Step 16: We replace missing monetary shocks (NA) with 0
merged_data_DK_Shock_FED$Shocks_FED[is.na(merged_data_DK_Shock_FED$Shocks_FED)] <- 0
merged_data_DK_Shock_FED$INFO_Shocks_FED[is.na(merged_data_DK_Shock_FED$INFO_Shocks_FED)] <- 0


# Step 17: We load the CSV file containing European Volatility index
VIX_data_EU <- read.csv("C:/Users/Markus/SyNologyDrive/Drive/Desktop/Speciale/data/VIX_EU.csv", sep = ";")  # Set delimiter as semicolon


# Step 18: We make sure both tables has the same date format
VIX_data_EU$Date <- format(VIX_data_EU$Date, format = "%Y-%m-%d")
merged_data_DK_Shock_FED$Date <- format(merged_data_DK_Shock_FED$Date, format = "%Y-%m-%d")


# Step 19: We merge the datasets based on the Date column
Finale_DK <- left_join(merged_data_DK_Shock_FED, VIX_data_EU, by = "Date")


# Step 20: Since we used returns, the first date is gonna be zero, so we remove it
Finale_DK <- Finale_DK %>%
  filter(Date != as.Date("2000-01-03"))


# Step 21: We save the data set as a rds file
saveRDS(Finale_DK, "Finale_DK.rds")


# Step 22: We rename the return variable and save the data set as a dta file
# Rename the variable from 'daily.returns' to 'daily_returns'
Finale_DK <- Finale_DK %>%
  rename(daily_returns = daily.returns)
write_dta(Finale_DK, "C:/Users/Markus/SyNologyDrive/Drive/Desktop/Speciale/Code/Finale_DK.dta")



##############################################################################################################################
#
# In this section we calculate some descriptive statistics
#
##############################################################################################################################


# Step 23: We find the amount of unique Tickers present and display it(127)
num_unique_tickers_DK <- Finale_DK %>%
  summarise(Unique_Tickers = n_distinct(Ticker))
print(paste("Number of unique tickers:", num_unique_tickers_DK$Unique_Tickers))


#  Step 24: We find the amount of tickers in each sector and display it
stocks_by_sector <- Finale_DK %>%
  group_by(Sector) %>%
  summarise(Num_Stocks = n_distinct(Ticker)) %>%
  arrange(desc(Num_Stocks))  # Sort by number of stocks (optional)
print(stocks_by_sector)


# Step 25: We check how many observations each ticker has
ticker_observation_count_DK <- Finale_DK %>%
  group_by(Ticker) %>%
  summarise(Observation_Count = n()) %>%
  arrange(desc(Observation_Count))  # Optional: sort by descending order of observation count


# Step 26: We calculate the average amount of observations for the tickers and display it
ticker_obs_count <- Finale_DK %>%
  group_by(Ticker) %>%
  summarise(observations = n())
average_observations <- mean(ticker_obs_count$observations)
print(paste("The average number of observations per ticker is:", round(average_observations, 2)))


#########################################################################################################################
#########################################################################################################################
