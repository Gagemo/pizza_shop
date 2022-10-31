# Pizza sales

##### Load packages #######
library(tidyverse)
library(lubridate)

##### Load data #######
all_sales <- list.files(path = "./sales", pattern = ".csv")

# Loop that loads all sale data sets #
i <- 1
for (salesdata in all_sales) {
  datafile <- read_csv(paste0("./sales/", salesdata)) #reads each data files found in folder
  assign(paste0("dataset_", i), datafile) #concatenates character strings with given sep _ symbol
  i <- i + 1 # When one file is loaded it loads the next file and adds 1 value to it (so data_1 then data_2)
}
############## JOIN ###############
# Use a tidyverse join to join all the data together into one file
# called sales_data, then run the rest of the code

#This is missing?? So I used the made up data I made?

sales_data = read.csv("sales/202210_sales_LaPierre.csv")

########################################

##### 3. Create summaries #####
sales_summary <- sales_data %>% # takes combined data set
  group_by(pizza, month) %>% # groups this data by pizza and month
  summarize(total_sales = sum(number)) # then it summarizes this data by sales numbers
#  sorts/groups the data based on pizza type and month and then gives the total sales each month per pizza type.

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))
# Creates a column named date and pastes the data from three columns into the 
# newly created date column.

# Summarize data
sales_summary_daily <- sales_data %>% # takes the combined sales data
  group_by(pizza, date) %>% # groups this data by pizza and month
  summarize(total_sales = sum(number)) #then it summarizes this data by sales numbers
# sorts/groups the data based on pizza type and date and then gives the total sales per pizza type date wise.

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")

# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")
