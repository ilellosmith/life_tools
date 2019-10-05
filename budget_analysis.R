'This is a script to analyze my monthly spending'

################################################

# establish globals 
WORKING_DIRECTORY <- "~/r/finance/"
FILE_NAME <- "2019_Expenses_Budget.xlsx"
NECESSITIES <- c(
  "groceries",
  "toiletries",
  "furnishing",
  "medical",
  "transit",
  "transportation",
  "rent",
  "utilities",
  "credit")
MONTHS <- list(
  "1" = "January",
  "2" = "February",
  "3" = "March",
  "4" = "April",
  "5" = "May",
  "6" = "June",
  "7" = "July",
  "8" = "August",
  "9" = "September",
  "10" = "October",   
  "11" = "November", 
  "12" = "December"
)
# TODO: create list mapping naming variants to one option

################################################

# set working directory 
setwd(WORKING_DIRECTORY)

# load dependencies, installing if necessary
REQUIRED_PACKAGES <- c("tidyverse")
package.check <- lapply(REQUIRED_PACKAGES, FUN = function(x) {
  if (! require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

################################################
# define functions

################## 
# data formating functions 

# read_month returns a dataframe with desired columns from input file and month name
read_month <- function (month, FILE_NAME) {
  return(FILE_NAME %>% 
    readxl::read_excel(sheet = month, col_names = T) %>%
    select('Date',
            'Purchase',
            'Category',
            'Price',
            'Date',
            'Income_Source',
            'Type',
            'Amount') %>%
    mutate('Category' = tolower(Category))
  )
}

# iter_months returns a list of data frames, each element contains a month
iter_months <- function (FILE_NAME) {
  months_recorded <- readxl::excel_sheets(FILE_NAME)[-1]
  dat_list <- list()
  for (month in months_recorded) {
    mon_name <- month %>% substr(1,3)
    mon_dat <- read_month(month, FILE_NAME)
    mon_dat <- mon_dat %>% 
      mutate(
        # clean names 
        Category = case_when(
              Category %in% c('furnishing', 'furnishing/home care') ~ 'furnishing',
              Category %in% c('medicine', 'medical') ~ 'medicine',
              Category %in% c('transport', 'transportation') ~ 'transportation',
              Category %in% c('leisure', 'recreation') ~ 'recreation',
              TRUE ~ Category
            ),
        # categorize to necessities
        necessity =
          ifelse(Category %in% NECESSITIES, TRUE, FALSE)
      )
    dat_list[[mon_name]] <- mon_dat
  }
  return(dat_list)
}

################## 
# plotting functions 

# plot_longitudinal plots expenses and earnings by month for an input year
plot_longitudinal <- function(year, net = F) {
  if(net == F){
  print("Plotting Expenses and Earnings")
  year %>%
    group_by(month=lubridate::floor_date(Date, "month")) %>%
    summarize(month_spend=sum(Price), month_earn = sum(Amount, na.rm=T)) %>%
    ggplot(mapping = aes(ymin = 0)) +
    geom_line(aes(x = month, y = month_spend, color = "Expenses"), size = 2) +
    geom_line(aes(x = month, y = month_earn, color = "Earnings"), size = 2) +
    scale_color_manual(name = "+/-", 
                       values = c("Expenses" = "#CC0000" ,"Earnings" ="#00CC33")) +
    theme_minimal() +
    theme(text = element_text(size=12),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
    labs(x = "Month",
         y = "Amount")
  }
  else {
    print("Plotting Net Earnings")
    year %>%
      group_by(month=lubridate::floor_date(Date, "month")) %>% 
      calculate_savings() %>%
      ggplot(aes(x = month, y = savings, fill = (savings > 0))) +
      geom_bar(aes(group = 1), stat = "identity") +
      scale_fill_manual(name = "+/-",
                         values = c("TRUE" = "#00CC33", "FALSE" = "#CC0000"),
                         labels = c("TRUE" = "Saved", "FALSE" = "Lost" )) +
      theme_minimal() +
      theme(text = element_text(size=12),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
      labs(x = "Month",
           y = "Amount Earned")
  }
}

# plot_category plots expenses for a month by category, pct = T will plot as pct of total
plot_category <- function(month, pct = F) {
  # get string for the month (or year) for plot titles
  if(max(lubridate::month(month$Date)) != min(lubridate::month(month$Date))) {
    month_label <- "Year"
  }
  else{
    month_label <- MONTHS[toString(lubridate::month(month$Date[1]))]
  }
  if (pct == F) {
  print("Plotting Absolute Amount Spent")  
  month %>%
    group_by(Category, necessity) %>%
    summarize(spend = sum(Price, na.rm = T)) %>%
    ggplot(aes(x = reorder(Category,spend), y = spend)) +
    scale_fill_manual(values = c("TRUE" = "cornflowerblue", "FALSE" = "darkorange3"),
                      labels = c("TRUE" = "Necessities", "FALSE" = "Other"),
                      name = "Spending Type") +
    geom_bar(stat= "identity", aes(fill = necessity)) +
    theme_minimal() +
    theme(text = element_text(size=12))+
          #,
          #axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
    labs(x = "Spending Category",
         y = "Amount Spent", 
         title = paste("Spending for ", month_label, sep = '')) +
    coord_flip()
  }
  else {
    print("Plotting Amount Spent As % Of Amount Earned")  
    amt_earned <- month %>% summarize(total_earned = sum(Amount, na.rm =T)) 
    month %>%
      group_by(Category, necessity) %>%
      summarize(percent = sum(Price, na.rm = T)/amt_earned$total_earned) %>%
      ggplot(aes(x = reorder(Category,percent), y = percent)) +
      scale_fill_manual(values = c("TRUE" = "cornflowerblue", "FALSE" = "darkorange3"),
                        labels = c("TRUE" = "Necessities", "FALSE" = "Other"),
                        name = "Spending Type") +
      geom_bar(stat= "identity", aes(fill = necessity)) +
      theme_minimal() +
      theme(text = element_text(size=12)) +
            #,
            #axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
      labs(x = "Spending Category",
           y = paste("Amount Spent as Percentage of Total Earnings:", as.integer(amt_earned$total_earned)),
           title = paste("Spending for ", month_label, sep = '')) +
      coord_flip()
  }
}

################## 
# calculation functions 

# returns net earnings for input time period
calculate_savings <- function(time) {
  return(time %>% 
    summarize(savings = sum(Amount, na.rm = T) - sum(Price, na.rm =T))
  )
}

# returns summary statistics about earnings and spendings for a given time block
# for by month or by year results, pass in data grouped by month and year 
# for by category results, pass in data grouped by category
summary_spend <- function(time, Category = F) {
  if(Category == F) {
  return(time %>% 
    summarize(mean_earnings = mean(Amount, na.rm = T),
              median_earnings = median(Amount, na.rm = T),
              max_earnings = max(Amount, na.rm = T),
              min_earnings = min(Amount, na.rm = T),
              sd_earnings = sd(Amount, na.rm = T),
              n_earnings = length(Amount),
              mean_spend = mean(Price, na.rm = T),
              median_spend = median(Price, na.rm = T),
              max_spend = max(Price, na.rm = T),
              min_spend = min(Price, na.rm = T),
              sd_spend = sd(Price, na.rm = T), 
              n_spends = count(Price)
              ))
  }
  else {
    return(time %>% 
             summarize(mean_spend = mean(Price, na.rm = T),
                       median_spend = median(Price, na.rm = T),
                       max_spend = max(Price, na.rm = T),
                       min_spend = min(Price, na.rm = T),
                       sd_spend = sd(Price, na.rm = T),
                       n_spends = count(Price)))
  }
}

dat <- iter_months(FILE_NAME)
year <- rbind(dat$Jan, 
              dat$Feb, 
              dat$Mar,
              dat$Apr, 
              dat$May, 
              dat$Jun,
              dat$Jul,
              dat$Aug,
              dat$Sep,
              dat$Oct)

# spend by category for the year 
plot_category(year, pct = T)  

# spend by category by month
map(dat, plot_category)

# analyze mexico city trip
mx_month <- rbind(dat$Apr, dat$May, dat$Jun)
mx_month %>% 
  filter(Category %in% c("travel", "experiences", "gifts")) %>%
  summarize(sum(Price))

# too many categories to be meaningful
# year %>%
#   group_by(Category, month=lubridate::floor_date(Date, "month")) %>%
#   summarize(spend = sum(Price)) %>%
#   arrange(desc(spend)) %>%
#   ggplot(aes(x= month, y = spend)) +
#   geom_line(aes(color = Category))

# dining vs. groceries spend
year %>%
  group_by(Category, month=lubridate::floor_date(Date, "month")) %>%
  summarize(spend = sum(Price)) %>%
  arrange(desc(spend)) %>%
  filter(Category %in% c('groceries','dining')) %>%
  ggplot(aes(x= month, y = spend)) +
  geom_line(aes(color = Category))
  
# category spend for year
plot_category(year, pct = T)  

# longitudinal earnings and expenses
plot_longitudinal(year)

# longitudinal net
plot_longitudinal(year, net = T)

# delta per month
map(dat, calculate_savings)

# savings for the year
calculate_savings(year)

# spend by day
year %>% 
  group_by(Date) %>%
  summarize(day_spend = sum(Price)) %>%
  ggplot(aes(x = Date, y = day_spend)) +
  geom_step(stat = "identity")