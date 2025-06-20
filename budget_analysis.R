'This is a script to analyze my monthly spending'

################################################

# establish globals 
WORKING_DIRECTORY <- "~/Documents/GitHubFiles/Finance/"
FILE_NAMES <- list("2019_Expenses_Budget.xlsx",
                "2020_Expenses_Budget.xlsx",
                "2021_Expenses_Budget.xlsx",
                "2022_Expenses_Budget.xlsx",
                "2023_Expenses_Budget.xlsx", 
                "2024_Expenses_Budget.xlsx")

FILE_NAME <- "2020_Expenses_Budget.xlsx"

NECESSITIES <- c(
  "groceries",
  "toiletries",
  "furnishing",
  "medical/therapy",
  "health",
  "transit",
  "transportation",
  "rent",
  "utilities",
  "internet",
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
REQUIRED_PACKAGES <- c("tidyverse", 'scales')
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
  FILE_NAME %>% 
    readxl::read_excel(sheet = month, col_names = T) %>%
    select('Date',
            'Purchase',
            'Category',
            'Price',
            'Date',
            'Income_Source',
            'Type',
            'Amount') %>%
    mutate('Category' = tolower(Category)) %>%
    drop_na(Date)
}

# iter_months returns a list of data frames, each element contains a month
iter_months <- function (FILE_NAME, MONTHS) {
  months_recorded <- readxl::excel_sheets(FILE_NAME)
  months_sheet_filter <- (MONTHS %>% unlist() %>% unique())
  months_recorded <- months_recorded[months_recorded %in% months_sheet_filter]
  dat_list <- list()
  for (month in months_recorded) {
    mon_name <- month %>% substr(1,3)
    mon_dat <- read_month(month, FILE_NAME)
    mon_dat <- mon_dat %>% 
      mutate(
        # clean names 
        Category = case_when(
              str_trim(str_to_lower(Category)) %in% c('furnishing', 'furnishing/home care',
                                            'cleaning supplies', 'house supplies',
                                            'home supplies', 'furniture', 'bedding',
                                            'plants/gardening', 'kitchen', 'cooking gear',
                                            'cooking', 'cookware') ~ 'furnishing',
              str_trim(str_to_lower(Category)) %in% c('medicine', 'medical', 'health',
                                            'medication', 'therapy', 'dentist', 'dentists') ~ 'medical/therapy',
              str_trim(str_to_lower(Category)) %in% c('transport', 'transportation') ~ 'transportation',
              str_trim(str_to_lower(Category)) %in% c('leisure', 'recreation', 'running', 'gear', 'exercise') ~ 'recreation/gear',
              str_trim(str_to_lower(Category)) %in% c('vacation', 'travel') ~ 'travel',
              str_trim(str_to_lower(Category)) %in% c('concerts', 'experiences', 'movies', 'shows') ~ 'experiences',
              str_trim(str_to_lower(Category)) %in% c('insurance', 'internet', 'laundry') ~ 'utilities',
              str_trim(str_to_lower(Category)) %in% c('gift', 'gifts') ~ 'gifts',
              str_trim(str_to_lower(Category)) %in% c('cycling', 'bike') ~ 'cycling',
              str_trim(str_to_lower(Category)) %in% c('donation', 'donations') ~ 'donations',
              str_trim(str_to_lower(Category)) %in% c('alcohol', 'night life') ~ 'night life',
              str_trim(str_to_lower(Category)) %in% c('learning', 'education', 'professional development',
                                            'professional', 'website', 'documents', 'office supplies', 'mail', 'work') ~ 'education',
              str_trim(str_to_lower(Category)) %in% c('&za', 'coffee') ~ 'dining',
              str_trim(str_to_lower(Category)) %in% c('clothes', 'clothing', 'costume') ~ 'clothing',
              str_trim(str_to_lower(Category)) %in% c('computer', 'technology', 'electronics', 'tech') ~ 'computer/tech',
              str_trim(str_to_lower(Category)) %in% c('personal care', 'haircut') ~ 'haircut',
              str_trim(str_to_lower(Category)) %in% c('storage', 'phone') ~ 'phone',
              str_trim(str_to_lower(Category)) %in% c('rental car', 'moving') ~ 'rental car/moving',
              str_trim(str_to_lower(Category)) %in% c('music', 'news', 'tv', 'entertainment', 'books', 'toys') ~ 'entertainment',
              TRUE ~ str_trim(str_to_lower(Category))
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
    scale_y_continuous(labels = dollar_format()) +
    theme_minimal() +
    theme(text = element_text(size=12),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
    labs(x = "\nMonth",
         y = "Amount\n")
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
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      theme(text = element_text(size=12),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
      labs(x = "\nMonth",
           y = "Amount Earned\n")
  }
}

# plot_category plots expenses for a month by category, pct = T will plot as pct of total
plot_category <- function(month, pct = F) {
  # get string for the month (or year) for plot titles
  if(max(lubridate::month(month$Date)) != min(lubridate::month(month$Date))) {
    month_label <- paste0(min(month$Date), ' to ', max(month$Date))
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
    labs(x = "Spending Category\n",
         y = "Amount Spent", 
         title = paste("Spending for ", month_label, sep = '')) +
    coord_flip() + 
    scale_y_continuous(labels = percent_format())
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
      labs(x = "Spending Category\n",
           y = paste("\n Amount Spent as Percentage of Total Net Earnings:", dollar(amt_earned$total_earned)),
           title = paste("Spending for ", month_label, sep = '')) +
      coord_flip() + 
      scale_y_continuous(labels = percent_format())
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

# Prep lifetime
all_dat <- FILE_NAMES %>% map(~iter_months(.x, MONTHS))
all_dat_unlisted <- unlist(all_dat, recursive = F)
dat <- bind_rows(all_dat_unlisted) %>% 
  filter(Date < '2024-10-01')

# Prep single year
year <- rbind(dat$Jan, 
              dat$Feb, 
              dat$Mar,
              dat$Apr, 
              dat$May, 
              dat$Jun,
              dat$Jul,
              dat$Aug,
              dat$Sep,
              dat$Oct,
              dat$Nov,
              dat$Dec
)
# spend by category 
dat %>% 
  plot_category(pct = T)

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

# Stacked bar
dat %>% 
  group_by(Category, month=lubridate::floor_date(Date, "month")) %>%
  summarize(spend = sum(Price)) %>%
  arrange(desc(spend)) %>%
  filter(Category %in% c('groceries','dining')) %>%
  ggplot(aes(x= month, y = spend)) +
  geom_bar(aes(fill = Category), stat = 'identity') +
  scale_fill_manual(values = c("groceries" = "#0127a4", "dining" = "#e3c236"))+ 
  scale_y_continuous(labels = dollar_format()) +
  labs(x = "\nMonth",
       y = "Spend\n") +
  theme_minimal()

dat %>% 
  group_by(Category, month=lubridate::floor_date(Date, "month")) %>%
  summarize(spend = sum(Price)) %>%
  arrange(desc(spend)) %>%
  filter(Category %in% c('rent','utilities')) %>%
  ggplot(aes(x= month, y = spend)) +
  geom_bar(aes(fill = Category), stat = 'identity') +
  scale_fill_manual(values = c("rent" = "#0127a4", "utilities" = "#e3c236"))+ 
  scale_y_continuous(labels = dollar_format()) +
  labs(x = "\nMonth",
       y = "Spend\n") +
  theme_minimal()

# Line Plot
dat %>% 
  group_by(Category, month=lubridate::floor_date(Date, "month")) %>%
  summarize(spend = sum(Price)) %>%
  arrange(desc(spend)) %>%
  filter(Category %in% c('groceries','dining')) %>%
  ggplot(aes(x= month, y = spend)) +
  geom_line(aes(color = Category), size = 2) +
  scale_color_manual(values = c("groceries" = "#0127a4", "dining" = "#e3c236"))+ 
  scale_y_continuous(labels = dollar_format()) +
  labs(x = "\nMonth",
       y = "Spend\n") +
  theme_minimal()

# longitudinal earnings and expenses lifetime
dat %>% 
  plot_longitudinal()

# longitudinal expenses barplot
dat %>% 
  group_by(month=lubridate::floor_date(Date, "month")) %>%
  summarize(spend = sum(Price)) %>%
  ggplot(aes(x= month, y = spend)) +
  geom_bar(stat = 'identity', fill = "#CC0000") +
  scale_y_continuous(labels = dollar_format()) +
  labs(x = "\nMonth",
       y = "Spend\n") +
  theme_minimal()

# longitudinal net
dat %>% 
  plot_longitudinal(net = T)

# delta per month
map(dat, calculate_savings)

# savings for the year
dat %>% 
  calculate_savings()

# spend by day
dat %>% 
  group_by(Date) %>%
  summarize(day_spend = sum(Price)) %>%
  ggplot(aes(x = Date, y = day_spend)) +
  geom_step(stat = "identity")

# budget
dat %>%
  filter(Category %in% c('groceries', 'dining', 'transit',
                         'transportation', 'toiletries', 'night life')) %>% 
  group_by(Category, month=lubridate::floor_date(Date, "month")) %>%
  summarize(month_spend=sum(Price), month_earn = sum(Amount, na.rm=T)) %>% 
  filter(month >= '2021-12-31') %>% 
  summarize(avg_spend = mean(month_spend))

