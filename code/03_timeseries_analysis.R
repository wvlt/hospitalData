library(tidyverse)
library(data.table)
library(TSstudio)
library(lubridate)
library(hrbrthemes)
library(patchwork)



# Import data -------------------------------------------------------------

data_raw <- fread("data/Generic ED 2009_cleaned_v1.0_27102021.csv")

data_left_patients <- data_raw %>% 
  filter(`TimeDiff TreatDrNr-Act. Depart (mins)` == 0)

data_raw <- setdiff(data_raw, data_left_patients)

# Length of data
date_sum <- data_raw %>% 
  group_by(year, month) %>% 
  count(month, sort = FALSE)

ggplot(date_sum, aes(x = month, y = n, group = year, color = year)) +
  geom_line(size = 2) +
  ylab("Number of Patients") +
  xlab("Month") +
  xlim(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
        "Oct", "Nov", "Dec")) +
  theme_ipsum()

date_sum <- data_raw %>% 
  group_by(year, month, day) %>% 
  count(month, sort = FALSE)

month <- c(1,2,3,4,5,6,7,8,9,10,11,12)

res_plots <- lapply(X = month, 
                    FUN = function(var_x){
                      ggplot(date_sum %>% 
                               filter(month == var_x), aes(x = day, y = n,
                                                           group = year, color = year)) +
                        geom_line(size = 1) +
                        ylab("Number of Patients") +
                        xlab("Day") +
                        xlim(c(range(1,31))) +
                        theme_classic() +
                        theme(legend.position = "none")
})

wrap_plots(res_plots, ncol=3, heights=40) + plot_annotation(
  title = 'Number of Patients in Months of the Years 2009 and 2010', 
  subtitle = 'Months')


# Timeseries --------------------------------------------------------------

# Length of data
date_sum <- data_raw %>% 
  group_by(`Arrival Date`) %>% 
  count(`Arrival Date`, sort = FALSE)

ts_plot(date_sum,
        title = "Daily Admissions to the Hospital",
        Xtitle = "Time",
        Ytitle = "Number of patients",
        Xgrid = TRUE, Ygrid = TRUE)

test <- ts(data_raw)

wide_data <- reshape(data_raw, idvar = "Arrival Date", timevar = "Age  (yrs)", direction = "wide")
