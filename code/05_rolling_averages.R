library(tidyverse)
library(data.table)
library(hrbrthemes)
library(zoo)
library(hrbrthemes)
library(patchwork)


data_raw <- fread("data/Generic ED 2009_cleaned_v1.0_27102021.csv")

# Rolling averages --------------------------------------------------------

month <- c(1,2,3,4,5,6,7,8,9,10,11,12)

res_plots <- lapply(X = month, 
                    FUN = function(var_x){
                      ggplot(data_raw %>% 
                               filter(month == var_x), aes(x = day, y = seven_avg,
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


data_raw %>%
  mutate(seven_avg= rollmean(wait_time_mins, 7,
                             align="left", 
                             fill=0)) %>%
  ggplot(aes(x=date,
             y=wait_time_mins, group = year, color = year)) +
  geom_col(fill="pink")+
  geom_line(aes(y = seven_avg), 
            color = "red", 
            size = .75)+
  labs(title="Wait Time",
       y="Patients Wait Time")

data_raw %>% 
  drop_na() %>% 
  group_by(year, month) %>% 
  summarise_at(vars(wait_time_mins), list(name = mean)) %>% 
  ggplot(aes(x = month, y = name, group = year, color= year)) +
  geom_line(size = 1) +
  ylab("Rolling Average") + 
  xlab("Month") +
  xlim(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
         "Oct", "Nov", "Dec"))

       