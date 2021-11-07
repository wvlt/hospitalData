library(tidyverse)
library(data.table)
library(hrbrthemes)
library(viridis)


# Import data -------------------------------------------------------------

data_raw <- fread("data/Generic ED 2009_cleaned_v1.0_27102021.csv")


# General stats -----------------------------------------------------------

# Age
age_counts <- data_raw %>% 
  count(`Age  (yrs)`, sort = FALSE)

# calculate percentages
age_counts <- mutate(age_counts, percent = round(n/sum(n) * 100, 2))
age_counts <- mutate(age_counts, percent_cum = round(cumsum(n)/sum(n) * 100, 2))
age_counts <- mutate(age_counts, 
                     age_group = ifelse(0 <= `Age  (yrs)` &  `Age  (yrs)` <= 5,
                                        "0-5", ifelse(5 < `Age  (yrs)` & `Age  (yrs)` <= 10,
                                                           "5-10", "10-15")))

ggplot(age_counts, aes(x = reorder(age_group, `Age  (yrs)`), y = percent,
                       fill = as.factor(age_group))) +
  geom_bar(stat = "identity") +
  scale_color_viridis(option = "turbo", discrete = TRUE) +
  theme_ipsum() + ylab("Percentage of Patients") +
  xlab("Age Group") +
  labs(title = "Cumulative Percentage of Patients by Age Group",
       fill = "Age Group\n")

# Triage Priority
triage_pri <- data_raw %>% 
  count(`Triage Priority`, `Age  (yrs)`, sort = TRUE)

triage_pri <- mutate(triage_pri, 
                     age_group = ifelse(0 <= `Age  (yrs)` &  `Age  (yrs)` <= 5,
                                        "0-5", ifelse(5 < `Age  (yrs)` & `Age  (yrs)` <= 10,
                                                      "5-10", "10-15")))

ggplot(triage_pri, aes(x = reorder(age_group, `Age  (yrs)`), y = n,
                       fill = as.factor(`Triage Priority`))) +
  geom_bar(stat = "identity") +
  scale_color_viridis(option = "turbo", discrete = TRUE) +
  theme_ipsum() + ylab("Number of Patients") +
  xlab("Age Group") +
  labs(title = "Number of Patients by Triage Priority in Various Age Groups",
       fill = "Triage Priority\n")

ggplot(triage_pri, aes(x = `Age  (yrs)`, y = n,
                       fill = as.factor(`Triage Priority`))) +
  geom_bar(stat = "identity") +
  scale_color_viridis(option = "turbo", discrete = TRUE) +
  theme_ipsum() + ylab("Number of Patients") + xlab("Age") +
  labs(title = "Number of Patients by Triage Priority and Age",
       fill = "Triage Priority\n")


# Sankey for age and triage -----------------------------------------------

results <- triage_pri %>% 
  select(`Triage Priority`, age_group, n)

results$`Triage Priority` <- as.character(results$`Triage Priority`)

# create nodes dataframe

subgroups <- unique(results$`Triage Priority`)
nodes <- data.frame(node = c(0:7), 
                    name = c(subgroups, "0-5", "5-10", "10-15"))

#create links dataframe
results <- merge(results, nodes, by.x = "Triage Priority", by.y = "name")
results <- merge(results, nodes, by.x = "age_group", by.y = "name")
links <- results[ , c("node.x", "node.y", "n")]
colnames(links) <- c("source", "target", "value")

networkD3::sankeyNetwork(Links = links, Nodes = nodes, Source = 'source', 
                         Target = 'target', Value = 'value', NodeID = 'name',
                         units = 'votes')


# Depart statistics -------------------------------------------------------

depart_stat <- data_raw %>% 
  count(`Departure Status Desc.`, `Age  (yrs)`, sort = TRUE)

ggplot(depart_stat, aes(x = `Departure Status Desc.`, y = n,
                       fill = as.factor(`Departure Status Desc.`))) +
  geom_bar(stat = "identity") +
  scale_color_viridis(option = "turbo", discrete = TRUE) +
  theme_ipsum() + ylab("Number of Patients") + xlab("Status Description") +
  labs(title = "Number of Patients by Departure Status",
       fill = "Triage Priority\n") +
  coord_flip() +
  theme(legend.position = "none")


# Raskey
results <- depart_stat %>% 
  select(`Departure Status Desc.`, `Age  (yrs)`, n)

results$`Age  (yrs)` <- as.character(results$`Age  (yrs)`)

# create nodes dataframe

subgroups <- unique(results$`Age  (yrs)`)
nodes <- data.frame(node = c(0:24), 
                    name = c(subgroups, "ED SERVICE EVENT COMPLETED - DISCHARGED", "DID NOT WAIT",                               
                             "ADMITTED (EXCL.ED BED)", "TRANSFER TO ANOTHER HOSPITAL",                 
                             "LEFT AFTER TREATMENT COMMENCED", "DIED IN ED",
                             "PATIENT FOR DELETION BY CAMPUS ADMINISTRATOR",  "** ADMITTED TO OBS WARD **",                   
                             "DEAD ON ARRIVAL (NO TREATMENT PROVIDED IN ED)"))

#create links dataframe
results <- merge(results, nodes, by.x = "Age  (yrs)", by.y = "name")
results <- merge(results, nodes, by.x = "Departure Status Desc.", by.y = "name")
links <- results[ , c("node.x", "node.y", "n")]
colnames(links) <- c("source", "target", "value")

networkD3::sankeyNetwork(Links = links, Nodes = nodes, Source = 'source', 
                         Target = 'target', Value = 'value', NodeID = 'name',
                         units = 'votes')

# Arrival-Actual Depart -----------------------------------------------------

ggplot(data=data_raw %>% 
         filter(`TimeDiff Arrival-Actual Depart (mins)` < 500),
       aes(x=`TimeDiff Arrival-Actual Depart (mins)`, group=`Age  (yrs)`, fill=as.factor(`Age  (yrs)`))) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

ggplot(data_raw %>% 
         filter(`TimeDiff Arrival-Actual Depart (mins)` < 500),
         aes(x = `TimeDiff Arrival-Actual Depart (mins)`,
                     group = `Age  (yrs)`,
                     fill = as.factor(`Age  (yrs)`))) +
  geom_boxplot() +
  coord_flip() +
  scale_color_viridis(option = "turbo", discrete = TRUE) +
  theme_ipsum() + ylab("Age")

# TreatDrNr-Act. Depart -----------------------------------------------------

ggplot(data=data_raw %>% 
         filter(`TimeDiff TreatDrNr-Act. Depart (mins)` < 500),
       aes(x=`TimeDiff TreatDrNr-Act. Depart (mins)`, group=`Age  (yrs)`, fill=`Age  (yrs)`)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

ggplot(data_raw %>% 
         filter(`TimeDiff TreatDrNr-Act. Depart (mins)` < 500),
       aes(x = `TimeDiff TreatDrNr-Act. Depart (mins)`,
           group = `Age  (yrs)`,
           fill = `Age  (yrs)`)) +
  geom_boxplot() +
  coord_flip() +
  scale_color_viridis(option = "turbo", discrete = TRUE) +
  theme_ipsum() + ylab("Age")

# wait_time_mins -------------------------------------------------------------

ggplot(data=data_raw %>% 
         filter(wait_time_mins < 500),
       aes(x=wait_time_mins, group=`Age  (yrs)`, fill=`Age  (yrs)`)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

ggplot(data_raw %>% 
         filter(wait_time_mins < 500),
       aes(x = wait_time_mins,
           group = `Age  (yrs)`,
           fill = `Age  (yrs)`)) +
  geom_boxplot() +
  coord_flip() +
  scale_color_viridis(option = "turbo", discrete = TRUE) +
  theme_ipsum() + ylab("Age")


# Time in general ---------------------------------------------------------

ggplot(data_raw, aes(x = `Age  (yrs)`,
                     y = `TimeDiff Arrival-Actual Depart (mins)`,
                     fill = as.factor(`Age  (yrs)`))) +
  geom_bar(stat = "identity") +
  scale_color_viridis(option = "turbo", discrete = TRUE) +
  theme_ipsum() + ylab("Time") + xlab("Age") +
  labs(title = "Amount of Time from Arrival to Departure (mins)",
       fill = "Age\n")

# Average time spent in each age category
mean_time <- tibble()

for (i in 0:15) {
  data <- data_raw %>% 
    filter(`Age  (yrs)` == i) %>% 
    select(`TimeDiff TreatDrNr-Act. Depart (mins)`) %>% 
    drop_na()
  mean <- mean(data$`TimeDiff TreatDrNr-Act. Depart (mins)`)
  mean_time <- rbind(mean_time, mean)
}
colnames(mean_time) <- c("average_wait_time_mins")

mean_time <- mutate(mean_time, age = c(0:15))


ggplot(mean_time, aes(x = age, y = average_wait_time_mins, fill = as.factor(age))) +
  geom_bar(stat = "identity") +
  scale_color_viridis(option = "turbo", discrete = TRUE) +
  theme_ipsum() + ylab("Time") + xlab("Age") +
  labs(title = "Average Time from Doctor/Nurse Treatment to Departure",
       fill = "Age\n")

# Diagnosis stats ---------------------------------------------------------

diag_stat <- data_raw %>% 
  count(`Diagnosis Desc.`, sort = TRUE)

ggplot(diag_stat[1:30,], aes(x = reorder(`Diagnosis Desc.`, n), y = n,
                             size = n)) +
  geom_point() +
  coord_flip() +
  scale_color_viridis(option = "turbo", discrete = TRUE) +
  theme_ipsum() + xlab("Diagnosis")

diag_group <- data_raw %>% 
  group_by(`Age  (yrs)`) %>% 
  count(`Diagnosis Desc.`, sort = TRUE)

ggplot(data_raw, aes(`Depart Status Code`, `Age  (yrs)`)) +
  geom_jitter(aes(color = `Depart Status Code`), size = 0.3)+
  ggpubr::color_palette("jco")+
  ggpubr::theme_pubclean()


# MRN ---------------------------------------------------------------------

mrn_stats <- data_raw %>% 
  group_by(MRN) %>% 
  count(MRN, `Diagnosis Desc.`, wait_time_mins, sort = TRUE)

ggplot(mrn_stats[1:30,], aes(MRN, `Diagnosis Desc.`)) +
  geom_jitter(aes(color = `Diagnosis Desc.`, size = wait_time_mins))+
  ggpubr::color_palette("jco")+
  ggpubr::theme_pubclean()


# Days in hospital --------------------------------------------------------

days_in <- data_raw %>% 
  count(days_in_hospital, sort = TRUE)

ggplot(days_in, aes(x = days_in_hospital, y = n)) +
  geom_bar(stat = "identity")


# Scatterplots ------------------------------------------------------------

ggplot(data_raw, aes(x = `Triage Priority`, y = days_in_hospital)) +
  geom_bar(stat = "identity")
