library(corrplot)
library(data.table)
library(TSstudio)
library(tidyverse)

# Import data -------------------------------------------------------------

data_raw <- fread("data/Generic ED 2009_cleaned_v1.0_27102021.csv") %>% 
  as.data.frame()


# Correlation studies -----------------------------------------------------

data_corr <- data_raw %>% 
  select(MRN, `Presentation Visit Number`, `Triage Priority`,
         `Age  (yrs)`,
         `TimeDiff Arrival-Actual Depart (mins)`,
         `TimeDiff TreatDrNr-Act. Depart (mins)`,
         wait_time_mins, days_in_hospital, revisit, num_visits, complaint_label,
         diagnosis_label) %>% 
  drop_na()

res <- cor(data_corr)

corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 45)


