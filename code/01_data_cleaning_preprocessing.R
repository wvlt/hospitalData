library(tidyverse)
library(data.table)
library(viridis)
library(lubridate)



# Import data -------------------------------------------------------------

# Import data and drop NA values
data_raw <- fread("data/Generic ED 2009.csv") %>% 
  drop_na()

# Pre-processing ----------------------------------------------------------

# Convert data columns to appropriate formats
data_raw <- mutate(data_raw, 
                   `Arrival Date` = gsub("00", "", `Arrival Date`))
data_raw <- mutate(data_raw, 
                   `Arrival Date` = as.Date(`Arrival Date`, format="%d-%m-%y"))
data_raw <- mutate(data_raw, 
                   `Dr Seen Date` = as.Date(`Dr Seen Date`, format="%d/%m/%y"))
data_raw <- mutate(data_raw, 
                   `Depart Actual Date` = as.Date(`Depart Actual Date`, format="%d/%m/%y"))

data_raw <- mutate(data_raw,
                   `TimeDiff TreatDrNr-Act. Depart (mins)` = 
                     as.numeric(gsub(",", "", `TimeDiff TreatDrNr-Act. Depart (mins)`)))

data_raw <- mutate(data_raw,
                   wait_time_mins =  `TimeDiff Arrival-Actual Depart (mins)` - 
                     `TimeDiff TreatDrNr-Act. Depart (mins)`)

data_raw <- mutate(data_raw, wait_time_quarters = round(wait_time_mins / 15))

data_raw <- mutate(data_raw, 
                   days_in_hospital = `Depart Actual Date` - `Arrival Date`)


# Check if a patient has returned -----------------------------------------

duplicated_MRN <- data_raw[duplicated(data_raw$MRN)]

data_raw <- mutate(data_raw, revisit = ifelse(MRN %in% duplicated_MRN$MRN, 1, 0))

sum_patient_return <- data_raw %>% count(MRN)

data_raw <- left_join(data_raw, sum_patient_return)

colnames(data_raw)[22] <- "num_visits"


# Make year, month and day columns ----------------------------------------

data_raw <- mutate(data_raw, year = year(as.POSIXlt(`Arrival Date`, format="%Y/%m/%d")))
data_raw <- mutate(data_raw, month = month(as.POSIXlt(`Arrival Date`, format="%Y/%m/%d")))
data_raw <- mutate(data_raw, day = day(as.POSIXlt(`Arrival Date`, format="%Y/%m/%d")))

# Convert discrete values to numeric --------------------------------------

data_raw <- mutate(data_raw, complaint_label = as.integer(as.factor(data_raw$`Presenting Complaint Code`)))
data_raw <- mutate(data_raw, diagnosis_label = as.integer(as.factor(data_raw$`Diag Code`)))


# Moving average ----------------------------------------------------------

data_raw <- data_raw %>% mutate(seven_avg = rollmean(wait_time_mins, 7, 
                                                     align="left", fill=0))

# Write_CSV ---------------------------------------------------------------

write_csv(data_raw, "data/Generic ED 2009_cleaned_v1.0_27102021.csv")

