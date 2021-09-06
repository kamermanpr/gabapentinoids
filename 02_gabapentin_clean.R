############################################################
#                                                          #
#                     Gabapentin: clean                    #
#                                                          #
############################################################

#-- Load packages --#
library(dplyr)
library(readr)

#-- Notes --#
# Items: prescriptions
# Quantity: number of pills dispensed
# When calculating daily dose per month, all prescriptions assumed to be for 30 days

#-- Import data --#
# 100mg data
gabapentin_100_cap <- read_csv(file = 'data-original/gabapentin_100_cap.csv')

neurontin_100_cap <- read_csv(file = 'data-original/neurontin_100_cap.csv')

# 300mg 
gabapentin_300_cap <- read_csv(file = 'data-original/gabapentin_300_cap.csv')

neurontin_300_cap <- read_csv(file = 'data-original/neurontin_300_cap.csv')

# 400mg 
gabapentin_400_cap <- read_csv(file = 'data-original/gabapentin_400_cap.csv')

neurontin_400_cap <- read_csv(file = 'data-original/neurontin_400_cap.csv')

# 600mg 
gabapentin_600_tab <- read_csv(file = 'data-original/gabapentin_600_tab.csv')

neurontin_600_tab <- read_csv(file = 'data-original/neurontin_600_tab.csv')

# 800mg 
gabapentin_800_tab <- read_csv(file = 'data-original/gabapentin_800_tab.csv')

neurontin_800_tab <- read_csv(file = 'data-original/neurontin_800_tab.csv')

#-- Create output directory for clean data --#
if(!dir.exists('data-clean')) {
    dir.create('data-clean')
}

#-- Clean data --#
# 100mg data
gabapentin_100 <- left_join(gabapentin_100_cap, neurontin_100_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_100 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_100 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_100, pills_100) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_100 = pills_100 * 100) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_100 = total_prescribed_dose_100 / prescriptions_100) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_100 = total_dose_per_prescription_100 / 30) %>% 
    # Select columns
    select(date, prescriptions_100, pills_100, 
           total_prescribed_dose_100,
           total_dose_per_prescription_100,
           average_daily_dose_100)

# 300mg data
gabapentin_300 <- left_join(gabapentin_300_cap, neurontin_300_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items
    mutate(prescriptions_300 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities 
    mutate(pills_300 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_300, pills_300) %>% 
   # Total dose per month (dose = 300mg)
    mutate(total_prescribed_dose_300 = pills_300 * 300) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_300 = total_prescribed_dose_300 / prescriptions_300) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_300 = total_dose_per_prescription_300 / 30) %>% 
    # Select columns
    select(date, prescriptions_300, pills_300,  
           total_prescribed_dose_300,
           total_dose_per_prescription_300, 
           average_daily_dose_300)

# 400mg data
gabapentin_400 <- left_join(gabapentin_400_cap, neurontin_400_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items
    mutate(prescriptions_400 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities 
    mutate(pills_400 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_400, pills_400) %>% 
    # Total dose per month (dose = 400mg)
    mutate(total_prescribed_dose_400 = pills_400 * 400) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_400 = total_prescribed_dose_400 / prescriptions_400) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_400 = total_dose_per_prescription_400 / 30) %>% 
    # Select columns
    select(date, prescriptions_400, pills_400,  
           total_prescribed_dose_400,
           total_dose_per_prescription_400,
           average_daily_dose_400)

# 600mg data
gabapentin_600 <- left_join(gabapentin_600_tab, neurontin_600_tab, by = 'date') %>% 
    rowwise() %>% 
    # Sum items
    mutate(prescriptions_600 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities 
    mutate(pills_600 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_600, pills_600) %>% 
    # Total dose per month (dose = 600mg)
    mutate(total_prescribed_dose_600 = pills_600 * 600) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_600 = total_prescribed_dose_600 / prescriptions_600) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_600 = total_dose_per_prescription_600 / 30) %>% 
    # Select columns
    select(date, prescriptions_600, pills_600, 
           total_prescribed_dose_600, 
           total_dose_per_prescription_600,
           average_daily_dose_600)

# 800mg data
gabapentin_800 <- left_join(gabapentin_800_tab, neurontin_800_tab, by = 'date') %>% 
    rowwise() %>% 
    # Sum items
    mutate(prescriptions_800 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities 
    mutate(pills_800 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Select columns
    select(date, prescriptions_800, pills_800) %>% 
    # Total dose per month (dose = 800mg)
    mutate(total_prescribed_dose_800 = pills_800 * 800) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_800 = total_prescribed_dose_800 / prescriptions_800) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_800 = total_dose_per_prescription_800 / 30) %>% 
    # Select columns
    select(date, prescriptions_800, pills_800, 
           total_prescribed_dose_800, 
           total_dose_per_prescription_800,
           average_daily_dose_800)

#-- Join all dose data --#
gabapentin <- gabapentin_100 %>% 
    left_join(gabapentin_300) %>% 
    left_join(gabapentin_400) %>% 
    left_join(gabapentin_600) %>% 
    left_join(gabapentin_800)

#-- Calculate monthly totals --#
gabapentin_totals <- gabapentin %>% 
    rowwise() %>% 
    # Prescriptions
    mutate(prescriptions_total = sum(c_across(cols = starts_with('prescriptions')), na.rm = TRUE)) %>% 
    # Pills
    mutate(pills_total = sum(c_across(cols = starts_with('pills')), na.rm = TRUE)) %>%
    # Total dose
    mutate(dose_total = sum(c_across(cols = starts_with('total_prescribed')), na.rm = TRUE)) %>%
    ungroup() %>% 
    # Total dose per prescription
    mutate(dose_per_prescription_total = dose_total / prescriptions_total) %>% 
    # Average daily dose (assuming that each prescription item is for 30 days)
    mutate(average_daily_dose_total = dose_per_prescription_total / 30)

#-- Save analysis set --#
gabapentin_totals %>% 
    # Select month range (starting 2017-04-01 to 2021-03-01)
    filter(date >= as.Date('2017-04-01') & date < as.Date('2021-04-01')) %>% 
    # Add month counter
    mutate(month = row_number()) %>% 
    # Arrange columns
    select(date, month, everything()) %>% 
    write_csv(file = 'data-clean/gabapentin_analysis-set.csv')

#-- Save full dataset --#
gabapentin_totals %>% 
    write_csv(file = 'data-clean/gabapentin_full-record.csv')
