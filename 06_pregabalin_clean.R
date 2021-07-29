############################################################
#                                                          #
#                     Pregabalin: clean                    #
#                                                          #
############################################################

#-- Load packages --#
library(dplyr)
library(readr)
library(lubridate)

#-- Notes --#
# Items: prescriptions
# Quantity: number of pills
# When calculating daily dose per month, all prescriptions assumed to be for 30 days

#-- Import data --#
# 25mg data
pregabalin_25_cap <- read_csv(file = 'data-original/pregabalin_25_cap.csv')

pregabalin_25_tab <- read_csv(file = 'data-original/pregabalin_25_tab.csv')

lyrica_25_cap <- read_csv(file = 'data-original/lyrica_25_cap.csv')

# 50mg 
pregabalin_50_cap <- read_csv(file = 'data-original/pregabalin_50_cap.csv')

pregabalin_50_tab <- read_csv(file = 'data-original/pregabalin_50_tab.csv')

lyrica_50_cap <- read_csv(file = 'data-original/lyrica_50_cap.csv')

# 75mg 
pregabalin_75_cap <- read_csv(file = 'data-original/pregabalin_75_cap.csv')

pregabalin_75_tab <- read_csv(file = 'data-original/pregabalin_75_tab.csv')

lyrica_75_cap <- read_csv(file = 'data-original/lyrica_75_cap.csv')

# 100mg 
pregabalin_100_cap <- read_csv(file = 'data-original/pregabalin_100_cap.csv')

pregabalin_100_tab <- read_csv(file = 'data-original/pregabalin_100_tab.csv')

lyrica_100_cap <- read_csv(file = 'data-original/lyrica_100_cap.csv')

# 150mg 
pregabalin_150_cap <- read_csv(file = 'data-original/pregabalin_150_cap.csv')

pregabalin_150_tab <- read_csv(file = 'data-original/pregabalin_150_tab.csv')

lyrica_150_cap <- read_csv(file = 'data-original/lyrica_150_cap.csv')

# 200mg 
pregabalin_200_cap <- read_csv(file = 'data-original/pregabalin_200_cap.csv')

pregabalin_200_tab <- read_csv(file = 'data-original/pregabalin_200_tab.csv')

lyrica_200_cap <- read_csv(file = 'data-original/lyrica_200_cap.csv')

# 225mg 
pregabalin_225_cap <- read_csv(file = 'data-original/pregabalin_225_cap.csv')

pregabalin_225_tab <- read_csv(file = 'data-original/pregabalin_225_tab.csv')

lyrica_225_cap <- read_csv(file = 'data-original/lyrica_225_cap.csv')

# 300mg 
pregabalin_300_cap <- read_csv(file = 'data-original/pregabalin_300_cap.csv')

pregabalin_300_tab <- read_csv(file = 'data-original/pregabalin_300_tab.csv')

lyrica_300_cap <- read_csv(file = 'data-original/lyrica_300_cap.csv')

#-- Create output directory for clean data --#
if(!dir.exists('data-clean')) {
    dir.create('data-clean')
}

#-- Clean data --#
# 25mg data
pregabalin_25 <- left_join(pregabalin_25_cap, pregabalin_25_tab, by = 'date') %>% 
    left_join(lyrica_25_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_25 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_25 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_25, pills_25) %>% 
    # Add days in a month
    mutate(days_per_month = days_in_month(date)) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_25 = pills_25 * 25) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_25 = total_prescribed_dose_25 / prescriptions_25) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_25 = total_dose_per_prescription_25 / 30) %>% 
    # Select columns
    select(date, prescriptions_25, pills_25, average_daily_dose_25)

# 50mg data
pregabalin_50 <- left_join(pregabalin_50_cap, pregabalin_50_tab, by = 'date') %>% 
    left_join(lyrica_50_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_50 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_50 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_50, pills_50) %>% 
    # Add days in a month
    mutate(days_per_month = days_in_month(date)) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_50 = pills_50 * 50) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_50 = total_prescribed_dose_50 / prescriptions_50) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_50 = total_dose_per_prescription_50 / 30) %>% 
    # Select columns
    select(date, prescriptions_50, pills_50, average_daily_dose_50)

# 75mg data
pregabalin_75 <- left_join(pregabalin_75_cap, pregabalin_75_tab, by = 'date') %>% 
    left_join(lyrica_75_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_75 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_75 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_75, pills_75) %>% 
    # Add days in a month
    mutate(days_per_month = days_in_month(date)) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_75 = pills_75 * 75) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_75 = total_prescribed_dose_75 / prescriptions_75) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_75 = total_dose_per_prescription_75 / 30) %>% 
    # Select columns
    select(date, prescriptions_75, pills_75, average_daily_dose_75)

# 100mg data
pregabalin_100 <- left_join(pregabalin_100_cap, pregabalin_100_tab, by = 'date') %>% 
    left_join(lyrica_100_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_100 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_100 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_100, pills_100) %>% 
    # Add days in a month
    mutate(days_per_month = days_in_month(date)) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_100 = pills_100 * 100) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_100 = total_prescribed_dose_100 / prescriptions_100) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_100 = total_dose_per_prescription_100 / 30) %>% 
    # Select columns
    select(date, prescriptions_100, pills_100, average_daily_dose_100)

# 150mg data
pregabalin_150 <- left_join(pregabalin_150_cap, pregabalin_150_tab, by = 'date') %>% 
    left_join(lyrica_150_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_150 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_150 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_150, pills_150) %>% 
    # Add days in a month
    mutate(days_per_month = days_in_month(date)) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_150 = pills_150 * 150) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_150 = total_prescribed_dose_150 / prescriptions_150) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_150 = total_dose_per_prescription_150 / 30) %>% 
    # Select columns
    select(date, prescriptions_150, pills_150, average_daily_dose_150)

# 200mg
pregabalin_200 <- left_join(pregabalin_200_cap, pregabalin_200_tab, by = 'date') %>% 
    left_join(lyrica_200_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_200 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_200 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_200, pills_200) %>% 
    # Add days in a month
    mutate(days_per_month = days_in_month(date)) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_200 = pills_200 * 200) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_200 = total_prescribed_dose_200 / prescriptions_200) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_200 = total_dose_per_prescription_200 / 30) %>% 
    # Select columns
    select(date, prescriptions_200, pills_200, average_daily_dose_200)

# 225mg
pregabalin_225 <- left_join(pregabalin_225_cap, pregabalin_225_tab, by = 'date') %>% 
    left_join(lyrica_225_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_225 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_225 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_225, pills_225) %>% 
    # Add days in a month
    mutate(days_per_month = days_in_month(date)) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_225 = pills_225 * 225) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_225 = total_prescribed_dose_225 / prescriptions_225) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_225 = total_dose_per_prescription_225 / 30) %>% 
    # Select columns
    select(date, prescriptions_225, pills_225, average_daily_dose_225)

# 300mg
pregabalin_300 <- left_join(pregabalin_300_cap, pregabalin_300_tab, by = 'date') %>% 
    left_join(lyrica_300_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_300 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_300 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_300, pills_300) %>% 
    # Add days in a month
    mutate(days_per_month = days_in_month(date)) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_300 = pills_300 * 300) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_300 = total_prescribed_dose_300 / prescriptions_300) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_300 = total_dose_per_prescription_300 / 30) %>% 
    # Select columns
    select(date, prescriptions_300, pills_300, average_daily_dose_300)

#-- Join all dose data --#
pregabalin <- pregabalin_25 %>% 
    left_join(pregabalin_50) %>% 
    left_join(pregabalin_75) %>% 
    left_join(pregabalin_100) %>% 
    left_join(pregabalin_150) %>% 
    left_join(pregabalin_200) %>% 
    left_join(pregabalin_225) %>% 
    left_join(pregabalin_300) 

#-- Calculate monthly totals --#
pregabalin_totals <- pregabalin %>% 
    rowwise() %>% 
    # Prescriptions
    mutate(prescriptions_total = sum(c_across(cols = starts_with('prescription')), na.rm = TRUE)) %>% 
    # Pills
    mutate(pills_total = sum(c_across(cols = starts_with('pills')), na.rm = TRUE)) %>% 
    ungroup() 

#-- Get dose weightings --#
pregabalin_weighted <- pregabalin_totals %>% 
    # Divide each pills_* by pills_total
    mutate(weight_25 = pills_25 / pills_total,
           weight_50 = pills_50 / pills_total,
           weight_75 = pills_75 / pills_total,
           weight_100 = pills_100 / pills_total,
           weight_150 = pills_150 / pills_total,
           weight_200 = pills_200 / pills_total,
           weight_225 = pills_225 / pills_total,
           weight_300 = pills_300 / pills_total) %>% 
    # Multiple each dose_* by weight
    mutate(weighted_average_daily_dose_25 = average_daily_dose_25 * weight_25,
           weighted_average_daily_dose_50 = average_daily_dose_50 * weight_50,
           weighted_average_daily_dose_75 = average_daily_dose_75 * weight_75,
           weighted_average_daily_dose_100 = average_daily_dose_100 * weight_100,
           weighted_average_daily_dose_150 = average_daily_dose_150 * weight_150,
           weighted_average_daily_dose_200 = average_daily_dose_200 * weight_200,
           weighted_average_daily_dose_225 = average_daily_dose_225 * weight_225,
           weighted_average_daily_dose_300 = average_daily_dose_300 * weight_300) %>% 
    # Get average daily dose
    rowwise() %>% 
    mutate(weighted_average_daily_dose_total = sum(c_across(starts_with('weighted_')), na.rm = TRUE)) %>% 
    ungroup()

#-- Save analysis set --#
pregabalin_weighted %>% 
    # Select month range (starting April 2017 to latest date)
    filter(date >= as.Date('2017-04-01')) %>% 
    # Add month counter
    mutate(month = row_number()) %>% 
    # Arrange columns
    select(date, month, everything()) %>% 
    write_csv(file = 'data-clean/pregabalin_analysis-set.csv')

#-- Save full dataset --#
pregabalin_weighted %>% 
    write_csv(file = 'data-clean/pregabalin_full-record.csv')
