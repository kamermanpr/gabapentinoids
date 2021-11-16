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

alzain_25_cap <- read_csv(file = 'data-original/alzain_25_cap.csv')

axalid_25_cap <- read_csv(file = 'data-original/axalid_25_cap.csv')

lecaent_25_cap <- read_csv(file = 'data-original/lecaent_25_cap.csv')

rewisca_25_cap <- read_csv(file = 'data-original/rewisca_25_cap.csv')

# 50mg 
pregabalin_50_cap <- read_csv(file = 'data-original/pregabalin_50_cap.csv')

pregabalin_50_tab <- read_csv(file = 'data-original/pregabalin_50_tab.csv')

lyrica_50_cap <- read_csv(file = 'data-original/lyrica_50_cap.csv')

alzain_50_cap <- read_csv(file = 'data-original/alzain_50_cap.csv')

axalid_50_cap <- read_csv(file = 'data-original/axalid_50_cap.csv')

lecaent_50_cap <- read_csv(file = 'data-original/lecaent_50_cap.csv')

rewisca_50_cap <- read_csv(file = 'data-original/rewisca_50_cap.csv')

# 75mg 
pregabalin_75_cap <- read_csv(file = 'data-original/pregabalin_75_cap.csv')

pregabalin_75_tab <- read_csv(file = 'data-original/pregabalin_75_tab.csv')

lyrica_75_cap <- read_csv(file = 'data-original/lyrica_75_cap.csv')

alzain_75_cap <- read_csv(file = 'data-original/alzain_75_cap.csv')

axalid_75_cap <- read_csv(file = 'data-original/axalid_75_cap.csv')

lecaent_75_cap <- read_csv(file = 'data-original/lecaent_75_cap.csv')

rewisca_75_cap <- read_csv(file = 'data-original/rewisca_75_cap.csv')

# 100mg 
pregabalin_100_cap <- read_csv(file = 'data-original/pregabalin_100_cap.csv')

pregabalin_100_tab <- read_csv(file = 'data-original/pregabalin_100_tab.csv')

lyrica_100_cap <- read_csv(file = 'data-original/lyrica_100_cap.csv')

alzain_100_cap <- read_csv(file = 'data-original/alzain_100_cap.csv')

axalid_100_cap <- read_csv(file = 'data-original/axalid_100_cap.csv')

lecaent_100_cap <- read_csv(file = 'data-original/lecaent_100_cap.csv')

rewisca_100_cap <- read_csv(file = 'data-original/rewisca_100_cap.csv')

# 150mg 
pregabalin_150_cap <- read_csv(file = 'data-original/pregabalin_150_cap.csv')

pregabalin_150_tab <- read_csv(file = 'data-original/pregabalin_150_tab.csv')

lyrica_150_cap <- read_csv(file = 'data-original/lyrica_150_cap.csv')

alzain_150_cap <- read_csv(file = 'data-original/alzain_150_cap.csv')

axalid_150_cap <- read_csv(file = 'data-original/axalid_150_cap.csv')

lecaent_150_cap <- read_csv(file = 'data-original/lecaent_150_cap.csv')

rewisca_150_cap <- read_csv(file = 'data-original/rewisca_150_cap.csv')

# 200mg 
pregabalin_200_cap <- read_csv(file = 'data-original/pregabalin_200_cap.csv')

pregabalin_200_tab <- read_csv(file = 'data-original/pregabalin_200_tab.csv')

lyrica_200_cap <- read_csv(file = 'data-original/lyrica_200_cap.csv')

alzain_200_cap <- read_csv(file = 'data-original/alzain_200_cap.csv')

axalid_200_cap <- read_csv(file = 'data-original/axalid_200_cap.csv')

lecaent_200_cap <- read_csv(file = 'data-original/lecaent_200_cap.csv')

rewisca_200_cap <- read_csv(file = 'data-original/rewisca_200_cap.csv')

# 225mg 
pregabalin_225_cap <- read_csv(file = 'data-original/pregabalin_225_cap.csv')

pregabalin_225_tab <- read_csv(file = 'data-original/pregabalin_225_tab.csv')

lyrica_225_cap <- read_csv(file = 'data-original/lyrica_225_cap.csv')

alzain_225_cap <- read_csv(file = 'data-original/alzain_225_cap.csv')

axalid_225_cap <- read_csv(file = 'data-original/axalid_225_cap.csv')

lecaent_225_cap <- read_csv(file = 'data-original/lecaent_225_cap.csv')

rewisca_225_cap <- read_csv(file = 'data-original/rewisca_225_cap.csv')

# 300mg 
pregabalin_300_cap <- read_csv(file = 'data-original/pregabalin_300_cap.csv')

pregabalin_300_tab <- read_csv(file = 'data-original/pregabalin_300_tab.csv')

lyrica_300_cap <- read_csv(file = 'data-original/lyrica_300_cap.csv')

alzain_300_cap <- read_csv(file = 'data-original/alzain_300_cap.csv')

axalid_300_cap <- read_csv(file = 'data-original/axalid_300_cap.csv')

lecaent_300_cap <- read_csv(file = 'data-original/lecaent_300_cap.csv')

rewisca_300_cap <- read_csv(file = 'data-original/rewisca_300_cap.csv')

#-- Create output directory for clean data --#
if(!dir.exists('data-clean')) {
    dir.create('data-clean')
}

#-- Clean data --#
# 25mg data
pregabalin_25 <- left_join(pregabalin_25_cap, pregabalin_25_tab, by = 'date') %>% 
    left_join(lyrica_25_cap, by = 'date') %>% 
    left_join(alzain_25_cap, by = 'date') %>% 
    left_join(axalid_25_cap, by = 'date') %>% 
    left_join(lecaent_25_cap, by = 'date') %>% 
    left_join(rewisca_25_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_25 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_25 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_25, pills_25) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_25 = pills_25 * 25) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_25 = total_prescribed_dose_25 / prescriptions_25) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_25 = total_dose_per_prescription_25 / 30) %>% 
    # Select columns
    select(date, prescriptions_25, pills_25, 
           total_prescribed_dose_25,
           total_dose_per_prescription_25,
           average_daily_dose_25)

# 50mg data
pregabalin_50 <- left_join(pregabalin_50_cap, pregabalin_50_tab, by = 'date') %>% 
    left_join(lyrica_50_cap, by = 'date') %>% 
    left_join(alzain_50_cap, by = 'date') %>% 
    left_join(axalid_50_cap, by = 'date') %>% 
    left_join(lecaent_50_cap, by = 'date') %>% 
    left_join(rewisca_50_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_50 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_50 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_50, pills_50) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_50 = pills_50 * 50) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_50 = total_prescribed_dose_50 / prescriptions_50) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_50 = total_dose_per_prescription_50 / 30) %>% 
    # Select columns
    select(date, prescriptions_50, pills_50, 
           total_prescribed_dose_50,
           total_dose_per_prescription_50,
           average_daily_dose_50)

# 75mg data
pregabalin_75 <- left_join(pregabalin_75_cap, pregabalin_75_tab, by = 'date') %>% 
    left_join(lyrica_75_cap, by = 'date') %>%  
    left_join(alzain_75_cap, by = 'date') %>%  
    left_join(axalid_75_cap, by = 'date') %>%  
    left_join(lecaent_75_cap, by = 'date') %>% 
    left_join(rewisca_75_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_75 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_75 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_75, pills_75) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_75 = pills_75 * 75) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_75 = total_prescribed_dose_75 / prescriptions_75) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_75 = total_dose_per_prescription_75 / 30) %>% 
    # Select columns
    select(date, prescriptions_75, pills_75, 
           total_prescribed_dose_75,
           total_dose_per_prescription_75,
           average_daily_dose_75)

# 100mg data
pregabalin_100 <- left_join(pregabalin_100_cap, pregabalin_100_tab, by = 'date') %>% 
    left_join(lyrica_100_cap, by = 'date') %>%
    left_join(alzain_100_cap, by = 'date') %>%
    left_join(axalid_100_cap, by = 'date') %>%
    left_join(lecaent_100_cap, by = 'date') %>%
    left_join(rewisca_100_cap, by = 'date') %>% 
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

# 150mg data
pregabalin_150 <- left_join(pregabalin_150_cap, pregabalin_150_tab, by = 'date') %>% 
    left_join(lyrica_150_cap, by = 'date') %>% 
    left_join(alzain_150_cap, by = 'date') %>% 
    left_join(axalid_150_cap, by = 'date') %>% 
    left_join(lecaent_150_cap, by = 'date') %>% 
    left_join(rewisca_150_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_150 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_150 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_150, pills_150) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_150 = pills_150 * 150) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_150 = total_prescribed_dose_150 / prescriptions_150) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_150 = total_dose_per_prescription_150 / 30) %>% 
    # Select columns
    select(date, prescriptions_150, pills_150, 
           total_prescribed_dose_150,
           total_dose_per_prescription_150,
           average_daily_dose_150)

# 200mg
pregabalin_200 <- left_join(pregabalin_200_cap, pregabalin_200_tab, by = 'date') %>% 
    left_join(lyrica_200_cap, by = 'date') %>% 
    left_join(alzain_200_cap, by = 'date') %>% 
    left_join(axalid_200_cap, by = 'date') %>% 
    left_join(lecaent_200_cap, by = 'date') %>% 
    left_join(rewisca_200_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_200 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_200 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_200, pills_200) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_200 = pills_200 * 200) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_200 = total_prescribed_dose_200 / prescriptions_200) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_200 = total_dose_per_prescription_200 / 30) %>% 
    # Select columns
    select(date, prescriptions_200, pills_200, 
           total_prescribed_dose_200,
           total_dose_per_prescription_200,
           average_daily_dose_200)
# 225mg
pregabalin_225 <- left_join(pregabalin_225_cap, pregabalin_225_tab, by = 'date') %>% 
    left_join(lyrica_225_cap, by = 'date') %>% 
    left_join(alzain_225_cap, by = 'date') %>% 
    left_join(axalid_225_cap, by = 'date') %>% 
    left_join(lecaent_225_cap, by = 'date') %>% 
    left_join(rewisca_225_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_225 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_225 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_225, pills_225) %>% 
    # Total dose per month (dose = 100mg)
    mutate(total_prescribed_dose_225 = pills_225 * 225) %>% 
    # Dose per prescription
    mutate(total_dose_per_prescription_225 = total_prescribed_dose_225 / prescriptions_225) %>% 
    # Dose per month assuming a 30-day prescription
    mutate(average_daily_dose_225 = total_dose_per_prescription_225 / 30) %>% 
    # Select columns
    select(date, prescriptions_225, pills_225, 
           total_prescribed_dose_225,
           total_dose_per_prescription_225,
           average_daily_dose_225)

# 300mg
pregabalin_300 <- left_join(pregabalin_300_cap, pregabalin_300_tab, by = 'date') %>% 
    left_join(lyrica_300_cap, by = 'date') %>% 
    left_join(alzain_300_cap, by = 'date') %>% 
    left_join(axalid_300_cap, by = 'date') %>% 
    left_join(lecaent_300_cap, by = 'date') %>% 
    left_join(rewisca_300_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items (prescriptions)
    mutate(prescriptions_300 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities (number of pills)
    mutate(pills_300 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_300, pills_300) %>% 
    # Total dose per month (dose = 100mg)
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
pregabalin_totals %>% 
    # Select month range (starting 2017-04-01 to 2021-03-01)
    filter(date >= as.Date('2017-04-01') & date < as.Date('2021-04-01')) %>% 
    # Add month counter
    mutate(month = row_number()) %>% 
    # Arrange columns
    select(date, month, everything()) %>% 
    write_csv(file = 'data-clean/pregabalin_analysis-set.csv')

#-- Save full dataset --#
pregabalin_totals %>% 
    write_csv(file = 'data-clean/pregabalin_full-record.csv')
