############################################################
#                                                          #
#         Gabapentin: import and preliminary clean         #
#                                                          #
############################################################

#-- Date of download --#
# 2021-06-20

#- load packages --#
library(dplyr)
library(readr)

#-- Download all tablet/capsule data --#
# cap: capsule
# tab: tablet
gabapentin_100_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0AAAAAA&format=csv') 
gabapentin_300_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0AAABAB&format=csv') 
gabapentin_400_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0AAACAC&format=csv') 
gabapentin_600_tab <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0AAAJAJ&format=csv') 
gabapentin_800_tab <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0AAAKAK&format=csv') 
neurontin_100_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0BBAAAA&format=csv') 
neurontin_300_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0BBABAB&format=csv') 
neurontin_400_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0BBACAC&format=csv') 
neurontin_600_tab <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0BBADAJ&format=csv') 
neurontin_800_tab <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0BBAEAK&format=csv') 

#-- Save original data --#
if(!dir.exists('data-original')) {
    dir.create('data-original')
}

# 100mg data
write_csv(x = gabapentin_100_cap,
          file = 'data-original/gabapentin_100_cap.csv')

write_csv(x = neurontin_100_cap,
          file = 'data-original/neurontin_100_cap.csv')

# 300mg 
write_csv(x = gabapentin_300_cap,
          file = 'data-original/gabapentin_300_cap.csv')

write_csv(x = neurontin_300_cap,
          file = 'data-original/neurontin_300_cap.csv')

# 400mg 
write_csv(x = gabapentin_400_cap,
          file = 'data-original/gabapentin_400_cap.csv')

write_csv(x = neurontin_400_cap,
          file = 'data-original/neurontin_400_cap.csv')

# 600mg 
write_csv(x = gabapentin_600_tab,
          file = 'data-original/gabapentin_600_tab.csv')

write_csv(x = neurontin_600_tab,
          file = 'data-original/neurontin_600_tab.csv')

# 800mg 
write_csv(x = gabapentin_800_tab,
          file = 'data-original/gabapentin_800_tab.csv')

write_csv(x = neurontin_800_tab,
          file = 'data-original/neurontin_800_tab.csv')

#-- Process data by dose --#
if(!dir.exists('data-clean')) {
    dir.create('data-clean')
}

# 100mg data
gabapentin_100 <- left_join(gabapentin_100_cap, neurontin_100_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items
    mutate(prescriptions_100 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities 
    mutate(pills_100 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_100, pills_100) %>% 
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_100 = pills_100 / prescriptions_100)

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
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_300 = pills_300 / prescriptions_300)

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
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_400 = pills_400 / prescriptions_400)

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
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_600 = pills_600 / prescriptions_600)

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
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_800 = pills_800 / prescriptions_800)

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
    mutate(prescriptions_total = sum(c_across(cols = starts_with('prescription')), na.rm = TRUE)) %>% 
    # Pills
    mutate(pills_total = sum(c_across(cols = starts_with('pills')), na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_total = pills_total / prescriptions_total)

#-- Save analysis set --#
gabapentin_totals %>% 
    # Select month range (starting April 2017 to latest date)
    filter(date >= as.Date('2017-04-01')) %>% 
    # Add month counter
    mutate(month = row_number()) %>% 
    # Arrange columns
    select(date, month, everything()) %>% 
    write_csv(file = 'data-clean/gabapentin_analysis-set.csv')

#-- Save full dataset --#
gabapentin_totals %>% 
    write_csv(file = 'data-clean/gabapentin_full-record.csv')
