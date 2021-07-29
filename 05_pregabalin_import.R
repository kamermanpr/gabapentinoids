############################################################
#                                                          #
#         Pregabalin: import and preliminary clean         #
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
pregabalin_25_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAAAAA&format=csv') 
pregabalin_25_tab <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAASAS&format=csv') 
pregabalin_50_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAABAB&format=csv') 
pregabalin_50_tab <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAATAT&format=csv') 
pregabalin_75_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAACAC&format=csv') 
pregabalin_75_tab <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAAXAX&format=csv') 
pregabalin_100_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAADAD&format=csv') 
pregabalin_100_tab <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAAUAU&format=csv') 
pregabalin_150_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAAEAE&format=csv') 
pregabalin_150_tab <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAAYAY&format=csv') 
pregabalin_200_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAAFAF&format=csv') 
pregabalin_200_tab <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAAVAV&format=csv') 
pregabalin_225_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAAIAI&format=csv') 
pregabalin_225_tab <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAAZAZ&format=csv') 
pregabalin_300_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAAGAG&format=csv') 
pregabalin_300_tab <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEAAAWAW&format=csv') 
lyrica_25_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBBAAAA&format=csv')
lyrica_50_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBBABAB&format=csv')
lyrica_75_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBBACAC&format=csv')
lyrica_100_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBBADAD&format=csv')
lyrica_150_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBBAEAE&format=csv')
lyrica_200_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBBAFAF&format=csv')
lyrica_225_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBBAHAI&format=csv')
lyrica_300_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBBAGAG&format=csv')

#-- Save original data --#
if(!dir.exists('data-original')) {
    dir.create('data-original')
}

# 25mg data
write_csv(x = pregabalin_25_cap,
          file = 'data-original/pregabalin_25_cap.csv')

write_csv(x = pregabalin_25_tab,
          file = 'data-original/pregabalin_25_tab.csv')

write_csv(x = lyrica_25_cap,
          file = 'data-original/lyrica_25_cap.csv')

# 50mg data
write_csv(x = pregabalin_50_cap,
          file = 'data-original/pregabalin_50_cap.csv')

write_csv(x = pregabalin_50_tab,
          file = 'data-original/pregabalin_50_tab.csv')

write_csv(x = lyrica_50_cap,
          file = 'data-original/lyrica_50_cap.csv')

# 75mg data
write_csv(x = pregabalin_75_cap,
          file = 'data-original/pregabalin_75_cap.csv')

write_csv(x = pregabalin_75_tab,
          file = 'data-original/pregabalin_75_tab.csv')

write_csv(x = lyrica_75_cap,
          file = 'data-original/lyrica_75_cap.csv')

# 100mg
write_csv(x = pregabalin_100_cap,
          file = 'data-original/pregabalin_100_cap.csv')

write_csv(x = pregabalin_100_tab,
          file = 'data-original/pregabalin_100_tab.csv')

write_csv(x = lyrica_100_cap,
          file = 'data-original/lyrica_100_cap.csv')

# 150mg
write_csv(x = pregabalin_150_cap,
          file = 'data-original/pregabalin_150_cap.csv')

write_csv(x = pregabalin_150_tab,
          file = 'data-original/pregabalin_150_tab.csv')

write_csv(x = lyrica_150_cap,
          file = 'data-original/lyrica_150_cap.csv')

# 200mg
write_csv(x = pregabalin_200_cap,
          file = 'data-original/pregabalin_200_cap.csv')

write_csv(x = pregabalin_200_tab,
          file = 'data-original/pregabalin_200_tab.csv')

write_csv(x = lyrica_200_cap,
          file = 'data-original/lyrica_200_cap.csv')

# 225mg
write_csv(x = pregabalin_225_cap,
          file = 'data-original/pregabalin_225_cap.csv')

write_csv(x = pregabalin_225_tab,
          file = 'data-original/pregabalin_225_tab.csv')

write_csv(x = lyrica_225_cap,
          file = 'data-original/lyrica_225_cap.csv')

# 300mg
write_csv(x = pregabalin_300_cap,
          file = 'data-original/pregabalin_300_cap.csv')

write_csv(x = pregabalin_300_tab,
          file = 'data-original/pregabalin_300_tab.csv')

write_csv(x = lyrica_300_cap,
          file = 'data-original/lyrica_300_cap.csv')

#-- Process data by dose --#
if(!dir.exists('data-clean')) {
    dir.create('data-clean')
}

# 25mg data
pregabalin_25 <- left_join(pregabalin_25_cap, pregabalin_25_tab, by = 'date') %>% 
    left_join(lyrica_25_cap, by = 'date') %>%  
    rowwise() %>% 
    # Sum items
    mutate(prescriptions_25 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities 
    mutate(pills_25 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_25, pills_25) %>% 
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_25 = pills_25 / prescriptions_25)

# 50mg data
pregabalin_50 <- left_join(pregabalin_50_cap, pregabalin_50_tab, by = 'date') %>% 
    left_join(lyrica_50_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items
    mutate(prescriptions_50 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities 
    mutate(pills_50 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_50, pills_50) %>% 
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_50 = pills_50 / prescriptions_50)

# 75mg data
pregabalin_75 <- left_join(pregabalin_75_cap, pregabalin_75_tab, by = 'date') %>% 
    left_join(lyrica_75_cap, by = 'date') %>%  
    rowwise() %>% 
    # Sum items
    mutate(prescriptions_75 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities 
    mutate(pills_75 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>%  
    # Select columns
    select(date, prescriptions_75, pills_75) %>% 
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_75 = pills_75 / prescriptions_75)

# 100mg data
pregabalin_100 <- left_join(pregabalin_100_cap, pregabalin_100_tab, by = 'date') %>% 
    left_join(lyrica_100_cap, by = 'date') %>%  
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

# 150mg data
pregabalin_150 <- left_join(pregabalin_150_cap, pregabalin_150_tab, by = 'date') %>% 
    left_join(lyrica_150_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items
    mutate(prescriptions_150 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities 
    mutate(pills_150 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Select columns
    select(date, prescriptions_150, pills_150) %>% 
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_150 = pills_150 / prescriptions_150)

# 200mg data
pregabalin_200 <- left_join(pregabalin_200_cap, pregabalin_200_tab, by = 'date') %>% 
    left_join(lyrica_200_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items
    mutate(prescriptions_200 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities 
    mutate(pills_200 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Select columns
    select(date, prescriptions_200, pills_200) %>% 
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_200 = pills_200 / prescriptions_200)

# 225mg data
pregabalin_225 <- left_join(pregabalin_225_cap, pregabalin_225_tab, by = 'date') %>% 
    left_join(lyrica_225_cap, by = 'date') %>% 
    rowwise() %>% 
    # Sum items
    mutate(prescriptions_225 = sum(c_across(cols = starts_with('item')), na.rm = TRUE)) %>% 
    # Sum quantities 
    mutate(pills_225 = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Select columns
    select(date, prescriptions_225, pills_225) %>% 
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_225 = pills_225 / prescriptions_225)

# 300mg data
pregabalin_300 <- left_join(pregabalin_300_cap, pregabalin_300_tab, by = 'date') %>% 
    left_join(lyrica_300_cap, by = 'date') %>% 
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
    ungroup() %>% 
    # Calculate pills (quantity) per prescription (item)
    mutate(quantity_per_item_total = pills_total / prescriptions_total)

#-- Save analysis set --#
pregabalin_totals %>% 
    # Select month range (starting April 2017 to latest date)
    filter(date >= as.Date('2017-04-01')) %>% 
    # Add month counter
    mutate(month = row_number()) %>% 
    # Arrange columns
    select(date, month, everything()) %>% 
    write_csv(file = 'data-clean/pregabalin_analysis-set.csv')

#-- Save full dataset --#
pregabalin_totals %>% 
    write_csv(file = 'data-clean/pregabalin_full-record.csv')

