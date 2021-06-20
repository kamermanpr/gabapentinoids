# Load packages   
library(dplyr)
library(readr)

############################################################
#                                                          #
#                   Clean data: by year                    #
#                                                          #
############################################################

# Fetching and cleaning the data only needs to be done once.
# Date completed: 2021-06-20

###############################################
#   Fetch the data from openprescribing.net   #
###############################################
#-- OpenPrescribing API details --#
# API details can be found here: https://openprescribing.net/api/
# Drug codes can be obtained here: https://openprescribing.net/analyse/

#-- Download pregabalin data (chemical designation only) --#
# Total items prescribed and quantity of tablets/capsules
# Code: 0408010AE
pregabalin <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AE&format=csv') 

#-- Download gabapentin data (chemical designation only) --#
# Total items prescribed and quantity of tablets/capsules 
# under anti-epileptic drugs
# Code: 0408010G0
gabapentin_epi <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0&format=csv') 

# Total items prescribed and quantity of tablets/capsules 
# under drugs for neuropathic pain
# Code: 0407030AD
gabapentin_neup <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0407030AD&format=csv') 

################################
#   Combined gabapentin data   #
################################
gabapentin <- gabapentin_epi %>% 
    left_join(gabapentin_neup, by = 'date')

#############
#   Clean   #
#############
#-- Pregabalin --#
pregabalin_clean <- pregabalin %>% 
    # Select columns
    select(date, items, quantity) %>% 
    # Filter date range: April 2017 to April 2021
    # Note: April 2021 was the last data point so only need to remove dates before April 2017.
    filter(date >= as.Date('2017-04-01')) %>% 
    # Calculate quantity per item
    mutate(quantity_per_item = quantity / items)

#-- Gabapentin --#
gabapentin_clean <- gabapentin %>% 
    # Select columns
    select(date, items.x, items.y, quantity.x, quantity.y) %>% 
    # Filter date range: April 2017 to April 2021
    # Note: April 2021 was the last data point so only need to remove dates before April 2017.
    filter(date >= as.Date('2017-04-01')) %>% 
    rowwise() %>% 
    # Sum items columns from items.x (antiepileptic) and items.y (neuropathic pain)
    mutate(items = sum(c_across(cols = starts_with('items')), na.rm = TRUE)) %>% 
    # Sum items columns from quantity.x (antiepileptic) and quantity.y (neuropathic pain)
    mutate(quantity = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Select columns
    select(date, items, quantity) %>% 
    # Calculate quantity per item
    mutate(quantity_per_item = quantity / items)

####################
#   Save to file   #
####################
#-- Create directory for saved files --#
if(!dir.exists('data-clean')) {
    dir.create('data-clean')
}

#-- Save pregabalin --#
write_csv(x = pregabalin_clean,
          file = 'data-clean/analysis-set_pregabalin.csv')

#-- Save gabapentin --#
write_csv(x = gabapentin_clean,
          file = 'data-clean/analysis-set_gabapentin.csv')

##############################################
#   Full datasets downloaded on 2021-06-17   #
##############################################
#-- Pregabalin --#
pregabalin %>% 
    # Select columns
    select(date, items, quantity) %>% 
    # Calculate quantity per item
    mutate(quantity_per_item = quantity / items) %>% 
    # Write to file
    write_csv(file = 'data-clean/full-record_pregabalin.csv')

#-- Gabapentin --#
gabapentin %>% 
    # Select columns
    select(date, items.x, items.y, quantity.x, quantity.y) %>%
    rowwise() %>% 
    # Sum items columns from items.x (antiepileptic) and items.y (neuropathic pain)
    mutate(items = sum(c_across(cols = starts_with('items')), na.rm = TRUE)) %>% 
    # Sum items columns from quantity.x (antiepileptic) and quantity.y (neuropathic pain)
    mutate(quantity = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Select columns
    select(date, items, quantity) %>%
    # Calculate quantity per item
    mutate(quantity_per_item = quantity / items) %>% 
    # Write to file
    write_csv(file = 'data-clean/full-record_gabapentin.csv')
    
############################################################
#                                                          #
#               Clean data: by CCG and year                #
#                                                          #
############################################################

# Fetching and cleaning the data only needs to be done once.
# Date completed: 2021-06-20

###############################################
#   Fetch the data from openprescribing.net   #
###############################################
#-- OpenPrescribing API details --#
# API details can be found here: https://openprescribing.net/api/
# Drug codes can be obtained here: https://openprescribing.net/analyse/

#-- Download pregabalin data (chemical designation only) --#
# Total items prescribed and quantity of tablets/capsules
# Code: 0408010AE
pregabalin_ccg <- read_csv('https://openprescribing.net/api/1.0/spending_by_ccg/?code=0408010AE&format=csv') 

#-- Download gabapentin data (chemical designation only) --#
# Total items prescribed uand quantity of tablets/capsules
# under anti-epileptic drugs
# Code: 0408010G0
gabapentin_ccg_epi <- read_csv('https://openprescribing.net/api/1.0/spending_by_ccg/?code=0408010G0&format=csv') 

# Total items prescribed and quantity of tablets/capsules
# under drugs for neuropathic pain
# Code: 0407030AD
gabapentin_ccg_neup <- read_csv('https://openprescribing.net/api/1.0/spending_by_ccg/?code=0407030AD&format=csv') 

################################
#   Combined gabapentin data   #
################################
gabapentin_ccg <- gabapentin_ccg_epi %>% 
    left_join(gabapentin_ccg_neup, by = c('row_id', 'row_name', 'date'))

#############
#   Clean   #
#############
#-- Pregabalin --#
pregabalin_ccg_clean <- pregabalin_ccg %>% 
    # Select columns
    select(row_id, row_name, date, items, quantity) %>% 
    # Calculate quantity per item
    mutate(quantity_per_item = quantity / items) %>% 
    # Filter date range: April 2017 to April 2021
    # Note: April 2021 was the last data point so only need to remove dates before April 2017.
    filter(date >= as.Date('2017-04-01'))

#-- Gabapentin --#
gabapentin_ccg_clean <- gabapentin_ccg %>% 
    # Select columns
    select(row_id, row_name, date, items.x, items.y, quantity.x, quantity.y) %>% 
    # Filter date range: April 2017 to April 2021
    # Note: April 2021 was the last data point so only need to remove dates before April 2017.
    filter(date >= as.Date('2017-04-01')) %>% 
    rowwise() %>% 
    # Sum items columns from items.x (antiepileptic) and items.y (neuropathic pain)
    mutate(items = sum(c_across(cols = starts_with('items')), na.rm = TRUE)) %>% 
    # Sum items columns from quantity.x (antiepileptic) and quantity.y (neuropathic pain)
    mutate(quantity = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Select columns
    select(row_id, row_name, date, items, quantity) %>% 
    # Calculate quantity per item
    mutate(quantity_per_item = quantity / items)

####################
#   Save to file   #
####################
#-- Create directory for saved files --#
if(!dir.exists('data-clean')) {
    dir.create('data-clean')
}

#-- Save pregabalin --#
write_csv(x = pregabalin_ccg_clean,
          file = 'data-clean/analysis-set_pregabalin_ccg.csv')

#-- Save gabapentin --#
write_csv(x = gabapentin_ccg_clean,
          file = 'data-clean/analysis-set_gabapentin_ccg.csv')

##############################################
#   Full datasets downloaded on 2021-06-18   #
##############################################
#-- Pregabalin --#
pregabalin_ccg %>% 
    # Select columns
    select(row_id, row_name, date, items, quantity) %>% 
    # Calculate quantity per item
    mutate(quantity_per_item = quantity / items) %>% 
    # Write to file
    write_csv(file = 'data-clean/full-record_pregabalin_ccg.csv')

#-- Gabapentin --#
gabapentin_ccg %>% 
    # Select columns
    select(row_id, row_name, date, items.x, items.y, quantity.x, quantity.y) %>% 
    rowwise() %>% 
    # Sum items columns from items.x (antiepileptic) and items.y (neuropathic pain)
    mutate(items = sum(c_across(cols = starts_with('items')), na.rm = TRUE)) %>% 
    # Sum items columns from quantity.x (antiepileptic) and quantity.y (neuropathic pain)
    mutate(quantity = sum(c_across(cols = starts_with('quantity')), na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Select columns
    select(row_id, row_name, date, items, quantity) %>% 
    # Calculate quantity per item
    mutate(quantity_per_item = quantity / items) %>% 
    # Write to file
    write_csv(file = 'data-clean/full-record_gabapentin_ccg.csv')
