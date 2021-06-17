############################################################
#                                                          #
#                        Clean data                        #
#                                                          #
############################################################

# Fetching and cleaning the data only needs to be done once.
# Date completed: 2021-06-17

#####################
#   Load packages   #
#####################
library(dplyr)
library(readr)

###############################################
#   Fetch the data from openprescribing.net   #
###############################################
#-- OpenPrescribing API details --#
# API details can be found here: https://openprescribing.net/api/
# Drug codes can be obtained here: https://openprescribing.net/analyse/

#-- Download pregabalin data (chemical designation only) --#
# Total items prescribed
# Code: 0408010AE
pregabalin <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AE&format=csv') 

#-- Download gabapentin data (chemical designation only) --#
# Total items prescribed under anti-epileptic drugs
# Code: 0408010G0
gabapentin_epi <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010G0&format=csv') 

# Total items prescribed under drugs for neuropathic pain
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
    select(date, items) %>% 
    # Filter date range: April 2017 to April 2021
    # Note: April 2021 was the last data point so only need to remove dates before April 2017.
    filter(date >= as.Date('2017-04-01'))

#-- Gabapentin --#
gabapentin_clean <- gabapentin %>% 
    # Select columns
    select(date, items.x, items.y) %>% 
    # Filter date range: April 2017 to April 2021
    # Note: April 2021 was the last data point so only need to remove dates before April 2017.
    filter(date >= as.Date('2017-04-01')) %>% 
    # Sum items columns from items.x (antiepileptic) and items.y (neuropathic pain)
    rowwise() %>% 
    mutate(items = sum(c_across(cols = starts_with('items')), na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Select columns
    select(date, items)

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
    select(date, items) %>% 
    # Write to file
    write_csv(file = 'data-clean/full-record_pregabalin.csv')

#-- Gabapentin --#
gabapentin %>% 
    # Select columns
    select(date, items.x, items.y) %>% 
    # Sum items columns from items.x (antiepileptic) and items.y (neuropathic pain)
    rowwise() %>% 
    mutate(items = sum(c_across(cols = starts_with('items')), na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Select columns
    select(date, items) %>% 
    # Write to file
    write_csv(file = 'data-clean/full-record_gabapentin.csv')
    
