############################################################
#                                                          #
#                    Gabapentin: import                    #
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