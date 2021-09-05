############################################################
#                                                          #
#         Pregabalin: import and preliminary clean         #
#                                                          #
############################################################

#-- Date of download --#
# 2021-06-20 (original download)
# 2021-09-03 (added Alzain, Axalid, Lecaent, Rewisca to downlaoded files)

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

alzain_25_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBEAAAA&format=csv')
alzain_50_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBEABAB&format=csv')
alzain_75_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBEACAC&format=csv')
alzain_100_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBEADAD&format=csv')
alzain_150_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBEAEAE&format=csv')
alzain_200_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBEAFAF&format=csv')
alzain_225_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBEAGAI&format=csv')
alzain_300_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBEAHAG&format=csv')

axalid_25_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBFAAAA&format=csv')
axalid_50_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBFABAB&format=csv')
axalid_75_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBFACAC&format=csv')
axalid_100_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBFADAD&format=csv')
axalid_150_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBFAEAE&format=csv')
axalid_200_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBFAFAF&format=csv')
axalid_225_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBFAGAI&format=csv')
axalid_300_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBFAHAG&format=csv')

lecaent_25_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBDAAAA&format=csv')
lecaent_50_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBDABAB&format=csv')
lecaent_75_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBDACAC&format=csv')
lecaent_100_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBDADAD&format=csv')
lecaent_150_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBDAEAE&format=csv')
lecaent_200_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBDAFAF&format=csv')
lecaent_225_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBDAGAI&format=csv')
lecaent_300_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBDAHAG&format=csv')

rewisca_25_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBCABAA&format=csv')
rewisca_50_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBCACAB&format=csv')
rewisca_75_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBCADAC&format=csv')
rewisca_100_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBCAAAD&format=csv')
rewisca_150_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBCAEAE&format=csv')
rewisca_200_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBCAFAF&format=csv')
rewisca_225_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBCAGAI&format=csv')
rewisca_300_cap <- read_csv('https://openprescribing.net/api/1.0/spending/?code=0408010AEBCAHAG&format=csv')

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

write_csv(x = alzain_25_cap,
          file = 'data-original/alzain_25_cap.csv')

write_csv(x = axalid_25_cap,
          file = 'data-original/axalid_25_cap.csv')

write_csv(x = lecaent_25_cap,
          file = 'data-original/lecaent_25_cap.csv')

write_csv(x = rewisca_25_cap,
          file = 'data-original/rewisca_25_cap.csv')

# 50mg data
write_csv(x = pregabalin_50_cap,
          file = 'data-original/pregabalin_50_cap.csv')

write_csv(x = pregabalin_50_tab,
          file = 'data-original/pregabalin_50_tab.csv')

write_csv(x = lyrica_50_cap,
          file = 'data-original/lyrica_50_cap.csv')

write_csv(x = alzain_50_cap,
          file = 'data-original/alzain_50_cap.csv')

write_csv(x = axalid_50_cap,
          file = 'data-original/axalid_50_cap.csv')

write_csv(x = lecaent_50_cap,
          file = 'data-original/lecaent_50_cap.csv')

write_csv(x = rewisca_50_cap,
          file = 'data-original/rewisca_50_cap.csv')

# 75mg data
write_csv(x = pregabalin_75_cap,
          file = 'data-original/pregabalin_75_cap.csv')

write_csv(x = pregabalin_75_tab,
          file = 'data-original/pregabalin_75_tab.csv')

write_csv(x = lyrica_75_cap,
          file = 'data-original/lyrica_75_cap.csv')

write_csv(x = alzain_75_cap,
          file = 'data-original/alzain_75_cap.csv')

write_csv(x = axalid_75_cap,
          file = 'data-original/axalid_75_cap.csv')

write_csv(x = lecaent_75_cap,
          file = 'data-original/lecaent_75_cap.csv')

write_csv(x = rewisca_75_cap,
          file = 'data-original/rewisca_75_cap.csv')

# 100mg
write_csv(x = pregabalin_100_cap,
          file = 'data-original/pregabalin_100_cap.csv')

write_csv(x = pregabalin_100_tab,
          file = 'data-original/pregabalin_100_tab.csv')

write_csv(x = lyrica_100_cap,
          file = 'data-original/lyrica_100_cap.csv')

write_csv(x = alzain_100_cap,
          file = 'data-original/alzain_100_cap.csv')

write_csv(x = axalid_100_cap,
          file = 'data-original/axalid_100_cap.csv')

write_csv(x = lecaent_100_cap,
          file = 'data-original/lecaent_100_cap.csv')

write_csv(x = rewisca_100_cap,
          file = 'data-original/rewisca_100_cap.csv')

# 150mg
write_csv(x = pregabalin_150_cap,
          file = 'data-original/pregabalin_150_cap.csv')

write_csv(x = pregabalin_150_tab,
          file = 'data-original/pregabalin_150_tab.csv')

write_csv(x = lyrica_150_cap,
          file = 'data-original/lyrica_150_cap.csv')

write_csv(x = alzain_150_cap,
          file = 'data-original/alzain_150_cap.csv')

write_csv(x = axalid_150_cap,
          file = 'data-original/axalid_150_cap.csv')

write_csv(x = lecaent_150_cap,
          file = 'data-original/lecaent_150_cap.csv')

write_csv(x = rewisca_150_cap,
          file = 'data-original/rewisca_150_cap.csv')

# 200mg
write_csv(x = pregabalin_200_cap,
          file = 'data-original/pregabalin_200_cap.csv')

write_csv(x = pregabalin_200_tab,
          file = 'data-original/pregabalin_200_tab.csv')

write_csv(x = lyrica_200_cap,
          file = 'data-original/lyrica_200_cap.csv')

write_csv(x = alzain_200_cap,
          file = 'data-original/alzain_200_cap.csv')

write_csv(x = axalid_200_cap,
          file = 'data-original/axalid_200_cap.csv')

write_csv(x = lecaent_200_cap,
          file = 'data-original/lecaent_200_cap.csv')

write_csv(x = rewisca_200_cap,
          file = 'data-original/rewisca_200_cap.csv')

# 225mg
write_csv(x = pregabalin_225_cap,
          file = 'data-original/pregabalin_225_cap.csv')

write_csv(x = pregabalin_225_tab,
          file = 'data-original/pregabalin_225_tab.csv')

write_csv(x = lyrica_225_cap,
          file = 'data-original/lyrica_225_cap.csv')

write_csv(x = alzain_225_cap,
          file = 'data-original/alzain_225_cap.csv')

write_csv(x = axalid_225_cap,
          file = 'data-original/axalid_225_cap.csv')

write_csv(x = lecaent_225_cap,
          file = 'data-original/lecaent_225_cap.csv')

write_csv(x = rewisca_225_cap,
          file = 'data-original/rewisca_225_cap.csv')

# 300mg
write_csv(x = pregabalin_300_cap,
          file = 'data-original/pregabalin_300_cap.csv')

write_csv(x = pregabalin_300_tab,
          file = 'data-original/pregabalin_300_tab.csv')

write_csv(x = lyrica_300_cap,
          file = 'data-original/lyrica_300_cap.csv')

write_csv(x = alzain_300_cap,
          file = 'data-original/alzain_300_cap.csv')

write_csv(x = axalid_300_cap,
          file = 'data-original/axalid_300_cap.csv')

write_csv(x = lecaent_300_cap,
          file = 'data-original/lecaent_300_cap.csv')

write_csv(x = rewisca_300_cap,
          file = 'data-original/rewisca_300_cap.csv')
