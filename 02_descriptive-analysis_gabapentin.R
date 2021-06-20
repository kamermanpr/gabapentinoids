############################################################
#                                                          #
#                         Analysis                         #
#                                                          #
############################################################

#####################
#   Load packages   #
#####################
library(readr)
library(dplyr)
library(lubridate)
library(skimr)
library(ggplot2)

###############
#   Options   #
###############
# Scientific notation threshold
options(scipen = 9999)

# Create figure directory
if(!dir.exists('figures')) {
    dir.create('figures')
}

########################
#   Set ggplot theme   #
########################
theme_set(new = theme_minimal(base_size = 18) +
              theme(legend.position = 'none',
                    plot.title = element_text(size = 18),
                    plot.caption = element_text(size = 12),
                    panel.grid = element_blank(),
                    axis.text = element_text(colour = '#000000'),
                    axis.line = element_line(size = 0.5),
                    axis.ticks = element_line(size = 0.5)))

######################
#   Skimr settings   #
######################
my_skim <- skim_with(numeric = sfl(hist = NULL, iqr = IQR),
                     base = sfl(n_missing = 'n_missing', n_complete = 'n_complete'))

###################
#   Import data   #
###################
#-- Gabapentin --#
gabapentin <- read_csv('data-clean/full-record_gabapentin.csv')
    
#-- Pregabalin --#
pregabalin <- read_csv('data-clean/full-record_pregabalin.csv')

####################
#   Process data   #
####################
# Add a year column

#-- Gabapentin --#
gabapentin_2 <- gabapentin %>% 
    mutate(year = year(date)) 

#-- Pregabalin --#
pregabalin_2 <- pregabalin %>% 
    mutate(year = year(date)) 

##############################
#   Summarise data by year   #
##############################
#-- Gabapentin --#
# Tabulate
gabapentin_2 %>% 
    select(items, year) %>% 
    group_by(year) %>% 
    my_skim()

# Plot
plot_gabapentin <- ggplot(data = gabapentin_2) +
    aes(x = factor(year),
        y = items) +
    geom_boxplot(fill = '#000000',
                 alpha = 0.3, 
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(height = 0,
                                          width = 0.2)) +
    labs(title = 'Gabapentin',
         x = 'Year',
         y = 'Number of prescribed items') 

plot_gabapentin

ggsave(filename = 'figures/01_gabapentin_2016-to-2021.png',
       plot = plot_gabapentin,
       height = 6,
       width = 8)

#-- Pregabalin --#
# Tabulate
pregabalin_2 %>% 
    select(items, year) %>% 
    group_by(year) %>% 
    my_skim()

# Plot
plot_pregabalin <- ggplot(data = pregabalin_2) +
    aes(x = factor(year),
        y = items) +
    geom_boxplot(fill = '#000000',
                 alpha = 0.3, 
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(height = 0,
                                          width = 0.2)) +
    labs(title = 'Pregabalin',
         x = 'Year',
         y = 'Number of prescribed items') 

plot_pregabalin

ggsave(filename = 'figures/02_pregabalin_2016-to-2021.png',
       plot = plot_pregabalin,
       height = 6,
       width = 8)

########################
#   Publication plot   #
########################
#-- Combine data --#
gabapentin_3 <- gabapentin_2 %>% 
    mutate(drug = 'Gabapentin')

pregabalin_3 <- pregabalin_2 %>% 
    mutate(drug = 'Pregabalin')

combined <- bind_rows(gabapentin_3, pregabalin_3)

plot_combined <- ggplot(data = combined) +
    aes(x = factor(year),
        y = items) +
    geom_boxplot(fill = '#000000',
                 alpha = 0.3, 
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(height = 0,
                                          width = 0.2)) +
    labs(x = 'Year',
         y = 'Number of prescribed items') +
    theme(strip.text = element_text(hjust = 0,
                                    face = 'bold')) +
    facet_wrap(~drug, ncol = 1)

plot_combined

ggsave(filename = 'figures/03_pregabalin-and-gabapentin_2016-to-2021.png',
       plot = plot_combined,
       height = 11,
       width = 8)
