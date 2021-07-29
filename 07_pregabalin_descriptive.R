############################################################
#                                                          #
#                         Analysis                         #
#                                                          #
############################################################

#-- Load packages --#
library(readr)
library(dplyr)
library(lubridate)
library(skimr)
library(knitr)
library(ggplot2)
library(ggthemes)
library(patchwork)

#-- Options --#
# Scientific notation threshold
options(scipen = 9999)

# Create figure directory
if(!dir.exists('figures')) {
    dir.create('figures')
}

# Create figures directory for gabapentin
if(!dir.exists('figures/pregabalin')) {
    dir.create('figures/pregabalin')
}

#-- Set ggplot theme --#
theme_set(new = theme_minimal(base_size = 18) +
              theme(legend.position = 'none',
                    plot.title = element_text(size = 18),
                    plot.caption = element_text(size = 12),
                    panel.grid = element_blank(),
                    axis.text = element_text(colour = '#000000'),
                    axis.line = element_line(size = 0.5),
                    axis.ticks = element_line(size = 0.5)))

#-- Set skimr settings --#
my_skim <- skim_with(numeric = sfl(hist = NULL, iqr = IQR),
                     base = sfl(n_missing = 'n_missing', n_complete = 'n_complete'))

#-- Import data --#
pregabalin <- read_csv('data-clean/pregabalin_full-record.csv')

#-- Process data --#
pregabalin_year <- pregabalin %>% 
    mutate(year = year(date))

# 25mg dose
D25 <- pregabalin_year %>% 
    select(year, prescriptions_25, pills_25, average_daily_dose_25) %>% 
    rename(number_of_prescriptions = prescriptions_25,
           number_of_pills_prescribed = pills_25,
           average_daily_dose_mg = average_daily_dose_25)

# 50mg dose
D50 <- pregabalin_year %>% 
    select(year, prescriptions_50, pills_50, average_daily_dose_50) %>% 
    rename(number_of_prescriptions = prescriptions_50,
           number_of_pills_prescribed = pills_50,
           average_daily_dose_mg = average_daily_dose_50)

# 75mg dose
D75 <- pregabalin_year %>% 
    select(year, prescriptions_75, pills_75, average_daily_dose_75) %>% 
    rename(number_of_prescriptions = prescriptions_75,
           number_of_pills_prescribed = pills_75,
           average_daily_dose_mg = average_daily_dose_75)

# 100mg dose
D100 <- pregabalin_year %>% 
    select(year, prescriptions_100, pills_100, average_daily_dose_100) %>% 
    rename(number_of_prescriptions = prescriptions_100,
           number_of_pills_prescribed = pills_100,
           average_daily_dose_mg = average_daily_dose_100)

# 150mg dose
D150 <- pregabalin_year %>% 
    select(year, prescriptions_150, pills_150, average_daily_dose_150) %>% 
    rename(number_of_prescriptions = prescriptions_150,
           number_of_pills_prescribed = pills_150,
           average_daily_dose_mg = average_daily_dose_150)

# 200mg dose
D200 <- pregabalin_year %>% 
    select(year, prescriptions_200, pills_200, average_daily_dose_200) %>% 
    rename(number_of_prescriptions = prescriptions_200,
           number_of_pills_prescribed = pills_200,
           average_daily_dose_mg = average_daily_dose_200)

# 225mg dose
D225 <- pregabalin_year %>% 
    select(year, prescriptions_225, pills_225, average_daily_dose_225) %>% 
    rename(number_of_prescriptions = prescriptions_225,
           number_of_pills_prescribed = pills_225,
           average_daily_dose_mg = average_daily_dose_225)

# 300mg dose
D300 <- pregabalin_year %>% 
    select(year, prescriptions_300, pills_300, average_daily_dose_300) %>% 
    rename(number_of_prescriptions = prescriptions_300,
           number_of_pills_prescribed = pills_300,
           average_daily_dose_mg = average_daily_dose_300)

# Total
total <- pregabalin_year %>% 
    select(year, prescriptions_total, pills_total, weighted_average_daily_dose_total) %>% 
    rename(number_of_prescriptions = prescriptions_total,
           number_of_pills_prescribed = pills_total,
           average_daily_dose_mg = weighted_average_daily_dose_total)
    
#-- Summary stats by year --#
# 25mg dose
## Tabulated
D25 %>% 
    group_by(year) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    kable(digits = 0)

## Plotted
### Number of prescriptions
plot_D25_prescriptions <- ggplot(data = D25) +
    aes(x = factor(year),
        y = number_of_prescriptions,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of prescriptions',
         x = NULL,
         y = 'Prescriptions') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Number of pills prescribed
plot_D25_pills <- ggplot(data = D25) +
    aes(x = factor(year),
        y = number_of_pills_prescribed,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of pills prescribed',
         x = NULL,
         y = 'Pills') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Average daily dose
plot_D25_dose <- ggplot(data = D25) +
    aes(x = factor(year),
        y = average_daily_dose_mg,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Average daily dose',
         x = NULL,
         y = 'Dose (mg)') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) 

plot_D25_combined <- plot_D25_prescriptions + 
    plot_D25_pills + 
    plot_D25_dose +
    plot_layout(ncol = 1) +
    plot_annotation(title = 'Pregabalin: 25mg dose')

plot_D25_combined

ggsave(filename = 'figures/pregabalin/01_descriptive_25mg-dose.png',
       plot = plot_D25_combined,
       height = 12.5,
       width = 7.5)

# 50mg dose
## Tabulated
D50 %>% 
    group_by(year) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    kable(digits = 0)

## Plotted
### Number of prescriptions
plot_D50_prescriptions <- ggplot(data = D50) +
    aes(x = factor(year),
        y = number_of_prescriptions,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of prescriptions',
         x = NULL,
         y = 'Prescriptions') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Number of pills prescribed
plot_D50_pills <- ggplot(data = D50) +
    aes(x = factor(year),
        y = number_of_pills_prescribed,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of pills prescribed',
         x = NULL,
         y = 'Pills') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Average daily dose
plot_D50_dose <- ggplot(data = D50) +
    aes(x = factor(year),
        y = average_daily_dose_mg,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Average daily dose',
         x = NULL,
         y = 'Dose (mg)') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) 

plot_D50_combined <- plot_D50_prescriptions + 
    plot_D50_pills + 
    plot_D50_dose +
    plot_layout(ncol = 1) +
    plot_annotation(title = 'Pregabalin: 50mg dose')

plot_D50_combined

ggsave(filename = 'figures/pregabalin/02_descriptive_50mg-dose.png',
       plot = plot_D50_combined,
       height = 12.5,
       width = 7.5)

# 75mg dose
## Tabulated
D75 %>% 
    group_by(year) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    kable(digits = 0)

## Plotted
### Number of prescriptions
plot_D75_prescriptions <- ggplot(data = D75) +
    aes(x = factor(year),
        y = number_of_prescriptions,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of prescriptions',
         x = NULL,
         y = 'Prescriptions') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Number of pills prescribed
plot_D75_pills <- ggplot(data = D75) +
    aes(x = factor(year),
        y = number_of_pills_prescribed,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of pills prescribed',
         x = NULL,
         y = 'Pills') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Average daily dose
plot_D75_dose <- ggplot(data = D75) +
    aes(x = factor(year),
        y = average_daily_dose_mg,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Average daily dose',
         x = NULL,
         y = 'Dose (mg)') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) 

plot_D75_combined <- plot_D75_prescriptions + 
    plot_D75_pills + 
    plot_D75_dose +
    plot_layout(ncol = 1) +
    plot_annotation(title = 'Pregabalin: 75mg dose')

plot_D75_combined

ggsave(filename = 'figures/pregabalin/03_descriptive_75mg-dose.png',
       plot = plot_D75_combined,
       height = 12.5,
       width = 7.5)

# 100mg dose
## Tabulated
D100 %>% 
    group_by(year) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    kable(digits = 0)

## Plotted
### Number of prescriptions
plot_D100_prescriptions <- ggplot(data = D100) +
    aes(x = factor(year),
        y = number_of_prescriptions,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of prescriptions',
         x = NULL,
         y = 'Prescriptions') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Number of pills prescribed
plot_D100_pills <- ggplot(data = D100) +
    aes(x = factor(year),
        y = number_of_pills_prescribed,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of pills prescribed',
         x = NULL,
         y = 'Pills') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Average daily dose
plot_D100_dose <- ggplot(data = D100) +
    aes(x = factor(year),
        y = average_daily_dose_mg,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Average daily dose',
         x = NULL,
         y = 'Dose (mg)') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) 

plot_D100_combined <- plot_D100_prescriptions + 
    plot_D100_pills + 
    plot_D100_dose +
    plot_layout(ncol = 1) +
    plot_annotation(title = 'Pregabalin: 100mg dose')

plot_D100_combined

ggsave(filename = 'figures/pregabalin/04_descriptive_100mg-dose.png',
       plot = plot_D100_combined,
       height = 12.5,
       width = 7.5)

# 150mg dose
## Tabulated
D150 %>% 
    group_by(year) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    kable(digits = 0)

## Plotted
### Number of prescriptions
plot_D150_prescriptions <- ggplot(data = D150) +
    aes(x = factor(year),
        y = number_of_prescriptions,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of prescriptions',
         x = NULL,
         y = 'Prescriptions') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Number of pills prescribed
plot_D150_pills <- ggplot(data = D150) +
    aes(x = factor(year),
        y = number_of_pills_prescribed,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of pills prescribed',
         x = NULL,
         y = 'Pills') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Average daily dose
plot_D150_dose <- ggplot(data = D150) +
    aes(x = factor(year),
        y = average_daily_dose_mg,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Average daily dose',
         x = NULL,
         y = 'Dose (mg)') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) 

plot_D150_combined <- plot_D150_prescriptions + 
    plot_D150_pills + 
    plot_D150_dose +
    plot_layout(ncol = 1) +
    plot_annotation(title = 'Pregabalin: 150mg dose')

plot_D150_combined

ggsave(filename = 'figures/pregabalin/05_descriptive_150mg-dose.png',
       plot = plot_D150_combined,
       height = 12.5,
       width = 7.5)

# 200mg dose
## Tabulated
D200 %>% 
    group_by(year) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    kable(digits = 0)

## Plotted
### Number of prescriptions
plot_D200_prescriptions <- ggplot(data = D200) +
    aes(x = factor(year),
        y = number_of_prescriptions,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of prescriptions',
         x = NULL,
         y = 'Prescriptions') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Number of pills prescribed
plot_D200_pills <- ggplot(data = D200) +
    aes(x = factor(year),
        y = number_of_pills_prescribed,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of pills prescribed',
         x = NULL,
         y = 'Pills') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Average daily dose
plot_D200_dose <- ggplot(data = D200) +
    aes(x = factor(year),
        y = average_daily_dose_mg,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Average daily dose',
         x = NULL,
         y = 'Dose (mg)') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) 

plot_D200_combined <- plot_D200_prescriptions + 
    plot_D200_pills + 
    plot_D200_dose +
    plot_layout(ncol = 1) +
    plot_annotation(title = 'Pregabalin: 200mg dose')

plot_D200_combined

ggsave(filename = 'figures/pregabalin/06_descriptive_200mg-dose.png',
       plot = plot_D200_combined,
       height = 12.5,
       width = 7.5)

# 225mg dose
## Tabulated
D225 %>% 
    group_by(year) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    kable(digits = 0)

## Plotted
### Number of prescriptions
plot_D225_prescriptions <- ggplot(data = D225) +
    aes(x = factor(year),
        y = number_of_prescriptions,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of prescriptions',
         x = NULL,
         y = 'Prescriptions') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Number of pills prescribed
plot_D225_pills <- ggplot(data = D225) +
    aes(x = factor(year),
        y = number_of_pills_prescribed,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of pills prescribed',
         x = NULL,
         y = 'Pills') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Average daily dose
plot_D225_dose <- ggplot(data = D225) +
    aes(x = factor(year),
        y = average_daily_dose_mg,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Average daily dose',
         x = NULL,
         y = 'Dose (mg)') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) 

plot_D225_combined <- plot_D225_prescriptions + 
    plot_D225_pills + 
    plot_D225_dose +
    plot_layout(ncol = 1) +
    plot_annotation(title = 'Pregabalin: 225mg dose')

plot_D225_combined

ggsave(filename = 'figures/pregabalin/07_descriptive_225mg-dose.png',
       plot = plot_D225_combined,
       height = 12.5,
       width = 7.5)

# 300mg dose
## Tabulated
D300 %>% 
    group_by(year) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    kable(digits = 0)

## Plotted
### Number of prescriptions
plot_D300_prescriptions <- ggplot(data = D300) +
    aes(x = factor(year),
        y = number_of_prescriptions,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of prescriptions',
         x = NULL,
         y = 'Prescriptions') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Number of pills prescribed
plot_D300_pills <- ggplot(data = D300) +
    aes(x = factor(year),
        y = number_of_pills_prescribed,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of pills prescribed',
         x = NULL,
         y = 'Pills') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Average daily dose
plot_D300_dose <- ggplot(data = D300) +
    aes(x = factor(year),
        y = average_daily_dose_mg,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Average daily dose',
         x = NULL,
         y = 'Dose (mg)') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) 

plot_D300_combined <- plot_D300_prescriptions + 
    plot_D300_pills + 
    plot_D300_dose +
    plot_layout(ncol = 1) +
    plot_annotation(title = 'Pregabalin: 300mg dose')

plot_D300_combined

ggsave(filename = 'figures/pregabalin/08_descriptive_300mg-dose.png',
       plot = plot_D300_combined,
       height = 12.5,
       width = 7.5)

# totalmg dose
## Tabulated
total %>% 
    group_by(year) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    kable(digits = 0)

## Plotted
### Number of prescriptions
plot_total_prescriptions <- ggplot(data = total) +
    aes(x = factor(year),
        y = number_of_prescriptions,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of prescriptions',
         x = NULL,
         y = 'Prescriptions') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Number of pills prescribed
plot_total_pills <- ggplot(data = total) +
    aes(x = factor(year),
        y = number_of_pills_prescribed,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Number of pills prescribed',
         x = NULL,
         y = 'Pills') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    theme(axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

### Average daily dose
plot_total_dose <- ggplot(data = total) +
    aes(x = factor(year),
        y = average_daily_dose_mg,
        fill = factor(year)) +
    geom_boxplot(outlier.size = -Inf) +
    geom_point(position = position_jitter(height = 0, 
                                          width = 0.25),
               shape = 21,
               size = 2) +
    labs(title = 'Average daily dose',
         x = NULL,
         y = 'Dose (mg)') +
    scale_fill_tableau() +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) 

plot_total_combined <- plot_total_prescriptions + 
    plot_total_pills + 
    plot_total_dose +
    plot_layout(ncol = 1) +
    plot_annotation(title = 'Pregabalin: total dose')

plot_total_combined

ggsave(filename = 'figures/pregabalin/09_descriptive_total-dose.png',
       plot = plot_total_combined,
       height = 12.5,
       width = 7.5)
