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
if(!dir.exists('figures/gabapentin')) {
    dir.create('figures/gabapentin')
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
gabapentin <- read_csv('data-clean/gabapentin_full-record.csv')

#-- Process data --#
gabapentin_year <- gabapentin %>% 
    mutate(year = year(date))

# 100mg dose
D100 <- gabapentin_year %>% 
    select(year, prescriptions_100, pills_100, average_daily_dose_100) %>% 
    rename(number_of_prescriptions = prescriptions_100,
           number_of_pills_prescribed = pills_100,
           average_daily_dose_mg = average_daily_dose_100)

# 300mg dose
D300 <- gabapentin_year %>% 
    select(year, prescriptions_300, pills_300, average_daily_dose_300) %>% 
    rename(number_of_prescriptions = prescriptions_300,
           number_of_pills_prescribed = pills_300,
           average_daily_dose_mg = average_daily_dose_300)

# 400mg dose
D400 <- gabapentin_year %>% 
    select(year, prescriptions_400, pills_400, average_daily_dose_400) %>% 
    rename(number_of_prescriptions = prescriptions_400,
           number_of_pills_prescribed = pills_400,
           average_daily_dose_mg = average_daily_dose_400)

# 600mg dose
D600 <- gabapentin_year %>% 
    select(year, prescriptions_600, pills_600, average_daily_dose_600) %>% 
    rename(number_of_prescriptions = prescriptions_600,
           number_of_pills_prescribed = pills_600,
           average_daily_dose_mg = average_daily_dose_600)

# 800mg dose
D800 <- gabapentin_year %>% 
    select(year, prescriptions_800, pills_800, average_daily_dose_800) %>% 
    rename(number_of_prescriptions = prescriptions_800,
           number_of_pills_prescribed = pills_800,
           average_daily_dose_mg = average_daily_dose_800)

# Total
total <- gabapentin_year %>% 
    select(year, prescriptions_total, pills_total, weighted_average_daily_dose_total) %>% 
    rename(number_of_prescriptions = prescriptions_total,
           number_of_pills_prescribed = pills_total,
           average_daily_dose_mg = weighted_average_daily_dose_total)
    
#-- Summary stats by year --#
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
    plot_annotation(title = 'Gabapentin: 100mg dose')

plot_D100_combined

ggsave(filename = 'figures/gabapentin/01_descriptive_100mg-dose.png',
       plot = plot_D100_combined,
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
    plot_annotation(title = 'Gabapentin: 300mg dose')

plot_D300_combined

ggsave(filename = 'figures/gabapentin/02_descriptive_300mg-dose.png',
       plot = plot_D300_combined,
       height = 12.5,
       width = 7.5)

# 400mg dose
## Tabulated
D400 %>% 
    group_by(year) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    kable(digits = 0)

## Plotted
### Number of prescriptions
plot_D400_prescriptions <- ggplot(data = D400) +
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
plot_D400_pills <- ggplot(data = D400) +
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
plot_D400_dose <- ggplot(data = D400) +
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

plot_D400_combined <- plot_D400_prescriptions + 
    plot_D400_pills + 
    plot_D400_dose +
    plot_layout(ncol = 1) +
    plot_annotation(title = 'Gabapentin: 400mg dose')

plot_D400_combined

ggsave(filename = 'figures/gabapentin/03_descriptive_400mg-dose.png',
       plot = plot_D400_combined,
       height = 12.5,
       width = 7.5)

# 600mg dose
## Tabulated
D600 %>% 
    group_by(year) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    kable(digits = 0)

## Plotted
### Number of prescriptions
plot_D600_prescriptions <- ggplot(data = D600) +
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
plot_D600_pills <- ggplot(data = D600) +
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
plot_D600_dose <- ggplot(data = D600) +
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

plot_D600_combined <- plot_D600_prescriptions + 
    plot_D600_pills + 
    plot_D600_dose +
    plot_layout(ncol = 1) +
    plot_annotation(title = 'Gabapentin: 600mg dose')

plot_D600_combined

ggsave(filename = 'figures/gabapentin/04_descriptive_600mg-dose.png',
       plot = plot_D600_combined,
       height = 12.5,
       width = 7.5)

# 800mg dose
## Tabulated
D800 %>% 
    group_by(year) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    kable(digits = 0)

## Plotted
### Number of prescriptions
plot_D800_prescriptions <- ggplot(data = D800) +
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
plot_D800_pills <- ggplot(data = D800) +
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
plot_D800_dose <- ggplot(data = D800) +
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

plot_D800_combined <- plot_D800_prescriptions + 
    plot_D800_pills + 
    plot_D800_dose +
    plot_layout(ncol = 1) +
    plot_annotation(title = 'Gabapentin: 800mg dose')

plot_D800_combined

ggsave(filename = 'figures/gabapentin/05_descriptive_800mg-dose.png',
       plot = plot_D800_combined,
       height = 12.5,
       width = 7.5)

# Total
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
    plot_annotation(title = 'Gabapentin: total')

plot_total_combined

ggsave(filename = 'figures/gabapentin/06_descriptive_total.png',
       plot = plot_total_combined,
       height = 12.5,
       width = 7.5)
