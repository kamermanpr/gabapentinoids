############################################################
#                                                          #
#                  Descriptive summarises                  #
#                                                          #
############################################################

#-- Load packages --#
library(readr)
library(dplyr)
library(skimr)
library(kableExtra)
library(ggplot2)
library(ggthemes)

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
theme_set(new = theme_minimal(base_size = 16) +
              theme(legend.position = 'none',
                    plot.background = element_rect(colour = '#FFFFFF',
                                                   fill = '#FFFFFF'),
                    plot.title = element_text(size = 16),
                    plot.title.position = 'plot',
                    plot.caption = element_text(size = 12),
                    panel.grid = element_blank(),
                    axis.text = element_text(colour = '#000000'),
                    axis.line = element_line(size = 0.5),
                    axis.ticks = element_line(size = 0.5)))

#-- Set skimr settings --#
my_skim <- skim_with(numeric = sfl(hist = NULL),
                     base = sfl(n_missing = 'n_missing', 
                                n_complete = 'n_complete'))

#-- Import data --#
gabapentin <- read_csv('data-clean/gabapentin_analysis-set.csv')

#-- Process data --#
# Define pre vs post 2019-04-01 time periods
gabapentin_period <- gabapentin %>% 
    mutate(period = ifelse(date < as.Date('2019-04-01'),
                           yes = 'Before rescheduling',
                           no = 'After rescheduling')) %>% 
    mutate(period = factor(period,
                           levels = c('Before rescheduling',
                                      'After rescheduling'),
                           ordered = TRUE))

# 100mg dose
gabapentin_100 <- gabapentin_period %>% 
    select(date, 
           period, 
           prescriptions_100, 
           pills_100,
           total_prescribed_dose_100,
           total_dose_per_prescription_100,
           average_daily_dose_100)

# 300mg dose
gabapentin_300 <- gabapentin_period %>% 
    select(date, 
           period, 
           prescriptions_300, 
           pills_300,
           total_prescribed_dose_300,
           total_dose_per_prescription_300,
           average_daily_dose_300)

# 400mg dose
gabapentin_400 <- gabapentin_period %>% 
    select(date, 
           period, 
           prescriptions_400, 
           pills_400,
           total_prescribed_dose_400,
           total_dose_per_prescription_400,
           average_daily_dose_400)

# 600mg dose
gabapentin_600 <- gabapentin_period %>% 
    select(date, 
           period, 
           prescriptions_600, 
           pills_600,
           total_prescribed_dose_600,
           total_dose_per_prescription_600,
           average_daily_dose_600)

# 800mg dose
gabapentin_800 <- gabapentin_period %>% 
    select(date, 
           period, 
           prescriptions_800, 
           pills_800,
           total_prescribed_dose_800,
           total_dose_per_prescription_800,
           average_daily_dose_800)

# Total dose
gabapentin_total <- gabapentin_period %>% 
    select(date, 
           period, 
           prescriptions_total, 
           pills_total,
           dose_total,
           dose_per_prescription_total,
           average_daily_dose_total)

#-- Summaries (all outputs saved to file) --#
# 100mg dose
## Number of prescription items by period
gabapentin_100 %>% 
    select(period,
           prescriptions_100) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 100mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/100_table_number-of-prescription-items.pdf')

p100_01 <- gabapentin_100 %>% 
    select(period,
           prescriptions_100) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = prescriptions_100,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 100mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/100_figure_number-of-prescription-items.png',
       plot = p100_01,
       height = 6,
       width = 7)
    
## Number of pills dispensed per month
gabapentin_100 %>% 
    select(period,
           pills_100) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 100mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/100_table_number-of-pills-dispensed.pdf')

p100_02 <- gabapentin_100 %>% 
    select(period,
           pills_100) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = pills_100,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 100mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/100_figure_number-of-pills-dispensed.png',
       plot = p100_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
gabapentin_100 %>% 
    select(period,
           total_prescribed_dose_100) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 100mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/100_table_total-quantity-dispensed.pdf')

p100_03 <- gabapentin_100 %>% 
    select(period,
           total_prescribed_dose_100) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_prescribed_dose_100,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 100mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/100_figure_total-quantity-dispensed.png',
       plot = p100_03,
       height = 6,
       width = 7)

## Monthly dose per prescription
gabapentin_100 %>% 
    select(period,
           total_dose_per_prescription_100) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 100mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/100_table_dose-per-prescription.pdf')

p100_04 <- gabapentin_100 %>% 
    select(period,
           total_dose_per_prescription_100) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_dose_per_prescription_100,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 100mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/100_figure_dose-per-prescription.png',
       plot = p100_04,
       height = 6,
       width = 7)

## Average daily dose
gabapentin_100 %>% 
    select(period,
           average_daily_dose_100) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 100mg: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/100_table_average-daily-dose.pdf')

p100_05 <- gabapentin_100 %>% 
    select(period,
           average_daily_dose_100) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = average_daily_dose_100,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 100mg: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/100_figure_average-daily-dose.png',
       plot = p100_05,
       height = 6,
       width = 7)

# 300mg dose
## Number of prescription items by period
gabapentin_300 %>% 
    select(period,
           prescriptions_300) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 300mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/300_table_number-of-prescription-items.pdf')

p300_01 <- gabapentin_300 %>% 
    select(period,
           prescriptions_300) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = prescriptions_300,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 300mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/300_figure_number-of-prescription-items.png',
       plot = p300_01,
       height = 6,
       width = 7)

## Number of pills dispensed per month
gabapentin_300 %>% 
    select(period,
           pills_300) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 300mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/300_table_number-of-pills-dispensed.pdf')

p300_02 <- gabapentin_300 %>% 
    select(period,
           pills_300) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = pills_300,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 300mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/300_figure_number-of-pills-dispensed.png',
       plot = p300_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
gabapentin_300 %>% 
    select(period,
           total_prescribed_dose_300) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 300mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/300_table_total-quantity-dispensed.pdf')

p300_03 <- gabapentin_300 %>% 
    select(period,
           total_prescribed_dose_300) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_prescribed_dose_300,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 300mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/300_figure_total-quantity-dispensed.png',
       plot = p300_03,
       height = 6,
       width = 7)

## Average monthly dose per prescription
gabapentin_300 %>% 
    select(period,
           total_dose_per_prescription_300) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 300mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/300_table_dose-per-prescription.pdf')

p300_04 <- gabapentin_300 %>% 
    select(period,
           total_dose_per_prescription_300) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_dose_per_prescription_300,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 300mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/300_figure_dose-per-prescription.png',
       plot = p300_04,
       height = 6,
       width = 7)

## Average daily dose
gabapentin_300 %>% 
    select(period,
           average_daily_dose_300) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 300mg: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/300_table_average-daily-dose.pdf')

p300_05 <- gabapentin_300 %>% 
    select(period,
           average_daily_dose_300) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = average_daily_dose_300,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 300mg: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/300_figure_average-daily-dose.png',
       plot = p300_05,
       height = 6,
       width = 7)

# 400mg dose
## Number of prescription items by period
gabapentin_400 %>% 
    select(period,
           prescriptions_400) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 400mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/400_table_number-of-prescription-items.pdf')

p400_01 <- gabapentin_400 %>% 
    select(period,
           prescriptions_400) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = prescriptions_400,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 400mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/400_figure_number-of-prescription-items.png',
       plot = p400_01,
       height = 6,
       width = 7)

## Number of pills dispensed per month
gabapentin_400 %>% 
    select(period,
           pills_400) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 400mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/400_table_number-of-pills-dispensed.pdf')

p400_02 <- gabapentin_400 %>% 
    select(period,
           pills_400) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = pills_400,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 400mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/400_figure_number-of-pills-dispensed.png',
       plot = p400_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
gabapentin_400 %>% 
    select(period,
           total_prescribed_dose_400) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 400mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/400_table_total-quantity-dispensed.pdf')

p400_03 <- gabapentin_400 %>% 
    select(period,
           total_prescribed_dose_400) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_prescribed_dose_400,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 400mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/400_figure_total-quantity-dispensed.png',
       plot = p400_03,
       height = 6,
       width = 7)

## Average monthly dose per prescription
gabapentin_400 %>% 
    select(period,
           total_dose_per_prescription_400) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 400mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/400_table_dose-per-prescription.pdf')

p400_04 <- gabapentin_400 %>% 
    select(period,
           total_dose_per_prescription_400) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_dose_per_prescription_400,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 400mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/400_figure_dose-per-prescription.png',
       plot = p400_04,
       height = 6,
       width = 7)

## Average daily dose
gabapentin_400 %>% 
    select(period,
           average_daily_dose_400) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 400mg: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/400_table_average-daily-dose.pdf')

p400_05 <- gabapentin_400 %>% 
    select(period,
           average_daily_dose_400) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = average_daily_dose_400,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 400mg: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/400_figure_average-daily-dose.png',
       plot = p400_05,
       height = 6,
       width = 7)

# 600mg dose
## Number of prescription items by period
gabapentin_600 %>% 
    select(period,
           prescriptions_600) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 600mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/600_table_number-of-prescription-items.pdf')

p600_01 <- gabapentin_600 %>% 
    select(period,
           prescriptions_600) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = prescriptions_600,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 600mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/600_figure_number-of-prescription-items.png',
       plot = p600_01,
       height = 6,
       width = 7)

## Number of pills dispensed per month
gabapentin_600 %>% 
    select(period,
           pills_600) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 600mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/600_table_number-of-pills-dispensed.pdf')

p600_02 <- gabapentin_600 %>% 
    select(period,
           pills_600) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = pills_600,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 600mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/600_figure_number-of-pills-dispensed.png',
       plot = p600_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
gabapentin_600 %>% 
    select(period,
           total_prescribed_dose_600) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 600mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/600_table_total-quantity-dispensed.pdf')

p600_03 <- gabapentin_600 %>% 
    select(period,
           total_prescribed_dose_600) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_prescribed_dose_600,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 600mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/600_figure_total-quantity-dispensed.png',
       plot = p600_03,
       height = 6,
       width = 7)

## Average monthly dose per prescription
gabapentin_600 %>% 
    select(period,
           total_dose_per_prescription_600) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 600mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/600_table_dose-per-prescription.pdf')

p600_04 <- gabapentin_600 %>% 
    select(period,
           total_dose_per_prescription_600) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_dose_per_prescription_600,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 600mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/600_figure_dose-per-prescription.png',
       plot = p600_04,
       height = 6,
       width = 7)

## Average daily dose
gabapentin_600 %>% 
    select(period,
           average_daily_dose_600) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 600mg: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/600_table_average-daily-dose.pdf')

p600_05 <- gabapentin_600 %>% 
    select(period,
           average_daily_dose_600) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = average_daily_dose_600,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 600mg: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/600_figure_average-daily-dose.png',
       plot = p600_05,
       height = 6,
       width = 7)

# 800mg dose
## Number of prescription items by period
gabapentin_800 %>% 
    select(period,
           prescriptions_800) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 800mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/800_table_number-of-prescription-items.pdf')

p800_01 <- gabapentin_800 %>% 
    select(period,
           prescriptions_800) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = prescriptions_800,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 800mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/800_figure_number-of-prescription-items.png',
       plot = p800_01,
       height = 6,
       width = 7)

## Number of pills dispensed per month
gabapentin_800 %>% 
    select(period,
           pills_800) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 800mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/800_table_number-of-pills-dispensed.pdf')

p800_02 <- gabapentin_800 %>% 
    select(period,
           pills_800) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = pills_800,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 800mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/800_figure_number-of-pills-dispensed.png',
       plot = p800_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
gabapentin_800 %>% 
    select(period,
           total_prescribed_dose_800) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 800mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/800_table_total-quantity-dispensed.pdf')

p800_03 <- gabapentin_800 %>% 
    select(period,
           total_prescribed_dose_800) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_prescribed_dose_800,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 800mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/800_figure_total-quantity-dispensed.png',
       plot = p800_03,
       height = 6,
       width = 7)

## Average monthly dose per prescription
gabapentin_800 %>% 
    select(period,
           total_dose_per_prescription_800) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 800mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/800_table_dose-per-prescription.pdf')

p800_04 <- gabapentin_800 %>% 
    select(period,
           total_dose_per_prescription_800) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_dose_per_prescription_800,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 800mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/800_figure_dose-per-prescription.png',
       plot = p800_04,
       height = 6,
       width = 7)

## Average daily dose
gabapentin_800 %>% 
    select(period,
           average_daily_dose_800) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin 800mg: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/800_table_average-daily-dose.pdf')

p800_05 <- gabapentin_800 %>% 
    select(period,
           average_daily_dose_800) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = average_daily_dose_800,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin 800mg: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/800_figure_average-daily-dose.png',
       plot = p800_05,
       height = 6,
       width = 7)

# Total
## Number of prescription items by period
gabapentin_total %>% 
    select(period,
           prescriptions_total) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin total: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/total_table_number-of-prescription-items.pdf')

p_total_01 <- gabapentin_total %>% 
    select(period,
           prescriptions_total) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = prescriptions_total,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin total: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/total_figure_number-of-prescription-items.png',
       plot = p_total_01,
       height = 6,
       width = 7)

## Number of pills dispensed per month
gabapentin_total %>% 
    select(period,
           pills_total) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin total: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/total_table_number-of-pills-dispensed.pdf')

p_total_02 <- gabapentin_total %>% 
    select(period,
           pills_total) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = pills_total,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin total: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/total_figure_number-of-pills-dispensed.png',
       plot = p_total_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
gabapentin_total %>% 
    select(period,
           dose_total) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin total: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/total_table_total-quantity-dispensed.pdf')

p_total_03 <- gabapentin_total %>% 
    select(period,
           dose_total) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = dose_total,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin total: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/total_figure_total-quantity-dispensed.png',
       plot = p_total_03,
       height = 6,
       width = 7)

## Average monthly dose per prescription
gabapentin_total %>% 
    select(period,
           dose_per_prescription_total) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin total: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/total_table_dose-per-prescription.pdf')

p_total_04 <- gabapentin_total %>% 
    select(period,
           dose_per_prescription_total) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = dose_per_prescription_total,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin total: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/total_figure_dose-per-prescription.png',
       plot = p_total_04,
       height = 6,
       width = 7)

## Average daily dose
gabapentin_total %>% 
    select(period,
           average_daily_dose_total) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Gabapentin total: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/gabapentin/total_table_average-daily-dose.pdf')

p_total_05 <- gabapentin_total %>% 
    select(period,
           average_daily_dose_total) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = average_daily_dose_total,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.25, 
                                          height = 0)) +
    labs(title = 'Gabapentin total: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/gabapentin/total_figure_average-daily-dose.png',
       plot = p_total_05,
       height = 6,
       width = 7)
