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

# Create figures directory for pregabalin
if(!dir.exists('figures/pregabalin')) {
    dir.create('figures/pregabalin')
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
pregabalin <- read_csv('data-clean/pregabalin_analysis-set.csv')

#-- Process data --#
# Define pre vs post 2019-04-01 time periods
pregabalin_period <- pregabalin %>% 
    mutate(period = ifelse(date < as.Date('2019-04-01'),
                           yes = 'Before rescheduling',
                           no = 'After rescheduling')) %>% 
    mutate(period = factor(period,
                           levels = c('Before rescheduling',
                                      'After rescheduling'),
                           ordered = TRUE))

# 200mg dose
pregabalin_200 <- pregabalin_period %>% 
    select(date, 
           period, 
           prescriptions_200, 
           pills_200,
           total_prescribed_dose_200,
           total_dose_per_prescription_200,
           average_daily_dose_200)

# 50mg
pregabalin_50 <- pregabalin_period %>% 
    select(date, 
           period, 
           prescriptions_50, 
           pills_50,
           total_prescribed_dose_50,
           total_dose_per_prescription_50,
           average_daily_dose_50)

# 75mg
pregabalin_75 <- pregabalin_period %>% 
    select(date, 
           period, 
           prescriptions_75, 
           pills_75,
           total_prescribed_dose_75,
           total_dose_per_prescription_75,
           average_daily_dose_75)

# 100mg
pregabalin_100 <- pregabalin_period %>% 
    select(date, 
           period, 
           prescriptions_100, 
           pills_100,
           total_prescribed_dose_100,
           total_dose_per_prescription_100,
           average_daily_dose_100)

# 150mg
pregabalin_150 <- pregabalin_period %>% 
    select(date, 
           period, 
           prescriptions_150, 
           pills_150,
           total_prescribed_dose_150,
           total_dose_per_prescription_150,
           average_daily_dose_150)

# 200mg
pregabalin_200 <- pregabalin_period %>% 
    select(date, 
           period, 
           prescriptions_200, 
           pills_200,
           total_prescribed_dose_200,
           total_dose_per_prescription_200,
           average_daily_dose_200)

# 2200mg
pregabalin_2200 <- pregabalin_period %>% 
    select(date, 
           period, 
           prescriptions_2200, 
           pills_2200,
           total_prescribed_dose_2200,
           total_dose_per_prescription_2200,
           average_daily_dose_2200)

# 300mg
pregabalin_300 <- pregabalin_period %>% 
    select(date, 
           period, 
           prescriptions_300, 
           pills_300,
           total_prescribed_dose_300,
           total_dose_per_prescription_300,
           average_daily_dose_300)

# Total dose
pregabalin_total <- pregabalin_period %>% 
    select(date, 
           period, 
           prescriptions_total, 
           pills_total,
           dose_total,
           dose_per_prescription_total,
           average_daily_dose_total)

#-- Summaries (all outputs saved to file) --#
# 200mg dose
## Number of prescription items by period
pregabalin_200 %>% 
    select(period,
           prescriptions_200) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 200mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/200_table_number-of-prescription-items.pdf')

p200_01 <- pregabalin_200 %>% 
    select(period,
           prescriptions_200) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = prescriptions_200,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.200, 
                                          height = 0)) +
    labs(title = 'Pregabalin 200mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/200_figure_number-of-prescription-items.png',
       plot = p200_01,
       height = 6,
       width = 7)
    
## Number of pills dispensed per month
pregabalin_200 %>% 
    select(period,
           pills_200) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 200mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/200_table_number-of-pills-dispensed.pdf')

p200_02 <- pregabalin_200 %>% 
    select(period,
           pills_200) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = pills_200,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.200, 
                                          height = 0)) +
    labs(title = 'Pregabalin 200mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/200_figure_number-of-pills-dispensed.png',
       plot = p200_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
pregabalin_200 %>% 
    select(period,
           total_prescribed_dose_200) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 200mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/200_table_total-quantity-dispensed.pdf')

p200_03 <- pregabalin_200 %>% 
    select(period,
           total_prescribed_dose_200) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_prescribed_dose_200,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.200, 
                                          height = 0)) +
    labs(title = 'Pregabalin 200mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/200_figure_total-quantity-dispensed.png',
       plot = p200_03,
       height = 6,
       width = 7)

## Monthly dose per prescription
pregabalin_200 %>% 
    select(period,
           total_dose_per_prescription_200) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 200mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/200_table_dose-per-prescription.pdf')

p200_04 <- pregabalin_200 %>% 
    select(period,
           total_dose_per_prescription_200) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_dose_per_prescription_200,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.200, 
                                          height = 0)) +
    labs(title = 'Pregabalin 200mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/200_figure_dose-per-prescription.png',
       plot = p200_04,
       height = 6,
       width = 7)

## Average daily dose
pregabalin_200 %>% 
    select(period,
           average_daily_dose_200) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 200mg: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/200_table_average-daily-dose.pdf')

p200_05 <- pregabalin_200 %>% 
    select(period,
           average_daily_dose_200) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = average_daily_dose_200,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.200, 
                                          height = 0)) +
    labs(title = 'Pregabalin 200mg: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/200_figure_average-daily-dose.png',
       plot = p200_05,
       height = 6,
       width = 7)

# 50mg dose
## Number of prescription items by period
pregabalin_50 %>% 
    select(period,
           prescriptions_50) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 50mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/50_table_number-of-prescription-items.pdf')

p50_01 <- pregabalin_50 %>% 
    select(period,
           prescriptions_50) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = prescriptions_50,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.50, 
                                          height = 0)) +
    labs(title = 'Pregabalin 50mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/50_figure_number-of-prescription-items.png',
       plot = p50_01,
       height = 6,
       width = 7)
    
## Number of pills dispensed per month
pregabalin_50 %>% 
    select(period,
           pills_50) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 50mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/50_table_number-of-pills-dispensed.pdf')

p50_02 <- pregabalin_50 %>% 
    select(period,
           pills_50) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = pills_50,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.50, 
                                          height = 0)) +
    labs(title = 'Pregabalin 50mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/50_figure_number-of-pills-dispensed.png',
       plot = p50_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
pregabalin_50 %>% 
    select(period,
           total_prescribed_dose_50) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 50mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/50_table_total-quantity-dispensed.pdf')

p50_03 <- pregabalin_50 %>% 
    select(period,
           total_prescribed_dose_50) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_prescribed_dose_50,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.50, 
                                          height = 0)) +
    labs(title = 'Pregabalin 50mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/50_figure_total-quantity-dispensed.png',
       plot = p50_03,
       height = 6,
       width = 7)

## Monthly dose per prescription
pregabalin_50 %>% 
    select(period,
           total_dose_per_prescription_50) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 50mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/50_table_dose-per-prescription.pdf')

p50_04 <- pregabalin_50 %>% 
    select(period,
           total_dose_per_prescription_50) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_dose_per_prescription_50,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.50, 
                                          height = 0)) +
    labs(title = 'Pregabalin 50mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/50_figure_dose-per-prescription.png',
       plot = p50_04,
       height = 6,
       width = 7)

## Average daily dose
pregabalin_50 %>% 
    select(period,
           average_daily_dose_50) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 50mg: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/50_table_average-daily-dose.pdf')

p50_05 <- pregabalin_50 %>% 
    select(period,
           average_daily_dose_50) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = average_daily_dose_50,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.50, 
                                          height = 0)) +
    labs(title = 'Pregabalin 50mg: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/50_figure_average-daily-dose.png',
       plot = p50_05,
       height = 6,
       width = 7)

# 75mg dose
## Number of prescription items by period
pregabalin_75 %>% 
    select(period,
           prescriptions_75) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 75mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/75_table_number-of-prescription-items.pdf')

p75_01 <- pregabalin_75 %>% 
    select(period,
           prescriptions_75) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = prescriptions_75,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.75, 
                                          height = 0)) +
    labs(title = 'Pregabalin 75mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/75_figure_number-of-prescription-items.png',
       plot = p75_01,
       height = 6,
       width = 7)
    
## Number of pills dispensed per month
pregabalin_75 %>% 
    select(period,
           pills_75) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 75mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/75_table_number-of-pills-dispensed.pdf')

p75_02 <- pregabalin_75 %>% 
    select(period,
           pills_75) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = pills_75,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.75, 
                                          height = 0)) +
    labs(title = 'Pregabalin 75mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/75_figure_number-of-pills-dispensed.png',
       plot = p75_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
pregabalin_75 %>% 
    select(period,
           total_prescribed_dose_75) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 75mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/75_table_total-quantity-dispensed.pdf')

p75_03 <- pregabalin_75 %>% 
    select(period,
           total_prescribed_dose_75) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_prescribed_dose_75,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.75, 
                                          height = 0)) +
    labs(title = 'Pregabalin 75mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/75_figure_total-quantity-dispensed.png',
       plot = p75_03,
       height = 6,
       width = 7)

## Monthly dose per prescription
pregabalin_75 %>% 
    select(period,
           total_dose_per_prescription_75) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 75mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/75_table_dose-per-prescription.pdf')

p75_04 <- pregabalin_75 %>% 
    select(period,
           total_dose_per_prescription_75) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_dose_per_prescription_75,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.75, 
                                          height = 0)) +
    labs(title = 'Pregabalin 75mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/75_figure_dose-per-prescription.png',
       plot = p75_04,
       height = 6,
       width = 7)

# 100mg dose
## Number of prescription items by period
pregabalin_100 %>% 
    select(period,
           prescriptions_100) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 100mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/100_table_number-of-prescription-items.pdf')

p100_01 <- pregabalin_100 %>% 
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
               position = position_jitter(width = 0.100, 
                                          height = 0)) +
    labs(title = 'Pregabalin 100mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/100_figure_number-of-prescription-items.png',
       plot = p100_01,
       height = 6,
       width = 7)
    
## Number of pills dispensed per month
pregabalin_100 %>% 
    select(period,
           pills_100) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 100mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/100_table_number-of-pills-dispensed.pdf')

p100_02 <- pregabalin_100 %>% 
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
               position = position_jitter(width = 0.100, 
                                          height = 0)) +
    labs(title = 'Pregabalin 100mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/100_figure_number-of-pills-dispensed.png',
       plot = p100_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
pregabalin_100 %>% 
    select(period,
           total_prescribed_dose_100) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 100mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/100_table_total-quantity-dispensed.pdf')

p100_03 <- pregabalin_100 %>% 
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
               position = position_jitter(width = 0.100, 
                                          height = 0)) +
    labs(title = 'Pregabalin 100mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/100_figure_total-quantity-dispensed.png',
       plot = p100_03,
       height = 6,
       width = 7)

## Monthly dose per prescription
pregabalin_100 %>% 
    select(period,
           total_dose_per_prescription_100) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 100mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/100_table_dose-per-prescription.pdf')

p100_04 <- pregabalin_100 %>% 
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
               position = position_jitter(width = 0.100, 
                                          height = 0)) +
    labs(title = 'Pregabalin 100mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/100_figure_dose-per-prescription.png',
       plot = p100_04,
       height = 6,
       width = 7)

## Average daily dose
pregabalin_100 %>% 
    select(period,
           average_daily_dose_100) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 100mg: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/100_table_average-daily-dose.pdf')

p100_05 <- pregabalin_100 %>% 
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
               position = position_jitter(width = 0.100, 
                                          height = 0)) +
    labs(title = 'Pregabalin 100mg: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/100_figure_average-daily-dose.png',
       plot = p100_05,
       height = 6,
       width = 7)

# 150mg dose
## Number of prescription items by period
pregabalin_150 %>% 
    select(period,
           prescriptions_150) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 150mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/150_table_number-of-prescription-items.pdf')

p150_01 <- pregabalin_150 %>% 
    select(period,
           prescriptions_150) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = prescriptions_150,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.150, 
                                          height = 0)) +
    labs(title = 'Pregabalin 150mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/150_figure_number-of-prescription-items.png',
       plot = p150_01,
       height = 6,
       width = 7)
    
## Number of pills dispensed per month
pregabalin_150 %>% 
    select(period,
           pills_150) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 150mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/150_table_number-of-pills-dispensed.pdf')

p150_02 <- pregabalin_150 %>% 
    select(period,
           pills_150) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = pills_150,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.150, 
                                          height = 0)) +
    labs(title = 'Pregabalin 150mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/150_figure_number-of-pills-dispensed.png',
       plot = p150_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
pregabalin_150 %>% 
    select(period,
           total_prescribed_dose_150) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 150mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/150_table_total-quantity-dispensed.pdf')

p150_03 <- pregabalin_150 %>% 
    select(period,
           total_prescribed_dose_150) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_prescribed_dose_150,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.150, 
                                          height = 0)) +
    labs(title = 'Pregabalin 150mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/150_figure_total-quantity-dispensed.png',
       plot = p150_03,
       height = 6,
       width = 7)

## Monthly dose per prescription
pregabalin_150 %>% 
    select(period,
           total_dose_per_prescription_150) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 150mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/150_table_dose-per-prescription.pdf')

p150_04 <- pregabalin_150 %>% 
    select(period,
           total_dose_per_prescription_150) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_dose_per_prescription_150,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.150, 
                                          height = 0)) +
    labs(title = 'Pregabalin 150mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/150_figure_dose-per-prescription.png',
       plot = p150_04,
       height = 6,
       width = 7)

## Average daily dose
pregabalin_150 %>% 
    select(period,
           average_daily_dose_150) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 150mg: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/150_table_average-daily-dose.pdf')

p150_05 <- pregabalin_150 %>% 
    select(period,
           average_daily_dose_150) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = average_daily_dose_150,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.150, 
                                          height = 0)) +
    labs(title = 'Pregabalin 150mg: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/150_figure_average-daily-dose.png',
       plot = p150_05,
       height = 6,
       width = 7)

# 200mg dose
## Number of prescription items by period
pregabalin_200 %>% 
    select(period,
           prescriptions_200) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 200mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/200_table_number-of-prescription-items.pdf')

p200_01 <- pregabalin_200 %>% 
    select(period,
           prescriptions_200) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = prescriptions_200,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.200, 
                                          height = 0)) +
    labs(title = 'Pregabalin 200mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/200_figure_number-of-prescription-items.png',
       plot = p200_01,
       height = 6,
       width = 7)
    
## Number of pills dispensed per month
pregabalin_200 %>% 
    select(period,
           pills_200) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 200mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/200_table_number-of-pills-dispensed.pdf')

p200_02 <- pregabalin_200 %>% 
    select(period,
           pills_200) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = pills_200,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.200, 
                                          height = 0)) +
    labs(title = 'Pregabalin 200mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/200_figure_number-of-pills-dispensed.png',
       plot = p200_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
pregabalin_200 %>% 
    select(period,
           total_prescribed_dose_200) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 200mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/200_table_total-quantity-dispensed.pdf')

p200_03 <- pregabalin_200 %>% 
    select(period,
           total_prescribed_dose_200) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_prescribed_dose_200,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.200, 
                                          height = 0)) +
    labs(title = 'Pregabalin 200mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/200_figure_total-quantity-dispensed.png',
       plot = p200_03,
       height = 6,
       width = 7)

## Monthly dose per prescription
pregabalin_200 %>% 
    select(period,
           total_dose_per_prescription_200) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 200mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/200_table_dose-per-prescription.pdf')

p200_04 <- pregabalin_200 %>% 
    select(period,
           total_dose_per_prescription_200) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_dose_per_prescription_200,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.200, 
                                          height = 0)) +
    labs(title = 'Pregabalin 200mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/200_figure_dose-per-prescription.png',
       plot = p200_04,
       height = 6,
       width = 7)

## Average daily dose
pregabalin_200 %>% 
    select(period,
           average_daily_dose_200) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 200mg: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/200_table_average-daily-dose.pdf')

p200_05 <- pregabalin_200 %>% 
    select(period,
           average_daily_dose_200) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = average_daily_dose_200,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.200, 
                                          height = 0)) +
    labs(title = 'Pregabalin 200mg: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/200_figure_average-daily-dose.png',
       plot = p200_05,
       height = 6,
       width = 7)

# 225mg dose
## Number of prescription items by period
pregabalin_225 %>% 
    select(period,
           prescriptions_225) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 225mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/225_table_number-of-prescription-items.pdf')

p225_01 <- pregabalin_225 %>% 
    select(period,
           prescriptions_225) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = prescriptions_225,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.225, 
                                          height = 0)) +
    labs(title = 'Pregabalin 225mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/225_figure_number-of-prescription-items.png',
       plot = p225_01,
       height = 6,
       width = 7)
    
## Number of pills dispensed per month
pregabalin_225 %>% 
    select(period,
           pills_225) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 225mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/225_table_number-of-pills-dispensed.pdf')

p225_02 <- pregabalin_225 %>% 
    select(period,
           pills_225) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = pills_225,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.225, 
                                          height = 0)) +
    labs(title = 'Pregabalin 225mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/225_figure_number-of-pills-dispensed.png',
       plot = p225_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
pregabalin_225 %>% 
    select(period,
           total_prescribed_dose_225) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 225mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/225_table_total-quantity-dispensed.pdf')

p225_03 <- pregabalin_225 %>% 
    select(period,
           total_prescribed_dose_225) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_prescribed_dose_225,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.225, 
                                          height = 0)) +
    labs(title = 'Pregabalin 225mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/225_figure_total-quantity-dispensed.png',
       plot = p225_03,
       height = 6,
       width = 7)

## Monthly dose per prescription
pregabalin_225 %>% 
    select(period,
           total_dose_per_prescription_225) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 225mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/225_table_dose-per-prescription.pdf')

p225_04 <- pregabalin_225 %>% 
    select(period,
           total_dose_per_prescription_225) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = total_dose_per_prescription_225,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.225, 
                                          height = 0)) +
    labs(title = 'Pregabalin 225mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/225_figure_dose-per-prescription.png',
       plot = p225_04,
       height = 6,
       width = 7)

## Average daily dose
pregabalin_225 %>% 
    select(period,
           average_daily_dose_225) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 225mg: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/225_table_average-daily-dose.pdf')

p225_05 <- pregabalin_225 %>% 
    select(period,
           average_daily_dose_225) %>% 
    ggplot(data = .) +
    aes(x = period,
        y = average_daily_dose_225,
        colour = period,
        fill = period) +
    geom_boxplot(alpha = 0.6,
                 outlier.size = -Inf) +
    geom_point(size = 3,
               position = position_jitter(width = 0.225, 
                                          height = 0)) +
    labs(title = 'Pregabalin 225mg: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/225_figure_average-daily-dose.png',
       plot = p225_05,
       height = 6,
       width = 7)

# 300mg dose
## Number of prescription items by period
pregabalin_300 %>% 
    select(period,
           prescriptions_300) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 300mg: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/300_table_number-of-prescription-items.pdf')

p300_01 <- pregabalin_300 %>% 
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
               position = position_jitter(width = 0.300, 
                                          height = 0)) +
    labs(title = 'Pregabalin 300mg: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/300_figure_number-of-prescription-items.png',
       plot = p300_01,
       height = 6,
       width = 7)
    
## Number of pills dispensed per month
pregabalin_300 %>% 
    select(period,
           pills_300) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 300mg: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/300_table_number-of-pills-dispensed.pdf')

p300_02 <- pregabalin_300 %>% 
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
               position = position_jitter(width = 0.300, 
                                          height = 0)) +
    labs(title = 'Pregabalin 300mg: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/300_figure_number-of-pills-dispensed.png',
       plot = p300_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
pregabalin_300 %>% 
    select(period,
           total_prescribed_dose_300) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 300mg: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/300_table_total-quantity-dispensed.pdf')

p300_03 <- pregabalin_300 %>% 
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
               position = position_jitter(width = 0.300, 
                                          height = 0)) +
    labs(title = 'Pregabalin 300mg: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/300_figure_total-quantity-dispensed.png',
       plot = p300_03,
       height = 6,
       width = 7)

## Monthly dose per prescription
pregabalin_300 %>% 
    select(period,
           total_dose_per_prescription_300) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 300mg: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/300_table_dose-per-prescription.pdf')

p300_04 <- pregabalin_300 %>% 
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
               position = position_jitter(width = 0.300, 
                                          height = 0)) +
    labs(title = 'Pregabalin 300mg: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/300_figure_dose-per-prescription.png',
       plot = p300_04,
       height = 6,
       width = 7)

## Average daily dose
pregabalin_300 %>% 
    select(period,
           average_daily_dose_300) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin 300mg: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/300_table_average-daily-dose.pdf')

p300_05 <- pregabalin_300 %>% 
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
               position = position_jitter(width = 0.300, 
                                          height = 0)) +
    labs(title = 'Pregabalin 300mg: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/300_figure_average-daily-dose.png',
       plot = p300_05,
       height = 6,
       width = 7)

# Total
## Number of prescription items by period
pregabalin_total %>% 
    select(period,
           prescriptions_total) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin total: Number of prescription items per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/total_table_number-of-prescription-items.pdf')

p_total_01 <- pregabalin_total %>% 
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
               position = position_jitter(width = 0.50, 
                                          height = 0)) +
    labs(title = 'Pregabalin total: Number of prescription items per month',
         x = NULL,
         y = 'Number of prescriptions') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/total_figure_number-of-prescription-items.png',
       plot = p_total_01,
       height = 6,
       width = 7)

## Number of pills dispensed per month
pregabalin_total %>% 
    select(period,
           pills_total) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin total: Number of pills dispensed per month',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/total_table_number-of-pills-dispensed.pdf')

p_total_02 <- pregabalin_total %>% 
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
               position = position_jitter(width = 0.50, 
                                          height = 0)) +
    labs(title = 'Pregabalin total: Number of pills dispensed per month',
         x = NULL,
         y = 'Number of pills') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/total_figure_number-of-pills-dispensed.png',
       plot = p_total_02,
       height = 6,
       width = 7)

## Total dose prescribed per month
pregabalin_total %>% 
    select(period,
           dose_total) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin total: Total quantity dispensed (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/total_table_total-quantity-dispensed.pdf')

p_total_03 <- pregabalin_total %>% 
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
               position = position_jitter(width = 0.50, 
                                          height = 0)) +
    labs(title = 'Pregabalin total: Total quantity dispensed',
         x = NULL,
         y = 'Quantity (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/total_figure_total-quantity-dispensed.png',
       plot = p_total_03,
       height = 6,
       width = 7)

## Average monthly dose per prescription
pregabalin_total %>% 
    select(period,
           dose_per_prescription_total) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin total: Monthly dose per prescription item (mg)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/total_table_dose-per-prescription.pdf')

p_total_04 <- pregabalin_total %>% 
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
               position = position_jitter(width = 0.50, 
                                          height = 0)) +
    labs(title = 'Pregabalin total: Monthly dose per prescription item',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/total_figure_dose-per-prescription.png',
       plot = p_total_04,
       height = 6,
       width = 7)

## Average daily dose
pregabalin_total %>% 
    select(period,
           average_daily_dose_total) %>% 
    group_by(period) %>% 
    my_skim() %>% 
    yank(skim_type = 'numeric') %>% 
    select(-skim_variable, -n_missing, -n_complete) %>% 
    kbl(caption = 'Pregabalin total: Average daily dose (mg, assumes 30-day prescriptions)',
        digits = 0,
        col.names = c('Period', 'Mean', 'SD', 
                      'Min', 'Q1', 'Median', 'Q3', 'Max')) %>% 
    kable_styling(bootstrap_options = 'striped') %>% 
    save_kable(file = 'figures/pregabalin/total_table_average-daily-dose.pdf')

p_total_05 <- pregabalin_total %>% 
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
               position = position_jitter(width = 0.50, 
                                          height = 0)) +
    labs(title = 'Pregabalin total: Average daily dose*',
         caption = '* Assumes 30-day prescriptions',
         x = NULL,
         y = 'Dose (mg)') +
    scale_y_continuous(labels = function(x){format(x, 
                                                   big.mark = ' ', 
                                                   small.mark = ' ')}) +
    scale_colour_tableau() +
    scale_fill_tableau()

ggsave(filename = 'figures/pregabalin/total_figure_average-daily-dose.png',
       plot = p_total_05,
       height = 6,
       width = 7)
