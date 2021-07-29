############################################################
#                                                          #
#                   Modelling: pregabalin                  #
#                                                          #
############################################################

#-- Load packages --#
library(readr)
library(dplyr)
library(broom)
library(ggplot2)
library(quantreg)
library(lspline)
library(patchwork)
library(moderndive)

#-- Set ggplot theme --#
theme_set(new = theme_minimal(base_size = 18) +
              theme(legend.position = 'none',
                    plot.title = element_text(size = 18),
                    plot.caption = element_text(size = 12),
                    panel.grid = element_blank(),
                    axis.text = element_text(colour = '#000000', 
                                             size = 18),
                    axis.line = element_line(size = 0.5),
                    axis.ticks = element_line(size = 0.5)))

#-- Import data --#
pregabalin <- read_csv('data-clean/pregabalin_analysis-set.csv')

#-- Process data --#
data <- pregabalin %>%
    # Add a colour variable (before and after April 2019)
    mutate(colour = case_when(
        month <= 24 ~ 'before',
        month > 24 ~ 'after'
    )) %>% 
    # Rename columns to simplify analysis
    rename(dose_average = weighted_average_daily_dose_total) %>% 
    # Select columns
    select(date, month, colour, prescriptions_total, dose_average)

########
# Dose #
########
#-- Simple robust regression (knots = 0) --#
# Generate model
dose_simple <- rq(dose_average ~ month, 
                  data = data)

# Inspect model coefficients
tidy(dose_simple)

# Get fitted values
fitted_dose_simple <- data.frame(month = 1:49,
                                 dose_average = data$dose_average,
                                 .fitted = predict(dose_simple),
                                 colour = c(rep('before', 24), rep('after', 25)))

## Generate plot
plot_dose_simple <- ggplot(data = fitted_dose_simple) +
    aes(x = month,
        y = dose_average) +
    geom_point(aes(colour = colour,
                   fill = colour),
               shape = 21,
               size = 5) +
    geom_line(aes(y = .fitted),
              colour = '#000000',
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = 'Simple',
         y = 'Average daily dose (mg)',
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_y_continuous(position = 'right') +
    scale_fill_manual(values = c('#67A0C9', '#FDA568')) +
    scale_colour_manual(values = c('#2678B2', '#FD7F28')); plot_dose_simple

#-- Interrupted robust regression (knots = 1) --#
# Generate model
dose_spline <- rq(dose_average ~ lspline(x = month,
                                         knots = 25,
                                         marginal = FALSE), 
                  data = data)

# Inspect model coefficients
tidy(dose_spline)

# Get fitted values
fitted_dose_spline <- data.frame(month = 1:49,
                                 dose_average = data$dose_average,
                                 .fitted = predict(dose_spline),
                                 colour = c(rep('before', 24), rep('after', 25)))

## Generate plot
plot_dose_spline <- ggplot(data = fitted_dose_spline) +
    aes(x = month,
        y = dose_average) +
    geom_point(aes(colour = colour,
                   fill = colour),
               shape = 21,
               size = 5) +
    geom_line(aes(y = .fitted),
              colour = '#000000',
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = 'Spline (knots = 1)',
         y = 'Average daily dose (mg)',
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_y_continuous(position = 'right') +
    scale_fill_manual(values = c('#67A0C9', '#FDA568')) +
    scale_colour_manual(values = c('#2678B2', '#FD7F28')); plot_dose_spline 

#-- Compare models --#
# Get AIC for each model
AIC(dose_simple)
AIC(dose_spline)

# Calculate difference in AIC 
## Values < 2 indicate no meaningful information loss (Burnham & Anderson 2004)
AIC(dose_simple) - AIC(dose_spline)

#-- Simple robust regression (knots = 0): parallel slopes
# Generate model
dose_parallel <- rq(dose_average ~ month + colour,
                    data = data)

# Inspect model coefficients
tidy(dose_parallel)

## Generate plot
plot_dose_parallel <- ggplot(data = data) +
    aes(x = month,
        y = dose_average) +
    geom_point(aes(colour = colour,
                   fill = colour),
               shape = 21,
               size = 5) +
    geom_parallel_slopes(aes(colour = colour),
                         se = FALSE, 
                         size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = 'Parallel slopes',
         y = 'Average daily dose (mg)',
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_y_continuous(position = 'right') +
    scale_fill_manual(values = c('#67A0C9', '#FDA568')) +
    scale_colour_manual(values = c('#2678B2', '#FD7F28')); plot_dose_parallel 

#-- Compare models --#
# Get AIC for each model
AIC(dose_simple)
AIC(dose_parallel)

# Calculate difference in AIC 
## Values < 2 indicate no meaningful information loss (Burnham & Anderson 2004)
## Values > 10 indicate meaningful information loss, i.e., the models are not comparable
AIC(dose_parallel) - AIC(dose_simple)

anova(dose_simple, dose_parallel)

#################
# Prescriptions #
#################

#-- Simple robust regression (knots = 0) --#
# Generate model
prescription_simple <- rq(prescriptions_total ~ month, 
                          data = data)

# Inspect model coefficients
tidy(prescription_simple)

# Get fitted values
fitted_prescription_simple <- data.frame(month = 1:49,
                                         prescriptions_total = data$prescriptions_total,
                                         .fitted = predict(prescription_simple),
                                         colour = c(rep('before', 24), rep('after', 25)))

## Generate plot
plot_prescription_simple <- ggplot(data = fitted_prescription_simple) +
    aes(x = month,
        y = prescriptions_total) +
    geom_point(aes(colour = colour,
                   fill = colour),
               shape = 21,
               size = 5) +
    geom_line(aes(y = .fitted),
              colour = '#000000',
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = 'Simple',
         y = 'Number of prescription items',
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_y_continuous(labels = function(x){format(x, big.mark = ' ')}) +
    scale_fill_manual(values = c('#67A0C9', '#FDA568')) +
    scale_colour_manual(values = c('#2678B2', '#FD7F28')); plot_prescription_simple

#-- Interrupted Robust regression (knots = 1) --#
# Generate model
prescription_spline <- rq(prescriptions_total ~ lspline(x = month,
                                         knots = 25,
                                         marginal = FALSE), 
                          data = data)

# Inspect model coefficients
tidy(prescription_spline)

# Get fitted values
fitted_prescription_spline <- data.frame(month = 1:49,
                                         prescriptions_total = data$prescriptions_total,
                                         .fitted = predict(prescription_spline),
                                         colour = c(rep('before', 24), rep('after', 25)))

## Generate plot
plot_prescription_spline <- ggplot(data = fitted_prescription_spline) +
    aes(x = month,
        y = prescriptions_total) +
    geom_point(aes(colour = colour,
                   fill = colour),
               shape = 21,
               size = 5) +
    geom_line(aes(y = .fitted),
              colour = '#000000',
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = 'Spline (knots = 1)',
         y = 'Number of prescription items',
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_y_continuous(labels = function(x){format(x, big.mark = ' ')}) +
    scale_fill_manual(values = c('#67A0C9', '#FDA568')) +
    scale_colour_manual(values = c('#2678B2', '#FD7F28')); plot_prescription_spline 

#-- Compare models --#
# Get AIC for each model
AIC(prescription_simple)
AIC(prescription_spline)

# Calculate difference in AIC 
## Values < 2 indicate no meaningful information loss (Burnham & Anderson 2004)
AIC(prescription_simple) - AIC(prescription_spline)

#-- Simple robust regression (knots = 0): parallel slopes
# Generate model
prescription_parallel <- rq(prescriptions_total ~ month + colour,
                            data = data)

# Inspect model coefficients
tidy(prescription_parallel)

## Generate plot
plot_prescription_parallel <- ggplot(data = data) +
    aes(x = month,
        y = prescriptions_total) +
    geom_point(aes(colour = colour,
                   fill = colour),
               shape = 21,
               size = 5) +
    geom_parallel_slopes(aes(colour = colour),
                         se = FALSE, 
                         size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = 'Parallel slopes',
         y = 'Number of prescription items',
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_y_continuous(labels = function(x){format(x, big.mark = ' ')}) +
    scale_fill_manual(values = c('#67A0C9', '#FDA568')) +
    scale_colour_manual(values = c('#2678B2', '#FD7F28')) ; plot_prescription_parallel 

#-- Compare models --#
# Get AIC for each model
AIC(prescription_simple)
AIC(prescription_parallel)

# Calculate difference in AIC 
## Values < 2 indicate no meaningful information loss (Burnham & Anderson 2004)
## Values > 10 indicate meaningful information loss, i.e., the models are not comparable
AIC(prescription_parallel) - AIC(prescription_simple)

anova(prescription_simple, prescription_parallel)

########################
#   Publication plot   #
########################

# Check margins
theme_minimal()$plot.margin

#-- Dose --#
publication_dose_simple <- plot_dose_simple +
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          plot.margin = margin(t = 5.5, l = 20, b = 5.5, r = 5.5))

publication_dose_spline <- plot_dose_spline +
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = margin(t = 5.5, l = 20, b = 5.5, r = 5.5))

publication_dose_parallel <- plot_dose_parallel +
    theme(axis.title.y = element_blank(),
          plot.margin = margin(t = 5.5, l = 20, b = 5.5, r = 5.5))

#-- Prescription items --#
publication_prescription_simple <- plot_prescription_simple +
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank() ,
          plot.margin = margin(t = 5.5, r = 20, b = 5.5, l = 5.5))

publication_prescription_spline <- plot_prescription_spline +
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank() ,
          plot.margin = margin(t = 5.5, r = 20, b = 5.5, l = 5.5))

publication_prescription_parallel <- plot_prescription_parallel +
    theme(axis.title.y = element_blank(),
          plot.margin = margin(t = 5.5, r = 20, b = 5.5, l = 5.5))

#-- Combined plot --#
publication_plot <- publication_prescription_simple +
    publication_prescription_spline +
    publication_prescription_parallel + 
    publication_dose_simple +
    publication_dose_spline +
    publication_dose_parallel +
    plot_layout(ncol = 2, byrow = FALSE) +
    plot_annotation(tag_levels = 'A')

ggsave('figures/pregabalin/07_publication-plot.png',
       plot = publication_plot,
       height = 15,
       width = 16)
