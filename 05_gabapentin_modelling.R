############################################################
#                                                          #
#                  Modelling: gabapentin                   #
#                                                          #
############################################################

#-- Load packages --#
library(readr)
library(dplyr)
library(broom)
library(ggplot2)
library(ggthemes)
library(quantreg)
library(lspline)
library(patchwork)
library(moderndive)
library(MuMIn)

#-- Set ggplot theme --#
theme_set(new = theme_minimal(base_size = 18) +
              theme(legend.position = 'none',
                    plot.title = element_text(size = 18),
                    plot.caption = element_text(size = 12),
                    plot.background = element_rect(colour = '#FFFFFF',
                                                   fill = '#FFFFFF'),
                    panel.grid = element_blank(),
                    axis.text = element_text(colour = '#000000', 
                                             size = 18),
                    axis.line = element_line(size = 0.5),
                    axis.ticks = element_line(size = 0.5)))

#-- Define functions --#
# Evidence ratio
## x = vector of relative likelihood for the models (best model first)
## y = vector of names for comparisons
evidence_ratio = function(x = vec_rl, y = vec_names) {
    er <- vector(mode = 'numeric', length = length(vec_names))
    for(i in 1:length(vec_names)){
        er[[i]] <- vec_rl[[1]] / vec_rl[[i]]
    }
    names(er) <- vec_names
    return(er)
}

# Evidence weights
## x = relative likelihood for the models (best model first)
## y = vector of comparison names
evidence_weight <- function(x = vec_rl, y = vec_names) {
    sum_rel = sum(vec_rl)
    wt = vector(mode = 'numeric', length = length(vec_names))
    for(i in 1:length(vec_names)){
        wt[[i]] <- vec_rl[[i]] / sum_rel
        names(wt) <- vec_names 
    }
    return(wt)
}

#-- Import data --#
gabapentin <- read_csv('data-clean/gabapentin_analysis-set.csv')

#-- Process data --#
data <- gabapentin %>%
    # Add a colour variable (before and after April 2019)
    mutate(period = ifelse(date < as.Date('2019-04-01'),
                           yes = 'before',
                           no = 'after')) %>%
    # Select columns
    select(month, 
           period, 
           prescriptions_total, 
           pills_total, 
           dose_total,
           dose_per_prescription_total) 

########################################
# Monthly number of prescription items #
########################################

#-- Simple quantile regression (tau = 0.5, knots = 0) --#
# Generate model
prescriptions_simple <- rq(prescriptions_total ~ month, 
                           data = data)

# Inspect model coefficients
tidy(prescriptions_simple)

# Get fitted values
fitted_prescriptions_simple <- data.frame(month = 1:48,
                                          prescriptions_total = data$prescriptions_total,
                                          .fitted = predict(prescriptions_simple),
                                          period = c(rep('before', 24), rep('after', 24)))

## Generate plot
plot_prescriptions_simple <- ggplot(data = fitted_prescriptions_simple) +
    aes(x = month,
        y = prescriptions_total/10000) +
    geom_point(aes(colour = period,
                   fill = period),
               shape = 21,
               size = 5,
               alpha = 0.8) +
    geom_line(aes(y = .fitted/10000),
              colour = '#000000',
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = paste0('Simple (AICc =  ', round(AICc(prescriptions_simple), 2), ')'),
         y = expression('Number of prescription items (10'^4*')'),
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_fill_tableau() +
    scale_colour_tableau() +
    theme(plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_prescriptions_simple

#-- Interrupted quantile regression (tau = 0.5, knots = 1) --#
# Generate model
prescriptions_spline <- rq(prescriptions_total ~ lspline(x = month,
                                                         knots = 25,
                                                         marginal = FALSE), 
                           data = data)

# Inspect model coefficients
tidy(prescriptions_spline)

# Get fitted values
fitted_prescriptions_spline <- data.frame(month = 1:48,
                                          prescriptions_total = data$prescriptions_total,
                                          .fitted = predict(prescriptions_spline),
                                          period = c(rep('before', 24), rep('after', 24)))

## Generate plot
plot_prescriptions_spline <- ggplot(data = fitted_prescriptions_spline) +
    aes(x = month,
        y = prescriptions_total/10000) +
    geom_point(aes(colour = period,
                   fill = period),
               shape = 21,
               size = 5,
               alpha = 0.8) +
    geom_line(aes(y = .fitted/10000),
              colour = '#000000',
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = paste0('Spline (knots = 1) (AICc =  ', round(AICc(prescriptions_spline), 2), ')'),
         y = expression('Number of prescription items (10'^4*')'),
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_fill_tableau() +
    scale_colour_tableau() +
    theme(plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_prescriptions_spline 

#-- Simple robust regression (knots = 0): parallel slopes --#
# Generate model
prescriptions_parallel <- rq(prescriptions_total ~ month + period,
                             data = data)

# Inspect model coefficients
tidy(prescriptions_parallel)

# Get fitted values
fitted_prescriptions_parallel <- data.frame(month = 1:48,
                                            prescriptions_total = data$prescriptions_total,
                                            .fitted = predict(prescriptions_parallel),
                                            period = c(rep('before', 24), rep('after', 24)))

## Generate plot
plot_prescriptions_parallel <- ggplot(data = fitted_prescriptions_parallel) +
    aes(x = month,
        y = prescriptions_total/10000) +
    geom_point(aes(colour = period,
                   fill = period),
               shape = 21,
               size = 5,
               alpha = 0.8) +
    geom_parallel_slopes(aes(group = period),
                         colour = '#000000',
                         se = FALSE, 
                         size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = paste0('Parallel slopes (AICc =  ', round(AICc(prescriptions_parallel), 2), ')'),
         y = expression('Number of prescription items (10'^4*')'),
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_fill_tableau() +
    scale_colour_tableau() +
    theme(plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_prescriptions_parallel

#-- Compare models --#
# Get AIC for each model
AICc(prescriptions_simple)
AICc(prescriptions_spline)
AICc(prescriptions_parallel)

# Calculate difference in AIC (vs parallel slopes regression, the simplest model)
## Values < 2 indicate no meaningful information loss (Burnham & Anderson 2004)
prescription_delta_aic0 <- AICc(prescriptions_parallel) - AICc(prescriptions_parallel)
prescription_delta_aic0
prescription_delta_aic1 <- AICc(prescriptions_spline) - AICc(prescriptions_parallel)
prescription_delta_aic1
prescription_delta_aic2 <- AICc(prescriptions_simple) - AICc(prescriptions_parallel)
prescription_delta_aic2

# Relative likelihoods
rl0 <- exp(-1/2 * prescription_delta_aic0)
rl0

rl1 <- exp(-1/2 * prescription_delta_aic1)
rl1

rl2 <- exp(-1/2 * prescription_delta_aic2)
rl2

# Get data
vec_rl <- c(rl0, rl1, rl2)

vec_names <- c('parallel v parallel', 'parallel v spline', 'parallel vs simple')

# Evidence ratio 
evidence_ratio(x = vec_rel,
               y = vec_names)

# Evidence weights
evidence_weight(x = vec_rl,
                y = vec_names)

#######################################
# Number of pills dispensed per month #
#######################################

#-- Simple quantile regression (tau = 0.5, knots = 0) --#
# Generate model
pills_simple <- rq(pills_total ~ month, 
                   data = data)

# Inspect model coefficients
tidy(pills_simple)

# Get fitted values
fitted_pills_simple <- data.frame(month = 1:48,
                                  pills_total = data$pills_total,
                                  .fitted = predict(pills_simple),
                                  period = c(rep('before', 24), rep('after', 24)))

## Generate plot
plot_pills_simple <- ggplot(data = fitted_pills_simple) +
    aes(x = month,
        y = pills_total/1000000) +
    geom_point(aes(colour = period,
                   fill = period),
               shape = 21,
               size = 5,
               alpha = 0.8) +
    geom_line(aes(y = .fitted/1000000),
              colour = '#000000',
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = paste0('Simple (AICc =  ', round(AICc(pills_simple), 2), ')'),
         y = NULL,
         x = NULL) +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_fill_tableau() +
    scale_colour_tableau() +
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_pills_simple

#-- Interrupted quantile regression (tau = 0.5, knots = 1) --#
# Generate model
pills_spline <- rq(pills_total ~ lspline(x = month,
                                         knots = 25,
                                         marginal = FALSE), 
                   data = data)

# Inspect model coefficients
tidy(pills_spline)

# Get fitted values
fitted_pills_spline <- data.frame(month = 1:48,
                                  pills_total = data$pills_total,
                                  .fitted = predict(pills_spline),
                                  period = c(rep('before', 24), rep('after', 24)))

## Generate plot
plot_pills_spline <- ggplot(data = fitted_pills_spline) +
    aes(x = month,
        y = pills_total/1000000) +
    geom_point(aes(colour = period,
                   fill = period),
               shape = 21,
               size = 5,
               alpha = 0.8) +
    geom_line(aes(y = .fitted/1000000),
              colour = '#000000',
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = paste0('Spline (knots = 1) (AICc =  ', round(AICc(pills_spline), 2), ')'),
         y = expression('Number of pills (10'^6*')'),
         x = NULL) +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_fill_tableau() +
    scale_colour_tableau() +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_pills_spline 

#-- Simple robust regression (knots = 0): parallel slopes --#
# Generate model
pills_parallel <- rq(pills_total ~ month + period,
                     data = data)

# Inspect model coefficients
tidy(pills_parallel)

# Get fitted values
fitted_pills_parallel <- data.frame(month = 1:48,
                                    pills_total = data$pills_total,
                                    .fitted = predict(pills_parallel),
                                    period = c(rep('before', 24), rep('after', 24)))

## Generate plot
plot_pills_parallel <- ggplot(data = fitted_pills_parallel) +
    aes(x = month,
        y = pills_total/1000000) +
    geom_point(aes(colour = period,
                   fill = period),
               shape = 21,
               size = 5,
               alpha = 0.8) +
    geom_parallel_slopes(aes(group = period),
                         colour = '#000000',
                         se = FALSE, 
                         size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = paste0('Parallel slopes (AICc =  ', round(AICc(pills_parallel), 2), ')'),
         y = NULL,
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_fill_tableau() +
    scale_colour_tableau() +
    theme(plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_pills_parallel

#-- Compare models --#
# Get AIC for each model
AICc(pills_simple)
AICc(pills_spline)
AICc(pills_parallel)

# Calculate difference in AIC (vs parallel slopes regression, the simplest model)
## Values < 2 indicate no meaningful information loss (Burnham & Anderson 2004)

pills_delta_aic0 <- AICc(pills_parallel) - AICc(pills_parallel)
pills_delta_aic0
pills_delta_aic1 <- AICc(pills_spline) - AICc(pills_parallel)
pills_delta_aic1
pills_delta_aic2 <- AICc(pills_simple) - AICc(pills_parallel)
pills_delta_aic2

# Relative likelihoods
rl0 <- exp(-1/2 * pills_delta_aic0)
rl0

rl1 <- exp(-1/2 * pills_delta_aic1)
rl1

rl2 <- exp(-1/2 * pills_delta_aic2)
rl2

# Get data
vec_rl <- c(rl0, rl1, rl2)

vec_names <- c('parallel v parallel', 'parallel v spline', 'parallel vs simple')

# Evidence ratio 
evidence_ratio(x = vec_rel,
               y = vec_names)

# Evidence weights
evidence_weight(x = vec_rl,
                y = vec_names)

#################################
# Monthly total dose prescribed #
#################################

#-- Simple quantile regression (tau = 0.5, knots = 0) --#
# Generate model
quantity_simple <- rq(dose_total ~ month, 
                      data = data)

# Inspect model coefficients
tidy(quantity_simple)

# Get fitted values
fitted_quantity_simple <- data.frame(month = 1:48,
                                     dose_total = data$dose_total,
                                     .fitted = predict(quantity_simple),
                                     period = c(rep('before', 24), rep('after', 24)))

## Generate plot
plot_quantity_simple <- ggplot(data = fitted_quantity_simple) +
    aes(x = month,
        y = dose_total/10000000000) +
    geom_point(aes(colour = period,
                   fill = period),
               shape = 21,
               size = 5,
               alpha = 0.8) +
    geom_line(aes(y = .fitted/10000000000),
              colour = '#000000',
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = paste0('Simple (AICc =  ', round(AICc(quantity_simple), 2), ')'),
         y = NULL,
         x = NULL) +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_fill_tableau() +
    scale_colour_tableau() +
    theme(axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_quantity_simple

#-- Interrupted quantile regression (tau = 0.5, knots = 1) --#
# Generate model
quantity_spline <- rq(dose_total ~ lspline(x = month,
                                           knots = 25,
                                           marginal = FALSE), 
                      data = data)

# Inspect model coefficients
tidy(quantity_spline)

# Get fitted values
fitted_quantity_spline <- data.frame(month = 1:48,
                                     dose_total = data$dose_total,
                                     .fitted = predict(quantity_spline),
                                     period = c(rep('before', 24), rep('after', 24)))

## Generate plot
plot_quantity_spline <- ggplot(data = fitted_quantity_spline) +
    aes(x = month,
        y = dose_total/10000000000) +
    geom_point(aes(colour = period,
                   fill = period),
               shape = 21,
               size = 5,
               alpha = 0.8) +
    geom_line(aes(y = .fitted/10000000000),
              colour = '#000000',
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = paste0('Spline (knots = 1) (AICc =  ', round(AICc(quantity_spline), 2), ')'),
         y = expression('Total dose dispensed (10'^10*' mg)'),
         x = NULL) +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_fill_tableau() +
    scale_colour_tableau() +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_quantity_spline 

#-- Simple robust regression (knots = 0): parallel slopes --#
# Generate model
quantity_parallel <- rq(dose_total ~ month + period,
                        data = data)

# Inspect model coefficients
tidy(quantity_parallel)

# Get fitted values
fitted_quantity_parallel <- data.frame(month = 1:48,
                                       dose_total = data$dose_total,
                                       .fitted = predict(quantity_parallel),
                                       period = c(rep('before', 24), rep('after', 24)))

## Generate plot
plot_quantity_parallel <- ggplot(data = fitted_quantity_parallel) +
    aes(x = month,
        y = dose_total/10000000000) +
    geom_point(aes(colour = period,
                   fill = period),
               shape = 21,
               size = 5,
               alpha = 0.8) +
    geom_parallel_slopes(aes(group = period),
                         colour = '#000000',
                         se = FALSE, 
                         size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = paste0('Parallel slopes (AICc =  ', round(AICc(quantity_parallel), 2), ')'),
         y = NULL,
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_fill_tableau() +
    scale_colour_tableau() +
    theme(plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_quantity_parallel

#-- Compare models --#
# Get AIC for each model
AICc(quantity_simple)
AICc(quantity_spline)
AICc(quantity_parallel)

# Calculate difference in AIC (vs parallel slopes regression, the simplest model)
## Values < 2 indicate no meaningful information loss (Burnham & Anderson 2004)

quantity_delta_aic0 <- AICc(quantity_parallel) - AICc(quantity_parallel)
quantity_delta_aic0
quantity_delta_aic1 <- AICc(quantity_spline) - AICc(quantity_parallel)
quantity_delta_aic1
quantity_delta_aic2 <- AICc(quantity_simple) - AICc(quantity_parallel)
quantity_delta_aic2

# Relative likelihoods
rl0 <- exp(-1/2 * quantity_delta_aic0)
rl0

rl1 <- exp(-1/2 * quantity_delta_aic1)
rl1

rl2 <- exp(-1/2 * quantity_delta_aic2)
rl2

# Get data
vec_rl <- c(rl0, rl1, rl2)

vec_names <- c('parallel v parallel', 'parallel v spline', 'parallel vs simple')

# Evidence ratio 
evidence_ratio(x = vec_rel,
               y = vec_names)

# Evidence weights
evidence_weight(x = vec_rl,
                y = vec_names)

######################################
# Monthly dose per prescription item #
######################################

#-- Simple quantile regression (tau = 0.5, knots = 0) --#
# Generate model
dose_simple <- rq(dose_per_prescription_total ~ month, 
                  data = data)

# Inspect model coefficients
tidy(dose_simple)

# Get fitted values
fitted_dose_simple <- data.frame(month = 1:48,
                                 dose_total = data$dose_per_prescription_total,
                                 .fitted = predict(dose_simple),
                                 period = c(rep('before', 24), rep('after', 24)))

## Generate plot
plot_dose_simple <- ggplot(data = fitted_dose_simple) +
    aes(x = month,
        y = dose_total/1000) +
    geom_point(aes(colour = period,
                   fill = period),
               shape = 21,
               size = 5,
               alpha = 0.8) +
    geom_line(aes(y = .fitted/1000),
              colour = '#000000',
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = paste0('Simple (AICc =  ', round(AICc(dose_simple), 2), ')'),
         y = expression('Dose per prescription item (10'^3*' mg)'),
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_fill_tableau() +
    scale_colour_tableau() +
    theme(plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_dose_simple

#-- Interrupted quantile regression (tau = 0.5, knots = 1) --#
# Generate model
dose_spline <- rq(dose_per_prescription_total ~ lspline(x = month,
                                                        knots = 25,
                                                        marginal = FALSE), 
                  data = data)

# Inspect model coefficients
tidy(dose_spline)

# Get fitted values
fitted_dose_spline <- data.frame(month = 1:48,
                                 dose_total = data$dose_per_prescription_total,
                                 .fitted = predict(dose_spline),
                                 period = c(rep('before', 24), rep('after', 24)))

## Generate plot
plot_dose_spline <- ggplot(data = fitted_dose_spline) +
    aes(x = month,
        y = dose_total/1000) +
    geom_point(aes(colour = period,
                   fill = period),
               shape = 21,
               size = 5,
               alpha = 0.8) +
    geom_line(aes(y = .fitted/1000),
              colour = '#000000',
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = paste0('Spline (knots = 1) (AICc =  ', round(AICc(dose_spline), 2), ')'),
         y = expression('Dose per prescription item (10'^3*' mg)'),
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_fill_tableau() +
    scale_colour_tableau() +
    theme(plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_dose_spline 

#-- Simple robust regression (knots = 0): parallel slopes --#
# Generate model
dose_parallel <- rq(dose_per_prescription_total ~ month + period,
                    data = data)

# Inspect model coefficients
tidy(dose_parallel)

# Get fitted values
fitted_dose_parallel <- data.frame(month = 1:48,
                                   dose_total = data$dose_per_prescription_total,
                                   .fitted = predict(dose_parallel),
                                   period = c(rep('before', 24), rep('after', 24)))

## Generate plot
plot_dose_parallel <- ggplot(data = fitted_dose_parallel) +
    aes(x = month,
        y = dose_total/1000) +
    geom_point(aes(colour = period,
                   fill = period),
               shape = 21,
               size = 5,
               alpha = 0.8) +
    geom_parallel_slopes(aes(group = period),
                         colour = '#000000',
                         se = FALSE, 
                         size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = paste0('Parallel slopes (AICc =  ', round(AICc(dose_parallel), 2), ')'),
         y = expression('Dose per prescription item (10'^3*' mg)'),
         x = 'Date') +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    scale_fill_tableau() +
    scale_colour_tableau() +
    theme(plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_dose_parallel

#-- Compare models --#
# Get AIC for each model
AICc(dose_simple)
AICc(dose_spline)
AICc(dose_parallel)

# Calculate difference in AIC (vs parallel slopes regression, the simplest model)
## Values < 2 indicate no meaningful information loss (Burnham & Anderson 2004)

dose_delta_aic0 <- AICc(dose_parallel) - AICc(dose_parallel)
dose_delta_aic0
dose_delta_aic1 <- AICc(dose_spline) - AICc(dose_parallel)
dose_delta_aic1
dose_delta_aic2 <- AICc(dose_simple) - AICc(dose_parallel)
dose_delta_aic2

# Relative likelihoods
rl0 <- exp(-1/2 * dose_delta_aic0)
rl0

rl1 <- exp(-1/2 * dose_delta_aic1)
rl1

rl2 <- exp(-1/2 * dose_delta_aic2)
rl2

# Get data
vec_rl <- c(rl0, rl1, rl2)

vec_names <- c('parallel v parallel', 'parallel v spline', 'parallel vs simple')

# Evidence ratio 
evidence_ratio(x = vec_rel,
               y = vec_names)

# Evidence weights
evidence_weight(x = vec_rl,
                y = vec_names)

########################
#   Publication plot   #
########################

#-- Process figures for plotting --#
# Monthly number of prescription items
plot_prescriptions_simple2 <- plot_prescriptions_simple +
    theme(axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

plot_prescriptions_spline2 <- plot_prescriptions_spline +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

plot_prescriptions_parallel2 <- plot_prescriptions_parallel +
    theme(axis.title.y = element_blank())

# Monthly dose per prescription item
plot_dose_simple2 <- plot_dose_simple +
    theme(axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

plot_dose_spline2 <- plot_dose_spline +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())

plot_dose_parallel2 <- plot_dose_parallel +
    theme(axis.title.y = element_blank())

#-- Combined plot --#
publication_plot <- plot_prescriptions_simple2 +
    plot_prescriptions_spline2 +
    plot_prescriptions_parallel2 + 
    plot_dose_simple2 +
    plot_dose_spline2 +
    plot_dose_parallel2 +
    plot_layout(ncol = 2, byrow = FALSE) +
    plot_annotation(tag_levels = 'A')

ggsave('figures/publication-plot_gabapentin.png',
       plot = publication_plot,
       height = 15,
       width = 16)
