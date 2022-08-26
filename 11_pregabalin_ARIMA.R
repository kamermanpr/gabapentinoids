############################################################
#                                                          #
#              ARIMA modelling for pregabalin              #
#                                                          #
############################################################

#-- Load packages --#
library(readr)
library(dplyr)
library(tseries)
library(forecast)
library(ggplot2)
library(ggthemes)
library(patchwork)

#-- Import data --#
pregabalin <- read_csv('data-clean/pregabalin_analysis-set.csv')

head(pregabalin$date, 1)

tail(pregabalin$date, 1)

#-- Process data --#
data <- pregabalin |> 
    # Select columns
    select(month, 
           prescriptions_total, 
           pills_total, 
           dose_total,
           dose_per_prescription_total) 

########################################
# Monthly number of prescription items #
########################################
#-- Generate time series --#
ts_prescription <- ts(data$prescriptions_total, 
                      frequency = 12, 
                      start = c(2017, 4),
                      end = c(2021, 3))

#-- Plot time series --#
plot(ts_prescription)

#-- Check whether stationary --#
# Significant outcome indicates stationary time series
# Augmented Dickey-Fuller Test
adf.test(ts_prescription) 

#-- Check whether seasonal --#
# ETS interpretation: Error, Trend, Seasonal components
# Looking to see whether Seasonal component is: 
# 'None; N', 'Additive; A', or 'Multiplicative; M'
ets(ts_prescription)

# Double-check seasonality with ACF and PACF
acf(ts_prescription, lag.max = 24)

pacf(ts_prescription, lag.max = 24)

#-- Construct variables for ramp and step change at April 2019 --#
# Step
stepped <- c(rep(x = 0, times = 24), 
             rep(x = 1, times = 24))

# Ramp
ramped <- c(rep(x = 0, times = 24), 
            seq(from = 1, to = 24))

#-- Search for best model --#
# Auto build model
mod_prescription_auto <- auto.arima(y = ts_prescription,
                                    xreg = cbind(stepped, ramped), # predictors of y
                                    stationary = FALSE, # Based on ADF test
                                    seasonal = TRUE, # A lot of auto-correlation
                                    stepwise = FALSE,
                                    trace = TRUE)

# Check residuals
checkresiduals(mod_prescription_auto, lag = 24)

#-- Final step --#
# Build model pre-April 2019 using seasonal and non-seasonal components identified in automated search
mod_prescription_final <- Arima(window(ts_prescription, end = c(2019, 3)), 
                                order = c(2, 1, 0), 
                                seasonal = list(order = c(1, 0, 0), 
                                                period = 12),
                                method = 'ML')

# Forecast values >= April 2019
forecast_prescription <- forecast(mod_prescription_final, 
                                  h = 24)

# Quick look
plot(ts.union(ts_prescription, forecast_prescription$mean), 
     type = 'l', plot.type = 'single',
     col = c('blue', 'red'),
     xlab = 'Year', ylab = 'Number of prescription items')

# Add to dataframe
data_2 <- data |> 
    mutate(prescriptions_total_forecast_mean = c(rep(NA, 24), forecast_prescription$mean),
           prescriptions_total_forecast_lower95 = c(rep(NA, 24), forecast_prescription$lower[,2]),
           prescriptions_total_forecast_upper95 = c(rep(NA, 24), forecast_prescription$upper[,2]))

#######################################
# Number of pills dispensed per month #
#######################################
#-- Generate time series --#
ts_pills <- ts(data$pills_total, 
               frequency = 12,
               start = c(2017, 4),
               end = c(2021, 3))

#-- Plot time series --#
plot(ts_pills)

#-- Check whether stationary --#
# Significant outcome indicates stationary time series
# Augmented Dickey-Fuller Test
adf.test(ts_pills) 

#-- Check whether seasonal --#
# ETS interpretation: Error, Trend, Seasonal components
# Looking to see whether Seasonal component is: 
# 'None; N', 'Additive; A', or 'Multiplicative; M'
ets(ts_pills)

# Double-check seasonality with ACF and PACF
acf(ts_pills, lag.max = 24)

pacf(ts_pills, lag.max = 24)

#-- Construct variables for ramp and step change at April 2019 --#
# See 'Monthly number of prescription items' above

#-- Search for best model --#
# Auto build model
mod_pills_auto <- auto.arima(y = ts_pills,
                             xreg = cbind(stepped, ramped), # predictors of y
                             stationary = FALSE, # Based on ADF test
                             seasonal = TRUE, # Possible seasonal component based on ACF
                             stepwise = FALSE,
                             trace = TRUE)

# Check residuals
checkresiduals(mod_pills_auto, lag = 24)

#-- Final step --#
# Build model pre-April 2019 using seasonal and non-seasonal components identified in automated search
mod_pills_final <- Arima(window(ts_pills, end = c(2019, 3)), 
                         order = c(2, 1, 0), 
                         seasonal = list(order = c(1, 0, 0), 
                                         period = 12),
                         method = 'ML')

# Forecast values >= April 2019
forecast_pills <- forecast(mod_pills_final, 
                           h = 24)

# Quick look
plot(ts.union(ts_pills, forecast_pills$mean), 
     type = 'l', plot.type = 'single',
     col = c('blue', 'red'),
     xlab = 'Year', ylab = 'Number of pills dispensed')

# Add to dataframe
data_3 <- data_2 |> 
    mutate(pills_total_forecast_mean = c(rep(NA, 24), forecast_pills$mean),
           pills_total_forecast_lower95 = c(rep(NA, 24), forecast_pills$lower[,2]),
           pills_total_forecast_upper95 = c(rep(NA, 24), forecast_pills$upper[,2]))

#################################
# Monthly total dose prescribed #
#################################
#-- Generate time series --#
ts_dose <- ts(data$dose_total/1e6, # Divide by 1,000,000 otherwise auto.arima cannot find a solution
              frequency = 12,
              start = c(2017, 4),
              end = c(2021, 3))

#-- Plot time series --#
plot(ts_dose, 
     ylab = 'dose (1e6)')

#-- Check whether stationary --#
# Significant outcome indicates stationary time series
# Augmented Dickey-Fuller Test
adf.test(ts_dose) 

#-- Check whether seasonal --#
# ETS interpretation: Error, Trend, Seasonal components
# Looking to see whether Seasonal component is: 
# 'None; N', 'Additive; A', or 'Multiplicative; M'
ets(ts_dose)

# Double-check seasonality with ACF and PACF
acf(ts_dose, lag.max = 24)

pacf(ts_dose, lag.max = 24)

#-- Construct variables for ramp and step change at April 2019 --#
# See 'Monthly number of prescription items' above

#-- Search for best model --#
# Auto build model
mod_dose_auto <- auto.arima(y = ts_dose,
                            xreg = cbind(ramped, stepped), # predictors of y
                            stationary = FALSE, # Based on ADF test
                            seasonal = TRUE, # Possible seasonal component based on ACF
                            stepwise = FALSE,
                            trace = TRUE)
# Check residuals
checkresiduals(mod_dose_auto, lag = 24)

#-- Final step --#
# Build model pre-April 2019 using seasonal and non-seasonal components identified in automated search
mod_dose_final <- Arima(window(ts_dose, end = c(2019, 3)), 
                         order = c(3, 0, 0), 
                         seasonal = list(order = c(1, 0, 0), 
                                         period = 12),
                        method = 'ML')

# Forecast values >= April 2019
forecast_dose <- forecast(mod_dose_final, 
                          h = 24)

# Quick look
plot(ts.union(ts_dose, forecast_dose$mean), 
     type = 'l', plot.type = 'single',
     col = c('blue', 'red'),
     xlab = 'Year', ylab = 'Total monthly dose dispensed (1e6)')

# Add to dataframe
data_4 <- data_3 |> 
    # 'Correct' dose back to original raw scale
    mutate(dose_total_forecast_mean = c(rep(NA, 24), (forecast_dose$mean * 1e6)),
           dose_total_forecast_lower95 = c(rep(NA, 24), (forecast_dose$lower[,2] * 1e6)),
           dose_total_forecast_upper95 = c(rep(NA, 24), (forecast_dose$upper[,2] * 1e6)))

######################################
# Monthly dose per prescription item #
######################################
#-- Generate time series --#
ts_dose2 <- ts(data$dose_per_prescription_total,
               frequency = 12,
               start = c(2017, 4),
               end = c(2021, 3))

#-- Plot time series --#
plot(ts_dose2)

#-- Check whether stationary --#
# Significant outcome indicates stationary time series
# Augmented Dickey-Fuller Test
adf.test(ts_dose2) 

#-- Check whether seasonal --#
# ETS interpretation: Error, Trend, Seasonal components
# Looking to see whether Seasonal component is: 
# 'None; N', 'Additive; A', or 'Multiplicative; M'
ets(ts_dose2)

# Double-check seasonality with ACF and PACF
acf(ts_dose2, lag.max = 24)

pacf(ts_dose2, lag.max = 24)

#-- Construct variables for ramp and step change at April 2019 --#
# See 'Monthly number of prescription items' above

#-- Search for best model --#
# Auto build model
mod_dose2_auto <- auto.arima(y = ts_dose2,
                             xreg = cbind(ramped, stepped), # predictors of y
                             stationary = FALSE, # Based on ADF test
                             seasonal = TRUE, # Possible seasonal component based on ACF
                             stepwise = FALSE,
                             trace = TRUE)
# Check residuals
checkresiduals(mod_dose2_auto, lag = 24)

#-- Final step --#
# Build model pre-April 2019 using seasonal and non-seasonal components identified in automated search
mod_dose2_final <- Arima(window(ts_dose2, end = c(2019, 3)), 
                          order = c(0, 1, 1), 
                          seasonal = list(order = c(0, 0, 0), 
                                          period = 12),
                         method = 'ML')

# Forecast values >= April 2019
forecast_dose2 <- forecast(mod_dose2_final, 
                           h = 24)

# Quick look
plot(ts.union(ts_dose2, forecast_dose2$mean), 
     type = 'l', plot.type = 'single',
     col = c('blue', 'red'),
     xlab = 'Year', ylab = 'Total monthly dose per prescription item')

# Add to dataframe
data_5 <- data_4 |> 
    # 'Correct' dose back to original raw scale
    mutate(dose_per_prescription_total_forecast_mean = c(rep(NA, 24), (forecast_dose2$mean)),
           dose_per_prescription_total_forecast_lower95 = c(rep(NA, 24), (forecast_dose2$lower[,2])),
           dose_per_prescription_total_forecast_upper95 = c(rep(NA, 24), (forecast_dose2$upper[,2])))

####################
# Publication plot #
####################

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

#--- Palette ---#
arima_col <- tableau_color_pal()(2)[[2]]

#--- Generate plots ---#
# Prescription items
plot_prescriptions <- ggplot(data = data_5) +
    aes(x = month) +
    geom_ribbon(aes(ymax = prescriptions_total_forecast_upper95/10000,
                    ymin = prescriptions_total_forecast_lower95/10000),
              fill = arima_col,
              alpha = 0.4) +
    geom_line(aes(y = prescriptions_total/10000),
              colour = '#000000') +
    geom_line(aes(y = prescriptions_total_forecast_mean/10000),
              colour = arima_col,
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = expression('ARIMA(2,1,0)(1,0,0)'[12]),
         y = expression('Number of prescription items (10'^4*')'),
         x = 'Date') +
    scale_y_continuous(limits = c(40, 80)) +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    theme(plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_prescriptions

# Monthly dose per prescription item
plot_dose <- ggplot(data = data_5) +
    aes(x = month) +
    geom_ribbon(aes(ymax = dose_per_prescription_total_forecast_upper95/1000,
                    ymin = dose_per_prescription_total_forecast_lower95/1000),
              fill = arima_col,
              alpha = 0.4) +
    geom_line(aes(y = dose_per_prescription_total/1000),
              colour = '#000000') +
    geom_line(aes(y = dose_per_prescription_total_forecast_mean/1000),
              colour = arima_col,
              size = 1) +
    geom_vline(xintercept = 25,
               linetype = 2) +
    labs(title = expression('ARIMA(0,1,1)(0,0,0)'[12]),
         y = expression('Dose per prescription item (10'^3*' mg)'),
         x = 'Date') +
    scale_y_continuous(limits = c(6, 6.5)) +
    scale_x_continuous(breaks = c(1, 13, 25, 37, 49),
                       labels = c('April 2017', 'April 2018', 
                                  'April 2019', 
                                  'April 2020', 'April 2021')) +
    theme(plot.margin = margin(t = 5.5, r = 50, b = 5.5, l = 20)); plot_dose

# Plots without x-axis text
# plot_prescriptions_2 <- plot_prescriptions +
#     theme(axis.text.x = element_blank(),
#           axis.title.x = element_blank())
# 
# plot_dose_2 <- plot_dose +
#     theme(axis.text.x = element_blank(),
#           axis.title.x = element_blank())

plot_patchwork <- plot_prescriptions + plot_dose + plot_layout(ncol = 2)

ggsave(plot = plot_patchwork,
       filename = 'figures/pregabalin_arima.png',
       height = 6,
       width = 16)
