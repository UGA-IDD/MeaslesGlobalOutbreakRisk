
#### create monthly interpolated dataset -- constant
library(data.table)
library(tidyr)
library(dplyr)

df_raw_covariates <- fread(paste0('~/Google Drive/My Drive/Gates_MeaslesForecast/updates_jhu/df_raw_covariates_20260310.csv'))

#### fix mcv2
df_raw_covariates$mcv2 <- as.numeric(df_raw_covariates$mcv2)
df_raw_covariates$mcv2 <- ifelse(is.na(df_raw_covariates$mcv2) & df_raw_covariates$month == 1 & df_raw_covariates$year < 2024, 0, df_raw_covariates$mcv2)
# df_raw_covariates$mcv2 <- ifelse(df_raw_covariates$month == 1 & df_raw_covariates$year == 2024 & df_raw_covariates$iso3 == 'PSE', NA, df_raw_covariates$mcv2)

#### ----------------------------------------------------------------------------------------------------------------------------
# do nothing / already monthly!

# "average_age_suspected_cases", "covid_months", "extreme_precipitation_high_2SD"  "extreme_precipitation_high_3SD"
# "extreme_precipitation_low_2SD", "extreme_precipitation_low_3SD", "extreme_temperature_high_2SD", "extreme_temperature_high_3SD"
# "extreme_temperature_low_2SD", "extreme_temperature_low_3SD", "igm_negative_cases", "measles_igm_positive_cases",
# "mean_precipitation", "mean_temperature", "proportion_igm_measles_pos", "proportion_igm_mr_neg", "proportion_igm_rubella_pos",
# "reported_cases", "rubella_igm_positive_cases", "school_term", "suspected_cases", "total_precipitation",
# "sia_coverage_under5,"sia_coverage_under5_no_outbreak", "sia_coverage_under5_only_outbreak",
# "sia_coverage_under15,"sia_coverage_under15_no_outbreak", "sia_coverage_under15_only_outbreak"
# "Ii" "p_value_adjusted" "cluster_type" "latitude" "longitude", "seroprevalence_under5s", "seroprevalence_over5s", 
# "radial_mean_month", "radial_sd_month"

df_raw_covariates$mcv1_annual <- NULL
df_raw_covariates$mcv2_annual <- NULL


#### ----------------------------------------------------------------------------------------------------------------------------
#### define and apply function to carry over values -- i.e., fill in month 1 through months 2-12
carry_values <- function(df, columns_to_fill) {
  # Ensure the data is sorted by iso3, year, and month
  df <- df[order(df$iso3, df$year, df$month), ]
  
  # Group the data by iso3 (country) and year
  df <- df %>%
    group_by(iso3, year) %>%
    # Apply the carry-over logic only to the specified columns
    mutate(across(all_of(columns_to_fill), ~ ifelse(month == 1, ., NA))) %>%
    fill(all_of(columns_to_fill), .direction = "down") %>%
    ungroup()
  
  return(df)
}

df_interpolate_constant <- carry_values(df_raw_covariates, c('district_stockout', 'estimated_infections', 'estimated_infections_minor_leakage', 'gini', 
                                                             'mcv1_adm1_range', 'mcv2_adm1_range', 'mcv2_intro', 'national_stockout', 
                                                             'prop_estimated_infections_under10', 'prop_estimated_infections_under5', 
                                                             'prop_susceptible_under10', 'prop_susceptible_under5', 'rcv_intro', 'school_based_immunization', 
                                                             'stockout_duration', 'vitA_campaign', 'vitA_routine'))

## account for annual sums that need to be dispersed over months
df_interpolate_constant$estimated_infections <- df_interpolate_constant$estimated_infections / 12
df_interpolate_constant$estimated_infections_minor_leakage <- df_interpolate_constant$estimated_infections_minor_leakage / 12


#### ----------------------------------------------------------------------------------------------------------------------------
#### define and apply functions to 1) shift values to july and 2) linearly interpolate over year (july to july)

shift_values_to_midyear <- function(df, columns_to_fill) {
  # Ensure data is sorted by country, year, and month
  df <- df[order(df$iso3, df$year, df$month), ]
  
  df <- df %>%
    group_by(iso3) %>%  # Group only by country to interpolate across years
    mutate(across(all_of(columns_to_fill), 
                  ~ ifelse(month == 1, ., NA))) %>%  # Keep only January values
    mutate(across(all_of(columns_to_fill), 
                  ~ ifelse(month == 7, lag(., 6), .))) %>%  # Move values to July
    mutate(across(all_of(columns_to_fill), 
                  ~ ifelse(month == 1, NA, .))) %>%  # Delete values in month 1
    ungroup()
  
  return(df)
}


df_interpolate_constant_shift <- shift_values_to_midyear(df_interpolate_constant, c('av_age', 'battle_mr', 'birth_rate', 'death_rate', 'gdp_pc', 'hdi', 'health_expenditure', 
                                                                              'hiv', 'idp', 'land_dispute_index', 'malaria', 'mcv1', 'mcv2', 'net_migration', 
                                                                              'pop_density', 'pop_size', 'position_canonical_path', 'position_canonical_path_no_leakage',
                                                                              'proportion_under10', 'proportion_under5', 'stunting', 'u5mr', 'underweight', 'vitA_daly_rate', 
                                                                              'vitA_death_rate', 'wasting'))


interpolate_df <- function(df, country_col, year_col, month_col, cols_to_interp) {
  # Load necessary library
  library(dplyr)

  # Define the interpolation function
  interpolate_na <- function(x) {
    valid_idx <- which(!is.na(x))
    if (length(valid_idx) == 0) return(x)  # If all values are NA, return as is

    # If there is only one valid value for the country, hold it constant
    if (length(valid_idx) == 1) {
      return(rep(x[valid_idx], length(x)))  # Replicate the single valid value for all NA slots
    }

    # Perform linear interpolation between known points
    interpolated_values <- approx(valid_idx, x[valid_idx], xout = seq_along(x))$y

    # Extend the linear trend beyond the last valid value (extrapolation)
    if (length(valid_idx) > 1) {
      last_valid_idx <- valid_idx[length(valid_idx)]
      second_last_valid_idx <- valid_idx[length(valid_idx) - 1]

      slope <- (x[last_valid_idx] - x[second_last_valid_idx]) / (last_valid_idx - second_last_valid_idx)

      # Extrapolate beyond the last valid point
      for (i in (last_valid_idx + 1):length(x)) {
        interpolated_values[i] <- interpolated_values[last_valid_idx] + slope * (i - last_valid_idx)
      }
    }

    return(interpolated_values)
  }

  # Convert character columns to numeric (force coercion to numeric)
  df <- df %>%
    mutate(across(all_of(cols_to_interp), ~ as.numeric(as.character(.))))

  # Ensure data is sorted by country, year, and month
  df_sorted <- df %>%
    arrange(.data[[country_col]], .data[[year_col]], .data[[month_col]])  # Sorting by country, year, and month

  # Apply interpolation to each country and each specified column
  df_interpolated <- df_sorted %>%
    group_by(.data[[country_col]]) %>%
    mutate(across(all_of(cols_to_interp), interpolate_na)) %>%
    ungroup()

  return(df_interpolated)
}

df_interpolate_constant_shift$mcv2 <- as.numeric(df_interpolate_constant_shift$mcv2)
# df_interpolate_constant_shift$battle_mr <- as.numeric(df_interpolate_constant_shift$battle_mr)
df_interpolate_constant_shift_linear <- interpolate_df(df_interpolate_constant_shift, 'iso3', 'year', 'month', c('av_age', 'battle_mr', 'birth_rate', 'death_rate', 'gdp_pc', 'hdi', 'health_expenditure', 
                                                         'hiv', 'idp', 'land_dispute_index', 'malaria', 'mcv1', 'mcv2', 'net_migration', 
                                                         'pop_density', 'pop_size', 'position_canonical_path', 'position_canonical_path_no_leakage',
                                                         'proportion_under10', 'proportion_under5', 'stunting', 'u5mr', 'underweight', 'vitA_daly_rate', 
                                                         'vitA_death_rate', 'wasting'))


#### ------------------------------------------------------------------------------------------
#### define lagging function

lag_monthly_data <- function(df, iso3_col, year_col, month_col, value_cols, months_forward) {
  df <- df %>%
    mutate(time_index = .data[[year_col]] * 12 + .data[[month_col]]) %>%
    arrange(.data[[iso3_col]], time_index) %>%  # Ensure sorting by country and time
    group_by(.data[[iso3_col]]) %>%  # Group by country
    
    # Apply lag within each country group and overwrite original columns
    mutate(across(all_of(value_cols), ~ lag(.x, months_forward))) %>%
    
    ungroup() %>%
    select(-time_index)  # Remove temporary column
  
  return(df)
}

# 
# 
# df_interpolate_constant_shift_linear <- data.table(df_interpolate_constant_shift_linear)
# ### add extra rows to be able to carry forward through 2025-2026
# new_months <- unique(subset(df_interpolate_constant_shift_linear, select = c(iso3, month)))
# 
# new_months_2025 <- copy(new_months)
# new_months_2025$year <- 2025
# 
# new_months_2026 <- copy(new_months)
# new_months_2026$year <- 2026
# 
# df_interpolate_constant_shift_linear <- rbind(df_interpolate_constant_shift_linear, new_months_2025, fill = T)
# df_interpolate_constant_shift_linear <- rbind(df_interpolate_constant_shift_linear, new_months_2026, fill = T)


df_interpolate_constant_shift_linear <- data.table(df_interpolate_constant_shift_linear)

# Get all unique iso3 values
all_iso3 <- unique(df_interpolate_constant_shift_linear$iso3)

# Find missing months in 2025 (months 1-12 not already present)
existing_2025 <- unique(subset(df_interpolate_constant_shift_linear[year == 2025], select = c(iso3, month)))
all_2025 <- CJ(iso3 = all_iso3, month = 1:12, year = 2025)
missing_2025 <- all_2025[!existing_2025, on = c("iso3", "month")]

# Add all months in 2026
new_months_2026 <- CJ(iso3 = all_iso3, month = 1:12, year = 2026)

df_interpolate_constant_shift_linear <- rbind(df_interpolate_constant_shift_linear, missing_2025, fill = TRUE)
df_interpolate_constant_shift_linear <- rbind(df_interpolate_constant_shift_linear, new_months_2026, fill = TRUE)



#### ------------------------------------------------------------------------------------------
#### IVB and dependent

ivb_list <- c("average_age_suspected_cases", "igm_negative_cases", "measles_igm_positive_cases", "proportion_igm_measles_pos", 
              "proportion_igm_mr_neg", "proportion_igm_rubella_pos", 
              "rubella_igm_positive_cases", "suspected_cases", 
              "radial_mean_month", "radial_sd_month",
              "cv_cases", "case_range_adm1")

ivb_dependent_list <- c("estimated_infections", "estimated_infections_minor_leakage", "prop_estimated_infections_under10",
                        "prop_estimated_infections_under5", "prop_susceptible_under10", "prop_susceptible_under5",  
                        "position_canonical_path", "position_canonical_path_no_leakage")


#### IVB lag
ivb_lag <- 12
df_interpolate_constant_shift_linear_lag <- lag_monthly_data(df_interpolate_constant_shift_linear, "iso3","year", "month", 
                                                             ivb_list, ivb_lag)

#### IVB-dependent lag
df_interpolate_constant_shift_linear_lag <- lag_monthly_data(df_interpolate_constant_shift_linear_lag, "iso3","year", "month", 
                                                             ivb_dependent_list, ivb_lag + 5)





#### ------------------------------------------------------------------------------------------
#### non-IVB dependent

#### 0 month lag
# c("av_age", "covid_months", "pop_density", "pop_size", 
# "proportion_of_birth_cohort_suscpetible", "proportion_under10", "proportion_under5", "sia_size_larger_birth_cohort",
# "school_term", "birth_rate", "net_migration", "u5mr", "death_rate", 
#  "latitude" "longitude", "region", "sia_coverage_under5", "outbreak_reponse_SIA", "outbreak_mode"

#### 1 month lag
df_interpolate_constant_shift_linear_lag <- lag_monthly_data(df_interpolate_constant_shift_linear_lag, "iso3","year", "month", 
                                                             c("extreme_precipitation_high_2SD", "extreme_precipitation_high_3SD", 
                                                               "extreme_precipitation_low_2SD", "extreme_precipitation_low_3SD", "extreme_temperature_high_2SD", 
                                                               "extreme_temperature_high_3SD", "extreme_temperature_low_2SD", "extreme_temperature_low_3SD", 
                                                               "mean_precipitation", "mean_temperature", "total_precipitation"), 1)

#### 6 month lag 
df_interpolate_constant_shift_linear_lag <- lag_monthly_data(df_interpolate_constant_shift_linear_lag, "iso3","year", "month", 
                                                             c("battle_mr", "district_stockout", "gdp_pc", "hdi", "hiv", "health_expenditure", "land_dispute_index",
                                                               "idp", "malaria", "mcv1",  "mcv2", "mcv2_intro", "national_stockout", "rcv_intro", "school_based_immunization", 
                                                               "stockout_duration", "stunting", "underweight", "vitA_campaign", "vitA_routine","wasting", 
                                                               "seroprevalence_under5s", "seroprevalence_over5s"), 6)

#### 18 month lag
df_interpolate_constant_shift_linear_lag <- lag_monthly_data(df_interpolate_constant_shift_linear_lag, "iso3","year", "month", 
                                                             c("gini", "mcv1_adm1_range", "mcv2_adm1_range"), 18)

#### 24 month lag
df_interpolate_constant_shift_linear_lag <- lag_monthly_data(df_interpolate_constant_shift_linear_lag, "iso3","year", "month", 
                                                             c("vitA_daly_rate", "vitA_death_rate"), 24)



#### ------------------------------------------------------------------------------------------
#### save

#fwrite(df_interpolate_constant_shift_linear, file = paste0('~/Google Drive/My Drive/Gates_MeaslesForecast/updates_jhu/df_interpolate_linear_20250808.csv'))
fwrite(df_interpolate_constant_shift_linear_lag, file = paste0('~/Google Drive/My Drive/Gates_MeaslesForecast/updates_jhu/df_interpolate_linear_lag_20260310_ivb_lag_sensitivity_',ivb_lag,'.csv'))



 