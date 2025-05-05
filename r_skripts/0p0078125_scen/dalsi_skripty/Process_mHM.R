rm(list = ls())

library(ncdf4)
library(terra)

# TWS is total water storage computed as
# cdo -expr,'tws = interception + snowpack + SWC_L01 + SWC_L02 + SWC_L03 + SWC_L04 + SWC_L05 + SWC_L06 + unsatSTW + satSTW +sealedSTW' ./mHM_Fluxes_States.nc mHM_TWS.nc

mHM_2030 <- nc_open("./2030_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc")

# List all the variables
names(mHM_2030$var)
nc_close(mHM_2030)

load_nc_var <- function(nc_path, varname) {
  srcs <- sources(rast(nc_path))
  src_var <- srcs[grepl(paste0(":", varname, "$"), srcs)]
  rast(src_var)
}

time_sel <- function(rast_obj, start_date, end_date) {
  times <- as.Date(time(rast_obj))
  sel <- which(times >= start_date & times <= end_date)
  
  # Number of years (based on months divided by 12)
  n_y <- length(unique(format(times[sel], "%Y-%m"))) / 12
  
  # Subset the raster
  rast_sel <- rast_obj[[sel]]
  
  return(list(rast_sel = rast_sel, n_y = n_y))
}

daily_to_monthly_sum <- function(rast_daily) {
  # 1. Extract times
  times <- as.Date(time(rast_daily))
  
  # 2. Extract year from times
  years <- format(times, "%Y")
  unique_years <- unique(years)
  
  # 3. Initialize empty list
  monthly_list <- list()
  
  # 4. Loop over each year
  for (y in unique_years) {
    message("Processing year: ", y)
    
    # Subset the layers for this year
    idx <- which(years == y)
    rast_year <- subset(rast_daily, idx)
    
    # Inside year: group into year-month
    year_month <- format(as.Date(time(rast_year)), "%Y-%m")
    group_index <- as.numeric(factor(year_month))
    
    # Sum daily into monthly
    monthly_sum <- tapp(rast_year, group_index, fun = sum)
    
    # Correct time: last day of month
    unique_months <- unique(year_month)
    end_of_month_dates <- as.Date(format(as.Date(paste0(unique_months, "-01")) + 31, "%Y-%m-01")) - 1
    time(monthly_sum) <- end_of_month_dates
    
    # Save into list
    monthly_list[[y]] <- monthly_sum
  }
  
  # 5. Merge all years back into a single SpatRaster
  result <- do.call(c, unname(monthly_list))
  
  return(result)
}

# Raw data load
mHM_baseline_aET_raw <- load_nc_var("./mHM_Fluxes_States.nc", "aET")
mHM_baseline_Q_raw <- load_nc_var("./mHM_Fluxes_States.nc", "Q")
mHM_baseline_PET_raw <- load_nc_var("./mHM_Fluxes_States.nc", "PET")
mHM_baseline_P_raw <- rast("./pre_1961-2024_compressed.nc")
mHM_baseline_TWS_raw <- rast("./mHM_TWS.nc")

mHM_2030_aET_raw <- load_nc_var("./2030_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc", "aET")
mHM_2030_Q_raw <- load_nc_var("./2030_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc", "Q")
mHM_2030_PET_raw <- load_nc_var("./2030_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc", "PET")
mHM_2030_P_raw <- rast("./pre_2030_SSP245_MPI-ESM1-2-HR_compressed.nc")
mHM_2030_TWS_raw <- rast("./2030_SSP245_MPI-ESM1-2-HR/mHM_TWS.nc")

mHM_2050_aET_raw <- load_nc_var("./2050_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc", "aET")
mHM_2050_Q_raw <- load_nc_var("./2050_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc", "Q")
mHM_2050_PET_raw <- load_nc_var("./2050_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc", "PET")
mHM_2050_P_raw <- rast("./pre_2050_SSP245_MPI-ESM1-2-HR_compressed.nc")
mHM_2050_TWS_raw <- rast("./2050_SSP245_MPI-ESM1-2-HR/mHM_TWS.nc")

mHM_2070_aET_raw <- load_nc_var("./2070_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc", "aET")
mHM_2070_Q_raw <- load_nc_var("./2070_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc", "Q")
mHM_2070_PET_raw <- load_nc_var("./2070_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc", "PET")
mHM_2070_P_raw <- rast("./pre_2070_SSP245_MPI-ESM1-2-HR_compressed.nc")
mHM_2070_TWS_raw <- rast("./2070_SSP245_MPI-ESM1-2-HR/mHM_TWS.nc")

mHM_2085_aET_raw <- load_nc_var("./2085_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc", "aET")
mHM_2085_Q_raw <- load_nc_var("./2085_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc", "Q")
mHM_2085_PET_raw <- load_nc_var("./2085_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc", "PET")
mHM_2085_P_raw <- rast("./pre_2085_SSP245_MPI-ESM1-2-HR_compressed.nc")
mHM_2085_TWS_raw <- rast("./2085_SSP245_MPI-ESM1-2-HR/mHM_TWS.nc")

################################################################################
# Baseline

start_date <- as.Date("1980-11-01")
end_date <- as.Date("2010-10-31")

# aET
mHM_baseline_aET_pre <- time_sel(mHM_baseline_aET_raw, start_date, end_date)
mHM_baseline_aET <- mHM_baseline_aET_pre$rast_sel
mHM_baseline_aET_y <- trim(sum(mHM_baseline_aET)/mHM_baseline_aET_pre$n_y)

# Print mean over entire basin
mean(values(mHM_baseline_aET_y), na.rm = TRUE)

# Q
mHM_baseline_Q_pre <- time_sel(mHM_baseline_Q_raw, start_date, end_date)
mHM_baseline_Q <- mHM_baseline_Q_pre$rast_sel
mHM_baseline_Q_y <- trim(sum(mHM_baseline_Q)/mHM_baseline_Q_pre$n_y)

# Print mean over entire basin
mean(values(mHM_baseline_Q_y), na.rm = TRUE)

# PET
mHM_baseline_PET_pre <- time_sel(mHM_baseline_PET_raw, start_date, end_date)
mHM_baseline_PET <- mHM_baseline_PET_pre$rast_sel
mHM_baseline_PET_y <- trim(sum(mHM_baseline_PET)/mHM_baseline_PET_pre$n_y)

# Print mean over entire basin
mean(values(mHM_baseline_PET_y), na.rm = TRUE)

# P
mHM_baseline_P_pre <- time_sel(mHM_baseline_P_raw, start_date, end_date)
mHM_baseline_P_d <- mHM_baseline_P_pre$rast_sel
mHM_baseline_P <- daily_to_monthly_sum(mHM_baseline_P_d)

mHM_baseline_P_y <- sum(mHM_baseline_P)/mHM_baseline_P_pre$n_y
mHM_baseline_P_y <- mask(crop(mHM_baseline_P_y, mHM_baseline_Q_y), mHM_baseline_Q_y)

# Print mean over entire basin
mean(values(mHM_baseline_P_y), na.rm = TRUE)

# TSW
mHM_baseline_TWS_pre <- time_sel(mHM_baseline_TWS_raw, start_date, end_date)
mHM_baseline_TWS <- trim(mHM_baseline_TWS_pre$rast_sel)
mHM_baseline_TWS_delta <- (subset(mHM_baseline_TWS, nlyr(mHM_baseline_TWS)) - subset(mHM_baseline_TWS, 1))/mHM_baseline_TWS_pre$n_y

# Print mean over entire basin
mean(values(mHM_baseline_TWS_delta), na.rm = TRUE)

################################################################################
# 2030

start_date <- as.Date("2014-11-01")
end_date <- as.Date("2044-10-31")

# aET
mHM_2030_aET_pre <- time_sel(mHM_2030_aET_raw, start_date, end_date)
mHM_2030_aET <- mHM_2030_aET_pre$rast_sel
mHM_2030_aET_y <- trim(sum(mHM_2030_aET)/mHM_2030_aET_pre$n_y)

# Print mean over entire basin
mean(values(mHM_2030_aET_y), na.rm = TRUE)

# Q
mHM_2030_Q_pre <- time_sel(mHM_2030_Q_raw, start_date, end_date)
mHM_2030_Q <- mHM_2030_Q_pre$rast_sel
mHM_2030_Q_y <- trim(sum(mHM_2030_Q)/mHM_2030_Q_pre$n_y)

# Print mean over entire basin
mean(values(mHM_2030_Q_y), na.rm = TRUE)

# PET
mHM_2030_PET_pre <- time_sel(mHM_2030_PET_raw, start_date, end_date)
mHM_2030_PET <- mHM_2030_PET_pre$rast_sel
mHM_2030_PET_y <- trim(sum(mHM_2030_PET)/mHM_2030_PET_pre$n_y)

# Print mean over entire basin
mean(values(mHM_2030_PET_y), na.rm = TRUE)

# P
mHM_2030_P_pre <- time_sel(mHM_2030_P_raw, start_date, end_date)
mHM_2030_P_d <- mHM_2030_P_pre$rast_sel
mHM_2030_P <- daily_to_monthly_sum(mHM_2030_P_d)

mHM_2030_P_y <- sum(mHM_2030_P)/mHM_2030_P_pre$n_y
mHM_2030_P_y <- mask(crop(mHM_2030_P_y, mHM_2030_Q_y), mHM_2030_Q_y)

# Print mean over entire basin
mean(values(mHM_2030_P_y), na.rm = TRUE)

# TSW
mHM_2030_TWS_pre <- time_sel(mHM_2030_TWS_raw, start_date, end_date)
mHM_2030_TWS <- trim(mHM_2030_TWS_pre$rast_sel)
mHM_2030_TWS_delta <- (subset(mHM_2030_TWS, nlyr(mHM_2030_TWS)) - subset(mHM_2030_TWS, 1))/mHM_2030_TWS_pre$n_y

# Print mean over entire basin
mean(values(mHM_2030_TWS_delta), na.rm = TRUE)

################################################################################
# 2050

start_date <- as.Date("2034-11-01")
end_date <- as.Date("2064-10-31")

# aET
mHM_2050_aET_pre <- time_sel(mHM_2050_aET_raw, start_date, end_date)
mHM_2050_aET <- mHM_2050_aET_pre$rast_sel
mHM_2050_aET_y <- trim(sum(mHM_2050_aET)/mHM_2050_aET_pre$n_y)

# Print mean over entire basin
mean(values(mHM_2050_aET_y), na.rm = TRUE)

# Q
mHM_2050_Q_pre <- time_sel(mHM_2050_Q_raw, start_date, end_date)
mHM_2050_Q <- mHM_2050_Q_pre$rast_sel
mHM_2050_Q_y <- trim(sum(mHM_2050_Q)/mHM_2050_Q_pre$n_y)

# Print mean over entire basin
mean(values(mHM_2050_Q_y), na.rm = TRUE)

# PET
mHM_2050_PET_pre <- time_sel(mHM_2050_PET_raw, start_date, end_date)
mHM_2050_PET <- mHM_2050_PET_pre$rast_sel
mHM_2050_PET_y <- trim(sum(mHM_2050_PET)/mHM_2050_PET_pre$n_y)

# Print mean over entire basin
mean(values(mHM_2050_PET_y), na.rm = TRUE)

# P
mHM_2050_P_pre <- time_sel(mHM_2050_P_raw, start_date, end_date)
mHM_2050_P_d <- mHM_2050_P_pre$rast_sel
mHM_2050_P <- daily_to_monthly_sum(mHM_2050_P_d)

mHM_2050_P_y <- sum(mHM_2050_P)/mHM_2050_P_pre$n_y
mHM_2050_P_y <- mask(crop(mHM_2050_P_y, mHM_2050_Q_y), mHM_2050_Q_y)

# Print mean over entire basin
mean(values(mHM_2050_P_y), na.rm = TRUE)

# Print mean over entire basin
mean(values(mHM_2050_P_y), na.rm = TRUE)

# TSW
mHM_2050_TWS_pre <- time_sel(mHM_2050_TWS_raw, start_date, end_date)
mHM_2050_TWS <- trim(mHM_2050_TWS_pre$rast_sel)
mHM_2050_TWS_delta <- (subset(mHM_2050_TWS, nlyr(mHM_2050_TWS)) - subset(mHM_2050_TWS, 1))/mHM_2050_TWS_pre$n_y

# Print mean over entire basin
mean(values(mHM_2050_TWS_delta), na.rm = TRUE)

################################################################################
# 2070

start_date <- as.Date("2054-11-01")
end_date <- as.Date("2084-10-31")

# aET
mHM_2070_aET_pre <- time_sel(mHM_2070_aET_raw, start_date, end_date)
mHM_2070_aET <- mHM_2070_aET_pre$rast_sel
mHM_2070_aET_y <- trim(sum(mHM_2070_aET)/mHM_2070_aET_pre$n_y)

# Print mean over entire basin
mean(values(mHM_2070_aET_y), na.rm = TRUE)

# Q
mHM_2070_Q_pre <- time_sel(mHM_2070_Q_raw, start_date, end_date)
mHM_2070_Q <- mHM_2070_Q_pre$rast_sel
mHM_2070_Q_y <- trim(sum(mHM_2070_Q)/mHM_2070_Q_pre$n_y)

# Print mean over entire basin
mean(values(mHM_2070_Q_y), na.rm = TRUE)

# PET
mHM_2070_PET_pre <- time_sel(mHM_2070_PET_raw, start_date, end_date)
mHM_2070_PET <- mHM_2070_PET_pre$rast_sel
mHM_2070_PET_y <- trim(sum(mHM_2070_PET)/mHM_2070_PET_pre$n_y)

# Print mean over entire basin
mean(values(mHM_2070_PET_y), na.rm = TRUE)

# P
mHM_2070_P_pre <- time_sel(mHM_2070_P_raw, start_date, end_date)
mHM_2070_P_d <- mHM_2070_P_pre$rast_sel
mHM_2070_P <- daily_to_monthly_sum(mHM_2070_P_d)

mHM_2070_P_y <- sum(mHM_2070_P)/mHM_2070_P_pre$n_y
mHM_2070_P_y <- mask(crop(mHM_2070_P_y, mHM_2070_Q_y), mHM_2070_Q_y)

# Print mean over entire basin
mean(values(mHM_2070_P_y), na.rm = TRUE)

# TSW
mHM_2070_TWS_pre <- time_sel(mHM_2070_TWS_raw, start_date, end_date)
mHM_2070_TWS <- trim(mHM_2070_TWS_pre$rast_sel)
mHM_2070_TWS_delta <- (subset(mHM_2070_TWS, nlyr(mHM_2070_TWS)) - subset(mHM_2070_TWS, 1))/mHM_2070_TWS_pre$n_y

# Print mean over entire basin
mean(values(mHM_2070_TWS_delta), na.rm = TRUE)

################################################################################
# 2085

start_date <- as.Date("2069-11-01")
end_date <- as.Date("2099-10-31")

# aET
mHM_2085_aET_pre <- time_sel(mHM_2085_aET_raw, start_date, end_date)
mHM_2085_aET <- mHM_2085_aET_pre$rast_sel
mHM_2085_aET_y <- trim(sum(mHM_2085_aET)/mHM_2085_aET_pre$n_y)

# Print mean over entire basin
mean(values(mHM_2085_aET_y), na.rm = TRUE)

# Q
mHM_2085_Q_pre <- time_sel(mHM_2085_Q_raw, start_date, end_date)
mHM_2085_Q <- mHM_2085_Q_pre$rast_sel
mHM_2085_Q_y <- trim(sum(mHM_2085_Q)/mHM_2085_Q_pre$n_y)

# Print mean over entire basin
mean(values(mHM_2085_Q_y), na.rm = TRUE)

# PET
mHM_2085_PET_pre <- time_sel(mHM_2085_PET_raw, start_date, end_date)
mHM_2085_PET <- mHM_2085_PET_pre$rast_sel
mHM_2085_PET_y <- trim(sum(mHM_2085_PET)/mHM_2085_PET_pre$n_y)

# Print mean over entire basin
mean(values(mHM_2085_PET_y), na.rm = TRUE)

# P
mHM_2085_P_pre <- time_sel(mHM_2085_P_raw, start_date, end_date)
mHM_2085_P_d <- mHM_2085_P_pre$rast_sel
mHM_2085_P <- daily_to_monthly_sum(mHM_2085_P_d)

mHM_2085_P_y <- sum(mHM_2085_P)/mHM_2085_P_pre$n_y
mHM_2085_P_y <- mask(crop(mHM_2085_P_y, mHM_2085_Q_y), mHM_2085_Q_y)

# Print mean over entire basin
mean(values(mHM_2085_P_y), na.rm = TRUE)

# TSW
mHM_2085_TWS_pre <- time_sel(mHM_2085_TWS_raw, start_date, end_date)
mHM_2085_TWS <- trim(mHM_2085_TWS_pre$rast_sel)
mHM_2085_TWS_delta <- (subset(mHM_2085_TWS, nlyr(mHM_2085_TWS)) - subset(mHM_2085_TWS, 1))/mHM_2085_TWS_pre$n_y

# Print mean over entire basin
mean(values(mHM_2085_TWS_delta), na.rm = TRUE)

################################################################################
# Define periods
periods <- c("Baseline", "2030", "2050", "2070", "2085")

# Compute mean values directly
mean_aET <- c(
  mean(values(mHM_baseline_aET_y), na.rm = TRUE),
  mean(values(mHM_2030_aET_y), na.rm = TRUE),
  mean(values(mHM_2050_aET_y), na.rm = TRUE),
  mean(values(mHM_2070_aET_y), na.rm = TRUE),
  mean(values(mHM_2085_aET_y), na.rm = TRUE)
)

mean_Q <- c(
  mean(values(mHM_baseline_Q_y), na.rm = TRUE),
  mean(values(mHM_2030_Q_y), na.rm = TRUE),
  mean(values(mHM_2050_Q_y), na.rm = TRUE),
  mean(values(mHM_2070_Q_y), na.rm = TRUE),
  mean(values(mHM_2085_Q_y), na.rm = TRUE)
)

mean_PET<- c(
  mean(values(mHM_baseline_PET_y), na.rm = TRUE),
  mean(values(mHM_2030_PET_y), na.rm = TRUE),
  mean(values(mHM_2050_PET_y), na.rm = TRUE),
  mean(values(mHM_2070_PET_y), na.rm = TRUE),
  mean(values(mHM_2085_PET_y), na.rm = TRUE)
)

mean_P <- c(
  mean(values(mHM_baseline_P_y), na.rm = TRUE),
  mean(values(mHM_2030_P_y), na.rm = TRUE),
  mean(values(mHM_2050_P_y), na.rm = TRUE),
  mean(values(mHM_2070_P_y), na.rm = TRUE),
  mean(values(mHM_2085_P_y), na.rm = TRUE)
)

mean_TWS_delta <- c(
  mean(values(mHM_baseline_TWS_delta), na.rm = TRUE),
  mean(values(mHM_2030_TWS_delta), na.rm = TRUE),
  mean(values(mHM_2070_TWS_delta), na.rm = TRUE),
  mean(values(mHM_2070_TWS_delta), na.rm = TRUE),
  mean(values(mHM_2085_TWS_delta), na.rm = TRUE)
)


# Build the final data.frame
results_df <- data.frame(
  Period = periods,
  aET_mm_per_year = mean_aET,
  Q_mm_per_year = mean_Q,
  PET_mm_per_year = mean_PET,
  P_mm_per_year = mean_P,
  mean_TWS_delta
)

# View results
print(results_df)

write.table(results_df, "Basin_water_balance.csv", sep = ",", col.names = TRUE, row.names = FALSE)

################################################################################
# Budyko framework

# Aridity index
results_df$AI <- with(results_df, PET_mm_per_year/P_mm_per_year)

# Evaporative index
results_df$EI <- with(results_df, aET_mm_per_year/P_mm_per_year)

plot(results_df$AI, results_df$EI, xlim = c(0, 1.5), ylim =c(0, 1))

x <- seq(0, 3, 0.01)

# Modified (refitted with mHM simulations) after Fischer et al. (2023) Attributing the drivers of runoff decline in the Thaya river basin
n <- 2.749395
Choudhury_EI <- 1/(1+(1/x)^n)^(1/n)

lines(x, Choudhury_EI)

results_df$aET_mm_per_year_Budyko_estimate <- with(results_df, 1/(1+(1/AI)^n)^(1/n)*P_mm_per_year)
results_df$Q_mm_per_year_Budyko_estimate <- with(results_df, P_mm_per_year - aET_mm_per_year_Budyko_estimate)

# View results
print(results_df)

write.table(results_df, "Basin_water_balance.csv", sep = ",", col.names = TRUE, row.names = FALSE)
