#' Calculate Day Length and Sunrise Time
#' @param doy Day of year (1-365)
#' @param lat Latitude in decimal degrees
#' @return List with day_length (hours) and sunrise (hour of day)
get_solar_parameters <- function(doy, lat) {
  # Solar declination (radians)
  dec <- 0.409 * sin((2 * pi / 365) * doy - 1.39)
  
  # Sunset hour angle (radians)
  # tan(lat)*tan(dec) must be between -1 and 1
  lat_rad <- lat * (pi / 180)
  term <- -tan(lat_rad) * tan(dec)
  term <- pmax(-1, pmin(1, term)) # Clamp values for polar regions
  ws <- acos(term)
  
  # Day length (hours)
  day_length <- 24 * (ws / pi)
  
  # Sunrise time (assuming solar noon is at 12:00)
  sunrise <- 12 - (day_length / 2)
  
  return(list(day_length = day_length, sunrise = sunrise))
}


#' Calculate Tmean using Parton & Logan (1981) Simulation
#' 
#' @param tmin Daily Minimum Temperature
#' @param tmax Daily Maximum Temperature
#' @param doy Day of Year
#' @param lat Latitude
#' 
#' # Coefficients below default to Dall'Amico & Hornsteiner (2006) values [Alps/Complex Terrain].
#' # For Parton & Logan (1981) originals [US Plains], use: a=2.12, b=2.83, c=0.18
#' @param a Lag of Tmax (hours after noon). 
#'   Controls how long heat persists in the afternoon. 
#'   Default 1.86 (Orig: 2.12).
#' @param b Nighttime decay coefficient (unitless). 
#'   High values = rapid cooling (dry/high plains). 
#'   Low values = slow cooling (humid/valleys). 
#'   Default 2.20 (Orig: 2.83).
#' @param c Lag of Tmin relative to sunrise (hours). 
#'   Negative means min temp occurs slightly before sunrise. 
#'   Default -0.17 (Orig: 0.18).
calculate_tmean_parton_logan <- function(tmin, tmax, doy, lat, 
                                         a = 1.86, b = 2.20, c = -0.17) {
  
  solar <- get_solar_parameters(doy, lat)
  day_len <- solar$day_length
  sunrise <- solar$sunrise
  sunset <- 12 + (day_len / 2)
  
  # Pre-allocate hourly temperatures
  t_hourly <- numeric(24)
  hours <- 0:23
  
  for (i in seq_along(hours)) {
    h <- hours[i]
    
    if (h >= sunrise + c && h <= sunset) {
      # DAYTIME (Sine Wave)
      m <- h - (sunrise + c)
      val <- pi * m / (day_len - c + a)
      t_hourly[i] <- tmin + (tmax - tmin) * sin(val)
    } else {
      # NIGHTTIME (Exponential Decay)
      # Calculate time since sunset
      if (h > sunset) {
        n <- h - sunset
      } else {
        # Pre-dawn hours belong to the "night" of the previous cycle
        n <- (24 - sunset) + h
      }
      
      # Temperature at sunset (boundary condition)
      val_sunset <- pi * (sunset - (sunrise + c)) / (day_len - c + a)
      t_sunset <- tmin + (tmax - tmin) * sin(val_sunset)
      
      night_len <- 24 - day_len
      term1 <- tmin - t_sunset * exp(-b / night_len)
      term2 <- t_sunset - tmin
      term3 <- exp(-b * n / night_len)
      
      t_hourly[i] <- (term1 + term2 * term3) / (1 - exp(-b / night_len))
    }
  }
  
  # Return the mean of the 24 simulated hours
  return(mean(t_hourly))
}

#' Calculate Tmean using Dall'Amico & Hornsteiner (2006) Weighted Method
#' 
#' @param tmin Daily Minimum Temperature
#' @param tmax Daily Maximum Temperature
#' @param doy Day of Year
#' @param lat Latitude
calculate_tmean_dall_amico <- function(tmin, tmax, doy, lat) {
  
  solar <- get_solar_parameters(doy, lat)
  sunrise <- solar$sunrise
  
  # Calculate 'alpha' (Weight of Tmax).
  # Formula is derived from linear regression of the integrated Parton-Logan curves 
  # (Dall'Amico & Hornsteiner 2006, Eq. 10).
  # 
  # 0.21  = Intercept (Base weight of Tmax).
  # 0.022 = Slope for day length correction.
  # (12 - sunrise) = Half-day duration (hours past 6:00).
  #
  # Result:
  # alpha ~ 0.33 in Winter (Short days -> Tmean closer to Tmin).
  # alpha ~ 0.45 in Summer (Long days  -> Tmean shifts toward Tmax).
  alpha <- 0.21 + 0.022 * (12 - sunrise) 

  # Calculate Weighted Mean: Tmean = Tmin + alpha * (Tmax - Tmin)
  t_mean <- tmin + alpha * (tmax - tmin)
  
  return(t_mean)
}

# Example Data: Summer day in Vienna
lat <- 48.2   # Latitude
doy <- 180    # Summer solstice (June)
t_min <- 15
t_max <- 30

# 1. Standard Arithmetic Mean (Weight 0.5)
# Inadequate because it assumes day and night are equal length
mean_arithmetic <- (t_min + t_max) / 2

# 2. Parton & Logan (1981) - The Simulation Approach
mean_parton <- calculate_tmean_parton_logan(t_min, t_max, doy, lat)

# 3. Dall'Amico (2006) - The Weighted Approach
mean_dall_amico <- calculate_tmean_dall_amico(t_min, t_max, doy, lat)

# Results
cat("Comparison of Tmean Calculation Methods:\n")
cat("----------------------------------------\n")
cat("Arithmetic (0.5/0.5): ", round(mean_arithmetic, 2), "°C\n")
cat("Parton & Logan (Sim): ", round(mean_parton, 2), "°C\n")
cat("Dall'Amico (Weight):  ", round(mean_dall_amico, 2), "°C\n")