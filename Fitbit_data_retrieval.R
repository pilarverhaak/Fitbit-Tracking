library(httr)
library(jsonlite)
library(base64enc)

# Step #1: load data retrieval script
client_id <- "23TRZM"
client_secret <- "f3fc77b9e63b8d4c6e962cb0169c3595"
demo_accounts <- read.csv("demo_accounts.csv", stringsAsFactors = FALSE)
refresh_access_token <- function(refresh_token) {
  auth_string <- paste0(client_id, ":", client_secret)
  basic_auth <- paste0("Basic ", base64encode(charToRaw(auth_string)))
  response <- POST(
    "https://api.fitbit.com/oauth2/token",
    add_headers(
      Authorization = basic_auth,
      `Content-Type` = "application/x-www-form-urlencoded"
    ),
    body = list(
      grant_type = "refresh_token",
      refresh_token = refresh_token
    ),
    encode = "form"
  )
  if (status_code(response) == 200) {
    return(content(response, "parsed"))
  } else {
    cat("Token refresh failed:", content(response, "text"), "\n")
    return(NULL)
  }
}
get_valid_token <- function(study_id) {
  demo_accounts <- read.csv("demo_accounts.csv", stringsAsFactors = FALSE)
  idx <- which(demo_accounts$study_id == study_id)
  
  if (length(idx) == 0) {
    cat("Study ID not found!\n")
    return(NULL)
  }
  if (!demo_accounts$authorized[idx]) {
    cat("This account is not authorized yet!\n")
    return(NULL)
  }
  expiry <- as.POSIXct(demo_accounts$token_expiry[idx])   # Checks if token is expired
  if (Sys.time() > expiry) {
    cat("Access token expired. Refreshing...\n")
    new_tokens <- refresh_access_token(demo_accounts$refresh_token[idx])
    if (!is.null(new_tokens)) {
      # Update the CSV with new tokens
      demo_accounts$access_token[idx] <- new_tokens$access_token
      demo_accounts$refresh_token[idx] <- new_tokens$refresh_token
      demo_accounts$token_expiry[idx] <- as.character(Sys.time() + new_tokens$expires_in)
      write.csv(demo_accounts, "demo_accounts.csv", row.names = FALSE)
      
      cat("Token refreshed successfully!\n")
      return(new_tokens$access_token)
    } else {
      return(NULL)
    }
  } else {
    return(demo_accounts$access_token[idx])
  }
}
get_profile <- function(study_id) {
  access_token <- get_valid_token(study_id)
  if (is.null(access_token)) return(NULL)
  response <- GET(
    "https://api.fitbit.com/1/user/-/profile.json",
    add_headers(Authorization = paste0("Bearer ", access_token))
  )
  if (status_code(response) == 200) {
    return(content(response, "parsed"))
  } else {
    cat("Error:", content(response, "text"), "\n")
    return(NULL)
  }
}


# FUNCTION: Get activity summary for a date
get_activity <- function(study_id, date = "today") {
  access_token <- get_valid_token(study_id)
  if (is.null(access_token)) return(NULL)
  
  response <- GET(
    paste0("https://api.fitbit.com/1/user/-/activities/date/", date, ".json"),
    add_headers(Authorization = paste0("Bearer ", access_token))
  )
  
  if (status_code(response) == 200) {
    return(content(response, "parsed"))
  } else {
    cat("Error:", content(response, "text"), "\n")
    return(NULL)
  }
}

# FUNCTION: Get heart rate for a date
get_heart_rate <- function(study_id, date = "today") {
  access_token <- get_valid_token(study_id)
  if (is.null(access_token)) return(NULL)
  response <- GET(
    paste0("https://api.fitbit.com/1/user/-/activities/heart/date/", date, "/1d.json"),
    add_headers(Authorization = paste0("Bearer ", access_token))
  )
  if (status_code(response) == 200) {
    return(content(response, "parsed"))
  } else {
    cat("Error:", content(response, "text"), "\n")
    return(NULL)
  }
}

# FUNCTION: Get heart rate INTRADAY (minute-by-minute)
get_heart_rate_intraday <- function(study_id, date = "today") {
  access_token <- get_valid_token(study_id)
  if (is.null(access_token)) return(NULL)
  response <- GET(
    paste0("https://api.fitbit.com/1/user/-/activities/heart/date/", date, "/1d/1min.json"),
    add_headers(Authorization = paste0("Bearer ", access_token))
  )
  
  if (status_code(response) == 200) {
    return(content(response, "parsed"))
  } else {
    cat("Error:", content(response, "text"), "\n")
    return(NULL)
  }
}
# FUNCTION: Get sleep data
get_sleep <- function(study_id, date = "today") {
  access_token <- get_valid_token(study_id)
  if (is.null(access_token)) return(NULL)
  response <- GET(
    paste0("https://api.fitbit.com/1.2/user/-/sleep/date/", date, ".json"),
    add_headers(Authorization = paste0("Bearer ", access_token))
  )
  if (status_code(response) == 200) {
    return(content(response, "parsed"))
  } else {
    cat("Error:", content(response, "text"), "\n")
    return(NULL)
  }
}
# FUNCTION: Get devices
get_devices <- function(study_id) {
  access_token <- get_valid_token(study_id)
  if (is.null(access_token)) return(NULL)
  response <- GET(
    "https://api.fitbit.com/1/user/-/devices.json",
    add_headers(Authorization = paste0("Bearer ", access_token))
  )
  if (status_code(response) == 200) {
    return(content(response, "parsed"))
  } else {
    cat("Error:", content(response, "text"), "\n")
    return(NULL)
  }
}

# FUNCTION: Get comprehensive data summary
get_data_summary <- function(study_id, date = "today") {
  cat("\n==============================================\n")
  cat("DATA SUMMARY FOR:", study_id, "\n")
  cat("Date:", date, "\n")
  cat("==============================================\n\n")
  cat("Fetching profile...\n")
  profile <- get_profile(study_id)
  if (!is.null(profile)) {
    cat("✓ User:", profile$user$displayName, "\n")
    cat("  Member since:", profile$user$memberSince, "\n")
  }
  cat("\nFetching devices...\n")
  devices <- get_devices(study_id)
  if (!is.null(devices) && length(devices) > 0) {
    cat("✓ Devices paired:", length(devices), "\n")
    for (i in seq_along(devices)) {
      cat("  -", devices[[i]]$deviceVersion, "| Battery:", devices[[i]]$battery, 
          "| Last sync:", devices[[i]]$lastSyncTime, "\n")
    }
  } else {
    cat("✗ No devices paired to this account\n")
  }
  cat("\nFetching activity...\n")
  activity <- get_activity(study_id, date)
  if (!is.null(activity)) {
    cat("✓ Steps:", activity$summary$steps, "\n")
    cat("  Distance:", activity$summary$distances[[1]]$distance, "km\n")
    cat("  Calories:", activity$summary$caloriesOut, "\n")
  }
  cat("\nFetching heart rate...\n")
  hr <- get_heart_rate(study_id, date)
  if (!is.null(hr) && length(hr$`activities-heart`) > 0) {
    if (!is.null(hr$`activities-heart`[[1]]$value$restingHeartRate)) {
      cat("✓ Resting HR:", hr$`activities-heart`[[1]]$value$restingHeartRate, "bpm\n")
    }
  }
  cat("\nFetching sleep...\n")
  sleep <- get_sleep(study_id, date)
  if (!is.null(sleep) && length(sleep$sleep) > 0) {
    cat("✓ Sleep records:", length(sleep$sleep), "\n")
    cat("  Total sleep:", sleep$summary$totalMinutesAsleep, "minutes\n")
  }
  return(list(
    profile = profile,
    devices = devices,
    activity = activity,
    heart_rate = hr,
    sleep = sleep
  ))
}


cat("Fitbit Data Retrieval Tool Loaded!\n")
cat("Available functions:\n")
cat("  get_profile(study_id)\n")
cat("  get_activity(study_id, date)\n")
cat("  get_heart_rate(study_id, date)\n")
cat("  get_heart_rate_intraday(study_id, date)\n")
cat("  get_sleep(study_id, date)\n")
cat("  get_devices(study_id)\n")
cat("  get_data_summary(study_id, date)\n")
cat("\nExample: get_data_summary('P001', 'today')\n")
cat("==============================================\n\n")







######## Run when ready!!! ########## (for an individual participant)
get_data_summary("P001", "today")










# Check the profiles of different accounts
get_profile("P002")
get_profile("P005")
get_profile("P010")





# Check all the authorized accounts
demo_accounts <- read.csv("demo_accounts.csv", stringsAsFactors = FALSE)
authorized <- demo_accounts[demo_accounts$authorized == TRUE, ]

cat("Authorized accounts and their User IDs:\n")
print(authorized[, c("study_id", "email", "user_id")])

cat("\nUnique User IDs:", length(unique(authorized$user_id)), "\n")
cat("Total Authorized Accounts:", nrow(authorized), "\n")
