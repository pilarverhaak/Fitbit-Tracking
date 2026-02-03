# # # # # # Single Account Check # # # # # # # 


# (Run one step at a time!!)


#wd should be where you have your csv with demo accounts and two relevant R scripts
setwd("/Users/pv891/Desktop/Sparkle/Fitbit") 


# Step 0: load script for authorization 
source("fitbit_authorization.R")



# Step 1: Specify account to authorize
account_to_authorize <- "P001" #update as needed 
demo_accounts <- read.csv("demo_accounts.csv", stringsAsFactors = FALSE)
idx <- which(demo_accounts$study_id == account_to_authorize)
demo_accounts$authorized[idx] <- FALSE
demo_accounts$user_id[idx] <- NA
demo_accounts$access_token[idx] <- NA
demo_accounts$refresh_token[idx] <- NA
demo_accounts$token_expiry[idx] <- NA
write.csv(demo_accounts, "demo_accounts.csv", row.names = FALSE)
cat("Email:", demo_accounts$email[idx], "\n")
cat("Password:", demo_accounts$password[idx], "\n\n")


# Step 2: Authorization time!

cat("INSTRUCTIONS:\n")
cat("1. Copy the authorization URL when it appears\n")
cat("2. Open Safari → File → New INCOGNITO Window (Cmd+Shift+N)\n")
cat("3. Paste the URL\n")
cat("4. Log in with the email/password shown above\n")
cat("5. Click 'Allow All' to authorize\n")
cat("6. Copy ONLY the code from the redirect URL\n")
cat("7. Paste here\n\n")

authorize_single_account(account_to_authorize)



# Step 3: data retrieval 

source("fitbit_data_retrieval.R")

profile <- get_profile(account_to_authorize)
cat("\nUser ID:", profile$user$encodedId, "\n")
cat("Display Name:", profile$user$displayName, "\n")
cat("Member Since:", profile$user$memberSince, "\n")

# Check devices
devices <- get_devices(account_to_authorize)
if (length(devices) > 0) {
  cat("Devices paired:", length(devices), "\n")
} else {
  cat("No devices paired yet\n")
}

# Check activity for Sept 23 (change to date of interest)
activity <- get_activity(account_to_authorize, "2025-09-23")
cat("\nSept 23, 2025 Steps:", activity$summary$steps, "\n")

# Check today
today <- format(Sys.Date(), "%Y-%m-%d")
activity_today <- get_activity(account_to_authorize, today)
cat("Today (", today, ") Steps:", activity_today$summary$steps, "\n")


