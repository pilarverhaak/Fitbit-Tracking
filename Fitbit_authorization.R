# # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                   #
#       Accessing the fitbit Web API                #
#                                                   #
# # # # # # # # # # # # # # # # # # # # # # # # # # # 


# STEP 0: Make sure you have these packages are installed 
install.packages("httr")
install.packages("jsonlite")
install.packages("digest")
install.packages("base64enc")

library(httr)
library(jsonlite)
library(digest)
library(base64enc)




# STEP 1: load the csv with demo account info and reformat it 
accounts <- read.csv("~/Desktop/generated_accounts.csv", 
                     stringsAsFactors = FALSE, 
                     header = FALSE)  # This tells R there are no column names
colnames(accounts) <- c("email", "password")
head(accounts)
cat("Total accounts:", nrow(accounts), "\n")
demo_accounts <- data.frame(
  study_id = paste0("P", sprintf("%03d", 1:nrow(accounts))),  # P001 to P750
  email = accounts$email,
  password = accounts$password,
  user_id = NA,
  access_token = NA,
  refresh_token = NA,
  token_expiry = as.POSIXct(NA),
  authorized = FALSE,
  stringsAsFactors = FALSE
)
write.csv(demo_accounts, "demo_accounts.csv", row.names = FALSE)
cat("\nCreated demo_accounts.csv with", nrow(demo_accounts), "accounts\n")
head(demo_accounts, 10)
tail(demo_accounts, 5)
cat("\nMissing emails:", sum(is.na(demo_accounts$email) | demo_accounts$email == ""), "\n")
cat("Missing passwords:", sum(is.na(demo_accounts$password) | demo_accounts$password == ""), "\n")




# STEP 2: APP CREDENTIALS
client_id <- "23TRZM"  # Your client ID
client_secret <- "f3fc77b9e63b8d4c6e962cb0169c3595"  # CLIENT SECRET HERE
redirect_uri <- "http://localhost:1410/"
demo_accounts <- read.csv("demo_accounts.csv", stringsAsFactors = FALSE)



# STEP 3: Generate PKCE codes + authorization URLs (per demo acct)
generate_pkce <- function() {
  code_verifier <- paste0(sample(c(letters, LETTERS, 0:9, "-", ".", "_", "~"), 
                                 128, replace = TRUE), collapse = "")
  
  code_challenge <- base64encode(digest(code_verifier, algo = "sha256", 
                                        serialize = FALSE, raw = TRUE))
  code_challenge <- gsub("=+$", "", code_challenge)
  code_challenge <- gsub("\\+", "-", code_challenge)
  code_challenge <- gsub("/", "_", code_challenge)
  
  return(list(verifier = code_verifier, challenge = code_challenge))
}
generate_auth_url <- function(code_challenge, state) {
  scopes <- c("activity", "heartrate", "sleep", "profile", "weight", 
              "cardio_fitness", "respiratory_rate", "oxygen_saturation", 
              "temperature", "nutrition", "location", "social", "settings",
              "electrocardiogram", "irregular_rhythm_notifications")
  scope_string <- paste(scopes, collapse = " ")
  auth_url <- paste0(
    "https://www.fitbit.com/oauth2/authorize?",
    "response_type=code",
    "&client_id=", client_id,
    "&redirect_uri=", URLencode(redirect_uri, reserved = TRUE),
    "&scope=", URLencode(scope_string, reserved = TRUE),
    "&code_challenge=", code_challenge,
    "&code_challenge_method=S256",
    "&state=", state
  )
  return(auth_url)
}





# STEP 4: Exchange authorization code for tokens
exchange_code_for_tokens <- function(auth_code, code_verifier) {
  auth_string <- paste0(client_id, ":", client_secret)
  basic_auth <- paste0("Basic ", base64encode(charToRaw(auth_string)))
  response <- POST(
    "https://api.fitbit.com/oauth2/token",
    add_headers(
      Authorization = basic_auth,
      `Content-Type` = "application/x-www-form-urlencoded"
    ),
    body = list(
      client_id = client_id,
      grant_type = "authorization_code",
      redirect_uri = redirect_uri,
      code = auth_code,
      code_verifier = code_verifier
    ),
    encode = "form"
  )
  if (status_code(response) == 200) {
    return(content(response, "parsed"))
  } else {
    cat("Error:", content(response, "text"), "\n")
    return(NULL)
  }
}





# STEP 5: Authorize a single account
authorize_single_account <- function(study_id) {
  demo_accounts <- read.csv("demo_accounts.csv", stringsAsFactors = FALSE)
  idx <- which(demo_accounts$study_id == study_id)
  if (length(idx) == 0) {
    cat("Study ID not found!\n")
    return(NULL)
  }
  if (demo_accounts$authorized[idx]) {
    cat("This account is already authorized!\n")
    return(NULL)
  }
  # Generate PKCE codes (NEW codes for THIS account)
  pkce <- generate_pkce()
  code_verifier <- pkce$verifier
  code_challenge <- pkce$challenge
  state <- study_id  # Use study_id as state for tracking
  auth_url <- generate_auth_url(code_challenge, state)
  
  # Display instructions
  cat("AUTHORIZING:", study_id, "\n")
  cat("Email:", demo_accounts$email[idx], "\n")
  cat("Password:", demo_accounts$password[idx], "\n")
  cat("==============================================\n\n")
  cat("INSTRUCTIONS:\n")
  cat("1. The authorization URL will open in your browser\n")
  cat("2. Log OUT of any current Fitbit account (click profile icon → Log Out)\n")
  cat("3. Log IN with the credentials shown above\n")
  cat("4. Click 'Allow' to authorize the app\n")
  cat("5. After redirect, copy ONLY the code from the URL\n")
  cat("   (URL will look like: http://localhost:1410/?code=XXXXX&state=", state, ")\n", sep = "")
  cat("6. Paste the code here\n\n")
  
  # Open browser
  readline(prompt = "Press ENTER to open the authorization URL...")
  browseURL(auth_url)
  
  # Get authorization code
  auth_code <- readline(prompt = "\nPaste the authorization code here: ")
  auth_code <- trimws(auth_code)
  
  # Clean up the code in case they pasted the whole URL
  if (grepl("code=", auth_code)) {
    auth_code <- sub(".*code=([^&]+).*", "\\1", auth_code)
  }
  
  if (nchar(auth_code) == 0) {
    cat("No code entered. Skipping...\n")
    return(NULL)
  }
  
  # Exchange code for tokens
  cat("\nExchanging code for tokens...\n")
  tokens <- exchange_code_for_tokens(auth_code, code_verifier)
  
  if (!is.null(tokens)) {
    # Reload to ensure we have latest data
    demo_accounts <- read.csv("demo_accounts.csv", stringsAsFactors = FALSE)
    idx <- which(demo_accounts$study_id == study_id)
    
    # Update the dataframe
    demo_accounts$user_id[idx] <- tokens$user_id
    demo_accounts$access_token[idx] <- tokens$access_token
    demo_accounts$refresh_token[idx] <- tokens$refresh_token
    demo_accounts$token_expiry[idx] <- as.character(Sys.time() + tokens$expires_in)
    demo_accounts$authorized[idx] <- TRUE
    
    # Save immediately
    write.csv(demo_accounts, "demo_accounts.csv", row.names = FALSE)
    
    cat("\n✓ SUCCESS! Tokens saved for", study_id, "\n")
    cat("User ID:", tokens$user_id, "\n")
    cat("Access token expires in:", round(tokens$expires_in / 3600, 1), "hours\n\n")
    
    return(tokens)
  } else {
    cat("\n✗ FAILED to get tokens for", study_id, "\n\n")
    return(NULL)
  }
}




# STEP 6: Batch authorize multiple accounts

batch_authorize <- function(start_id = 1, end_id = 10) {
  study_ids <- paste0("P", sprintf("%03d", start_id:end_id))
  
  cat("\n==============================================\n")
  cat("BATCH AUTHORIZATION\n")
  cat("Authorizing accounts:", start_id, "to", end_id, "\n")
  cat("Total accounts:", length(study_ids), "\n")
  cat("==============================================\n\n")
  
  success_count <- 0
  fail_count <- 0
  
  for (study_id in study_ids) {
    result <- authorize_single_account(study_id)
    
    if (!is.null(result)) {
      success_count <- success_count + 1
    } else {
      fail_count <- fail_count + 1
    }
    
    # Small pause between accounts
    Sys.sleep(1)
  }
}

# STEP 7: Check authorization status
check_status <- function() {
  demo_accounts <- read.csv("demo_accounts.csv", stringsAsFactors = FALSE)
  
  total <- nrow(demo_accounts)
  authorized <- sum(demo_accounts$authorized, na.rm = TRUE)
  remaining <- total - authorized
  
  cat("AUTHORIZATION STATUS\n")
  cat("Total accounts:", total, "\n")
  cat("Authorized:", authorized, "(", round(authorized/total*100, 1), "%)\n")
  cat("Remaining:", remaining, "\n")
  cat("==============================================\n\n")
  
  if (remaining > 0) {
    next_account <- demo_accounts$study_id[!demo_accounts$authorized][1]
    cat("Next account to authorize:", next_account, "\n")
    cat("Email:", demo_accounts$email[demo_accounts$study_id == next_account], "\n\n")
  } else {
    cat("All accounts are authorized!\n\n")
  }
}


# STEP 8: Define function: MAIN MENU
main_menu <- function() {

  cat("1. Authorize single account\n")
  cat("2. Authorize batch (specify range)\n")
  cat("3. Check authorization status\n")
  cat("4. Continue from last authorized\n")
  cat("5. Exit\n")
  
  choice <- readline(prompt = "Enter choice (1-5): ")
  
  if (choice == "1") {
    study_id <- readline(prompt = "Enter Study ID (e.g., P001): ")
    authorize_single_account(study_id)
    main_menu()
    
  } else if (choice == "2") {
    start <- as.numeric(readline(prompt = "Start number (e.g., 1): "))
    end <- as.numeric(readline(prompt = "End number (e.g., 10): "))
    batch_authorize(start, end)
    main_menu()
    
  } else if (choice == "3") {
    check_status()
    main_menu()
    
  } else if (choice == "4") {
    demo_accounts <- read.csv("demo_accounts.csv", stringsAsFactors = FALSE)
    next_idx <- which(!demo_accounts$authorized)[1]
    if (!is.na(next_idx)) {
      authorize_single_account(demo_accounts$study_id[next_idx])
      main_menu()
    } else {
      cat("All accounts are authorized!\n")
      main_menu()
    }
    
  } else if (choice == "5") {
    cat("Goodbye!\n")
    
  } else {
    cat("Invalid choice. Try again.\n")
    main_menu()
  }
}


cat("Fitbit Authorization Tool Loaded!\n")
cat("Run: main_menu() to start\n\n")
cat("Make sure you have:\n")
cat("1. Added your client_secret to this script\n")
cat("2. demo_accounts.csv in your working directory\n")
cat("==============================================\n\n")



######## RUN WHEN READY TO BEGIN ########
main_menu()