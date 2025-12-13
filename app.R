# app.R - FieldConektiv Landing Page with MySQL Database
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DBI)
library(RMySQL)
library(bcrypt)

# MySQL Database Configuration
db_config <- list(
  host = "localhost",
  port = 3306,
  dbname = "FieldConektivDB",
  user = "root",  # Change to your MySQL username
  password = "View10cm"   # Change to your MySQL password
)

# Function to connect to MySQL database
connect_to_db <- function() {
  tryCatch({
    con <- dbConnect(
      MySQL(),
      host = db_config$host,
      port = db_config$port,
      dbname = db_config$dbname,
      user = db_config$user,
      password = db_config$password
    )
    return(con)
  }, error = function(e) {
    # Return NULL without showing notification
    return(NULL)
  })
}

# Function to hash password
hash_password <- function(password) {
  hash <- bcrypt::hashpw(password)
  return(hash)
}

# Function to verify password
verify_password <- function(password, hash) {
  return(bcrypt::checkpw(password, hash))
}

# Function to check if user exists
check_user_exists <- function(email) {
  con <- connect_to_db()
  if (is.null(con)) return(FALSE)
  
  tryCatch({
    query <- sprintf("SELECT COUNT(*) as count FROM users WHERE email_address = '%s'", 
                     dbEscapeStrings(con, email))
    result <- dbGetQuery(con, query)
    dbDisconnect(con)
    return(result$count > 0)
  }, error = function(e) {
    try(dbDisconnect(con), silent = TRUE)
    return(FALSE)
  })
}

# Function to create new user
create_user <- function(full_name, email, phone, password) {
  con <- connect_to_db()
  if (is.null(con)) return(list(success = FALSE, message = "Database connection failed"))
  
  tryCatch({
    # Check if email already exists
    check_query <- sprintf("SELECT COUNT(*) as count FROM users WHERE email_address = '%s'",
                           dbEscapeStrings(con, email))
    check_result <- dbGetQuery(con, check_query)
    
    if (check_result$count > 0) {
      dbDisconnect(con)
      return(list(success = FALSE, message = "Email address already registered"))
    }
    
    # Hash the password
    password_hash <- hash_password(password)
    confirm_hash <- hash_password(password)  # For confirm password
    
    # Get default role ID (User)
    role_query <- "SELECT id FROM roles WHERE role_name = 'User'"
    role_result <- dbGetQuery(con, role_query)
    
    if (nrow(role_result) == 0) {
      dbDisconnect(con)
      return(list(success = FALSE, message = "Default role not found in database"))
    }
    
    role_id <- role_result$id[1]
    
    # Insert user into database
    insert_query <- sprintf(
      "INSERT INTO users (full_name, email_address, phone_number, password_hash, confirm_hash, role_id) 
       VALUES ('%s', '%s', '%s', '%s', '%s', %d)",
      dbEscapeStrings(con, full_name),
      dbEscapeStrings(con, email),
      dbEscapeStrings(con, phone),
      password_hash,
      confirm_hash,
      role_id
    )
    
    dbExecute(con, insert_query)
    dbDisconnect(con)
    return(list(success = TRUE, message = "User created successfully"))
  }, error = function(e) {
    try(dbDisconnect(con), silent = TRUE)
    error_msg <- gsub("['\"]", "", e$message)
    error_msg <- gsub("[\r\n]", " ", error_msg)
    return(list(success = FALSE, message = paste("Database error:", error_msg)))
  })
}

# Function to authenticate user
authenticate_user <- function(email, password) {
  con <- connect_to_db()
  if (is.null(con)) return(NULL)
  
  tryCatch({
    query <- sprintf(
      "SELECT id, full_name, email_address, password_hash FROM users WHERE email_address = '%s'",
      dbEscapeStrings(con, email)
    )
    
    result <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    if (nrow(result) == 0) {
      return(NULL)  # User not found
    }
    
    # Verify password
    if (verify_password(password, result$password_hash[1])) {
      return(list(
        id = result$id[1],
        full_name = result$full_name[1],
        email = result$email_address[1]
      ))
    } else {
      return(NULL)  # Password incorrect
    }
  }, error = function(e) {
    try(dbDisconnect(con), silent = TRUE)
    return(NULL)
  })
}

# Function to validate phone number (11 digits, numbers only)
validate_phone_number <- function(phone) {
  # Remove any whitespace
  phone_clean <- gsub("\\s+", "", phone)
  
  # Check if it contains only digits
  if (!grepl("^[0-9]+$", phone_clean)) {
    return(list(valid = FALSE, message = "Phone number should contain only numbers"))
  }
  
  # Check if it's exactly 11 digits
  if (nchar(phone_clean) != 11) {
    return(list(valid = FALSE, message = "Phone number must be exactly 11 digits"))
  }
  
  return(list(valid = TRUE, message = "Phone number is valid"))
}

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  
  # Custom CSS for styling
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&display=swap');
      
      * {
        font-family: 'Poppins', sans-serif;
      }
      
      body {
        background-color: #f9fff9;
        margin: 0;
        padding: 0;
      }
      
      .landing-container, .signup-container, .policy-container, .dashboard-container {
        min-height: 100vh;
        display: flex;
        flex-direction: column;
        background: linear-gradient(135deg, #f1f9f1 0%, #e6f7e6 100%);
      }
      
      .header {
        padding: 20px 40px;
        display: flex;
        justify-content: space-between;
        align-items: center;
        background-color: rgba(255, 255, 255, 0.95);
        box-shadow: 0 2px 10px rgba(0, 100, 0, 0.05);
      }
      
      .dashboard-header {
        padding: 20px 40px;
        display: flex;
        justify-content: space-between;
        align-items: center;
        background-color: #E8F5E9;
        box-shadow: 0 2px 10px rgba(0, 100, 0, 0.1);
        border-bottom: 3px solid #4CAF50;
      }
      
      .logo {
        display: flex;
        align-items: center;
        gap: 12px;
        cursor: pointer;
      }
      
      .logo-icon {
        width: 40px;
        height: 40px;
        background-color: #4CAF50;
        border-radius: 8px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: white;
        font-weight: bold;
        font-size: 20px;
      }
      
      .logo-text {
        font-size: 28px;
        font-weight: 700;
        color: #2E7D32;
        letter-spacing: -0.5px;
      }
      
      .main-content {
        flex: 1;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 40px 20px;
      }
      
      .dashboard-content {
        flex: 1;
        padding: 40px;
        max-width: 1400px;
        margin: 0 auto;
        width: 100%;
      }
      
      .policy-content {
        flex: 1;
        padding: 40px 20px;
        max-width: 1200px;
        margin: 0 auto;
      }
      
      .content-wrapper {
        max-width: 1200px;
        width: 100%;
        display: flex;
        align-items: center;
        gap: 60px;
      }
      
      .hero-text {
        flex: 1;
      }
      
      .hero-title {
        font-size: 48px;
        font-weight: 700;
        color: #1B5E20;
        line-height: 1.2;
        margin-bottom: 20px;
      }
      
      .hero-subtitle {
        font-size: 20px;
        color: #388E3C;
        line-height: 1.6;
        margin-bottom: 30px;
        max-width: 600px;
      }
      
      .features {
        display: flex;
        flex-wrap: wrap;
        gap: 20px;
        margin-bottom: 40px;
      }
      
      .feature {
        display: flex;
        align-items: center;
        gap: 10px;
        color: #2E7D32;
        font-weight: 500;
      }
      
      .feature-icon {
        color: #4CAF50;
        font-size: 20px;
      }
      
      .auth-card {
        background-color: white;
        border-radius: 16px;
        padding: 40px;
        box-shadow: 0 10px 30px rgba(0, 100, 0, 0.1);
        width: 100%;
        max-width: 420px;
        text-align: center;
      }
      
      .signup-card {
        background-color: white;
        border-radius: 16px;
        padding: 40px;
        box-shadow: 0 10px 30px rgba(0, 100, 0, 0.1);
        width: 100%;
        max-width: 500px;
        text-align: center;
      }
      
      .policy-card {
        background-color: white;
        border-radius: 16px;
        padding: 40px;
        box-shadow: 0 10px 30px rgba(0, 100, 0, 0.1);
        width: 100%;
        max-width: 800px;
        margin: 0 auto;
      }
      
      .dashboard-card {
        background-color: white;
        border-radius: 16px;
        padding: 40px;
        box-shadow: 0 10px 30px rgba(0, 100, 0, 0.1);
        width: 100%;
        margin-bottom: 30px;
      }
      
      .auth-title {
        font-size: 28px;
        font-weight: 600;
        color: #1B5E20;
        margin-bottom: 10px;
      }
      
      .policy-title {
        font-size: 32px;
        font-weight: 700;
        color: #1B5E20;
        margin-bottom: 20px;
        text-align: center;
      }
      
      .dashboard-title {
        font-size: 36px;
        font-weight: 700;
        color: #1B5E20;
        margin-bottom: 10px;
      }
      
      .dashboard-subtitle {
        font-size: 18px;
        color: #666;
        margin-bottom: 30px;
      }
      
      .auth-subtitle {
        font-size: 16px;
        color: #666;
        margin-bottom: 30px;
      }
      
      .form-group {
        margin-bottom: 20px;
        text-align: left;
      }
      
      .form-label {
        display: block;
        margin-bottom: 8px;
        color: #2E7D32;
        font-weight: 500;
      }
      
      .form-label .required {
        color: #f44336;
        font-weight: bold;
      }
      
      .form-input {
        width: 100%;
        padding: 14px 16px;
        border: 2px solid #C8E6C9;
        border-radius: 10px;
        font-size: 16px;
        transition: all 0.3s;
        box-sizing: border-box;
      }
      
      .form-input:focus {
        outline: none;
        border-color: #4CAF50;
        box-shadow: 0 0 0 3px rgba(76, 175, 80, 0.2);
      }
      
      .btn-primary {
        background-color: #4CAF50;
        color: white;
        border: none;
        padding: 16px 32px;
        border-radius: 10px;
        font-size: 16px;
        font-weight: 600;
        cursor: pointer;
        transition: all 0.3s;
        width: 100%;
        margin-top: 10px;
      }
      
      .btn-primary:hover {
        background-color: #388E3C;
        transform: translateY(-2px);
        box-shadow: 0 5px 15px rgba(56, 142, 60, 0.3);
      }
      
      .btn-secondary {
        background-color: transparent;
        color: #4CAF50;
        border: 2px solid #4CAF50;
        padding: 16px 32px;
        border-radius: 10px;
        font-size: 16px;
        font-weight: 600;
        cursor: pointer;
        transition: all 0.3s;
        width: 100%;
        margin-top: 10px;
      }
      
      .btn-secondary:hover {
        background-color: #E8F5E9;
        transform: translateY(-2px);
      }
      
      .btn-back {
        background-color: transparent;
        color: #4CAF50;
        border: none;
        padding: 12px 24px;
        border-radius: 10px;
        font-size: 14px;
        font-weight: 500;
        cursor: pointer;
        transition: all 0.3s;
        display: flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 20px;
      }
      
      .btn-back:hover {
        background-color: #E8F5E9;
      }
      
      .divider {
        display: flex;
        align-items: center;
        margin: 25px 0;
        color: #81C784;
      }
      
      .divider-line {
        flex: 1;
        height: 1px;
        background-color: #C8E6C9;
      }
      
      .divider-text {
        padding: 0 15px;
        font-size: 14px;
      }
      
      .footer {
        text-align: center;
        padding: 25px;
        color: #66BB6A;
        font-size: 14px;
        background-color: rgba(255, 255, 255, 0.9);
        border-top: 1px solid #E8F5E9;
      }
      
      .footer-links {
        margin-top: 10px;
      }
      
      .footer-links a {
        color: #4CAF50;
        text-decoration: none;
        margin: 0 10px;
        transition: color 0.3s;
      }
      
      .footer-links a:hover {
        color: #2E7D32;
        text-decoration: underline;
      }
      
      .dashboard-nav {
        display: flex;
        gap: 15px;
        margin-bottom: 30px;
        flex-wrap: wrap;
      }
      
      .nav-btn {
        background-color: white;
        color: #2E7D32;
        border: 2px solid #C8E6C9;
        padding: 15px 25px;
        border-radius: 10px;
        font-size: 16px;
        font-weight: 600;
        cursor: pointer;
        transition: all 0.3s;
        display: flex;
        align-items: center;
        gap: 10px;
        min-width: 180px;
      }
      
      .nav-btn:hover {
        background-color: #4CAF50;
        color: white;
        border-color: #4CAF50;
        transform: translateY(-3px);
        box-shadow: 0 5px 15px rgba(76, 175, 80, 0.2);
      }
      
      .nav-btn.active {
        background-color: #4CAF50;
        color: white;
        border-color: #4CAF50;
      }
      
      .nav-btn i {
        font-size: 18px;
      }
      
      .dashboard-stats {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        gap: 25px;
        margin-bottom: 40px;
      }
      
      .stat-card {
        background-color: white;
        border-radius: 16px;
        padding: 30px;
        box-shadow: 0 5px 15px rgba(0, 100, 0, 0.08);
        text-align: center;
        border-top: 5px solid #4CAF50;
        transition: transform 0.3s;
      }
      
      .stat-card:hover {
        transform: translateY(-5px);
      }
      
      .stat-icon {
        font-size: 40px;
        color: #4CAF50;
        margin-bottom: 15px;
      }
      
      .stat-value {
        font-size: 36px;
        font-weight: 700;
        color: #1B5E20;
        margin-bottom: 5px;
      }
      
      .stat-label {
        font-size: 16px;
        color: #666;
      }
      
      .recent-activity {
        background-color: white;
        border-radius: 16px;
        padding: 30px;
        box-shadow: 0 5px 15px rgba(0, 100, 0, 0.08);
      }
      
      .activity-title {
        font-size: 22px;
        font-weight: 600;
        color: #1B5E20;
        margin-bottom: 20px;
        padding-bottom: 15px;
        border-bottom: 2px solid #E8F5E9;
      }
      
      .activity-item {
        display: flex;
        align-items: center;
        gap: 15px;
        padding: 15px 0;
        border-bottom: 1px solid #F1F8E9;
      }
      
      .activity-item:last-child {
        border-bottom: none;
      }
      
      .activity-icon {
        width: 40px;
        height: 40px;
        background-color: #E8F5E9;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        color: #4CAF50;
        font-size: 18px;
      }
      
      .activity-content {
        flex: 1;
      }
      
      .activity-text {
        color: #333;
        margin-bottom: 5px;
      }
      
      .activity-time {
        font-size: 12px;
        color: #888;
      }
      
      .demo-title {
        font-weight: 600;
        margin-bottom: 8px;
      }
      
      .password-input-wrapper {
        position: relative;
        width: 100%;
      }
      
      .password-toggle {
        position: absolute;
        right: 12px;
        top: 50%;
        transform: translateY(-50%);
        background: none;
        border: none;
        color: #4CAF50;
        cursor: pointer;
        padding: 4px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .password-toggle:hover {
        color: #388E3C;
      }
      
      .password-toggle:focus {
        outline: none;
      }
      
      .progress-indicator {
        display: flex;
        justify-content: center;
        margin-bottom: 30px;
        gap: 10px;
      }
      
      .progress-step {
        width: 30px;
        height: 30px;
        border-radius: 50%;
        background-color: #C8E6C9;
        color: #2E7D32;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: 600;
        font-size: 14px;
      }
      
      .progress-step.active {
        background-color: #4CAF50;
        color: white;
      }
      
      .progress-step.completed {
        background-color: #2E7D32;
        color: white;
      }
      
      .progress-line {
        height: 3px;
        background-color: #C8E6C9;
        flex: 1;
        margin-top: 14px;
      }
      
      .progress-line.active {
        background-color: #4CAF50;
      }
      
      .password-requirements {
        font-size: 12px;
        color: #666;
        margin-top: 5px;
      }
      
      .requirement {
        display: flex;
        align-items: center;
        gap: 5px;
        margin-bottom: 3px;
      }
      
      .requirement.valid {
        color: #4CAF50;
      }
      
      .phone-validation {
        font-size: 12px;
        color: #666;
        margin-top: 5px;
      }
      
      .phone-validation.valid {
        color: #4CAF50;
      }
      
      .phone-validation.error {
        color: #f44336;
      }
      
      .policy-section {
        margin-bottom: 30px;
      }
      
      .policy-section h2 {
        color: #2E7D32;
        font-size: 22px;
        margin-bottom: 15px;
        padding-bottom: 10px;
        border-bottom: 2px solid #C8E6C9;
      }
      
      .policy-section h3 {
        color: #388E3C;
        font-size: 18px;
        margin: 20px 0 10px 0;
      }
      
      .policy-section p {
        color: #555;
        line-height: 1.6;
        margin-bottom: 15px;
      }
      
      .policy-section ul {
        color: #555;
        line-height: 1.6;
        margin-left: 20px;
        margin-bottom: 15px;
      }
      
      .policy-section li {
        margin-bottom: 8px;
      }
      
      .contact-info {
        background-color: #E8F5E9;
        padding: 20px;
        border-radius: 10px;
        margin: 20px 0;
      }
      
      .contact-item {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 10px;
        color: #2E7D32;
      }
      
      .contact-item i {
        color: #4CAF50;
        font-size: 18px;
        width: 24px;
      }
      
      .contact-form {
        background-color: #F1F8E9;
        padding: 25px;
        border-radius: 10px;
        margin-top: 20px;
      }
      
      /* NEW STYLES FOR SURVEY BUTTONS */
      .survey-grid {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 30px;
        margin-top: 40px;
      }
      
      .survey-btn {
        background-color: white;
        border: none;
        border-radius: 20px;
        padding: 40px 30px;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        text-align: center;
        cursor: pointer;
        transition: all 0.3s ease;
        box-shadow: 0 8px 25px rgba(0, 100, 0, 0.1);
        border: 3px solid transparent;
        height: 250px;
      }
      
      .survey-btn:hover {
        transform: translateY(-10px);
        box-shadow: 0 15px 35px rgba(0, 100, 0, 0.2);
        border-color: #4CAF50;
      }
      
      .survey-icon {
        font-size: 70px;
        margin-bottom: 20px;
        color: #4CAF50;
        transition: all 0.3s ease;
      }
      
      .survey-btn:hover .survey-icon {
        transform: scale(1.1);
      }
      
      .survey-title {
        font-size: 24px;
        font-weight: 700;
        color: #1B5E20;
        margin-bottom: 10px;
      }
      
      .survey-desc {
        font-size: 14px;
        color: #666;
        line-height: 1.5;
        max-width: 80%;
      }
      
      /* Color variants for each survey type */
      .relocation-btn {
        background: linear-gradient(135deg, #E8F5E9 0%, #C8E6C9 100%);
      }
      
      .relocation-btn:hover {
        background: linear-gradient(135deg, #C8E6C9 0%, #A5D6A7 100%);
      }
      
      .subdivision-btn {
        background: linear-gradient(135deg, #F1F8E9 0%, #DCEDC8 100%);
      }
      
      .subdivision-btn:hover {
        background: linear-gradient(135deg, #DCEDC8 0%, #C5E1A5 100%);
      }
      
      .verification-btn {
        background: linear-gradient(135deg, #F9FBE7 0%, #F0F4C3 100%);
      }
      
      .verification-btn:hover {
        background: linear-gradient(135deg, #F0F4C3 0%, #E6EE9C 100%);
      }
      
      .topography-btn {
        background: linear-gradient(135deg, #E3F2FD 0%, #BBDEFB 100%);
      }
      
      .topography-btn:hover {
        background: linear-gradient(135deg, #BBDEFB 0%, #90CAF9 100%);
      }
      
      /* Survey form container */
      .survey-form-container {
        max-width: 1000px;
        margin: 0 auto;
        padding: 20px;
      }
      
      .survey-back-btn {
        background-color: transparent;
        color: #4CAF50;
        border: 2px solid #4CAF50;
        padding: 12px 24px;
        border-radius: 10px;
        font-size: 16px;
        font-weight: 600;
        cursor: pointer;
        transition: all 0.3s;
        display: inline-flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 30px;
      }
      
      .survey-back-btn:hover {
        background-color: #E8F5E9;
      }
      
      /* NEW STYLES FOR RELOCATION SURVEY FORM */
      .survey-section {
        background-color: white;
        border-radius: 12px;
        padding: 25px;
        margin-bottom: 25px;
        box-shadow: 0 4px 15px rgba(0, 100, 0, 0.05);
        border: 1px solid #E8F5E9;
      }
      
      .survey-section h3 {
        color: #2E7D32;
        font-size: 20px;
        font-weight: 600;
        margin-top: 0;
      }
      
      .file-input-container {
        border: 2px dashed #C8E6C9;
        border-radius: 8px;
        padding: 20px;
        text-align: center;
        background-color: #F9FFF9;
        transition: all 0.3s;
      }
      
      .file-input-container:hover {
        border-color: #4CAF50;
        background-color: #F1F8E9;
      }
      
      .file-input-container .btn-file {
        background-color: #4CAF50;
        color: white;
        border: none;
        padding: 10px 20px;
        border-radius: 6px;
        font-weight: 500;
        cursor: pointer;
        transition: all 0.3s;
      }
      
      .file-input-container .btn-file:hover {
        background-color: #388E3C;
      }
      
      .required-field::after {
        content: ' *';
        color: #f44336;
        font-weight: bold;
      }
      
      .conditional-documents {
        background-color: #F1F8E9;
        border-radius: 8px;
        padding: 20px;
        margin: 15px 0;
        border-left: 4px solid #4CAF50;
      }
      
      .conditional-documents h4 {
        color: #388E3C;
        margin-top: 0;
        margin-bottom: 15px;
        font-size: 18px;
      }
      
      .document-note {
        background-color: #FFF8E1;
        border-radius: 6px;
        padding: 10px 15px;
        margin: 10px 0;
        font-size: 14px;
        color: #795548;
        border-left: 3px solid #FFB300;
      }
      
      @media (max-width: 992px) {
        .content-wrapper {
          flex-direction: column;
          gap: 40px;
        }
        
        .hero-title {
          font-size: 36px;
        }
        
        .auth-card, .signup-card, .policy-card, .dashboard-card {
          max-width: 100%;
          padding: 30px 20px;
        }
        
        .header, .dashboard-header {
          padding: 15px 20px;
        }
        
        .policy-title {
          font-size: 28px;
        }
        
        .dashboard-title {
          font-size: 28px;
        }
        
        .dashboard-nav {
          justify-content: center;
        }
        
        .nav-btn {
          min-width: 160px;
        }
        
        .dashboard-stats {
          grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
        }
        
        .survey-grid {
          grid-template-columns: 1fr;
          gap: 20px;
        }
        
        .survey-btn {
          height: 220px;
          padding: 30px 20px;
        }
        
        .survey-icon {
          font-size: 60px;
        }
        
        .survey-title {
          font-size: 22px;
        }
        
        .survey-desc {
          font-size: 14px;
        }
        
        .survey-form-container {
          padding: 15px;
        }
        
        .survey-section {
          padding: 20px;
        }
      }
      
      @media (max-width: 768px) {
        .nav-btn {
          min-width: 140px;
          padding: 12px 20px;
          font-size: 14px;
        }
        
        .stat-value {
          font-size: 28px;
        }
        
        .dashboard-content {
          padding: 20px;
        }
        
        .survey-btn {
          height: 200px;
          padding: 25px 15px;
        }
        
        .survey-icon {
          font-size: 50px;
          margin-bottom: 15px;
        }
        
        .survey-title {
          font-size: 20px;
        }
        
        .survey-desc {
          font-size: 13px;
        }
        
        .survey-section h3 {
          font-size: 18px;
        }
        
        .survey-back-btn {
          padding: 10px 20px;
          font-size: 14px;
        }
      }
      
      .hidden {
        display: none !important;
      }
      
      .shiny-input-container {
        width: 100% !important;
        margin-bottom: 0 !important;
      }
      
      .shiny-input-password {
        width: 100% !important;
      }
      
      .shiny-input-checkbox {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }
      
      .checkbox {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }
    "))
  ),
  
  # Main UI structure
  div(
    id = "main-app",
    # Header will be conditionally rendered in server
    uiOutput("page_header"),
    
    # Conditional main content based on current page
    uiOutput("main_content"),
    
    # Footer that appears on all pages
    div(class = "footer",
        "© 2023 FieldConektiv. All rights reserved.",
        div(class = "footer-links",
            actionLink("privacy_link", "Privacy Policy", style = "color: #4CAF50;"),
            " | ",
            actionLink("terms_link", "Terms of Service", style = "color: #4CAF50;"),
            " | ",
            actionLink("contact_link", "Contact Support", style = "color: #4CAF50;")
        )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive value to track current page
  current_page <- reactiveVal("home")
  
  # Reactive value to store user session
  user_session <- reactiveValues(
    logged_in = FALSE,
    user_id = NULL,
    full_name = NULL,
    email = NULL
  )
  
  # Track active dashboard tab
  dashboard_tab <- reactiveVal("dashboard")
  
  # Track selected survey type
  selected_survey_type <- reactiveVal(NULL)
  
  # Header based on current page
  output$page_header <- renderUI({
    if (current_page() == "dashboard") {
      # Dashboard Header
      div(class = "dashboard-header",
          div(class = "logo", id = "logo",
              div(class = "logo-icon", "FC"),
              div(class = "logo-text", "FieldConektiv")
          ),
          div(
            style = "display: flex; align-items: center; gap: 20px;",
            span(style = "color: #2E7D32; font-weight: 600; font-size: 18px;", 
                 paste("Welcome,", user_session$full_name, "!")),
            actionButton("logout_btn", "Logout", 
                         icon = icon("sign-out-alt"),
                         style = "background-color: #4CAF50; color: white; border: none; padding: 10px 20px; border-radius: 8px; font-weight: 500;")
          )
      )
    } else {
      # Regular Header for other pages
      div(class = "header",
          div(class = "logo", id = "logo",
              div(class = "logo-icon", "FC"),
              div(class = "logo-text", "FieldConektiv")
          ),
          uiOutput("header_content")
      )
    }
  })
  
  # Header content for non-dashboard pages
  output$header_content <- renderUI({
    if (current_page() == "home") {
      if (user_session$logged_in) {
        div(
          style = "display: flex; align-items: center; gap: 20px;",
          span(style = "color: #4CAF50; font-weight: 500;", 
               paste("Welcome,", user_session$full_name)),
          actionButton("go_to_dashboard", "Go to Dashboard", 
                       style = "background-color: #4CAF50; color: white; border: none; padding: 10px 20px; border-radius: 8px; font-weight: 500;"),
          actionButton("logout_btn", "Logout", 
                       icon = icon("sign-out-alt"),
                       class = "btn-back")
        )
      } else {
        div(
          style = "color: #4CAF50; font-weight: 500;",
          "Connecting Fields, Empowering Industries"
        )
      }
    } else if (current_page() %in% c("signup", "privacy", "terms", "contact")) {
      actionButton("back_to_home", "Back to Home", 
                   icon = icon("arrow-left"), 
                   class = "btn-back")
    }
  })
  
  # Main content based on current page
  output$main_content <- renderUI({
    if (current_page() == "home") {
      # Landing Page Content
      div(class = "landing-container",
          div(class = "main-content",
              div(class = "content-wrapper",
                  
                  # Hero text section
                  div(class = "hero-text",
                      h1(class = "hero-title", "Apply for Land Operations with Precision"),
                      p(class = "hero-subtitle", 
                        "FieldConektiv is the comprehensive platform for managing Land Survey operations, 
                        design works, construction works, and other miscellaneous in one seamless system"),
                      
                      div(class = "features",
                          div(class = "feature",
                              span(class = "feature-icon", "✓"),
                              span("Credential Company for Land Surveys")
                          ),
                          div(class = "feature",
                              span(class = "feature-icon", "✓"),
                              span("Trusted Land Survey company for 10 years")
                          ),
                          div(class = "feature",
                              span(class = "feature-icon", "✓"),
                              span("1K Yearly Customers")
                          )
                      )
                  ),
                  
                  # Authentication card
                  div(class = "auth-card",
                      if (user_session$logged_in) {
                        div(
                          h2(class = "auth-title", paste("Welcome Back,", user_session$full_name)),
                          p(class = "auth-subtitle", "You are successfully logged in to FieldConektiv"),
                          actionButton("dashboard_btn", "Go to Dashboard", class = "btn-primary",
                                       style = "margin-top: 20px;"),
                          actionButton("profile_btn", "View Profile", class = "btn-secondary",
                                       style = "margin-top: 10px;")
                        )
                      } else {
                        div(
                          h2(class = "auth-title", "Welcome to FieldConektiv"),
                          
                          # Login Form
                          div(id = "login-form",
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "login-email", 
                                             span("Email Address", span(class = "required", "*"))),
                                  textInput("login_email", label = NULL, placeholder = "Enter your email")
                              ),
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "login-password", 
                                             span("Password", span(class = "required", "*"))),
                                  div(class = "password-input-wrapper",
                                      passwordInput("login_password", label = NULL, placeholder = "Enter your password"),
                                      tags$button(id = "toggle-login-password", type = "button", class = "password-toggle",
                                                  icon("eye"))
                                  )
                              ),
                              actionButton("login_btn", "Login to Your Account", class = "btn-primary"),
                              
                              div(class = "divider",
                                  div(class = "divider-line"),
                                  div(class = "divider-text", "New to FieldConektiv?"),
                                  div(class = "divider-line")
                              ),
                              
                              actionButton("go_to_signup", "Create New Account", class = "btn-secondary")
                          )
                        )
                      }
                  )
              )
          )
      )
    } else if (current_page() == "signup") {
      # Create Account Page Content
      div(class = "signup-container",
          div(class = "main-content",
              div(class = "signup-card",
                  h2(class = "auth-title", "Create Your FieldConektiv Account"),
                  p(class = "auth-subtitle", "Join our platform and start managing your land operations efficiently"),
                  
                  # Progress indicator
                  div(class = "progress-indicator",
                      div(class = "progress-step completed", "1"),
                      div(class = "progress-line active"),
                      div(class = "progress-step active", "2"),
                      div(class = "progress-line"),
                      div(class = "progress-step", "3")
                  ),
                  
                  # Signup Form
                  div(id = "signup-form",
                      div(class = "form-group",
                          tags$label(class = "form-label", `for` = "signup-fullname", 
                                     span("Full Name", span(class = "required", "*"))),
                          textInput("signup_fullname", label = NULL, placeholder = "Enter your full name")
                      ),
                      
                      div(class = "form-group",
                          tags$label(class = "form-label", `for` = "signup-email", 
                                     span("Email Address", span(class = "required", "*"))),
                          textInput("signup_email", label = NULL, placeholder = "Enter your email address")
                      ),
                      
                      div(class = "form-group",
                          tags$label(class = "form-label", `for` = "signup-phone", 
                                     span("Phone Number", span(class = "required", "*"))),
                          textInput("signup_phone", label = NULL, placeholder = "Enter 11-digit phone number"),
                          div(class = "phone-validation", id = "phone-validation",
                              "Enter 11 digits (numbers only)")
                      ),
                      
                      div(class = "form-group",
                          tags$label(class = "form-label", `for` = "signup-password", 
                                     span("Create Password", span(class = "required", "*"))),
                          div(class = "password-input-wrapper",
                              passwordInput("signup_password", label = NULL, placeholder = "Create a strong password"),
                              tags$button(id = "toggle-password", type = "button", class = "password-toggle",
                                          icon("eye"))
                          ),
                          div(class = "password-requirements",
                              div(class = "requirement", id = "req-length", "✗ At least 8 characters"),
                              div(class = "requirement", id = "req-uppercase", "✗ At least one uppercase letter"),
                              div(class = "requirement", id = "req-number", "✗ At least one number")
                          )
                      ),
                      
                      div(class = "form-group",
                          tags$label(class = "form-label", `for` = "signup-confirm", 
                                     span("Confirm Password", span(class = "required", "*"))),
                          div(class = "password-input-wrapper",
                              passwordInput("signup_confirm", label = NULL, placeholder = "Confirm your password"),
                              tags$button(id = "toggle-confirm-password", type = "button", class = "password-toggle",
                                          icon("eye"))
                          )
                      ),
                      
                      div(class = "form-group",
                          tags$label(class = "form-label", `for` = "signup-terms",
                                     checkboxInput("signup_terms", label = NULL, width = "20px"),
                                     "I agree to the ",
                                     actionLink("signup_terms_link", "Terms of Service", style = "color: #4CAF50;"),
                                     " and ",
                                     actionLink("signup_privacy_link", "Privacy Policy", style = "color: #4CAF50;"),
                                     span(class = "required", " *")
                          )
                      ),
                      
                      actionButton("create_account_btn", "Create Account", class = "btn-primary"),
                      
                      div(class = "divider",
                          div(class = "divider-line"),
                          div(class = "divider-text", "Already have an account?"),
                          div(class = "divider-line")
                      ),
                      
                      actionButton("go_to_login", "Back to Login", class = "btn-secondary")
                  )
              )
          )
      )
    } else if (current_page() == "dashboard") {
      # Dashboard Page Content
      div(class = "dashboard-container",
          div(class = "dashboard-content",
              div(class = "dashboard-card",
                  h1(class = "dashboard-title", "FieldConektiv Dashboard"),
                  p(class = "dashboard-subtitle", "Manage your land survey operations and projects efficiently"),
                  
                  # Navigation buttons
                  div(class = "dashboard-nav",
                      actionButton("nav_dashboard", 
                                   label = div(
                                     icon("tachometer-alt"),
                                     "Dashboard"
                                   ), 
                                   class = ifelse(dashboard_tab() == "dashboard", "nav-btn active", "nav-btn")),
                      
                      actionButton("nav_create_survey", 
                                   label = div(
                                     icon("map-marked-alt"),
                                     "Create Survey"
                                   ), 
                                   class = ifelse(dashboard_tab() == "create_survey", "nav-btn active", "nav-btn")),
                      
                      actionButton("nav_profile", 
                                   label = div(
                                     icon("user-circle"),
                                     "Profile"
                                   ), 
                                   class = ifelse(dashboard_tab() == "profile", "nav-btn active", "nav-btn")),
                      
                      actionButton("nav_logout", 
                                   label = div(
                                     icon("sign-out-alt"),
                                     "Logout"
                                   ), 
                                   class = "nav-btn")
                  ),
                  
                  # Dashboard content based on selected tab
                  if (dashboard_tab() == "dashboard") {
                    div(
                      # Statistics cards
                      div(class = "dashboard-stats",
                          div(class = "stat-card",
                              div(class = "stat-icon", icon("map")),
                              div(class = "stat-value", "12"),
                              div(class = "stat-label", "Active Surveys")
                          ),
                          div(class = "stat-card",
                              div(class = "stat-icon", icon("check-circle")),
                              div(class = "stat-value", "48"),
                              div(class = "stat-label", "Completed Projects")
                          ),
                          div(class = "stat-card",
                              div(class = "stat-icon", icon("users")),
                              div(class = "stat-value", "5"),
                              div(class = "stat-label", "Team Members")
                          ),
                          div(class = "stat-card",
                              div(class = "stat-icon", icon("calendar-alt")),
                              div(class = "stat-value", "3"),
                              div(class = "stat-label", "Upcoming Deadlines")
                          )
                      ),
                      
                      # Recent Activity
                      div(class = "recent-activity",
                          h2(class = "activity-title", "Recent Activity"),
                          div(class = "activity-item",
                              div(class = "activity-icon", icon("map-marker-alt")),
                              div(class = "activity-content",
                                  div(class = "activity-text", "New survey project 'Downtown Development' created"),
                                  div(class = "activity-time", "2 hours ago")
                              )
                          ),
                          div(class = "activity-item",
                              div(class = "activity-icon", icon("file-upload")),
                              div(class = "activity-content",
                                  div(class = "activity-text", "Uploaded survey data for 'Riverside Property'"),
                                  div(class = "activity-time", "Yesterday, 3:45 PM")
                              )
                          ),
                          div(class = "activity-item",
                              div(class = "activity-icon", icon("user-check")),
                              div(class = "activity-content",
                                  div(class = "activity-text", "Team member John Smith joined 'Mountain View Project'"),
                                  div(class = "activity-time", "2 days ago")
                              )
                          ),
                          div(class = "activity-item",
                              div(class = "activity-icon", icon("chart-line")),
                              div(class = "activity-content",
                                  div(class = "activity-text", "Monthly report generated and sent to client"),
                                  div(class = "activity-time", "3 days ago")
                              )
                          ),
                          div(class = "activity-item",
                              div(class = "activity-icon", icon("tools")),
                              div(class = "activity-content",
                                  div(class = "activity-text", "Updated equipment calibration settings"),
                                  div(class = "activity-time", "1 week ago")
                              )
                          )
                      )
                    )
                  } else if (dashboard_tab() == "create_survey") {
                    # Survey type selection buttons
                    if (is.null(selected_survey_type())) {
                      div(
                        h2("Select Survey Type", style = "color: #1B5E20; margin-bottom: 25px; text-align: center;"),
                        p("Choose the type of survey you want to create:", 
                          style = "color: #666; margin-bottom: 40px; text-align: center; font-size: 18px;"),
                        
                        div(class = "survey-grid",
                            actionButton("select_relocation", 
                                         label = div(
                                           div(class = "survey-icon", icon("exchange-alt")),
                                           div(class = "survey-title", "Relocation Survey"),
                                           div(class = "survey-desc", "Survey for property relocation and boundary determination")
                                         ),
                                         class = "survey-btn relocation-btn"),
                            
                            actionButton("select_subdivision", 
                                         label = div(
                                           div(class = "survey-icon", icon("map-signs")),
                                           div(class = "survey-title", "Subdivision Survey"),
                                           div(class = "survey-desc", "Divide larger properties into smaller parcels")
                                         ),
                                         class = "survey-btn subdivision-btn"),
                            
                            actionButton("select_verification", 
                                         label = div(
                                           div(class = "survey-icon", icon("check-double")),
                                           div(class = "survey-title", "Verification Survey"),
                                           div(class = "survey-desc", "Verify existing property boundaries and measurements")
                                         ),
                                         class = "survey-btn verification-btn"),
                            
                            actionButton("select_topography", 
                                         label = div(
                                           div(class = "survey-icon", icon("mountain")),
                                           div(class = "survey-title", "Topography Survey"),
                                           div(class = "survey-desc", "Detailed topographic mapping of land features")
                                         ),
                                         class = "survey-btn topography-btn")
                        )
                      )
                    } else if (selected_survey_type() == "Relocation") {
                      # Relocation Survey Form
                      div(class = "survey-form-container",
                          actionButton("back_to_survey_types", "← Back to Survey Types", 
                                       class = "survey-back-btn"),
                          
                          h2("Relocation Survey Request Form", 
                             style = "color: #1B5E20; margin-bottom: 25px; text-align: center;"),
                          p("Please fill out all required fields for your relocation survey request.", 
                            style = "color: #666; margin-bottom: 30px; text-align: center;"),
                          
                          # A. Personal Information Section
                          div(class = "survey-section",
                              h3("A. Personal Information", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "full_name", 
                                             span("Full Name", span(class = "required", "*"))),
                                  textInput("relocation_full_name", label = NULL, placeholder = "Enter your full name")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "government_id_type", 
                                             span("Valid Government-Issued ID Type", span(class = "required", "*"))),
                                  selectInput("government_id_type", label = NULL,
                                              choices = c("Select ID type..." = "",
                                                          "Philippine Passport" = "passport",
                                                          "Driver's License" = "driver_license",
                                                          "UMID" = "umid",
                                                          "PRC ID" = "prc_id",
                                                          "Voter's ID" = "voter_id"),
                                              selected = "")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "government_id_image", 
                                             span("Government ID Image", span(class = "required", "*"))),
                                  fileInput("government_id_image", label = NULL, 
                                            accept = c("image/png", "image/jpeg", "image/jpg", "application/pdf"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload image/scan of your government ID")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "applicant_full_name", 
                                             span("Applicant Full Name", span(class = "required", "*"))),
                                  textInput("applicant_full_name", label = NULL, placeholder = "Enter applicant's full name")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "special_power_attorney", 
                                             span("Special Power of Attorney (If different from above)")),
                                  fileInput("special_power_attorney", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Special Power of Attorney document")
                              ),
                              
                              div(class = "document-note",
                                  tags$b("Note:"), " Special Power of Attorney is only required if the Applicant Full Name is different from the Full Name above.")
                          ),
                          
                          # B. Essential Ownership Section
                          div(class = "survey-section",
                              h3("B. Essential Ownership Documents", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "land_title_status", 
                                             span("Land Title Status", span(class = "required", "*"))),
                                  selectInput("land_title_status", label = NULL,
                                              choices = c("Select status..." = "",
                                                          "Available" = "available",
                                                          "Lost" = "lost",
                                                          "Untitled" = "untitled",
                                                          "Inheritance" = "inheritance"),
                                              selected = "")
                              ),
                              
                              # Conditional panels based on land title status
                              # Available Title Documents
                              conditionalPanel(
                                condition = "input.land_title_status == 'available'",
                                div(class = "conditional-documents",
                                    h4("Required Documents for Available Title:"),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "original_title", 
                                                   span("Original Certificate of Title / Transfer Certificate of Title", span(class = "required", "*"))),
                                        fileInput("original_title", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Title document")
                                    )
                                )
                              ),
                              
                              # Lost Title Documents
                              conditionalPanel(
                                condition = "input.land_title_status == 'lost'",
                                div(class = "conditional-documents",
                                    h4("Required Documents for Lost Title:"),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "ctc_title", 
                                                   span("CTC of Title from Register of Deeds", span(class = "required", "*"))),
                                        fileInput("ctc_title", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload CTC document")
                                    ),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "owners_duplicate", 
                                                   span("Owner's Duplicate Copy of Title", span(class = "required", "*"))),
                                        fileInput("owners_duplicate", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Owner's Duplicate document")
                                    ),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "tax_declaration_lost", 
                                                   span("Tax Declaration", span(class = "required", "*"))),
                                        fileInput("tax_declaration_lost", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Tax Declaration")
                                    )
                                )
                              ),
                              
                              # Untitled Documents
                              conditionalPanel(
                                condition = "input.land_title_status == 'untitled'",
                                div(class = "conditional-documents",
                                    h4("Required Documents for Untitled Land:"),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "deed_conveyance", 
                                                   span("Deed of Absolute Sale / Extrajudicial Settlement / Other Notarized Deeds", span(class = "required", "*"))),
                                        fileInput("deed_conveyance", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Deed document")
                                    ),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "tax_declaration_untitled", 
                                                   span("Tax Declaration under your Name", span(class = "required", "*"))),
                                        fileInput("tax_declaration_untitled", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Tax Declaration")
                                    ),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "tax_clearance", 
                                                   span("Tax Clearance / Official Receipt of Real Property Tax", span(class = "required", "*"))),
                                        fileInput("tax_clearance", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Tax Clearance/Receipt")
                                    )
                                )
                              ),
                              
                              # Inheritance Documents
                              conditionalPanel(
                                condition = "input.land_title_status == 'inheritance'",
                                div(class = "conditional-documents",
                                    h4("Required Documents for Inherited Land:"),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "estate_settlement", 
                                                   span("Extrajudicial Settlement of Estate / Court Order", span(class = "required", "*"))),
                                        fileInput("estate_settlement", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Estate Settlement/Court Order")
                                    ),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "affidavit_adjudication", 
                                                   span("Affidavit of Self-Adjudication", span(class = "required", "*"))),
                                        fileInput("affidavit_adjudication", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Affidavit of Self-Adjudication")
                                    ),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "death_certificate", 
                                                   span("Death Certificate of the deceased owner", span(class = "required", "*"))),
                                        fileInput("death_certificate", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Death Certificate")
                                    )
                                )
                              )
                          ),
                          
                          # C. Required Clearances Section
                          div(class = "survey-section",
                              h3("C. Required Clearances", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "barangay_clearance", 
                                             span("Barangay Clearance", span(class = "required", "*"))),
                                  fileInput("barangay_clearance", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Barangay Clearance")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "tax_delinquency_cert", 
                                             span("Certificate of Tax Delinquency", span(class = "required", "*"))),
                                  fileInput("tax_delinquency_cert", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Certificate of Tax Delinquency")
                              )
                          ),
                          
                          # D. Technical Property Information Section
                          div(class = "survey-section",
                              h3("D. Technical Property Information", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
                                  div(class = "form-group",
                                      tags$label(class = "form-label", `for` = "block_number", 
                                                 span("Block Number", span(class = "required", "*"))),
                                      numericInput("block_number", label = NULL, value = NULL, min = 1, step = 1)
                                  ),
                                  
                                  div(class = "form-group",
                                      tags$label(class = "form-label", `for` = "lot_number", 
                                                 span("Lot Number", span(class = "required", "*"))),
                                      numericInput("lot_number", label = NULL, value = NULL, min = 1, step = 1)
                                  )
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "subdivision_name", 
                                             span("Subdivision Name (Optional)")),
                                  textInput("subdivision_name", label = NULL, placeholder = "Enter subdivision name if applicable")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "area", 
                                             span("Area", span(class = "required", "*"))),
                                  div(style = "display: flex; gap: 10px;",
                                      numericInput("area", label = NULL, value = NULL, min = 0, step = 0.01, width = "70%"),
                                      selectInput("area_unit", label = NULL, 
                                                  choices = c("Square Meters" = "sqm",
                                                              "Hectares" = "hectares",
                                                              "Acres" = "acres",
                                                              "Square Feet" = "sqft"),
                                                  selected = "sqm", width = "30%")
                                  )
                              )
                          ),
                          
                          # Additional Information Section
                          div(class = "survey-section",
                              h3("Additional Information", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "contact_phone", 
                                             span("Contact Phone Number", span(class = "required", "*"))),
                                  textInput("contact_phone", label = NULL, placeholder = "Enter your contact phone number")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "contact_email", 
                                             span("Contact Email Address", span(class = "required", "*"))),
                                  textInput("contact_email", label = NULL, placeholder = "Enter your email address")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "survey_reason", 
                                             span("Reason for Relocation Survey", span(class = "required", "*"))),
                                  textAreaInput("survey_reason", label = NULL, 
                                                placeholder = "Please describe the reason for requesting this relocation survey...", 
                                                rows = 4)
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "additional_notes", 
                                             span("Additional Notes (Optional)")),
                                  textAreaInput("additional_notes", label = NULL, 
                                                placeholder = "Any additional information or special requirements...", 
                                                rows = 3)
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "survey_urgency", 
                                             span("Urgency Level")),
                                  selectInput("survey_urgency", label = NULL,
                                              choices = c("Normal (2-4 weeks)" = "normal",
                                                          "High Priority (1-2 weeks)" = "high",
                                                          "Urgent (3-7 days)" = "urgent",
                                                          "Emergency (1-3 days)" = "emergency"),
                                              selected = "normal")
                              )
                          ),
                          
                          # Form submission buttons
                          div(style = "display: flex; gap: 15px; margin-top: 40px; padding-top: 20px; border-top: 2px solid #E8F5E9;",
                              actionButton("submit_relocation_survey", "Submit Relocation Survey Request", 
                                           class = "btn-primary",
                                           style = "width: auto; min-width: 250px; flex: 1;"),
                              actionButton("clear_relocation_form", "Clear Form", 
                                           class = "btn-secondary",
                                           style = "width: auto; min-width: 150px;")
                          )
                      )
                    } else if (selected_survey_type() == "Subdivision") {
                      # Subdivision Survey Form
                      div(class = "survey-form-container",
                          actionButton("back_to_survey_types", "← Back to Survey Types", 
                                       class = "survey-back-btn"),
                          
                          h2("Subdivision Survey Request Form", 
                             style = "color: #1B5E20; margin-bottom: 25px; text-align: center;"),
                          p("Please fill out all required fields for your subdivision survey request.", 
                            style = "color: #666; margin-bottom: 30px; text-align: center;"),
                          
                          # Part I. Prerequisite Documents (Ownership & Eligibility)
                          div(class = "survey-section",
                              h3("Part I. Prerequisite Documents (Ownership & Eligibility)", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "mother_lot_title", 
                                             span("Mother Lot Original Certificate of Title / Transfer Certificate of Title", span(class = "required", "*"))),
                                  fileInput("mother_lot_title", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Mother Lot Title document")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "bank_waiver", 
                                             span("Bank Waiver (If Mortgaged)")),
                                  fileInput("bank_waiver", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Bank Waiver if property is mortgaged")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "tax_receipt", 
                                             span("Mother Lot Latest Real Property Tax Receipt", span(class = "required", "*"))),
                                  fileInput("tax_receipt", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Latest Real Property Tax Receipt")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "no_tax_delinquency", 
                                             span("Certificate of No Tax Delinquency", span(class = "required", "*"))),
                                  fileInput("no_tax_delinquency", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Certificate of No Tax Delinquency")
                              )
                          ),
                          
                          # Part II. Applicant Information
                          div(class = "survey-section",
                              h3("Part II. Applicant Information", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "ownership_type", 
                                             span("Application Ownership Type", span(class = "required", "*"))),
                                  selectInput("ownership_type", label = NULL,
                                              choices = c("Select ownership type..." = "",
                                                          "Sole Proprietorship" = "sole",
                                                          "Partnership" = "partnership",
                                                          "Corporation" = "corporation",
                                                          "Special Power of Attorney" = "spa"),
                                              selected = "")
                              ),
                              
                              # Conditional panels based on ownership type
                              # Sole Proprietorship
                              conditionalPanel(
                                condition = "input.ownership_type == 'sole'",
                                div(class = "conditional-documents",
                                    h4("Sole Proprietorship Information:"),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "sole_full_name", 
                                                   span("Full Name", span(class = "required", "*"))),
                                        textInput("sole_full_name", label = NULL, placeholder = "Enter your full name")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "sole_government_id", 
                                                   span("Valid Government-Issued ID Type", span(class = "required", "*"))),
                                        selectInput("sole_government_id", label = NULL,
                                                    choices = c("Select ID type..." = "",
                                                                "Philippine Passport" = "passport",
                                                                "Driver's License" = "driver_license",
                                                                "UMID" = "umid",
                                                                "PRC ID" = "prc_id",
                                                                "Voter's ID" = "voter_id"),
                                                    selected = "")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "sole_government_id_image", 
                                                   span("Government ID Image", span(class = "required", "*"))),
                                        fileInput("sole_government_id_image", label = NULL,
                                                  accept = c("image/png", "image/jpeg", "image/jpg", "application/pdf"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload image/scan of your government ID")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "sole_applicant_full_name", 
                                                   span("Applicant Full Name", span(class = "required", "*"))),
                                        textInput("sole_applicant_full_name", label = NULL, placeholder = "Enter applicant's full name")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "sole_special_power_attorney", 
                                                   span("Special Power of Attorney (Optional - if different from above)")),
                                        fileInput("sole_special_power_attorney", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Special Power of Attorney document")
                                    ),
                                    
                                    div(class = "document-note",
                                        tags$b("Note:"), " Special Power of Attorney is only required if the Applicant Full Name is different from the Full Name above.")
                                )
                              ),
                              
                              # Partnership
                              conditionalPanel(
                                condition = "input.ownership_type == 'partnership'",
                                div(class = "conditional-documents",
                                    h4("Partnership Information:"),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "partner1_full_name", 
                                                   span("Full Name (Partner 1)", span(class = "required", "*"))),
                                        textInput("partner1_full_name", label = NULL, placeholder = "Enter Partner 1 full name")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "partner2_full_name", 
                                                   span("Full Name (Partner 2)", span(class = "required", "*"))),
                                        textInput("partner2_full_name", label = NULL, placeholder = "Enter Partner 2 full name")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "partnership_government_id", 
                                                   span("Valid Government-Issued ID Type", span(class = "required", "*"))),
                                        selectInput("partnership_government_id", label = NULL,
                                                    choices = c("Select ID type..." = "",
                                                                "Philippine Passport" = "passport",
                                                                "Driver's License" = "driver_license",
                                                                "UMID" = "umid",
                                                                "PRC ID" = "prc_id",
                                                                "Voter's ID" = "voter_id"),
                                                    selected = "")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "partnership_government_id_image", 
                                                   span("Government ID Image", span(class = "required", "*"))),
                                        fileInput("partnership_government_id_image", label = NULL,
                                                  accept = c("image/png", "image/jpeg", "image/jpg", "application/pdf"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload image/scan of government ID")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "partnership_applicant_full_name", 
                                                   span("Applicant Full Name", span(class = "required", "*"))),
                                        textInput("partnership_applicant_full_name", label = NULL, placeholder = "Enter applicant's full name")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "partnership_special_power_attorney", 
                                                   span("Special Power of Attorney (Optional - if different from partners)")),
                                        fileInput("partnership_special_power_attorney", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Special Power of Attorney document")
                                    ),
                                    
                                    div(class = "document-note",
                                        tags$b("Note:"), " Special Power of Attorney is only required if any of the Partner's Full Name is not the same as Applicant Full Name.")
                                )
                              ),
                              
                              # Corporation
                              conditionalPanel(
                                condition = "input.ownership_type == 'corporation'",
                                div(class = "conditional-documents",
                                    h4("Corporation Information:"),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "corporation_name", 
                                                   span("Corporation Name", span(class = "required", "*"))),
                                        textInput("corporation_name", label = NULL, placeholder = "Enter corporation name")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "sec_registration", 
                                                   span("SEC Registration", span(class = "required", "*"))),
                                        fileInput("sec_registration", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload SEC Registration document")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "articles_incorporation", 
                                                   span("Articles of Incorporation", span(class = "required", "*"))),
                                        fileInput("articles_incorporation", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Articles of Incorporation")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "board_resolution", 
                                                   span("Board Resolution", span(class = "required", "*"))),
                                        fileInput("board_resolution", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Board Resolution")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "corporation_special_power_attorney", 
                                                   span("Special Power of Attorney (for the representative)", span(class = "required", "*"))),
                                        fileInput("corporation_special_power_attorney", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Special Power of Attorney")
                                    )
                                )
                              ),
                              
                              # Special Power of Attorney
                              conditionalPanel(
                                condition = "input.ownership_type == 'spa'",
                                div(class = "conditional-documents",
                                    h4("Special Power of Attorney Information:"),
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "spa_document", 
                                                   span("Special Power of Attorney Document", span(class = "required", "*"))),
                                        fileInput("spa_document", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Special Power of Attorney document")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "spa_applicant_full_name", 
                                                   span("Applicant Full Name", span(class = "required", "*"))),
                                        textInput("spa_applicant_full_name", label = NULL, placeholder = "Enter applicant's full name")
                                    )
                                )
                              )
                          ),
                          
                          # Part III: Parent Land Details
                          div(class = "survey-section",
                              h3("Part III: Parent Land Details", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "mother_lot_tech_description", 
                                             span("Mother Lot Technical Description", span(class = "required", "*"))),
                                  fileInput("mother_lot_tech_description", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Technical Description document")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "vicinity_map", 
                                             span("Vicinity Map", span(class = "required", "*"))),
                                  fileInput("vicinity_map", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Vicinity Map")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "proposed_subdivision_plan", 
                                             span("Proposed Subdivision Plan", span(class = "required", "*"))),
                                  div(style = "display: flex; align-items: center; gap: 10px;",
                                      fileInput("proposed_subdivision_plan", label = NULL,
                                                accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                buttonLabel = "Browse...",
                                                placeholder = "Upload Proposed Subdivision Plan"),
                                      tags$span(title = "Must show the new lot boundaries, dimensions, areas, road lots (if any), and open spaces. It must conform to the Local Zoning Ordinance and the National Standards",
                                                style = "cursor: help; color: #4CAF50; font-size: 18px;",
                                                "?")
                                  )
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "lot_data_computation", 
                                             span("Lot Data Computation (per house)", span(class = "required", "*"))),
                                  fileInput("lot_data_computation", label = NULL,
                                            accept = c("application/pdf", "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Lot Data Computation spreadsheet")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "num_proposed_houses", 
                                             span("Number of Proposed Houses", span(class = "required", "*"))),
                                  numericInput("num_proposed_houses", label = NULL, value = NULL, min = 1, step = 1, width = "100%")
                              )
                          ),
                          
                          # Part IV. Clearances and Permits
                          div(class = "survey-section",
                              h3("Part IV. Clearances and Permits", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "barangay_clearance_subdivision", 
                                             span("Barangay Clearance", span(class = "required", "*"))),
                                  fileInput("barangay_clearance_subdivision", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Barangay Clearance")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "zoning_compliance", 
                                             span("Zoning Compliance Certificate", span(class = "required", "*"))),
                                  fileInput("zoning_compliance", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Zoning Compliance Certificate")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "preliminary_approval", 
                                             span("Preliminary Subdivision Approval", span(class = "required", "*"))),
                                  fileInput("preliminary_approval", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Preliminary Subdivision Approval")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "ecc_certificate", 
                                             span("Environmental Compliance Certificate", span(class = "required", "*"))),
                                  fileInput("ecc_certificate", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Environmental Compliance Certificate")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "land_use_conversion", 
                                             span("Certificate of Land Use Conversion")),
                                  div(style = "display: flex; align-items: center; gap: 10px;",
                                      fileInput("land_use_conversion", label = NULL,
                                                accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                buttonLabel = "Browse...",
                                                placeholder = "Upload Certificate of Land Use Conversion"),
                                      tags$span(title = "Optional if the mother lot is classified as agricultural and will be converted to residential, commercial, or industrial use.",
                                                style = "cursor: help; color: #4CAF50; font-size: 18px;",
                                                "?")
                                  )
                              )
                          ),
                          
                          # Additional Information
                          div(class = "survey-section",
                              h3("Additional Information", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "subdivision_contact_phone", 
                                             span("Contact Phone Number", span(class = "required", "*"))),
                                  textInput("subdivision_contact_phone", label = NULL, placeholder = "Enter your contact phone number")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "subdivision_contact_email", 
                                             span("Contact Email Address", span(class = "required", "*"))),
                                  textInput("subdivision_contact_email", label = NULL, placeholder = "Enter your email address")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "subdivision_survey_reason", 
                                             span("Reason for Subdivision Survey", span(class = "required", "*"))),
                                  textAreaInput("subdivision_survey_reason", label = NULL, 
                                                placeholder = "Please describe the reason for requesting this subdivision survey...", 
                                                rows = 4)
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "subdivision_additional_notes", 
                                             span("Additional Notes (Optional)")),
                                  textAreaInput("subdivision_additional_notes", label = NULL, 
                                                placeholder = "Any additional information or special requirements...", 
                                                rows = 3)
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "subdivision_survey_urgency", 
                                             span("Urgency Level")),
                                  selectInput("subdivision_survey_urgency", label = NULL,
                                              choices = c("Normal (2-4 weeks)" = "normal",
                                                          "High Priority (1-2 weeks)" = "high",
                                                          "Urgent (3-7 days)" = "urgent",
                                                          "Emergency (1-3 days)" = "emergency"),
                                              selected = "normal")
                              )
                          ),
                          
                          # Form submission buttons
                          div(style = "display: flex; gap: 15px; margin-top: 40px; padding-top: 20px; border-top: 2px solid #E8F5E9;",
                              actionButton("submit_subdivision_survey", "Submit Subdivision Survey Request", 
                                           class = "btn-primary",
                                           style = "width: auto; min-width: 250px; flex: 1;"),
                              actionButton("clear_subdivision_form", "Clear Form", 
                                           class = "btn-secondary",
                                           style = "width: auto; min-width: 150px;")
                          )
                      )
                    } else if (selected_survey_type() == "Verification") {
                      # Verification Survey Form
                      div(class = "survey-form-container",
                          actionButton("back_to_survey_types", "← Back to Survey Types", 
                                       class = "survey-back-btn"),
                          
                          h2("Verification Survey Request Form", 
                             style = "color: #1B5E20; margin-bottom: 25px; text-align: center;"),
                          p("Please fill out all required fields for your verification survey request.", 
                            style = "color: #666; margin-bottom: 30px; text-align: center;"),
                          
                          # Part I. Applicant Information Section
                          div(class = "survey-section",
                              h3("Part I. Applicant Information", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_full_name", 
                                             span("Full Name", span(class = "required", "*"))),
                                  textInput("verification_full_name", label = NULL, placeholder = "Enter your full name")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_government_id_type", 
                                             span("Valid Government-Issued ID Type", span(class = "required", "*"))),
                                  selectInput("verification_government_id_type", label = NULL,
                                              choices = c("Select ID type..." = "",
                                                          "Philippine Passport" = "passport",
                                                          "Driver's License" = "driver_license",
                                                          "UMID" = "umid",
                                                          "PRC ID" = "prc_id",
                                                          "Voter's ID" = "voter_id"),
                                              selected = "")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_government_id_image", 
                                             span("Government ID Image", span(class = "required", "*"))),
                                  fileInput("verification_government_id_image", label = NULL, 
                                            accept = c("image/png", "image/jpeg", "image/jpg", "application/pdf"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload image/scan of your government ID")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_applicant_full_name", 
                                             span("Applicant Full Name", span(class = "required", "*"))),
                                  textInput("verification_applicant_full_name", label = NULL, placeholder = "Enter applicant's full name")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_special_power_attorney", 
                                             span("Special Power of Attorney (Optional - if different from above)")),
                                  fileInput("verification_special_power_attorney", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Special Power of Attorney document")
                              ),
                              
                              div(class = "document-note",
                                  tags$b("Note:"), " Special Power of Attorney is only required if the Applicant Full Name is different from the Full Name above.")
                          ),
                          
                          # Part II. Core Application Documents Section
                          div(class = "survey-section",
                              h3("Part II. Core Application Documents", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_application_form", 
                                             span("Duly Accomplished Application Form (LMB or DENR prescribed Form)", span(class = "required", "*"))),
                                  fileInput("verification_application_form", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload LMB/DENR Application Form")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_notarized_application", 
                                             span("Notarized Verification Survey Application", span(class = "required", "*"))),
                                  fileInput("verification_notarized_application", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Notarized Verification Survey Application")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_blueprint_copies", 
                                             span("Three Blueprint Copies of Survey Plan", span(class = "required", "*"))),
                                  fileInput("verification_blueprint_copies", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            multiple = TRUE,
                                            placeholder = "Upload three blueprint copies (you can select multiple files)")
                              ),
                              
                              div(class = "document-note",
                                  tags$b("Note:"), " Please upload all three blueprint copies. You can select multiple files by holding Ctrl/Cmd while selecting.")
                          ),
                          
                          # Part III. Survey Return / Computation Section
                          div(class = "survey-section",
                              h3("Part III. Survey Return / Computation", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_lot_data_computation", 
                                             span("Lot Data Computation", span(class = "required", "*"))),
                                  fileInput("verification_lot_data_computation", label = NULL,
                                            accept = c("application/pdf", "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Lot Data Computation document or spreadsheet")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_location_coordinates", 
                                             span("Location Coordinates", span(class = "required", "*"))),
                                  fileInput("verification_location_coordinates", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload document showing location coordinates")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_bearings_distances", 
                                             span("Observation and Computation of Bearings and Distances", span(class = "required", "*"))),
                                  fileInput("verification_bearings_distances", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload bearings and distances computation")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_monuments_description", 
                                             span("Description of Monuments Established", span(class = "required", "*"))),
                                  fileInput("verification_monuments_description", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload description of monuments established")
                              )
                          ),
                          
                          # Part IV. Supporting Documents Section
                          div(class = "survey-section",
                              h3("Part IV. Supporting Documents", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_ctc_title", 
                                             span("CTC of Title", span(class = "required", "*"))),
                                  fileInput("verification_ctc_title", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Certified True Copy of Title")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_vicinity_map", 
                                             span("Vicinity Map", span(class = "required", "*"))),
                                  fileInput("verification_vicinity_map", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Vicinity Map")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_geodetic_engineer_cert", 
                                             span("Geodetic Engineer's Certificate", span(class = "required", "*"))),
                                  fileInput("verification_geodetic_engineer_cert", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Geodetic Engineer's Certificate")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_regional_survey_clearance", 
                                             span("Regional Survey Division Clearance", span(class = "required", "*"))),
                                  fileInput("verification_regional_survey_clearance", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Regional Survey Division Clearance")
                              )
                          ),
                          
                          # Additional Information Section
                          div(class = "survey-section",
                              h3("Additional Information", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_contact_phone", 
                                             span("Contact Phone Number", span(class = "required", "*"))),
                                  textInput("verification_contact_phone", label = NULL, placeholder = "Enter your contact phone number")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_contact_email", 
                                             span("Contact Email Address", span(class = "required", "*"))),
                                  textInput("verification_contact_email", label = NULL, placeholder = "Enter your email address")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_survey_reason", 
                                             span("Reason for Verification Survey", span(class = "required", "*"))),
                                  textAreaInput("verification_survey_reason", label = NULL, 
                                                placeholder = "Please describe the reason for requesting this verification survey...", 
                                                rows = 4)
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_additional_notes", 
                                             span("Additional Notes (Optional)")),
                                  textAreaInput("verification_additional_notes", label = NULL, 
                                                placeholder = "Any additional information or special requirements...", 
                                                rows = 3)
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "verification_survey_urgency", 
                                             span("Urgency Level")),
                                  selectInput("verification_survey_urgency", label = NULL,
                                              choices = c("Normal (2-4 weeks)" = "normal",
                                                          "High Priority (1-2 weeks)" = "high",
                                                          "Urgent (3-7 days)" = "urgent",
                                                          "Emergency (1-3 days)" = "emergency"),
                                              selected = "normal")
                              )
                          ),
                          
                          # Form submission buttons
                          div(style = "display: flex; gap: 15px; margin-top: 40px; padding-top: 20px; border-top: 2px solid #E8F5E9;",
                              actionButton("submit_verification_survey", "Submit Verification Survey Request", 
                                           class = "btn-primary",
                                           style = "width: auto; min-width: 250px; flex: 1;"),
                              actionButton("clear_verification_form", "Clear Form", 
                                           class = "btn-secondary",
                                           style = "width: auto; min-width: 150px;")
                          )
                      )
                    } else if (selected_survey_type() == "Topography") {
                      # Topography Survey Form
                      div(class = "survey-form-container",
                          actionButton("back_to_survey_types", "← Back to Survey Types", 
                                       class = "survey-back-btn"),
                          
                          h2("Topography Survey Request Form", 
                             style = "color: #1B5E20; margin-bottom: 25px; text-align: center;"),
                          p("Please fill out all required fields for your topography survey request.", 
                            style = "color: #666; margin-bottom: 30px; text-align: center;"),
                          
                          # A. Basic Project Information Section
                          div(class = "survey-section",
                              h3("A. Basic Project Information", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_project_title", 
                                             span("Project Title / Purpose of Topo Survey", span(class = "required", "*"))),
                                  textInput("topo_project_title", label = NULL, 
                                            placeholder = "e.g., 'For Building Permit', 'For Road Design', 'For Site Development'")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_client_name", 
                                             span("Client Name", span(class = "required", "*"))),
                                  textInput("topo_client_name", label = NULL, placeholder = "Enter client's full name")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_project_location", 
                                             span("Project Location", span(class = "required", "*"))),
                                  textInput("topo_project_location", label = NULL, placeholder = "Enter complete project address")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_survey_area", 
                                             span("Estimated Survey Area", span(class = "required", "*"))),
                                  div(style = "display: flex; gap: 10px;",
                                      numericInput("topo_survey_area", label = NULL, value = NULL, min = 0, step = 0.01, width = "70%"),
                                      selectInput("topo_area_unit", label = NULL, 
                                                  choices = c("Square Meters" = "sqm",
                                                              "Hectares" = "hectares",
                                                              "Acres" = "acres",
                                                              "Square Feet" = "sqft"),
                                                  selected = "sqm", width = "30%")
                                  )
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_required_accuracy", 
                                             span("Required Accuracy Level", span(class = "required", "*"))),
                                  selectInput("topo_required_accuracy", label = NULL,
                                              choices = c("Select accuracy level..." = "",
                                                          "Standard (±5 cm)" = "standard",
                                                          "High Precision (±2 cm)" = "high",
                                                          "Engineering Grade (±1 cm)" = "engineering",
                                                          "Survey Grade (±0.5 cm)" = "survey"),
                                              selected = "")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_project_description", 
                                             span("Project Description", span(class = "required", "*"))),
                                  textAreaInput("topo_project_description", label = NULL, 
                                                placeholder = "Describe the project and specific requirements for the topography survey...", 
                                                rows = 4)
                              )
                          ),
                          
                          # B. Ownership Documents Section
                          div(class = "survey-section",
                              h3("B. Ownership Documents", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              p("Note: Topo surveys can be done even if the applicant is not the owner, but owner consent is recommended.", 
                                style = "color: #666; font-style: italic; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_ownership_docs", 
                                             span("Land Title OR Tax Declaration", span(class = "required", "*"))),
                                  fileInput("topo_ownership_docs", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Land Title OR Tax Declaration document")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_location_map", 
                                             span("Location Map", span(class = "required", "*"))),
                                  fileInput("topo_location_map", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload location map showing property boundaries")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_existing_plans", 
                                             span("Lot Plan or Previous Topographic Map (if available)")),
                                  fileInput("topo_existing_plans", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload existing plans or previous topographic maps")
                              )
                          ),
                          
                          # C. Authorization Documents Section
                          div(class = "survey-section",
                              h3("C. Authorization Documents", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_applicant_is_owner", 
                                             span("Is the applicant the owner of the property?", span(class = "required", "*"))),
                                  selectInput("topo_applicant_is_owner", label = NULL,
                                              choices = c("Select..." = "",
                                                          "Yes, I am the owner" = "yes",
                                                          "No, I am not the owner" = "no"),
                                              selected = "")
                              ),
                              
                              # Conditional panel for non-owners
                              conditionalPanel(
                                condition = "input.topo_applicant_is_owner == 'no'",
                                div(class = "conditional-documents",
                                    h4("Required Authorization Documents (for non-owners):"),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "topo_authorization_letter", 
                                                   span("Authorization Letter or Special Power of Attorney", span(class = "required", "*"))),
                                        fileInput("topo_authorization_letter", label = NULL,
                                                  accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload Authorization Letter or SPA")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "topo_owner_id_type", 
                                                   span("Owner's Valid Government-Issued ID Type", span(class = "required", "*"))),
                                        selectInput("topo_owner_id_type", label = NULL,
                                                    choices = c("Select ID type..." = "",
                                                                "Philippine Passport" = "passport",
                                                                "Driver's License" = "driver_license",
                                                                "UMID" = "umid",
                                                                "PRC ID" = "prc_id",
                                                                "Voter's ID" = "voter_id"),
                                                    selected = "")
                                    ),
                                    
                                    div(class = "form-group",
                                        tags$label(class = "form-label", `for` = "topo_owner_id_image", 
                                                   span("Owner's Government ID Image", span(class = "required", "*"))),
                                        fileInput("topo_owner_id_image", label = NULL,
                                                  accept = c("image/png", "image/jpeg", "image/jpg", "application/pdf"),
                                                  buttonLabel = "Browse...",
                                                  placeholder = "Upload image/scan of owner's government ID")
                                    ),
                                    
                                    div(class = "document-note",
                                        tags$b("Note:"), " If the applicant is not the owner, authorization from the property owner is required.")
                                )
                              ),
                              
                              # Always required: Applicant's ID
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_applicant_id_type", 
                                             span("Applicant's Valid Government-Issued ID Type", span(class = "required", "*"))),
                                  selectInput("topo_applicant_id_type", label = NULL,
                                              choices = c("Select ID type..." = "",
                                                          "Philippine Passport" = "passport",
                                                          "Driver's License" = "driver_license",
                                                          "UMID" = "umid",
                                                          "PRC ID" = "prc_id",
                                                          "Voter's ID" = "voter_id"),
                                              selected = "")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_applicant_id_image", 
                                             span("Applicant's Government ID Image", span(class = "required", "*"))),
                                  fileInput("topo_applicant_id_image", label = NULL,
                                            accept = c("image/png", "image/jpeg", "image/jpg", "application/pdf"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload image/scan of applicant's government ID")
                              )
                          ),
                          
                          # D. Additional Engineering/Architectural Requirements Section
                          div(class = "survey-section",
                              h3("D. Additional Engineering or Architectural Requirements", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_site_development_plan", 
                                             span("Site Development Plan")),
                                  fileInput("topo_site_development_plan", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload Site Development Plan (if available)")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_design_guidelines", 
                                             span("Architect's / Engineer's Design Guidelines")),
                                  fileInput("topo_design_guidelines", label = NULL,
                                            accept = c("application/pdf", "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload design guidelines or specifications")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_dem_specs", 
                                             span("DEM / Project Specifications")),
                                  fileInput("topo_dem_specs", label = NULL,
                                            accept = c("application/pdf", "application/vnd.ms-excel", 
                                                       "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                                       "image/png", "image/jpeg", "image/jpg"),
                                            buttonLabel = "Browse...",
                                            placeholder = "Upload DEM files or project specifications from contractor")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_grid_spacing", 
                                             span("Required Grid Spacing / Point Density")),
                                  div(style = "display: flex; gap: 10px;",
                                      numericInput("topo_grid_spacing", label = NULL, value = NULL, min = 0.1, step = 0.1, width = "70%"),
                                      selectInput("topo_grid_unit", label = NULL, 
                                                  choices = c("Meters" = "m",
                                                              "Centimeters" = "cm",
                                                              "Feet" = "ft"),
                                                  selected = "m", width = "30%")
                                  )
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_deliverables", 
                                             span("Required Deliverables")),
                                  checkboxGroupInput("topo_deliverables", label = NULL,
                                                     choices = c("2D Contour Map" = "2d_contour",
                                                                 "3D Digital Terrain Model (DTM)" = "3d_dtm",
                                                                 "Cross Sections" = "cross_sections",
                                                                 "Volume Calculations" = "volume_calc",
                                                                 "CAD Files (DWG/DXF)" = "cad_files",
                                                                 "GIS-Compatible Files" = "gis_files",
                                                                 "PDF Report with Analysis" = "pdf_report"),
                                                     selected = c("2d_contour", "pdf_report"))
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_special_requirements", 
                                             span("Special Requirements")),
                                  textAreaInput("topo_special_requirements", label = NULL, 
                                                placeholder = "Any special survey requirements, equipment needs, or specific features to be captured...", 
                                                rows = 3)
                              )
                          ),
                          
                          # Additional Information Section
                          div(class = "survey-section",
                              h3("Additional Information", 
                                 style = "color: #2E7D32; border-bottom: 2px solid #C8E6C9; padding-bottom: 10px; margin-bottom: 20px;"),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_contact_phone", 
                                             span("Contact Phone Number", span(class = "required", "*"))),
                                  textInput("topo_contact_phone", label = NULL, placeholder = "Enter your contact phone number")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_contact_email", 
                                             span("Contact Email Address", span(class = "required", "*"))),
                                  textInput("topo_contact_email", label = NULL, placeholder = "Enter your email address")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_preferred_date", 
                                             span("Preferred Survey Date")),
                                  dateInput("topo_preferred_date", label = NULL, value = Sys.Date() + 7)
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_survey_urgency", 
                                             span("Urgency Level")),
                                  selectInput("topo_survey_urgency", label = NULL,
                                              choices = c("Normal (2-4 weeks)" = "normal",
                                                          "High Priority (1-2 weeks)" = "high",
                                                          "Urgent (3-7 days)" = "urgent",
                                                          "Emergency (1-3 days)" = "emergency"),
                                              selected = "normal")
                              ),
                              
                              div(class = "form-group",
                                  tags$label(class = "form-label", `for` = "topo_additional_notes", 
                                             span("Additional Notes (Optional)")),
                                  textAreaInput("topo_additional_notes", label = NULL, 
                                                placeholder = "Any additional information or special requirements...", 
                                                rows = 3)
                              )
                          ),
                          
                          # Form submission buttons
                          div(style = "display: flex; gap: 15px; margin-top: 40px; padding-top: 20px; border-top: 2px solid #E8F5E9;",
                              actionButton("submit_topo_survey", "Submit Topography Survey Request", 
                                           class = "btn-primary",
                                           style = "width: auto; min-width: 250px; flex: 1;"),
                              actionButton("clear_topo_form", "Clear Form", 
                                           class = "btn-secondary",
                                           style = "width: auto; min-width: 150px;")
                          )
                      )
                    }
                  } else if (dashboard_tab() == "profile") {
                    div(
                      h2("User Profile", style = "color: #1B5E20; margin-bottom: 25px;"),
                      div(class = "form-group",
                          tags$label(class = "form-label", "Full Name"),
                          textInput("profile_name", label = NULL, value = user_session$full_name)
                      ),
                      div(class = "form-group",
                          tags$label(class = "form-label", "Email Address"),
                          textInput("profile_email", label = NULL, value = user_session$email)
                      ),
                      div(class = "form-group",
                          tags$label(class = "form-label", "Phone Number"),
                          textInput("profile_phone", label = NULL, placeholder = "Enter your phone number")
                      ),
                      div(class = "form-group",
                          tags$label(class = "form-label", "Company/Organization"),
                          textInput("profile_company", label = NULL, placeholder = "Enter your company name")
                      ),
                      div(class = "form-group",
                          tags$label(class = "form-label", "Professional Title"),
                          textInput("profile_title", label = NULL, placeholder = "Enter your professional title")
                      ),
                      div(class = "form-group",
                          tags$label(class = "form-label", "Biography"),
                          textAreaInput("profile_bio", label = NULL, placeholder = "Tell us about yourself...", rows = 4)
                      ),
                      div(style = "display: flex; gap: 15px; margin-top: 30px;",
                          actionButton("update_profile", "Update Profile", class = "btn-primary",
                                       style = "width: auto; min-width: 150px;"),
                          actionButton("change_password", "Change Password", class = "btn-secondary",
                                       style = "width: auto; min-width: 150px;")
                      )
                    )
                  }
              )
          )
      )
    } else if (current_page() == "privacy") {
      # Privacy Policy Page
      div(class = "policy-container",
          div(class = "policy-content",
              div(class = "policy-card",
                  h1(class = "policy-title", "Privacy Policy"),
                  div(class = "policy-section",
                      h2("1. Introduction"),
                      p("Welcome to FieldConektiv. We respect your privacy and are committed to protecting your personal data. This privacy policy will inform you about how we look after your personal data when you visit our website and tell you about your privacy rights and how the law protects you."),
                      
                      h2("2. Data We Collect"),
                      h3("Personal Identification Information"),
                      p("We may collect personal identification information from Users in a variety of ways, including, but not limited to:"),
                      tags$ul(
                        tags$li("Full name"),
                        tags$li("Email address"),
                        tags$li("Phone number"),
                        tags$li("Password (encrypted)"),
                        tags$li("Professional credentials and certifications")
                      ),
                      
                      h3("Technical Data"),
                      p("We automatically collect certain information when you visit our website:"),
                      tags$ul(
                        tags$li("IP address"),
                        tags$li("Browser type and version"),
                        tags$li("Time zone setting and location"),
                        tags$li("Operating system and platform"),
                        tags$li("Other technology on the devices you use to access this website")
                      ),
                      
                      h2("3. How We Use Your Data"),
                      p("We use your personal data for the following purposes:"),
                      tags$ul(
                        tags$li("To create and manage your account"),
                        tags$li("To provide and maintain our services"),
                        tags$li("To notify you about changes to our services"),
                        tags$li("To provide customer support"),
                        tags$li("To gather analysis or valuable information to improve our services"),
                        tags$li("To monitor the usage of our services"),
                        tags$li("To detect, prevent and address technical issues")
                      ),
                      
                      h2("4. Data Security"),
                      p("We implement appropriate security measures to protect your personal data:"),
                      tags$ul(
                        tags$li("Password encryption using bcrypt hashing algorithm"),
                        tags$li("Secure database connections"),
                        tags$li("Regular security audits"),
                        tags$li("Access controls and authentication"),
                        tags$li("Data encryption in transit and at rest")
                      ),
                      
                      h2("5. Data Retention"),
                      p("We will retain your personal data only for as long as is necessary for the purposes set out in this privacy policy. We will retain and use your personal data to the extent necessary to comply with our legal obligations, resolve disputes, and enforce our legal agreements and policies."),
                      
                      h2("6. Your Rights"),
                      p("You have the right to:"),
                      tags$ul(
                        tags$li("Access your personal data"),
                        tags$li("Correct inaccurate personal data"),
                        tags$li("Request deletion of your personal data"),
                        tags$li("Object to processing of your personal data"),
                        tags$li("Request transfer of your personal data"),
                        tags$li("Withdraw consent at any time")
                      ),
                      
                      h2("7. Changes to This Policy"),
                      p("We may update our privacy policy from time to time. We will notify you of any changes by posting the new privacy policy on this page and updating the 'Last Updated' date."),
                      
                      h2("8. Contact Us"),
                      p("If you have any questions about this privacy policy, please contact us:"),
                      div(class = "contact-info",
                          div(class = "contact-item",
                              icon("envelope"),
                              span("Email: privacy@fieldconektiv.com")
                          ),
                          div(class = "contact-item",
                              icon("phone"),
                              span("Phone: +1 (555) 123-4567")
                          ),
                          div(class = "contact-item",
                              icon("map-marker-alt"),
                              span("Address: 123 Tech Street, Innovation City, IC 12345")
                          )
                      ),
                      
                      p("Last Updated: December 2023")
                  )
              )
          )
      )
    } else if (current_page() == "terms") {
      # Terms of Service Page
      div(class = "policy-container",
          div(class = "policy-content",
              div(class = "policy-card",
                  h1(class = "policy-title", "Terms of Service"),
                  div(class = "policy-section",
                      h2("1. Acceptance of Terms"),
                      p("By accessing and using FieldConektiv, you accept and agree to be bound by the terms and provision of this agreement. If you do not agree to abide by these terms, please do not use this service."),
                      
                      h2("2. Description of Service"),
                      p("FieldConektiv provides a comprehensive platform for managing land survey operations, design works, construction works, and other related services. The service includes:"),
                      tags$ul(
                        tags$li("User account creation and management"),
                        tags$li("Project management tools"),
                        tags$li("Document storage and sharing"),
                        tags$li("Communication tools"),
                        tags$li("Reporting and analytics")
                      ),
                      
                      h2("3. User Accounts"),
                      h3("3.1 Account Creation"),
                      p("To use our services, you must create an account by providing accurate and complete information. You are responsible for maintaining the confidentiality of your account and password."),
                      
                      h3("3.2 Account Security"),
                      p("You agree to:"),
                      tags$ul(
                        tags$li("Keep your password secure and confidential"),
                        tags$li("Notify us immediately of any unauthorized use of your account"),
                        tags$li("Take responsibility for all activities that occur under your account")
                      ),
                      
                      h3("3.3 Account Termination"),
                      p("We reserve the right to suspend or terminate your account at any time for conduct that we believe violates these terms or is harmful to other users, us, or third parties, or for any other reason."),
                      
                      h2("4. User Responsibilities"),
                      p("As a user of FieldConektiv, you agree to:"),
                      tags$ul(
                        tags$li("Provide accurate and complete information"),
                        tags$li("Use the service only for lawful purposes"),
                        tags$li("Not attempt to gain unauthorized access to the system"),
                        tags$li("Not interfere with or disrupt the service"),
                        tags$li("Not use the service to transmit viruses or malicious code"),
                        tags$li("Not use the service for any fraudulent or illegal activities")
                      ),
                      
                      h2("5. Intellectual Property"),
                      p("All content included on this site, such as text, graphics, logos, button icons, images, audio clips, digital downloads, data compilations, and software, is the property of FieldConektiv or its content suppliers and protected by international copyright laws."),
                      
                      h2("6. Limitation of Liability"),
                      p("FieldConektiv shall not be liable for any indirect, incidental, special, consequential or punitive damages, including without limitation, loss of profits, data, use, goodwill, or other intangible losses, resulting from:"),
                      tags$ul(
                        tags$li("Your access to or use of or inability to access or use the service"),
                        tags$li("Any conduct or content of any third party on the service"),
                        tags$li("Any content obtained from the service"),
                        tags$li("Unauthorized access, use or alteration of your transmissions or content")
                      ),
                      
                      h2("7. Service Modifications"),
                      p("We reserve the right to modify or discontinue, temporarily or permanently, the service (or any part thereof) with or without notice. We shall not be liable to you or to any third party for any modification, price change, suspension, or discontinuance of the service."),
                      
                      h2("8. Governing Law"),
                      p("These terms shall be governed and construed in accordance with the laws of the jurisdiction in which FieldConektiv operates, without regard to its conflict of law provisions."),
                      
                      h2("9. Changes to Terms"),
                      p("We reserve the right, at our sole discretion, to modify or replace these terms at any time. By continuing to access or use our service after those revisions become effective, you agree to be bound by the revised terms."),
                      
                      h2("10. Contact Information"),
                      div(class = "contact-info",
                          div(class = "contact-item",
                              icon("envelope"),
                              span("Email: legal@fieldconektiv.com")
                          ),
                          div(class = "contact-item",
                              icon("phone"),
                              span("Phone: +1 (555) 987-6543")
                          ),
                          div(class = "contact-item",
                              icon("clock"),
                              span("Business Hours: Mon-Fri 9:00 AM - 5:00 PM EST")
                          )
                      ),
                      
                      p("Effective Date: December 2023")
                  )
              )
          )
      )
    } else if (current_page() == "contact") {
      # Contact Support Page
      div(class = "policy-container",
          div(class = "policy-content",
              div(class = "policy-card",
                  h1(class = "policy-title", "Contact Support"),
                  div(class = "policy-section",
                      h2("Get in Touch"),
                      p("We're here to help! Choose from the following options to get support:"),
                      
                      h2("Contact Information"),
                      div(class = "contact-info",
                          h3("General Inquiries"),
                          div(class = "contact-item",
                              icon("envelope"),
                              span("Email: info@fieldconektiv.com")
                          ),
                          div(class = "contact-item",
                              icon("phone"),
                              span("Phone: +1 (800) 555-FIELD")
                          ),
                          
                          h3("Technical Support"),
                          div(class = "contact-item",
                              icon("envelope"),
                              span("Email: support@fieldconektiv.com")
                          ),
                          div(class = "contact-item",
                              icon("phone"),
                              span("Phone: +1 (800) 555-TECH")
                          ),
                          
                          h3("Billing Department"),
                          div(class = "contact-item",
                              icon("envelope"),
                              span("Email: billing@fieldconektiv.com")
                          ),
                          div(class = "contact-item",
                              icon("phone"),
                              span("Phone: +1 (800) 555-BILL")
                          ),
                          
                          h3("Headquarters"),
                          div(class = "contact-item",
                              icon("map-marker-alt"),
                              span("FieldConektiv Inc.")
                          ),
                          div(class = "contact-item",
                              icon(""),
                              span("123 Technology Drive")
                          ),
                          div(class = "contact-item",
                              icon(""),
                              span("Suite 500")
                          ),
                          div(class = "contact-item",
                              icon(""),
                              span("San Francisco, CA 94107")
                          ),
                          div(class = "contact-item",
                              icon(""),
                              span("United States")
                          )
                      ),
                      
                      h2("Business Hours"),
                      p("Our support teams are available during the following hours:"),
                      tags$ul(
                        tags$li("Monday - Friday: 8:00 AM - 8:00 PM EST"),
                        tags$li("Saturday: 9:00 AM - 5:00 PM EST"),
                        tags$li("Sunday: 10:00 AM - 4:00 PM EST"),
                        tags$li("Emergency Support: 24/7 for critical issues")
                      ),
                      
                      h2("Send Us a Message"),
                      div(class = "contact-form",
                          p("Fill out the form below and we'll get back to you within 24 hours."),
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "contact-name", "Your Name"),
                              textInput("contact_name", label = NULL, placeholder = "Enter your name")
                          ),
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "contact-email", "Email Address"),
                              textInput("contact_email", label = NULL, placeholder = "Enter your email")
                          ),
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "contact-subject", "Subject"),
                              textInput("contact_subject", label = NULL, placeholder = "Enter subject")
                          ),
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "contact-message", "Message"),
                              textAreaInput("contact_message", label = NULL, placeholder = "Type your message here...", rows = 5)
                          ),
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "contact-urgency", "Urgency Level"),
                              selectInput("contact_urgency", label = NULL,
                                          choices = c("Low" = "low", 
                                                      "Medium" = "medium", 
                                                      "High" = "high", 
                                                      "Critical" = "critical"),
                                          selected = "medium")
                          ),
                          actionButton("send_message", "Send Message", class = "btn-primary",
                                       style = "width: auto; min-width: 200px;")
                      ),
                      
                      h2("Frequently Asked Questions"),
                      h3("How quickly will I get a response?"),
                      p("We typically respond to all inquiries within 24 hours during business days. Emergency support requests are addressed immediately."),
                      
                      h3("Do you offer phone support?"),
                      p("Yes, we offer phone support during business hours. For urgent matters outside business hours, please use the emergency contact number provided in your account dashboard."),
                      
                      h3("Can I schedule a demo?"),
                      p("Absolutely! Contact our sales team at demo@fieldconektiv.com to schedule a personalized demo of our platform."),
                      
                      h3("Where can I find documentation?"),
                      p("Visit our documentation portal at docs.fieldconektiv.com for user guides, API documentation, and troubleshooting articles.")
                  )
              )
          )
      )
    }
  })
  
  # Navigation handlers
  observeEvent(input$go_to_signup, {
    current_page("signup")
  })
  
  observeEvent(input$back_to_home, {
    current_page("home")
  })
  
  observeEvent(input$go_to_login, {
    current_page("home")
  })
  
  observeEvent(input$logo, {
    current_page("home")
  })
  
  observeEvent(input$go_to_dashboard, {
    current_page("dashboard")
  })
  
  observeEvent(input$dashboard_btn, {
    current_page("dashboard")
  })
  
  # Footer link handlers
  observeEvent(input$privacy_link, {
    current_page("privacy")
  })
  
  observeEvent(input$terms_link, {
    current_page("terms")
  })
  
  observeEvent(input$contact_link, {
    current_page("contact")
  })
  
  # Signup form link handlers
  observeEvent(input$signup_terms_link, {
    current_page("terms")
  })
  
  observeEvent(input$signup_privacy_link, {
    current_page("privacy")
  })
  
  # Dashboard navigation handlers
  observeEvent(input$nav_dashboard, {
    dashboard_tab("dashboard")
    selected_survey_type(NULL)
  })
  
  observeEvent(input$nav_create_survey, {
    dashboard_tab("create_survey")
    selected_survey_type(NULL)
  })
  
  observeEvent(input$nav_profile, {
    dashboard_tab("profile")
    selected_survey_type(NULL)
  })
  
  observeEvent(input$nav_logout, {
    # Logout user
    user_session$logged_in <- FALSE
    user_session$user_id <- NULL
    user_session$full_name <- NULL
    user_session$email <- NULL
    
    # Navigate to home
    current_page("home")
    
    showNotification("You have been logged out successfully.", type = "default")
  })
  
  # Survey type selection handlers
  observeEvent(input$select_relocation, {
    selected_survey_type("Relocation")
  })
  
  observeEvent(input$select_subdivision, {
    selected_survey_type("Subdivision")
  })
  
  observeEvent(input$select_verification, {
    selected_survey_type("Verification")
  })
  
  observeEvent(input$select_topography, {
    selected_survey_type("Topography")
  })
  
  observeEvent(input$back_to_survey_types, {
    selected_survey_type(NULL)
  })
  
  # Handle Subdivision Survey submission
  observeEvent(input$submit_subdivision_survey, {
    # Validate required fields
    required_fields <- list(
      "Mother Lot Title" = input$mother_lot_title,
      "Tax Receipt" = input$tax_receipt,
      "No Tax Delinquency Certificate" = input$no_tax_delinquency,
      "Ownership Type" = input$ownership_type,
      "Mother Lot Technical Description" = input$mother_lot_tech_description,
      "Vicinity Map" = input$vicinity_map,
      "Proposed Subdivision Plan" = input$proposed_subdivision_plan,
      "Lot Data Computation" = input$lot_data_computation,
      "Number of Proposed Houses" = input$num_proposed_houses,
      "Barangay Clearance" = input$barangay_clearance_subdivision,
      "Zoning Compliance Certificate" = input$zoning_compliance,
      "Preliminary Subdivision Approval" = input$preliminary_approval,
      "Environmental Compliance Certificate" = input$ecc_certificate,
      "Contact Phone" = input$subdivision_contact_phone,
      "Contact Email" = input$subdivision_contact_email,
      "Survey Reason" = input$subdivision_survey_reason
    )
    
    # Check conditional requirements based on ownership type
    ownership_type <- input$ownership_type
    
    if (ownership_type == "sole") {
      required_fields <- c(required_fields, list(
        "Sole Proprietor Full Name" = input$sole_full_name,
        "Sole Proprietor Government ID Type" = input$sole_government_id,
        "Sole Proprietor Government ID Image" = input$sole_government_id_image,
        "Sole Proprietor Applicant Full Name" = input$sole_applicant_full_name
      ))
    } else if (ownership_type == "partnership") {
      required_fields <- c(required_fields, list(
        "Partner 1 Full Name" = input$partner1_full_name,
        "Partner 2 Full Name" = input$partner2_full_name,
        "Partnership Government ID Type" = input$partnership_government_id,
        "Partnership Government ID Image" = input$partnership_government_id_image,
        "Partnership Applicant Full Name" = input$partnership_applicant_full_name
      ))
    } else if (ownership_type == "corporation") {
      required_fields <- c(required_fields, list(
        "Corporation Name" = input$corporation_name,
        "SEC Registration" = input$sec_registration,
        "Articles of Incorporation" = input$articles_incorporation,
        "Board Resolution" = input$board_resolution,
        "Corporation Special Power of Attorney" = input$corporation_special_power_attorney
      ))
    } else if (ownership_type == "spa") {
      required_fields <- c(required_fields, list(
        "SPA Document" = input$spa_document,
        "SPA Applicant Full Name" = input$spa_applicant_full_name
      ))
    }
    
    missing_fields <- c()
    for (field_name in names(required_fields)) {
      value <- required_fields[[field_name]]
      if (is.null(value) || (is.character(value) && value == "") || 
          (is.numeric(value) && is.na(value))) {
        missing_fields <- c(missing_fields, field_name)
      }
    }
    
    if (length(missing_fields) > 0) {
      showNotification(
        paste("Please fill in all required fields:", 
              paste(missing_fields, collapse = ", ")),
        type = "warning",
        duration = 10
      )
      return()
    }
    
    # Validate email format
    if (!grepl("^[^@]+@[^@]+\\.[^@]+$", input$subdivision_contact_email)) {
      showNotification("Please enter a valid email address.", type = "warning")
      return()
    }
    
    # Validate number of proposed houses
    if (is.na(input$num_proposed_houses) || input$num_proposed_houses < 1) {
      showNotification("Number of proposed houses must be at least 1.", type = "warning")
      return()
    }
    
    # All validation passed - show success message
    showNotification(
      HTML(paste(
        "<h4>Subdivision Survey Request Submitted Successfully!</h4>",
        "<p><strong>Reference Number:</strong> SUBDIV-", 
        format(Sys.time(), "%Y%m%d%H%M%S"), "</p>",
        "<p><strong>Ownership Type:</strong> ", tools::toTitleCase(gsub("_", " ", ownership_type)), "</p>",
        "<p><strong>Number of Proposed Houses:</strong> ", input$num_proposed_houses, "</p>",
        "<p>We will review your documents and contact you within 5 business days.</p>"
      )),
      type = "success",
      duration = 15
    )
    
    # Reset form and return to survey types
    shinyjs::reset("subdivision-survey-form")
    selected_survey_type(NULL)
  })
  
  # Handle clearing the Subdivision Survey form
  observeEvent(input$clear_subdivision_form, {
    # Reset all inputs in the subdivision form
    updateSelectInput(session, "ownership_type", selected = "")
    updateNumericInput(session, "num_proposed_houses", value = NULL)
    updateTextInput(session, "subdivision_contact_phone", value = "")
    updateTextInput(session, "subdivision_contact_email", value = "")
    updateTextAreaInput(session, "subdivision_survey_reason", value = "")
    updateTextAreaInput(session, "subdivision_additional_notes", value = "")
    updateSelectInput(session, "subdivision_survey_urgency", selected = "normal")
    
    # Reset sole proprietorship fields
    updateTextInput(session, "sole_full_name", value = "")
    updateSelectInput(session, "sole_government_id", selected = "")
    updateTextInput(session, "sole_applicant_full_name", value = "")
    
    # Reset partnership fields
    updateTextInput(session, "partner1_full_name", value = "")
    updateTextInput(session, "partner2_full_name", value = "")
    updateSelectInput(session, "partnership_government_id", selected = "")
    updateTextInput(session, "partnership_applicant_full_name", value = "")
    
    # Reset corporation fields
    updateTextInput(session, "corporation_name", value = "")
    
    # Reset SPA fields
    updateTextInput(session, "spa_applicant_full_name", value = "")
    
    showNotification("Subdivision form cleared successfully.", type = "default", duration = 3)
  })
  
  # Handle Topography Survey submission
  observeEvent(input$submit_topo_survey, {
    # Validate required fields
    required_fields <- list(
      "Project Title" = input$topo_project_title,
      "Client Name" = input$topo_client_name,
      "Project Location" = input$topo_project_location,
      "Estimated Survey Area" = input$topo_survey_area,
      "Required Accuracy Level" = input$topo_required_accuracy,
      "Project Description" = input$topo_project_description,
      "Land Title OR Tax Declaration" = input$topo_ownership_docs,
      "Location Map" = input$topo_location_map,
      "Is applicant the owner?" = input$topo_applicant_is_owner,
      "Applicant's ID Type" = input$topo_applicant_id_type,
      "Applicant's ID Image" = input$topo_applicant_id_image,
      "Contact Phone" = input$topo_contact_phone,
      "Contact Email" = input$topo_contact_email
    )
    
    # Add conditional requirements for non-owners
    if (!is.null(input$topo_applicant_is_owner) && input$topo_applicant_is_owner == "no") {
      required_fields <- c(required_fields, list(
        "Authorization Letter/SPA" = input$topo_authorization_letter,
        "Owner's ID Type" = input$topo_owner_id_type,
        "Owner's ID Image" = input$topo_owner_id_image
      ))
    }
    
    missing_fields <- c()
    for (field_name in names(required_fields)) {
      value <- required_fields[[field_name]]
      if (is.null(value) || (is.character(value) && value == "") || 
          (is.numeric(value) && is.na(value))) {
        missing_fields <- c(missing_fields, field_name)
      }
    }
    
    if (length(missing_fields) > 0) {
      showNotification(
        paste("Please fill in all required fields:", 
              paste(missing_fields, collapse = ", ")),
        type = "warning",
        duration = 10
      )
      return()
    }
    
    # Validate email format
    if (!grepl("^[^@]+@[^@]+\\.[^@]+$", input$topo_contact_email)) {
      showNotification("Please enter a valid email address.", type = "warning")
      return()
    }
    
    # Validate survey area
    if (is.na(input$topo_survey_area) || input$topo_survey_area <= 0) {
      showNotification("Survey area must be greater than 0.", type = "warning")
      return()
    }
    
    # All validation passed - show success message
    deliverables_text <- ""
    if (!is.null(input$topo_deliverables)) {
      deliverables_text <- paste("Deliverables:", paste(input$topo_deliverables, collapse = ", "))
    }
    
    owner_status <- ifelse(input$topo_applicant_is_owner == "yes", "Applicant is the owner", "Applicant is NOT the owner")
    
    showNotification(
      HTML(paste(
        "<h4>Topography Survey Request Submitted Successfully!</h4>",
        "<p><strong>Reference Number:</strong> TOPO-", 
        format(Sys.time(), "%Y%m%d%H%M%S"), "</p>",
        "<p><strong>Project:</strong> ", input$topo_project_title, "</p>",
        "<p><strong>Location:</strong> ", input$topo_project_location, "</p>",
        "<p><strong>Survey Area:</strong> ", input$topo_survey_area, " ", input$topo_area_unit, "</p>",
        "<p><strong>Accuracy Level:</strong> ", tools::toTitleCase(input$topo_required_accuracy), "</p>",
        "<p><strong>Owner Status:</strong> ", owner_status, "</p>",
        ifelse(nchar(deliverables_text) > 0, paste("<p>", deliverables_text, "</p>"), ""),
        "<p>Our survey team will contact you within 2 business days to schedule the survey.</p>"
      )),
      type = "success",
      duration = 15
    )
    
    # Reset form and return to survey types
    shinyjs::reset("topo-survey-form")
    selected_survey_type(NULL)
  })
  
  # Handle clearing the Topography Survey form
  observeEvent(input$clear_topo_form, {
    # Reset all inputs in the topography form
    updateTextInput(session, "topo_project_title", value = "")
    updateTextInput(session, "topo_client_name", value = "")
    updateTextInput(session, "topo_project_location", value = "")
    updateNumericInput(session, "topo_survey_area", value = NULL)
    updateSelectInput(session, "topo_area_unit", selected = "sqm")
    updateSelectInput(session, "topo_required_accuracy", selected = "")
    updateTextAreaInput(session, "topo_project_description", value = "")
    updateSelectInput(session, "topo_applicant_is_owner", selected = "")
    updateSelectInput(session, "topo_applicant_id_type", selected = "")
    updateSelectInput(session, "topo_owner_id_type", selected = "")
    updateTextInput(session, "topo_contact_phone", value = "")
    updateTextInput(session, "topo_contact_email", value = "")
    updateDateInput(session, "topo_preferred_date", value = Sys.Date() + 7)
    updateSelectInput(session, "topo_survey_urgency", selected = "normal")
    updateNumericInput(session, "topo_grid_spacing", value = NULL)
    updateSelectInput(session, "topo_grid_unit", selected = "m")
    updateCheckboxGroupInput(session, "topo_deliverables", selected = c("2d_contour", "pdf_report"))
    updateTextAreaInput(session, "topo_special_requirements", value = "")
    updateTextAreaInput(session, "topo_additional_notes", value = "")
    
    showNotification("Topography form cleared successfully.", type = "default", duration = 3)
  })
  
  # Handle Relocation Survey submission
  observeEvent(input$submit_relocation_survey, {
    # Validate required fields
    required_fields <- list(
      "Full Name" = input$relocation_full_name,
      "Government ID Type" = input$government_id_type,
      "Government ID Image" = input$government_id_image,
      "Applicant Full Name" = input$applicant_full_name,
      "Land Title Status" = input$land_title_status,
      "Block Number" = input$block_number,
      "Lot Number" = input$lot_number,
      "Area" = input$area,
      "Contact Phone" = input$contact_phone,
      "Contact Email" = input$contact_email,
      "Survey Reason" = input$survey_reason,
      "Barangay Clearance" = input$barangay_clearance,
      "Tax Delinquency Certificate" = input$tax_delinquency_cert
    )
    
    missing_fields <- c()
    for (field_name in names(required_fields)) {
      value <- required_fields[[field_name]]
      if (is.null(value) || (is.character(value) && value == "") || 
          (is.numeric(value) && is.na(value))) {
        missing_fields <- c(missing_fields, field_name)
      }
    }
    
    if (length(missing_fields) > 0) {
      showNotification(
        paste("Please fill in all required fields:", 
              paste(missing_fields, collapse = ", ")),
        type = "warning",
        duration = 10
      )
      return()
    }
    
    # Validate conditional documents based on land title status
    title_status <- input$land_title_status
    
    if (title_status == "available") {
      if (is.null(input$original_title)) {
        showNotification("Please upload the Original Certificate of Title for Available land.", 
                         type = "warning", duration = 10)
        return()
      }
    } else if (title_status == "lost") {
      if (is.null(input$ctc_title) || is.null(input$owners_duplicate) || is.null(input$tax_declaration_lost)) {
        showNotification("Please upload all required documents for Lost title.", 
                         type = "warning", duration = 10)
        return()
      }
    } else if (title_status == "untitled") {
      if (is.null(input$deed_conveyance) || is.null(input$tax_declaration_untitled) || is.null(input$tax_clearance)) {
        showNotification("Please upload all required documents for Untitled land.", 
                         type = "warning", duration = 10)
        return()
      }
    } else if (title_status == "inheritance") {
      if (is.null(input$estate_settlement) || is.null(input$affidavit_adjudication) || is.null(input$death_certificate)) {
        showNotification("Please upload all required documents for Inherited land.", 
                         type = "warning", duration = 10)
        return()
      }
    }
    
    # Validate email format
    if (!grepl("^[^@]+@[^@]+\\.[^@]+$", input$contact_email)) {
      showNotification("Please enter a valid email address.", type = "warning")
      return()
    }
    
    # All validation passed - show success message
    showNotification(
      HTML(paste(
        "<h4>Relocation Survey Request Submitted Successfully!</h4>",
        "<p><strong>Reference Number:</strong> RELOC-", 
        format(Sys.time(), "%Y%m%d%H%M%S"), "</p>",
        "<p><strong>Applicant:</strong> ", input$applicant_full_name, "</p>",
        "<p><strong>Land Title Status:</strong> ", tools::toTitleCase(title_status), "</p>",
        "<p>We will review your documents and contact you within 3 business days.</p>"
      )),
      type = "success",
      duration = 15
    )
    
    # Reset form and return to survey types
    shinyjs::reset("relocation-survey-form")
    selected_survey_type(NULL)
  })
  
  # Handle clearing the Relocation Survey form
  observeEvent(input$clear_relocation_form, {
    # Reset all inputs in the relocation form
    updateTextInput(session, "relocation_full_name", value = "")
    updateSelectInput(session, "government_id_type", selected = "")
    updateTextInput(session, "applicant_full_name", value = "")
    updateSelectInput(session, "land_title_status", selected = "")
    updateNumericInput(session, "block_number", value = NULL)
    updateNumericInput(session, "lot_number", value = NULL)
    updateTextInput(session, "subdivision_name", value = "")
    updateNumericInput(session, "area", value = NULL)
    updateSelectInput(session, "area_unit", selected = "sqm")
    updateTextInput(session, "contact_phone", value = "")
    updateTextInput(session, "contact_email", value = "")
    updateTextAreaInput(session, "survey_reason", value = "")
    updateTextAreaInput(session, "additional_notes", value = "")
    updateSelectInput(session, "survey_urgency", selected = "normal")
    
    showNotification("Form cleared successfully.", type = "default", duration = 3)
  })
  
  observeEvent(input$submit_verification_survey, {
    # Validate required fields
    required_fields <- list(
      "Full Name" = input$verification_full_name,
      "Government ID Type" = input$verification_government_id_type,
      "Government ID Image" = input$verification_government_id_image,
      "Applicant Full Name" = input$verification_applicant_full_name,
      "Application Form" = input$verification_application_form,
      "Notarized Application" = input$verification_notarized_application,
      "Blueprint Copies" = input$verification_blueprint_copies,
      "Lot Data Computation" = input$verification_lot_data_computation,
      "Location Coordinates" = input$verification_location_coordinates,
      "Bearings and Distances" = input$verification_bearings_distances,
      "Monuments Description" = input$verification_monuments_description,
      "CTC of Title" = input$verification_ctc_title,
      "Vicinity Map" = input$verification_vicinity_map,
      "Geodetic Engineer's Certificate" = input$verification_geodetic_engineer_cert,
      "Regional Survey Clearance" = input$verification_regional_survey_clearance,
      "Contact Phone" = input$verification_contact_phone,
      "Contact Email" = input$verification_contact_email,
      "Survey Reason" = input$verification_survey_reason
    )
    
    missing_fields <- c()
    for (field_name in names(required_fields)) {
      value <- required_fields[[field_name]]
      if (is.null(value) || (is.character(value) && value == "") || 
          (is.numeric(value) && is.na(value))) {
        missing_fields <- c(missing_fields, field_name)
      }
    }
    
    # Special check for blueprint copies (needs exactly 3 files)
    if (!is.null(input$verification_blueprint_copies) && 
        length(input$verification_blueprint_copies$name) < 3) {
      missing_fields <- c(missing_fields, "Blueprint Copies (need 3 copies)")
    }
    
    if (length(missing_fields) > 0) {
      showNotification(
        paste("Please fill in all required fields:", 
              paste(missing_fields, collapse = ", ")),
        type = "warning",
        duration = 10
      )
      return()
    }
    
    # Validate email format
    if (!grepl("^[^@]+@[^@]+\\.[^@]+$", input$verification_contact_email)) {
      showNotification("Please enter a valid email address.", type = "warning")
      return()
    }
    
    # Validate blueprint copies count
    if (length(input$verification_blueprint_copies$name) != 3) {
      showNotification("Please upload exactly three blueprint copies.", type = "warning")
      return()
    }
    
    # All validation passed - show success message
    showNotification(
      HTML(paste(
        "<h4>Verification Survey Request Submitted Successfully!</h4>",
        "<p><strong>Reference Number:</strong> VERIFY-", 
        format(Sys.time(), "%Y%m%d%H%M%S"), "</p>",
        "<p><strong>Applicant:</strong> ", input$verification_applicant_full_name, "</p>",
        "<p><strong>Blueprint Copies Uploaded:</strong> ", length(input$verification_blueprint_copies$name), "</p>",
        "<p>We will process your verification survey request and contact you within 5 business days.</p>"
      )),
      type = "success",
      duration = 15
    )
    
    # Reset form and return to survey types
    shinyjs::reset("verification-survey-form")
    selected_survey_type(NULL)
  })
  
  # Handle clearing the Verification Survey form
  observeEvent(input$clear_verification_form, {
    # Reset all inputs in the verification form
    updateTextInput(session, "verification_full_name", value = "")
    updateSelectInput(session, "verification_government_id_type", selected = "")
    updateTextInput(session, "verification_applicant_full_name", value = "")
    updateTextInput(session, "verification_contact_phone", value = "")
    updateTextInput(session, "verification_contact_email", value = "")
    updateTextAreaInput(session, "verification_survey_reason", value = "")
    updateTextAreaInput(session, "verification_additional_notes", value = "")
    updateSelectInput(session, "verification_survey_urgency", selected = "normal")
    
    showNotification("Verification form cleared successfully.", type = "default", duration = 3)
  })
  
  # Update profile handler
  observeEvent(input$update_profile, {
    showNotification("Profile updated successfully!", type = "default", duration = 5)
  })
  
  # Change password handler
  observeEvent(input$change_password, {
    showModal(modalDialog(
      title = "Change Password",
      div(class = "form-group",
          tags$label(class = "form-label", "Current Password"),
          passwordInput("current_password", label = NULL, placeholder = "Enter current password")
      ),
      div(class = "form-group",
          tags$label(class = "form-label", "New Password"),
          passwordInput("new_password", label = NULL, placeholder = "Enter new password")
      ),
      div(class = "form-group",
          tags$label(class = "form-label", "Confirm New Password"),
          passwordInput("confirm_new_password", label = NULL, placeholder = "Confirm new password")
      ),
      footer = tagList(
        actionButton("submit_password_change", "Change Password", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$submit_password_change, {
    current_pass <- input$current_password
    new_pass <- input$new_password
    confirm_pass <- input$confirm_new_password
    
    if (is.null(current_pass) || current_pass == "") {
      showNotification("Please enter your current password.", type = "warning")
      return()
    }
    
    if (is.null(new_pass) || new_pass == "") {
      showNotification("Please enter a new password.", type = "warning")
      return()
    }
    
    if (nchar(new_pass) < 8) {
      showNotification("New password must be at least 8 characters long.", type = "warning")
      return()
    }
    
    if (is.null(confirm_pass) || confirm_pass == "") {
      showNotification("Please confirm your new password.", type = "warning")
      return()
    }
    
    if (new_pass != confirm_pass) {
      showNotification("New passwords do not match.", type = "warning")
      return()
    }
    
    # In a real app, you would verify current password and update it in database
    removeModal()
    showNotification("Password changed successfully!", type = "default", duration = 5)
  })
  
  # Contact form submission
  observeEvent(input$send_message, {
    name <- input$contact_name
    email <- input$contact_email
    subject <- input$contact_subject
    message <- input$contact_message
    
    if (is.null(name) || name == "" || is.null(email) || email == "" || 
        is.null(subject) || subject == "" || is.null(message) || message == "") {
      showNotification("Please fill in all fields before sending your message.", type = "warning")
      return()
    }
    
    if (!grepl("^[^@]+@[^@]+\\.[^@]+$", email)) {
      showNotification("Please enter a valid email address.", type = "warning")
      return()
    }
    
    showNotification(
      "Thank you for your message! Our support team will get back to you within 24 hours.",
      type = "default",
      duration = 10
    )
    
    # Reset the form
    updateTextInput(session, "contact_name", value = "")
    updateTextInput(session, "contact_email", value = "")
    updateTextInput(session, "contact_subject", value = "")
    updateTextAreaInput(session, "contact_message", value = "")
    updateSelectInput(session, "contact_urgency", selected = "medium")
  })
  
  # Password toggle functionality
  observe({
    # Toggle for login password
    onclick("toggle-login-password", {
      shinyjs::runjs("
        var passwordField = document.getElementById('login_password');
        var toggleButton = document.getElementById('toggle-login-password');
        if (passwordField.type === 'password') {
          passwordField.type = 'text';
          toggleButton.innerHTML = '<i class=\"fa fa-eye-slash\"></i>';
        } else {
          passwordField.type = 'password';
          toggleButton.innerHTML = '<i class=\"fa fa-eye\"></i>';
        }
      ")
    })
    
    # Toggle for signup password
    onclick("toggle-password", {
      shinyjs::runjs("
        var passwordField = document.getElementById('signup_password');
        var toggleButton = document.getElementById('toggle-password');
        if (passwordField.type === 'password') {
          passwordField.type = 'text';
          toggleButton.innerHTML = '<i class=\"fa fa-eye-slash\"></i>';
        } else {
          passwordField.type = 'password';
          toggleButton.innerHTML = '<i class=\"fa fa-eye\"></i>';
        }
      ")
    })
    
    # Toggle for confirm password
    onclick("toggle-confirm-password", {
      shinyjs::runjs("
        var passwordField = document.getElementById('signup_confirm');
        var toggleButton = document.getElementById('toggle-confirm-password');
        if (passwordField.type === 'password') {
          passwordField.type = 'text';
          toggleButton.innerHTML = '<i class=\"fa fa-eye-slash\"></i>';
        } else {
          passwordField.type = 'password';
          toggleButton.innerHTML = '<i class=\"fa fa-eye\"></i>';
        }
      ")
    })
  })
  
  # Real-time phone number validation
  observe({
    phone <- input$signup_phone
    
    if (!is.null(phone) && nchar(phone) > 0) {
      validation <- validate_phone_number(phone)
      
      if (validation$valid) {
        shinyjs::html("phone-validation", "✓ Valid 11-digit phone number")
        shinyjs::addClass("phone-validation", "valid")
        shinyjs::removeClass("phone-validation", "error")
      } else {
        shinyjs::html("phone-validation", paste("✗", validation$message))
        shinyjs::addClass("phone-validation", "error")
        shinyjs::removeClass("phone-validation", "valid")
      }
    } else {
      shinyjs::html("phone-validation", "Enter 11 digits (numbers only)")
      shinyjs::removeClass("phone-validation", "valid")
      shinyjs::removeClass("phone-validation", "error")
    }
  })
  
  # Password validation
  observe({
    password <- input$signup_password
    
    if (!is.null(password) && nchar(password) > 0) {
      # Check password requirements
      has_length <- nchar(password) >= 8
      has_uppercase <- grepl("[A-Z]", password)
      has_number <- grepl("[0-9]", password)
      
      # Update requirement indicators
      shinyjs::toggleClass("req-length", "valid", has_length)
      shinyjs::html("req-length", if(has_length) "✓ At least 8 characters" else "✗ At least 8 characters")
      
      shinyjs::toggleClass("req-uppercase", "valid", has_uppercase)
      shinyjs::html("req-uppercase", if(has_uppercase) "✓ At least one uppercase letter" else "✗ At least one uppercase letter")
      
      shinyjs::toggleClass("req-number", "valid", has_number)
      shinyjs::html("req-number", if(has_number) "✓ At least one number" else "✗ At least one number")
    }
  })
  
  # Handle login button click
  observeEvent(input$login_btn, {
    email <- input$login_email
    password <- input$login_password
    
    # Simple validation
    if (is.null(email) || email == "" || is.null(password) || password == "") {
      showNotification("Please enter both email and password.", type = "warning")
      return()
    }
    
    # Authenticate user with database
    user <- authenticate_user(email, password)
    
    if (!is.null(user)) {
      # Login successful
      user_session$logged_in <- TRUE
      user_session$user_id <- user$id
      user_session$full_name <- user$full_name
      user_session$email <- user$email
      
      showNotification(paste("Login successful! Welcome,", user$full_name), type = "default")
      
      # Navigate to dashboard immediately
      current_page("dashboard")
    } else {
      showNotification("Invalid credentials. Please check your email and password.", type = "error")
    }
  })
  
  # Handle create account button click
  observeEvent(input$create_account_btn, {
    # Get form values
    fullname <- input$signup_fullname
    email <- input$signup_email
    phone <- input$signup_phone
    password <- input$signup_password
    confirm <- input$signup_confirm
    terms <- input$signup_terms
    
    # Basic validation first
    if (is.null(fullname) || fullname == "") {
      showNotification("Full name is required", type = "warning", duration = 5)
      return()
    }
    
    if (is.null(email) || email == "") {
      showNotification("Email address is required", type = "warning", duration = 5)
      return()
    } else if (!grepl("^[^@]+@[^@]+\\.[^@]+$", email)) {
      showNotification("Please enter a valid email address", type = "warning", duration = 5)
      return()
    }
    
    # Phone number validation with specific rules
    if (is.null(phone) || phone == "") {
      showNotification("Phone number is required", type = "warning", duration = 5)
      return()
    } else {
      phone_validation <- validate_phone_number(phone)
      if (!phone_validation$valid) {
        showNotification(phone_validation$message, type = "warning", duration = 5)
        return()
      }
    }
    
    if (is.null(password) || password == "") {
      showNotification("Password is required", type = "warning", duration = 5)
      return()
    } else if (nchar(password) < 8) {
      showNotification("Password must be at least 8 characters long", type = "warning", duration = 5)
      return()
    } else if (!grepl("[A-Z]", password)) {
      showNotification("Password must contain at least one uppercase letter", type = "warning", duration = 5)
      return()
    } else if (!grepl("[0-9]", password)) {
      showNotification("Password must contain at least one number", type = "warning", duration = 5)
      return()
    }
    
    if (is.null(confirm) || confirm == "") {
      showNotification("Please confirm your password", type = "warning", duration = 5)
      return()
    } else if (password != confirm) {
      showNotification("Passwords do not match", type = "warning", duration = 5)
      return()
    }
    
    if (!isTRUE(terms)) {
      showNotification("You must agree to the Terms of Service and Privacy Policy", type = "warning", duration = 5)
      return()
    }
    
    # If we get here, all validation passed
    # Create user in database
    result <- create_user(fullname, email, phone, password)
    
    if (result$success) {
      showNotification(
        paste("Account created successfully!", 
              "Welcome to FieldConektiv,", fullname, "!"),
        type = "default",
        duration = 5
      )
      
      # Reset form
      shinyjs::reset("signup-form")
      
      # Navigate back to home immediately (no delay)
      current_page("home")
    } else {
      showNotification(
        paste("Error:", result$message),
        type = "error",
        duration = 10
      )
    }
  })
  
  # Handle logout from anywhere
  observeEvent(input$logout_btn, {
    # Logout user
    user_session$logged_in <- FALSE
    user_session$user_id <- NULL
    user_session$full_name <- NULL
    user_session$email <- NULL
    
    # Navigate to home
    current_page("home")
    
    showNotification("You have been logged out successfully.", type = "default")
  })
  
  # Handle profile button on home page
  observeEvent(input$profile_btn, {
    if (user_session$logged_in) {
      current_page("dashboard")
      dashboard_tab("profile")
    } else {
      showNotification("Please log in first to view your profile.", type = "warning")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)