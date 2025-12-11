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
        max-width: 800px;
        margin: 0 auto;
        padding: 30px;
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
                    # NEW: Survey type selection buttons
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
                    } else {
                      # Survey form for selected type
                      div(class = "survey-form-container",
                          actionButton("back_to_survey_types", "← Back to Survey Types", 
                                       class = "survey-back-btn"),
                          
                          h2(paste(selected_survey_type(), "Survey Request"), 
                             style = "color: #1B5E20; margin-bottom: 25px;"),
                          p("Fill out the form below to submit your survey request.", 
                            style = "color: #666; margin-bottom: 30px;"),
                          
                          # Survey Form
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "surveyor-name", 
                                         span("Surveyor Name", span(class = "required", "*"))),
                              textInput("surveyor_name", label = NULL, placeholder = "Enter surveyor's full name")
                          ),
                          
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "survey-start-date", 
                                         span("Start Date", span(class = "required", "*"))),
                              dateInput("survey_start_date", label = NULL, value = Sys.Date())
                          ),
                          
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "survey-reason", 
                                         span("Reason for getting Survey", span(class = "required", "*"))),
                              textAreaInput("survey_reason", label = NULL, 
                                            placeholder = "Describe the purpose and reason for this survey...", 
                                            rows = 4)
                          ),
                          
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "client-name", "Client Name (Optional)"),
                              textInput("client_name", label = NULL, placeholder = "Enter client's name")
                          ),
                          
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "property-address", "Property Address (Optional)"),
                              textInput("property_address", label = NULL, placeholder = "Enter property address")
                          ),
                          
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "property-size", "Property Size (Optional)"),
                              div(style = "display: flex; gap: 10px;",
                                  textInput("property_size", label = NULL, placeholder = "Enter size", width = "70%"),
                                  selectInput("size_unit", label = NULL, 
                                              choices = c("Square Meters" = "sqm",
                                                          "Hectares" = "hectares",
                                                          "Acres" = "acres",
                                                          "Square Feet" = "sqft"),
                                              width = "30%")
                              )
                          ),
                          
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "survey-urgency", "Urgency Level"),
                              selectInput("survey_urgency", label = NULL,
                                          choices = c("Normal (2-4 weeks)" = "normal",
                                                      "High Priority (1-2 weeks)" = "high",
                                                      "Urgent (3-7 days)" = "urgent",
                                                      "Emergency (1-3 days)" = "emergency"),
                                          selected = "normal")
                          ),
                          
                          div(class = "form-group",
                              tags$label(class = "form-label", `for` = "additional-notes", "Additional Notes (Optional)"),
                              textAreaInput("additional_notes", label = NULL, 
                                            placeholder = "Any additional information or special requirements...", 
                                            rows = 3)
                          ),
                          
                          div(style = "display: flex; gap: 15px; margin-top: 30px;",
                              actionButton("create_survey_btn", "Submit Survey Request", class = "btn-primary",
                                           style = "width: auto; min-width: 200px;"),
                              actionButton("clear_survey_form", "Clear Form", class = "btn-secondary",
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
  
  # Create survey handler - UPDATED
  observeEvent(input$create_survey_btn, {
    # Get form values
    surveyor_name <- input$surveyor_name
    start_date <- input$survey_start_date
    survey_reason <- input$survey_reason
    survey_type <- selected_survey_type()
    
    # Basic validation
    if (is.null(surveyor_name) || surveyor_name == "") {
      showNotification("Surveyor Name is required.", type = "warning")
      return()
    }
    
    if (is.null(start_date)) {
      showNotification("Start Date is required.", type = "warning")
      return()
    }
    
    if (is.null(survey_reason) || survey_reason == "") {
      showNotification("Reason for getting Survey is required.", type = "warning")
      return()
    }
    
    # Get optional fields
    client_name <- input$client_name
    property_address <- input$property_address
    property_size <- input$property_size
    size_unit <- input$size_unit
    survey_urgency <- input$survey_urgency
    additional_notes <- input$additional_notes
    
    # Format urgency for display
    urgency_display <- switch(survey_urgency,
                              "normal" = "Normal (2-4 weeks)",
                              "high" = "High Priority (1-2 weeks)",
                              "urgent" = "Urgent (3-7 days)",
                              "emergency" = "Emergency (1-3 days)",
                              survey_urgency)
    
    # Create success message
    success_msg <- paste(
      "Survey request submitted successfully!\n\n",
      "Survey Type:", survey_type, "\n",
      "Surveyor:", surveyor_name, "\n",
      "Start Date:", format(start_date, "%B %d, %Y"), "\n",
      "Urgency:", urgency_display
    )
    
    if (client_name != "") {
      success_msg <- paste(success_msg, "\nClient:", client_name)
    }
    
    showNotification(
      success_msg,
      type = "default",
      duration = 10
    )
    
    # Reset form and go back to survey types
    shinyjs::reset("survey-form")
    selected_survey_type(NULL)
    
    # In a real application, you would save this data to the database
    # For example:
    # con <- connect_to_db()
    # if (!is.null(con)) {
    #   query <- sprintf("INSERT INTO surveys (survey_type, surveyor_name, start_date, reason, client_name, property_address, property_size, size_unit, urgency, additional_notes, user_id) VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', %d)",
    #                    dbEscapeStrings(con, survey_type),
    #                    dbEscapeStrings(con, surveyor_name),
    #                    format(start_date, "%Y-%m-%d"),
    #                    dbEscapeStrings(con, survey_reason),
    #                    dbEscapeStrings(con, client_name),
    #                    dbEscapeStrings(con, property_address),
    #                    dbEscapeStrings(con, property_size),
    #                    dbEscapeStrings(con, size_unit),
    #                    dbEscapeStrings(con, survey_urgency),
    #                    dbEscapeStrings(con, additional_notes),
    #                    user_session$user_id)
    #   dbExecute(con, query)
    #   dbDisconnect(con)
    # }
  })
  
  # Clear form handler
  observeEvent(input$clear_survey_form, {
    updateTextInput(session, "surveyor_name", value = "")
    updateDateInput(session, "survey_start_date", value = Sys.Date())
    updateTextAreaInput(session, "survey_reason", value = "")
    updateTextInput(session, "client_name", value = "")
    updateTextInput(session, "property_address", value = "")
    updateTextInput(session, "property_size", value = "")
    updateSelectInput(session, "size_unit", selected = "sqm")
    updateSelectInput(session, "survey_urgency", selected = "normal")
    updateTextAreaInput(session, "additional_notes", value = "")
    
    showNotification("Form cleared successfully.", type = "default", duration = 3)
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