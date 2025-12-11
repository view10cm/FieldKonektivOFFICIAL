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
    return(list(success = FALSE, message = paste("Database error:", e$message)))
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
      
      .landing-container, .signup-container {
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
      
      .auth-title {
        font-size: 28px;
        font-weight: 600;
        color: #1B5E20;
        margin-bottom: 10px;
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
      
      @media (max-width: 992px) {
        .content-wrapper {
          flex-direction: column;
          gap: 40px;
        }
        
        .hero-title {
          font-size: 36px;
        }
        
        .auth-card, .signup-card {
          max-width: 100%;
          padding: 30px 20px;
        }
        
        .header {
          padding: 15px 20px;
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
    # Header that appears on both pages
    div(class = "header",
        div(class = "logo", id = "logo",
            div(class = "logo-icon", "FC"),
            div(class = "logo-text", "FieldConektiv")
        ),
        uiOutput("header_content")
    ),
    
    # Conditional main content based on current page
    uiOutput("main_content"),
    
    # Footer that appears on both pages
    div(class = "footer",
        "© 2023 FieldConektiv. All rights reserved. | ",
        tags$a(href = "#", style = "color: #4CAF50;", "Privacy Policy"),
        " | ",
        tags$a(href = "#", style = "color: #4CAF50;", "Terms of Service"),
        " | ",
        tags$a(href = "#", style = "color: #4CAF50;", "Contact Support")
    )
  )
)

# Server logic (same as before, no changes needed)
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
  
  # Header content based on current page
  output$header_content <- renderUI({
    if (current_page() == "home") {
      if (user_session$logged_in) {
        div(
          style = "display: flex; align-items: center; gap: 20px;",
          span(style = "color: #4CAF50; font-weight: 500;", 
               paste("Welcome,", user_session$full_name)),
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
    } else if (current_page() == "signup") {
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
                                     tags$a(href = "#", style = "color: #4CAF50;", "Terms of Service"),
                                     " and ",
                                     tags$a(href = "#", style = "color: #4CAF50;", "Privacy Policy"),
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
      
      showNotification(paste("Login successful! Welcome,", user$full_name), type = "success")
    } else {
      showNotification("Invalid credentials. Please check your email and password.", type = "error")
    }
  })
  
  # Handle create account button click - WITH BETTER ERROR HANDLING
  observeEvent(input$create_account_btn, {
    # Use tryCatch to prevent R from crashing
    tryCatch({
      # Get form values
      fullname <- input$signup_fullname
      email <- input$signup_email
      phone <- input$signup_phone
      password <- input$signup_password
      confirm <- input$signup_confirm
      terms <- input$signup_terms
      
      # Basic validation first
      if (is.null(fullname) || fullname == "") {
        showNotification("Full name is required", type = "error", duration = 5)
        return()
      }
      
      if (is.null(email) || email == "") {
        showNotification("Email address is required", type = "error", duration = 5)
        return()
      } else if (!grepl("^[^@]+@[^@]+\\.[^@]+$", email)) {
        showNotification("Please enter a valid email address", type = "error", duration = 5)
        return()
      }
      
      # Check if user exists - handle potential errors
      user_exists <- tryCatch({
        check_user_exists(email)
      }, error = function(e) {
        cat("Error checking if user exists:", e$message, "\n")
        FALSE
      })
      
      if (user_exists) {
        showNotification("Email address is already registered", type = "error", duration = 5)
        return()
      }
      
      # Phone number validation with specific rules
      if (is.null(phone) || phone == "") {
        showNotification("Phone number is required", type = "error", duration = 5)
        return()
      } else {
        phone_validation <- validate_phone_number(phone)
        if (!phone_validation$valid) {
          showNotification(phone_validation$message, type = "error", duration = 5)
          return()
        }
      }
      
      if (is.null(password) || password == "") {
        showNotification("Password is required", type = "error", duration = 5)
        return()
      } else if (nchar(password) < 8) {
        showNotification("Password must be at least 8 characters long", type = "error", duration = 5)
        return()
      } else if (!grepl("[A-Z]", password)) {
        showNotification("Password must contain at least one uppercase letter", type = "error", duration = 5)
        return()
      } else if (!grepl("[0-9]", password)) {
        showNotification("Password must contain at least one number", type = "error", duration = 5)
        return()
      }
      
      if (is.null(confirm) || confirm == "") {
        showNotification("Please confirm your password", type = "error", duration = 5)
        return()
      } else if (password != confirm) {
        showNotification("Passwords do not match", type = "error", duration = 5)
        return()
      }
      
      if (!isTRUE(terms)) {
        showNotification("You must agree to the Terms of Service and Privacy Policy", type = "error", duration = 5)
        return()
      }
      
      # If we get here, all validation passed
      # Create user in database
      result <- create_user(fullname, email, phone, password)
      
      if (result$success) {
        showNotification(
          paste("Account created successfully!", 
                "Welcome to FieldConektiv,", fullname, "!"),
          type = "success",
          duration = 5
        )
        
        # Reset form
        shinyjs::reset("signup-form")
        
        # Navigate back to home immediately (no delay)
        current_page("home")
      } else {
        showNotification(
          result$message,
          type = "error",
          duration = 10
        )
      }
    }, error = function(e) {
      # Log the error but don't crash
      cat("Unexpected error in account creation:", e$message, "\n")
      
      # Use alert instead of showNotification to avoid recursive errors
      shinyjs::alert("An unexpected error occurred. Please try again.")
    })
  })
  
  # Handle logout
  observeEvent(input$logout_btn, {
    user_session$logged_in <- FALSE
    user_session$user_id <- NULL
    user_session$full_name <- NULL
    user_session$email <- NULL
    
    showNotification("You have been logged out successfully.", type = "info")
  })
  
  # Handle dashboard button
  observeEvent(input$dashboard_btn, {
    showNotification("Dashboard feature coming soon!", type = "info")
  })
  
  # Handle profile button
  observeEvent(input$profile_btn, {
    showNotification("Profile feature coming soon!", type = "info")
  })
}

# Run the application
shinyApp(ui = ui, server = server)