-- Create FieldConektiv Database
CREATE DATABASE IF NOT EXISTS FieldConektivDB;
USE FieldConektivDB;

-- Create roles table with default 'User' role
CREATE TABLE IF NOT EXISTS roles (
    id INT AUTO_INCREMENT PRIMARY KEY,
    role_name VARCHAR(50) NOT NULL UNIQUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Insert default 'User' role
INSERT IGNORE INTO roles (role_name) VALUES ('User');

-- Create users table
CREATE TABLE IF NOT EXISTS users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    full_name VARCHAR(100) NOT NULL,
    email_address VARCHAR(100) NOT NULL UNIQUE,
    phone_number VARCHAR(20) NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    confirm_hash VARCHAR(255) NOT NULL,
    role_id INT DEFAULT 1,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (role_id) REFERENCES roles(id)
);

-- Create an index on email for faster lookups
CREATE INDEX idx_email ON users(email_address);

-- Create an index on phone number
CREATE INDEX idx_phone ON users(phone_number);

-- Optional: Create a view for user details with role names
CREATE VIEW user_details AS
SELECT 
    u.id,
    u.full_name,
    u.email_address,
    u.phone_number,
    r.role_name,
    u.created_at,
    u.updated_at
FROM users u
JOIN roles r ON u.role_id = r.id;