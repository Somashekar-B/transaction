# Digital Wallet Platform

## Overview
This project implements a digital wallet platform using Erlang, Cowboy (HTTP server), and MySQL database. The system supports two types of users: Admins and Account Holders. Admins can manage account holder data, while account holders can perform transactions like deposits and withdrawals with configurable transaction limits.

## Features
- Admin User Management: Create, update, delete, and view account holders.
- Account Holder Management: Deposit, withdraw, and view account balance and transaction history.
- Configurable transaction limits.
- Supports growing user base across multiple regions.

## Technologies Used
- **Erlang**: Core language.
- **Cowboy**: Web server for handling API requests.
- **MySQL**: Database for permanent storage.
- **Rebar3**: Build and dependency management tool.

## Prerequisites
Ensure you have the following installed:
- Erlang/OTP
- rebar3
- MySQL

## Points to Highlight

1. **Admin Access**: Admin users have the ability to access any user accounts and can perform deposit operations on their behalf.

2. **Caching with ETS**: To improve scalability and reduce database load, the application uses **ETS** (Erlang Term Storage) as a caching mechanism. This minimizes the frequency of database hits for frequently accessed data.

3. **User Accounts**: A single user can have multiple accounts (1 to N), allowing for greater flexibility in managing funds.

4. **Transaction Limits**: The maximum number of transactions per day applies specifically to withdrawals. This helps enforce limits on user spending.

5. **Default Admin Credentials**:
   - **Username**: `admin`
   - **Password**: `payNow123`


## Database Schema
The system uses the following MySQL tables:

### 1. `users`: Stores information about all users (both admins and account holders).
```sql
CREATE TABLE `users` (
  `userid` int NOT NULL AUTO_INCREMENT,
  `username` varchar(100),
  `password` varchar(200),
  `onboarddate` datetime DEFAULT CURRENT_TIMESTAMP,
  `role` varchar(50) NOT NULL,
  PRIMARY KEY (`userid`)
);
```

### 2. users_data: Stores additional information about account holders.
```sql
CREATE TABLE `users_data` (
  `userid` int NOT NULL,
  `username` varchar(100),
  `email` varchar(100),
  `phone` varchar(20),
  `address` varchar(255),
  `onboarddate` timestamp DEFAULT CURRENT_TIMESTAMP,
  `updated_datetime` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`userid`)
);
```

### 3. account_details: Stores account information for account holders.
```sql
CREATE TABLE `account_details` (
  `accountNo` int NOT NULL AUTO_INCREMENT,
  `userid` int,
  `amount` decimal(10,2),
  `onboardeddatetime` timestamp DEFAULT CURRENT_TIMESTAMP,
  `updateddatetime` timestamp DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`accountNo`),
  CONSTRAINT `fk_user` FOREIGN KEY (`userid`) REFERENCES `users` (`userid`),
  CONSTRAINT `chk_amount` CHECK (`amount` >= 0)
);
```

### 4. transactions: Logs all deposit and withdrawal transactions.
```sql
CREATE TABLE `transactions` (
  `transaction_id` int NOT NULL AUTO_INCREMENT,
  `userid` int,
  `accountNo` int,
  `amount` decimal(10,2),
  `txn_type` tinyint,  -- 0 for deposit, 1 for withdrawal
  `transaction_datetime` timestamp DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`transaction_id`),
  CONSTRAINT `transactions_ibfk_1` FOREIGN KEY (`userid`) REFERENCES `users` (`userid`),
  CONSTRAINT `transactions_ibfk_2` FOREIGN KEY (`accountNo`) REFERENCES `account_details` (`accountNo`)
);
```

## API Endpoints

### 1. **Add User (Admin)**
Create a new account holder. Only admin users can create account holder details.
```bash
POST /api/user
Authorization: Basic [admin credentials]
Content-Type: application/json

Request Body:
[
  {
    "username": "somashekar",
    "password": "payNow@123",
    "role": "user",
    "email": "bsomashekar108@gmail.com",
    "phone": 8904302607,
    "address": "Hosapete"
  }
]
```

### 2. **Update User (Admin)**
Update account holder information. Only admin users can update account holder details.

```bash
PUT /api/user
Authorization: Basic [admin credentials]
Content-Type: application/json

Request Body:
{
  "userId": 6,               # The ID of the user to update
  "email": "bsomashekar108@gmail.com",  # New email address
  "phone": 8904302608         # Updated phone number
}
```

### 3. **Delete User (Admin)**
Delete an account holder by their user ID. Only admin users have the ability to delete account holders.

```bash
DELETE /api/user/{userId}
Authorization: Basic [admin credentials]
```

### 4. **Get All Users (Admin)**
Retrieve a list of all users (admin and account holders). Only admin users can access this endpoint.

```bash
GET /api/user
Authorization: Basic [admin credentials]
```


### 5. **Get User Transactions (Account Holder)**
Retrieve the transaction history for a specific user and their account. Account holders can only access their own transaction data.

```bash
GET /api/transaction/{userId}/{accountNo}
Authorization: Basic [account holder credentials]
```

### 6. **Get User Data (Account Holder)**
Retrieve personal information for a specific user. Account holders can only access their own data.

```bash
GET /api/user/{userId}
Authorization: Basic [account holder credentials]
```

### 7. **Deposit (Account Holder)**
Deposit money into the user's account. The amount must respect the configured transaction limits.

```bash
PUT /api/transaction/deposit/{userId}
Authorization: Basic [account holder credentials]
Content-Type: application/json

Request Body:
{
  "accountNo": 1001,     # The account number for the deposit
  "amount": 480          # Amount to deposit
}
```

### 8. **Withdraw (Account Holder)**
Withdraw money from the user's account. The amount must respect the configured transaction limits.

```bash
PUT /api/transaction/withdraw/{userId}
Authorization: Basic [account holder credentials]
Content-Type: application/json

Request Body:
{
  "accountNo": 1001,     # The account number for withdrawal
  "amount": 100          # Amount to withdraw
}
```

## Configuration

The system allows configurable minimum and maximum transaction limits for deposit and withdraw, set at deployment. The configurations are specified in the `sys.config` file.

## Running the Project

To run the digital wallet platform project, follow these steps:

### Prerequisites

1. **Erlang**: Ensure that Erlang is installed on your system. You can check by running:
   ```bash
   erl
   ```
   If not installed, download and install Erlang from Erlang's official website.

2. **Rebar3**: Make sure Rebar3 is installed. Check by running:
   ```bash
   rebar3 --version
   ```
3. **MySQL**: Ensure that MySQL server is installed and running. You should have a database named transaction created.
   
### Setup

1. **Clone the Repository**:
  ```bash
git clone [repository-url]
cd [repository-folder]
  ```

2. **Configure the Database**:
   Update the database connection details in the sys.config file if necessary.

3. **Compile the Project**: Run the following command to compile the project:
   ```erlang
   rebar3 compile
   ```
4. **Start the Application**: You can start the application using the Erlang shell:
   ```erlang
   rebar3 shell
   ```
5. **Creating Tables (Only for first time)**:
   Run the below command to create all the tables required for the application
     ```erlang
   db_setup:initialize_db(dbrw_pool)
   ```
5. **Accessing the API**:
   Once the application is running, you can access the above mentioned API endpoints using a tool like cURL or Postman.
   


   





