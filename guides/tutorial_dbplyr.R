## -------------------------------------------------------- ##
                      # dbplyr Tutorial
## -------------------------------------------------------- ##
# This contains a `dbplyr` tutorial

# Load needed packages
# install.packages("librarian")
librarian::shelf(DBI,
                 RSQLite,
                 tidyverse)

# Clear environment
rm(list = ls())

# Tutorial #1: R, Databases & SQL -----------------------

# T1 - Chapter 1: DBI -----------------------------------
# Link: rdbsql.rsquaredacademy.com/dbi.html

# Connect
con <- dbConnect(RSQLite::SQLite(), ":memory:")

# Summary of connection
summary(con)

# Copy mtcars to database
dplyr::copy_to(con, mtcars)

# See which tables are included in the database
dbListTables(con)

# List fields (i.e., columns) in that element of the database
dbListFields(conn = con, name = "mtcars")

# We can examine everything in a given element of the database
dbReadTable(conn = con, name = 'mtcars')

# Query database using SQL commands
dbGetQuery(conn = con, statement = "SELECT * FROM mtcars LIMIT 10")

# Can seperately query and then specify how many rows to return
## Query
query <- dbSendQuery(conn = con, 'SELECT * FROM mtcars')
## Get result
result <- dbFetch(res = query, n = 5)
result

# Can check query status (useful for queries that take a long time to process)
dbHasCompleted(res = query)

# Can also get more complete information
dbGetInfo(dbObj = query)

# Return most recent query:
dbGetStatement(res = query)

# How many rows were requested
dbGetRowCount(res = query)

# See how many rows are modified by query
dbGetRowsAffected(res = query)

# See column names and format content
dbColumnInfo(res = query)

# Can create a table and put it into an existing database as a new element
x <- 1:10
y <- letters[1:10]
trial <- tibble::tibble(x, y)
dbWriteTable(conn = con, name = "trial", value = trial)

# Check that the data got added and look right
dbListTables(conn = con)
dbGetQuery(conn = con, statement = "SELECT * FROM trial LIMIT 3")
dbExistsTable(conn = con, name = "trial")

# Can also overwrite a given table in a database
x <- sample(100, 10)
y <- letters[11:20]
trial2 <- tibble::tibble(x, y)
dbWriteTable(conn = con, name = "trial", value = trial2, overwrite = T)

# Check it
dbListTables(conn = con)
dbGetQuery(conn = con, statement = "SELECT * FROM trial LIMIT 3")

# Can also append additional information to an existing table
x <- sample(100, 10)
y <- letters[5:14]
trial3 <- tibble::tibble(x, y)
dbWriteTable(conn = con, name = "trial", value = trial3, append = T)
dbReadTable(conn = con, name = "trial")

# Can insert rows into an existing table
## Method #1
dbExecute(conn = con,
          statement = "INSERT into trial (x, y)
          VALUES (32, 'c'), (45, 'k'), (61, 'h')")

## Method #2 (prints some diagnostic info upon completion)
dbSendStatement(conn = con,
                statement = "INSERT into trial (x, y)
                VALUES (25, 'm'), (54, 'l'), (16, 'y')")

# Deleting a table is also possible
dbRemoveTable(conn = con, name = "trial")
dbListTables(conn = con)

# Want to check the data type?
dbDataType(dbObj = RSQLite::SQLite(), obj = "a")
dbDataType(dbObj = RSQLite::SQLite(), obj = 1:5)
dbDataType(dbObj = RSQLite::SQLite(), obj = 1.5)

# End by closing connection
dbDisconnect(con)

# Clear environment
rm(list = ls())

# T1 - Chapter 2: dbplyr -----------------------------------
# Link: rdbsql.rsquaredacademy.com/dbplyr.html

# Connect
con <- dbConnect(RSQLite::SQLite(), ":memory:")

# Copy mtcars to database
dplyr::copy_to(con, mtcars)

# Reference data using `tbl()`
mtcars2 <- dplyr::tbl(con, "mtcars")
mtcars2

# Look at some columns
select(mtcars2, mpg, cyl, drat)

# Filter works too
filter(mtcars2, mpg > 25)

# Try summarizing
mtcars2 %>%
  group_by(cyl) %>%
  summarise(mileage = mean(mpg))

# View SQL query for a given operation
## 1) Perform operation & assign to object
mileages <- mtcars2 %>%
  group_by(cyl) %>%
  summarise(mileage = mean(mpg, na.rm = T))

## 2a) Show query
dplyr::show_query(mileages)

## 2b) Explain query
dplyr::explain(mileages)

# Interestingly, `dplyr` doesn't actually get the data into R until you explicitly ask for it (e.g., printing the object)

# To get the data into R for subsequent use, we need `collect`
dplyr::collect(mileages)

# When done with connection:
dbDisconnect(con)

# Clear environment
rm(list = ls())

# T1 - Chapter 3: SQL Basics -----------------------------------
# Link: rdbsql.rsquaredacademy.com/sqlbasics.html

# Any new libraries required?
library(readr)

# Read in data
ecom <- readr::read_csv(file = 'Data/SQL Tutorial Data/ecom.csv')

# Open connection to database
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# Copy data into database
dplyr::copy_to(dest = con, df = ecom)

# Check that it worked & look at data
dbListTables(conn = con)
dbGetQuery(conn = con, statement = "SELECT * FROM ecom LIMIT 5")

# Look at a single column
dbGetQuery(conn = con, statement = "SELECT device FROM ecom")

# Can also select multiple columns
dbGetQuery(conn = con, statement = "SELECT referrer, device, purchase FROM ecom")

# An asterisk after SELECT grabs all of the columns
dbGetQuery(conn = con, statement = "SELECT * FROM ecom;")

# LIMIT reduces the number of rows returned to the specified integer
dbGetQuery(conn = con, statement = "SELECT * FROM ecom LIMIT 1;")

# Can also get only unique (i.e., distinct) entries
dbGetQuery(conn = con,
           statement = "SELECT DISTINCT referrer
           FROM ecom;")

# Or subset by conditions ("WHERE")
dbGetQuery(conn = con,
           statement = "SELECT *
           FROM ecom
           WHERE bouncers > 0;")

# Can also use R syntax for 'exactly equal to'
dbGetQuery(conn = con,
           statement = "SELECT *
           FROM ecom
           WHERE referrer == 'Organic';")

# Can also use AND, OR, or NOT
## AND
dbGetQuery(conn = con, statement = "SELECT *
           FROM ecom
           WHERE device == 'Desktop' AND referrer == 'Organic'
           LIMIT 2;")
## OR
dbGetQuery(conn = con, statement = "SELECT *
           FROM ecom
           WHERE device == 'Desktop' OR device == 'Mobile';")
## NOT
dbGetQuery(conn = con, statement = "SELECT DISTINCT device
           FROM ecom
           WHERE NOT device == 'Desktop';")


# Inclusively select BETWEEN a range of values
dbGetQuery(conn = con, statement = "SELECT *
           FROM ecom
           WHERE bouncers BETWEEN 0 AND 1 AND device == 'Mobile';")

# SQL's answer to "%in%" is "IN"
dbGetQuery(conn = con, statement = "SELECT *
           FROM ecom
           WHERE device in ('Mobile', 'Desktop');")

# Identify missing values ("IS NULL")
## "IS NOT NULL" is also allowed
dbGetQuery(conn = con, statement = "SELECT *
           FROM ecom
           WHERE device IS NULL;")

# Can look for similar values with LIKE
## LIKE + % = zero, one, or multiple characters
## LIKE + _ = one character
dbGetQuery(conn = con, statement = "SELECT *
           FROM ecom
           WHERE device LIKE 'M%';")
dbGetQuery(conn = con, statement = "SELECT *
           FROM ecom
           WHERE bouncers LIKE '_';")

# Disconnect and clear environment
dbDisconnect(conn = con)
rm(list = ls())

# T1 - Chapter 4: SQL Advanced -----------------------------------
# Link: rdbsql.rsquaredacademy.com/sql2.html

# Any new libraries required?
library(readr)

# Read in data
hr <- readr::read_csv(file = "Data/SQL Tutorial Data/hr-data.csv")

# Open connection to database
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# Copy data into database
dplyr::copy_to(dest = con, df = hr)
dbListTables(conn = con)

# Look at it
dbReadTable(conn = con, name = "hr")


# Can SUM a column
dbGetQuery(conn = con, statement = "SELECT SUM(SpecialProjectsCount)
           FROM hr;")

# Can also rename that column using "AS"
dbGetQuery(con, "SELECT SUM(SpecialProjectsCount) AS SpecialProjectsTotal
           FROM hr;")

# Can sum within a condition as well
dbGetQuery(con, "SELECT SUM(SpecialProjectsCount) AS SpecialProjectsTotal
           FROM hr
           WHERE GenderId == 0;")

# Can also do averages with AVG
dbGetQuery(con, "SELECT AVG(SpecialProjectsCount) AS MeanSpecialProjects
           FROM hr;")

# Or average conditionally with wildcards
dbGetQuery(con, "SELECT AVG(SpecialProjectsCount) AS MeanSpecialProjects
           FROM hr
           WHERE Department LIKE 'Admin %';")

# Can also find maximum (MAX)
dbGetQuery(con, "SELECT MAX(SpecialProjectsCount) AS MaxSpecialProject
           FROM hr
           WHERE RecruitmentSource == 'Diversity Job Fair';")

# Minimum is also an option (MIN)
dbGetQuery(con, "SELECT MIN(SpecialProjectsCount) AS MinSpecialProject
           FROM hr
           WHERE NOT RecruitmentSource == 'Diversity Job Fair';")

# Order output by a given column
dbGetQuery(con, "SELECT *
           FROM hr
           WHERE RecruitmentSource IS NOT NULL
           ORDER BY RecruitmentSource;")

# And can swap the order to DESCending if desired
dbGetQuery(con, "SELECT DISTINCT ManagerName, RecruitmentSource
           FROM hr
           WHERE RecruitmentSource IS NOT NULL
           ORDER BY RecruitmentSource DESC;")

# Can also perform operations (AVG, COUNT, MAX, etc.) within groups
dbGetQuery(con, "SELECT SUM(SpecialProjectsCount) AS SpecialProjectsTotal, RecruitmentSource
           FROM hr
           GROUP BY RecruitmentSource;")

# Disconnect and clear environment
dbDisconnect(conn = con)
rm(list = ls())

# End ----
