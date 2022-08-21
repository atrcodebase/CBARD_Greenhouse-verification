# Organization:   ATR
# Date:           August 19, 2022
# Script:         CBARD - Greenhouse Verification Data Cleaning and Analysis
# Author:         ATR Data Management Department

# load required packages ----------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(readxl)) install.packages("readxl")
if(!require(writexl)) install.packages("writexl")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(lubridate)) install.packages("lubridate")
if(!require(knitr)) install.packages("knitr")
if(!require(glue)) install.packages("glue")
if(!require(atRfunctions)) remotes::install_github("atrcodebase/atRfunctions")

`%notin%` <- Negate(`%in%`)

# define paths ----------------------------------------
data_path <- "input/raw_data/CBARD_Greenhouse_Verification.xlsx"
tool_path <- "input/tool/CBARD_Greenhouse_Verification.xlsx"
sampling_path <- "input/sampling/cbardsampling.csv"

# read data ----------------------------------------
data <- readxl::read_excel(data_path, sheet = "data")
family_roster <- readxl::read_excel(data_path, sheet = "Family_Roster")
crops <- readxl::read_excel(data_path, sheet = "Crops")

# read qa-log ----------------------------------------
googlesheets4::gs4_deauth()
qa_log <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/117MxcnblNnwndZzClsXrf3JJJi_MNVIwFktocRiOxL4/edit?usp=sharing", sheet = "QA_Log")

# qa-backlog ----------------------------------------
# file.edit("R/qa_backlog.R")
source("R/qa_backlog.R")

# data cleaning ----------------------------------------
# file.edit("R/data_cleaning.R")
source("R/data_cleaning.R")

# filter for specific dates ----------------------------------------
count(data, SubmissionDate)

start_date <- "2022-08-19" # start date of data collection
end_date <- "2022-08-23" # keep updating this

data <- data %>% filter(SubmissionDate >= start_date & SubmissionDate <= end_date)
family_roster <- family_roster %>% filter(SubmissionDate >= start_date & SubmissionDate <= end_date)
crops <- crops %>% filter(SubmissionDate >= start_date & SubmissionDate <= end_date)

# data for dashboard ----------------------------------------
dash_dt <- list(
  data = data,
  Family_Roster = family_roster,
  Crops = crops
)

# cleaned data ----------------------------------------
count(data, qa_status)
count(family_roster, qa_status)
count(crops, qa_status)

cleaned_dt <- list(
  data = filter(data, qa_status == "Approved"),
  Family_Roster = filter(family_roster, qa_status == "Approved"),
  Crops = filter(crops, qa_status == "Approved")
)

# export result ----------------------------------------
writexl::write_xlsx(x = dash_dt, path = "output/dashboard_data/CBARD_Greenhouse_Verification_dashboard_dt.xlsx")
writexl::write_xlsx(x = cleaned_dt, path = "output/cleaned_data/CBARD_Greenhouse_Verification_cleaned_dt.xlsx")
writexl::write_xlsx(x = progress_report, path = glue::glue("output/weekly_reports/progress_report_{Sys.Date()}.xlsx"))
writexl::write_xlsx(x = qa_log, path = glue::glue("output/qa_log_{Sys.Date()}.xlsx"))
