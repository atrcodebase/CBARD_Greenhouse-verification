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
data <- readxl::read_excel(data_path, sheet = "data", na = c("", "-", " ", "NA", "N/A"))
family_roster <- readxl::read_excel(data_path, sheet = "Family_Roster", na = c("", "-", " ", "NA", "N/A"))
crops <- readxl::read_excel(data_path, sheet = "Crops", na = c("", "-", " ", "NA", "N/A"))

# read qa-log ----------------------------------------
googlesheets4::gs4_deauth()
url <- "https://docs.google.com/spreadsheets/d/117MxcnblNnwndZzClsXrf3JJJi_MNVIwFktocRiOxL4/edit?usp=sharing"
browseURL(url)
qa_log <- googlesheets4::read_sheet(url, sheet = "QA_Log")
correction_log <- googlesheets4::read_sheet(url, sheet = "correction_log") %>% mutate(new_value = ifelse(new_value == "NULL", NA_character_, new_value))
remove_from_child_sheets <- googlesheets4::read_sheet(url, sheet = "remove_from_repeating_group")
crops_add_to_child_sheet <- googlesheets4::read_sheet(url, sheet = "add_to_Crops_repeating_group")
family_roster_add_to_child_sheet <- googlesheets4::read_sheet(url, sheet = "add_to_Family_roaster_repeating_group")
qa_cleaning_log <- googlesheets4::read_sheet(url, sheet = "Data_Cleaning")
sold_new_relocated_incompelted_gh <- googlesheets4::read_sheet(url, sheet = "Sold+New+Relocated GH")

qa_tracker <- list(
  QA_Log = qa_log,
  correction_log = correction_log,
  remove_from_repeating_group = remove_from_child_sheets,
  add_to_Crops_repeating_group = crops_add_to_child_sheet,
  add_to_Family_roaster_repeating_group = family_roster_add_to_child_sheet,
  Data_Cleaning = qa_cleaning_log,
  'Sold+New+Relocated GH' = sold_new_relocated_incompelted_gh
)

# qa-backlog ----------------------------------------
# file.edit("R/qa_backlog.R")
count(qa_log, `Final QA Status`)
source("R/qa_backlog.R")

# apply correction log ----------------------------------------
# file.edit("R/apply_correction_log.R")
count(correction_log, sheet)
source("R/apply_correction_log.R")

# add/remove cases to/from child sheets ----------------------------------------
# file.edit("R/add_remove_child_sheets.R")
source("R/add_remove_child_sheets.R")

# check whether more than one option is selected in single-select questions ----------------------------------------
single_select_questions <- readxl::read_excel(tool_path, sheet = "survey") %>% 
  filter(str_detect(type, "select_one|select one")) %>% 
  select(name) %>% pull() %>% unique()

more_than_one_option_in_single_select_vars <- data %>% 
  select(any_of(single_select_questions), -c(Surveyor_Name, TPM), qa_status, KEY) %>% 
  mutate(across(everything(), function(x)
    x = as.character(x)
  )) %>% 
  pivot_longer(-c(qa_status, KEY)) %>% 
  filter(str_detect(value, " ")) %>% 
  filter(qa_status != "Rejected") %>%
  select(-qa_status) %>% 
  arrange(name) %>% 
  mutate(Remarks = glue::glue("'{name} is a select_one variable but more than one option is selected.'")) %>% 
  mutate(sheet = "Data")

# data cleaning ----------------------------------------
# file.edit("R/data_cleaning.R")
source("R/data_cleaning.R")

# manual weekly checking  ----------------------------------------
# file.edit("R/weekly_checks.R")
source("R/weekly_checks.R")

# filter for specific dates ----------------------------------------
count(data, SubmissionDate)

start_date <- "2022-08-01" # start date of data collection
end_date <- "2022-10-31" # keep updating this

data <- data %>% filter(SubmissionDate >= start_date & SubmissionDate <= end_date)
family_roster <- family_roster %>% filter(SubmissionDate >= start_date & SubmissionDate <= end_date)
crops <- crops %>% filter(SubmissionDate >= start_date & SubmissionDate <= end_date)

# progress report ----------------------------------------
# file.edit("R/progress_report.R")
sampling <- read.csv(file = sampling_path)
weekly_start_date <- "2022-09-28" # keep updating this
weekly_end_date <- "2022-10-04" # keep updating htis

source("R/progress_report.R")

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
  data = filter(data, qa_status == "Approved" & KEY %notin% c("uuid:21b48841-01c7-4fab-b656-c2190e7daddf",
                                                            "uuid:90b18cda-ea65-466b-bc50-42be97e6796f")),
  Family_Roster = filter(family_roster, qa_status == "Approved"),
  Crops = filter(crops, qa_status == "Approved")
)

# export result ----------------------------------------
writexl::write_xlsx(x = dash_dt, path = "output/dashboard_data/CBARD_Greenhouse_Verification_dashboard_dt.xlsx", format_headers = FALSE)
writexl::write_xlsx(x = cleaned_dt, path = "output/cleaned_data/CBARD_Greenhouse_Verification_cleaned_dt.xlsx", format_headers = FALSE)
writexl::write_xlsx(x = progress_report, path = glue::glue("output/weekly_reports/progress_report_{Sys.Date()}.xlsx"))
if (nrow(more_than_one_option_in_single_select_vars) > 0) {
  writexl::write_xlsx(more_than_one_option_in_single_select_vars, "output/for_qa_review/more_than_one_option_in_single_select_vars.xlsx")
}
if(length(checks) != 0) {
  writexl::write_xlsx(checks, glue::glue("output/for_qa_review/regular_checks_{Sys.Date()}.xlsx"), format_headers = FALSE) 
}
if (length(logic_checks) != 0) {
  writexl::write_xlsx(logic_checks, glue::glue("output/for_qa_review/logic_fails_{Sys.Date()}.xlsx"), format_headers = FALSE)
}
writexl::write_xlsx(qa_tracker, glue::glue("output/QA_tracker_{Sys.Date()}.xlsx"), format_headers = FALSE)

