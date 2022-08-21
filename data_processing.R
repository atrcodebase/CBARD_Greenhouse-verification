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

