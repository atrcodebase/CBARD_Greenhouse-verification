# status: completed ----------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
source("R/functions/Analysis_double_disagg.R")

# define paths ----------------------------------------
data_path <- "output/cleaned_data/CBARD_Greenhouse_Verification_cleaned_dt.xlsx"
ap_path <- "input/analysis_plan/analysis_plan.xlsx"
  
# read data and analysis plan -----------------------------------------
data <- readxl::read_excel(data_path, sheet = "data", na = c("", "-", " ", "NA", "N/A", "NULL"))
family_roster <- readxl::read_excel(data_path, sheet = "Family_Roster", na = c("", "-", " ", "NA", "N/A", "NULL"))
crops <- readxl::read_excel(data_path, sheet = "Crops", na = c("", "-", " ", "NA", "N/A", "NULL"))

crops <- crops %>% 
  mutate(
    How_Many_Meters_Square = case_when(
      Unit == "Jeribs" ~ Area * 2021.572106764277,
      Unit == "Kilo Meter Square" ~ Area * 1000000,
      Unit == "Meter Square (m2)" ~ Area * 1,
      TRUE ~ How_Many_Meters_Square
    )
  )

# productivity analysis -----------------------------------------
## overall
cultivate <- crops %>% 
  filter(Type == "Cultivated") %>% 
  group_by(Location, Type, Type_of_Crop) %>% 
  summarise(
    mean_meters_square_cultivate = round(mean(How_Many_Meters_Square, na.rm = T)),
    median_meters_square_cultivate = round(median(How_Many_Meters_Square, na.rm = T))
  ) %>% 
  ungroup()

harvest <- crops %>% 
  filter(Type == "Harvested") %>% 
  group_by(Location, Type, Type_of_Crop) %>% 
  summarise(
    mean_Kg_production = round(mean(Production_in_KG, na.rm = T)),
    median_Kg_production = round(median(Production_in_KG, na.rm = T)),
    mean_estimated_income = round(mean(Estimated_Income_AFN, na.rm = T)),
    median_estimated_income = round(median(Estimated_Income_AFN, na.rm = T))
  ) %>% 
  ungroup()

cultivate_harvest <- full_join(
  cultivate %>% select(-Type),
  harvest %>% select(-Type),
  by = c("Location", "Type_of_Crop")
) %>% 
  arrange(Location, -median_estimated_income)

## by province
cultivate_by_prov <- crops %>% 
  filter(Type == "Cultivated") %>% 
  group_by(Province, Location, Type, Type_of_Crop) %>% 
  summarise(
    mean_meters_square_cultivate = round(mean(How_Many_Meters_Square, na.rm = T)),
    median_meters_square_cultivate = round(median(How_Many_Meters_Square, na.rm = T))
  ) %>% 
  ungroup()

harvest_by_prov <- crops %>% 
  filter(Type == "Harvested") %>% 
  group_by(Province, Location, Type, Type_of_Crop) %>% 
  summarise(
    mean_Kg_production = round(mean(Production_in_KG, na.rm = T)),
    median_Kg_production = round(median(Production_in_KG, na.rm = T)),
    mean_estimated_income = round(mean(Estimated_Income_AFN, na.rm = T)),
    median_estimated_income = round(median(Estimated_Income_AFN, na.rm = T))
  ) %>% 
  ungroup()

cultivate_harvest_by_prov <- full_join(
  cultivate_by_prov %>% select(-Type),
  harvest_by_prov %>% select(-Type),
  by = c("Province", "Location", "Type_of_Crop")
) %>% 
  arrange(Province, Location, -median_estimated_income)

productivity <- list(
  cultivate_harvest = cultivate_harvest,
  cultivate_harvest_by_prov = cultivate_harvest_by_prov
)

# descriptive statistics -----------------------------------------
analysis_plan <- readxl::read_excel(ap_path)
descriptive_analysis <- analysis_func(df = data, ap = analysis_plan)
descriptive_analysis <- descriptive_analysis %>% filter(Denominator != 0)
descriptive_analysis <- descriptive_analysis %>% 
  left_join(
    analysis_plan %>%
      select(Questionnaire_question, Question = variable) %>% 
      distinct(.keep_all = TRUE),
    by = "Question"
  ) %>% 
  relocate(Questionnaire_question, .before = Question)

# export the result  -----------------------------------------
writexl::write_xlsx(productivity, "output/analysis/productivity_analysis.xlsx", format_headers = F)
writexl::write_xlsx(descriptive_analysis, "output/analysis/cbard_descriptive_analysis.xlsx", format_headers = F)

