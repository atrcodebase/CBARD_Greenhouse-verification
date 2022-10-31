# weekly checks -----------------------------------------
## missing value in 'Family_Member_Gender' OR 'Family_Member_Age' OR 'Family_Member_Age' is less than 1
(check_1 <- family_roster %>% 
  filter(qa_status != "Rejected") %>%
  filter(is.na(Family_Member_Gender) | is.na(Family_Member_Age) | Family_Member_Age < 1) %>% 
  select(Family_Member_Gender, Family_Member_Age, KEY, qa_status) %>% 
  mutate(remarks = "missing value in 'Family_Member_Gender' OR 'Family_Member_Age', OR 'Family_Member_Age' is less than 1"))

## missing value in 'N_family'
(check_2 <- data %>% 
    filter(Consent == "Yes") %>% 
    filter(qa_status != "Rejected") %>% 
    filter(is.na(N_family) | N_family < 1) %>% 
    select(N_family, KEY) %>% 
    mutate(remarks = "N_family should not be missing or less than 1"))

## missing value in 'Location' OR 'Type' OR 'Type_of_Crop' OR 'Season'
(check_3 <- crops %>% 
    filter(qa_status != "Rejected") %>%
    select(Location, Type, Type_of_Crop, Type_of_Crop_Other, Season, KEY) %>% 
    filter(is.na(Location) | is.na(Type) | is.na(Type_of_Crop) | is.na(Season)) %>% 
    mutate(renamrks = "missing value in 'Location' OR 'Type' OR 'Type_of_Crop' OR 'Season'"))

## 'How_Many_Meters_Square' is missing or less than 1
(check_4 <- crops %>% 
    filter(qa_status != "Rejected") %>%
    select(Location, Type, Type_of_Crop, How_Many_Meters_Square, KEY) %>% 
    filter(Location == "Inside" & Type == "Cultivated" & (is.na(How_Many_Meters_Square) | How_Many_Meters_Square < 1)) %>% 
    mutate(remarks = "when 'Inside' is selected in 'Location' and 'Type' is 'Cultivated', the 'How_Many_Meters_Square' column should not be missing or less than1"))

## 'Area' is missing or less than 1
(check_5 <- crops %>% 
    filter(qa_status != "Rejected") %>%
    select(Location, Type, Unit, Area, KEY) %>% 
    filter(Location == "Outside" & Type == "Cultivated" & (is.na(Unit) | is.na(Area) | Area < 1)) %>% 
    mutate(remarks = "when 'Outside' is selected in 'Location' and 'Type' is 'Cultivated', the 'Area' column should not be missing or less than 0" ))

## 'Production_in_KG' is missing or less than 1 OR 'Estimated_Income_AFN' is missing
(check_6 <- crops %>% 
    filter(qa_status != "Rejected") %>%
    select(Location, Type, Production_in_KG, Estimated_Income_AFN, KEY) %>% 
    filter(Type == "Harvested" & ((is.na(Production_in_KG) | Production_in_KG < 1) | (is.na(Estimated_Income_AFN)))) %>% 
    mutate(remarks = "when 'Type' is 'Harvested', the 'Production_in_KG' OR 'Estimated_Income_AFN' shoule not be missing. Also, please double check the instances where 'Production_in_KG' is 0"))

(check_7 <- data %>% filter(Consent == "Yes") %>% 
  select(KEY, N_family_data = N_family) %>% 
  full_join(
    family_roster %>% 
      count(KEY = PARENT_KEY, name = "N_Family_family_roster")
  ) %>% 
  filter(N_family_data != N_Family_family_roster | is.na(N_family_data) | is.na(N_Family_family_roster)) %>% 
  mutate(remarks = "Number of family reported in main sheet is not equal to the number of family recorded in child (repeating group) sheet"))

## sum of cultivated/harvested crops does not add up to 'N_Crops' OR number of crops reported in main sheet is not equal to the number of crops recorded in child sheet
(check_8 <- data %>% 
  filter(qa_status != "Rejected") %>%
  select(Cultivate_Harvest, Inside_Cultivation_N, Inside_Harvest_N, Outside_Cultivation_N, Outside_Harvest_N, N_Crops, KEY) %>% 
  mutate(N_Crops_calculate = rowSums(select(., Inside_Cultivation_N:Outside_Harvest_N), na.rm = F)) %>% 
  left_join(
    crops %>% count(KEY = PARENT_KEY, name = "N_Crops_from_child_sheet"),
    by = "KEY"
  ) %>% 
  filter((N_Crops_calculate != N_Crops) | (N_Crops != N_Crops_from_child_sheet)) %>% 
  mutate(remarks = "sum of cultivated/harvested crops does not add up to 'N_Crops' OR number of crops reported in main sheet is not equal to the number of crops recorded in child (repeating group) sheet"))

## same crops is reported more than one time
(check_9 <- crops %>% 
  group_by(PARENT_KEY, Location, Type, Season, Type_of_Crop) %>% 
  mutate(n_crops = n()) %>% 
  filter(n_crops != 1) %>% 
  arrange(PARENT_KEY) %>% 
  select(-n_crops))

# check the area cultivated inside the greenhouse should not be more than the greenhouse size
(check_10 <- left_join(
  crops %>% 
    filter(Location == "Inside") %>%
    filter(Type == "Cultivated"),
  data %>% 
    mutate(E1 = gsub("m2", "", E1)) %>% 
    mutate(E1 = ifelse(E1 == "Other", E1_Other, E1)) %>% 
    mutate(E1 = as.numeric(E1)) %>% 
    select(E1, KEY),
  by = c("PARENT_KEY" = "KEY")
) %>% 
  relocate(E1, .after = How_Many_Meters_Square) %>% 
  filter(How_Many_Meters_Square > E1))

(check_11 <- rbind(
  data %>% 
    filter(KEY %in% (sold_new_relocated_incompelted_gh %>% 
                       filter(`GH Status` == "Never Built on the Site") %>% 
                       pull(UUID))) %>% 
    filter(B2 != "The greenhouse was never built in this given site" | is.na(B2)) %>% 
    select(B1, B2, B2_Other, KEY) %>% 
    mutate(remakrs = "the greenhouse status between tracker and data does not match")
  ,
  data %>% 
    filter(KEY %in% (sold_new_relocated_incompelted_gh %>% 
                       filter(`GH Status` == "Relocated") %>% 
                       pull(UUID))) %>% 
    filter(B2 != "The greenhouse has been relocated" | is.na(B2)) %>% 
    select(B1, B2, B2_Other, KEY) %>% 
    mutate(remakrs = "the greenhouse status between tracker and data does not match")
  ,
  data %>% 
    filter(KEY %in% (sold_new_relocated_incompelted_gh %>% 
                       filter(`GH Status` == "Sold") %>% 
                       pull(UUID))) %>% 
    filter(B2 != "The greenhouse has been sold" | is.na(B2)) %>% 
    select(B1, B2, B2_Other, KEY) %>% 
    mutate(remakrs = "the greenhouse status between tracker and data does not match")
))

checks <- list(
  check_1 = check_1,
  check_2 = check_2,
  check_3 = check_3,
  check_4 = check_4,
  check_5 = check_5,
  check_6 = check_6,
  check_7 = check_7,
  check_8 = check_8,
  check_9 = check_9,
  check_10 = check_10,
  check_11 = check_11
)

checks[sapply(checks, nrow) == 0] <- NULL
length(checks); names(checks)

## skip logics ----------------------------------------
# file.edit("R/functions/check_skip_logic.R")
source("R/functions/check_skip_logic.R")

# count(data, is_incomplete)
data_temp <- data %>% filter(is_incomplete != "Yes")

logic_checks <- lapply(colnames(data_temp), function(var) {
  check_skip_logic(dt = data_temp, tool_path = tool_path, column = var, value_labels_are_attached = T)
})

length(logic_checks) == length(colnames(data_temp))
names(logic_checks) <- colnames(data_temp)

logic_checks[sapply(logic_checks, is.null)] <- NULL
logic_checks[sapply(logic_checks, nrow) == 0] <- NULL
sum((sapply(logic_checks, nrow) %in% 0) | (sapply(logic_checks, is.null))) # should be 0
sum(sapply(logic_checks, nrow)) # 101

if (nrow(qa_cleaning_log) != 0) {
  for (names in names(logic_checks)) {
    if (names %in% unique(qa_cleaning_log$name)) {
      logic_checks[[names]] <- logic_checks[[names]] %>% 
        left_join(qa_cleaning_log %>% filter(name == names) %>%
                    mutate(already_shared_with_QA = "Yes") %>% 
                    select(KEY, already_shared_with_QA,
                           'QA - Fixed in SCTO?`' = `Fixed in SCTO?`,
                           'QA - Added to correction log?' = `Added to correction log?`,
                           'QA - Remakrs?' = Remarks
                    ),
                  by = "KEY"
        ) %>% 
        mutate(already_shared_with_QA = ifelse(is.na(already_shared_with_QA), "No", already_shared_with_QA))
    } else {
      logic_checks[[names]] <- logic_checks[[names]] %>% 
        mutate(already_shared_with_QA = "No")
    }
  }
}

# delete extra objects -----------------------------------------
rm(check_1, check_2, check_3, check_4, check_5, check_6, check_7, check_8, check_9, check_10, check_11)
rm(data_temp)
rm(qa_cleaning_log)
rm(check_skip_logic)
rm(sold_new_relocated_incompelted_gh)
