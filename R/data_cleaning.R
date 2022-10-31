##### data cleaning #####
# remove unnecessary columns ----------------------------------------
extra_cols <- c("Starttime", "Endtime", "Deviceid", "Subscriberid", "Simid", "Devicephonenum", "Username", "mean_light_level", "sd_light_level",
                "mean_movement", "sd_movement", "mean_sound_level", "sd_sound_level", "pct_quiet", "pct_still", "Text_Audio", "Audio_Audit",
                "instanceID", "instanceName", "formdef_version", "i", "ii", "Location_Dari", "Location_Pashto", "Type_Dari", "Type_Pashto",
                "Family_Roster_count", "Crops_count", "review_status", "review_quality", "review_comments", "review_corrections", "G9_Label",
                "Audio_Audit_2", "Audio_Audit_3")

data <- data %>% select(-any_of(extra_cols), -starts_with("SET-OF"), -ends_with("_QA"))
family_roster <- family_roster %>% select(-any_of(extra_cols), -starts_with("SET-OF"), -ends_with("_QA"))
crops <- crops %>% select(-any_of(extra_cols), -starts_with("SET-OF"), -ends_with("_QA"))

print("--------------- Remove unnecessary columns.")

# pull information from sample and merge with data ----------------------------------------
# sampling <- read.csv(file = sampling_path)
# sampling_cols <- c(Province = "q3", District = "q4", Village_Name = "q5", "Intervention_Code", TPM = "tmp")
# 
# data <- sampling %>% 
#   select(all_of(sampling_cols)) %>% 
#   right_join(data, by = "TPM") %>% 
#   select(SubmissionDate, Duration, Surveyor_Name, Province, District, Village_Name, TPM, everything())
# 
# print("--------------- Pull information from sampling and merge with data.")

# attach value labels ----------------------------------------
data <- atRfunctions::labeler(data = data,
                              tool = tool_path,
                              survey_label = "label",
                              choice_lable = "label")

family_roster <- atRfunctions::labeler(data = family_roster,
                                       tool = tool_path,
                                       survey_label = "label",
                                       choice_lable = "label")

crops <- atRfunctions::labeler(data = crops,
                               tool = tool_path,
                               survey_label = "label",
                               choice_lable = "label")

print("--------------- Attach value lables.")

# recode ----------------------------------------
crops <- crops %>% 
  mutate(
    Type_of_Crop = case_when(
      Type_of_Crop == "Onions" ~ "Onion",
      Type_of_Crop == "Other" ~ Type_of_Crop_Other,
      TRUE ~ Type_of_Crop
    ),
    Type_of_Crop_Other = NA
  )

data <- data %>% 
  mutate(B2_Other = ifelse(B2_Other %in% c("The greenhouse has been sold",
                                           "The greenhouse has been sold.",
                                           "There is no greenhouse it has been sold"),
                           "The greenhouse has been sold", B2_Other)) %>% 
  mutate(
    B2 = ifelse(B2 == "Other", B2_Other, B2),
    B2_Other = NA
  )

data <- data %>% 
  mutate(
    C3 = ifelse(C3 == "Other", "+60 Years", C3),
    D3_Other = NULL
  )

# fix audio/image link ----------------------------------------
data <- atRfunctions::concat_url(data = data, tool = tool_path)
family_roster <- atRfunctions::concat_url(data = family_roster, tool = tool_path)
crops <- atRfunctions::concat_url(data = crops, tool = tool_path)

print("--------------- Fix audio/image urls.")

# merge questions from main sheet to child sheets ----------------------------------------
main_cols <- c("SubmissionDate", "Province", "District", "Village_Name", "TPM", "qa_status", "PARENT_KEY" = "KEY")

family_roster <- right_join(data %>% select(all_of(main_cols)),
                            family_roster, by = "PARENT_KEY") %>% 
  relocate(qa_status, PARENT_KEY, .before = KEY)

crops <- right_join(data %>% select(all_of(main_cols)),
                    crops, by = "PARENT_KEY") %>% 
  relocate(qa_status, PARENT_KEY, .before = KEY)

print("--------------- Merge questions from main sheet to child sheets.")

# incomplete greenhouses ----------------------------------------
incomplete_gh_keys <- sold_new_relocated_incompelted_gh %>%
  filter(`GH Status` == "In-Complete") %>% 
  pull(UUID) %>% unique()

incomplete_gh_keys[incomplete_gh_keys %notin% data$KEY]

data <- data %>% 
  mutate(is_incomplete = case_when(
    KEY %in% incomplete_gh_keys ~ "Yes",
    TRUE ~ "No"
  ), .after = B1)

questions_to_replace_with_NA <- c(
  "D1", "D2", "D3", "D4", "D4_Other", "D5", "D6", "D7", "D7_Other", "D8", "D9", "D9_Other", "D10", "D10_10",
  "D11", "D11_Other", "D12", "D12_Other", "Audio_Audit_2", "E1", "E1_Other", "E1_Photo_1", "E1_Photo_1_QA",
  "E1_Audio", "E1_Translation", "E2", "E3", "E3_Other", "E4", "E4_Other", "E5_Note", "E5_Photo_1", "E5_Photo_1_QA",
  "E5_Photo_2", "E5_Photo_2_QA", "E5_Photo_3", "E5_Photo_3_QA", "E5_Photo_4", "E5_Photo_4_QA", "E5_Photo_5",
  "E5_Photo_5_QA", "Cultivate_Harvest", "Inside_Cultivation_N", "Inside_Harvest_N", "Outside_Cultivation_N",
  "Outside_Harvest_N", "N_Crops", "Location", "Type", "Location_Dari", "Location_Pashto", "Type_Dari", "Type_Pashto",
  "Type_of_Crop", "Type_of_Crop_Other", "Season", "How_Many_Meters_Square", "Unit", "Area", "Production_in_KG",
  "Estimated_Income_AFN", "Cultivate_Harvest_Photo", "Cultivate_Harvest_QA", "F6", "F7", "F8", "F9", "F10",
  "F10_Other", "G1", "G2", "G2_Other", "G3", "G3_Other", "G4", "G5", "G5_Other", "G6", "G7", "G7_Other", "G8",
  "G8_Photo_1", "G8_Photo_1_QA", "G8_Photo_2", "G8_Photo_2_QA", "G8_Photo_3", "G8_Photo_3_QA", "G9_Label", "G9_1",
  "G9_2", "G9_3", "G9_4", "G9_5", "G9_6", "G9_7", "G9_8", "G9_9", "G9_10", "G9_Other", "G10", "G11", "G11_Other",
  "G12", "G13", "G13_Other", "G14", "G15", "G16", "G16_Other", "G17", "G18", "G19", "G20", "G20_Other", "G21",
  "G21_Other", "G22", "Audio_Audit_3", "H2_Photo", "H2_Photo_QA", "H3_Photo", "H3_Photo_QA", "H_Note_1",
  "H4_Photo", "H4_Photo_QA", "H5_Photo", "H5_Photo_QA", "H6_Photo", "H6_Photo_QA", "H7_Photo", "H7_Photo_QA",
  "I1", "I2", "I2_Other", "I3", "I4", "I4_Other", "I5", "I6", "I6_Other", "I7", "I8", "I9", "I9_Other", "I10",
  "I11", "I11_Other", "I12_1", "I12_2", "I12_3", "I12_4", "I12_5", "I12_6", "I12_7", "I12_8", "I12_9_Photo",
  "I12_9_Photo_QA", "J1", "J1_Photo", "J1_Photo_QA", "J2", "J2_Photo", "J2_Photo_QA", "J3", "J4", "J5", "J6",
  "J6_Other", "J7", "I7_Other", "J8", "J9", "J9_Other", "Comment_Audio", "Comment_Translation"
)

data <- data %>% 
  mutate(across(any_of(questions_to_replace_with_NA), function(x)
    x = case_when(
      KEY %in% incomplete_gh_keys ~ x[NA][1],
      TRUE ~ x
    )
  ))


crops <- crops %>% filter(PARENT_KEY %notin% incomplete_gh_keys)
print("--------------- replace main questions to missing values in incomplete greenhouses")

# delete extra objects ----------------------------------------
rm(extra_cols)
rm(main_cols)
rm(incomplete_gh_keys)
rm(questions_to_replace_with_NA)


