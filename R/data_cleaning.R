##### data cleaning #####
# remove unnecessary columns ----------------------------------------
extra_cols <- c("Starttime", "Endtime", "Deviceid", "Subscriberid", "Simid", "Devicephonenum", "Username", "mean_light_level", "sd_light_level",
                "mean_movement", "sd_movement", "mean_sound_level", "sd_sound_level", "pct_quiet", "pct_still", "Text_Audio", "Audio_Audit",
                "instanceID", "instanceName", "formdef_version", "i", "ii", "Location_Dari", "Location_Pashto", "Type_Dari", "Type_Pashto")

data <- data %>% select(-any_of(extra_cols), -starts_with("SET-OF"))
family_roster <- family_roster %>% select(-any_of(extra_cols), -starts_with("SET-OF"))
crops <- crops %>% select(-any_of(extra_cols), -starts_with("SET-OF"))

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

# fix audio/image link ----------------------------------------
data <- atRfunctions::concat_url(data = data, tool = tool_path)
family_roster <- atRfunctions::concat_url(data = family_roster, tool = tool_path)
crops <- atRfunctions::concat_url(data = crops, tool = tool_path)

print("--------------- Fix audio/image urls.")

# merge questions from main sheet to child sheets ----------------------------------------
main_cols <- c("SubmissionDate", "Province", "District", "Village_Name", "TPM", "qa_status", "PARENT_KEY" = "KEY")

family_roster <- right_join(data %>% select(all_of(main_cols)),
                            family_roster, by = "PARENT_KEY") %>% 
  relocate(PARENT_KEY, .before = KEY)

crops <- right_join(data %>% select(all_of(main_cols)),
                    crops, by = "PARENT_KEY") %>% 
  relocate(PARENT_KEY, .before = KEY)

print("--------------- Merge questions from main sheet to child sheets.")

rm(extra_cols)
rm(main_cols)

