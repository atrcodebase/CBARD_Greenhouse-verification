data_raw <- data
family_roster_raw <- family_roster
crops_raw <- crops

# apply correction log ----------------------------------------
if (nrow(filter(correction_log, sheet == "data")) > 0) {
  data <- atRfunctions::apply_log(data = data, log = filter(correction_log, sheet == "data"),
                                  data_KEY = "KEY",
                                  log_columns = c(question = "question",
                                                  old_value = "old_value",
                                                  new_value = "new_value",
                                                  KEY = "KEY")
  )
}

# if (nrow(filter(correction_log, sheet == "Family_Roster")) > 0) {
#   family_roster <- atRfunctions::apply_log(data = family_roster, log = filter(correction_log, sheet == "Family_Roster"),
#                                            data_KEY = "KEY",
#                                            log_columns = c(question = "question",
#                                                            old_value = "old_value",
#                                                            new_value = "new_value",
#                                                            KEY = "KEY")
#   )
# }

if (nrow(filter(correction_log, sheet == "Crops")) > 0) {
  crops <- atRfunctions::apply_log(data = crops, log = filter(correction_log, sheet == "Crops"),
                                   data_KEY = "KEY",
                                   log_columns = c(question = "question",
                                                   old_value = "old_value",
                                                   new_value = "new_value",
                                                   KEY = "KEY")
  )
}

rm(correction_log)


