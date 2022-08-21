## progress report - cumulative
progress_report_cumulative <- full_join(sampling %>% 
                                          group_by(Province = q3, District = q4) %>% 
                                          count(name = "target")
                                        ,
                                        data %>% 
                                          group_by(Province, District) %>% 
                                          count(name = "visited")
                                        ,
                                        by = c("Province", "District")) %>% 
  full_join(data %>% 
              group_by(Province, District) %>% 
              count(qa_status) %>% 
              pivot_wider(names_from = qa_status, values_from = n)
            ,
            by = c("Province", "District")) %>% 
  mutate(across(everything(), function(x)
    ifelse(is.na(x), 0, x)
  ))

progress_report_cumulative

## progress report - weekly
progress_report_weekly <- data %>% 
    filter(SubmissionDate >= weekly_start_date & SubmissionDate <= weekly_end_date) %>% 
    group_by(Province, District) %>% 
    count(qa_status) %>% 
    pivot_wider(names_from = qa_status, values_from = n)

cat(glue::glue("Progress report, cumulative: from {start_date} to {end_date}."))
print(knitr::kable(progress_report_cumulative, format = "simple"))
cat("\n")
cat(glue::glue("Progress report, weekly: from {weekly_start_date} to {weekly_end_date}."))
print(knitr::kable(progress_report_weekly, format = "simple"))

progress_report <- list(
  cumulative = progress_report_cumulative,
  weekly = progress_report_weekly
)

rm(start_date, end_date, weekly_start_date, weekly_end_date, sampling, progress_report_cumulative, progress_report_weekly)

