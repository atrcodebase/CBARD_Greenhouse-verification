## merge qa-log with data
data <- left_join(data,
                  qa_log %>% mutate(KEY = as.character(UUID), qa_status = as.character(`Final QA Status`)) %>%
                    select(qa_status, KEY),
                  by = "KEY"
                  ) %>% 
  mutate(
    qa_status = case_when(
      is.na(qa_status) ~ "Pending",
      TRUE ~ qa_status
      ),
    SubmissionDate = lubridate::ymd(word(SubmissionDate, start = 1, end = 1))
    )

## qa-backlog summary
unresolved_cases <- data %>% 
  filter(qa_status %notin% c("Approved", "Rejected")) %>% 
  select(SubmissionDate, qa_status, KEY)

unresolved_cases_summary <- unresolved_cases %>% 
  group_by(SubmissionDate) %>% 
  count(qa_status) %>% 
  pivot_wider(names_from = qa_status, values_from = n)

cat("Unresovled Cases either Pending or NA in the QA log\n")
cat("Only displays the dates where there is still QA Backlog:")
print(knitr::kable(unresolved_cases_summary, format = "simple"))

rm(unresolved_cases_summary)


