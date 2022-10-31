# check skip logics
check_skip_logic <- function(dt, tool_path, column, value_labels_are_attached = TRUE) {
  
  questions <- readxl::read_excel(tool_path, sheet = "survey")
  choices <- readxl::read_excel(tool_path, sheet = "choices")
  
  question_values <- questions %>% 
    select(type, name, relevance) %>% 
    mutate(logic = relevance) %>% 
    filter(name %in% colnames(dt)) %>% 
    mutate(relevance = gsub("selected\\(|\\$\\{|'|)|}", "", relevance)) %>% 
    mutate(relevance = gsub("=", ",", relevance)) %>% 
    # filter(!str_detect(relevance, "or")) %>% # check this part later
    separate(relevance, into = c("check_with_ques", "check_with_value"), sep = ",") %>% 
    mutate(is_multi_select = ifelse(str_detect(type, "select_multiple"), "Yes", "No")) %>%
    mutate(type = gsub(".* ", "", type)) %>% 
    rename(ques = name)
  
  question_values_labels <- choices %>%
    select(type = list_name, check_with_value = name, check_with_label = label) %>%
    drop_na() %>% 
    left_join(
      question_values %>% select(type, check_with_ques = ques, is_multi_select),
      by = "type"
    ) %>% 
    filter(!is.na(check_with_ques)) %>% 
    select(-type) %>% 
    right_join(
      question_values %>% select(-is_multi_select),
      by = c("check_with_ques", "check_with_value")
    ) %>% 
    select(type, ques, check_with_ques, check_with_value, check_with_label, logic, is_multi_select) %>% 
    filter(!is.na(check_with_ques)) %>% 
    filter(!str_detect(check_with_value, "or"))
  
  question <- column
  check_with_ques <- question_values_labels %>% filter(ques == question) %>% pull(check_with_ques)
  
  if(length(check_with_ques) != 0) {
    
    if(value_labels_are_attached) {
      logic <- question_values_labels %>% filter(ques == question) %>% pull(check_with_label)
    } else {
      logic <- question_values_labels %>% filter(ques == question) %>% pull(check_with_value)
    }
    
    is_ques_multi_select <- question_values_labels %>% filter(ques == question) %>% pull(is_multi_select)
    rel <- question_values_labels %>% filter(ques == question) %>% pull(logic)
    
    
    if(is_ques_multi_select == "Yes") {
      
      dt %>% 
        filter(
          (is.na(get(question)) & str_detect(get(check_with_ques), logic)) |
            (!is.na(get(question)) & !str_detect(get(check_with_ques), logic))
        ) %>% 
        select(check_with_ques, question, qa_status, KEY) %>% 
        mutate(
          logic = paste0("Ask ", question, " when '", logic, "' is selected in ", check_with_ques),
          relevance_in_coodebook = rel
        )
      
    } else {
      
      dt %>% 
        filter(
          (is.na(get(question)) & get(check_with_ques) == logic) |
            (!is.na(get(question)) & get(check_with_ques) != logic)
        ) %>% 
        select(all_of(check_with_ques), question, qa_status, KEY) %>% 
        mutate(
          logic = paste0("Ask ", question, " when '", logic, "' is selected in ", check_with_ques),
          relevance_in_coodebook = rel
        )
      
    }
    
    # dt %>% 
    #   filter(
    #     (is.na(get(question)) & get(check_with_ques) == logic) |
    #       (!is.na(get(question)) & get(check_with_ques) != logic)
    #   ) %>% 
    #   select(check_with_ques, question, KEY) %>% 
    #   mutate(
    #     logic = paste0("Ask ", question, " when '", logic, "' is selected in ", check_with_ques),
    #     relevance_in_coodebook = rel
    #   )
    
  }
}





