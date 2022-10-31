# status: in-progress 

# load required packages ----------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(tidytext)) install.packages("tidytext")

# custom functions ----------------------------------------
plot_overall <- function(dt,  var) {
  dt %>%
    filter(Question == var) %>%
    arrange(desc(Response)) %>%
    mutate(ypos = cumsum(Result)- 0.5*Result ) %>% 
    {if (nrow(.) < 5) {
      ggplot(., aes(x = "", y = Result, fill = Response)) +
        geom_bar(stat="identity", width=1,) +
        geom_text(aes(y = ypos, label = paste0(round(Result, 1), "%")),
                  size = 3) +
        scale_fill_brewer(palette="Set2") +
        coord_polar("y", start=0) +
        labs(x = NULL, y = "Percent",
             title = unique(.["Questionnaire_question"]),
             fill = NULL
        ) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "bottom"
        )
    } else {
      ggplot(., aes(x = reorder(Response, Result), y = Result)) +
        geom_col(fill = "skyblue") +
        geom_text(aes(label = paste0(round(Result, 1), "%")),
                  size = 3, hjust = -0.1) +
        theme_bw() +
        coord_flip() +
        labs(x = NULL, y = "Percent",
             title = unique(.["Questionnaire_question"]),
             fill = NULL
        )
    }
    }
}

# read cleaned data and analysis table ----------------------------------------
data_path <- "output/cleaned_data/CBARD_Greenhouse_Verification_cleaned_dt.xlsx"
data <- readxl::read_excel(data_path, sheet = "data", na = c("", "-", " ", "NA", "N/A", "NULL"))
family_roster <- readxl::read_excel(data_path, sheet = "Family_Roster", na = c("", "-", " ", "NA", "N/A", "NULL"))
crops <- readxl::read_excel(data_path, sheet = "Crops", na = c("", "-", " ", "NA", "N/A", "NULL"))

descriptive_analysis <- readxl::read_excel("output/analysis/cbard_descriptive_analysis.xlsx", sheet = "analysis")

########## plots - overall
table_for_plots_all <- descriptive_analysis %>% 
  filter(Disaggregation == "all") %>% 
  filter(Aggregation_method == "perc") %>% 
  group_by(Question) %>% 
  mutate(n = n_distinct(Response)) %>% 
  ungroup() %>% 
  filter(n != 1)

plot_list <- map(table_for_plots_all %>% pull(Question) %>% unique(),
                 function(x)
                   plot_overall(dt = table_for_plots_all, var = x)
                 )

names(plot_list) <- (table_for_plots_all %>% pull(Question) %>% unique())
plot_list$B1
plot_list$J9

# "G4" - check title title 
# do not include questions from G9_1 to G9_9
# do not include questions from I1 to I11
# do not include questions that start with I12 (I12_2, I12_4, I12_6, I12_8)

########## plots - by province
# plot_grp <- function(dt, var, grp) {}
table_for_plots_by_province <- descriptive_analysis %>% 
  filter(Disaggregation == "Province")

########## custom plots
## common crops cultivated inside the greenhouses, by province
crops %>% 
  filter(Type == "Cultivated") %>% 
  filter(Location == "Inside") %>% 
  group_by(Province) %>% 
  mutate(n_greenhouse = n_distinct(TPM)) %>% 
  group_by(Province, n_greenhouse) %>% 
  count(Type_of_Crop) %>% 
  mutate(percent = round(n/n_greenhouse*100, 1)) %>% 
  mutate(Province = as.factor(Province),
         Type_of_Crop = tidytext::reorder_within(Type_of_Crop, -n, Province)) %>% 
  ggplot(aes(x = Type_of_Crop, y = percent, label = paste0(round(percent), "%"), fill = Province)) +
  geom_col(show.legend = TRUE) +
  geom_text(size = 2, vjust = -0.5) +
  scale_x_reordered() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -30, size = 6),
    legend.position = "bottom"
  ) +
  labs(x = NULL, y = "Percent", fill = NULL) +
  scale_fill_manual(values = c("darkred", "orange", "skyblue")) +
  NULL

## common crops cultivated inside the greenhouses, by greenhouse's size
crops %>% 
  filter(Type == "Cultivated") %>% 
  filter(Location == "Inside") %>% 
  left_join(
    data %>% select(E1, PARENT_KEY = KEY),
    by = c("PARENT_KEY")
  ) %>% 
  filter(E1 != "Other") %>% 
  group_by(E1) %>% 
  mutate(n_greenhouse = n_distinct(TPM)) %>% 
  group_by(E1, n_greenhouse) %>% 
  count(Type_of_Crop) %>% 
  mutate(percent = round(n/n_greenhouse*100, 1)) %>% 
  mutate(E1 = as.factor(E1),
         Type_of_Crop = tidytext::reorder_within(Type_of_Crop, -n, E1)) %>% 
  ggplot(aes(x = Type_of_Crop, y = percent, label = paste0(round(percent), "%"), fill = E1)) +
  geom_col(show.legend = TRUE) +
  geom_text(size = 2, vjust = -0.5) +
  scale_x_reordered() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -30, size = 6),
    legend.position = "bottom"
  ) +
  labs(x = NULL, y = "Percent", fill = NULL) +
  scale_fill_manual(values = c("darkred", "orange", "skyblue")) +
  NULL

