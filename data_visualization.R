# status: in-progress ----------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(tidytext)) install.packages("tidytext")

# read data and analysis plan -----------------------------------------
data_path <- "output/cleaned_data/CBARD_Greenhouse_Verification_cleaned_dt.xlsx"
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

# visualization -----------------------------------------
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
  NULL; p

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


