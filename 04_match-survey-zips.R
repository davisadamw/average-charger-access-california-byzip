library(tidyverse)

# load survey zips
survey_zips <- readxl::read_excel("Data/Zip codes.xlsx",
                                  col_types = c("text", "text"),
                                  col_names = c("row", "home_zip"))

# load zip charger summaries
zip_charger_summaries <- read_rds("Outputs/homezip_workcharger_summaries.rds")

# join and write to disk
survey_zips %>% 
  left_join(zip_charger_summaries, by = "home_zip") %>% 
  write_csv("Outputs/survey_zip_chargers.csv")
