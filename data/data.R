library(tidyverse)

data <-
  readxl::read_excel("data-raw/210607 Serology_results HK14e & HK14c.xlsx")

data_renamed <- data %>%
  mutate(
    titre = 2^(cL2HI),
  ) %>%
  select(
    ferret,
    experiment = Expt,
    day = Day,
    dose,
    innoculum,
    innoculum_n = innoculumN,
    virus_year = vYear,
    virus_short = Abbr,
    virus_n = VirusN,
    titre,
    plate = `Plate Name`,
  )

data_renamed %>% write_csv("data/data.csv")
