library(here)

library(tidyr)
library(dplyr)

library(stringr)
library(zoo)

library(ggplot2)
library(Themo)

Sys.setlocale("LC_TIME", "es_ES")

raw <- readxl::read_excel(here("./por_rama.xlsx"),
                    na = c("N/E", "NA"), skip = 4)

long <- raw |>
    select(-2) |>
    mutate(n_Rama = str_extract(Título,  "(?<=Rama )\\d{4}"),
        Rama = str_extract(Título, "(?<=Rama \\d{4}\\. ).+"),
        Sector = str_extract(Título, "(?<=Sector económico )\\w+"),
        Sector_scian = str_extract(Título, "(?<=(\\d{2}\\. )).*(?=, Rama)")) |>
    select(-Título) |>
    pivot_longer(`40544`:last_col(4),
                names_to = "Mes", values_to = "Índice") |>
    mutate(Mes = openxlsx::convertToDate(Mes) |> as.yearmon())
    