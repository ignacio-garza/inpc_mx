library(readxl)

library(tidyr)
library(dplyr)

raw <- read_excel("./genericos.xlsx", na = c("N/E", "NA"), skip = 4)

key <- readRDS("./Data/Key.RDS")

long <- raw |>
    select(-c(`Periodo disponible`, Periodicidad, `Tipo cifra`,
        Unidad, Fuente, Aviso)) |>
    pivot_longer(`25204`:last_col(), names_to = "Mes", values_to = "Indice") |>
mutate(Título = str_remove(Título, ".*\\d{3} "),
    Mes = openxlsx::convertToDate(Mes) |> as.yearmon()) |>
    left_join(key, by = "Serie")

main <- long |>
    group_by(Título) |>
    arrange(Mes) |>
    mutate(Inflación = Indice / lag(Indice, 12) - 1)
