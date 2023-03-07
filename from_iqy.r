library(readxl)

library(tidyr)
library(dplyr)

library(stringr)
library(zoo)

raw <- read_excel("./Inegi/genericos.xlsx", na = c("N/E", "NA"), skip = 4)

key <- readRDS("./Data/Key.RDS")
key_s <- readRDS("./Data/Keys/key_s.RDS")
key_s2 <- readRDS("./Data/Keys/key_s2.RDS")
key_s3 <- readRDS("./Data/Keys/key_s3.RDS")
key_og <- readRDS("./Data/Keys/key_og.RDS")

long <- raw |>
    select(-c(`Periodo disponible`, Periodicidad, `Tipo cifra`,
        Unidad, Fuente, Aviso)) |>
    pivot_longer(`25204`:last_col(), names_to = "Mes", values_to = "Indice") |>
mutate(Título = str_remove(Título, ".*\\d{3} "),
    Mes = openxlsx::convertToDate(Mes) |> as.yearmon()) |>
    left_join(key, by = c("Serie", "Título"))

main <- long |>
    group_by(Título) |>
    arrange(Mes) |>
    mutate(Inflación = Indice / lag(Indice, 12) - 1) |>
    ungroup()

## Sanity Check


## Inflationer

inflationer <- function(.data, .prueba) {
.data |>
    arrange(Mes) |>
    mutate(Inflación = Total / lag(Total, 12) - 1) |>
    filter(Mes == .prueba)
}


prueba <- as.yearmon("2023-01-01")

g <- long |>
    mutate(Contribución = (Indice * Ponderador) / 100) |>
    group_by(Mes) |>
    summarise(Total = sum(Contribución))

og <- long |>
    left_join(key_gg, by = "Objeto_Gasto") |>
    mutate(Ponderador_s = Ponderador.x / Ponderador.y,
            Contribución = (Indice * Ponderador_s) / 100) |>
    group_by(Mes, Objeto_Gasto) |>
    summarise(Índice_gg = sum(Contribución))
    
   # |>
   # inflationer(prueba)

s <- long |>
    left_join(key_s, by = "Subíndice") |>
    mutate(Ponderador_s = Ponderador.x / Ponderador.y) |>
    mutate(Contribución = (Indice * Ponderador_s) / 100) |>
    group_by(Mes, Subíndice) |>
    summarise(Total = sum(Contribución)) |>
    group_by(Subíndice) |>
    inflationer(prueba)

s2 <- long |>
    left_join(key_s2, by = "Subíndice_2") |>
    mutate(Ponderador_s2 = Ponderador.x / Ponderador.y) |>
    mutate(Contribución = (Indice * Ponderador_s2) / 100) |>
    group_by(Mes, Subíndice_2) |>
    summarise(Total = sum(Contribución)) |>
    group_by(Subíndice_2) |>
    inflationer(prueba)

g
s
s2