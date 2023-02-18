library(readxl)

library(tidyr)
library(dplyr)

library(stringr)
library(inegiR)
library(zoo)

Sys.setlocale("LC_TIME", "es_ES")

raw <- read_excel("./genericos.xlsx", na = c("N/E", "NA"), skip = 4)

long2 <- raw |>
    select(-c(`Periodo disponible`, Periodicidad, `Tipo cifra`,
        Unidad, Fuente, Aviso)) |>
    pivot_longer(`25204`:last_col(), names_to = "Mes", values_to = "Indice") |>
mutate(Número = str_extract(Título, " \\d{3} "),
    Título = str_remove(Título, ".*\\d{3} "),
    Mes = openxlsx::convertToDate(Mes) |> as.yearmon())

main <- long |>
    group_by(Título) |>
    arrange(Mes) |>
    mutate(Inflación = Indice / lag(Indice, 12) - 1)


pnd <- readRDS("./Data/Ponderadores.RDS")

a <- full_join(long, pnd, by = c("Título" = "Genérico"))

key <- a |>
    pivot_wider(names_from = Mes, values_from = Indice) |>
    select(Título, Serie, Ponderador, Grupo_General,
            Subíndice, Subíndice_2, Subíndice_3)

saveRDS(key, "./Data/Key.RDS")

token <- "4c8e02dc-59ab-dff6-3f81-cc9694d615ff"

series <- key[[2]]

prueba <- inegi_series_multiple(series, token)


library(httr)
library(jsonlite)
library(rjson)

#Llamado al API

url <- paste("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/", series[1], "es/00/false/BIE/2.0/", token, "?type=json", sep = "")


url <-"https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/1002000001/es/00000/false/BISE/2.0/[Aquí va tu Token]?type=json"
respuesta<-GET(url)
datosGenerales<-content(respuesta,"text")
flujoDatos<-paste(datosGenerales,collapse = " ")

flujoDatos<-fromJSON(flujoDatos)


url <- "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/627614,627615/es/0700/false/BIE/2.0/4c8e02dc-59ab-dff6-3f81-cc9694d615ff?type=json"

#Llamado al API

respuesta<-GET(url)
datosGenerales<-content(respuesta,"text")
flujoDatos<-paste(datosGenerales,collapse = " ")

#Obtención de la lista de observaciones 
flujoDatos<-fromJSON(flujoDatos)

flujoDatos<-flujoDatos$Series

flujoDatos<-

flujoDatos$OBSERVATIONS

inegi_series(627614, token)

as_tibble(matrix(nrow = 300, ncol = length(series)), .name_repair = ~ as.character(series))

tibble(as.character(series), .rows = 200)

tbl <- tibble::tibble(!!!series, .rows = 649)

tibble(a = 2, b = 4)
