library(here)

library(tidyr)
library(dplyr)

library(stringr)
library(zoo)

library(ggplot2)
library(Themo)

Sys.setlocale("LC_TIME", "es_ES")

raw <- readxl::read_excel(here("./durabilidad_bienes.xlsx"),
                    na = c("N/E", "NA"), skip = 4)

long <- raw |>
    select(-2) |>
    mutate(Título = str_extract(Título, "(?<=bienes, ).*")) |>
    slice(-1) |>
    pivot_longer(`36526`:last_col(),
                names_to = "Mes", values_to = "Índice") |>
    mutate(Mes = openxlsx::convertToDate(Mes) |> as.yearmon())


main <- long |>
    filter(Mes >= as.yearmon("2020-01-01")) |>
    group_by(Título) |>
    mutate(Inflación = (Índice / Índice[1]) - 1)


main |>
    ggplot(aes(Mes, Inflación, color = Título)) +
    geom_line(linewidth = 1.5) +
    scale_y_continuous(labels = scales::percent, limits = c(-0.025, .3)) +
    scale_x_yearmon() +
    themo() +
    theme(axis.title.y = element_text(hjust = .99,
                            margin = margin(t = 0, r = 0, b = 0, l = 10)),
            legend.position = c(.05, 1),
              legend.background = element_blank(),
              legend.direction = "vertical",
        ) +
        labs(title = "Inflación acumulada del INPC por durabilidad",
        subtitle = "La distribución de la inflación anual de los genéricos se ha vuelto asimétrica con mayor peso hacia\ninflaciones altas. En general, la distribución se aleja de la meta de Banxico (línea de trazos)." ,
        caption = "Fuente: INEGI") +
        ylab("Variación %")