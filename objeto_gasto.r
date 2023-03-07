library(readxl)

library(tidyr)
library(dplyr)

library(stringr)
library(zoo)

library(ggplot2)
library(ggforce)
library(ggdist)
library(gghalves)
library(ggridges)
library(Themo)

Sys.setlocale("LC_TIME", "es_ES")

main |>
    filter(Mes > as.yearmon("2021-12-31"),
        months(Mes) == "enero") |>
    mutate(Mes = as.character(Mes)) |>
    ggplot(aes(x = Mes, y = Inflación, fill = Grupo_General,
        group = NA, order = Grupo_General)) +
    geom_dots(linewidth = 0) +
    scale_y_continuous(limits = c(-0.3, 0.3), labels = scales::percent, n.breaks = 6) +
    themo() +
    theme(axis.title.y = element_text(hjust = .99, margin = margin(t = 0, r = 0, b = 0, l = 10)),
            legend.position = c(.67, .23),
              legend.background = element_blank(),
              legend.direction = "vertical") +
        labs(title = "Inflación de los genéricos por finalidades",
        subtitle = "",
        caption = "Fuente: INEGI \nNota:")
