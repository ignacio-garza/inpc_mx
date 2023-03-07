library(here)

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

raw <- readxl::read_excel(here("./Inegi/por_rama.xlsx"),
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

main <- long |>
    group_by(Rama) |>
    arrange(Mes) |>
    mutate(Inflación = Índice / lag(Índice, 12) - 1)

graphy <- 


main |>
    filter(Mes > as.yearmon("2018-12-31"),
        months(Mes) == "enero") |>
    mutate(Mes = as.character(Mes)) |>
    ggplot(aes(Mes, Inflación, fill = Sector)) +
    stat_halfeye(adjust = .5, width = .5, .width = c(0.5, 1), 
                point_colour = "black", interval_colour = "black", fill = "#388697") +
    geom_dots(side = "left", dotsize = 1, justification = 1.05, binwidth = .003) +
    geom_hline(yintercept = .03, linetype = "dashed", color = "black") +
    ###Scales
    scale_y_continuous(limits = c(-0.3, 0.3), labels = scales::percent, n.breaks = 6) +
    #scale_color_manual(values = c("#27187E", "#BA1200")) +
    #scale_fill_manual(values = c("#27187E", "#BA1200")) +
    ###Themes
    themo()

  lolos <- main |>
    filter(Mes > as.yearmon("2018-12-31")) |>
    pivot_wider(names_from = n_Rama)
    
    
    write.csv(lolos, "lolin.csv")
