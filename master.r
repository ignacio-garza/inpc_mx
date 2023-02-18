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
theme_set(theme_light(base_size = 16))

raw <- read_excel("./genericos.xlsx", na = c("N/E", "NA"), skip = 4)

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






graphy <- 


main |>
    filter(Mes > as.yearmon("2018-12-31"),
        months(Mes) == "enero") |>
    mutate(Mes = as.character(Mes)) |>
    ggplot(aes(Mes, Inflación, fill = Subíndice, color = Subíndice, group = NA)) +
    stat_halfeye(adjust = .5, width = .5, .width = c(0.5, 1), point_colour = "black", interval_colour = "black", fill = "#DB9D47") +
    geom_dots(side = "left", dotsize = 1, justification = 1.05, binwidth = .003) +
    geom_hline(yintercept = .03, linetype = "dashed", color = "black") +
    scale_y_continuous(limits = c(-0.3, 0.3), labels = scales::percent, n.breaks = 6) +
    themo() +
    scale_color_manual(values = c("#7D83FF", "#BB342F")) +
    scale_fill_manual(values = c("#7D83FF", "#BB342F")) +
    labs(title = "Inflación de los genéricos del INPC",
        subtitle = "La distribución de la inflación anual de los genéricos se ha vuelto asimétrica con mayor peso hacia\ninflaciones altas. En general, la distribución se aleja de la meta de Banxico (línea de trazos)." ,
        caption = "Fuente: INEGI \nNota: Algunos genéricos tuvieron inflaciones fuera de los límites de la gráfica, por ejemplo, el chile serrano en \nenero de 2023 tuvo una inflación de 115%. Los consideré outliers y ninguno tiene una poderación muy alta.") +
    theme(axis.title.y = element_text(hjust = .99, margin = margin(t = 0, r = 0, b = 0, l = 10)),
            legend.position = c(.1, 1),
              legend.background = element_blank(),
              legend.direction = "horizontal")

graphy +
    guides(fill = guide_legend(override.aes = list(fill = NA, linetype = c(0,0))),
            color = guide_legend(size = 3))



    #scale_color_manual(values = c("#E493C1", "#7D83FF", "#BB342F", "#638475", "#DB9D47")) +

themo_line <-  theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.text = element_text(family = "Georgia",
                                        size = 20,
                                        colour = "black"),
              #panel.grid.major.x = element_blank(),
              #panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"),
              legend.title = element_blank(),
              legend.text = element_blank(),
              legend.key = element_blank(),
              legend.position = "none",
              #legend.justification = c(0.1, 1.5),
              #legend.position = c(.1, 1),
              #legend.background = element_blank(),
              #legend.direction = "horizontal",
              text = element_text(family = "Georgia", size = 18),
              plot.title = element_text(size = 26, margin = margin(b = 10)),
              plot.subtitle = element_text(size = 18,
                                        color = "darkslategrey",
                                        margin = margin(b = 5)),
              plot.caption = element_text(size = 14,
                                            margin = margin(t = 3),
                                            color = "#8e8e8e",
                                            hjust = 0),
              plot.margin = margin(0.025, 0, 0.02, 0, "npc"))


ignier()

jpeg("other_pr.jpg", width = 900, height = 900, units = "px")
dev.off()


main |>
    filter(Mes > as.yearmon("2018-12-31"),
        months(Mes) == "enero") |>
    mutate(Mes = as.character(Mes)) |>
    ggplot(aes(Inflación, Mes)) +
    geom_density_ridges() +
    scale_x_continuous(limits = c(-0.2, 0.3)) +
    themo_line

main |>
    mutate(Mes = as.character(Mes)) |>
    View()