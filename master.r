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

## Main Inflation Plot

graphy <- main |>
    filter(Mes > as.yearmon("2018-12-31"),
        months(Mes) == "enero") |>
    mutate(Mes = as.character(Mes)) |>
    ggplot(aes(Mes, Inflación, fill = Subíndice, color = Subíndice, group = NA, order = Subíndice)) +
    stat_halfeye(adjust = .5, width = .5, .width = c(0.5, 1), 
                point_colour = "black", interval_colour = "black", fill = "#388697") +
    geom_dots(side = "left", dotsize = 1, justification = 1.05, binwidth = .003) +
    geom_hline(yintercept = .03, linetype = "dashed", color = "black") +
    ###Scales
    scale_y_continuous(limits = c(-0.3, 0.3), labels = scales::percent, n.breaks = 6) +
    scale_color_manual(values = c("#27187E", "#BA1200")) +
    scale_fill_manual(values = c("#27187E", "#BA1200")) +
    ###Themes
    themo() +
    theme(axis.title.y = element_text(hjust = .99, margin = margin(t = 0, r = 0, b = 0, l = 10)),
            legend.position = c(.01, 1),
              legend.background = element_blank(),
              legend.direction = "horizontal") +
    ###Legend Changes
    guides(fill = guide_legend(override.aes = list(linetype = c(0,0))),
            color = guide_legend(size = 3)) +
    ###Labels
    labs(title = "Inflación de los genéricos del INPC",
        subtitle = "La distribución de la inflación anual de los genéricos se ha vuelto asimétrica con mayor peso hacia\ninflaciones altas. En general, la distribución se aleja de la meta de Banxico (línea de trazos)." ,
        caption = "Fuente: INEGI \nNota: Algunos genéricos tuvieron inflaciones fuera de los límites de la gráfica, por ejemplo, el chile serrano en \nenero de 2023 tuvo una inflación de 115%. Los consideré outliers y ninguno tiene una poderación muy alta.")


    #scale_color_manual(values = c("#E493C1", "#7D83FF", "#BB342F", "#638475", "#DB9D47")) +


jpeg("other_pr.jpg", width = 900, height = 900, units = "px")
graphy
ignier()
dev.off()
