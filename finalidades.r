library(ggplot2)
library(Themo)

key_f <- readRDS("./Data/Keys/key_f.RDS")

main <- long |>
    left_join(key_f, by = "Finalidades") |>
    mutate(Ponderador_s = Ponderador.x / Ponderador.y,
            Contribución = (Indice * Ponderador_s) / 100) |>
    group_by(Mes, Finalidades, Ponderador.y) |>
    summarise(Índice = sum(Contribución)) |>
    ungroup() |>
    left_join(g, by = "Mes") |>
    group_by(Finalidades) |>
    mutate(Contribución = Ponderador.y * ((Índice - lag(Índice, 12)) / lag(Total, 12))) |>
    ungroup() |>
    filter(Mes >= as.yearmon("2022-09-01")) |>
    mutate(Mes = as.factor(Mes))

general <- g |>
    mutate(General = Total / lag(Total, 12) - 1) |>
    filter(Mes >= as.yearmon("2022-09-01")) |>
    mutate(Mes = as.factor(Mes))

colors <- c("#AA4465", "#D58936", "#437C90", "#A61C3C", "#255957", "#CBD4C2",
            "#694A38", "#E0777D", "#3D348B", "#70AE6E", "#0F8B8D", "#0D0630")

graphy  <- 

ggplot() +
    geom_bar(data = main,
            aes(Mes, Contribución, fill = Finalidades),
            position = "stack", stat = "identity") +
    geom_line(data = general,
            aes(Mes, General, group = NA),
            linewidth = 1.5) +
    geom_point(data = general,
            aes(Mes, General),
            size = 10, shape = 18) +
    themo() +
    theme(legend.position = c(0.075, 1.13),
        ) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::percent,
                        breaks = seq(0, 0.14, by = 0.02),
                        limits = c(-0.003, .14)) +
        labs(title = "Variación del INPC y contribución por finalidades",
        caption = "Fuente: INEGI") +
        ylab("Contribución")





jpeg("fint.jpg", width = 900, height = 900, units = "px")
graphy
ignier()
dev.off()