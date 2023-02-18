library(readxl)
library(tidyr)
library(dplyr)

a <- read_excel("./Ponderadores.xlsx")

a |>
    group_by(Subíndice_3) |>
    summarise(total = sum(Ponderador))

saveRDS(a, "Ponderadores.RDS")
