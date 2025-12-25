# clear all
rm(list = ls())

# load libraries
library(dplyr)
options(warn = -1)

# create directory for results
if (!dir.exists(file.path(getwd(), "datasets", "Saraiva 2023"))) {
  dir.create(file.path(getwd(), "datasets", "Saraiva 2023"),
    recursive = TRUE
  )
}
results_folder <- "datasets/Saraiva 2023"

# -------- DATASET READ AND CLEAN --------
# read dataset from Nathalia Oliveira et al. (2023)
raw_dataset <- readxl::read_xlsx(file.path(
  getwd(),
  "datasets",
  "Saraiva 2023",
  "Tabelas do Relatorio - Nathalia Oliveira (Agnaldo Lopes) - marco 2023.xlsx")
)

# drop header rows
raw_dataset <- raw_dataset[-c(1, 2), , drop = FALSE]
colnames(raw_dataset) <- as.character(unlist(raw_dataset[1, ]))
raw_dataset <- raw_dataset[-1, ]

# rename and convert variables to numeric
raw_dataset <- raw_dataset |>
  dplyr::rename(
    Idade = `Idade (anos)`,
    Peso = `Peso (kg)`,
    Altura = `Altura (m)`,
    IMC = `IMC (kg/m2)`,
    FC = `FC antes (bpm)`,
    Borg = `BORG 6'`,
    `6MWD` = `Distância TC6' (m)`
  ) |>
  dplyr::mutate(dplyr::across(
    c(Idade, Peso, Altura, IMC, FC, Borg, `6MWD`),
    as.numeric
  ))

# define variables per protocol
raw_dataset <- raw_dataset |>
  dplyr::mutate(
    Sexo = "female",
    Corredor = 30
  )

# select only relevant variables
raw_dataset <- raw_dataset %>%
  dplyr::select(Corredor, Sexo, Idade, Peso, Altura, IMC, FC, Borg, `6MWD`)

dataset <- raw_dataset

# -------- MODEL PARAMETERS --------
B <- 1000

source("ensemble.R")

# the end
