# clear all
rm(list = ls())

# create directory for results
if (!dir.exists(file.path(getwd(), "datasets", "Pinto 2019"))) {
  dir.create(file.path(getwd(), "datasets", "Pinto 2019"),
             recursive = TRUE
  )
}

results_folder <- "datasets/Pinto 2019"

# -------- DATASET READ AND CLEAN --------
# read dataset from Pinto et al. (2019)
raw_dataset_1 <- readxl::read_xlsx(file.path(
  getwd(),
  "datasets",
  "Pinto 2019",
  "tABELA DADOS DOUTORADO05.09.2017.xlsx"),
  sheet = 1
)

# rename and convert variables to numeric
raw_dataset_1 <- raw_dataset_1 |>
  rename(
    Sexo = `SEXO`,
    Idade = `IDADE`,
    Altura = `ALTURA`,
    Peso = `PESO`,
    IMC = `IMC`,
    IPAQ_cod = `IPAQ CÓDIGO`
  ) |>
  dplyr::mutate(dplyr::across(
    c(Sexo, Idade, Altura, Peso, IMC, IPAQ),
    as.numeric
  ))

dataset <- raw_dataset_1 %>% dplyr::select(
  Sexo,
  Idade,
  Altura,
  Peso,
  IMC,
  IPAQ_cod
)

# rename IPAQ_cod to IPAQ
dataset <- dataset %>%
  dplyr::rename(IPAQ = IPAQ_cod)

# recode female = 0, male = 1
dataset <- dataset %>%
  dplyr::mutate(Sexo = dplyr::case_when(
    Sexo == 0 ~ "female",
    Sexo == 1 ~ "male"
  ))

# read second sheet with more variables
raw_dataset_2 <- readxl::read_xlsx(file.path(
  getwd(),
  "datasets",
  "Pinto 2019",
  "tABELA DADOS DOUTORADO05.09.2017.xlsx"),
  sheet = 2
)

# rename and convert variables to numeric
raw_dataset_2 <- raw_dataset_2 |>
  rename(
    `VEF¹` = `VEF1`
  ) |>
  dplyr::mutate(dplyr::across(
    c(`VEF¹`),
    as.numeric
  ))

# bind dataset with new variables from sheet 2
dataset <- dataset %>%
  dplyr::bind_cols(
    raw_dataset_2 %>%
      dplyr::select(
        `VEF¹` = `VEF¹`
      )
  )

# read second sheet with more variables
raw_dataset_3 <- readxl::read_xlsx(file.path(
  getwd(),
  "datasets",
  "Pinto 2019",
  "tABELA DADOS DOUTORADO05.09.2017.xlsx"),
  sheet = 3
)

col_names <- c("Nome", colnames(raw_dataset_3)[2:5], raw_dataset_3[1, 6:ncol(raw_dataset_3)])
# rename columns
raw_dataset_3 <- raw_dataset_3 %>%
  `colnames<-`(col_names)
raw_dataset_3 <- raw_dataset_3[-1, ]
colnames(raw_dataset_3) <- make.unique(colnames(raw_dataset_3))

# bind dataset with new variables from sheet 3
dataset <- dataset %>%
  dplyr::bind_cols(
    raw_dataset_3 %>%
      dplyr::select(
        `6MWD` = `DTC6M.2`,
        `FC` = `FC 0'`,
        `FC6` = `FC 6'`,
        `Borg` = `BORG 0'`
      )
  ) %>%
  dplyr::mutate(
    `6MWD` = as.numeric(`6MWD`),
    FC = as.numeric(FC),
    FC6 = as.numeric(FC6),
    Borg = as.numeric(Borg)
  )

# calculate deltaFC and drop FC6
dataset <- dataset |>
  dplyr::mutate(
    `∆FC` = FC6 - FC
  ) |>
  dplyr::select(-FC6)

# define variables per protocol
dataset <- dataset |>
  dplyr::mutate(
    Corredor = 30
  )

# -------- MODEL PARAMETERS --------
B <- 100

source("ensemble.R")

# the end
