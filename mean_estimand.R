# -------- DATASET READ AND CLEAN --------
# read XLSX files from dataset folder
dataset_natalia <- readxl::read_xlsx(file.path(
  getwd(),
  "datasets",
  "Tabelas do Relatorio - Nathalia Oliveira (Agnaldo Lopes) - marco 2023.xlsx")
)
# remove first 2 rows
dataset_natalia <- dataset_natalia[-c(1, 2), ]
# use 1st row as column header
colnames(dataset_natalia) <- as.character(unlist(dataset_natalia[1, ]))
dataset_natalia <- dataset_natalia[-1, ]

# -------- MODEL READ --------
model_params <- readxl::read_xlsx(file.path(
  getwd(),
  "models.xlsx")
)

# loop across all columns to calculate pred_6MWD
pred_6MWD <- data.frame(
  Model = rep(NA, ncol(model_params)-5),
  Prediction = rep(NA, ncol(model_params)-5)
)

# for(i in 5:ncol(eq_6MWD)){
for(i in 10){
  # select equation, show label and parameter
  eq <- model_params[, c(1:4, i)]
  # drop empty parameters
  intercept <- eq[which(eq$Variable == "Intercept"), ]
  parameters <- eq[-which(eq$Variable == "Intercept"), ]
  parameters <- parameters[complete.cases(parameters), ]
  print(intercept)
  print(parameters)
  
  # select columns from dataset that match equation labels
  cols_of_interest <- c()
  for(j in 1:nrow(parameters)) {
    label <- parameters$`Variable (PT)`[j]
    col_index <- grep(label, colnames(dataset_natalia))
    cols_of_interest <- c(cols_of_interest, col_index)
  }
  # create subset of dataset with only relevant columns
  data_subset <- dataset_natalia[, cols_of_interest]
  # reorder columns to match parameters
  data_subset <- data_subset[, order(match(colnames(data_subset), parameters$`Variable (PT)`))]
  print(colnames(data_subset))
  
  # convert from cm to m if necessary
  for(k in 1:nrow(parameters)) {
    var_name <- parameters$`Variable (PT)`[k]
    var_unit <- parameters$Unit[k]
    if(var_unit == "cm") {
      row <- grep(var_name, colnames(data_subset))
      data_subset[[row]] <- as.numeric(data_subset[[row]]) * 100
    }
  }
  
  # calculate prediction
  intercept <- as.numeric(intercept[i])
  for(k in 1:nrow(parameters)) {
    var_name <- parameters$`Variable (PT)`[k]
    row <- grep(var_name, colnames(data_subset))
    param_value <- as.numeric(parameters[[i]][k])
    var_values <- as.numeric(data_subset[[row]])
    if(k == 1) {
      prediction <- intercept + (param_value * var_values)
    } else {
      prediction <- prediction + (param_value * var_values)
    }
  }
  # store prediction
  pred_6MWD$Model[i-4] <- colnames(model_params)[i]
  pred_6MWD$Prediction[i-4] <- mean(prediction, na.rm = TRUE)
}
