# HOMEWORK 
#Pietro Ruzzante
#Matricola: 2124590

# LABORATORY 2
#------------------------------------
#EXERCISE 1
setwd("~/Desktop/esami/Medical Big Data/homework")
load("data.Rdata")
ncol <- ncol(data)
nrow <- nrow(data)
print(paste("Number of columns:", ncol))
print(paste("Number of rows:", nrow))

print(summary(data))

#EXERCISE 2

set.seed(0)

# Indici dei soggetti con outcome 0 e 1
ind_0 <- which(data$outcome == 0)
ind_1 <- which(data$outcome == 1)

# Frazione per il training set
p <- 0.80

ind_0_training <- sample(ind_0, round(p * length(ind_0)))
ind_1_training <- sample(ind_1, round(p * length(ind_1)))

ind_training <- c(ind_0_training, ind_1_training)

ind_all <- 1:nrow(data)
ind_test <- ind_all[-ind_training]

train_data <- data[ind_training,]
test_data <- data[ind_test, ]

print(dim(train_data))
print(dim(test_data))
print(table(train_data$outcome))
print(table(test_data$outcome))

#EXERCISE 3

# Rule of thumb for classification: #sample_in_the_minority_class/coefficients >= 10

n_coeff <- 0

categorical_vars <- c("gender", "smoking", "chest_pain", 
                      "phlegm", "short_breath_walking", "wheezing", "wake_breath", 
                      "asthma_hx", "outcome")

for (var in categorical_vars) {
  train_data[[var]] <- as.factor(train_data[[var]])
  test_data[[var]] <- as.factor(test_data[[var]])
}


for (colname in colnames(train_data)) {
  if (colname == "outcome") next
  var <- train_data[[colname]]
  if (is.numeric(var)) {
    n_coeff <- n_coeff + 1
    print(paste("Variabile:", colname))
    print(paste("Numero livelli:", 1))
  } else if (is.factor(var)) {
    print(paste("Variabile:", colname))
    print(paste("Numero livelli:", nlevels(var)))
    n_levels <- nlevels(var)
    n_coeff <- n_coeff + (n_levels - 1)
  }
}

print(paste("Numero stimato di coefficienti:", n_coeff))

n_class_1 = length(which(train_data$outcome == 1))

print(n_class_1)

print(paste("Coefficients rule of thumb is respected: ", n_class_1 / n_coeff >= 10))

print("---------------------")
#EXERCISE 4

# Variabili numeriche
X <- sapply(train_data, is.factor)
ind_numeric <- which(!X)
var_names <- colnames(train_data[ind_numeric])

png("boxplots_numeric_variables.png", width = 6000, height = 4000, res = 400)

layout(matrix(1:length(ind_numeric), nrow = 2, byrow = TRUE), heights = rep(1, 2), widths = rep(1, 4))

# Margini ridotti tra i plot
par(oma = c(2, 2, 4, 2), mar = c(4, 4, 3, 1)) 


for (var in var_names) {
  boxplot(train_data[[var]],
          main = var,
          ylab = var,
          col = "gray90",
          border = "black",
          cex.main = 1.5,
          cex.lab = 1.3)
  vals <- train_data[[var]]
  lims <- mean(vals, na.rm = TRUE) + c(-4, 4) * sd(vals, na.rm = TRUE)
  idx_outliers <- which(vals < lims[1] | vals > lims[2])
  train_data[idx_outliers, var] <- NA
  cat(paste("Replaced", length(idx_outliers), "extreme values in", var, "\n"))
}


# Titolo generale
mtext("Boxplot of numeric varibles", outer = TRUE, cex = 2, line = 1)

# Chiudi il dispositivo grafico
dev.off()

print("---------------------")
#EXERCISE 5

png("correlation_plot.png", width = 6000, height = 4000, res = 600)

correlation_matrix <- cor(na.omit(train_data[ind_numeric]))
library("corrplot")
par(mfrow = c(1, 1))
corrplot(correlation_matrix, method ="circle", col = COL2('RdYlBu', 20), tl.col='Black', cl.ratio = 0.2, addCoef.col = "black")

#remove fev1
train_data <- subset(train_data, select = -fev1)
test_data <- subset(test_data, select = -fev1)

dev.off()

print("---------------------")

#EXERCISE 6
for (i in 1:length(colnames(train_data))) {
  ind_nan <- which(is.na(train_data[, i]))
  print(paste(colnames(train_data)[i], ", percentage of nan:", round(length(ind_nan)*100/nrow(train_data),2), "%"))
}
data_imputation_array <- vector(mode = "list", length = length(colnames(train_data)))
mean_var <- c("age", "bmi", "fvc", "pf", "fev1_fvc_ratio")

for (i in 1:length(colnames(train_data))) {
  col_name <- colnames(train_data)[i]
  
  if (col_name %in% mean_var) {
    data_imputation_array[i] <- mean(train_data[, i], na.rm = TRUE)
  } else {
    data_imputation_array[i] <- as.factor(names(sort(table(train_data[, i]), decreasing = TRUE))[1])
  }
}


for (i in 1:length(colnames(train_data))) {
  if (is.numeric(train_data[, i])) {
    print(paste("mean value for ", var_names[i], data_imputation_array[i]))
  } else {
    print(paste("mode value for ", var_names[i], data_imputation_array[i]))
  }
}

for (i in 1:length(colnames(train_data))) {
  for(j in 1:nrow(train_data)){
    if(is.na(train_data[j,i])){
      train_data[j,i] = data_imputation_array[i]
    }
  }
}

for (i in 1:ncol(train_data)) {
  for(j in 1:nrow(train_data)){
    if(is.na(train_data[j,i])){
      train_data[j,i] = data_imputation_array[i]
    }
  }
}

for (i in 1:ncol(test_data)) {
  for(j in 1:nrow(test_data)){
    if(is.na(test_data[j,i])){
      test_data[j,i] = data_imputation_array[i]
    }
  }
}
print("-----------------")
print("Check nan after data imputation in training set")
for (i in 1:length(colnames(train_data))) {
  ind_nan <- which(is.na(train_data[, i]))
  print(paste(colnames(train_data)[i], ", number of nan:", length(ind_nan)))
}
print("-----------------")
print("Check nan after data imputation in test set")
for (i in 1:length(colnames(test_data))) {
  ind_nan <- which(is.na(test_data[, i]))
  print(paste(colnames(test_data)[i], ", number of nan:", length(ind_nan)))
}
#EXERCISE 7

# numeric variables
is_numeric <- sapply(train_data, is.numeric)
numeric_vars <- names(train_data)[is_numeric]

# min and max values from training_set
scaling_params <- list()

for (var in numeric_vars) {
  min_val <- min(train_data[[var]])
  max_val <- max(train_data[[var]])
  scaling_params[[var]] <- list(min = min_val, max = max_val)
  
  # min-max scaling to training_set
  train_data[[var]] <- (train_data[[var]] - min_val) / (max_val - min_val)
}

# same scaling to test_set
for (var in numeric_vars) {
  min_val <- scaling_params[[var]]$min
  max_val <- scaling_params[[var]]$max
  test_data[[var]] <- (test_data[[var]] - min_val) / (max_val - min_val)
}

# check that all values are in the range [0, 1]
cat("Normalized value ranges in training_set:\n")
for (var in numeric_vars) {
  range_vals <- range(train_data[[var]])
  cat(paste0(var, ": [", round(range_vals[1], 3), ", ", round(range_vals[2], 3), "]\n"))
}

cat("\nNormalized value ranges in test_set:\n")
for (var in numeric_vars) {
  range_vals <- range(test_data[[var]])
  cat(paste0(var, ": [", round(range_vals[1], 3), ", ", round(range_vals[2], 3), "]\n"))
}

if (any(is.na(train_data))){
  print("Nan values is training set")
}

if (any(is.na(test_data))){
  print("Nan values is test set")
}

# LABORATORY 3
#------------------------------------

# #EXERCISE 1

test_label = test_data["outcome"]
#test_label$outcome <- NULL

####
full_model <- glm(outcome ~ ., data = train_data, family = binomial)
print(summary(full_model))

### EXERCISE 2

predicted_full <- predict(full_model, newdata=test_data, type ="response")

library(pROC)

png("roc_test_set.png", width = 2000, height = 2000, res = 300)

roc_full <- roc(response=test_data$outcome, predictor=predicted_full)
print(paste("AUC: ", roc_full$auc))
plot(roc_full, col = "blue", lwd = 2, legacy.axes = TRUE, main = "Full Model", cex.main = 1)

dev.off()

### EXERCISE 4

null_model <- glm(outcome ~ 1, data = train_data, family = "binomial")
model_range <- list(lower = formula(null_model), upper = formula(full_model))

# backward
reduced_model_bwd <- step(object = full_model, scope = model_range, direction = "backward", trace=TRUE)

print(summary(reduced_model_bwd))

predicted_reduced_bwd <- predict(reduced_model_bwd, newdata=test_data, type ="response")

roc_reduced_bwd <- roc(response=test_data$outcome, predictor=predicted_reduced_bwd)
print(paste("AUC: ", roc_reduced_bwd$auc))

# forward
reduced_model_fwd <- step(object = null_model, scope = model_range, direction = "forward", trace=TRUE)

print(summary(reduced_model_fwd))

predicted_reduced_fwd <- predict(reduced_model_fwd, newdata=test_data, type ="response")

roc_reduced_fwd <- roc(response=test_data$outcome, predictor=predicted_reduced_fwd)
print(paste("AUC: ", roc_reduced_fwd$auc))
#plot(roc_reduced_fwd, col = "blue", lwd = 2, legacy.axes = TRUE)


# stepwise backward
reduced_model_sbwd <- step(object = full_model, scope = model_range, direction = "both", trace=TRUE)

print(summary(reduced_model_sbwd))

predicted_reduced_sbwd <- predict(reduced_model_sbwd, newdata=test_data, type ="response")

roc_reduced_sbwd <- roc(response=test_data$outcome, predictor=predicted_reduced_sbwd)
print(paste("AUC: ", roc_reduced_sbwd$auc))
#plot(roc_reduced_sbwd, col = "blue", lwd = 2, legacy.axes = TRUE)

# stepwise forward
reduced_model_sfwd <- step(object = null_model, scope = model_range, direction = "both", trace=TRUE)

print(summary(reduced_model_sfwd))

predicted_reduced_sfwd <- predict(reduced_model_sfwd, newdata=test_data, type ="response")

roc_reduced_sfwd <- roc(response=test_data$outcome, predictor=predicted_reduced_sfwd)
print(paste("AUC: ", roc_reduced_sfwd$auc))
#plot(roc_reduced_sfwd, col = "blue", lwd = 2, legacy.axes = TRUE)

png("roc_comparison_panels.png", width = 4000, height = 4000, res = 300)

# Imposta 1 riga con 5 colonne
par(mfrow = c(2, 2))  # 1 row, 5 columns

# Plot individuali
#plot(roc_full, col = "blue", lwd = 2, legacy.axes = TRUE, main = "Full Model")
plot(roc_reduced_bwd, col = "red", lwd = 4, legacy.axes = TRUE, main = "Backward", cex.main = 2)
plot(roc_reduced_fwd, col = "green", lwd = 4, legacy.axes = TRUE, main = "Forward", cex.main = 2)
plot(roc_reduced_sbwd, col = "purple", lwd = 4, legacy.axes = TRUE, main = "Stepwise Backward", cex.main = 2)
plot(roc_reduced_sfwd, col = "orange", lwd = 4, legacy.axes = TRUE, main = "Stepwise Forward", cex.main = 2)

dev.off()


# LABORATORY 4
#------------------------------------

#### RESAMPLING
test_label = test_data["outcome"]

selected_var <- list()

for(i in 1:50){
  set.seed(i)
  
  ind_0 <- which(train_data$outcome == 0)
  ind_1 <- which(train_data$outcome == 1)
  
  p <- 0.80
  
  ind_0_boot <- sample(ind_0, size = round(p*length(ind_0)), replace = TRUE)
  ind_1_boot <- sample(ind_1, size = round(p*length(ind_1)), replace = TRUE)
  
  ind_training <- c(ind_0_boot, ind_1_boot)
  
  internal_train_data <- train_data[ind_training, ]
  
  full_model <- glm(outcome ~ ., data = internal_train_data, family = binomial)
  #predicted_full <- predict(full_model, newdata=internal_test_data, type ="response")
  null_model <- glm(outcome ~ 1, data = internal_train_data, family = "binomial")
  model_range <- list(lower = formula(null_model), upper = formula(full_model))
  reduced_model_bwd <- step(object = full_model, scope = model_range, direction = "backward", trace=TRUE)
  print(names(coef(reduced_model_bwd)))
  
  selected_var[[i]] <- attr(terms(reduced_model_bwd), "term.labels")
  
  
}

all_selected <- unlist(selected_var)
var_count <- table(all_selected)
var_count_pct <- (var_count / length(selected_var)) * 100
final_selected_var <- names(var_count_pct[var_count_pct > 60])

png("histrogram.png", width = 3000, height = 3000, res = 500)
par(mar = c(10, 5, 4, 2))
barplot(sort(var_count_pct, decreasing = TRUE),
        main = "Histogram: variable selection %",
        ylab = "Percentage (%)",
        col = "skyblue",
        las = 2)
dev.off()

formula_final <- as.formula(paste("outcome ~", paste(final_selected_var, collapse = " + ")))


final_model <- glm(formula_final, data = train_data, family = binomial)
print(summary(final_model))

predicted_final <- predict(final_model, newdata = test_data, type = "response")

library(pROC)

png("roc_test_set.png", width = 2000, height = 2000, res = 300)

roc_final <- roc(response=test_data$outcome, predictor=predicted_final)
print(paste("AUC: ", roc_final$auc))
plot(roc_final, col = "blue", lwd = 2, legacy.axes = TRUE, main = "Final Model", cex.main = 1)

dev.off()















