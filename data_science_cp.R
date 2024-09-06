sam <- read.csv("C:\\Users\\Admin\\Documents\\Data science course project\\LungDataSet.csv")
View(sam)

count_low <- sum(sam$Level == "Low")
print(count_low)
count_medium <- sum(sam$Level == "Medium")
print(count_medium)
count_high <- sum(sam$Level == "High")
print(count_high)

cal_sum_par_low <- function(sam) {
  sum_low <- numeric(23)  
  
  for (i in 1:23) {
    temp <- sum(sam[, i + 1][sam$Level == "Low"])
    sum_low[i] <- temp
    
  }
  
  return(sum_low) 
}
sum_low <- cal_sum_par_low(sam)
print(sum_low)

cal_sum_par_med <- function(sam) {
  sum_med <- numeric(23)  
  
  for (i in 1:23) {
    temp <- sum(sam[, i + 1][sam$Level == "Medium"])
    sum_med[i] <- temp
    
  }
  
  return(sum_med)  
}
sum_med <- cal_sum_par_med(sam)
print(sum_med)

cal_sum_par_high <- function(sam) {
  sum_high <- numeric(23)  
  
  for (i in 1:23) {
    temp <- sum(sam[, i + 1][sam$Level == "High"])
    sum_high[i] <- temp
    
  }
  
  return(sum_high)  
}
sum_high <- cal_sum_par_high(sam)
print(sum_high)

cal_mean_par_low <- function(sum_low,count_low) {
  mean_low <- numeric(23)  
  
  for (i in 1:23) {
    temp <- sum_low[i]/count_low
    mean_low[i] <- temp
    
  }
  
  return(mean_low)  
}
mean_low <- cal_mean_par_low(sum_low, count_low)
print(mean_low)

cal_mean_par_med <- function(sum_med,count_medium) {
  mean_med <- numeric(23)  
  
  for (i in 1:23) {
    temp <- sum_med[i]/count_medium
    mean_med[i] <- temp
    
  }
  
  return(mean_med)  
}
mean_med <- cal_mean_par_med(sum_med, count_medium)
print(mean_med)

cal_mean_par_high <- function(sum_high,count_high) {
  mean_high <- numeric(23)  
  
  for (i in 1:23) {
    temp <- sum_high[i]/count_high
    mean_high[i] <- temp
    
  }
  
  return(mean_high)  
}
mean_high <- cal_mean_par_high(sum_high, count_high)
print(mean_high)

cal_sd_par_low <- function(sam,count_low,mean_low) {
  sd_low <- numeric(23)  
  
  for (i in 1:23) {
    x <- sam[ ,i + 1][sam$Level == "Low"]
    n <- count_low
    mean_x <- mean_low[i]
    abs_diff <- abs(x - mean_x)
    sd_value <- sum(abs_diff) / n
    sd_low[i] <- sd_value
    
  }
  
  return(sd_low)  
}
sd_low <- cal_sd_par_low(sam,count_low, mean_low)
print(sd_low)

cal_sd_par_med <- function(sam,count_medium,mean_med) {
  sd_med <- numeric(23)  
  
  for (i in 1:23) {
    x <- sam[ ,i + 1][sam$Level == "Medium"]
    n <- count_medium
    mean_x <- mean_med[i]
    abs_diff <- abs(x - mean_x)
    sd_value <- sum(abs_diff) / n
    sd_med[i] <- sd_value
    
  }
  
  return(sd_med)  
}
sd_med <- cal_sd_par_med(sam,count_medium, mean_med)
print(sd_med)

cal_sd_par_high <- function(sam,count_high,mean_high) {
  sd_high <- numeric(23)  
  
  for (i in 1:23) {
    x <- sam[ ,i + 1][sam$Level == "High"]
    n <- count_high
    mean_x <- mean_high[i]
    sum_sq_diff <- sum((x - mean_x)^2)
    sd_value <- sqrt(sum_sq_diff / (n - 1))
    sd_high[i] <- sd_value
    
  }
  
  return(sd_high)  
}
sd_high <- cal_sd_par_high(sam,count_high, mean_high)
print(sd_high)

cal_pdf_par_low <- function(sd_low,mean_low, sam) {
  pdf_low <- numeric(23)  
  likeh_low <- numeric(1000)
  for(j in 1:1000){
    for (i in 1:23) {
      sd_val <- sd_low[i]
      mean_val <- mean_low[i]
      x <- sam[ , i+1][j]
      pdf <- (1 / (sd_val * sqrt(2 * pi))) * exp(-0.5 * ((x - mean_val) / sd_val)^2)
      pdf_low[i] <- pdf
      
    }
    likeh_low[j] <- cal_likehood_par_low(pdf_low)
  }
  
  return(likeh_low)  
  
}

cal_pdf_par_med <- function(sd_med,mean_med, sam) {
  pdf_med <- numeric(23)  
  likeh_med <- numeric(1000)
  for(j in 1:1000){
    for (i in 1:23) {
      sd_val <- sd_med[i]
      mean_val <- mean_med[i]
      x <- sam[ , i+1][j]
      pdf <- (1 / (sd_val * sqrt(2 * pi))) * exp(-0.5 * ((x - mean_val) / sd_val)^2)
      pdf_med[i] <- pdf
      
    }
    likeh_med[j] <- cal_likehood_par_med(pdf_med)
  }
  
  return(likeh_med)  
  
}

cal_pdf_par_high <- function(sd_high,mean_high, sam) {
  pdf_high <- numeric(23)  
  likeh_high <- numeric(1000)
  for(j in 1:1000){
    for (i in 1:23) {
      sd_val <- sd_high[i]
      mean_val <- mean_high[i]
      x <- sam[ , i+1][j]
      pdf <- (1 / (sd_val * sqrt(2 * pi))) * exp(-0.5 * ((x - mean_val) / sd_val)^2)
      pdf_high[i] <- pdf
      
    }
    likeh_high[j] <- cal_likehood_par_high(pdf_high)
  }
  
  return(likeh_high)  
  
}

likehood_low <- cal_pdf_par_low(sd_low, mean_low, sam)
print(likehood_low)

likehood_med <- cal_pdf_par_med(sd_med,mean_med,sam)
print(likehood_med)

likehood_high <- cal_pdf_par_high(sd_high, mean_high, sam)
print(likehood_high)


cal_likehood_par_low <- function(pdf_low) {
  
  lik <- 1
  for (i in 1:23) {
    lik <- lik* pdf_low[i]
    
  }
  
  return(lik)  
}

cal_likehood_par_med <- function(pdf_med) {
  
  lik <- 1
  for (i in 1:23) {
    lik <- lik* pdf_med[i]
    
  }
  
  return(lik)  
}

cal_likehood_par_high <- function(pdf_high) {
  
  lik <- 1
  for (i in 1:23) {
    lik <- lik* pdf_high[i]
    
  }
  
  return(lik)  
}

Level <- character(1000)
i <- 1
cal_level <- function(likehood_low,likehood_med ,likehood_high){
  if(likehood_high > likehood_low && likehood_high > likehood_med)
  {
    Level[i] <- "High"
    i <- i+1
    
  }else if(likehood_med > likehood_low && likehood_med > likehood_high)
  {
    Level[i] <- "Medium"
    i <- i+1
    
  }else
  {
    Level[i] <- "Low"
    i <- i+1
  }
  
  return(Level)
  
}

for(i in 1:1000){
  Level <- cal_level(likehood_low[i], likehood_med[i], likehood_high[i])
  
}

print(Level)

true_labels <- c(sam$Level)  

print(true_labels)
print(class(true_labels))

conf_matrix <- table(true_labels, Level)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

sensitivity <- diag(conf_matrix) / rowSums(conf_matrix)

precision <- diag(conf_matrix) / colSums(conf_matrix)

specificity <- numeric(length(levels(Level)))
for (i in 1:length(levels(Level))) {
  specificity[i] <- sum(conf_matrix[-i, -i]) / sum(conf_matrix[-i, ])
}

print(paste("Accuracy:", accuracy))
print(paste("Sensitivity:", sensitivity))
print(paste("Precision:", precision))
print(paste("Specificity:", specificity))



library(ggplot2)


# Bar plot for model performance metrics
metrics_df <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision"),
  Value = c(accuracy, mean(sensitivity), mean(specificity), mean(precision))
)

ggplot(metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Model Performance Metrics", y = "Value") +
  scale_fill_manual(values = c("blue", "green", "red", "orange"))












par(mfrow = c(3, 4))  
for (i in 2:(ncol(sam) - 1)) {
  hist(sam[, i], main = paste("Histogram of", colnames(sam)[i]), xlab = "Values")
}

par(mfrow = c(1, 1))





