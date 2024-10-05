# Data Cleaning -----------------------------------------------------------

babies <- read.table("babies.txt", header = TRUE, sep="")

# Initial histograms prior to cleaning
hist(babies$bwt, 
     main = "bwt_description", 
     xlab = "birth weight (oz)")
hist(babies$gestation, 
     main = "gestation_description", 
     xlab = "length of gestation (days)")
hist(babies$parity, 
     breaks = 2, 
     main = "parity_description", 
     xlab = "first pregnancy indicator")
hist(babies$age, 
     main = "age_description", 
     xlab = "mother's age (yrs)")
hist(babies$height, 
     main = "height_description", 
     xlab = "mother's height (in)")
hist(babies$weight, 
     main = "weight_description", 
     xlab = "mother's weight (lbs)")
hist(babies$smoke, 
     main = "smoke_description", 
     xlab = "whether the mother smokes")

# Removing n/a values, outliers
cleaned_df <- subset(subset(babies, parity <= 1), smoke <= 1)
cleaned_df <- subset(cleaned_df, weight <= 500)
cleaned_df <- subset(cleaned_df, gestation <= 375)
cleaned_df <- subset(cleaned_df, height <= 85)
cleaned_df <- subset(cleaned_df, age <= 60)

# Histograms after cleaning
hist(cleaned_df$bwt, 
     main = "bwt_description v2.", 
     xlab = "birth weight (oz)")
hist(cleaned_df$gestation, 
     main = "gestation_description v2.", 
     xlab = "length of gestation (days)")
hist(cleaned_df$parity, 
     breaks = 2, 
     main = "parity_description v2.", 
     xlab = "first pregnancy indicator")
hist(cleaned_df$age, 
     main = "age_description v2.", 
     xlab = "mother's age (yrs)")
hist(cleaned_df$height, 
     main = "height_description v2.", 
     xlab = "mother's height (in)")
hist(cleaned_df$weight, 
     main = "weight_description v2.", 
     xlab = "mother's weight (lbs)")
hist(cleaned_df$smoke, 
     breaks = 2, 
     main = "smoke_description v2.", 
     xlab = "whether the mother smokes")


# Numerical Summaries -----------------------------------------------------

smoker_df <- babies[babies$smoke == 1,]
nonsmoker_df <- babies[babies$smoke == 0,]

# Minimum and maximum
smoker_min_bwt <- min(smoker_df$bwt) # 58L oz
smoker_max_bwt <- max(smoker_df$bwt) # 163 oz

nonsmoker_min_bwt <- min(nonsmoker_df$bwt) # 55 oz
nonsmoker_max_bwt <- max(nonsmoker_df$bwt) # 176 oz

# Mean
smoker_mean_bwt <- mean(smoker_df$bwt) # 114.1095 oz
nonsmoker_mean_bwt <- mean(nonsmoker_df$bwt) # 123.0472 oz

# Median
smoker_median_bwt <- median(smoker_df$bwt) #115 oz
nonsmoker_median_bwt <- median(nonsmoker_df$bwt) #123 oz

# Quartiles
smoker_quartiles <- quantile(smoker_df$bwt, probs = c(0.25,0.5,0.75))
nonsmoker_quartiles <- quantile(nonsmoker_df$bwt, probs = c(0.25,0.5,0.75))

names(smoker_quartiles) <- NULL
names(nonsmoker_quartiles) <- NULL

smoker_q1_bwt <- smoker_quartiles[1] # 102 oz
smoker_q2_bwt <- smoker_quartiles[2] # 115 oz
smoker_q3_bwt <- smoker_quartiles[3] # 126 oz

nonsmoker_q1_bwt <- nonsmoker_quartiles[1] # 113 oz
nonsmoker_q2_bwt <- nonsmoker_quartiles[2] #123 oz
nonsmoker_q3_bwt <- nonsmoker_quartiles[3] #134 oz

# Standard Deviation
smoker_std_bwt <- sd(smoker_df$bwt) # 18.0989 oz
nonsmoker_std_bwt <- sd(nonsmoker_df$bwt) # 17.3987 oz


# Graphical Summaries -----------------------------------------------------

# Frequency Histograms of Smoker and Non Smokers
hist(smoker_df$bwt,
     main = "Women who smoked",
     xlab = "birth weight (oz)",
     ylim = c(0,150))
hist(nonsmoker_df$bwt,
     main = "Women who did not smoke",
     xlab = "birth weight (oz)",
     ylim = c(0,200))

# Layered Histograms with Density
p1 <- hist(smoker_df$bwt, freq = F)
p2 <- hist(nonsmoker_df$bwt, freq = F)

plot( p1, 
      col=rgb(0,0,1,1/4),
      main = "Birth Weights",
      xlim = c(50,190),
      freq = F,
      ylim = c(0,0.026),
      xlab = "birth weight (oz)")

plot( p2, 
      col=rgb(1,0,0,1/4),  
      xlim = c(50,190),
      freq = F,
      add = T)

legend(150, 0.025, 
       c("smokers", "nonsmokers"), 
       lwd=4, 
       col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))

# Layered Histograms with Frequency
plot( p1, 
      col=rgb(0,0,1,1/4),
      main = "Birth Weights",
      xlim = c(50,190),
      ylim = c(0,200),
      xlab = "birth weight (oz)")

plot( p2, 
      col=rgb(1,0,0,1/4),  
      xlim = c(50,190),
      add = T)

legend(150, 190, 
       c("smokers", "nonsmokers"), 
       lwd=4, 
       col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))


# Low Birth Weight Babies -------------------------------------------------

low_weight_babies_df <- cleaned_df[cleaned_df$bwt < 100,]

# Proportions for low birth weight babies
low_bwt_nonsmoker <- dim(low_weight_babies_df[
  low_weight_babies_df$smoke == 0,])[1] / dim(low_weight_babies_df)[1] # 35.2941 %
print(low_bwt_nonsmoker)
low_bwt_smoker <- dim(low_weight_babies_df[
  low_weight_babies_df$smoke == 1,])[1] / dim(low_weight_babies_df)[1] # 64.7059 %
print(low_bwt_smoker)
results_df <- data.frame(threshold = integer(), 
                         low_bwt_nonsmoker = numeric(), 
                         low_bwt_smoker = numeric())

# Changes in proportions of birth weight
for (threshold in 55:176) {
  low_weight_df <- cleaned_df[cleaned_df$bwt < threshold, ]
  
    if (dim(low_weight_babies_df)[1] > 0) {
    nonsmoker <- dim(low_weight_df[
      low_weight_df$smoke == 0, ])[1] / dim(low_weight_df)[1]
    smoker <- dim(low_weight_df[
      low_weight_df$smoke == 1, ])[1] / dim(low_weight_df)[1]
  }
  
  results_df <- rbind(results_df, data.frame(threshold = threshold,
                                             low_bwt_nonsmoker = nonsmoker,
                                             low_bwt_smoker = smoker))
  results_df[, 2:3] <- lapply(results_df[, 2:3], function(x) round(x, 4))
}

plot(results_df$threshold, results_df$low_bwt_nonsmoker, type = "l", 
     col = "red", lwd = 2, 
     xlab = "Threshold (grams)", 
     ylab = "Proportion of Low Birth Weight Babies", 
     main = "Low Birth Weight Babies by Smoking Status",
     xlim = c(55,176),
     ylim = c(0,1))

lines(results_df$threshold, results_df$low_bwt_smoker, col = "blue", lwd = 2)

legend("topright", legend = c("Smoker", "Non-smoker"), 
       col = c("blue", "red"), lwd = 2)
