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

smoker_df = babies[babies$smoke == 1,]
nonsmoker_df = babies[babies$smoke == 0,]

# Minimum and maximum
smoker_min_bwt <- min(smoker_df$bwt)
smoker_max_bwt <- max(smoker_df$bwt)

nonsmoker_min_bwt <- min(nonsmoker_df$bwt)
nonsmoker_max_bwt <- max(nonsmoker_df$bwt)

# Mean
smoker_mean_bwt <- mean(smoker_df$bwt)
nonsmoker_mean_bwt <- mean(nonsmoker_df$bwt)

# Median
smoker_median_bwt <- median(smoker_df$bwt)
nonsmoker_median_bwt <- median(nonsmoker_df$bwt)

# Quartiles
smoker_quartiles <- quantile(smoker_df$bwt, probs = c(0.25,0.5,0.75))
nonsmoker_quartiles <- quantile(nonsmoker_df$bwt, probs = c(0.25,0.5,0.75))

names(smoker_quartiles) <- NULL
names(nonsmoker_quartiles) <- NULL

smoker_q1_bwt <- smoker_quartiles[1]
smoker_q2_bwt <- smoker_quartiles[2]
smoker_q3_bwt <- smoker_quartiles[3]

nonsmoker_q1_bwt <- nonsmoker_quartiles[1]
nonsmoker_q2_bwt <- nonsmoker_quartiles[2]
nonsmoker_q3_bwt <- nonsmoker_quartiles[3]

# Standard Deviation
smoker_std_bwt <- sd(smoker_df$bwt)
nonsmoker_std_bwt <- sd(nonsmoker_df$bwt)


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
