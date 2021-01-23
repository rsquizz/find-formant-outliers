# Outlier detection ####
# Modified for ABW 12/8/2020

library(dplyr)
library(readr)
library(joeyr)

rm(list=ls(all=T))

# Load data 
data <- read.table(file="YAm-noVL-5measpts.txt", fileEncoding = "UTF-16", sep = "\t", header = T)
# Create unique identifiers for each token (not a literal uuid)
data$uuid <- paste(data$Filename, data$Begin.Time..s., sep="")
# Change decimal point in Begin time to underscore (two periods in a filename 
# causes errors in Praat) 
data$uuid <- sub("\\.","_",data$uuid)

# Remove observations marked as "discard," "exclude," or "omit" in notes column
exclude <- grep("exclude", data$Notes)
omit <- grep("omit", data$Notes)
data <- data[-c(exclude,omit),]

# For Mahalanobis's Distance (MD), there must be at least 6 tokens per vowel
# per speaker if all 5 measurement points per formant go into the MD equation. 
# For Median Absolute Deviations (MAD), if there aren't least three tokens per 
# vowel per speaker, all tokens in that speaker & vowel will get flagged as 
# outliers. In practice, only 3 tokens flags a very high number of outliers 
# based on the formant values in our study, so the same cutoff is used for MAD 
# and MD.

# Tally number of vowels analyzed per speaker
speaker.vowel <- data %>% 
  count(Speaker, Vowel)
# Add ID for speaker and vowel combinations
speaker.vowel$Speaker.Vowel <- paste0(speaker.vowel$Speaker, speaker.vowel$Vowel)
six.or.more <- subset(speaker.vowel, speaker.vowel$n > 5)
three.or.more <- subset(speaker.vowel, speaker.vowel$n > 2)

# Remove word classes with fewer than six observations ####
data$Speaker.Vowel <- paste0(data$Speaker, data$Vowel)
data <- subset(data, Speaker.Vowel %in% (six.or.more)$Speaker.Vowel)
 
# Separate out groups with warnings
# warning.data <- subset(data, Speaker.Vowel %in% c("YZ40NF2Eʊl", "YU51NM2Eʊl", "YU51NM2Eo"))
# data <- subset(data, Speaker.Vowel %ni% c("YZ40NF2Eʊl", "YU51NM2Eʊl", "YU51NM2Eo"))

## Mahalanobis's Distance for each formant using all five measurement points ####
# F1
md.outliers <- data %>%
  group_by(Speaker, Vowel) %>%
  mutate(tidy_mahalanobis(F1.20., F1.35., F1.50., F1.65., F1.80.))

# F2
md.outliers.f2 <- data %>%
  group_by(Speaker, Vowel) %>%
  mutate(tidy_mahalanobis(F2.20., F2.35., F2.50., F2.65., F2.80.))

# F3
md.outliers.f3 <- data %>%
  group_by(Speaker, Vowel) %>%
  mutate(tidy_mahalanobis(F3.20., F3.35., F3.50., F3.65., F3.80.))

# Identify outliers based on critical values for a chi-square distribution with 
# 5 degrees of freedom. df in MD is equal to the number of dimensions 
# (measurement points, in this case). Mahalanobis's distance greater than the
# critical value means that the observation is  unlikely to have come from the
# distribution to which it is compared (its own distribution in this case).
# 
# Commonly used critical values:
# Conservative (identifies more outliers): p < .025 = 12.833
# Less conservative (identifies fewer outliers): p < .001 = 20.52
# A chi-sq table: http://uregina.ca/~gingrich/appchi.pdf

# F1
md.outliers$Is.outlier.001 <- ifelse(md.outliers$`tidy_mahalanobis(F1.20., F1.35., F1.50., F1.65., F1.80.)` > 20.52, T, F)
md.outliers$Is.outlier.025 <- ifelse(md.outliers$`tidy_mahalanobis(F1.20., F1.35., F1.50., F1.65., F1.80.)` > 12.833, T, F)
less.conservative <- subset(md.outliers, Is.outlier.001 == T)
conservative <- subset(md.outliers, Is.outlier.025 == T)
# F2
md.outliers.f2$Is.outlier.001 <- ifelse(md.outliers.f2$`tidy_mahalanobis(F2.20., F2.35., F2.50., F2.65., F2.80.)` > 20.52, T, F)
md.outliers.f2$Is.outlier.025 <- ifelse(md.outliers.f2$`tidy_mahalanobis(F2.20., F2.35., F2.50., F2.65., F2.80.)` > 12.833, T, F)
less.conservative.f2 <- subset(md.outliers.f2, Is.outlier.001 == T)
conservative.f2 <- subset(md.outliers.f2, Is.outlier.025 == T)
# F3
md.outliers.f3$Is.outlier.001 <- ifelse(md.outliers.f3$`tidy_mahalanobis(F3.20., F3.35., F3.50., F3.65., F3.80.)` > 20.52, T, F)
md.outliers.f3$Is.outlier.025 <- ifelse(md.outliers.f3$`tidy_mahalanobis(F3.20., F3.35., F3.50., F3.65., F3.80.)` > 12.833, T, F)
less.conservative.f3 <- subset(md.outliers.f3, Is.outlier.001 == T)
conservative.f3 <- subset(md.outliers.f3, Is.outlier.025 == T)

# Avoid duplicating tokens that are outliers in both F1 and F2
colnames(less.conservative)[48] <- "md"
colnames(less.conservative.f2)[48] <- "md"
tmp <- rbind(less.conservative, less.conservative.f2)
less.conservative.f1.f2 <- tmp %>%
  distinct(uuid, .keep_all = T)
distinct(tmp, uuid, .keep_all = F)

colnames(conservative)[48] <- "md"
colnames(conservative.f2)[48] <- "md"
tmp <- rbind(conservative, conservative.f2)
conservative.f1.f2 <- tmp %>%
  distinct(uuid, .keep_all = T)

# Write to file
write_excel_csv(less.conservative.f1.f2, file="YAm-noVL-5measpts-outliers.csv")

conservative.not.less <- subset(conservative.f1.f2, conservative.f1.f2$uuid %ni% less.conservative.f1.f2$uuid)

# Avoid duplicating tokens that are outliers in F1, F2, and F3
colnames(less.conservative.f3)[48] <- "md"
tmp <- rbind(less.conservative.f1.f2, less.conservative.f3)
less.conservative.f1.f2.f3 <- tmp %>%
  distinct(uuid, .keep_all = T)
distinct(tmp, uuid, .keep_all = F)

