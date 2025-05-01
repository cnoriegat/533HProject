library(readxl)
library(car)
library (interactions)
setwd("~/Documents/UNC/PSYC 533H/533HProject")
dat <- read_excel("prog_data_qualtrics.xlsx")

#### Part 1: Data Cleaning ####

# basic cleaning
dat <- dat[-1, ] # removing labels row
as.numeric(dat$Finished) # making characters to numbers for filtering
dat <- dat[dat$Finished == 1, ] # removing unfinished surveys
dat <- dat[, 12:51] # removing unnecessary columns

# columns to numeric
cols_to_convert <- setdiff(names(dat), "Q4_3_TEXT") # exclude text entry
dat[cols_to_convert] <- sapply(dat[cols_to_convert], function(x) as.numeric(as.character(x)))

# creating a reverse scoring function
reverse <- function(x) { 6 - x }

# extraversion: 1R, 6, 11, 16, 21R, 26R
dat$Extraversion <- rowMeans(cbind(
  reverse(dat$Q12_1),   # 1R
  dat$Q12_6,            # 6
  dat$Q13_1,            # 11
  dat$Q13_6,            # 16
  reverse(dat$Q14_1),   # 21R
  reverse(dat$Q14_6)    # 26R
), na.rm = TRUE)

# agreeableness: 2, 7R, 12, 17R, 22, 27R
dat$Agreeableness <- rowMeans(cbind(
  dat$Q12_2,            # 2
  reverse(dat$Q12_7),   # 7R
  dat$Q13_2,            # 12
  reverse(dat$Q13_7),   # 17R
  dat$Q14_2,            # 22
  reverse(dat$Q14_7)    # 27R
), na.rm = TRUE)

# conscientiousness: 3R, 8R, 13, 18, 23, 28R
dat$Conscientiousness <- rowMeans(cbind(
  reverse(dat$Q12_3),   # 3R
  reverse(dat$Q12_8),   # 8R
  dat$Q13_3,            # 13
  dat$Q13_8,            # 18
  dat$Q14_3,            # 23
  reverse(dat$Q14_8)    # 28R
), na.rm = TRUE)

# negative Emotionality (Neuroticism): 4, 9, 14R, 19R, 24R, 29
dat$Neuroticism <- rowMeans(cbind(
  dat$Q12_4,            # 4
  dat$Q12_9,            # 9
  reverse(dat$Q13_4),   # 14R
  reverse(dat$Q13_9),   # 19R
  reverse(dat$Q14_4),   # 24R
  dat$Q14_9             # 29
), na.rm = TRUE)

# open-Mindedness: 5, 10R, 15, 20R, 25, 30R
dat$Openness <- rowMeans(cbind(
  dat$Q12_5,            # 5
  reverse(dat$Q12_10),  # 10R
  dat$Q13_5,            # 15
  reverse(dat$Q13_10),  # 20R
  dat$Q14_5,            # 25
  reverse(dat$Q14_10)   # 30R
), na.rm = TRUE)

# life satisfaction score
dat$LifeSatisfaction_Total <- rowSums(dat[, c("Life Satisfaction_1", "Life Satisfaction_2", 
                                              "Life Satisfaction_3", "Life Satisfaction_4", 
                                              "Life Satisfaction_5")], na.rm = TRUE)

# renaming positive and negative affect columns
names(dat)[9:10] <- c("pos_af", "neg_af")

# Effect Coding Gender using other as base group
dat$Gender <- factor(dat$Q4, levels = c(1, 2, 3), labels = c("Woman", "Man", "Other"))

# coding matrix
C <- rbind(
  c( 1,  0),   # Woman
  c( 0,  1),   # Man
  c(-1, -1)    # Other (reference group in effect coding)
)
contrasts(dat$Gender) <- C

#### Part 2: Correlations ####

cor(dat[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")],
    dat[, c("pos_af", "neg_af")],
    use = "pairwise.complete.obs")

cor(dat[, c("Extraversion", "Agreeableness", "Conscientiousness", "Neuroticism", "Openness")],
    dat$LifeSatisfaction_Total,
    use = "pairwise.complete.obs")

#### Part 2: Running the model ####

# Centering Big Five traits
dat$Extraversion_c <- dat$Extraversion - mean(dat$Extraversion, na.rm = TRUE)
dat$Agreeableness_c <- dat$Agreeableness - mean(dat$Agreeableness, na.rm = TRUE)
dat$Conscientiousness_c <- dat$Conscientiousness - mean(dat$Conscientiousness, na.rm = TRUE)
dat$Neuroticism_c <- dat$Neuroticism - mean(dat$Neuroticism, na.rm = TRUE)
dat$Openness_c <- dat$Openness - mean(dat$Openness, na.rm = TRUE)

# Centering affect measures
dat$pos_af_c <- scale(dat$pos_af, center = TRUE, scale = FALSE)
dat$neg_af_c <- scale(dat$neg_af, center = TRUE, scale = FALSE)

# Centering age
dat$age_c <- dat$Q8 - mean(dat$Q8, na.rm = TRUE)

#####
# Main effects of Big Five
model_traits_5 <- lm(LifeSatisfaction_Total ~ Extraversion_c + Agreeableness_c + Conscientiousness_c + Neuroticism_c + Openness_c, data = dat)
summary(model_traits_5)

#histogram w/ density estimates
hist(model_traits_5$residuals,col="lightblue",prob=T)
lines(density(model_traits_5$residuals),col="black",lwd=2)
curve(dnorm(x,mean=mean(model_traits_5$residuals),sd=sqrt(var(model_traits_5$residuals))),lwd=2,add=T,col="red")

#qq plot
qqnorm(model_traits_5$residuals)
qqline(model_traits_5$residuals, lwd=2)

#plot residuals by predicted values
plot(model_traits_5$fitted.values, y=model_traits_5$residuals)
abline(h=0,lty=2,col="blue")

#add smoother to see potential trend
scatter.smooth(model_traits_5$fitted.values, y=model_traits_5$residuals)
abline(h=0,lty=2,col="blue")

#can do for each predictor in turn
scatter.smooth(dat$Openness_c, y=model_traits_5$residuals)
abline(h=0,lty=2,col="blue")
scatter.smooth(dat$Extraversion_c, y=model_traits_5$residuals)
abline(h=0,lty=2,col="blue")
scatter.smooth(dat$Conscientiousness_c, y=model_traits_5$residuals)
abline(h=0,lty=2,col="blue")
scatter.smooth(dat$Neuroticism_c, y=model_traits_5$residuals)
abline(h=0,lty=2,col="blue")
scatter.smooth(dat$Agreeableness_c, y=model_traits_5$residuals)
abline(h=0,lty=2,col="blue")

## partial regression plots
avPlots(model_traits_5)

#####

# Main effects of Big Five Separately
model_traits_ag <- lm(LifeSatisfaction_Total ~ Agreeableness_c, data = dat)
summary(model_traits_ag)

model_traits_con <- lm(LifeSatisfaction_Total ~ Conscientiousness_c, data = dat)
summary(model_traits_con)

model_traits_ex <- lm(LifeSatisfaction_Total ~ Extraversion_c, data = dat)
summary(model_traits_ex)

model_traits_neu <- lm(LifeSatisfaction_Total ~ Neuroticism_c, data = dat)
summary(model_traits_neu)

model_traits_op <- lm(LifeSatisfaction_Total ~ Openness_c, data = dat)
summary(model_traits_op)

# Gender
model_gender <- lm(LifeSatisfaction_Total ~ Gender, data = dat)
summary(model_gender)

# Gender and age
model_gender_age <- lm(LifeSatisfaction_Total ~ Gender + age_c, data = dat)
summary(model_gender_age)

# Age
model_age <- lm(LifeSatisfaction_Total ~ age_c, data = dat)
summary(model_age)

# Main effects of Big Five
model_traits <- lm(LifeSatisfaction_Total ~ Extraversion_c + Agreeableness_c + Conscientiousness_c + Neuroticism_c + Openness_c + Gender + age_c, data = dat)
summary(model_traits)

# Histogram of residuals
hist(model_traits$residuals, col = "lightblue", prob = TRUE)
lines(density(model_traits$residuals), col = "black", lwd = 2)
curve(dnorm(x, mean = mean(model_traits$residuals), sd = sqrt(var(model_traits$residuals))), 
      lwd = 2, add = TRUE, col = "red")

# QQ plot
qqnorm(model_traits$residuals)
qqline(model_traits$residuals, lwd = 2)

# Residuals vs. predicted
plot(model_traits$fitted.values, y = model_traits$residuals, 
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2, col = "blue")
scatter.smooth(model_traits$fitted.values, y = model_traits$residuals)
abline(h = 0, lty = 2, col = "blue")

# Residuals vs. individual predictors
scatter.smooth(dat$Extraversion_c, y = model_traits$residuals); abline(h = 0, lty = 2, col = "blue")
scatter.smooth(dat$Agreeableness_c, y = model_traits$residuals); abline(h = 0, lty = 2, col = "blue")
scatter.smooth(dat$Conscientiousness_c, y = model_traits$residuals); abline(h = 0, lty = 2, col = "blue")
scatter.smooth(dat$Neuroticism_c, y = model_traits$residuals); abline(h = 0, lty = 2, col = "blue")
scatter.smooth(dat$Openness_c, y = model_traits$residuals); abline(h = 0, lty = 2, col = "blue")

# Partial regression plots
avPlots(model_traits)


# Gender women 1, men 2, other 3
model_gender <- lm(LifeSatisfaction_Total ~ Gender, data = dat)
summary(model_gender)

# H4: The relationship between traits and life satisfaction by gender.
model_gender_extraversion <- lm(Extraversion ~ Gender, data = dat)
summary(model_gender_extraversion)

model_gender_agreeableness <- lm(Agreeableness ~ Gender, data = dat)
summary(model_gender_agreeableness)

model_gender_conscientiousness <- lm(Conscientiousness ~ Gender, data = dat)
summary(model_gender_conscientiousness)

model_gender_neuroticism <- lm(Neuroticism ~ Gender, data = dat)
summary(model_gender_neuroticism)

model_gender_openness <- lm(Openness ~ Gender, data = dat)
summary(model_gender_openness)

# Gender × Trait Interactions Predicting Life Satisfaction (H5)
# Agreeableness × Gender
model_agree_gender <- lm(LifeSatisfaction_Total ~ Agreeableness_c * Gender, data = dat)
summary(model_agree_gender)

interact_plot(model_agree_gender, pred = Agreeableness_c, modx = Gender)
sim_slopes(model_agree_gender, pred = Agreeableness_c, modx = Gender)

plot(model_agree_gender)
avPlots(model_agree_gender)

# Neuroticism × Gender
model_neuro_gender <- lm(LifeSatisfaction_Total ~ Neuroticism_c * Gender, data = dat)
summary(model_neuro_gender)

interact_plot(model_neuro_gender, pred = Neuroticism_c, modx = Gender)
sim_slopes(model_neuro_gender, pred = Neuroticism_c, modx = Gender)

# Predicting Positive Affect
model_traits_pa <- lm(pos_af ~ Extraversion_c + Agreeableness_c + Conscientiousness_c + Neuroticism_c + Openness_c, data = dat)
summary(model_traits_pa)

# Predicting Negative Affect
model_traits_na <- lm(neg_af ~ Extraversion_c + Agreeableness_c + Conscientiousness_c + Neuroticism_c + Openness_c, data = dat)
summary(model_traits_na)



