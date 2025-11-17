#In this simulated dataset, brook trout activity (movements/hr) declines as stream temperature increases. Streams with cold-water refugia have higher baseline activity and a weaker decline in activity with increasing temperature compared to streams without refugia. The ANCOVA model tests whether the slopes of activity–temperature relationships differ between the two stream types, answering this ecological question: Do brook trout with cold-water refugia maintain higher activity levels as temperatures rise compared to those without refugia?

set.seed(123)

# intercept for streams with cold refugia
alpha_refugia <- 10     
# lower intercept for streams without refugia
alpha_no_ref  <- 6  
# slope for refugia streams
beta_refugia  <- -0.4   
#slope for no-refugia streams
beta_no_ref   <- -0.8   

n_per_group   <- 50    
sdlog         <- 0.5

# Create the two-level categorical variable
group <- rep(c("ColdRefugia", "NoRefugia"), each = n_per_group)

# Continuous predictor: temperature (°C)
temp <- runif(2 * n_per_group, 10, 22)  

# Simulate lognormal errors 
err_raw <- rlnorm(2 * n_per_group, meanlog = 0, sdlog = sdlog)
err <- err_raw - mean(err_raw)

# Generate activity data (movements per hour)
activity <- ifelse(group == "ColdRefugia",
                   alpha_refugia + beta_refugia * temp + err,
                   alpha_no_ref  + beta_no_ref  * temp + err)

# Combine into dataframe
data <- data.frame(temp, group, activity)

# Fit ANCOVA model (interaction between temperature and group)
fit <- lm(activity ~ temp * group, data = data)
summary(fit)

#PLOT THAT BAD BOY

library(ggplot2)

ggplot(data, aes(x = temp, y = activity, color = group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Brook Trout Activity vs. Temperature in Streams With and Without Thermal Refugia",
       x = "Water Temperature (°C)",
       y = "Activity (movements per hour)",
       color = "Stream Type") +
  theme_minimal()

###PART 2###

#this simulated data represents the relationship between critical thermal maximum values (maximum temperature a fish can tolerate before losing the ability to maintain an upright posture when temperature is increased gradually) of both male and female herring and fish length. In some species, it is reported that females have higher CTmax values overall; however, this may depend on factors such as age and reproductive status.

#Question: Does the relationship between body length and critical thermal maximum differ between male and female herring?

ctmax<-read.csv("ctmax_simulated.csv")
str(ctmax)
summary(ctmax)
ctmax$sex <- factor(ctmax$sex)
#model with interaction
model <- lm(ctmax ~ mass * sex, data = ctmax)
summary(model)
#model without interaction
reduced_mod <- lm(ctmax ~ mass + sex, data = ctmax)
summary(reduced_mod)

#comparing the models
anova(model, reduced_mod)

library(ggplot2)

ggplot(ctmax, aes(x = mass, y = ctmax, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  labs(title = "CTmax vs Mass in Male and Female Herring",
       x = "Mass (g)",
       y = "Critical Thermal Maximum (°C)")

###############################
# FINAL MODEL DECISION
###############################
# Based on p-values and ANOVA model comparison, the slope of CTmax vs mass does not differ between males and females, so we can justify removing the interaction. But after removing the interaction, both mass and sex are significant. Larger fish tend to have a higher CTmax, and males have lower overall CTmax than females by about 2 degrees.

#my partner compared almost identically with my partner, with my parameter estimates being really close to the true values. We both came to the same conclusions about removing the interaction. 
