#Q1
library('ggplot2')
ages <- read.csv("D:\\UNI\\2Y 1SEM\\R\\Datasets\\longevity.csv")
View(ages)
ggplot(ages, aes(x = AgeAtDeath, fill = factor(Smokes))) +
  geom_density() +
  facet_grid(Smokes ~ .)

#Q2
summary(ages$AgeAtDeath)
guess <- 73
with(ages, mean((AgeAtDeath - guess)^2))
guess.accuracy <- data.frame()
for (guess in seq(63, 83, by = 1))
{
  prediction.error <- with(ages,
                           mean((AgeAtDeath - guess) ^ 2))
  guess.accuracy <- rbind(guess.accuracy,
                          data.frame(Guess = guess,
                                     Error = prediction.error))
}
ggplot(guess.accuracy, aes(x = Guess, y = Error)) +
  geom_point() +
  geom_line()

#Q3
constant.guess <- with(ages, mean(AgeAtDeath))
with(ages, sqrt(mean((AgeAtDeath - constant.guess) ^ 2)))
smokers.guess <- with(subset(ages, Smokes == 1),
                      mean(AgeAtDeath))
non.smokers.guess <- with(subset(ages, Smokes == 0),
                          mean(AgeAtDeath))
ages <- transform(ages,
                  NewPrediction = ifelse(Smokes == 0,
                                         non.smokers.guess,
                                         smokers.guess))
with(ages, sqrt(mean((AgeAtDeath - NewPrediction) ^ 2)))

#Q4
heights.weights <- read.csv ("D:\\UNI\\2Y 1SEM\\R\\Datasets\\01_heights_weights_genders1.csv")
ggplot(heights.weights, aes(x = Height, y = Weight)) +
  geom_point() +
  geom_smooth(method = 'lm')

#Q5
fitted.regression <- lm(Weight ~ Height,
                        data = heights.weights)
coef(fitted.regression)
intercept <- coef(fitted.regression)[1]
slope <- coef(fitted.regression)[2]
predict(fitted.regression)

#Q6
true.values <- with(heights.weights, Weight)
errors <- true.values - predict(fitted.regression)
residuals(fitted.regression)

#Q7
mean.mse <- 1.09209343
model.mse <- 0.954544
r2 <- 1-(model.mse /mean.mse)
r2
