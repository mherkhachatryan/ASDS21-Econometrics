library("wooldridge")
library("tidyverse")

data_ceo <- wooldridge::ceosal2
# variable assignment 
salary <- data_ceo$salary
ceoten <- data_ceo$ceoten
log_salary <- log(salary)

# 1
average_salary <- mean(salary) * 1000 # dollars
tenure <- mean(ceoten) # if I got right what is a tenure

# 2
first_year_ceo_number <- length(data_ceo[ceoten == 0, ])
longest_served_ceo <- max(ceoten)

# 3
model <- lm(log_salary ~ ceoten, data = data_ceo)
summary(model)
beta_1_model <- coef(model)[2]
beta_0_model <- coef(model)[1]

predicted_salary_increase <-beta_1_model

# 4
ggplot(data_ceo, aes(x=ceoten, y=log_salary)) +
  geom_point() + stat_smooth(se=FALSE, method=lm)

# 5
beta_1_theory <- cov(ceoten, log_salary) / var(ceoten)
print(paste("Error comparison of formula and lm model is: ",
            (beta_1_theory - beta_1_model)))
print("Model is consistent with theory")

# 6
mean_ceoten <- mean(ceoten)
beta_0_theory <- mean_ceoten - beta_1_theory * mean_ceoten

print(paste("Error comparison of formula and lm model is: ",
      (beta_0_theory - beta_0_model)))

# 7
log_salary_fit <- beta_0_model + beta_1_model * ceoten
n <- length(log_salary)

s <- sqrt (sum((log_salary - log_salary_fit)^2)/ (n-2))

se_theory_beta_0 <- s * sqrt(sum(ceoten^2) / (n* sum((ceoten - mean(ceoten))^2 ) ))

se_model <- sqrt(diag(vcov(model)))
se_model_beta_0 <- se_model[1]
print(paste("Difference in model and theory", se_model_beta_0 - se_theory_beta_0))
print("They are consistent!")

# 8
se_theory_beta_1 <- s * sqrt(n/ (n * sum(ceoten^2) - (sum(ceoten))^2))
se_model_beta_1 <- se_model[2]
print(paste("Difference in model and theory", se_model_beta_1 - se_theory_beta_1))
print("They are consistent!")

# 9
e <- log_salary - log_salary_fit
mean_log_salary <- mean(log_salary)

ESS<-sum(e^2)
RSS<-sum((log_salary_fit-mean_log_salary)^2)
TSS<-sum((log_salary-mean_log_salary)^2)

R2_theory <- (TSS-ESS)/TSS
R2_model <- summary(model)$r.squared

print(paste("Difference in model and theory", R2_model - R2_theory))
print("They are consistent!")