# Start by getting your data!
# IMPORTANT: change the "asymptotic_arias" to be your group name!

source("https://edin.ac/42QTz8T")
get_my_data(group_name = "sampling_seahorses")
#libraries(INSERT ALL LIBRARIES HERE)
library(tidyverse)
library(psych)
# after running the above line of code will, the various datasets should be in your environment
pilotA
pilotB
pilotC
nudges
followup

# below here, you can add R code to address the questions in the assignment

### -----------------------------------------------------------------------

### 3. The uninstalled ---

#combine followup data with nudges data
followupmerged <- merge(nudges, followup, by="ppt")

#clean up inconsistent binary data
table(followupmerged$installed)
followupmerged[followupmerged$installed=="1.0", "installed"] <- "1"

#clean up data by removing participants with unreasonable ages
table(followupmerged$age)
followupmerged <- followupmerged |>
  mutate(age = parse_number(as.character(age))) |>
  filter(age <= 110)

#clean up data by removing scores that fall outside of Likert score range
table(followupmerged$env_concern)
followupmerged <- followupmerged |>
  filter(env_concern >= 9 & env_concern <= 45)

#remove nudged=0 data from analysis for Part 3
followupmerged <- followupmerged |>
  filter(nudged == 1 | nudged == 2)

table(followupmerged$op_sys)
table(followupmerged$nudged)
table(followupmerged$EF)

# assigning labels for values
followupmerged$installed <- factor(followupmerged$installed,
                                   levels = c(0,1),
                                   labels = c("Uninstalled", "Installed"))
followupmerged$nudged <- factor(followupmerged$nudged,
                                   levels = c(1,2),
                                   labels = c("Opt-In Nudge", "Constant Nudge"))


#summary statistics by age and app installed/uninstalled 
followupmerged |> 
  group_by(installed) |>
  summarise(
    m = mean(age),
    s = sd(age)
  )

#density for age and app installed/uninstalled 
ggplot(followupmerged, aes(x = age)) +
  geom_density() + 
  facet_wrap(~installed)

#boxplot for age and app installed/uninstalled 
ggplot(followupmerged, aes(x = installed, y = age, fill = installed)) +
  geom_boxplot()+
  labs(x="App status",y="Age (years)",title="Age and App Install Status")+
  scale_fill_manual(values = c("salmon2", "darkseagreen"))

#summary statistics by environmental concern score and app installed/uninstalled 
followupmerged |> 
  group_by(installed) |>
  summarise(
    m = mean(env_concern),
    s = sd(env_concern)
  )

#density for environmental concern score and app installed/uninstalled
ggplot(followupmerged, aes(x = env_concern)) +
  geom_density() + 
  facet_wrap(~installed)

#boxplot for environmental concern score and app installed/uninstalled
ggplot(followupmerged, aes(x = installed, y = env_concern, fill = installed)) +
  geom_boxplot()+
  labs(x="App status",y="Envirnomental Concern Score",title="Enironmental Concern Score and App Install Status")+
  scale_fill_manual(values = c("salmon2", "darkseagreen"))

#summary statistics by environmental footprint score and app installed/uninstalled
followupmerged |> 
  group_by(installed) |>
  summarise(
    m = mean(EF),
    s = sd(EF)
  )

#density for environmental footprint score and app installed/uninstalled
ggplot(followupmerged, aes(x = EF)) +
  geom_density() + 
  facet_wrap(~installed)

#boxplot for environmental footprint score and app installed/uninstalled
ggplot(followupmerged, aes(x = installed, y = EF)) +
  geom_boxplot()+
  labs(x="uninstalled",y="envirnomental footprint")

#plot for operating system and app installed/uninstalled
plot(table(followupmerged$installed, followupmerged$op_sys))

#plot for nudge type and app installed/uninstalled
plot(table(followupmerged$installed, followupmerged$nudged), main = "App Installation by Nudge Type")

#proportions of nudge type installed/uninstalled the app
chisq.test(table(followupmerged$installed, followupmerged$nudged))$observed

#Multivariate analysis
GLM <- glm(installed ~ age + env_concern + op_sys + nudged + EF, 
           family = binomial, data = followupmerged)
plot(rstudent(GLM, type = 'deviance'), 
     ylab = 'Studentized Deviance Residuals')
rstudent(GLM, type = 'deviance')

arm::binnedplot(fitted(GLM), 
                rstudent(GLM, type = 'deviance'),
                xlab = 'Prob. of Uninstalling the app', 
                ylab = 'Studentized Deviance Residuals')

#Get the results of the Generalized Linear Model 
summary(GLM)
exp(coef(GLM))
exp(confint(GLM))

coefs <- coef(GLM)
se <- summary(GLM)$coefficients[, "Std. Error"]

# Calculate log-odds CI bounds
lower_log <- coefs - 1.96 * se
upper_log <- coefs + 1.96 * se

# Exponentiate to get OR CI bounds
lower_OR <- exp(lower_log)
upper_OR <- exp(upper_log)

# View results
cbind(OR = exp(coefs), Lower_CI = lower_OR, Upper_CI = upper_OR)
