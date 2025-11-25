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

### 1. Pilot studies ---
# Question A - Pilot study A
pilotA <- pilotA |>
  filter(
    !startsWith(room, "NA ")
  )
# identifying and removing duplicates (keeping first ones)
pilotA |> 
  count(room, sort = TRUE) |>
  filter(n>1)
pilotA <- pilotA |>
  group_by(room) |>
  slice(1) |>
  ungroup()
dim(pilotA)
# Assigning labels for values
pilotA$lights <- factor(pilotA$lights,
                        levels = c(0,1),
                        labels = c("Off", "On"))
pilotA$condition <- factor(pilotA$condition,
                           levels = c(0,1,2),
                           labels = c("Nothing", "A Written Message", "Googly Eyes"))
# Data Visualisation
pilotA_table <- table(pilotA$condition, pilotA$lights)
pilotA_table
pilotA_graph <- ggplot(pilotA) +
  aes(x = condition, fill = lights) +
  geom_bar(position = "dodge") +
  labs(
    title = "Lights Off vs On"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("salmon2", "darkseagreen2"))
pilotA_graph
# Excepted cell assumption
chisq.test(pilotA_table)$expected
# Analysis - Significant, p = .038
chisq.test(pilotA_table)
plot(pilotA_table, main = "Contingency Table of Light Status by Condition ")

# Question B - Pilot study B
# Data Cleaning
pilotB <- pilotB |>
  mutate(
    discount = replace(discount, 1:20, "50p keep cup discount"),
    discount = replace(discount, 21:40, "no discount")
  )
# Descriptive statistics
pilotB_descriptives <- pilotB|>
  group_by(discount) |>
  summarise(
    coffees_avg = mean(ncoffees),
    coffees_sd = sd(ncoffees)
  )
# Descriptive plot
pilotB_graph <- ggplot(pilotB) +
  aes(x = discount, y = ncoffees, fill = discount) +
  geom_boxplot(
    width = 0.5
  ) +
  labs(
    title = "Coffee sales: Discount vs No-Discount",
    y = "Coffees Sold",
    x = "Discount"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("darkseagreen2", "salmon2")) +
  theme(legend.position = "none")
pilotB_graph
# Normality assumption checks (Normal distribution, p > .05)
shapiro.test(pilotB$ncoffees[pilotB$discount == "50p keep cup discount"])
shapiro.test(pilotB$ncoffees[pilotB$discount == "no discount"])
# Analysis (Not Significant, p> .05)
t.test(ncoffees ~ discount, data = pilotB, alternative = "greater")

# Question C - Pilot study C
# Remove N/A entries 
pilotC <- pilotC |> filter(!is.na(device_usage)) 
# Test for normality - Initial
qqnorm(pilotC$device_usage, main="Q-Q Plot: Device Usage")
qqline(pilotC$device_usage, col="salmon2") 
pilotCmod <- lm(env_concern ~ device_usage, data = pilotC)
plot(pilotCmod, which = 2)
# Remove outliers 
pilotC <- pilotC[-c(9,29), ] 
# Descriptive Statistics
dim(pilotC) 
pilotCdescriptives <- pilotC |>
  select(device_usage, env_concern) |>
  describe() 
pilotCdescriptives
# Testing association 
cor.test(pilotC$device_usage, pilotC$env_concern) 
# Graphical representation 
ggplot(pilotC, aes(x= device_usage, y= env_concern)) + 
  geom_point() +
  labs(x= "Device Usage (hours per day)",
       y = "Environmental Concern Score",
       title = "Pilot Study C",
       subtitle = "Relationship Between Device Usage and Environmental Concern") +
  theme_minimal() +
  geom_smooth(method=lm, se= FALSE, color= "salmon2") 

### 2. Nudge nudge, wink wink ---
#identify problems
summary(nudges)

#remove incorrect age values and convert to numeric
nudges <- 
  nudges |>
  mutate(
    age = parse_number(as.character(age))) |>
  filter(age >= 18 & age <= 110)

#remove environmental concern scores under 9 and above 45
nudges <- 
  nudges |>
  mutate(
    env_concern = as.numeric(as.character(env_concern))) |>
  filter(env_concern >= 9 & env_concern <= 45)

#from 216 obs to 212

#assigning labels
nudges <- 
  nudges |>
  mutate(
    nudged_factor = factor(nudged, 
                           levels = c(0, 1, 2),
                           labels = c("No nudge", "Opt-in nudge", "Constant nudge")))

#descriptives
describe(nudges |> select(-nudged))
table(nudges$nudged)

# Normal Q-Q Plots
nudges |>
  select(nudged, env_concern, EF) |>
  mutate(nudged = as.numeric(nudged)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  mutate(variable = case_when(
    variable == "nudged" ~ "Nudged",
    variable == "env_concern" ~ "Environmental Concern",
    variable == "EF" ~ "Environmental Footprint",
    TRUE ~ variable
  )) |>
  ggplot(aes(sample = value, color = variable, fill = variable)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ variable, scales = "free") +
  scale_color_manual(values = c("Nudged" = "salmon1", 
                                "Environmental Concern" = "lightskyblue2", 
                                "Environmental Footprint" = "mediumslateblue")) +
  labs(title = "Normal Q-Q Plots") +
  theme_minimal() +
  theme(legend.position = "none")

#check for outliers
ggplot(nudges, aes(x = nudged_factor, y = EF, fill = nudged_factor)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  scale_fill_manual(values = c("No nudge" = "salmon1", 
                               "Opt-in nudge" = "lightskyblue2", 
                               "Constant nudge" = "mediumslateblue")) +
  labs(title = "Environmental Footprint by Nudge Type",
       x = "Nudge Type",
       y = "Environmental Footprint") +
  theme_minimal() +
  theme(legend.position = "none")

#REMOVE outliers purr
Q1 <- quantile(nudges$EF, 0.25, na.rm = TRUE)
Q3 <- quantile(nudges$EF, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

nudges <- nudges |>
  filter(EF >= lower_bound & EF <= upper_bound)

#check visually
ggplot(nudges, aes(x = nudged_factor, y = EF, fill = nudged_factor)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  scale_fill_manual(values = c("No nudge" = "salmon1", 
                               "Opt-in nudge" = "lightskyblue2", 
                               "Constant nudge" = "mediumslateblue")) +
  labs(title = "Environmental Footprint by Nudge Type (After Removing Outliers)",
       x = "Nudge Type",
       y = "Environmental Footprint") +
  theme_minimal() +
  theme(legend.position = "none")

#following visualisation
ggplot(nudges, aes(x = env_concern, y = EF, col = nudged_factor, fill = nudged_factor)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.0) + 
  scale_color_manual(values = c("No nudge" = "salmon1", 
                                "Opt-in nudge" = "lightskyblue2", 
                                "Constant nudge" = "mediumslateblue")) +
  scale_fill_manual(values = c("No nudge" = "salmon1", 
                               "Opt-in nudge" = "lightskyblue2", 
                               "Constant nudge" = "mediumslateblue")) +
  labs(
    title = "Environmental Footprint by Concern Level and Nudge Type",
    x = "Environmental Concern Score",
    y = "Environmental Footprint",
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#initial analysis
model <- lm(EF ~ nudged_factor * env_concern, data = nudges)
summary(model)

#plot data
plotdata <- expand_grid(
  env_concern = 9:45,
  nudged_factor = c("No nudge", "Opt-in nudge", "Constant nudge")
)

#plot
broom::augment(model, newdata = plotdata, interval="confidence") |>
  ggplot(aes(x= env_concern, y = .fitted, 
             col = nudged_factor, fill = nudged_factor)) + 
  geom_line() +
  geom_ribbon(aes(ymin=.lower,ymax=.upper), alpha=.3)

#refit model


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
plot(table(followupmerged$installed, followupmerged$nudged))

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
