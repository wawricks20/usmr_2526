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

#check visually
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
plot(model, which = 2)
plot(model, which = 4)

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
  geom_ribbon(aes(ymin=.lower,ymax=.upper), alpha=.3) + scale_color_manual(values = c("No nudge" = "salmon1", 
                                                                                      "Opt-in nudge" = "lightskyblue2", 
                                                                                      "Constant nudge" = "mediumslateblue")) +
  scale_fill_manual(values = c("No nudge" = "salmon1", 
                               "Opt-in nudge" = "lightskyblue2", 
                               "Constant nudge" = "mediumslateblue"))

#recenter model to mean of env concern
nudges_recentered <-
  nudges |>
  mutate(
    env_concern_recenter = env_concern - 26.64)
model_recentered <- lm(EF ~ nudged_factor * env_concern_recenter, data = nudges_recentered)
summary(model_recentered)
