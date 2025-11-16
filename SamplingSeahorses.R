# Start by getting your data!
# IMPORTANT: change the "asymptotic_arias" to be your group name!  
source("https://edin.ac/42QTz8T")
get_my_data(group_name = "sampling_seahorses")

# after running the above line of code will, the various datasets should be in your environment
pilotA
pilotB
pilotC
nudges
followup

# below here, you can add R code to address the questions in the assignment

### -----------------------------------------------------------------------

### 1. Pilot studies ---

#Access the data 

source("https://edin.ac/42QTz8T")
get_my_data(group_name = "sampling_seahorses") 

### Question 1
## Pilot study C

# Load libraries

library(tidyverse) 
library(dplyr) 
library(psych) 

# Remove N/A entries 

pilotC <- pilotC |> filter(!is.na(device_usage)) 

# Describe 

dim(pilotC) 

descriptives <- pilotC |>
  select(device_usage, env_concern) |>
  describe() 

descriptives

# Test for normality 

qqnorm(pilotC$device_usage, main="Q-Q Plot: Device Usage")
qqline(pilotC$device_usage, col="red") 

qqnorm(pilotC$env_concern, main="Q-Q Plot: Device Usage")
qqline(pilotC$env_concern, col="red") 

# Testing association 

cor.test(pilotC$device_usage, pilotC$env_concern) 

# Graphical representation 

ggplot(pilotC, aes(x= device_usage, y= env_concern)) + 
  geom_point() +
  labs(x= "Device Usage (minutes per day)",
       y = "Environmental Concern Score",
       title = "Pilot Study C",
       subtitle = "Relationship Between Device Usage and Environmental Concern"       ) +
  theme_classic() +
  geom_smooth(method=lm, se= FALSE, color= "brown") 


