# Start by getting your data!
# IMPORTANT: change the "asymptotic_arias" to be your group name!

source("https://edin.ac/42QTz8T")
get_my_data(group_name = "sampling_seahorses")
#libraries(INSERT ALL LIBRARIES HERE)
library(tidyverse)
# after running the above line of code will, the various datasets should be in your environment
pilotA
pilotB
pilotC
nudges
followup

# below here, you can add R code to address the questions in the assignment

### -----------------------------------------------------------------------

### 1. Pilot studies ---
  ### Question A - Pilot study A
    ### Extracting rows that contain "40 George Square" OR "40GS" using a pattern
pilotA <- pilotA |>
  filter(grepl("40 George Square", room) | grepl("40GS", room))
dim(pilotA)
    ### Removing duplicates(1.01, 13.07, 3.01, 7.18, LG07, Lecture 7)
pilotA <- pilotA[-c(4, 21, 19, 17, 22, 23, 6), ]
    ### Renaming "NA 40GS_LG.07" to "40 George Square 40GS_LG.07"
pilotA[3,1] <- "40 George Square 40GS_LG.07"



    ### Assigning labels for values
pilotA$lights <- factor(pilotA$lights,
                        levels = c(0,1),
                        labels = c("Off", "On"))
pilotA$condition <- factor(pilotA$condition,
                           levels = c(0,1,2),
                           labels = c("Nothing", "A Written Message", "Googly Eyes"))
    ### Data Visualisation of lights left on vs off
pilotA_table <- table(pilotA$condition, pilotA$lights)
pilotA_table
pilotA_graph <- ggplot(pilotA) +
  aes(x = condition, fill = lights) +
  geom_bar(position = "dodge") +
  labs(
    title = "Lights Off vs On"
  ) +
  scale_y_continuous(breaks = c(0,2,4,6)) +
  theme_classic() +
  scale_fill_manual(values = c("grey", "coral"))
  
pilotA_graph
    ### Assumption - Violated (cells had expected counts below 5)
chisq.test(pilotA_table)$expected
    ### Analysis - Not significant, p >- .05
chisq.test(pilotA_table)
  
  ### Question B - Pilot study B



### 2. Nudge nudge, wink wink ---





### 3. The uninstalled ---



