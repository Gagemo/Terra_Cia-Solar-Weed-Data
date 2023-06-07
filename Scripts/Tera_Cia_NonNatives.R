################################################################################
################################################################################
################### Terra Cia Non-Native Species     ###########################
###################        By: Gage LaPierre         ###########################
################################################################################
################################################################################

################### Installs Packages if Needed ################################

list.of.packages <- c("tidyverse", "agricolae", "labelled", "vegan", "labdsv")
new.packages <- list.of.packages[!(list.of.packages 
                                   %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

############################ Loads Packages  ###################################

library(tidyverse)
library(vegan)
library(agricolae)
library(labelled)
library(labdsv)

# Clears environment
rm(list=ls(all=TRUE))

# Clears history
cat("\014") 

set.seed(2)

############ Understory Species Richness/Diversity #############################

Data <- read.csv("Terra_Cia-Solar-Weed-Data/Data/Terra Cia - Solar Weed Data - Pre & Post Data.csv")
str(Data)

# Reclasifys coverage data (CV) from 1-7 scale to percent scale #
Data <- mutate(Data, CV_Midpoint = case_when(
  grepl(0, Cover) ~ 0,
  grepl(1, Cover) ~ 0.5,
  grepl(2, Cover) ~ 2.5,
  grepl(3, Cover) ~ 15.5,
  grepl(4, Cover) ~ 38,
  grepl(5, Cover) ~ 63,
  grepl(6, Cover) ~ 85.5,
  grepl(7, Cover) ~ 98,
))

# Remove Plots 1 & 2 #
Data = filter(Data, Plot != 1)
Data = filter(Data, Plot != 2)

# Remove extra quadrats #
Data = filter(Data, Qdrt != 3)
Data = filter(Data, Qdrt != 4)
Data = filter(Data, Qdrt != 5)

# Remove Native Species #
Data = filter(Data, Native..N..or...Non..NN. == "NN")

# Separate Data by Pre & Post #
Pre = filter(Data, Pre.Post == "Pre")
Post = filter(Data, Pre.Post == "Post")

################################## Gunieagrass Change ##########################

GRASS = filter(Data, Species == "UROMAX")

Pre_Abundance <- GRASS[which(GRASS$Pre.Post == "Pre"),]
Post_Abundance <- GRASS[which(GRASS$Pre.Post == "Post"),]
Abundance_w <- full_join(Pre_Abundance, Post_Abundance, by = c('Plot', 'Treatment'))
Abundance_w$CV_Midpoint.x <- ifelse(is.na(Abundance_w$CV_Midpoint.x), 0, Abundance_w$CV_Midpoint.x)
Abundance_w$CV_Midpoint.y <- ifelse(is.na(Abundance_w$CV_Midpoint.y), 0, Abundance_w$CV_Midpoint.y)
GRASS_Abundance <- Abundance_w %>% 
  dplyr::select(Plot, Treatment, CV_Midpoint.x, CV_Midpoint.y) %>% 
  group_by(Plot, Treatment) %>% 
  mutate(GRASS_Change_Abundance = CV_Midpoint.y - CV_Midpoint.x)

# Plot Gunieagrass Change in Cover
GRASS_Cov = 
  ggplot(GRASS_Abundance, aes(Treatment, GRASS_Change_Abundance, fill = Treatment))  +
  geom_boxplot() +
  ylim(-50, 100) +
  labs(x = "", y = "Gunieagrass % Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", size = 16, face = "bold"),
        axis.text.y = element_text(color = "black", size = 16, face = "bold"),  
        axis.title.x = element_text(color = "black", size = 16, face = "bold"),
        axis.title.y = element_text(color = "black", size = 16, face = "bold"),
        legend.position = "none",
        strip.text.x = element_text(size = 16, face = "bold"))

GRASS_Cov
ggsave("GRASS_Cov.png", width = 12, height = 8)


GRASS_Cov_aov = lme(GRASS_Change_Abundance ~ Treatment, 
                    random = ~1|Plot, data=GRASS_Abundance)
anova(GRASS_Cov_aov)

############################## IMPCYL Coverage ########################################
#CEASER = filter(Data, Species == "URELOB" | Species == "DEDURE")

CEASER = filter(Data, Species == "CEASER")

Pre_Abundance <- CEASER[which(CEASER$Pre.Post == "Pre"),]
Post_Abundance <- CEASER[which(CEASER$Pre.Post == "Post"),]
Abundance_w <- full_join(Pre_Abundance, Post_Abundance, by = c('Plot', 'Treatment'))
Abundance_w$CV_Midpoint.x <- ifelse(is.na(Abundance_w$CV_Midpoint.x), 0, Abundance_w$CV_Midpoint.x)
Abundance_w$CV_Midpoint.y <- ifelse(is.na(Abundance_w$CV_Midpoint.y), 0, Abundance_w$CV_Midpoint.y)
CEASER_Abundance <- Abundance_w %>% 
  dplyr::select(Plot, Treatment, CV_Midpoint.x, CV_Midpoint.y) %>% 
  group_by(Plot, Treatment) %>% 
  mutate(CEASER_Change_Abundance = CV_Midpoint.y - CV_Midpoint.x)


# Plot CEASER Change in Cover
CEASER_Cov = 
  ggplot(CEASER_Abundance, aes(Treatment, CEASER_Change_Abundance, fill = Treatment))  +
  geom_boxplot() +
  ylim(-50, 100) +
  labs(x = "", y = "CEASER % Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", size = 16, face = "bold"),
        axis.text.y = element_text(color = "black", size = 16, face = "bold"),  
        axis.title.x = element_text(color = "black", size = 16, face = "bold"),
        axis.title.y = element_text(color = "black", size = 16, face = "bold"),
        legend.position = "none",
        strip.text.x = element_text(size = 16, face = "bold"))

CEASER_Cov
ggsave("CEASER_Cov.png", width = 12, height = 8)

CEASER_Cov_aov = lme(CEASER_Change_Abundance ~ Treatment, 
                    random = ~1|Plot, data=CEASER_Abundance)
anova(CEASER_Cov_aov)
