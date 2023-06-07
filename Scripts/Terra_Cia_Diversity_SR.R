################################################################################
################################################################################
################### Terra Cia Understory Diversity   ###########################
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

############ 1Understory Species Richness/Diversity #############################

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

# Remove extra quadrats #
Data = filter(Data, Qdrt != 3)
Data = filter(Data, Qdrt != 4)
Data = filter(Data, Qdrt != 5)

# Remove physical variables #
Data = filter(Data, Species != "DEDVEG") %>%
       filter(Species != "BARSOI") %>%
       filter(Species != "DEDURO") %>%
       filter(Species != "DEDURE")

# Remove Non-Native Species #
#Data = filter(Data, Native..N..or...Non..NN. != "NN")

# Separate Data by Pre & Post #
Pre = filter(Data, Pre.Post == "Pre")
Post = filter(Data, Pre.Post == "Post")

# Count Species #
SR_ <- Post %>% 
  group_by(Plot, Qdrt, Treatment) %>% 
  summarise(count_SR=n())

################### Species Richness ###########################################
## Species Richness Boxplot ##
SR_Box = 
  ggplot(SR_) +
  geom_boxplot(aes(x = Treatment, y = count_SR, fill  = Treatment)) +
  geom_point(aes(x = Treatment, y = count_SR)) +
  labs(x="Treatment", y = "Species Richness") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))
SR_Box
ggsave("Figures/SR_Box_JustNatives.png")

Spp_rich_aov <- aov(count_SR ~ Treatment, data = SR_)
summary(Spp_rich_aov)

##################### Diversity ################################################

# Create Species Pivot Table #
Spp = dplyr::select(Post, Plot, Species, CV_Midpoint) %>% matrify() 
Spp[] <- lapply(Spp, as.numeric)

# Create Grouped Treatment/ Environment Table and Summaries to fit Species Table #
Treat <- select( Post, Plot, Treatment)
Treat = Treat %>%
  group_by(Plot, Treatment) %>% 
  summarise()

# Diversity Index # 

simpsons = diversity(Spp, index = "simpson")
simpsons = as.data.frame(simpsons)

shannon = diversity(Spp, index = "shannon")
shannon = as.data.frame(shannon)

inv_shan = diversity(Spp, index = "inv")
inv_shan = as.data.frame(inv_shan)

## Merge with diversity data for ggplot ##
simp_treat = cbind(Treat, simpsons) 
shn_treat = cbind(Treat, shannon) 
in_shn_treat = cbind(Treat, inv_shan) 

# Diversity ANOVAs #
simp_aov <- aov(simpsons ~ Treatment, data = simp_treat)
summary(simp_aov)
shannon_aov <- aov(shannon ~ Treatment, data = shn_treat)
summary(shannon_aov)
in_shn_aov <- aov(inv_shan ~ Treatment, data = in_shn_treat)
summary(in_shn_aov)

## Simpsons Boxplot ##
Simp_Box = 
  ggplot(simp_treat) +
  geom_boxplot(aes(x = Treatment, y = simpsons, fill  = Treatment)) +
  geom_point(aes(x = Treatment, y = simpsons)) +
  labs(x="Treatment", y = "Shannon's Diversity Index") +
  ylim(0, 1.1) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))
Simp_Box
ggsave("Figures/Simp_Box.png")

## Shannons Boxplot ##
Shn_Box =
  ggplot(shn_treat) +
  geom_boxplot(aes(x = Treatment, y = shannon, fill  = Treatment)) +
  geom_point(aes(x = Treatment, y = shannon)) +
  labs(x="Treatment", y = "Shannons Diversity Index") +
  theme_classic() +
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))
Shn_Box
ggsave("Figures/Shn_Box.png")


## Inv-Simp Boxplot ##
Inv_simp_Box = 
  ggplot(in_shn_treat) +
  geom_boxplot(aes(x = Treatment, y = inv_shan, fill  = Treatment)) +
  geom_point(aes(x = Treatment, y = inv_shan)) +
  labs(x="Treatment", y = "Inverse Simpson's Diversity Index") +
  theme_classic() +
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))
Inv_simp_Box
ggsave("Figures/Inv_simp_Box.png")

############################## Coverage ########################################

##  Cover Box Plot ##
cover_Box =
  ggplot(Post, aes(x = Treatment, y = CV_Midpoint, fill = Group)) + 
  geom_boxplot() +
  geom_point() +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Treatment") +
  ylab("% Coverage")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))
cover_Box
ggsave("Figures/Fun_cover_Box.png")



