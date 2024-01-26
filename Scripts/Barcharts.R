#############################################################
#### Panel A-E of Figure 1                               ####
#### Created by: Courtney Davis                          #### 
#### Modified by:                                        ####
#### Last checked: 26 Jan 2024                           ####
#############################################################


############  Load packages
library(here)
library(tidyverse)
library(ggplot2)
library(viridis)
library(readxl)

############ Load data
############ Load data
data<-read_xlsx(here("Data","Dataset 4. Who studies bias.xlsx"))
data <- data[-(data$Dataset == "Manuscripts submitted"),c(1:4)]
data$Dataset <- factor(data$Dataset, levels = c("Studies on solution for dem",
                                                "Studies on bias for dem",
                                                "All studies on bias",
                                                "Manuscripts accepted"))




### Plot individual panels
# Assumed gender
data_bcA<- subset(data, Demographic == "Assumed_gender")

ggplot(data = na.omit(data_bcA), mapping = aes(y = Dataset, x = Total, fill = Demographic.category)) + 
  geom_col(position = "fill", width = 0.8) + 
  scale_fill_manual(values = c("#FBED7D","#7B5585"), labels = c("Female", "Male")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_line(1),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        text = element_text(size = 20),
        axis.text.y=element_text(margin=margin(r=0)),
        plot.margin = unit(c(0.2,0.5,0.2,0.2),"cm"),
  panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = "white")) +
scale_x_continuous(expand = c(0,0), labels = c(0, 25, 50, 75, 100))
ggsave(here("Figures", "barchart gender.jpg"), width = 7, height = 3, units = "in", dpi = 600)

# Continent
data_bcB<- subset(data, Demographic == "Continent")
data_bcB$Demographic.category <- factor(data_bcB$Demographic.category, 
                                        levels = rev(c("Europe",
                                                   "North America",
                                                   "Oceania",
                                                   "Asia",
                                                   "Africa",
                                                   "Latin America")))

ggplot(data = na.omit(data_bcB), mapping = aes(y = Dataset, x = Total, fill = Demographic.category)) + 
  geom_col(position = "fill", width = 0.8) + 
  scale_fill_manual(values = c("#FBED7D","#ACDA8C","#7EBDA7","#779CAA","#787AA4","#7B5585"), labels = c("LAm", 
                                                                                                        "Af",
                                                                                                        "As",
                                                                                                        "Oc",
                                                                                                        "NAm",
                                                                                                        "Eur")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_line(1),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        text = element_text(size = 20),
        axis.text.y=element_text(margin=margin(r=0)),
        plot.margin = unit(c(0.2,0.5,0.2,0.2),"cm"),
        panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = "white")) +
scale_x_continuous(expand = c(0,0), labels = c(0, 25, 50, 75, 100))
ggsave(here("Figures", "barchart continent.jpg"), width = 7, height = 3, units = "in", dpi = 600)


# Language
data_bcC<- subset(data, Demographic == "Language")
data_bcC$Demographic.category <- factor(data_bcC$Demographic.category, levels = c("Not_English", "English"))
ggplot(data = na.omit(data_bcC), mapping = aes(y = Dataset, x = Total, fill = Demographic.category)) + 
  geom_col(position = "fill", width = 0.8) + 
  scale_fill_manual(values = c("#FBED7D","#7B5585"), labels = c("English not primary", "English primary")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_line(1),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        text = element_text(size = 20),
        axis.text.y=element_text(margin=margin(r=0)),
        plot.margin = unit(c(0.2,0.5,0.2,0.2),"cm"),
        panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = "white")) +
scale_x_continuous(expand = c(0,0), labels = c(0, 25, 50, 75, 100))
ggsave(here("Figures", "barchart language.jpg"), width = 7, height = 3, units = "in", dpi = 600)



# Development
data_bcD<- subset(data, Demographic == "Development")
ggplot(data = na.omit(data_bcD), mapping = aes(y = Dataset, x = Total, fill = Demographic.category)) + 
  geom_col(position = "fill", width = 0.8) + 
  scale_fill_manual(values = c("#FBED7D","#7B5585"), labels = c("High to low", "Very high")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_line(1),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        text = element_text(size = 20),
        axis.text.y=element_text(margin=margin(r=0)),
        plot.margin = unit(c(0.2,0.5,0.2,0.2),"cm"),
        panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = "white")) +
scale_x_continuous(expand = c(0,0), labels = c(0, 25, 50, 75, 100))
ggsave(here("Figures", "barchart development.jpg"), width = 7, height = 3, units = "in", dpi = 600)


# Prestige
data_bcE<- subset(data, Demographic == "Prestige")
data_bcE$Demographic.category <- factor(data_bcE$Demographic.category, levels = c("Low Prestige",
                                                                                  "Middle Prestige",
                                                                                  "High Prestige"))
ggplot(data = na.omit(data_bcE), mapping = aes(y = Dataset, x = Total, fill = Demographic.category)) + 
  geom_col(position = "fill", width = 0.8) + 
  scale_fill_manual(values = c("#FBED7D","#78ACAA","#7B5585"), labels = c("High", "Mid", "Low")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_line(1),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), 
        text = element_text(size = 20),
        axis.text.y=element_text(margin=margin(r=0)),
        plot.margin = unit(c(0.2,0.5,0.2,0.2),"cm"),
        panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = "white")) +
  scale_x_continuous(expand = c(0,0), labels = c(0, 25, 50, 75, 100))
ggsave(here("Figures", "barchart prestige.jpg"), width = 7, height = 3, units = "in", dpi = 600)
