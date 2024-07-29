install.packages('devtools') 
library(devtools)
devtools::install_github('BoulderCodeHub/CRSSIO')
force = TRUE
#load packages
library(tidyverse)
library(ggplot2)
library(scales)
library(RWDataPlyr)
library(CRSSIO)
library(ggpubr)

#Creating box plot for each financial year
test <- read.csv('PAF_Total_Timestamps_msPAF18_2024-05-29.csv')
names(test)[which(colnames(test) == "DATE")] <- "Timestamps"
test.plot1 <-  ggplot(data = test, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
  geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "light gray")) + 
  scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
  scale_y_continuous(trans ='log10') + 
  theme(legend.position = "top") + ggtitle("PRM Total 18 Pesticides per sample at Tully River") + xlab("Sampling Year") + ylab("ms-PAF") +
  geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1))

test.plot1 # view
  
#creating xy plot for each timestamp
test.plot2 <- ggplot(data = test, aes(x=factor(Timestamps), y = Daily.Ave.PAF)) + 
  geom_jitter(width = 0.25, height = 0.5) +
  theme(legend.position = "top") + ggtitle("PRM Total Pesticides per sample at Sandy Creek") + xlab("Sampling Year") + ylab("ms-PAF") +
  geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1))
test.plot2 #view

#creating xy plot for each financial year
test.plot3 <- ggplot(data = test, aes(x=factor(Sampling.Year), y = Daily.Ave.PAF)) + 
  geom_jitter(width = 0.25, height = 0.5) +
  theme(legend.position = "top") + ggtitle("PRM Total Pesticides per sample at Sandy Creek") + xlab("Sampling Year") + ylab("ms-PAF") +
  geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1))
test.plot3 #view
