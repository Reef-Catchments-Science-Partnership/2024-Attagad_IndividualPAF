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
library(scales)

#load in data - you will need to do a dataframe for site's csv
test <- read.csv('msPAF22_Total_Barratta.csv')
test2 <- read.csv("msPAF22_Total_Burdekin.csv")
test3 <- read.csv("msPAF22_Total_Comet.csv")
test4 <- read.csv("msPAF22_Total_Herbert.csv")
test5 <- read.csv("msPAF22_Total_Johnstone.csv")
test6 <- read.csv("msPAF22_Total_Mulgrave.csv")
test7 <- read.csv("msPAF22_Total_NorthJohnstone.csv")
test8 <- read.csv("msPAF22_Total_OConnell.csv")
test9 <- read.csv("msPAF22_Total_Pioneer.csv")
test10 <- read.csv("msPAF22_Total_Proserpine.csv")
test11 <- read.csv("msPAF22_Total_Russell.csv")
test12 <- read.csv("msPAF22_Total_Sandy.csv")
test13 <- read.csv("msPAF22_Total_Tully.csv")

#rename date variable for all data frames too
names(test)[which(colnames(test) == "DATE")] <- "Timestamps"
names(test2)[which(colnames(test2) == "DATE")] <- "Timestamps"
names(test3)[which(colnames(test3) == "DATE")] <- "Timestamps"
names(test4)[which(colnames(test4) == "DATE")] <- "Timestamps"
names(test5)[which(colnames(test5) == "DATE")] <- "Timestamps"
names(test6)[which(colnames(test6) == "DATE")] <- "Timestamps"
names(test7)[which(colnames(test7) == "DATE")] <- "Timestamps"
names(test8)[which(colnames(test8) == "DATE")] <- "Timestamps"
names(test9)[which(colnames(test9) == "DATE")] <- "Timestamps"
names(test10)[which(colnames(test10) == "DATE")] <- "Timestamps"
names(test11)[which(colnames(test11) == "DATE")] <- "Timestamps"
names(test12)[which(colnames(test12) == "DATE")] <- "Timestamps"
names(test13)[which(colnames(test13) == "DATE")] <- "Timestamps"

#combine all site csvs into 1 dataframe. add additional names of dataframes into the brackets.
combined <- bind_rows(test, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11,
                      test12)

combined2 <- rbind(test, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11,
                   test12,test13 )

####facet wrap by site - uses the 'combined' dataframe
comb_plot <-  ggplot(data = combined2, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
  geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
        plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
        strip.text = element_text(size = 12),
        axis.line = element_line(colour = "light gray")) + 
  scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
  scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
  theme(legend.position = "top") + ggtitle("Time-Series PAF") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
 # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
  facet_wrap(~SITENAME, scales = "free_y")
comb_plot
ggsave("All_Sites_Boxplot.png", width = 15, height = 9, dpi = 1000)

#individual box plot for each site 
#Baratta
{ 
ggplot(data = test, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
  geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
        plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
        strip.text = element_text(size = 12),
        axis.line = element_line(colour = "light gray")) + 
  scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
  scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
  theme(legend.position = "top") + ggtitle("Time-Series PAF of Barratta Creek at Northcote") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
  # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
  facet_wrap(~SITENAME, scales = "free_y")
ggsave("Barratta_Boxplot.png", width = 10, height = 9, dpi = 1000)
}
#Burdekin
{
  ggplot(data = test2, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
    geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
          strip.text = element_text(size = 12),
          axis.line = element_line(colour = "light gray")) + 
    scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
    scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
    theme(legend.position = "top") + ggtitle("Time-Series PAF of Burdekin River at Home Hill Inkerman Bridge") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
    # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
    facet_wrap(~SITENAME, scales = "free_y")
  ggsave("Burdekin_Boxplot.png", width = 10, height = 9, dpi = 1000) 
}
#Comet
{
  ggplot(data = test3, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
    geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
          strip.text = element_text(size = 12),
          axis.line = element_line(colour = "light gray")) + 
    scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
    scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
    theme(legend.position = "top") + ggtitle("Time-Series PAF of Comet River at Comet Weir") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
    # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
    facet_wrap(~SITENAME, scales = "free_y")
  ggsave("Comet_Boxplot.png", width = 10, height = 9, dpi = 1000) 
}
#Herbert
{
  ggplot(data = test4, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
    geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
          strip.text = element_text(size = 12),
          axis.line = element_line(colour = "light gray")) + 
    scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
    scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
    theme(legend.position = "top") + ggtitle("Time-Series PAF of Herbert River at John Row Bridge") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
    # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
    facet_wrap(~SITENAME, scales = "free_y")
  ggsave("Herbert_Boxplot.png", width = 10, height = 9, dpi = 1000) 
}
#Johnstone
{
  ggplot(data = test5, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
    geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
          strip.text = element_text(size = 12),
          axis.line = element_line(colour = "light gray")) + 
    scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
    scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
    theme(legend.position = "top") + ggtitle("Time-Series PAF of Johnstone River at Coquette Point") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
    # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
    facet_wrap(~SITENAME, scales = "free_y")
  ggsave("Johnstone_Boxplot.png", width = 10, height = 9, dpi = 1000) 
}
#Mulgrave
{
  ggplot(data = test6, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
    geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
          strip.text = element_text(size = 12),
          axis.line = element_line(colour = "light gray")) + 
    scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
    scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
    theme(legend.position = "top") + ggtitle("Time-Series PAF of Mulgrave River at Deeral") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
    # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
    facet_wrap(~SITENAME, scales = "free_y")
  ggsave("Mulgrave_Boxplot.png", width = 10, height = 9, dpi = 1000) 
}
#NorthJohnstone
{
  ggplot(data = test7, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
    geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
          strip.text = element_text(size = 12),
          axis.line = element_line(colour = "light gray")) + 
    scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
    scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
    theme(legend.position = "top") + ggtitle("Time-Series PAF of North Johnstone River at Goondi") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
    # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
    facet_wrap(~SITENAME, scales = "free_y")
  ggsave("NorthJohnstone_Boxplot.png", width = 10, height = 9, dpi = 1000) 
}
#OConnell
{
  ggplot(data = test8, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
    geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
          strip.text = element_text(size = 12),
          axis.line = element_line(colour = "light gray")) + 
    scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
    scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
    theme(legend.position = "top") + ggtitle("Time-Series PAF of O'Connell River at Caravan Park") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
    # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
    facet_wrap(~SITENAME, scales = "free_y")
  ggsave("OConnell_Boxplot.png", width = 10, height = 9, dpi = 1000) 
}
#Pioneer
{
  ggplot(data = test9, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
    geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
          strip.text = element_text(size = 12),
          axis.line = element_line(colour = "light gray")) + 
    scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
    scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
    theme(legend.position = "top") + ggtitle("Time-Series PAF of Pioneer River at Dumbleton Pump Station Headwater") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
    # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
    facet_wrap(~SITENAME, scales = "free_y")
  ggsave("Pioneer_Boxplot.png", width = 10, height = 9, dpi = 1000) 
}
#Proserpine
{
  ggplot(data = test10, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
    geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
          strip.text = element_text(size = 12),
          axis.line = element_line(colour = "light gray")) + 
    scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
    scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
    theme(legend.position = "top") + ggtitle("Time-Series PAF of Proserpine River at Glen Isla") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
    # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
    facet_wrap(~SITENAME, scales = "free_y")
  ggsave("Proserpine_Boxplot.png", width = 10, height = 9, dpi = 1000) 
}
#Russell
{
  ggplot(data = test11, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
    geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
          strip.text = element_text(size = 12),
          axis.line = element_line(colour = "light gray")) + 
    scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
    scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
    theme(legend.position = "top") + ggtitle("Time-Series PAF of Russell River at East Russell") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
    # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
    facet_wrap(~SITENAME, scales = "free_y")
  ggsave("Russell_Boxplot.png", width = 10, height = 9, dpi = 1000) 
}
#Sandy
{
  ggplot(data = test12, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
    geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
          strip.text = element_text(size = 12),
          axis.line = element_line(colour = "light gray")) + 
    scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
    scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
    theme(legend.position = "top") + ggtitle("Time-Series PAF of Sandy Creek at Homebush") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
    # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
    facet_wrap(~SITENAME, scales = "free_y")
  ggsave("Sandy_Boxplot.png", width = 10, height = 9, dpi = 1000) 
}
#Tully 
{
  ggplot(data = test13, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
    geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.5),
          plot.background = element_rect(colour= "black", linewidth = 0.5, fill = "white"),
          strip.text = element_text(size = 12),
          axis.line = element_line(colour = "light gray")) + 
    scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
    scale_y_continuous(labels = label_number() ) + #note removed scale_y_continuous(trans = 'log10') because I don't think its useful
    theme(legend.position = "top") + ggtitle("Time-Series PAF of Tully River at Euramo") + xlab("Sampling Year") + ylab("Potentially Affected Fraction (PAF)") +
    # geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) +
    facet_wrap(~SITENAME, scales = "free_y")
  ggsave("Tully_Boxplot.png", width = 10, height = 9, dpi = 1000) 
}


test



#normal boxplots - original
test.plot1 <-  ggplot(data = test, aes(x = factor(Sampling.Year), y = Daily.Ave.PAF )) + CRSSIO::stat_boxplot_custom(qs = c(.01, .05, .5, .95, .99), width=0.8, fill="white") +
  geom_jitter(height=.02, width=0.4, size = 1.0,alpha = 0.4) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "light gray")) + 
  scale_x_discrete(guide = guide_axis(angle = 65)) + # makes x-axis labels vertical = 90 
  scale_y_continuous(trans ='log10') + 
  theme(legend.position = "top") + ggtitle("PRM Total Pesticides per sample at Sandy Creek") + xlab("Sampling Year") + ylab("ms-PAF") +
  geom_smooth(method = "lm", se=FALSE, color="grey", aes(group=1)) 
test.plot1






### nat's original plots x y 
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
