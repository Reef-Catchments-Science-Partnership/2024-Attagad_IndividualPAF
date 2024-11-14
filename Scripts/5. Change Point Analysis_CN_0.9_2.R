# Change Point Analysis
# libraries ---------------

install.packages("rjags") 
Sys.setenv(JAGS_HOME="C:/Users/uqcneela/AppData/Local/Programs/JAGS/JAGS-4.3.1")

library(mcp)
library(ggplot2)
library(jagshelper)
library(Rbeast)
library(coda)
library(lubridate)
library(ggpubr)
library(forecast)
library(patchwork)
library(cowplot)
library(fitdistrplus)
library(jagsUI)
library(rjags)

# > Set WD and read in data ----------
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Other eg WQI work/Nat change point 2024")
Sandy = read.csv("msPAF22_Sandy_CN.csv", header=T)

# > Clean df and create additional variables -----------
{
  Sandy$DATE_conv <- as.Date(Sandy$DATE, format="%d/%m/%Y",  tz = "Australia/Brisbane") # fix DATE format
  Sandy$Month <- month(Sandy$DATE_conv) # extract 'Month'
  Sandy$Season <- ifelse(Sandy$Month %in% c(11, 12, 1, 2, 3, 4), 'Wet', 'Dry') # create 'Season' variable (i applied Nov-April as standardised wet season)
  Sandy$Season <- as.factor(Sandy$Season)
  financial_year_months <- c(7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6) # order months so that we go from 1 July - 30 June in plots, same as Sampling.Year
  Sandy$Month <- factor(Sandy$Month, levels = financial_year_months)
  Sandy$sqrtVALUE <- sqrt(Sandy$Daily.Ave.PAF) # create sqrt of PAF
  str(Sandy) # check
}


# > Plotting raw data for visualisation and patterns -----------
# plot PAF by month to investigate seasonality, facet wrap by year
PAF_plot <- ggplot(Sandy, aes(x = Month, y = Daily.Ave.PAF, fill=Season, alpha = 0.8)) +
  geom_boxplot() +
  geom_jitter(alpha=0.2, size = 1, position = position_jitterdodge())+
  scale_fill_brewer(palette = "Paired", direction = -1) +
  facet_wrap(~ Sampling.Year) +
  labs(x = "Month", y = "Daily Average PAF", title = "Box Plots of Daily Average PAF by Month and Sampling Year") +
  theme_bw()
PAF_plot
ggsave("PAF plot by season and sampling year.png", width = 15, height = 9, dpi = 1000)


# DAta visualisation - all data
# these plots show there is definite seasonality in the untransformed data --> we will investigate that further with the tranformed data later in the script
x <- ggplot(data = Sandy, aes(x =Daily.Ave.PAF)) + 
  geom_histogram(binwidth=5, 
                 fill = "#9eca10", color = 'black') + 
  labs(x = 'Percent Affected Fraction (%)', y = 'Relative frequency') + 
  theme_bw ()+ 
  theme(text = element_text(size = 12),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "light gray")) +
  theme(axis.title.x  = element_text(size = 12)) 
x

y <- ggplot(data = Sandy, aes(x=Daily.Ave.PAF, fill=Season)) + # this line tells r which dataset to use (data =...) and which column to plot (x =...) 
  geom_density(alpha=.45) + # this line sets the transparency of the fill
  theme(text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # remove background gridlines for a cleaner plot
        panel.background = element_blank(), axis.line = element_line(colour = "light gray"), # remove background colour and create cleaner grey axes
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), # remove ticks (optional)
        legend.position = c(0.2, 0.85)) + # push legend to top so it fits well when plots are aligned in grid
  # xlim(-3, 6) + # this line needs to be adjusted across BOTH plots to match the spread of the data and to ensure x-axes line up. Keep kernel density smoothing in mind.
  guides(fill = guide_legend(title = "Season")) +  
  labs(x= "Percent Affected Fraction (%)") 
y

z <- ggplot(Sandy, aes(x = Month, y = Daily.Ave.PAF, fill = Season)) +
  geom_boxplot(alpha=0.6) +
  geom_jitter(alpha=0.1, position = position_jitterdodge())+
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.position = c(0.85, 0.85)) +
  labs(x = 'Month') + 
  scale_fill_brewer(palette = "Paired") +
  scale_shape_manual(values = c(1, 2)) + 
  labs(y= "Percent Affected Fraction (%)")
z

# plot all 3 as grid
plot_grid(
  plot_grid(x, y, ncol = 1), # Combine x and y vertically
  z,                         # Combine z to the right of the combined x and y
  ncol = 2,                  # Number of columns
  rel_widths = c(1, 1)       # Relative widths of the columns
)
ggsave("PAF data visualisation.png", width = 16, height = 9, dpi = 1000)


# > Investigate which transform to use on the data -------------
# plot log and sqrt transforms to see which one normalises the data better
fit.norm <- fitdist(Sandy$logVALUE, "norm")
plot(fit.norm)

fit.norm <- fitdist(Sandy$sqrtVALUE, "norm")
plot(fit.norm)

# additional check - Investigate appropriate traensform for data using cellen and frey graph
# The kurtosis and squared skewness are plotted as a blue point named "Observation". 
# Observation: the sqrt transform is close to normal so this is the appropriate transform for the Sandy data
Dist_check_Sandy <- descdist(Sandy$sqrtVALUE, discrete = FALSE, boot = 1000)

# NOTE THAT DATA APPEARS SEASONAL IN GGPLOTS ABOVE!
# Investigate seasonality using autocorrelation (patterned autocorrlation can indicate seasonality) 
# the ACF plot shows that there is a seasonal cycle with strong autocorrelation 
# this needs to be accounted for to satisfy rjags model/method assumptions
# if not accounted for, it can result in:
#       - misidentification of change points
#       - high error rate/low model performance
#       - noise and patterning in the model residuals making it difficult to assess whether model has worked
Acf(Sandy$sqrtVALUE)




# >1CP rJAGS trigonomic model to account for seasonality --------------
# THIS IS THE CORRECT JAGS MODEL TO USE!

# clear cache --------
rm(list = ls()) # Clear all objects from the global environment so they dont interfere with the new model
gc() # Free up memory and report the memory usage

# reload data, Clean df and create additional variables -----------
Sandy <- read.csv("msPAF22_Sandy_CN.csv", header=T)
{
  Sandy$DATE_conv <- as.Date(Sandy$DATE, format="%d/%m/%Y",  tz = "Australia/Brisbane") # fix DATE format
  Sandy$Month <- month(Sandy$DATE_conv) # extract 'Month'
  Sandy$Season <- ifelse(Sandy$Month %in% c(11, 12, 1, 2, 3, 4), 'Wet', 'Dry') # create 'Season' variable (i applied Nov-April as standardised wet season)
  Sandy$Season <- as.factor(Sandy$Season)
  financial_year_months <- c(7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6) # order months so that we go from 1 July - 30 June in plots, same as Sampling.Year
  Sandy$Month <- factor(Sandy$Month, levels = financial_year_months)
  Sandy$sqrtVALUE <- sqrt(Sandy$Daily.Ave.PAF) # create sqrt of PAF
  str(Sandy) # check
}

# set up simpler trigonomic rjags model that accounts for seasonality----------
model = list(
  sqrtVALUE ~ 1 + sin(time), # Using only sine for seasonality
  ~ 1 + time # Linear trend
)

ex = mcp_example("trigonometric")
fit = mcp(model, cores = 3, data = Sandy)

# extract and plot residuals to check model fit
residuals <- residuals(fit) # extract residuals (leftover error that cant be accounted for) from model
fit.norm <- fitdist(residuals$residuals, "norm") # if there is a normal distribution there is no excess patterning in the residuals = model has fit well
plot(fit.norm)



# run the same model but in CODA to extract and plot diagnostics
# Define the model
library(coda)
library(fitdistrplus)
# Define the model
model_string <- "
model {
  for (i in 1:N) {
    sqrtVALUE[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta * time[i] + gamma * sin(time[i])
  }
  alpha ~ dnorm(0, 0.001)
  beta ~ dnorm(0, 0.001)
  gamma ~ dnorm(0, 0.001)
  tau <- pow(sigma, -2)
  sigma ~ dunif(0, 100)
}
"

# Prepare data
data_list <- list(
  sqrtVALUE = Sandy$sqrtVALUE,
  time = Sandy$time,
  N = nrow(Sandy)
)

# Initialize the model
model <- jags.model(textConnection(model_string), data = data_list, n.chains = 3)

# Update the model (burn-in)
update(model, n.iter = 11000)

# Sample from the posterior
samples <- coda.samples(model, variable.names = c("alpha", "beta", "gamma", "sigma"), n.iter = 11000)

# Plot the fit --> these plots are pretty easy to interpret, i can help if needed
plot(samples)

# apply summary to df and write to file so you can use it for your write up
# view model summary, plot fit, and check diagnostics
summary(fit)
fit_summary = summary(fit)
write.csv(fit_summary, "Sandy fit summary_JAGS.csv", row.names = FALSE)


# Plot final model-----------
# extract cp and find closest date for plotting
{
  cp_1 = fit_summary[1,2] # extract
  print(cp_1) # view
  differences <- abs(Sandy$time - cp_1) # calculate closest sqrtVALUE to cp_1
  closest_index <- which.min(differences) # find the index of this value 
  closest_date <- Sandy$DATE[closest_index] # Get the corresponding DATE value that aligns with this index
  closest_date <- format(as.POSIXct(closest_date, format = "%d/%m/%Y %H:%M"), "%d/%m/%Y") # remove minutes and seconds as they are meaningless for plot
  print(closest_date) #check
  # Plot with CP lines & PC values
  PC95 <- sqrt(5)
  PC99 <- sqrt(1)
}

SandyPlot <- plot(fit, q_predict = TRUE) + ggtitle("Sandy Creek Change Point") + 
  labs(y= "sqrt PAF (%)", x = "Numeric date") + 
  geom_vline(xintercept=cp_1, linetype='solid', col='red') +
  annotate("text", x=1840, y=9, label=closest_date, size=4, color="dark blue", angle = 90) +
  geom_hline(yintercept=PC95, linetype='dashed', col='blue') +
  annotate("text", x=4000, y=2.4, label="PC95", size=4, color="dark blue") +
  geom_hline(yintercept=PC99, linetype='dashed', col='blue') +
  annotate("text", x=4000, y=1.2, label="PC99", size=4, color="dark blue") 
SandyPlot
ggsave("Sandy CP with seasonality.png", width = 16, height = 9, dpi = 1000)

# > 2 or more CP rJAGS trigonomic model to account for seasonality --------------
# THIS IS THE CORRECT JAGS MODEL TO USE!

# clear cache --------
rm(list = ls()) # Clear all objects from the global environment so they dont interfere with the new model
gc() # Free up memory and report the memory usage
library(rjags)
library(mcp)
library(fitdistrplus)

# reload data, Clean df and create additional variables -----------
Sandy <- read.csv("msPAF22_Sandy_CN.csv", header=T)
{
  Sandy$DATE_conv <- as.Date(Sandy$DATE, format="%d/%m/%Y",  tz = "Australia/Brisbane") # fix DATE format
  Sandy$Month <- month(Sandy$DATE_conv) # extract 'Month'
  Sandy$Season <- ifelse(Sandy$Month %in% c(11, 12, 1, 2, 3, 4), 'Wet', 'Dry') # create 'Season' variable (i applied Nov-April as standardised wet season)
  Sandy$Season <- as.factor(Sandy$Season)
  financial_year_months <- c(7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6) # order months so that we go from 1 July - 30 June in plots, same as Sampling.Year
  Sandy$Month <- factor(Sandy$Month, levels = financial_year_months)
  Sandy$sqrtVALUE <- sqrt(Sandy$Daily.Ave.PAF) # create sqrt of PAF
  str(Sandy) # check
}

# set up simpler trigonomic rjags model that accounts for seasonality----------
model = list(
  sqrtVALUE ~ 1 + sin(time), # Using only sine for seasonality
  ~ 1 + time, # first CP
  ~ 1 + time, # second CP
  ~ 1 + time  # third CP (if you only want 2 CPs then delete this line and the comma from the line above)
)

ex = mcp_example("trigonometric")
fit = mcp(model, cores = 3, data = Sandy)

# extract and plot residuals to check model fit
residuals <- residuals(fit) # extract residuals (leftover error that cant be accounted for) from model
fit.norm <- fitdist(residuals$residuals, "norm") # if there is a normal distribution there is no excess patterning in the residuals = model has fit well
plot(fit.norm)

# run the same model but in CODA to extract and plot diagnostics
# Define the model
library(coda)
# Define the model (delete row 288 if you only want 2 CPs)
model_string <- "
model {
  for (i in 1:N) {
    sqrtVALUE[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta1 * time[i] * step(time[i] - cp1) + beta2 * time[i] * step(time[i] - cp2) + beta3 * time[i] * step(time[i] - cp3) + gamma * sin(time[i])
  }
  alpha ~ dnorm(0, 0.001)
  beta1 ~ dnorm(0, 0.001)
  beta2 ~ dnorm(0, 0.001)
  beta3 ~ dnorm(0, 0.001)
  gamma ~ dnorm(0, 0.001)
  cp1 ~ dunif(min(time), max(time))
  cp2 ~ dunif(min(time), max(time))
  cp3 ~ dunif(min(time), max(time))
  tau <- pow(sigma, -2)
  sigma ~ dunif(0, 100)
}
"
# Prepare data
data_list <- list(
  sqrtVALUE = Sandy$sqrtVALUE,
  time = Sandy$time,
  N = nrow(Sandy)
)

# Initialize the model
model <- jags.model(textConnection(model_string), data = data_list, n.chains = 3)

# Update the model (burn-in)
update(model, n.iter = 11000)

# Sample from the posterior (remove cp3 if not needed)
samples <- coda.samples(model, variable.names = c("alpha", "beta1", "beta2", "beta3", "gamma", "cp1", "cp2", "cp3", "sigma"), n.iter = 11000)

# Plot the fit --> these plots are pretty easy to interpret, i can help if needed
layout(matrix(1:10, nrow = 5, ncol = 2)) # first Set up the plotting window to have 5 rows and 2 columns
plot(samples) # Plot the fit
par(mfrow = c(1, 1)) # Reset to default single plot layout

# apply summary to df and write to file so you can use it for your write up
# view model summary, plot fit, and check diagnostics
summary(fit)
fit_summary = summary(fit)
write.csv(fit_summary, "Sandy fit summary_JAGS.csv", row.names = FALSE)


# Plot final model-----------
# extract cps and find closest date for plotting
# remove cp3 if not needed
{
  cp_1 = fit_summary[1,2] # extract CP1
  cp_2 = fit_summary[2,2] # extract CP2
  cp_3 = fit_summary[3,2] # extract CP3 (delete this line if you only want 2 CPs)
  print(cp_1) # view
  print(cp_2) # view
  print(cp_3) # view
  differences1 <- abs(Sandy$time - cp_1) # calculate closest sqrtVALUE to cp_1
  differences2 <- abs(Sandy$time - cp_2) # calculate closest sqrtVALUE to cp_2
  differences3 <- abs(Sandy$time - cp_3) # calculate closest sqrtVALUE to cp_3
  closest_index1 <- which.min(differences1) # find the index of this value cp_1
  closest_index2 <- which.min(differences2) # find the index of this value cp_2
  closest_index3 <- which.min(differences3) # find the index of this value cp_3
  closest_date1 <- Sandy$DATE[closest_index1] # Get the corresponding DATE value that aligns with this index cp_1
  closest_date2 <- Sandy$DATE[closest_index2] # Get the corresponding DATE value that aligns with this index cp_2
  closest_date3 <- Sandy$DATE[closest_index3] # Get the corresponding DATE value that aligns with this index cp_3
  closest_date1 <- format(as.POSIXct(closest_date1, format = "%d/%m/%Y %H:%M"), "%d/%m/%Y") # remove minutes and seconds as they are meaningless for plot
  closest_date2 <- format(as.POSIXct(closest_date2, format = "%d/%m/%Y %H:%M"), "%d/%m/%Y")
  closest_date3 <- format(as.POSIXct(closest_date3, format = "%d/%m/%Y %H:%M"), "%d/%m/%Y")
  print(closest_date1) #check
  print(closest_date2) #check
  print(closest_date3) #check
  # Plot with CP lines & PC values
  PC95 <- sqrt(5)
  PC99 <- sqrt(1)
}

SandyPlot <- plot(fit, q_predict = TRUE) + ggtitle("Sandy Creek Change Point") + 
  labs(y= "sqrt PAF (%)", x = "Numeric date") + 
  geom_vline(xintercept=cp_1, linetype='solid', col='red') +
  annotate("text", x=1840, y=9, label=closest_date, size=4, color="dark blue", angle = 90) +
  geom_hline(yintercept=PC95, linetype='dashed', col='blue') +
  annotate("text", x=4000, y=2.4, label="PC95", size=4, color="dark blue") +
  geom_hline(yintercept=PC99, linetype='dashed', col='blue') +
  annotate("text", x=4000, y=1.2, label="PC99", size=4, color="dark blue") 
SandyPlot
ggsave("Sandy CP with seasonality.png", width = 16, height = 9, dpi = 1000)





