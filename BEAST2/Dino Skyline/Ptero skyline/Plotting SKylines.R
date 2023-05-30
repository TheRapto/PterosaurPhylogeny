#Bethany Allen
#Code to plot exponential coalescent and fossilised-birth-death skylines

library(tidyverse)
library(coda)

###Plotting the exponential coalescent skyline

# Navigate to Session > Set Working Directory > Choose Directory (on RStudio)
# or change file name to the full path to the log file
#(Use "dinosaur_coal_final.log" if you used our pre-cooked XML)
coal_file <- "Ptero_coal.log"

#Read in coalescent log and trim 10% burn-in
coalescent <- read.table(coal_file, header = T) %>% slice_tail(prop = 0.9)

#Tell coda that this is an mcmc file, and calculate 95% HPD values
coalescent_mcmc <- as.mcmc(coalescent)
summary_data <- as.data.frame(HPDinterval(coalescent_mcmc))

#Add median values to data frame
summary_data$median <- apply(coalescent, 2, median)

#Select growth rate parameters
diversification_data <- summary_data[6:9,]

#Add the interval names
diversification_data$interval <- c("Rhaetian", "Norian",
                                   "Carnian", "Ladinian")

#Ensure that the time intervals plot in the correct order
diversification_data$interval <- factor(diversification_data$interval,
                                        levels = c("Ladinian", "Carnian",
                                                   "Norian",
                                                   "Rheatian"))
#Plot diversification skyline as error bars
ggplot(data = diversification_data, aes(x = interval, y = median, ymin = lower,
                                        ymax = upper)) +
  geom_point(size = 1.5) +
  geom_errorbar(linewidth = 1, width = 0.5) +
  scale_colour_manual(values = c("black")) +
  geom_hline(aes(yintercept = 0), colour = "black") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(x = "Interval", y = "Diversification rate") +
  theme_classic(base_size = 17)

#Plot diversification skyline as a ribbon plot
ages <- seq.int(252, 201.3)
interval <- c(rep("Ladinian", ((252 - 237) + 1)),
              rep("Carnian", ((236.9 - 227) + 1)),
              rep("Norian", ((226.9 - 208.5) + 1)),
              rep("Rheatian", ((208.4 - 201.3) + 1)))
age_table <- as.data.frame(cbind(ages, interval))
to_plot <- left_join(age_table, diversification_data, by = "interval")
to_plot$ages <- as.numeric(to_plot$ages)

ggplot(to_plot) +
  geom_ribbon(aes(x = ages, ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(x = ages, y = median)) +
  scale_x_reverse() +
  geom_hline(aes(yintercept = 0), colour = "black") +
  xlab("Age (Ma)") + ylab("Diversification rate") +
  theme_classic(base_size = 17)

#Select estimated species diversity at end of time interval
population <- pull(coalescent, "ePopSize")

#Tell coda that this is an mcmc file, and calculate 95% HPD values
pop_mcmc <- as.mcmc(population)
pop_data <- as.data.frame(HPDinterval(pop_mcmc))

#Add median values to data frame
pop_data$median <- median(population)

#Display estimated species diversity and HPDs
print(pop_data)


###Plotting the fossilised-birth-death skyline

# Navigate to Session > Set Working Directory > Choose Directory (on RStudio)
# or change file name to the full path to the log file
#(Use "dinosaur_BDSKY_final.log" if you used our pre-cooked XML)
fbd_file <- "Ptero_BDSKY.log"

#Read in coalescent log and trim 10% burn-in
fbd <- read.table(fbd_file, header = T) %>% slice_tail(prop = 0.9)

#Calculate diversification and turnover
birth_rates <- select(fbd, starts_with("birthRate"))
death_rates <- select(fbd, starts_with("deathRate"))

div_rates <- birth_rates - death_rates
colnames(div_rates) <- paste0("divRate.",
                              seq(1:ncol(div_rates)))

#Tell coda that this is an mcmc file, and calculate 95% HPD values
div_mcmc <- as.mcmc(div_rates)
div_data <- as.data.frame(HPDinterval(div_mcmc))

#Add median values to data frame
div_data$median <- apply(div_rates, 2, median)

#Add interval names
div_data$interval <- c("Ladinian", "Carnian",
                       "Norian",
                       "Rheatian")

#Select sampling estimates from log
samp_log <- select(fbd, starts_with("samplingBDS"))

#Tell coda that this is an mcmc file, and calculate 95% HPD values
samp_mcmc <- as.mcmc(samp_log)
samp_data <- as.data.frame(HPDinterval(samp_mcmc))

#Add median values to data frame
samp_data$median <- apply(samp_log, 2, median)

#Add interval names
samp_data$interval <- c("Ladinian", "Carnian",
                        "Norian",
                        "Rheatian")

#Ensure that the time intervals plot in the correct order
div_data$interval <- factor(div_data$interval,
                            levels = c("Ladinian", "Carnian",
                                       "Norian",
                                       "Rheatian"))

samp_data$interval <- factor(samp_data$interval,
                             levels = c("Ladinian", "Carnian",
                                        "Norian",
                                        "Rheatian"))

#Plot skylines as error bars
ggplot(data = div_data, aes(x = interval, y = median, ymin = lower,
                            ymax = upper)) +
  geom_point(size = 1.5) +
  geom_errorbar(linewidth = 1, width = 0.5) +
  scale_colour_manual(values = c("black")) +
  geom_hline(aes(yintercept = 0), colour = "black") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(x = "Interval", y = "Diversification rate") +
  theme_classic(base_size = 17)

ggplot(data = samp_data, aes(x = interval, y = median, ymin = lower,
                             ymax = upper)) +
  geom_point(size = 1.5) +
  geom_errorbar(linewidth = 1, width = 0.5) +
  scale_colour_manual(values = c("black")) +
  geom_hline(aes(yintercept = 0), colour = "black") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(x = "Interval", y = "Sampling rate") +
  theme_classic(base_size = 17)

#Plot skylines as a ribbon plot
ages <- seq.int(252, 201.3)
interval <- c(rep("Ladinian", ((252 - 237) + 1)),
              rep("Carnian", ((236.9 - 227) + 1)),
              rep("Norian", ((226.9 - 208.5) + 1)),
              rep("Rheatian", ((208.4 - 201.3) + 1)))
age_table <- as.data.frame(cbind(ages, interval))

div_plot <- left_join(age_table, div_data, by = "interval")
div_plot$ages <- as.numeric(div_plot$ages)
ggplot(div_plot) +
  geom_ribbon(aes(x = ages, ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(x = ages, y = median)) +
  scale_x_reverse() +
  geom_hline(aes(yintercept = 0), colour = "black") +
  xlab("Age (Ma)") + ylab("Diversification rate") +
  theme_classic(base_size = 17)

samp_plot <- left_join(age_table, samp_data, by = "interval")
samp_plot$ages <- as.numeric(samp_plot$ages)
ggplot(samp_plot) +
  geom_ribbon(aes(x = ages, ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(x = ages, y = median)) +
  scale_x_reverse() +
  geom_hline(aes(yintercept = 0), colour = "black") +
  xlab("Age (Ma)") + ylab("Sampling rate") +
  theme_classic(base_size = 17)

#Select origin data as an mcmc file, and calculate 95% HPD values
origin_mcmc <- as.mcmc(pull(fbd, "origin"))
origin_data <- as.data.frame(HPDinterval(origin_mcmc))

#Add median value to data frame
origin_data$median <- median(fbd$origin)

#Convert to time before present, rather than time before K-Pg boundary
origin_data <- origin_data + 201.3

#Display estimated origin and HPDs
print(origin_data)

