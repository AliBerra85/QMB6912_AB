##################################################
#
# QMB 6912 Capstone Project
# PMSM-BA program
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# February 22, 2022
#
##################################################
#
# Sample code for the problem sets in the course QMB 6912,
# Capstone Project in Business Analytics, for the PMSM-BA
# program.
# This script analyzes the potential for transforming the
# dependent variable with the Box-Cox transformation.
#
# Dependencies:
#   MASS library for the Box-Cox Transformation
#   car library for the Box-Cox Transformation
#   EnvStats for another version of the Box-Cox Transformation
#     (this package is not recommended but is included for completeness.)
#
##################################################

##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S22/demo_06/FlyReel_Box_Cox'
# setwd(wd_path)


# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
tab_dir <- 'Tables'


##################################################
# Load libraries
##################################################

# Packages with the Box-Cox Transformation
library(MASS)
library(car)
#install.packages("car")
#install.packages("EnvStats")
library(EnvStats)
library(xtable)
library(texreg)

##################################################
# Load Data
##################################################

# Set parameters for flyreel dataset.
in_file_name <- sprintf('%s/%s', data_dir, "UsedTrucks.dat")
Truck_col_names <- c('type', 'pauc', 'pret', 'mileage', 'make',
                     'year', 'damage', 'dealer', 'ror', 'cost')

# Load data.
Truck <- read.table(file = in_file_name, header = FALSE,
                    col.names = Truck_col_names)

# Initial inspection.
print("Summary of UsedTrucks.dat dataset:")
print(summary(Truck))



##################################################
# Generating Variables
##################################################

# Set categorical variables as factors.
cat_var_list <- colnames(Truck)[lapply(Truck, class) == "character"]
for (var_name in cat_var_list) {
  Truck[, var_name] <- as.factor(Truck[, var_name])
}

# Initial inspection.
print('Truck Dataset with Categorical Factors:')
print(summary(Truck))



# Create a density variable.
colnames(Truck)
flyreels[, 'Volume'] <- pi * (flyreels[, 'Diameter']/2)^2 * flyreels[, 'Width']
flyreels[, 'Density'] <- flyreels[, 'Weight'] / flyreels[, 'Volume']

# Create logarithm of dependent variable.
Truck[, 'log_ror'] <- log(Truck[, 'ror'])




##################################################
# Transforming the Dependent Variable
##################################################

# In Problem Set 4, we investigated the distribution of
# our dependent variable,.
# We analyzed the distribution of the prices in levels
# and by taking logarithms.
# Now we will employ the Box-Cox transformation
# to decide between these specifications.
# First, we can analyze the distributions
# to determine whether they are normally distributed.



##################################################
# Kernel-smoothed pdf of fly reel price.
print('Plotting kernel-smoothed pdf')
print('of fly reel ror')
##################################################

density_ror <- density(Truck[, 'ror'])
fig_file_name <- 'density_ror.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(density_ror,
     main = 'Kernel-Smoothed pdf of Truck ror',
     xlab = 'ror',
     col = 'blue', lwd = 3)
dev.off()




##################################################
# Kernel-smoothed pdf of the natural logarithm of price.
print('Plotting kernel-smoothed pdf')
print('of the natural logarithm of ror')
##################################################

density_log_ror <- density(Truck[, 'log_ror'])
fig_file_name <- 'density_log_ror.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(density_log_ror,
     main = 'Kernel-Smoothed pdf of the Natural Log. of Truck Ror',
     xlab = 'Logarithm of Ror',
     col = 'blue', lwd = 3)
dev.off()









##################################################
# Defining a Univariate Likelihood Function
##################################################

#--------------------------------------------------
# Coding and optimizing own function
#--------------------------------------------------

# First, define a function that performs a
# Box-Cox transformation.

# Note: "lambda" does not mean what it does in Python.
# You can make functions on-the-fly with the "function" function in R.
# For this reason, I call the function by a different name, Lambda_Price.

# Box-Cox transformation.
Lambda_ror <- function(ror, lambda) {
  
  if (lambda == 0) {
    return(log(ror))
  } else {
    return((ror^lambda - 1)/lambda)
  }
  
}

log_like_uni <- function(ror, lambda) {
  
  n <- length(ror)
  lambda_ror <- Lambda_ror(ror, lambda)
  mu_0_lambda <- mean(lambda_ror)
  sigma_2_lambda <- sum((lambda_ror - mu_0_lambda)^2)/n
  
  like <- - n/2*log(2*pi*sigma_2_lambda)
  like <- like - 1/2/sigma_2_lambda*sum((lambda_ror - mu_0_lambda)^2)
  like <- like + (lambda - 1)*sum(log(ror))
  
  return(like)
  
}

# Calculate values of the log-likelihood function.
lambda_grid <- seq(-1, 2.5, by = 0.001)
like_grid <- 0*lambda_grid
for (lambda_num in 1:length(lambda_grid)) {
  like_grid[lambda_num] <- log_like_uni(ror = Truck[, 'ror'],
                                        lambda = lambda_grid[lambda_num])
}

# Find the MLE, the unrestricted estimate.
lambda_hat <- lambda_grid[which.max(like_grid)]
like_MLE <- max(like_grid)
# Check:
# like_MLE == log_like_uni(price = flyreels[, 'Price'], lambda = lambda_hat)


# Generate new dependent variable with results from Problem Set 6.
Truck[, 'Trans_ror'] <- Lambda_ror(ror = Truck[, 'ror'],
                                          lambda = lambda_hat)


#Now plot the density of this transformed variable.

# Kernel-smoothed pdf of the Box-Cox Transformation of price.
density_trans_ror <- density(Truck[, 'Trans_ror'])
fig_file_name <- 'density_trans_ror.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(density_trans_ror,
     main = 'Kernel-Smoothed pdf of the Box-Cox Transformation of Fly Reel Prices',
     xlab = 'Logarithm of Price',
     col = 'blue',
     lwd = 3)
dev.off()

# Regression on (Un-transformed)

#--------------------------------------------------

var_list <- c('type', 'pauc', 'pret')

target_var <- 'ror'
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_1 <- lm(data = Truck, formula = lm_fmla)
print(summary(lm_model_1))


# Regression on Logarithm of ror
target_var <- 'log_ror'
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_2 <- lm(data = Truck, formula = lm_fmla)
print(summary(lm_model_2))


# Regression on Box-Cox transformation of ror

target_var <- 'Trans_ror'
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_3 <- lm(data = Truck, formula = lm_fmla)
print(summary(lm_model_3))


# Print the output to a LaTeX file.
#--------------------------------------------------



tab_file_name <- 'reg_by_dep_var.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_model_1,
                lm_model_2,
                lm_model_3),
       file = out_file_name,
       label = 'tab:reg_by_dep_var',
       caption = "Regression Models with Different Dependent Variables")


##################################################
##################################################

##################################################
# End
##################################################