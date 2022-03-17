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
install.packages("car")
install.packages("EnvStats")
library(EnvStats)



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


#--------------------------------------------------
# Compare Prices and Transformation for Normality
print(c('Calculating Q-Q Plots of Dependent Variable.'))
#--------------------------------------------------


# To compare these to the normal distribution,
# we can draw a Q-Q plot, plotting the quantiles of
# each on a scatterplot.


# Plot normal QQ plot for Fly Reel Prices.
fig_file_name <- 'qq_ror.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
qqnorm(Truck[, 'ror'],
       main = 'Q-Q Plot of Fly Reel Prices') # ,
qqline(Truck[, 'ror'],
       col = 'blue', lwd = 3) # ,
dev.off()

# Plot normal QQ plot for the log of Fly Reel Prices.
fig_file_name <- 'qq_log_ror.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
qqnorm(Truck[, 'log_ror'],
       main = 'Q-Q Plot of the Log. of Truck ror') # ,
qqline(Truck[, 'log_ror'],
       col = 'blue', lwd = 3) # ,
dev.off()



##################################################
# Calculating Box-Cox Transformation
# for Univariate Likelihood Function.
print(c('Calculating Box-Cox Transformation',
        'for Univariate Likelihood Function.'))
##################################################





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

# Calculate restricted likelihood values for mu = 0, 1.
like_mu_0 <- log_like_uni(ror = Truck[, 'ror'], lambda = 0)
like_mu_1 <- log_like_uni(ror = Truck[, 'ror'], lambda = 1)




# Plot the log-likelihood function.
fig_file_name <- 'box_cox_loglike_uni.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(x = lambda_grid, y = like_grid,
     type = 'l',
     main = 'Log-likelihood Function',
     xlab = 'Lambda',
     ylab = 'Log-likelihood',
     col = 'blue', lwd = 3)
points(c(0, 1), c(like_mu_0, like_mu_1),
       col = 'red', lwd = 2)
points(lambda_hat, like_MLE,
       col = 'red', lwd = 3)
dev.off()

#--------------------------------------------------
# Testing for appropriate transformation
#--------------------------------------------------

# Now consider the statistical properties of these estimates.

# Calculate likelihood ratio statistics.
LR_stat_0 <- - 2*(like_mu_0 - like_MLE)
print(LR_stat_0)
LR_stat_1 <- - 2*(like_mu_1 - like_MLE)
print(LR_stat_1)


# Compare to quantile of chi-squared distribution with 1 degree of freedom.
LR_cv_5 <- qchisq(p = 0.95, df = 1)
print(LR_cv_5)

# Calculate p-values for these tests.
p_value_0 <- 1 - pchisq(q = LR_stat_0, df = 1)
print(p_value_0)
p_value_1 <- 1 - pchisq(q = LR_stat_1, df = 1)
print(p_value_1)
# Statistically, this is evidence to reject them both.
# This suggests using the transformation at the MLE.


#--------------------------------------------------
# Using the MASS package
#--------------------------------------------------

# As an illustration, we calculated
# the likelihood ourselves.
# However, there exist other packages
# to output the estimation results for
# an optimal Box-Cox transformation.


# Use the function from the MASS package.
# In the MASS package, the notation is the same as for a linear model.
summary(lm(ror ~ 1, data = Truck))
# Note the package::function_name() notation here because
# the boxcox call is ambiguous (several boxcox functions are loaded
# each one from a different package).
bc_grid_MASS <- MASS::boxcox(ror ~ 1,
                             data = Truck,
                             lambda = lambda_grid)
# Find the MLE.
max_lambda_MASS <- bc_grid_MASS$x[which.max(bc_grid_MASS$y)]

# Plot from the model object.
fig_file_name <- 'plot_like_MASS.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(bc_grid_MASS$x, bc_grid_MASS$y,
     type = 'l',
     main = 'Log-likelihood Function (from MASS package)',
     xlab = 'Lambda',
     ylab = 'Log-likelihood',
     col = 'blue', lwd = 3)
lines(x = c(max_lambda_MASS, max_lambda_MASS),
      y = c(min(bc_grid_MASS$y), max(bc_grid_MASS$y)),
      lty = 'dashed')
dev.off()



#--------------------------------------------------
# Using the car package
#--------------------------------------------------

# Use the function from the car package.

# Plot from the model object.
fig_file_name <- 'plot_like_car.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
bc_grid_car <- car::boxCox(object = lm(data = Truck,
                                       formula = ror ~ 1),
                           lambda = lambda_grid)
dev.off()


#--------------------------------------------------
# Using the EnvStats package
#--------------------------------------------------

bc_grid_ES <- EnvStats::boxcox(x = Truck[, 'ror'],
                               lambda = lambda_grid,
                               optimize = FALSE,
                               objective.name = "Log-Likelihood")


# Find optimal value of lambda.
bc_grid_ES_opt <- EnvStats::boxcox(x = Truck[, 'ror'],
                                   lambda = range(lambda_grid),
                                   optimize = TRUE,
                                   objective.name = "Log-Likelihood")

bc_grid_ES_opt$lambda


fig_file_name <- 'plot_like_EnvStats.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(bc_grid_ES$lambda, bc_grid_ES$objective,
     type = 'l',
     main = 'Log-likelihood Function (from EnvStats package)',
     xlab = 'Lambda',
     ylab = 'Log-likelihood')
lines(x = c(bc_grid_ES_opt$lambda, bc_grid_ES_opt$lambda),
      y = c(min(bc_grid_ES$objective), max(bc_grid_ES$objective)),
      lty = 'dashed')
dev.off()




#--------------------------------------------------
# Compare Prices and Transformation for Normality
#--------------------------------------------------


# We already plotted normal QQ plot for Fly Reel Prices.


# Generate new dependent variable with results from estimates above.
Truck[, 'Trans_ror'] <- Lambda_ror(ror = Truck[, 'ror'],
                                          lambda = lambda_hat)

# Plot normal QQ plot for Transformed Fly Reel Prices.
fig_file_name <- 'qq_boxcox.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
qqnorm(Truck[, 'Trans_ror'],
       main = 'Q-Q Plot of the Log. of Truck ror') # ,
qqline(Truck[, 'Trans_ror'],
       col = 'blue', lwd = 3) # ,
dev.off()

# From a purely statistical perspective,
# this provides evidence that the prices are best modeled with the transformation
# at the optimal lambda_hat = 0.43.
# From a practical point of view, however,
# it is still an open question whether this
# added complexity is warranted when other variables are added to the model.


# Add variables to regression equation.
# Now modeling the residuals.
bc_grid_MASS <- MASS::boxcox(ror ~ Country,
                             data = Truck,
                             lambda = lambda_grid)
# Find the MLE.
max_lambda_MASS <- bc_grid_MASS$x[which.max(bc_grid_MASS$y)]

# Plot from the model object.
plot(bc_grid_MASS$x, bc_grid_MASS$y,
     type = 'l',
     main = 'Log-likelihood Function (from MASS package)',
     xlab = 'Lambda',
     ylab = 'Log-likelihood',
     col = 'blue', lwd = 3)
lines(x = c(max_lambda_MASS, max_lambda_MASS),
      y = c(min(bc_grid_MASS$y), max(bc_grid_MASS$y)),
      lty = 'dashed')



##################################################
# End
##################################################