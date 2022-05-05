# Clear workspace.
rm(list=ls(all=TRUE))

# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
tab_dir <- 'Tables'
# Libraries to print tables of regression results.
library(xtable)
library(texreg)


# library sampleSelection to estimate models
# with sample selection.
library(sampleSelection)
##################################################
# Loading the Data
##################################################
car_col_names <- c('type', 'pauc', 'pret', 'mileage', 'make',
                     'year', 'damage', 'dealer', 'ror', 'cost')
in_file_name <- sprintf('%s/%s', data_dir, 'UsedCars.dat')
DF <- read.table(file = in_file_name,header = FALSE,
                        col.names = car_col_names)

# Inspect the contents.
print('Summary of car_sales Dataset:')
print(summary(DF))
str(DF)
# Make sure there are no problems with the data.

DF$type <- as.factor(DF$type)
DF$make <- factor(DF$make)
#,levels = C(1,2,3,4,5,6,7,8,9))
 #                     labels = c("Ford", "Ford","Chevrolet", "Dodge", "GM", "Toyota","Nissan","Subaru", "others") )

DF$age <- 2020 - DF$year
DF$dealer <- as.factor(DF$dealer)
DF$damage <- as.factor(DF$damage)
str(DF)
#--------------------------------------------------
# Summarize numeric variables.
#--------------------------------------------------

print('Summarizing Numeric Variables')

print('Summary by type of Manufacture:')



##################################################
# Plot EDF in base R and output to figure.
print('Plotting ECDF.')
##################################################

# 
# 

library(xtable)

library(lattice)
library(latticeExtra)
#################################################################
ecdf_ror <- ecdf(DF[,'ror'])

vals <- data.frame(r1=DF$ror,
                   r2=DF$pret,
                   r3=DF$pauc)



ecdfplot(~ r1 + r2 + r3, data=vals, auto.key=list(space='right'))



fig_file_name <- 'ecdf_prices.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
pdf(out_file_name)
plot(ecdf_ror,
     main = 'Empirical Cumulative Distribution Function of ROR ',
     xlab = 'ror',
     ylab = 'Empirical C.D.F.')
dev.off()




fig_file_name <- 'ecdf_ror.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(ecdf_ror,
     main = 'Empirical Cumulative Distribution Function of ror ',
     xlab = 'ror',
     ylab = 'Empirical C.D.F.')
dev.off()

##################################################
# Relative histogram of price.
print('Ploting relative histogram of ROR.')
##################################################

fig_file_name <- 'hist_ror.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
hist(DF[, 'ror'],
     main = 'Relative Histogram of ROR',
     xlab = 'pret',
     probability = TRUE)
dev.off()


##################################################
# Kernel-smoothed pdf of the natural logarithm of price.
print('Plotting kernel-smoothed pdf')
print('of the ror.')
##################################################

density_log_price <- density((DF[, 'ror']))
fig_file_name <- 'density_ror.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
plot(density_log_price,
     main = 'Kernel-smoothed pdf of ROR',
     xlab = 'Price')
dev.off()



##################################################
# End
##################################################
##################################################
# Generating Scatterplot Matrices.
print('Generating Scatterplot Matrices.')
##################################################
str(DF)

# Create scatterplots of numeric variables.
splom_var_list <- c('pauc', 'pret', 'mileage',
                     'ror', 'cost' )
#dealer, age, level of damge
# fig_file_name <- 'slpom_num_only.eps'
fig_file_name <- 'slpom_num_only.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
splom(DF[, splom_var_list])
dev.off()

#dealer, age, level of damge
# Add some categorical variables to scatterplots.
splom_var_list <- c('type', 'pauc', 'pret', 'mileage',
                    'damage', 'ror', 'cost','dealer', 'age' )

# fig_file_name <- 'slpom_with_cat.eps'
fig_file_name <- 'slpom_with_cat.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
splom(DF[, splom_var_list])
dev.off()


#cheking the relationship between mileage and age 
library("tidyverse")

fig_file_name <- 'age_mileage_plot.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)

# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
ggplot(head(DF,nrow(DF)), aes(x =age, y =mileage, na.rm =TRUE)) +
  geom_point(data = DF, aes(x =age, y =mileage, na.rm =TRUE))
dev.off()

#cheking the relationship between pret and age 
fig_file_name <- 'pert_age_plot.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)

# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
ggplot(head(DF,nrow(DF)), aes(x =age, y =pret, na.rm =TRUE)) +
  geom_point(data = DF, aes(x =age, y =pret, na.rm =TRUE))
dev.off()

#cheking the relationship between pauc and age 
fig_file_name <- 'pauc_age_plot.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)

# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
ggplot(head(DF,nrow(DF)), aes(x =age, y =pauc, na.rm =TRUE)) +
  geom_point(data = DF, aes(x =age, y =pauc, na.rm =TRUE))
dev.off()

#####################################################################
#####################################################################
#Box_COx Transformation 
library(MASS)
library(car)
library(EnvStats)

##################################################
# Transforming the Dependent Variable
##################################################


##################################################
# Kernel-smoothed pdf of fly reel price.
print('Plotting kernel-smoothed pdf')
print('of car ror')
##################################################

density_ror <- density(DF[, 'ror'])
fig_file_name <- 'density_ror.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(density_ror,
     main = 'Kernel-Smoothed pdf of car ror',
     xlab = 'ror',
     col = 'blue', lwd = 3)
dev.off()   

##################################################
# Kernel-smoothed pdf of the natural logarithm of price.
print('Plotting kernel-smoothed pdf')
print('of the natural logarithm of ror')
##################################################
DF$log_ror <- log(DF$ror)

density_log_ror <- density(DF[, 'log_ror'])
fig_file_name <- 'density_log_ror.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(density_log_ror,
     main = 'Kernel-Smoothed pdf of the Natural Log. of car Ror',
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
qqnorm(DF[, 'ror'],
       main = 'Q-Q Plot of ror') # ,
qqline(DF[, 'ror'],
       col = 'blue', lwd = 3) # ,
dev.off()

# Plot normal QQ plot for the log of Fly Reel Prices.
fig_file_name <- 'qq_log_ror.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
qqnorm(DF[, 'log_ror'],
       main = 'Q-Q Plot of the Log. of car ror') # ,
qqline(DF[, 'log_ror'],
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
  like_grid[lambda_num] <- log_like_uni(ror = DF[, 'ror'],
                                        lambda = lambda_grid[lambda_num])
}

# Find the MLE, the unrestricted estimate.
lambda_hat <- lambda_grid[which.max(like_grid)]
like_MLE <- max(like_grid)
# Check:
# like_MLE == log_like_uni(price = flyreels[, 'Price'], lambda = lambda_hat)

# Calculate restricted likelihood values for mu = 0, 1.
like_mu_0 <- log_like_uni(ror = DF[, 'ror'], lambda = 0)
like_mu_1 <- log_like_uni(ror = DF[, 'ror'], lambda = 1)




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
summary(lm(ror ~ 1, data = DF))
# Note the package::function_name() notation here because
# the boxcox call is ambiguous (several boxcox functions are loaded
# each one from a different package).
bc_grid_MASS <- MASS::boxcox(ror ~ 1,
                             data = DF,
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
bc_grid_car <- car::boxCox(object = lm(data = DF,
                                       formula = ror ~ 1),
                           lambda = lambda_grid)
dev.off()


#--------------------------------------------------
# Using the EnvStats package
#--------------------------------------------------

bc_grid_ES <- EnvStats::boxcox(x = DF[, 'ror'],
                               lambda = lambda_grid,
                               optimize = FALSE,
                               objective.name = "Log-Likelihood")


# Find optimal value of lambda.
bc_grid_ES_opt <- EnvStats::boxcox(x = DF[, 'ror'],
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
DF[, 'Trans_ror'] <- Lambda_ror(ror = DF[, 'ror'],
                                   lambda = lambda_hat)

# Plot normal QQ plot for Transformed Fly Reel Prices.
fig_file_name <- 'qq_boxcox.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
qqnorm(DF[, 'Trans_ror'],
       main = 'Q-Q Plot of the Log. of CAR ror') # ,
qqline(DF[, 'Trans_ror'],
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
bc_grid_MASS <- MASS::boxcox(ror ~ type,
                             data = DF,
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

# Regression on (Un-transformed)

#--------------------------------------------------
str(DF)
var_list <- c('mileage', 'make', 'damage')

target_var <- 'ror'
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_1 <- lm(data = DF, formula = lm_fmla)
print(summary(lm_model_1))


# Regression on Logarithm of ror
target_var <- 'log_ror'
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_2 <- lm(data = DF, formula = lm_fmla)
print(summary(lm_model_2))


# Regression on Box-Cox transformation of ror

target_var <- 'Trans_ror'
lm_fmla <- as.formula(sprintf('%s ~ %s',
                              target_var,
                              paste(var_list, collapse = ' + ')))

lm_model_3 <- lm(data = DF, formula = lm_fmla)
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
# Create a variable squared_mileage 
# to investigate quadratic relationship of sale price to horsepower.
DF[, 'squared_mileage'] <- DF[, 'mileage']^2

str(DF)
##################################################
# Linear Regression Model
##################################################

# In Problem Set #7 I recommended the following model,
# which included a quadratic form for horsepower.

# Estimate a regression model.
lm_7 <- lm(data = DF,
           formula = ror ~
             squared_mileage +
             make +
             damage +
             dealer + mileage + age + type )

# Output the results to screen.
print(summary(lm_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_mileage.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_damage',
       caption = "Quadratic Model for CAR ROR")


##################################################
# Linear Regression Model
# Frisch-Waugh-Lovell regressions to partial out
# other variables
##################################################


# Next, consider the model without this variable.

# Estimate a regression model.
lm_no_mlg <- lm(data = DF,
               formula = ror ~
                 #squared_mileage +
                 make +
                 damage +
                 dealer  + age + type )

# Output the results to screen.
print(summary(lm_no_mlg))

# Next, estimate a model for the mileage variable,
# using the other dependent variables as covariates.
# This estimates the "excess mileage" above what one
# would predict using the other characteristics of the car

# Estimate a regression model.
lm_mlg <- lm(data = DF,
            formula = mileage ~
              make +
              
              dealer + mileage + age + type )

# Output the results to screen.
print(summary(lm_mlg))

# Do the same for mileage squared.
lm_mlg_2 <- lm(data = DF,
              formula = squared_mileage ~
                make +
                
                dealer + damage + age + type )

# Output the results to screen.
print(summary(lm_mlg_2))



# Finally, estimate a model for the
# value of a tractor using only the excess horsepower variable
# as a covariate.
# This regression is performed using the residuals
# from the two regressions using the other variables as covariates.

# Generate the residuals from each model.
DF[, 'mileage_resid'] <- lm_mlg$residuals
DF[, 'mileage_2_resid'] <- lm_mlg_2$residuals
DF[, 'ror_resid_mlg'] <- lm_no_mlg$residuals

# Finally, run a regression of the tractor price residuals
# on the horsepower residuals.
# This regression uses the Frisch-Waugh-Lovell theorem
# to partial out the other variables.


# Estimate a regression model.
lm_mlg_quad_fwl <- lm(data = DF,
                     formula = ror_resid_mlg ~ -1 +
                       mileage_resid + mileage_2_resid)

# Output the results to screen.
print(summary(lm_mlg_quad_fwl))

# Notice that the coefficients on the horsepower variables
# are the same as those from the original regression.
print(summary(lm_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_mileage_fwl.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7,
                lm_no_mlg,
                lm_mlg,
                lm_mlg_2,
                lm_mlg_quad_fwl),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_mileage_fwl',
       caption = "Quadratic Model for ROR: FWL Regressions")


##################################################
# Bivariate kernel estimation
##################################################

# You have used nonparametric methods to plot a density

# We can do something similar to predict one variable
# with the others.
# We will use the above transformations of the variables
# into residuals from regressions on the other variables.

#--------------------------------------------------
# Plot parametric model for horsepower
#--------------------------------------------------


# Plot a scattergraph to focus on horsepower.

fig_file_name <- 'dev_vs_ror.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(DF[, 'mileage'],
     DF[, 'ror_resid_mlg'],
     main = 'Quadratic Model for ROR',
     xlab = 'ror',
     ylab = 'Deviation of Log ROR',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(DF[, 'mileage'],
       predict(lm_mlg_quad_fwl),
       lwd = 6, col = 'red')

dev.off()


# Plot a scattergraph to focus on excess horsepower.

fig_file_name <- 'dev_vs_ror_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(DF[, 'mileage_resid'],
     DF[, 'ror_resid_mlg'],
     main = 'Nonparametric Model for ROR',
     xlab = 'Deviation of Mlieage',
     ylab = 'Deviation of Log Ror',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(DF[, 'mileage_resid'],
       predict(lm_mlg_quad_fwl),
       lwd = 3, col = 'red')

dev.off()


#--------------------------------------------------
# Estimate and plot Nonparametric model for horsepower
#--------------------------------------------------

# The loess function is a smoothing method
# for estimating nonparametric models.
np_hp_fit_1 <- loess(ror_resid_mlg ~ mileage_resid,
                     DF)
# Calculate the predictions.
DF[, 'mileage_np'] <- np_hp_fit_1$fitted


# Add a plot of this curve to the above scattergraph.


fig_file_name <- 'dev_np_vs_mileage_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(DF[, 'mileage_resid'],
     DF[, 'ror_resid_mlg'],
     main = 'Nonparametric Model for ROR',
     xlab = 'Deviation of Mileage',
     ylab = 'Deviation of ror',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(DF[, 'mileage_resid'],
       predict(lm_mlg_quad_fwl),
       lwd = 3, col = 'red')

# Add a line for the quadratic prediction from above.
points(DF[, 'mileage_resid'],
       np_hp_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()

# The nonparametric function is slightly more curved
# but the difference is not great.
# So far, it appears that the quadratic form
# is close enough.


#--------------------------------------------------
# Alternate models with different degrees of smoothing
#--------------------------------------------------

# When we estimated probability densities,
# we adjusted the bandwidth parameter to fit
# with different degrees of smoothness.
# The loess method has a span parameter for this function.
# The default smoother span (bandwidth parameter) is 0.75.

np_hp_fit_2 <- loess(ror_resid_mlg ~ mileage_resid,
                     DF, span = 2.0)

# Rebuild the previous plot to compare this estimate.
fig_file_name <- 'dev_np_vs_mileage_dev_bw.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(DF[, 'mileage_resid'],
     DF[, 'ror_resid_mlg'],
     main = 'Nonparametric Model for ROR',
     xlab = 'Deviation of MILEAGE',
     ylab = 'Deviation of ROR',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(DF[, 'mileage_resid'],
       predict(lm_mlg_quad_fwl),
       lwd = 3, col = 'red')

# Add a line for the quadratic prediction from above.
points(DF[, 'mileage_resid'],
       np_hp_fit_1$fitted,
       lwd = 3, col = 'green')


# Add a plot of the smoother curve to the scattergraph.
points(DF[, 'mileage_resid'],
       np_hp_fit_2$fitted,
       lwd = 2, col = 'orange')
# You can see some flattening with this
# more flexible estimator.


# Try again with less smoothing.
np_hp_fit_3 <- loess(ror_resid_mlg ~ mileage_resid,
                     DF, span = 0.1)


# Add a plot of this curve to the scattergraph.
points(DF[, 'mileage_resid'],
       np_hp_fit_3$fitted,
       lwd = 2, col = 'magenta')
# Much more rough but you capture the decline
# in value for tractors with high horsepower.

dev.off()


# Ultimately, you would choose one that captures what
# is happening and don't need to show all of the curves
# that you fit during your investigation.

# In this case, we will keep the first fit.
DF[, 'mileage_np'] <- np_hp_fit_1$fitted


# Try this again on other continuous variables.



#--------------------------------------------------
# Nonparametric model for age
#--------------------------------------------------


# First, fit Frish-Waugh-Lovell regressions
# to partial out other variables
# Consider the model without the age variable.

# Estimate a regression model.
lm_no_age <- lm(data = DF,
                formula = ror ~
                  squared_mileage +
                  make +
                  damage +
                  dealer + mileage  + type )

# Output the results to screen.
print(summary(lm_no_age))

# Next, estimate a model for the age variable,
# using the other dependent variables as covariates.
# This estimates the "excess age" above what one
# would predict using the other characteristics of the tractor.

# Estimate a regression model.
lm_age <- lm(data = DF,
             formula = age ~
               squared_mileage +
               make +
               damage +
               dealer + mileage  + type )
# Output the results to screen.
print(summary(lm_age))


# Finally, estimate a model for the
# value of a tractor using only the excess age variable
# as a covariate.
# This regression is performed using the residuals
# from the two regressions using the other variables as covariates.

# Generate the residuals from each model.
DF[, 'age_resid'] <- lm_age$residuals
DF[, 'ror_resid_age'] <- lm_no_age$residuals

# Finally, run a regression of the tractor price residuals
# on the age residuals.
# Again, this regression uses the Frisch-Waugh-Lovell theorem
# to partial out the other variables.


# Estimate a regression model.
lm_age_fwl <- lm(data = DF,
                 formula = ror_resid_age ~ -1 +
                   age_resid)

# Output the results to screen.
print(summary(lm_age_fwl))

# Notice that the coefficients on the age variable
# is the same as those from the original regression.
print(summary(lm_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_age_fwl.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7,
                lm_no_age,
                lm_age,
                lm_age_fwl),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_age_fwl',
       caption = "Linear Model for Age: FWL Regressions")



# Plot a scattergraph to focus on age.

fig_file_name <- 'dev_vs_age.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(DF[, 'age'],
     DF[, 'ror_resid_age'],
     main = 'Quadratic Model for ror',
     xlab = 'Age',
     ylab = 'Deviation of ror',
     col = 'blue')

# Add a line for the linear prediction from above.
points(DF[, 'age'],
       predict(lm_age_fwl),
       lwd = 3, col = 'red')

dev.off()


# Plot a scattergraph to focus on excess age.

fig_file_name <- 'dev_vs_age_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(DF[, 'age_resid'],
     DF[, 'ror_resid_age'],
     main = 'Nonparametric Model for ror',
     xlab = 'Deviation of Age',
     ylab = 'Deviation of ror',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(DF[, 'age_resid'],
       predict(lm_age_fwl),
       lwd = 3, col = 'red')

dev.off()

# Notice that this is a straight line,
# since we have a single variable with no
# quadratic transformation.

#--------------------------------------------------
# Estimate and plot Nonparametric model for age
#--------------------------------------------------

# Use the loess function.
np_age_fit_1 <- loess(ror_resid_age ~ age_resid,
                      DF,
                      span = 0.25)
# Calculate the predictions.
DF[, 'age_np'] <- np_age_fit_1$fitted


# Add a plot of this curve to the above scattergraph.


fig_file_name <- 'dev_np_vs_age_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(DF[, 'age_resid'],
     DF[, 'ror_resid_age'],
     main = 'Nonparametric Model for ror',
     xlab = 'Deviation of Age',
     ylab = 'Deviation of ror',
     col = 'blue')

# Add a line for the linear prediction from above.
points(DF[, 'age_resid'],
       predict(lm_age_fwl),
       lwd = 3, col = 'red')

# Add a line for the nonparametric prediction.
points(DF[, 'age_resid'],
       np_age_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()


# Not much of a difference from the linear prediction.

# Try it with the remaining continuous variable.

#--------------------------------------------------
# Nonparametric model for engine hours
#--------------------------------------------------

# First, fit Frisch-Waugh-Lovell regressions
# to partial out other variables
# Consider the model without the engine hours variable.

# Estimate a regression model.
lm_no_cost <- lm(data = DF,
                formula = ror ~
                  squared_mileage +
                  make +
                  damage +
                  mileage + age+ type )

# Output the results to screen.
print(summary(lm_no_cost))

# Next, estimate a model for the engine hours variable,
# using the other dependent variables as covariates.
# This estimates the "excess engine hours" above what one
# would predict using the other characteristics of the tractor.

# Estimate a regression model.
lm_cost <- lm(data = DF,
             formula = cost ~
               squared_mileage +
               make +
               damage +
               mileage + age + type )
# Output the results to screen.
print(summary(lm_cost))


# Finally, estimate a model for the
# value of a tractor using only the excess engine hours variable
# as a covariate.
# This regression is performed using the residuals
# from the two regressions using the other variables as covariates.

# Generate the residuals from each model.
DF[, 'cost_resid'] <- lm_cost$residuals
DF[, 'nocost_resid'] <- lm_no_cost$residuals

# Finally, run a regression of the tractor price residuals
# on the engine hours residuals.
# Again, this regression uses the Frisch-Waugh-Lovell theorem
# to partial out the other variables.


# Estimate a regression model.
lm_cost_fwl <- lm(data = DF,
                 formula = nocost_resid ~ -1 +
                   cost_resid)

# Output the results to screen.
print(summary(lm_cost_fwl))

# Notice again that the coefficients on the engine hour variable
# is the same as those from the original regression.
print(summary(lm_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_eng_fwl.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7,
                lm_no_cost,
                lm_cost,
                lm_cost_fwl),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_eng_fwl',
       caption = "Linear Model for type : FWL Regressions")



# Plot a scattergraph to focus on eng.

fig_file_name <- 'dev_vs_eng.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(DF[, 'cost'],
     DF[, 'cost_resid'],
     main = 'Quadratic Model for ROR',
     xlab = 'cost',
     ylab = 'Deviation of ROR',
     col = 'blue')

# Add a line for the linear prediction from above.
points(DF[, 'cost'],
       predict(lm_cost_fwl),
       lwd = 3, col = 'red')

dev.off()

# Skip the linear model and go straight to the
# nonparametric model.

#--------------------------------------------------
# Estimate and plot Nonparametric model for engine hours
#--------------------------------------------------

# Use the loess function.
np_cost_fit_1 <- loess(nocost_resid ~ cost_resid,
                      DF,
                      span = 0.25)
# Calculate the predictions.
DF[, 'cost_np'] <- np_cost_fit_1$fitted


# Add a plot of this curve to the above scattergraph.


fig_file_name <- 'dev_np_vs_cost_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(DF[, 'cost_resid'],
     DF[, 'nocost_resid'],
     main = 'Nonparametric Model for ror',
     xlab = 'Deviation of cost',
     ylab = 'Deviation of Ror',
     col = 'blue')

# Add a line for the linear prediction from above.
points(DF[, 'cost_resid'],
       predict(lm_cost_fwl),
       lwd = 3, col = 'red')

# Add a line for the nonparametric prediction.
points(DF[, 'cost_resid'],
       np_cost_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()






# As with age, it looks as though linear might also be close enough.


##################################################
# Semiparametric Models
##################################################





#--------------------------------------------------
# Revisit our best parametric model
# Model 7: Linear model for log of dollar sale price
# With quadratic form for horsepower
#--------------------------------------------------

# Estimate a regression model.
lm_7 <- lm(data = DF,
           formula = ror ~
             squared_mileage +
             make +
             damage +
             dealer + mileage + age + type )

# Output the results to screen.
print(summary(lm_7))



#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on horsepower.
# Semiparametric model for log of dollar sale price
#--------------------------------------------------

# Estimate a regression model.
lm_sp_hp_1 <- lm(data = DF,
                 formula = ror ~
                   mileage_np +
                   make +
                   damage +
                   dealer + mileage + age + type )

# Output the results to screen.
print(summary(lm_sp_hp_1))

# The fit is slightly better but the model is very similar.


#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on age.
# Semiparametric model for log of dollar sale price
#--------------------------------------------------

# Estimate a regression model.
lm_sp_age_1 <- lm(data = DF,
                  formula = ror ~
                    damage + squared_mileage +
                    age_np +
                    make +
                    damage +
                    dealer + mileage + type )

# Output the results to screen.
print(summary(lm_sp_age_1))

# Again, the fit is slightly better but the model is very similar.


#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on engine hours.
# Semiparametric model for log of dollar sale price
#--------------------------------------------------

# Estimate a regression model.
lm_sp_eng_1 <- lm(data = DF,
                  formula = ror ~
                    damage + squared_mileage +
                    make +
                    dealer +
                    dealer + mileage + age + type )

# Output the results to screen.
print(summary(lm_sp_eng_1))

# Again, the fit is slightly better but the model is very similar.

#--------------------------------------------------
# Compare this with a semi-parametric model
# with a nonparametric fit on engine hours.
# Semiparametric model for log of dollar sale price
#--------------------------------------------------

# Estimate a regression model.
lm_sp_full_1 <- lm(data = DF,
                   formula = ror ~
                     mileage_np + cost_np +
                     age_np + 
                     make +
                     damage +
                     dealer + mileage + age + type )

# Output the results to screen.
print(summary(lm_sp_full_1))

# Again, even with this aggressive step of including all three
# variables in a semiparametric form,
# the fit is still only slightly better and the model is very similar.


# Print the output to a LaTeX file.
tab_file_name <- 'reg_semipar.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7,
                lm_sp_hp_1,
                lm_sp_age_1,
                lm_sp_eng_1,
                lm_sp_full_1),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_semipar',
       caption = "Semiparametric Models for ROR")








# The full model is still statistically better.
# The submodels are also both acceptable models.
# Of course, the more flexible model does better
# but this, in some sense, uses many more "degrees of freedom"
# so it is not a fair comparison.
# Better to estimate the semiparametric part in the Box-Tidwell
# transformation, which estimates these features jointly.
# We we will do this in a future problem set.

# With these results, I would explore the GAM or the Box-Tidwell
# with the horsepower variable a candidate for the nonparametric term.




##################################################
# Generalized Additive Model
##################################################

# Now consider a semiparametric model using an
# estimation method that accounts for the joint estimation
# of the nonparametric functions and the parameters.
# This form of model is termed a Generalized Additive Model (GAM)
# and can be estimated with the mgcv package.

# Begin with the linear model specification.
library(mgcv)
gam_model_lin <- gam(formula = ror ~
                       damage + squared_mileage +
                       age +
                       make +
                       damage +
                       dealer + mileage + age + type,
                     data = DF)
print(summary(gam_model_lin))

# Print the output to a LaTeX file.
# Since texreg does not work for GAMs,
# I just printed the output in verbatim mode.
tab_file_name <- 'reg_GAM_lin.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_lin)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


# Allow for nonlinearity using the full model.
gam_model_full <- gam(formula = ror ~
                        s(mileage) +
                        s(age) 
                      , 
                      data = DF)

print(summary(gam_model_full))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_GAM_full.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_full)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


# Allow for nonlinearity in the horsepower variable.
gam_model_hp <- gam(formula = ror ~
                      s(mileage) +
                      age +
                      # s(age) +
                      
                      # s(enghours) +
                      make +
                      damage +
                      dealer + mileage + age + type,
                    data = DF)

print(summary(gam_model_hp))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_GAM_hp.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_hp)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)




############################################################





# Print the output to a LaTeX file.
tab_file_name <- 'reg_GAM_full.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_full)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)



##################################################
# End
##################################################




##################################################
# Box-Tidwell Transformation
##################################################

# The boxTidwell function tests for non-linear relationships
# to the mean of the dependent variable.
# The nonlinearity is in the form of an
# exponential transformation in the form of the Box-Cox
# transformation, except that the transformation is taken
# on the explanatory variables.

#--------------------------------------------------
# Transformation of horsepower
#--------------------------------------------------

# Modified from the linear model:
# saleprice ~ horsepower + squared_horsepower +
#   age + enghours +
#   diesel + fwd + manual + johndeere + cab
# This specification allows a single exponential
# transformation on horsepower, rather than a quadratic form.


bt_mlg <- boxTidwell(formula =
                      ror ~ mileage,
                    other.x = ~
                      age +
                      make +
                      damage +
                      dealer  + type,
                    data = DF)


# The summary method is not available.
# summary(bt_hp)

# The output is a test on the exponent.
print(bt_mlg)
# Note: The "MLE of lambda" is the exponent on horsepower.
# Similar to the Box-Cox transformation,
# with Box-Tidwell, the exponents are on the explanatory variables
# and are all called lambda.
# The exponent is significantly different from 0,
# although it is a small positive value,
# which suggests an increasing but sharply declining relationship.


# Print the output to a LaTeX file.
tab_file_name <- 'bt_mlg.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_mlg)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


#--------------------------------------------------
# Transformation of age
#--------------------------------------------------


bt_age <- boxTidwell(formula =
                       ror ~ age,
                     other.x = ~
                       damage + squared_mileage +
                       # age +
                       dealer + mileage + type,
                     data = DF)

print(bt_age)
# This coefficient is effectively 1, which is more evidence of
# a purely linear relationship between log_saleprice
# and age: the percentage depreciation rate is constant.

# Print the output to a LaTeX file.
tab_file_name <- 'bt_age.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_age)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


#--------------------------------------------------
# Transformation of engine hours
#--------------------------------------------------


bt_cost <- boxTidwell(formula =
                       ror ~ cost,
                     other.x = ~
                       dealer  + age + type,#mileage
                     data = DF)

print(bt_cost)
# Although not statistically significant,
# this suggests a moderately increasing relationship
# between log_saleprice and engine hours,
# which means that tractors with high hours of use
# depreciate more quickly with each additional hour of use.

# Since a nonlinear relationship was detected with horsepower,
# check with nonlinearity in all three continuous variables.




# Print the output to a LaTeX file.
tab_file_name <- 'bt_eng.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_cost)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)

#--------------------------------------------------
# Transformation of horsepower, ageand engine hours
#--------------------------------------------------

bt_full <- boxTidwell(formula =
                        ror ~  
                        age +
                        mileage,
                     other.x = ~make +dealer  + type,
                     data = DF)
                      
                        
                        

print(bt_full)
str(DF)
# This confirms the result of the above,
# with the only nonlinear transformation
# for horsepower.
# This suggests an additional model with
# this transformation of horsepower.

# Print the output to a LaTeX file.
tab_file_name <- 'bt_full.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_full)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


##################################################
# Linear specification from the significant
# exponent in the Box-Tidwell transformation.
##################################################

print(bt_hp)
# MLE of lambda Score Statistic (z)     Pr(>|z|)
#     0.1143693           -7.386372 1.508894e-13
bt_hp_lambda_hat <- 4.4456


# Create a variable horsepower_bt
# to investigate nonlinear relationship of log sale price to horsepower.
Truck[, 'damage_bt'] <-
  Truck[, 'damage']^bt_hp_lambda_hat

# Estimate a regression model.
lm_bt_hp <- lm(data = Truck,
               formula = log_cost ~
                 damage_bt +
                 age +
                 mileage +
                 make + dealer + type )
# damage + squared_damage +

# Output the results to screen.
print(summary(lm_bt_hp))

# The performance is similar to the other models with
# forms of nonlinearity for the value of horsepower.
# Put them in a table for a final comparison.

# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_horse_sp_bt.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7,
                lm_sp_hp_1,
                lm_bt_hp),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_horse_sp_bt',
       caption = "Alternate Models for Tractor Prices")

# The last model has the highest R-squared
# among the ones we have estimated.
# The differences are marginal, however, so the practical recommendation
# is the model with the quadratic relationship for horsepower.


  
##################################################
# End
##################################################
# Estimate a regression model.
lm_7 <- lm(data = DF,
           formula = ror ~
             squared_mileage +
             as.factor(make) +
             as.factor(damage) +
             as.factor(dealer) + mileage + age  )

# Output the results to screen.
print(summary(lm_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_mileage.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_damage',
       caption = "Quadratic Model for  ror")

#estimate a model according to the make type = 1 



lm_12 <- lm(data = DF[DF[, 'type'] == 1, ],
            formula = ror ~
              squared_mileage +
              as.factor(make) +
              as.factor(damage) +
              as.factor(dealer) + mileage + age  )

# Output the results to screen.
print(summary(lm_12))


# estimate a reduce model 
lm_13 <- lm(data = DF[DF[, 'type'] == 1, ],
            formula = ror ~
              squared_mileage +
              as.factor(make) +mileage + age  )
#as.factor(damage) +
# as.factor(dealer) + 

# Output the results to screen.
print(summary(lm_13))

#--------------------------------------------------
# Estimating a Regression Model
# Models 14-15: Linear model for log of cost
# Separate Model for other than make 1
#--------------------------------------------------
lm_14 <- lm(data = DF[DF[, 'type'] == 0, ],
            formula = ror ~
              squared_mileage +
              as.factor(make) +
              as.factor(damage) +
              as.factor(dealer) + mileage + age  )

print(summary(lm_14))

# estimate a reduced model 
lm_15 <- lm(data = DF[DF[, 'type'] == 0, ],
            formula = ror ~
              squared_mileage +
              as.factor(make) +mileage + age  )

print(summary(lm_15))

# Print the output to a LaTeX file.
tab_file_name <- 'reg_type.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7,
                lm_12,
                lm_13,
                lm_14,
                lm_15),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_make',
       caption = "Separate Models by Make")

##################################################
# Create Dependent Variables for
# Sample selection Models
##################################################

# The selection function requires each model
# (make 1 vs other brands) to be
# specified with a separate variable.

# Generate dependent variable in the outcome equation.
# Leave only what is observed.
DF[, 'ror_auction'] <-
  DF[, 'ror'] *
  (DF[, 'type'] == 1)
DF[, 'ror_Retail'] <-
  DF[, 'ror'] *
  (DF[, 'type'] == 0)
##################################################
# Summary of Variables by Brand
# to Investigate Sample Selection
##################################################


# As a preliminary step, compare the distributions of
# explanatory variables by brand of tractor.

# Compare continuous variables.
summary(DF[DF[, "type"] == 0,
              c('mileage', 'age')])
summary(DF[DF[, "type"] == 1,
              c('mileage', 'age')])
# we have differences in age and mileage indicators
#
summary(DF)
# Compare categorical variables.
summary(DF[DF[, "type"] == 0,
              c('damage', 'dealer')])
summary(DF[DF[, "type"] == 1,
              c('damage', 'dealer')])
#damage has different indicators according to the type of sale
#which can be useful in the model 


##################################################
# Probit Models to Investigate Sample Selection
##################################################

# Now estimate a probit model to predict the selection indicator.
# Start with all the other variables in the model.
tobit_5_sel_probit1 <- glm(formula = type ~
                             squared_mileage +
                             (make) +
                             (damage) +
                             (dealer) + mileage + age,
                           data = DF,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit1)

# Estimate a reduced model.
# Eliminating variables one-by-one,

tobit_5_sel_probit2 <- glm(formula = type ~
                             #squared_mileage +
                             #as.factor(make) +
                             #as.factor(damage) +as.factor(dealer)
                             + mileage + age,
                           data = DF,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit2)

# this model focus on mileage as main predictor of sales type
#
# Print the output to a LaTeX file.
tab_file_name <- 'reg_probit.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(tobit_5_sel_probit1,
                tobit_5_sel_probit2),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_probit',
       caption = "Probit Models for Sales Type selection")

##################################################
# Sample selection Models
##################################################

#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate full model first
#--------------------------------------------------

# Estimate the Tobit-5 sample selection model specification.

# Estimate the selection equation with the variables above
# from the probit model.
# For a first model, use the entire set of variables
# for both observation equations, for both sales type
library("sampleSelection")


tobit_5_sel_1 <-
  selection(selection = type ~
              age + mileage,
            outcome = list(ror_Retail ~
                             
                             #as.factor(make) +
                             #as.factor(damage) +
                             (dealer) + mileage + age,
                           ror_auction ~
                             
                             # as.factor(make) +
                             # as.factor(damage) +
                             (dealer) + mileage + age),
            
            iterlim = 20,
            # method = '2step',
            data = DF)

summary(tobit_5_sel_1)


tobit_5_sel_2 <-
  selection(selection = type ~
              age + mileage,
            outcome = list(ror_Retail ~
                             squared_mileage +
                             as.factor(make) +
                             mileage + age,
                           ror_auction ~
                             squared_mileage +
                             as.factor(make) +
                             mileage + age),
            
            iterlim = 20,
            # method = '2step',
            data = DF)
summary(tobit_5_sel_2)

###############################################################################
#--------------------------------------------------
# Estimate Model that Accounts for Sample Selection
# Estimate reduced model from separate estimation
#--------------------------------------------------

tobit_5_sel_3 <-
  selection(selection = type ~
              age + mileage,
            outcome = list(ror_Retail ~age  ,
                           #+ age
                           #as.factor(make) +
                           #as.factor(damage) +
                           #as.factor(dealer) + 
                           ror_auction ~age  ),
            #+ age
            # as.factor(make) +
            # as.factor(damage) +
            #as.factor(dealer) + 
            
            iterlim = 20,
            # method = '2step',
            data = DF)

summary(tobit_5_sel_3)

