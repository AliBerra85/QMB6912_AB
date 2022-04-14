##################################################
#
# QMB 6912 Capstone Project in Business Analytics
#
# Examples of Nonlinear Model Specification
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# March 22, 2022
#
##################################################
#
# Tractor_Nonparametric gives examples of linear
#   regression models augmented with a number of
#   different nonlinear model specifications,
#   which are estimated using nonparametric methods.
#
# Dependencies:
#   Libraries xtable and texreg
#   to print tables of regression results.
#   mgcv to fit the models within a generalized
#   additive model (GAM).
#
##################################################


##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S22/demo_08/Tractor_Nonparametric'
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

# Libraries to print tables of regression results.
library(xtable)
library(texreg)

# Library mgcv for estimating Generalized Additive Models
library(mgcv)


##################################################
# Loading the Data
##################################################

in_file_name <- sprintf('%s/%s', data_dir, "UsedTrucks.dat")
Truck_col_names <- c('type', 'pauc', 'pret', 'mileage', 'make',
                     'year', 'damage', 'dealer', 'ror', 'cost')

# Load data.
Truck <- read.table(file = in_file_name, header = FALSE,
                    col.names = Truck_col_names)

# Initial inspection.
print("Summary of UsedTrucks.dat dataset:")
print(summary(Truck))



# Make sure there are no problems with the data.



##################################################
# Generating New Variables
##################################################


# In Problem Set #6, we determined that taking logs
# of tractor prices produced a better model with
# a distribution closer to normal.

Truck[, 'log_cost'] <- log(Truck[, 'cost'])
str(Truck)
#create age so numbers look a little nicer on prediction
Truck$age <- 2020-Truck$year
# Create a variable squared_horsepower
# to investigate quadratic relationship of sale price to horsepower.
Truck[, 'squared_damage'] <- Truck[, 'damage']^2

str(Truck)
##################################################
# Linear Regression Model
##################################################

# In Problem Set #7 I recommended the following model,
# which included a quadratic form for horsepower.

# Estimate a regression model.
lm_7 <- lm(data = Truck,
           formula = log_cost ~
             squared_damage +
             make +
             damage +
             dealer + mileage + age + type )

# Output the results to screen.
print(summary(lm_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_damage.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_damage',
       caption = "Quadratic Model for Trucks cost")


##################################################
# Linear Regression Model
# Frisch-Waugh-Lovell regressions to partial out
# other variables
##################################################


# Next, consider the model without this variable.

# Estimate a regression model.
lm_no_hp <- lm(data = Truck,
               formula = log_cost ~
                 #squared_damage +
                 make +
                 #damage +
                 dealer + mileage + age + type )

# Output the results to screen.
print(summary(lm_no_hp))

# Next, estimate a model for the horsepower variable,
# using the other dependent variables as covariates.
# This estimates the "excess horsepower" above what one
# would predict using the other characteristics of the tractor.

# Estimate a regression model.
lm_hp <- lm(data = Truck,
            formula = damage ~
              make +
              
              dealer + mileage + age + type )

# Output the results to screen.
print(summary(lm_hp))

# Do the same for horsepower squared.
lm_hp_2 <- lm(data = Truck,
              formula = squared_damage ~
                make +
                
                dealer + mileage + age + type )

# Output the results to screen.
print(summary(lm_hp_2))



# Finally, estimate a model for the
# value of a tractor using only the excess horsepower variable
# as a covariate.
# This regression is performed using the residuals
# from the two regressions using the other variables as covariates.

# Generate the residuals from each model.
Truck[, 'damage_resid'] <- lm_hp$residuals
Truck[, 'damage_2_resid'] <- lm_hp_2$residuals
Truck[, 'log_cost_resid_hp'] <- lm_no_hp$residuals

# Finally, run a regression of the tractor price residuals
# on the horsepower residuals.
# This regression uses the Frisch-Waugh-Lovell theorem
# to partial out the other variables.


# Estimate a regression model.
lm_hp_quad_fwl <- lm(data = Truck,
                     formula = log_cost_resid_hp ~ -1 +
                       damage_resid + damage_2_resid)

# Output the results to screen.
print(summary(lm_hp_quad_fwl))

# Notice that the coefficients on the horsepower variables
# are the same as those from the original regression.
print(summary(lm_7))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_sq_damage_fwl.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_7,
                lm_no_hp,
                lm_hp,
                lm_hp_2,
                lm_hp_quad_fwl),
       digits = 5,
       file = out_file_name,
       label = 'tab:reg_sq_damage_fwl',
       caption = "Quadratic Model for Trucks cost: FWL Regressions")


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

fig_file_name <- 'dev_vs_cost.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(Truck[, 'damage'],
     Truck[, 'log_cost_resid_hp'],
     main = 'Quadratic Model for Truck cost',
     xlab = 'cost',
     ylab = 'Deviation of Log Truck cost',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(Truck[, 'cost'],
       predict(lm_hp_quad_fwl),
       lwd = 3, col = 'red')

dev.off()


# Plot a scattergraph to focus on excess horsepower.

fig_file_name <- 'dev_vs_cost_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(Truck[, 'damage_resid'],
     Truck[, 'log_cost_resid_hp'],
     main = 'Nonparametric Model for Trucks cost',
     xlab = 'Deviation of Damage',
     ylab = 'Deviation of Log Trucks cost',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(Truck[, 'damage_resid'],
       predict(lm_hp_quad_fwl),
       lwd = 3, col = 'red')

dev.off()


#--------------------------------------------------
# Estimate and plot Nonparametric model for horsepower
#--------------------------------------------------

# The loess function is a smoothing method
# for estimating nonparametric models.
np_hp_fit_1 <- loess(log_cost_resid_hp ~ damage_resid,
                     Truck)
# Calculate the predictions.
Truck[, 'damage_np'] <- np_hp_fit_1$fitted


# Add a plot of this curve to the above scattergraph.


fig_file_name <- 'dev_np_vs_damage_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(Truck[, 'damage_resid'],
     Truck[, 'log_cost_resid_hp'],
     main = 'Nonparametric Model for Trucks cost',
     xlab = 'Deviation of Horsepower',
     ylab = 'Deviation of Log Trucks cost',
     col = 'blue')

# Add a line for the quadratic prediction from above.
points(Truck[, 'damage_resid'],
       predict(lm_hp_quad_fwl),
       lwd = 3, col = 'red')

# Add a line for the quadratic prediction from above.
points(Truck[, 'damage_resid'],
       np_hp_fit_1$fitted,
       lwd = 3, col = 'green')

dev.off()

# The nonparametric function is slightly more curved
# but the difference is not great.
# So far, it appears that the quadratic form
# is close enough.



#--------------------------------------------------
# Nonparametric model for age
#--------------------------------------------------


# First, fit Frish-Waugh-Lovell regressions
# to partial out other variables
# Consider the model without the age variable.

# Estimate a regression model.
lm_no_age <- lm(data = Truck,
                formula = log_cost ~
                  squared_damage +
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
lm_age <- lm(data = Truck,
             formula = age ~
               squared_damage +
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
Truck[, 'age_resid'] <- lm_age$residuals
Truck[, 'log_cost_resid_age'] <- lm_no_age$residuals

# Finally, run a regression of the tractor price residuals
# on the age residuals.
# Again, this regression uses the Frisch-Waugh-Lovell theorem
# to partial out the other variables.


# Estimate a regression model.
lm_age_fwl <- lm(data = Truck,
                 formula = log_cost_resid_age ~ -1 +
                   age_resid)

# Output the results to screen.
print(summary(lm_age_fwl))

# Notice that the coefficients on the age variable
# is the same as those from the original regression.
print(summary(lm_7))










#--------------------------------------------------
# Estimate and plot Nonparametric model for age
#--------------------------------------------------

# Use the loess function.
np_age_fit_1 <- loess(log_cost_resid_age ~ age_resid,
                      Truck,
                      span = 0.25)
# Calculate the predictions.
Truck[, 'age_np'] <- np_age_fit_1$fitted


# Add a plot of this curve to the above scattergraph.


fig_file_name <- 'dev_np_vs_age_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(Truck[, 'age_resid'],
     Truck[, 'log_cost_resid_age'],
     main = 'Nonparametric Model for Tractor Prices',
     xlab = 'Deviation of Age',
     ylab = 'Deviation of Log Tractor Prices',
     col = 'blue')

# Add a line for the linear prediction from above.
points(Truck[, 'age_resid'],
       predict(lm_age_fwl),
       lwd = 3, col = 'red')

# Add a line for the nonparametric prediction.
points(Truck[, 'age_resid'],
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
lm_no_eng <- lm(data = Truck,
                formula = log_cost ~
                  squared_damage +
                  make +
                  damage +#mileage
                  age+ type )

# Output the results to screen.
print(summary(lm_no_eng))

# Next, estimate a model for the engine hours variable,
# using the other dependent variables as covariates.
# This estimates the "excess engine hours" above what one
# would predict using the other characteristics of the tractor.

# Estimate a regression model.
lm_eng <- lm(data = Truck,
             formula = mileage ~
               squared_damage +
               make +
               damage +
               age + type )
# Output the results to screen.
print(summary(lm_eng))


# Finally, estimate a model for the
# value of a tractor using only the excess engine hours variable
# as a covariate.
# This regression is performed using the residuals
# from the two regressions using the other variables as covariates.

# Generate the residuals from each model.
Truck[, 'mileage_resid'] <- lm_eng$residuals
Truck[, 'log_cost_resid_eng'] <- lm_no_eng$residuals

# Finally, run a regression of the tractor price residuals
# on the engine hours residuals.
# Again, this regression uses the Frisch-Waugh-Lovell theorem
# to partial out the other variables.


# Estimate a regression model.
lm_eng_fwl <- lm(data = Truck,
                 formula = log_cost_resid_eng ~ -1 +
                   mileage_resid)

# Output the results to screen.
print(summary(lm_eng_fwl))

# Notice again that the coefficients on the engine hour variable
# is the same as those from the original regression.
print(summary(lm_7))



# Skip the linear model and go straight to the
# nonparametric model.

#--------------------------------------------------
# Estimate and plot Nonparametric model for engine hours
#--------------------------------------------------

# Use the loess function.
np_eng_fit_1 <- loess(log_cost_resid_eng ~ mileage_resid,
                      Truck,
                      span = 0.25)
# Calculate the predictions.
Truck[, 'mileage_np'] <- np_eng_fit_1$fitted


# Add a plot of this curve to the above scattergraph.


fig_file_name <- 'dev_np_vs_eng_dev.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)

plot(Truck[, 'mileage_resid'],
     Truck[, 'log_cost_resid_eng'],
     main = 'Nonparametric Model for Truck cost',
     xlab = 'Deviation of mileage',
     ylab = 'Deviation of Log Trcuk cost',
     col = 'blue')

# Add a line for the linear prediction from above.
points(Truck[, 'mileage_resid'],
       predict(lm_eng_fwl),
       lwd = 3, col = 'red')

# Add a line for the nonparametric prediction.
points(Truck[, 'mileage_resid'],
       np_eng_fit_1$fitted,
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
lm_7 <- lm(data = Truck,
           formula = log_cost ~
             squared_damage +
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
lm_sp_hp_1 <- lm(data = Truck,
                 formula = log_cost ~
                   damage_np +
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
lm_sp_age_1 <- lm(data = Truck,
                  formula = log_cost ~
                    damage + squared_damage +
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
lm_sp_eng_1 <- lm(data = Truck,
                  formula = log_cost ~
                    damage + squared_damage +
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
lm_sp_full_1 <- lm(data = Truck,
                   formula = log_cost ~
                     damage_np + mileage_np +
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
       caption = "Semiparametric Models for Truck cost")








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
gam_model_lin <- gam(formula = log_cost ~
                       damage + squared_damage +
                       age +
                       make +
                       damage +
                       dealer + mileage + age + type,
                     data = Truck)
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
gam_model_full <- gam(formula = log_cost ~
                        s(damage) +
                        s(age) 
                      , 
                      data = Truck)

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
gam_model_hp <- gam(formula = log_cost ~
                      s(damage) +
                      age +
                      # s(age) +
                      
                      # s(enghours) +
                      make +
                      damage +
                      dealer + mileage + age + type,
                    data = Truck)

print(summary(gam_model_hp))


# Print the output to a LaTeX file.
tab_file_name <- 'reg_GAM_hp.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_hp)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)




# Print the output to a LaTeX file.
tab_file_name <- 'reg_GAM_full.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
summary(gam_model_full)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)

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


bt_hp <- boxTidwell(formula =
                      log_cost ~ damage,
                    other.x = ~
                      # damage + squared_damage +
                      age +
                      make +
                      damage +
                      dealer + mileage + age + type,
                    data = Truck)


# The summary method is not available.
# summary(bt_hp)

# The output is a test on the exponent.
print(bt_hp)
# Note: The "MLE of lambda" is the exponent on horsepower.
# Similar to the Box-Cox transformation,
# with Box-Tidwell, the exponents are on the explanatory variables
# and are all called lambda.
# The exponent is significantly different from 0,
# although it is a small positive value,
# which suggests an increasing but sharply declining relationship.


# Print the output to a LaTeX file.
tab_file_name <- 'bt_hp.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat("\\begin{verbatim}", file = out_file_name)
sink(out_file_name, append = TRUE)
print(bt_hp)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)


#--------------------------------------------------
# Transformation of age
#--------------------------------------------------


bt_age <- boxTidwell(formula =
                       log_cost ~ age,
                     other.x = ~
                       damage + squared_damage +
                       # age +
                       dealer + mileage + age + type,
                     data = Truck)

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


bt_eng <- boxTidwell(formula =
                       log_cost ~ mileage,
                     other.x = ~
                       dealer  + age + type,#mileage
                     data = Truck)

print(bt_eng)
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
print(bt_eng)
sink()
cat("\\end{verbatim}", file = out_file_name, append = TRUE)

#--------------------------------------------------
# Transformation of horsepower, ageand engine hours
#--------------------------------------------------

bt_full <- boxTidwell(formula =
                        log_cost ~ damage +
                        age +
                        mileage,
                      other.x = ~
                        # damage + squared_damage +
                        #age +
                        #mileage
                        make +
                      
                        dealer  + type,
                      data = Truck)

print(bt_full)

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



