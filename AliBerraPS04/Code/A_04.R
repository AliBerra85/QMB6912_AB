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
# January 26, 2022
#
##################################################


# Ali Berra
# Assignment 04
#

###################################################
# Sample code for the problem sets in the course QMB 6912,
# Capstone Project in Business Analytics, for the PMSM-BA
# program.
#
# Dependencies:
#   xtable for creating code for LaTeX tables
#
#
##################################################


##################################################
# Preparing the Workspace
##################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory, if running interactively.
# wd_path <- '~/GitHub/QMB6912S22/demo_03/FlyReel_Tables'
# setwd(wd_path)


# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
# fig_dir <- 'Figures' # Next week.

# Set directory for storing figures.
fig_dir <- 'Figures'



##################################################
# Load libraries
##################################################

#install.packages('xtable')
library(xtable)

library(lattice)
library(latticeExtra)
##################################################
# Problem Set 3: Summarize Covariates
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

#fig_file_name <- 'ecdf'
# ecdf_prices <- ecdf(Truck[,'ror'])
# vals <- data.frame(r1=Truck$ror,
#                    r2=Truck$type,
#                    r3=Truck$cost)
#
# ?ecdf
#
# plot(~ r1 + r2 + r3, data=vals, auto.key=list(space='right'))
#
#
#
# fig_file_name <- 'ecdf_prices.eps'
# out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# setEPS()
# postscript(out_file_name)
# plot(ecdf_prices,
#      main = 'Empirical Cumulative Distribution Function of return rate',
#      xlab = 'ror',
#      ylab = 'Empirical C.D.F.')
# dev.off()





#################################################################
ecdf_price <- ecdf(Truck[,'ror'])

vals <- data.frame(r1=Truck$ror,
                   r2=Truck$type,
                   r3=Truck$cost)



ecdfplot(~ r1 + r2 + r3, data=vals, auto.key=list(space='right'))



fig_file_name <- 'ecdf_prices.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
plot(ecdf_price,
     main = 'Empirical Cumulative Distribution Function of Prices',
     xlab = 'ror',
     ylab = 'Empirical C.D.F.')
dev.off()




fig_file_name <- 'ecdf_ror.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
pdf(out_file_name)
plot(ecdf_price,
     main = 'Empirical Cumulative Distribution Function of ror',
     xlab = 'ror',
     ylab = 'Empirical C.D.F.')
dev.off()

##################################################
# Relative histogram of price.
print('Ploting relative histogram of retail price.')
##################################################

fig_file_name <- 'hist_prices.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
hist(Truck[, 'pret'],
     main = 'Relative Histogram of retail Prices',
     xlab = 'pret',
     probability = TRUE)
dev.off()


##################################################
# Kernel-smoothed pdf of the natural logarithm of price.
print('Plotting kernel-smoothed pdf')
print('of the natural logarithm of price.')
##################################################

density_log_price <- density(log(Truck[, 'pauc']))
fig_file_name <- 'density_prices.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)
plot(density_log_price,
     main = 'Kernel-smoothed pdf of the Natural Log. of Used Truck Prices',
     xlab = 'Price')
dev.off()



##################################################
# End
##################################################
