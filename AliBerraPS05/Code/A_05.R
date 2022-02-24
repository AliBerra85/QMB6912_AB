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
# Assignment 05
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
install.packages("lattice")
install.packages("latticeExtra")
library(lattice)
library(latticeExtra)

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

#creating the variable age with 2020 as reference year.

Truck$age <- 2020 - Truck$year

##################################################
# Generating Scatterplot Matrices.
print('Generating Scatterplot Matrices.')
##################################################


# Create scatterplots of numeric variables.
splom_var_list <- c('type', 'pauc', 'pret', 'mileage',
                 'damage', 'ror', 'cost' )
#dealer, age, level of damge
# fig_file_name <- 'slpom_num_only.eps'
fig_file_name <- 'slpom_num_only.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
splom(Truck[, splom_var_list])
dev.off()

#dealer, age, level of damge
# Add some categorical variables to scatterplots.
splom_var_list <- c('type', 'pauc', 'pret', 'mileage',
                    'damage', 'ror', 'cost','dealer', 'age' )
Truck
# fig_file_name <- 'slpom_with_cat.eps'
fig_file_name <- 'slpom_with_cat.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
splom(Truck[, splom_var_list])
dev.off()


#cheking the relationship between mileage and age 
library("tidyverse")

fig_file_name <- 'age_mileage_plot.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)

# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
ggplot(head(Truck,nrow(Truck)), aes(x =age, y =mileage, na.rm =TRUE)) +
  geom_point(data = Truck, aes(x =age, y =mileage, na.rm =TRUE))
dev.off()

#cheking the relationship between pret and age 
fig_file_name <- 'pert_age_plot.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)

# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
ggplot(head(Truck,nrow(Truck)), aes(x =pret, y =age, na.rm =TRUE)) +
  geom_point(data = Truck, aes(x =pret, y =age, na.rm =TRUE))
dev.off()

#cheking the relationship between pauc and age 
fig_file_name <- 'pauc_age_plot.pdf'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)

# setEPS()
# postscript(out_file_name)
pdf(out_file_name)
ggplot(head(Truck,nrow(Truck)), aes(x =pauc, y =age, na.rm =TRUE)) +
  geom_point(data = Truck, aes(x =pauc, y =age, na.rm =TRUE))
dev.off()
