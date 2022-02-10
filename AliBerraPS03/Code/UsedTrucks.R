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
# Assignment 03
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

# Set directory for storing tables.
tab_dir <- 'Tables'


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

# Summarize numeric variables by country of manufacture.
type_sum <- data.frame(type = unique(Truck$type))
for (var_name in colnames(Truck)[lapply(Truck, class) == 'numeric']) {
  
  col_names <- sprintf('%s %s', c('Min.', 'Mean', 'Max.'), var_name)
  type_sum[, col_names] <- tapply(Truck[,var_name], Truck$type,
                                     function(x) format(summary(x), scientific = FALSE)[c(5,6)])
  
}
var_name
# Select values for output.
# t(X) denotes the transpose of X.
out_tab <- t(type_sum[, 2:ncol(type_sum)])
colnames(out_tab) <- type_sum[, 1]
print(out_tab)

# Output to TeX file.
out_xtable <- xtable(out_tab[, ],
                     digits = 0, label = 'tab:summ_by_type',
                     # caption = 'Goodness of Fit for Fixed vs. Monthly Transition Matrices (1-step-ahead forecasts)',
                     caption = 'Summary by type ')

tab_file_name <- sprintf('summ_by_type.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)





#--------------------------------------------------
# Summarize categorical variables.
#--------------------------------------------------

print('Summarizing Categorical Variables')


# Inspect visually before creating tables.
table(Truck[, 'type'], useNA = 'ifany')
table(Truck[, 'damage'], useNA = 'ifany')
table(Truck[, 'make'], useNA = 'ifany')
table(Truck[, 'dealer'], useNA = 'ifany')

# Comparison across brand names.
table(Truck[, 'type'], Truck[, 'damage'], useNA = 'ifany')
table(Truck[, 'type'], Truck[, 'make'], useNA = 'ifany')
table(Truck[, 'type'], Truck[, 'dealer'], useNA = 'ifany')


print('type of Sales')


# Assemble these into a table for output.
out_tab <- cbind(table(Truck[, "make"], useNA = 'ifany'),
                 table(Truck[, "make"], Truck[, 'dealer'], useNA = 'ifany'),
                 table(Truck[, "make"], Truck[, 'damage'], useNA = 'ifany'),
                 table(Truck[, "make"], Truck[, 'type'], useNA = 'ifany')
)
out_tab
# Specify column names and add totals.
colnames(out_tab) <- c("total", "Ford", "Ford", "Chevrolet",
                       "Dodge", "General Motors", "Toyota", "Nissan", "Subaru", "other",
                       "dealer1", "dealer2","dealer3","dealer4",
                       "dealer5","dealer6","dealer7","dealer8","dealer9","damage",
                       "Auction","Retail")
out_tab <- rbind(out_tab, colSums(out_tab))
rownames(out_tab)[length(rownames(out_tab))] <- "Totals"
print(out_tab)


# Output selected columns to TeX file.
out_xtable <- xtable(out_tab[, c(2, 3, 4, 1)],
                     digits = 0, label = 'tab:Truck_brand',
                     # caption = 'Goodness of Fit for Fixed vs. Monthly Transition Matrices (1-step-ahead forecasts)',
                     caption = 'Brand of Used Trucks')

tab_file_name <- sprintf('Truck_by_Brand.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)


print('Brand of Trucks')


# Output another set of columns to another TeX file.
out_xtable <- xtable(out_tab[, c(10:19)],
                     digits = 0, label = 'tab:By_dealer',
                     # caption = 'Goodness of Fit for Fixed vs. Monthly Transition Matrices (1-step-ahead forecasts)',
                     caption = 'by Dealer')

tab_file_name <- sprintf('by_Dealer.tex')
tab_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
cat(print(out_xtable), file = tab_file_name, append = FALSE)




##################################################
# End
##################################################
