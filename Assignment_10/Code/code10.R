#wd_path <- 'C:/Users/Alex/Desktop/R files/QMB6912/Assignment_10'
#setwd(wd_path)
options(scipen=100)
#rm(list=ls(all=TRUE))

library(xtable)
library(texreg)
library(mgcv)
library(car)
library(sampleSelection)
#Data
data <-read.table(file="Data/UsedTrucks.dat")

colnames(data) = c("type","pauc","pret","mileage","make","year","damage","dealer","ror","cost")
data$age <- 2020-data$year
data$mil_sq <-sqrt(data$mileage)
#directories
tab_dir <- 'Tables'
fig_dir <- 'Figures'


#LM Model suggested===

lm_7 <- lm(ror~ mileage+mil_sq+age +damage+as.factor(make)+as.factor(dealer), data=data)
summary(lm_7)
lm_aucf<-lm(ror~ mileage+mil_sq+age +damage+as.factor(make)+as.factor(dealer), 
            data=data[data[, 'type'] == 1, ])
summary(lm_aucf)
lm_retf<-lm(ror~ mileage+mil_sq+age +damage+as.factor(make)+as.factor(dealer), 
            data=data[data[, 'type'] == 0, ])
summary(lm_retf)

tab_file_name <- 'lm_rec_auc.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(lm_aucf,
                lm_retf),
       digits = 9,
       fontsize = 'small', # So table fits on page.
       file = out_file_name,
       label = 'tab:lm_rec_auc',
       caption = "Linear Models for Auction and Retail")
#===Generate dependent variable in the outcome equation.

data[, 'ror_retail'] <-
  data[, 'ror'] *
  (data[, 'type'] == 0)
data[, 'ror_auction'] <-
  data[, 'ror'] *
  (data[, 'type'] == 1)

#Probit models for sample selection=====

tobit_5_sel_probit1 <- glm(formula = type ~
                             mileage+
                             mil_sq+
                             age +
                             damage+
                             as.factor(make)+as.factor(dealer),
                          data = data,
                          family = binomial(link = "probit"))

summary(tobit_5_sel_probit1)

#reduce it


tobit_5_sel_probit2 <- glm(formula = type ~
                             mileage+
                             #age +
                             damage+
                             as.factor(make)+as.factor(dealer),
                           data = data,
                           family = binomial(link = "probit"))

summary(tobit_5_sel_probit2)


##################################################
# Sample selection Models
##################################################

tobit_5_sel_1 <-
  selection(selection = type ~
              mileage+damage,
            outcome = list(ror_retail ~
                             #mileage+
                             age +
                             damage+
                             #as.factor(make)+
                             as.factor(dealer),
                           ror_auction ~
                             #mileage+
                             mil_sq+
                             age +
                             damage),
                             #as.factor(make)+
                             #as.factor(dealer)),
            iterlim = 20,
            #method = '2step',
            data = data)
summary(tobit_5_sel_1
        )
summary(tobit_5_sel_1$lm1)
summary(tobit_5_sel_1$lm2)

sel_list<-type ~
            mileage+
            #mil_sq+
            damage+
            #as.factor(make)#+
            as.factor(dealer)#+
            #age
ret_list<-ror_retail ~
            #mileage+
            #mil_sq+
            damage+
            #as.factor(make)+
            as.factor(dealer)#+
            #age
auc_list<-ror_auction~
            #mileage#+
            mil_sq+
            damage+
            #as.factor(make)+
            #as.factor(dealer)#+
            age

tobit_5_sel_2 <-
  selection(selection = sel_list,
            outcome = list(ret_list,
                           auc_list),
                           iterlim = 20,
                           #method = '2step',
                           data = data)
summary(tobit_5_sel_2)
#summary(tobit_5_sel_2$lm1)
#summary(tobit_5_sel_2$lm2)

tobit_5_sel_3 <-
  selection(selection = type ~
              mileage+damage,
            outcome = list(ror_retail ~
                             #mileage+
                             age +
                             damage+
                             #as.factor(make)+
                             as.factor(dealer),
                           ror_auction ~
                             #mileage+
                             mil_sq+
                             age +
                             damage+
                            #as.factor(make)+
                            as.factor(dealer)),
            iterlim = 20,
            #method = '2step',
            data = data)
summary(tobit_5_sel_3)

tab_file_name <- 'tobit_5_sel.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(l = list(tobit_5_sel_3),
       digits = 5,
       fontsize = 'tiny', # So table fits on page.
       file = out_file_name,
       label = 'tab:tobit_5_sel',
       caption = "Selection Model for Used Truck Rate of Return")
