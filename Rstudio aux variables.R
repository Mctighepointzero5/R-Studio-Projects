#install packages.
# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); 
  require(installr)
} #load / install+load installr

updateR()

install.packages("fdir")
install.packages("lavaan")
install.packages('semTools')
devtools::install_github("neonira/fdir")
library(fdir)
library(lavaan)
library(semTools)


#Read_CSV.
ABER = read.csv("Master File SPSS V9.0.csv")

#inspect data
ABER

#identify correlates of missingness
ABER$mis.LOG_10_HF_HRV_PWR <- is.na(ABER$LOG_10_HF_HRV_PWR)
ABER$mis.HRV_RMSSD_NatLOG <- is.na(ABER$HRV_RMSSD_NatLOG)
ABER$mis.Base_SC_responses <- is.na(ABER$Base_SC_responses)
ABER$mis.IND_SC_responses <- is.na(ABER$IND_SC_responses)
ABER$mis.REC_SC_responses <- is.na(ABER$REC_SC_responses)

#inspect data
ABER$mis.LOG_10_HF_HRV_PWR 
ABER$mis.HRV_RMSSD_NatLOG 
ABER$mis.Base_SC_responses 
ABER$mis.IND_SC_responses 
ABER$mis.REC_SC_responses

#Create models that see if aux variables predicting missingness.
model.LOG_10_HF_HRV_PWR <-  '
      PANAS_2_TOTAL_POS ~ mis.LOG_10_HF_HRV_PWR
      PANAS2_TOTAL_NEG ~ mis.LOG_10_HF_HRV_PWR
      STAI_S_tot ~ mis.LOG_10_HF_HRV_PWR
      CESD_tot ~ mis.LOG_10_HF_HRV_PWR
      DERS_tot ~mis.LOG_10_HF_HRV_PWR
'

model.HRV_RMSSD_NatLOG <- '
      PANAS_2_TOTAL_POS ~ mis.HRV_RMSSD_NatLOG
      PANAS2_TOTAL_NEG ~ mis.HRV_RMSSD_NatLOG
      STAI_S_tot ~ mis.HRV_RMSSD_NatLOG
      CESD_tot ~ mis.HRV_RMSSD_NatLOG
      DERS_tot ~ mis.HRV_RMSSD_NatLOG
'

model.Base_SC_responses <- '
      PANAS_2_TOTAL_POS ~ mis.Base_SC_responses
      PANAS2_TOTAL_NEG ~ mis.Base_SC_responses
      STAI_S_tot ~ mis.Base_SC_responses
      CESD_tot ~ mis.Base_SC_responses
      DERS_tot ~ mis.Base_SC_responses
'


model.IND_SC_responses <- '
      PANAS_2_TOTAL_POS ~ mis.IND_SC_responses
      PANAS2_TOTAL_NEG ~ mis.IND_SC_responses
      STAI_S_tot ~ mis.IND_SC_responses
      CESD_tot ~ mis.IND_SC_responses
      DERS_tot ~ mis.IND_SC_responses
'

model.REC_SC_responses <- '
      PANAS_2_TOTAL_POS ~ mis.REC_SC_responses
      PANAS2_TOTAL_NEG ~ mis.REC_SC_responses
      STAI_S_tot ~ mis.REC_SC_responses
      CESD_tot ~ mis.REC_SC_responses
      DERS_tot ~ mis.REC_SC_responses
'

#Fit models and assess standardizes differences.
fit.LOG_10_HF_HRV_PWR <- sem(model.LOG_10_HF_HRV_PWR, ABER, missing = "fiml")
standardizedsolution(fit.LOG_10_HF_HRV_PWR, type = "std.nox")

fit.HRV_RMSSD_NatLOG  <- sem(model.HRV_RMSSD_NatLOG, ABER, missing = "fiml")
standardizedsolution(fit.HRV_RMSSD_NatLOG, type = "std.nox")

fit.Base_SC_responses <- sem(model.Base_SC_responses, ABER, missing = "fiml")
standardizedsolution(fit.Base_SC_responses, type = "std.nox")

fit.IND_SC_responses <- sem(model.IND_SC_responses, ABER, missing = "fiml")
standardizedsolution(fit.IND_SC_responses, type = "std.nox")

fit.REC_SC_responses <- sem(model.REC_SC_responses, ABER, missing = "fiml")
standardizedsolution(fit.REC_SC_responses, type = "std.nox")

#Create models 
model.LOG_10_HF_HRV_PWR <- 'LOG_10_HF_HRV_PWR ~ HRV_RMSSD_NatLOG + Base_SC_responses + IND_SC_responses + REC_SC_responses'
model.HRV_RMSSD_NatLOG <- 'HRV_RMSSD_NatLOG ~ LOG_10_HF_HRV_PWR + Base_SC_responses + IND_SC_responses + REC_SC_responses'
model.Base_SC_responses <- 'Base_SC_responses ~ LOG_10_HF_HRV_PWR + HRV_RMSSD_NatLOG + IND_SC_responses + REC_SC_responses'
model.IND_SC_responses <- 'IND_SC_responses ~ LOG_10_HF_HRV_PWR + HRV_RMSSD_NatLOG + Base_SC_responses + REC_SC_responses'
model.REC_SC_responses <- 'REC_SC_responses ~ LOG_10_HF_HRV_PWR + HRV_RMSSD_NatLOG + Base_SC_responses + IND_SC_responses'

#create aux variables
auxvars <- c('PANAS_2_TOTAL_POS','PANAS2_TOTAL_NEG','STAI_S_tot','CESD_tot','DERS_tot')

#Fit to see if residuals are correlates. Higher than 3.0 imply non response bias. 
fit.LOG_10_HF_HRV_PWR <- sem.auxiliary(model.LOG_10_HF_HRV_PWR, ABER, missing = "fiml", aux = auxvars)
summary(fit.LOG_10_HF_HRV_PWR,rsquare = T, standardize = T)

fit.HRV_RMSSD_NatLOG <- sem.auxiliary(model.HRV_RMSSD_NatLOG, ABER, missing = "fiml", aux = auxvars)
summary(fit.HRV_RMSSD_NatLOG,rsquare = T, standardize = T)

fit.Base_SC_responses <- sem.auxiliary(model.Base_SC_responses, ABER, missing = "fiml", aux = auxvars)
summary(fit.Base_SC_responses,rsquare = T, standardize = T)

fit.IND_SC_responses <- sem.auxiliary(model.IND_SC_responses, ABER, missing = "fiml", aux = auxvars)
summary(fit.IND_SC_responses,rsquare = T, standardize = T)

fit.REC_SC_responses <- sem.auxiliary(model.REC_SC_responses, ABER, missing = "fiml", aux = auxvars)
summary(fit.REC_SC_responses,rsquare = T, standardize = T)

