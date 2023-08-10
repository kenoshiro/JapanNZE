library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(magrittr)
library(ggplot2)
library(cowplot)
library(readxl)

dir.create(path='output',showWarnings=F)

df <- list()
lst <- list()
obj <- list()
p <- list()

df$var_config <- read_csv('define/variable.csv',col_names=c('Variable2','Variable'))
df$all <- read_csv('data/scenario_data.csv') %>% 
    gather(-Model,-Scenario,-Region,-Variable,-Unit,key=Year,value=Value) %>% 
    filter(Value != 'N/A') %>% 
    inner_join(df$var_config,by='Variable') %>% select(-Variable) %>% rename(Variable='Variable2') %>% 
    spread(key=Year,value='Value',fill=0) %>% 
    gather(-c('Model','Scenario','Region','Variable','Unit'), key='Year', value='Value') %>% 
    spread(key=Scenario,value='Value',fill=0) %>% 
    gather(-c('Model','Region','Variable','Year','Unit'), key='Scenario', value='Value') %>% 
    mutate(Year=as.integer(as.character(Year)))

source('prog/func_calc.R')
source('prog/calc_var.R')
source('prog/plot.R',echo=T)
