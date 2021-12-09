library(readxl)
library(lubridate)
library(tidyverse)
library(knitr)
library(magrittr)
library(stringr)
library(kableExtra)
library(psych)
library(data.table)

#################################### ДАННЫЕ ФОНДОВ ###########################

annual_return <- function(path, name){
  name = transform(readxl::read_xlsx(path), 
                   Дата = as.Date(Дата))
  print(head(name))
  j = 1 # будет стоять на месте
  i = 1 # будет отрываться
  vec = c()
  n = nrow(name)
  while ( i != n){
    while (year(name[j, 1]) == year(name[i, 1]) & i != n){
      i = i + 1
    }
    a.return = name[j, 2] / name[i, 2] * 100
    vec = c(vec, a.return)
    j = i
  }
  return(vec - 100)
}
setwd(file.path('C:', 'Ранх учеба', 'НИР', "Данные", "Акции"))
t.eq = data.frame(Fund = 'alpha.eq', nominal.returns = annual_return('Альфа - Ликвидные акции (по 07.11).xlsx', 
                                                                     alpha.eq))
t.eq = rbind(t.eq, data.frame(Fund = 'vtb.eq', nominal.returns = annual_return('ВТБ - Фонд Акций (по 07.11).xlsx', 
                                                                               vtb.eq)))
t.eq = rbind(t.eq, data.frame(Fund = 'promsv.eq', nominal.returns = annual_return('Промсвязь - Акции (по 07.11).xlsx', 
                                                                                  promsv.eq)))
t.eq = rbind(t.eq, data.frame(Fund = 'raif.eq', nominal.returns = annual_return('Райффайзен - Акции (по 07.11).xlsx', 
                                                                                raif.eq)))
t.eq = rbind(t.eq, data.frame(Fund = 'sber.eq', nominal.returns = annual_return('Сбербанк - Добрыня Никитич ( по 07.11).xlsx', 
                                                                                sber.eq)))


setwd(file.path('C:', 'Ранх учеба', 'НИР', "Данные", "Облигации"))
t.bonds = data.frame(Fund = 'alpha.b', nominal.returns = annual_return('Альфа - Капитал (по 07.11).xlsx', 
                                                                       alpha.b))
t.bonds = rbind(t.bonds, data.frame(Fund = 'vtb.b', 
                                    nominal.returns = annual_return('ВТБ - Фонд Казначеский (по 07.11).xlsx', 
                                                                    vtb.b)))
t.bonds = rbind(t.bonds, data.frame(Fund = 'raif.b', 
                                    nominal.returns = annual_return('Райффайзен - Облигации (по 07.11).xlsx', 
                                                                    raif.b)))
t.bonds = rbind(t.bonds, data.frame(Fund = 'sber_m.b', 
                                    nominal.returns = annual_return('Сбербанк - Илья Муромец (по 07.11).xlsx', 
                                                                    sber_m.b)))
t.bonds = rbind(t.bonds, data.frame(Fund = 'sber_p.b', 
                                    nominal.returns = annual_return('Сбербанк - Фонд перспективных облигаций (по 07.11).xlsx', 
                                                                    sber_p.b)))


setwd(file.path('C:', 'Ранх учеба', 'НИР', "Данные", "Смешанные"))
t.m = data.frame(Fund = 'alpha.m', nominal.returns = annual_return('Альфа - Баланс (по 07.11).xlsx', 
                                                                   alpha.m))
t.m = rbind(t.m, data.frame(Fund = 'vtb.m', 
                            nominal.returns = annual_return('ВТБ - Фонд Сбалансированный.xlsx', 
                                                            vtb.m)))
t.m = rbind(t.m, data.frame(Fund = 'sber.m', 
                            nominal.returns = annual_return('Сбербанк - Фонд Сбалансированный.xlsx', 
                                                            sber.m)))

# проставляет года
counting <- function(dataset){
  i = 1
  j = 1
  k = 1
  y = c()
  n = nrow(dataset)
  while (j <= n){
    
    while (j <= n & dataset[i, 1] == dataset[j, 1]){
      y = c(y, 2020 - k + 1)
      j = j + 1
      k = k + 1
    }
    k = 1
    i = j
  }
  return(data.frame(years = y, dataset))
} 
t.eq = counting(t.eq)
t.bonds = counting(t.bonds)
t.m = counting(t.m)
rm('counting')
rm('annual_return')
# # # # 
# Инфляция
inflation = rev(c(111.03, 184.43, 136.53, 120.18, 118.58, 115.06, 111.99, 111.73, 
                  110.92, 109.00, 111.87, 113.28, 108.80, 108.78, 106.10, 106.57, 
                  106.47, 111.35, 112.91, 105.39, 102.51, 104.26, 103.04, 104.9) - 100)
Names = rep(2020, 24)
for (i in 1:length(Names)){
  Names[i] = Names[i] - i + 1
}
names(inflation) = as.character(Names); rm(list = c('i', 'Names'))

real_annual_return <- function(dataset1){
  n.mat = dataset1 %>%
    group_by(Fund) %>%
    summarize(n = n())
  print(n.mat)
  i = 1 # будет оставаться на месте
  j = 1 # будет двигаться
  k = 1
  vec = c()
  n = nrow(dataset1)
  while (j <= n){
    row = which(n.mat$Fund == dataset1[i, 2])
    number = as.numeric(n.mat[row,2])
    print(number)
    inflation1 = inflation[1:number]
    while (dataset1[i, 'Fund'] == dataset1[j, 'Fund'] & j <= n){
      real_return = ((1 + dataset1[j, 'nominal.returns']/100) / (1 + inflation1[k]/100) - 1) * 100
      vec = c(vec, real_return)
      j = j + 1
      k = k +1
    }
    i = j
    k = 1
  }
  return(vec)
}


t.eq.real_returns = data.frame(t.eq, real.returns = real_annual_return(t.eq))
t.b.real_returns = data.frame(t.bonds, real.returns = real_annual_return(t.bonds))
t.m.real_returns = data.frame(t.m, real.returns = real_annual_return(t.m))
t.eq = t.eq.real_returns
t.b = t.b.real_returns
t.m = t.m.real_returns
rm('t.eq.real_returns', 't.b.real_returns', 't.m.real_returns', 't.bonds')









####################### РАБОТА С ИНДЕКСАМИ ########################################
setwd(file.path('C:', 'Ранх учеба', "НИР", "Данные", "Индексы"))

# чтение даннных с мосбиржи
reading_from_mx = function(name, dataset){
  name = read.csv2(dataset, header = T, skip = 2)
  name[, 'begin'] <- NULL
  name[, 'end'] %<>%
    unlist() %>%
    as.Date(format = '%Y-%m-%d')
  names(name)[7] = 'Date'
  name[, 'open'] %<>%
    unlist() %>%
    as.numeric()
  name %<>% 
    as_tibble()
  return(name)
}
RGBI = reading_from_mx(IMOEX, 'RGBITR.csv')
IMOEX = reading_from_mx(IMOEX, 'IMOEX (1).csv')
RUCBITR= reading_from_mx(RGBI, 'RUCBITR.csv')
RUGBITR = reading_from_mx(RUGBITR, 'RUGBITR.csv') # последний год не учитывать


# Завершили чтение индексов
# Получим номинальную доходность для индексов
annual_return_for_IMOEX <- function(dataset){
  dataset = as.data.frame(dataset)
  i = 1 # будет оставаться на месте
  j = 1 # будет итератором
  vec = c()
  n = nrow(dataset)
  while (j <= n){
    while (year(dataset[i, 'Date']) == year(dataset[j, 'Date']) & j <= n){
      j = j + 1 
    }
    annual_return = dataset[i, 'open'] / dataset[j, 'open'] * 100
    vec = c(vec, annual_return)
    i = j
  }
  return(vec - 100)
}
IMOEX = data.frame(year = 2020:1997, 
                                   annual.nominal.return = annual_return_for_IMOEX(IMOEX))
RGBI = data.frame(year = 2020:2002, 
                     annual.nominal.return = annual_return_for_IMOEX(RGBI))
RUCBITR = data.frame(year = 2020:2002, 
                     annual.nominal.return = annual_return_for_IMOEX(RUCBITR))
RUGBITR = data.frame(year = 2020:2010,
                      annual.nominal.return = annual_return_for_IMOEX(RUGBITR))
rm('annual_return_for_IMOEX')

IMOEX = IMOEX[-nrow(IMOEX), ]
RGBI = RGBI[-nrow(RGBI), ]
RUCBITR = RUCBITR[-nrow(RUCBITR), ]
RUGBITR = RUGBITR[-nrow(RUGBITR), ]




# реальная доходность для индексов
real_annual_return_for_indexes <- function(dataset){
  inflation = inflation[1: nrow(dataset)]
  vec = c()
  for (i in 1:nrow(dataset)){
    value = (( 1 + dataset[i, 'annual.nominal.return']/100 ) / ( 1 + inflation[i]/100 ) - 1) * 100
    vec = c(vec, value)
  }
  return(vec)
}
IMOEX = data.frame(IMOEX, 
                   annual.real.return = real_annual_return_for_indexes(IMOEX))
RGBI = data.frame(RGBI, 
                  annual.real.return = real_annual_return_for_indexes(RGBI))
RUCBITR = data.frame(RUCBITR, 
                     annual.real.return = real_annual_return_for_indexes(RUCBITR))
RUGBITR = data.frame(RUGBITR,
                      annual.real.return = real_annual_return_for_indexes(RUGBITR))
#mixed_index = data.frame(mixed_index, 
#                         annual.real.return = real_annual_return_for_indexes(mixed_index))
rm('IMOEX.nominal_returns', 'RGBI.nominal_returns', 'real_annual_return_for_indexes')



# # # # # # # # # # # # # 
names(t.eq) = c('year', 'Fund', 'nominal.returns', 'annual growth rate of real returns')
names(t.b) = c('year', 'Fund', 'nominal.returns', 'annual growth rate of real returns')
names(t.m) = c('year', 'Fund', 'nominal.returns', 'annual growth rate of real returns')
t.eq = data.frame(t.eq, 'real returns' = real_annual_return(t.eq) + 100)
t.b = data.frame(t.b, 'real returns' = real_annual_return(t.b) + 100)
t.m = data.frame(t.m, 'real returns' = real_annual_return(t.m) + 100)
