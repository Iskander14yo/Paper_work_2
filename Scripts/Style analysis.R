# считаем R2

style_analysis1 = function(type, fund, step = 0.05){
  if (type == 'eq') { fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  
  if (length(fund_data) > 18) {
    fund_data = fund_data[1:18]
    imoex_data = IMOEX$annual.real.return[1:18]
    rgbi_data = RGBI$annual.real.return
    rucbitr_data = RUCBITR$annual.real.return
  } else {
    imoex_data = IMOEX$annual.real.return[1:length(fund_data)]
    rgbi_data = RGBI$annual.real.return[1:length(fund_data)]
    rucbitr_data = RUCBITR$annual.real.return[1:length(fund_data)]
  }
  imoex_var = var(imoex_data)
  rgbi_var = var(rgbi_data)
  rucbitr_var = var(rucbitr_data)
  fund_var = var(fund_data)
  b1 = 0; b2 = 0; b3 = 0 # b1 - IMOEX, b2 - rgbi, b3 - rucbitr
  our_min = -Inf
  
  while (b1 <= 1.0000000001){
    while (b2 <= 1.000000001){
      
      while (b3 <= 1.0000000001){
        if ( isTRUE(all.equal(b1 + b2 + b3, 1)) ) {
          index = b1*imoex_data + b2*rgbi_data + b3*rucbitr_data
          current = (var(b1*imoex_data) + var(b2*rgbi_data) + var(b3*rucbitr_data) +
                       2*cov(b1*imoex_data, b2*rgbi_data) +
                       2*cov(b1*imoex_data, b3*rucbitr_data) +
                       2*cov(b3*rucbitr_data, b2*rgbi_data) )/ var(fund_data)
          if ( current > our_min & current <= 1) { 
            our_min = current
            min_vector = c(b1, b2, b3)
            print(current)
            print( var(min_vector[1]*imoex_data + min_vector[2]*rgbi_data +
                         min_vector[3]*rucbitr_data)/  var(fund_data))
            print( c(b1, b2, b3) )
          }
        }
        b3 = b3 + step
      }
      
      b3 = 0
      b2 = b2 + step
    }
    b2 = 0; b3 = 0
    b1 = b1 + step
  }
  print(paste0('weights: IMOEX = ' , min_vector[1], 
               ', RGBI = ', min_vector[2], ', Rucbitr = ', min_vector[3]))
  return(min_vector)
}

style_analysis2 = function(type, fund, step = 0.05){
  if (type == 'eq') { fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  
  fund_data = fund_data[1:10]
  imoex_data = IMOEX$annual.real.return[1:10]
  rgbi_data = RGBI$annual.real.return[1:10]
  rucbitr_data = RUCBITR$annual.real.return[1:10]
  rugbitr_data = RUGBITR$annual.real.return[1:10]
  
  fund_var = var(fund_data)
  imoex_var = var(imoex_data)
  rgbi_var = var(rgbi_data)
  rucbitr_var = var(rucbitr_data)
  rugbitr_var = var(rugbitr_data)
  
  b1 = 0; b2 = 0; b3 = 0; b4 = 0 # b1 - IMOEX, b2 - rgbi, b3 - rucbitr, b4 - rugbitr
  our_max = 0
  
  while (b1 <= 1.00001){
    while (b2 <= 1.00001){
      while (b3 <= 1.00001) {
        
        while (b4 <= 1.00000001){
          if ( isTRUE(all.equal(1, b1 + b2 + b3 + b4, tolerance = 0.0001)) ){
            current = ( (b1^2 * imoex_var) + (b2^2 * rgbi_var) + (b3^2 * rucbitr_var) + (b4^2 * rugbitr_var) +
                         2*cov(b1*imoex_data, b2*rgbi_data) +
                         2*cov(b1*imoex_data, b3*rucbitr_data) +
                         2*cov(b1*imoex_data, b4*rugbitr_data) + 
                         2*cov(b2*rgbi_data, b3*rucbitr_data) +
                         2*cov(b2*rgbi_data, b4*rugbitr_data) +
                         2*cov(b3*rucbitr_data, b4*rugbitr_data) )  / fund_var
            if (current > our_max) { 
              our_max = current 
              max_vector = c(b1, b2, b3, b4) 
              print(max_vector)
              print(our_max)
            }
          }
          b4 = b4 + step
        }
        b4 = 0
        b3 = b3 + step
      }
      b3 = 0; b4 = 0
      b2 = b2 + step
    }
    b2 = 0; b3 = 0; b4 = 0
    b1 = b1 + step
  }
  return(max_vector)
}


# Создаем бенчмарки для фондов
bench_for_fund1 = function(type, fund){
  if (type == 'eq') { fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  
  if (length(fund_data) > 18) {
    fund_data = fund_data[1:18]
    imoex_data = IMOEX$annual.real.return[1:18]
    rgbi_data = RGBI$annual.real.return
    rucbitr_data = RUCBITR$annual.real.return
  } else {
    imoex_data = IMOEX$annual.real.return[1:length(fund_data)]
    rgbi_data = RGBI$annual.real.return[1:length(fund_data)]
    rucbitr_data = RUCBITR$annual.real.return[1:length(fund_data)]
  }
  result = style_analysis1(type, fund)
  df = data.frame(Year = 2020:(2020 - length(fund_data) + 1), 
                  annual.real.return = c(imoex_data*result[1] + 
                                           rgbi_data*result[2] +
                                           rucbitr_data*result[3]))
 
  return(df)
}

bench_for_fund2 = function(type, fund){
  if (type == 'eq') { fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  
  if (type == 'eq') {
    bench = IMOEX$annual.real.return
  } else if (type == 'b') {
    bench = RGBI$annual.real.return*0.15 + RUCBITR$annual.real.return*0.85
  } else {
    bench = IMOEX$annual.real.return[1:18]*0.45 + RUCBITR$annual.real.return*0.45 +
      RGBI$annual.real.return*0.1
  }
  
  fund_data = fund_data[1:min(length(fund_data), length(bench))]
  bench = bench[1:min(length(fund_data), length(bench))]
  
  return(bench)
}

######### Коэффициенты ########
Sharpe_ratio = function(type, fund){
  if (type == 'eq') { fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  bench = bench_for_fund1(type, fund)
  
  fund_data = fund_data[1:min(length(fund_data), nrow(bench))]
  bench = bench[1:min(length(fund_data), nrow(bench)), ]
  
  return( (mean(fund_data) - mean(bench$annual.real.return))
          / sd(fund_data) )
}

beta_ratio = function(type, fund){
  if (type == 'eq') { fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  bench = bench_for_fund1(type, fund)
  
  fund_data = fund_data[1:min(length(fund_data), nrow(bench))]
  bench = bench[1:min(length(fund_data), nrow(bench)), ]
  
  return( cov(fund_data, bench$annual.real.return) / var(bench$annual.real.return) )
}

Treynor_ratio = function(type, fund){
  if (type == 'eq') { fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  bench = bench_for_fund1(type, fund)
  
  fund_data = fund_data[1:min(length(fund_data), nrow(bench))]
  bench = bench[1:min(length(fund_data), nrow(bench)), ]
  
  return( (mean(fund_data) - mean(bench$annual.real.return))/ beta_ratio(type, fund) )
}

alpha_ratio = function(type, fund){
  if (type == 'eq') { fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  bench = bench_for_fund(type, fund)
  
  fund_data = fund_data[1:min(length(fund_data), nrow(bench))]
  bench = bench[1:min(length(fund_data), nrow(bench)), ]
  
  return(mean(fund_data) - (mean(RGBI$annual.real.return) + 
        beta_ratio(type, fund) * (mean(bench$annual.real.return) - mean(RGBI$annual.real.return))))
}

information_ratio = function(type, fund){
  if (type == 'eq') { fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  bench = bench_for_fund(type, fund)
  
  
  fund_data = fund_data[1:min(length(fund_data), nrow(bench))]
  bench = bench[1:min(length(fund_data), nrow(bench)), ]
  
  residual = fund_data - alpha_ratio(type, fund) - 
             beta_ratio(type, fund)*bench$annual.real.return
  return( alpha_ratio(type, fund) / sd(residual))
}

R_squared = function(type, fund){
  if (type == 'eq') { fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  
  if (length(fund_data) > 16) fund_data = fund_data[1:16]
  
  return( var(bench_for_fund1(type, fund)[, 2]) / var(fund_data) )
}

R_squared1 = function(type, fund){
  if (type == 'eq') { fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  
  if (length(fund_data) > 16) {
    fund_data = fund_data[1:16]
    imoex_data = IMOEX$annual.real.return
    rgbi_data = RGBI$annual.real.return
    rucbitr_data = RUCBITR$annual.real.return
  } else {
    imoex_data = IMOEX$annual.real.return[1:length(fund_data)]
    rgbi_data = RGBI$annual.real.return[1:length(fund_data)]
    rucbitr_data = RUCBITR$annual.real.return[1:length(fund_data)]
  }
  
  if (type == 'eq') {
    bench = imoex_data
  } else if (type == 'b') {
    bench = rgbi_data*0.15 + rucbitr_data*0.85
  } else {
    bench = imoex_data*0.45 + rucbitr_data*0.45 +
            rgbi_data*0.1
  }
  return( var(bench)/var(fund_data) )
}








 
############ Таблица коэффициентов
names = c('alpha.eq', 'raif.eq', 'vtb.eq', 'sber.eq', 'promsv.eq',
          'alpha.b', 'raif.b', 'vtb.b', 'sber_p.b', 'sber_m.b',
          'alpha.m', 'vtb.m', 'sber.m')

l = list()
for (fund in names){
  type = str_split(fund, '\\.', 2, T)[2]
  if (type == 'eq') { fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  
  l[[fund]] = c(Sharpe_ratio(type, fund), sd(fund_data), R_squared(type, fund))
}

t = data.frame(matrix(rep(0, 3), ncol = 3))
for (i in 1:length(l)){
  t = rbind(t, l[[i]])
}
t = t[-1, ]
row.names(t) = names
t = setDT(t, keep.rownames = TRUE)[]
colnames(t) = c('Funds', 'Sharpe ratio', 'Volatility', 'R-squared')



t %>% kbl(caption = 'Fund`s Performance') %>%
  kable_styling(full_width = T, html_font = "Cambria Math") #kable_styling



######
l1 = list()
for (fund in names){
  type = str_split(fund, '\\.', 2, T)[2]
  if (type == 'eq') {
    fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund] 
  } else if (type == 'b') { 
    fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund] 
  } else fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
  l1[[fund]] = round(c(R_squared1(type,fund) , R_squared(type, fund)), 3)
}

t1 = data.frame(matrix(rep(0, 2), ncol = 2))
for (i in 1:length(l1)){
  t1 = rbind(t1, l1[[i]])
}
t1 = t1[-1, ]
row.names(t1) = names
t1 = setDT(t1, keep.rownames = TRUE)[]
t1 = t1 %>% add_column(c('Equities', rep('', 4), 'Bonds', rep('', 4), 'Mixed', rep('', 2)), .before = 1)
colnames(t1) = c('Type', 'Funds', 'Default', 'Adjusted')



log1 = t1$Default > t1$Adjusted
log2 = t1$Default < t1$Adjusted
t1$Default = cell_spec(t1$Default, color = ifelse(log1, 'green', 'black'), 
                       bold = ifelse(log1, T, F) )
t1$Adjusted = cell_spec(t1$Adjusted, color = ifelse(log2, 'green', 'black'),
                       bold = ifelse(log2, T, F))
rm('log1')
t1 %>% kbl(escape = F, caption = 'Comparison of R-squared') %>%
  kable_paper(c('condensed', 'hover'), full_width = T, font_size = 18,
              html_font = "Cambria Math") %>%
  column_spec(1, bold = T)



##########
names = c('alpha.eq', 'raif.eq', 'vtb.eq', 'sber.eq', 'promsv.eq',
          'alpha.b', 'raif.b', 'vtb.b', 'sber_p.b', 'sber_m.b',
          'alpha.m', 'vtb.m', 'sber.m')

l3 = list()
for (fund in names){
  type = str_split(fund, '\\.', 2, T)[2]
  if (type == 'eq') { 
    fund_data = t.eq$annual.growth.rate.of.real.returns[t.eq$Fund == fund]
    bench = IMOEX$annual.real.return
  } else if (type == 'b') { 
    fund_data = t.b$annual.growth.rate.of.real.returns[t.b$Fund == fund]
    bench = RGBI$annual.real.return*0.15 + RUCBITR$annual.real.return*0.85
  } else {
    fund_data = t.m$annual.growth.rate.of.real.returns[t.m$Fund == fund]
    bench = IMOEX$annual.real.return[1:18]*0.45 + RUCBITR$annual.real.return*0.45 +
      RGBI$annual.real.return*0.1
  }
  
  fund_data = fund_data[1:min(length(fund_data), length(bench))]
  bench = bench[1:min(length(fund_data), length(bench))]
  
  l3[[fund]] = c(sd(fund_data), sd(bench_for_fund1(type, fund)[, 2]), 
                 sd(bench_for_fund2(type, fund)))
}

t3 = data.frame(matrix(rep(0, 3), ncol = 3))
for (i in 1:length(l3)){
  t3 = rbind(t3, l3[[i]])
}
t3 = t3[-1, ]
t3 = cbind(t3, rep(0, 13))
class(t3$`rep(0, 13)`) = 'list'

for (idx in 1:nrow(t3)){
  type = str_split(names[idx], '\\.', 2, T)[2]
  fund = str_split(names[idx], '\\.', 2, T)[1]
  t3$`rep(0, 13)`[idx] = list(style_analysis1(type, paste(fund, '.', type, sep = '')))
}
row.names(t3) = names
t3 = setDT(t3, keep.rownames = TRUE)[]
colnames(t3) = c('Funds', 'Fund sd', 'Adjusted sd', 'Default sd', 'Weights')
t3$`Adjusted sd`[4] = t3$`Default sd`[4]


t3 %>% kbl(caption = 'Fund`s Performance') %>%
  kable_styling(full_width = T, html_font = "Cambria Math", font_size = 18) %>%
  column_spec(1, bold = T)#kable_styling


  
