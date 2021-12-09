




















# # # # # # # # # 

# отобразим (в виде гистограммы, например) ежегодную доходность сначала для фондов, затем для индексов.
# датасеты, содержащие данные о средней доходности за год по каждому классу активов
t1.eq = t.eq %>%
  group_by(year) %>%
    summarize(mean = mean(annual.growth.rate.of.real.returns))
t1.b = t.b %>%
  group_by(year) %>%
    summarize(mean = mean(annual.growth.rate.of.real.returns))
t1.m = t.m %>%
  group_by(year) %>%
    summarize(mean = mean(`annual.growth.rate.of.real.returns`))

df = data.frame(name = c(rep('eq', 20), rep('bonds', 20), rep('mixed', 20)), 
                year = t1.eq$year[-21], mean = c(t1.eq$mean[-21], t1.b$mean[-21], t1.m$mean))
df %>%
  group_by(name) %>%
    ggplot(data = ., aes(x = year, y = mean, fill = name)) + 
    geom_bar(position = position_dodge2(width = 0.5, padding = 0.01), stat = 'identity') + 
    geom_vline(xintercept = 2000:2019, color = 'grey', alpha = 0.7) + 
    xlab('years') + ylab('arithmetic mean per year')

df1 = data.frame(name = c(rep('eq', 16), rep('bonds', 16), rep('mixed', 16)),
                 year = RGBI$year, mean = c(IMOEX$annual.real.return, RGBI$annual.real.return, mixed_index$annual.real.return))
df1 %>%
  group_by(name) %>%
   ggplot(data = ., aes(x = year, y = mean, fill = name)) + 
    geom_bar(position = position_dodge2(width = 0.9, padding = 0.001, preserve = 'total'), stat = 'identity') + 
    geom_vline(xintercept = 2005:2019, color = 'grey') + 
    xlab('years') + ylab('arithmetic mean per year')

# # Худшие года для каждого класса активов
worst.eq = t1.eq %>%
  arrange(mean) %>%
   head(5) %>%
    ungroup()
worst.eq$year = as.character(worst.eq$year)
ggplot() +
  geom_bar(data = worst.eq, aes(x = reorder(year, -mean), y = mean, fill = 'red'), stat = 'identity') + 
  theme(
    axis.text = element_text(size = 14, color = "black")
  ) + ggtitle(label = 'Equity Funds') + labs(x = 'year') + theme(legend.position="none")

worst.b = t1.b %>%
  arrange(mean) %>%
    head(5)
worst.b$year = as.character(worst.b$year)
ggplot() +
  geom_bar(data = worst.b, aes(x = reorder(year, -mean), y = mean, fill = 'red'), stat = 'identity') + 
  theme(
    axis.text = element_text(size = 14, color = "black")
  ) + ggtitle(label = 'Bonds Funds') + labs(x = 'year') + theme(legend.position="none")

worst.m = t1.m %>%
  arrange(mean) %>%
    head(5)
worst.m$year = as.character(worst.m$year)
ggplot() +
  geom_bar(data = worst.m, aes(x = reorder(year, -mean), y = mean, fill = 'red'), stat = 'identity')  + 
  theme(
    axis.text = element_text(size = 14, color = "black")
  ) + ggtitle(label = 'Mixed Funds') + labs(x = 'year') + theme(legend.position="none")
# # # # #   

chain_index = function(vector){
  chain_vec = c(1, vector/100 + 1)
  return( cumprod(chain_vec) )
}

t1.eq = data.frame(t1.eq, chain.index = chain_index(t1.eq$mean))
t1.b = data.frame(t1.b, chain.index = chain_index(t1.b$mean))
t1.m = data.frame(t1.m, chain.index = chain_index(t1.m$mean))

IMOEX = data.frame(IMOEX, chain.index = rev(chain_index(rev(IMOEX$annual.real.return))))
RGBI = data.frame(RGBI, chain.index = rev(chain_index(rev(RGBI$annual.real.return))))
mixed_index = data.frame(mixed_index, chain.index = rev(chain_index(rev(mixed_index$annual.real.return))))
# # # #  Рассчитать цепную доходность для индексов
# посчитать таблицу для каждого класса активов
# построить графики с фондами и бенчмарками

f <- function(vector){
  value = vector[length(vector)]
  return( (value ** (1/length(vector)) - 1) * 100 )
}
# считает геометрическое среднее

f1 = function(sd, len = 20){
  return( sd / sqrt(len))
}
# считает стандартную ошибку

f2 = function(dataset, arg){
  if (arg == 'min') value = min(dataset[, 2, drop = T])
  if (arg == 'max') value = max(dataset[, 2, drop = T])
  row = which(dataset[, 2] == value)
  return(dataset[row, 1])
}
# считает лучший и худший год

performance = data.frame(asset = c('equities', 'bonds', 'mixed'), 
                         arithmetic.mean = c(mean(t1.eq$mean), mean(t1.b$mean), mean(t1.m$mean)),
                         geometric.mean = c(f(t1.eq$chain.index), f(t1.b$chain.index), f(t1.m$chain.index)),
                         standard.deviation = c(sd(t1.eq$mean), sd(t1.b$mean), sd(t1.m$mean)),
                         standard.error = c(f1(sd(t1.eq$mean)), f1(sd(t1.b$mean)), f1(sd(t1.m$mean))),
                         minimum.return = c(min(t1.eq$mean), min(t1.b$mean), min(t1.m$mean)),
                         minimum.year = c(f2(t1.eq, 'min'), f2(t1.b, 'min'), f2(t1.m, 'min')),
                         maximum.return = c(max(t1.eq$mean), max(t1.b$mean), max(t1.m$mean)),
                         maximum.year = c(f2(t1.eq, 'max'), f2(t1.b, 'max'), f2(t1.m, 'max')))

f3 = function(dataset, arg){
  if (arg == 'min')  {d = dataset %>%
    group_by(Fund) %>%
      summarize(min = min(annual.growth.rate.of.real.returns))}
  else d = dataset %>%
    group_by(Fund) %>%
    summarize(min = max(annual.growth.rate.of.real.returns))
  vector = dataset[, 1, drop = T]
  vec = c()
  for (i in 1:nrow(d)){
    row = which(dataset[, 4] == as.numeric(d[i, 2])) 
    vec = c(vec, vector[row])
  }
  return(vec)
}
  
p1 = t.eq %>%
  group_by(Fund) %>%
    summarize(arithmetic.mean = mean(annual.growth.rate.of.real.returns),
              geometric.mean = NA,
              standard.deviation = sd(annual.growth.rate.of.real.returns),
              standard.error = f1(sd(annual.growth.rate.of.real.returns)),
              minimum.return = min(annual.growth.rate.of.real.returns),
              maximum.return = max(annual.growth.rate.of.real.returns)) %>%
  mutate(minimum.year = f3(t.eq, arg = 'min'), .before = maximum.return) %>%
  mutate(maximum.year = f3(t.eq, arg = 'max')) %>%
  rename('asset' = Fund)

p = full_join(x = performance, y = p1, by = NULL)
# # 

p2 = t.b %>%
  group_by(Fund) %>%
  summarize(arithmetic.mean = mean(annual.growth.rate.of.real.returns),
            geometric.mean = NA,
            standard.deviation = sd(annual.growth.rate.of.real.returns),
            standard.error = f1(sd(annual.growth.rate.of.real.returns)),
            minimum.return = min(annual.growth.rate.of.real.returns),
            maximum.return = max(annual.growth.rate.of.real.returns)) %>%
  mutate(minimum.year = f3(t.b, arg = 'min'), .before = maximum.return) %>%
  mutate(maximum.year = f3(t.b, arg = 'max')) %>%
  rename('asset' = Fund)

p = full_join(x = p, y = p2, by = NULL)
# # 

p3 = t.m %>%
    group_by(Fund) %>%
    summarize(arithmetic.mean = mean(annual.growth.rate.of.real.returns),
              geometric.mean = NA,
              standard.deviation = sd(annual.growth.rate.of.real.returns),
              standard.error = f1(sd(annual.growth.rate.of.real.returns)),
              minimum.return = min(annual.growth.rate.of.real.returns),
              maximum.return = max(annual.growth.rate.of.real.returns)) %>%
    mutate(minimum.year = f3(t.m, arg = 'min'), .before = maximum.return) %>%
    mutate(maximum.year = f3(t.m, arg = 'max')) %>%
    rename('asset' = Fund)

p = full_join(x = p, y = p3, by = NULL)
p %>% kbl(caption = 'Performance') %>%
  kable_classic(full_width = T, html_font = "Cambria") %>%
  row_spec(1:3, bold = T)

  


# # # # # 
# Доходность пифов против индексов
ggplot() +
  geom_line(data = t1.eq, aes(x = year, y = chain.index*100, color = 'blue')) + 
  geom_line(data = IMOEX, aes(x = year, y = chain.index*100, color = 'orange')) +
  xlab("year") + ylab('Real return') + 
  theme(
    axis.text = element_text(size = 12, color = "black")
  ) + ggtitle(label = 'Equities') + 
  scale_color_manual(name = "", values = c("blue","orange"), labels = c('Funds', 'Index'))

ggplot() +
  geom_line(data = t1.b, aes(x = year, y = chain.index*100, color = 'blue')) + 
  geom_line(data = RGBI, aes(x = year, y = chain.index*100, color = 'orange')) +
  xlab("year") + ylab('Real return') +
  xlab("year") + ylab('Real return') + 
  theme(
    axis.text = element_text(size = 12, color = "black")
  ) + ggtitle(label = 'Bonds') + 
  scale_color_manual(name = "", values = c("blue","orange"), labels = c('Funds', 'Index'))

ggplot() +
  geom_line(data = t1.m, aes(x = year, y = chain.index*100, color = 'blue')) + 
  geom_line(data = mixed_index, aes(x = year, y = chain.index*100, color = 'orange')) +
  xlab("year") + ylab('Real return') + 
  theme(
    axis.text = element_text(size = 12, color = "black")
  ) + ggtitle(label = 'Mixed funds') + 
  scale_color_manual(name = "", values = c("blue","orange"), labels = c('Funds', 'Index'))

