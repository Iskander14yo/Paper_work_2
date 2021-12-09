chain_index = function(vector){
  chain_vec = na.omit(c(1, vector/100 + 1))
  return( cumprod(chain_vec) )
}

# график накопленной доходности
cum_df = data.frame(equities = rep(0, length(IMOEX$annual.nominal.return[-c(1:5)]) + 1))
cum_df$equities = chain_index(rev(IMOEX$annual.nominal.return)[-c(1:5)])
cum_df$corp_bonds = c( chain_index(rev(RUCBITR$annual.nominal.return)))
cum_df$gov_bonds = c( chain_index(rev(RGBI$annual.nominal.return)))
cum_df$inflation = chain_index(rev(inflation)[-(1:6)])
cum_df$Year = 2002:2020

ggplot(cum_df, aes(x = Year, group = 1)) +
  geom_line(aes(y = equities, group = 1, colour = 'equities'), size = 1.5) +
  geom_line(aes(y = corp_bonds, group = 1, colour = 'corp_bonds'), na.rm = T, size = 1.5) +
  geom_line(aes(y = gov_bonds, group = 1, colour = 'gov_bonds'), na.rm = T, size = 1.5) +
  geom_line(aes(y = inflation, colour = 'inflation'), size = 1.5) +
  scale_y_continuous(trans = 'log10') +
  ylab('') + ggtitle('Cumulative index value', 'start = 2002; log scale') +
  scale_colour_manual("", 
                      breaks = c("equities", "corp_bonds", "gov_bonds", 'inflation'),
                      values = c("darkred", "darkgreen", "blue", 'black')) +
  theme(legend.position = c(0.1, 0.8),
        axis.text=element_text(size = 12, face = 'bold'), 
        legend.text = element_text(size = 12, face = 'bold.italic'))


cum_df1 = data.frame(equities = rep(0, length(IMOEX$annual.real.return[-c(1:5)]) + 1))
cum_df1$equities = chain_index(rev(IMOEX$annual.real.return)[-c(1:5)])
cum_df1$corp_bonds = c( chain_index(rev(RUCBITR$annual.real.return)) )
cum_df1$gov_bonds = c( chain_index(rev(RGBI$annual.real.return)) )
cum_df1$inflation = rep(1, 19)
cum_df1$Year = 2002:2020

ggplot(cum_df1, aes(x = Year, group = 1)) +
  geom_line(aes(y = equities, group = 1, colour = 'equities'), size = 1.5) +
  geom_line(aes(y = corp_bonds, group = 1, colour = 'corp_bonds'), na.rm = T, size = 1.5) +
  geom_line(aes(y = gov_bonds, group = 1, colour = 'gov_bonds'), na.rm = T, size = 1.5) +
  geom_line(aes(y = inflation, colour = 'inflation'), size = 1.5) +
  scale_y_continuous(trans = 'log2') +
  ylab('') + ggtitle('Cumulative returns on asset classes in real terms', 'start = 2002; log scale') +
  scale_colour_manual("", 
                      breaks = c("equities", "corp_bonds", "gov_bonds", 'inflation'),
                      values = c("darkred", "darkgreen", "blue", 'black')) +
  theme(legend.position = c(0.1, 0.8),
        axis.text=element_text(size = 12, face = 'bold'), 
        legend.text = element_text(size = 12, face = 'bold.italic'))



######## 
table = data.frame('geometric mean' = rep(0, 3))
table['geometric mean'] = (c(cum_df1$equities[19], cum_df1$corp_bonds[19],
                         cum_df1$gov_bonds[19]) ** (1/18) - 1)*100
table['arithmetic mean'] = c(mean(IMOEX$annual.real.return), mean(RUCBITR$annual.real.return),
                          mean(RGBI$annual.real.return))
table['standard deviation'] = c(sd(IMOEX$annual.real.return), sd(RUCBITR$annual.real.return),
                             sd(RGBI$annual.real.return))
table$type = c('equities', 'corp_bonds', 'gov_bonds')
table$geometric.mean = NULL

table = table %>%
   pivot_longer(!type, names_to = 'measure', values_to = 'values')



p = table %>%
  group_by(type) %>%
  ggplot(aes(measure, values)) + geom_col(aes(fill = type), position = 'dodge')
p + geom_text(aes(x=measure, y = values + 1, group = type, 
                  label=format(values, nsmall = 0, scientific = FALSE, digits = 2)),
                  position=position_dodge(.9), color = 'blue') + 
    labs(title = 'Performane of assets classes', y='', x = '') + 
    theme(legend.position = c(0.1, 0.8),
          axis.text=element_text(size = 12, face = 'bold'), 
          legend.text = element_text(size = 12, face = 'bold.italic'))
  


##############
geom_mean = function(vector){
  return( ( chain_index(vector)[length(vector) + 1] ** (1/length(vector)) - 1)*100 )
}

t.eq$type = 'eq'
t.b$type = 'b'
t.m$type = 'm'
total_table = rbind(t.eq, t.b, t.m)
total_table = total_table %>%
    group_by(Fund) %>%
    summarize('type' = type[1],
              'arithmetic mean' = mean(annual.growth.rate.of.real.returns),
              'geometric mean' = geom_mean(rev(annual.growth.rate.of.real.returns)),
              'standard deviation' = sd(annual.growth.rate.of.real.returns),
              'beta ratio' = beta_ratio(type[1], Fund[1]),
              'sharpe ratio' = Sharpe_ratio(type[1], Fund[1]),
              ) %>%
    arrange(type)

t1 = cbind(total_table[, c(1,2)], round(total_table[, -c(1:2)], 3))

t1$`sharpe ratio` = cell_spec(t1$`sharpe ratio`, 
                              color = ifelse(t1$`sharpe ratio` > 0, 'green', 'red'))
t1$`beta ratio` = cell_spec(t1$`beta ratio`, 
                            color = ifelse(t1$`beta ratio` >= 1, 'green', 'red'))
t1$`arithmetic mean` = cell_spec(t1$`arithmetic mean`, 
                            color = ifelse(t1$`arithmetic mean` > 0, 'black', 'red'))
t1$`geometric mean` = cell_spec(t1$`geometric mean` , 
                                 color = ifelse(t1$`geometric mean`  > 0, 'black', 'red'))

t1[1:5, 2] = 'bonds'
t1[6:10, 2] = 'equities'
t1[11:13, 2] = 'mixed'


t1 %>% kbl(escape = F, caption = 'Mutual fonds` performance', ) %>%
  kable_paper(c('condensed', 'hover'), full_width = T, font_size = 18,
              html_font = "Cambria Math") %>%
  column_spec(1, bold = T)






