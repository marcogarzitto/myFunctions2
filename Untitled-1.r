

AAA <- data.frame(poly = rep(c('a', 'b', 'c', 'd'), 10))

AAA <- cbind(AAA, my_in_categorical_variable(name_in = 'poly', df_in = AAA, levels_in_to_exclude = c('d'), name_out = 'poly2', label_out = 'POLLO', levels_out = c('A', 'B', 'C')))

