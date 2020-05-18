### PIB
# Série
empregoPV %>% 
     filter(tipo == "PIB") %>% 
     mutate(mes = floor_date(mes, "3 months")) %>% 
     group_by(mes) %>% 
     summarise(PIB = mean(valores)) %>% 
     ggplot(aes(x = mes, y = PIB)) +
     geom_line() + 
     geom_point() +
     scale_x_date(date_breaks = "year", date_labels = "%Y") +
     labs(title = "PIB", x = "", y = "") +
     scale_y_continuous() +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           axis.text.x = element_text(angle = 0, hjust = 0.5))





# Média móvel
empregoPV %>% 
     filter(tipo == "PIB") %>% 
     mutate(mes = floor_date(mes, "3 months")) %>% 
     group_by(mes) %>% 
     summarise(valores = mean(valores)) %>% 
     ggplot(aes(x = mes, y = valores)) + 
     geom_line(size = .7) +
     geom_ma(aes(colour = 'MM3'), ma_fun = SMA, n = 3, size = 1, show.legend = TRUE) + # media movel com ordem 3
     geom_ma(aes(colour = 'MM5'), ma_fun = SMA, n = 5, size = 1, show.legend = TRUE) + # media movel com ordem 5
     geom_ma(aes(colour = 'MM12'), ma_fun = SMA, n = 12, size = 1, show.legend = TRUE) + # media movel com ordem 12
     labs(title = "Predição dos valores do PIB", subtitle = "(média móvel simples)", x = "", y = "") +
     scale_colour_manual(name = "", values = c('MM3' = "red",
                                               'MM5' = "blue",
                                               'MM12' = "darkgreen"), labels = c("MMS(12)",
                                                                                 "MMS(3)",
                                                                                 "MMS(5)")) +
     scale_x_date(breaks = "year", date_labels = "%Y") +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           legend.position = "bottom")
ggsave(filename = "PIB_MédiaMovel.png", path = "~/GitHub/estatisticaR/TEP_ts", width = 20, height = 12, units = "cm")



# Alisamento exponencial
empregoPV %>% 
     filter(tipo == "PIB") %>% 
     mutate(mes = floor_date(mes, "3 months")) %>% 
     group_by(tipo, mes) %>% 
     summarise(valores = mean(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            ETS = map(.x = serie, .f = ets, model = "AAA"),
            prev = map(ETS, forecast, h = 16, levels = c(85,90)),
            tidy = map(ETS, sw_tidy),
            glance = map(ETS, sw_glance),
            augment = map(ETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            sweep = map(prev, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>% 
     unnest(glance)


empregoPV %>% 
     filter(tipo == "Taxa de Câmbio") %>% 
     group_by(tipo, mes) %>% 
     summarise(valores = mean(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            ETS = map(.x = serie, .f = ets, model = "MAN"),
            prev = map(ETS, forecast, h = 16, levels = c(85,90)),
            tidy = map(ETS, sw_tidy),
            glance = map(ETS, sw_glance),
            augment = map(ETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            sweep = map(prev, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>% 
     unnest(sweep) %>% 
     ggplot(aes(x = index, y = valores, colour = key)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line() +
     labs(title = "Previsão para o PIB",
          subtitle = "(MAN)", x = "", y = "") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           axis.text.x = element_text(angle = 45, hjust = 1),
           legend.position = "bottom")


empregoPV %>% 
     filter(tipo == "PIB") %>% 
     mutate(mes = floor_date(mes, "3 months")) %>% 
     group_by(tipo, mes) %>% 
     summarise(valores = mean(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            treino = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2017,
                        freq = 12),
            ETS = map(.x = serie, .f = ets, model = "AAA"),
            prev = map(ETS, forecast, h = 16, levels = c(85,90)),
            tidy = map(ETS, sw_tidy),
            glance = map(ETS, sw_glance),
            augment = map(ETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            sweep = map(prev, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>% 
     unnest(sweep) %>% 
     ggplot(aes(x = index, y = valores, colour = key)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line() +
     labs(title = "Previsão para o PIB",
          subtitle = "(AAA)", x = "", y = "") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           axis.text.x = element_text(angle = 45, hjust = 1),
           legend.position = "bottom")


pib = ts(emprego$PIB, start = c(2005,1), end = c(2017, 12), frequency = 12)
pibTreino = window(pib, 2005, c(2016, 12))
pibTeste = window(pib, 2017, c(2017, 12))



### Inflação
# Série
emprego %>% 
     ggplot(aes(x = mes, y = inflacao)) +
     geom_line() + 
     geom_point() +
     scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
     labs(title = "Inflação", x = "", y = "") +
     scale_y_continuous() +
     theme(plot.title = element_text(size = 16, hjust = 0.5))


# Média móvel
empregoPV %>% 
     filter(tipo == "Inflação") %>% 
     group_by(mes) %>% 
     summarise(valores = mean(valores)) %>% 
     ggplot(aes(x = mes, y = valores)) + 
     geom_line(size = .7) +
     geom_ma(aes(colour = 'MM3'), ma_fun = SMA, n = 3, size = 1, show.legend = TRUE) + # media movel com ordem 3
     geom_ma(aes(colour = 'MM5'), ma_fun = SMA, n = 5, size = 1, show.legend = TRUE) + # media movel com ordem 5
     geom_ma(aes(colour = 'MM12'), ma_fun = SMA, n = 12, size = 1, show.legend = TRUE) + # media movel com ordem 12
     labs(title = "Predição dos valores da Inflação", subtitle = "(média móvel simples)", x = "", y = "") +
     scale_colour_manual(name = "", values = c('MM3' = "red",
                                               'MM5' = "blue",
                                               'MM12' = "darkgreen"), labels = c("MMS(12)",
                                                                                 "MMS(3)",
                                                                                 "MMS(5)")) +
     scale_x_date(breaks = "year", date_labels = "%Y") +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           legend.position = "bottom")
ggsave(filename = "Inflação_MédiaMovel.png", path = "~/GitHub/estatisticaR/TEP_ts", width = 20, height = 12, units = "cm")


# Alisamento exponencial
empregoPV %>% 
     filter(tipo == "Inflação") %>% 
     group_by(tipo, mes) %>% 
     summarise(valores = mean(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            ETS = map(.x = serie, .f = ets, model = "ANN"),
            prev = map(ETS, forecast, h = 16, levels = c(85,90)),
            tidy = map(ETS, sw_tidy),
            glance = map(ETS, sw_glance),
            augment = map(ETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            sweep = map(prev, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>% 
     unnest(sweep) %>% 
     ggplot(aes(x = index, y = valores, colour = key)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line() +
     labs(title = "Previsão para o inflação",
          subtitle = "(ANN)", x = "", y = "") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           axis.text.x = element_text(angle = 45, hjust = 1),
           legend.position = "bottom")



### Taxa de juros
# Série
emprego %>% 
     mutate(ano = year(mes)) %>% 
     group_by(mes) %>% 
     mutate(taxaJurosMes = mean(taxaJuros)) %>% 
     group_by(ano) %>% 
     mutate(taxaJurosAno = mean(taxaJuros)) %>% 
     ggplot(aes(x = mes)) +
     geom_line(aes(y = taxaJurosMes)) + 
     geom_point(aes(y = taxaJurosMes)) +
     scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
     labs(title = "Taxa de Juros", x = "", y = "") +
     scale_y_continuous() +
     theme(plot.title = element_text(size = 16, hjust = 0.5))



# Média móvel simples



# Alisamento exponencial
empregoPV %>% 
     filter(tipo == "Taxa de Juros") %>% 
     group_by(tipo, mes) %>% 
     summarise(valores = mean(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            ETS = map(.x = serie, .f = ets, model = "MAM"),
            prev = map(ETS, forecast, h = 16, levels = c(85,90)),
            tidy = map(ETS, sw_tidy),
            glance = map(ETS, sw_glance),
            augment = map(ETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            sweep = map(prev, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>% 
     unnest(sweep) %>% 
     ggplot(aes(x = index, y = valores, colour = key)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line() +
     labs(title = "Previsão para o taxa de juros",
          subtitle = "(MAM)", x = "", y = "") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           axis.text.x = element_text(angle = 45, hjust = 1),
           legend.position = "bottom")




# Taxa de câmbio
emprego %>% 
     ggplot(aes(x = mes, y = taxaCambio)) +
     geom_line() + 
     geom_point() +
     scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
     labs(title = "Taxa de Câmbio", x = "", y = "") +
     scale_y_continuous() +
     theme(plot.title = element_text(size = 16, hjust = 0.5))


# Alisamento exponencial
empregoPV %>% 
     filter(tipo == "Taxa de Câmbio") %>% 
     group_by(tipo, mes) %>% 
     summarise(valores = mean(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            ETS = map(.x = serie, .f = ets, model = "ANN"),
            prev = map(ETS, forecast, h = 16, levels = c(85,90)),
            tidy = map(ETS, sw_tidy),
            glance = map(ETS, sw_glance),
            augment = map(ETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            sweep = map(prev, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>% 
     unnest(sweep) %>% 
     ggplot(aes(x = index, y = valores, colour = key)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line() +
     labs(title = "Previsão para o taxa de câmbio",
          subtitle = "(ANN)", x = "", y = "") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           axis.text.x = element_text(angle = 45, hjust = 1),
           legend.position = "bottom")




# Patentes
emprego %>% 
     ggplot(aes(x = mes, y = patentes)) +
     geom_line() + 
     geom_point() +
     scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
     labs(title = "Patentes", x = "", y = "") +
     scale_y_continuous() +
     theme(plot.title = element_text(size = 16, hjust = 0.5))


empregoPV %>% 
     filter(tipo == "Patentes") %>% 
     group_by(tipo, mes) %>% 
     summarise(valores = mean(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            ETS = map(.x = serie, .f = ets, model = "MAM"),
            prev = map(ETS, forecast, h = 16, levels = c(85,90)),
            tidy = map(ETS, sw_tidy),
            glance = map(ETS, sw_glance),
            augment = map(ETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            sweep = map(prev, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>% 
     unnest(sweep) %>% 
     ggplot(aes(x = index, y = valores, colour = key)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line() +
     labs(title = "Previsão para patentes",
          subtitle = "(MAM)", x = "", y = "") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           axis.text.x = element_text(angle = 45, hjust = 1),
           legend.position = "bottom")
















previsoes <- function(){
     setwd("~/GitHub/estatisticaR/TEP_ts")
     
     colnames(emprego) = c("mes",  "PIB", "Inflação", "Taxa de Juros", "Taxa de Câmbio", "Patentes", "saldoEmprego")
     
     
     empregoPV <- emprego %>% 
          select(-saldoEmprego) %>% 
          pivot_longer(cols = c(PIB, "PIB", "Inflação", "Taxa de Juros", "Taxa de Câmbio", "Patentes"), names_to = "tipo", values_to = "valores")
     
     tipo1 <- c("PIB", "Inflação")
     modelos1 <- c("ANN", "AAN", "AAA")
     tipo2 <- c("Taxa de Juros", "Taxa de Câmbio", "Patentes")
     modelos2 <- c("ANN", "AAN", "AAA", "MAM", "ANA", "MNM")
     
     
     for(a in tipo1){
          for(b in modelos1){
               empregoPV %>% 
                    filter(tipo == a) %>% 
                    group_by(tipo, mes) %>% 
                    summarise(valores = mean(valores)) %>% 
                    nest() %>% 
                    mutate(serie = map(.x = data,
                                       .f = tk_ts,
                                       select = -mes,
                                       start = 2003,
                                       freq = 12),
                           ajusteETS = map(.x = serie, .f = ets, model = b),
                           prev = map(ajusteETS, forecast, h = 16, levels = c(85,90)),
                           sweep = map(prev, sw_sweep, fitted = TRUE, timetk_idx = TRUE)) %>% 
                    unnest(sweep) %>% 
                    ggplot(aes(x = index, y = valores, colour = key)) +
                    geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                                fill = "#D5DBFF", colour = NA, size = 0) +
                    geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                                fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
                    geom_line() +
                    labs(title = paste0("Previsão para ",a),
                         subtitle = paste0("(modelo: ", b, ")"), x = "", y = "") +
                    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
                    scale_colour_tq() +
                    scale_fill_tq() + 
                    theme(plot.title = element_text(size = 16, hjust = 0.5),
                          plot.subtitle = element_text(size = 13, hjust = 0.5),
                          axis.text.x = element_text(angle = 45, hjust = 1),
                          legend.position = "bottom")
               ggsave(filename = paste0(a, "_", b, ".png"), width = 20, height = 12, units = "cm")
          }
     }
     
     
     for(c in tipo2){
          for(d in modelos2){
               empregoPV %>% 
                    filter(tipo == c) %>% 
                    group_by(tipo, mes) %>% 
                    summarise(valores = mean(valores)) %>% 
                    nest() %>% 
                    mutate(serie = map(.x = data,
                                       .f = tk_ts,
                                       select = -mes,
                                       start = 2003,
                                       freq = 12),
                           ajusteETS = map(.x = serie, .f = ets, model = d),
                           prev = map(ajusteETS, forecast, h = 16, levels = c(85,90)),
                           sweep = map(prev, sw_sweep, fitted = TRUE, timetk_idx = TRUE)) %>% 
                    unnest(sweep) %>% 
                    ggplot(aes(x = index, y = valores, colour = key)) +
                    geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                                fill = "#D5DBFF", colour = NA, size = 0) +
                    geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                                fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
                    geom_line() +
                    labs(title = paste0("Previsão para ",c),
                         subtitle = paste0("(modelo: ", d, ")"), x = "", y = "") +
                    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
                    scale_colour_tq() +
                    scale_fill_tq() + 
                    theme(plot.title = element_text(size = 16, hjust = 0.5),
                          plot.subtitle = element_text(size = 13, hjust = 0.5),
                          axis.text.x = element_text(angle = 45, hjust = 1),
                          legend.position = "bottom")
               ggsave(filename = paste0(c, "_", d, ".png"), width = 20, height = 12, units = "cm")
          }
     }
}

previsoes()




ajustes <- function(){
     setwd("~/GitHub/estatisticaR/TEP_ts")
     
     empregoPV <- emprego
     
     colnames(empregoPV) = c("mes",  "PIB", "Inflação", "Taxa de Juros", "Taxa de Câmbio", "Patentes", "saldoEmprego")
     
     
     empregoPV <- empregoPV %>% 
          select(-saldoEmprego) %>% 
          pivot_longer(cols = c("PIB", "Inflação", "Taxa de Juros", "Taxa de Câmbio", "Patentes"),
                       names_to = "tipo", values_to = "valores")
     
     tipo1 <- c("PIB", "Inflação")
     modelos1 <- c("ANN", "AAN", "AAA")
     tipo2 <- c("Taxa de Juros", "Taxa de Câmbio", "Patentes")
     modelos2 <- c("ANN", "AAN", "AAA", "MAM", "ANA", "MNM")
     
     
     for(a in tipo1){
          for(b in modelos1){
               modelo1 <- empregoPV %>% 
                    filter(tipo == "PIB") %>% 
                    group_by(tipo, mes) %>% 
                    summarise(valores = mean(valores)) %>% 
                    nest() %>% 
                    mutate(serie = map(.x = data,
                                       .f = tk_ts,
                                       select = -mes,
                                       start = 2003,
                                       freq = 12),
                           ajusteETS = map(.x = serie, .f = ets, model = b),
                           glance = map(ajusteETS, sw_glance)) %>% 
                    unnest(glance)
               
               print(modelo1)
          }
     }
     
     
     for(c in tipo2){
          for(d in modelos2){
               modelo2 <- empregoPV %>% 
                    filter(tipo == c) %>% 
                    group_by(tipo, mes) %>% 
                    summarise(valores = mean(valores)) %>% 
                    nest() %>% 
                    mutate(serie = map(.x = data,
                                       .f = tk_ts,
                                       select = -mes,
                                       start = 2003,
                                       freq = 12),
                           ajusteETS = map(.x = serie, .f = ets, model = d),
                           glance = map(ajusteETS, sw_glance)) %>% 
                    unnest(glance)
               
               print(modelo2)
          }
     }
}

ajustes()
