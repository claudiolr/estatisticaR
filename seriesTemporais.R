library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)
library(lubridate)


empregoSE <- read.csv("EmpregoSetorEconomia.csv", sep = ";", stringsAsFactors = FALSE, dec = ",")

empregoSE <- rename(empregoSE, mes = Mês)

empregoSE$mes <- paste0("01/",empregoSE$mes)
empregoSE$mes <- as.Date(empregoSE$mes, tryFormats = "%d/%b/%y")

empregoSE <- empregoSE %>% 
     pivot_longer(cols = c(Comercio, Alimento, Metalurgia), values_to = "valores", names_to = "setor")



### Exploração dos dados
empregoSE %>% 
     mutate(Mes = month(mes, label = TRUE),
            Ano = year(mes)) %>% 
     group_by(Mes, Ano) %>% 
     summarise(Total = sum(valores))


# Comércio
empregoSE %>% 
     filter(setor == "Comercio") %>% 
     ggplot(aes(x = mes, y = valores)) +
     geom_line() + 
     geom_point() +
     scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
     labs(title = "Série temporal de Comércio", x = "", y = "") +
     scale_y_continuous() +
     theme(plot.title = element_text(size = 16, hjust = 0.5))


empregoSE %>% 
     filter(setor == "Comercio") %>% 
     ggplot(aes(x = valores)) +
     geom_histogram(bins = 10, colour = "white") +
     labs(title = "Histograma de Comércio", x = "", y = "") +
     theme(plot.title = element_text(size = 16, hjust = 0.5))

empregoSE %>% 
     filter(setor == "Comercio") %>% 
     ggplot(aes(x = "")) +
     geom_boxplot(aes(y = valores)) +
     labs(title = "Histograma de Comércio", x = "", y = "") +
     theme(plot.title = element_text(size = 16, hjust = 0.5))


# Alimento
empregoSE %>% 
     filter(setor == "Alimento") %>% 
     ggplot(aes(x = mes, y = valores)) +
     geom_line() + 
     geom_point() +
     scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
     labs(title = "Série temporal de Comércio", x = "", y = "") +
     scale_y_continuous() +
     theme(plot.title = element_text(size = 16, hjust = 0.5))


empregoSE %>% 
     filter(setor == "Alimento") %>% 
     ggplot(aes(x = valores)) +
     geom_histogram(bins = 10, colour = "white") +
     labs(title = "Histograma de Comércio", x = "", y = "") +
     theme(plot.title = element_text(size = 16, hjust = 0.5))

empregoSE %>% 
     filter(setor == "Alimento") %>% 
     ggplot(aes(x = "")) +
     geom_boxplot(aes(y = valores)) +
     labs(title = "Histograma de Comércio", x = "", y = "") +
     theme(plot.title = element_text(size = 16, hjust = 0.5))


# Metalurgia
empregoSE %>% 
     filter(setor == "Metalurgia") %>% 
     ggplot(aes(x = mes, y = valores)) +
     geom_line() + 
     geom_point() +
     scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
     scale_y_continuous() +
     labs(title = "Série temporal de Metalurgia", x = "", y = "") +
     theme(plot.title = element_text(size = 16, hjust = 0.5))

empregoSE %>% 
     filter(setor == "Metalurgia") %>% 
     ggplot(aes(x = valores)) +
     geom_histogram(bins = 10, colour = "white") +
     labs(title = "Histograma de Metalurgia", x = "", y = "") +
     theme(plot.title = element_text(size = 16, hjust = 0.5))

empregoSE %>% 
     filter(setor == "Metalurgia") %>% 
     ggplot(aes(x = "")) +
     geom_boxplot(aes(y = valores)) +
     labs(title = "Histograma de Metalurgia", x = "", y = "") +
     theme(plot.title = element_text(size = 16, hjust = 0.5))



### Explorando partes das séries
periodo <- seq.Date(dmy("01-01-2003"), dmy("01-12-2004"), by = "month")

empregoSE %>% 
     filter(mes %in% periodo,
            setor == "Comercio") %>% 
     ggplot(aes(x = mes, y = valores)) +
     geom_line(na.rm = TRUE) +
     geom_point() +
     scale_x_date(date_breaks = "years", date_labels = "%Y") +
     labs(title = "Série temporal de Comércio",
          subtitle = "Recorte da série", x = "", y = "") +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5))

empregoSE %>% 
     filter(mes %in% periodo,
            setor == "Alimento") %>% 
     ggplot(aes(x = mes, y = valores)) +
     geom_line(na.rm = TRUE) +
     geom_point() +
     scale_x_date(date_breaks = "years", date_labels = "%Y") +
     labs(title = "Série temporal de Comércio",
          subtitle = "Recorte da série", x = "", y = "") +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5))

empregoSE %>% 
     filter(mes %in% periodo,
            setor == "Metalurgia") %>% 
     ggplot(aes(x = mes, y = valores)) +
     geom_line(na.rm = TRUE) +
     geom_point() +
     scale_x_date(date_breaks = "years", date_labels = "%Y") +
     labs(title = "Série temporal de Comércio",
          subtitle = "Recorte da série", x = "", y = "") +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5))


### Modelo de média móvel
empregoSE %>% 
     filter(setor == "Comercio") %>% 
     select(-setor) %>% 
     mutate(valores = tk_ts(data = valores, start = 2003, freq = 12, silent = TRUE),
            MM3 = ma(valores, order = 3),
            MM5 = ma(valores, order = 5),
            MM12 = ma(valores, order = 12))


empregoSE %>% 
     filter(setor == "Comercio") %>% 
     ggplot(aes(x = mes, y = valores)) + 
     geom_line(size = .7) +
     geom_ma(aes(colour = 'MM3'), ma_fun = SMA, n = 3, size = 1, show.legend = TRUE) + # media movel com ordem 3
     geom_ma(aes(colour = 'MM5'), ma_fun = SMA, n = 5, size = 1, show.legend = TRUE) + # media movel com ordem 5
     geom_ma(aes(colour = 'MM12'), ma_fun = SMA, n = 12, size = 1, show.legend = TRUE) + # media movel com ordem 12
     labs(title = "Predição do setor de Comércio", subtitle = "média móvel simples", x = "", y = "") +
     scale_colour_manual(name = "", values = c('MM3' = "red",
                                               'MM5' = "blue",
                                               'MM12' = "darkgreen"), labels = c("MMS(12)",
                                                                                 "MMS(3)",
                                                                                 "MMS(5)")) +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           legend.position = "bottom")



empregoSE %>% 
     filter(setor == "Alimento") %>% 
     ggplot(aes(x = mes, y = valores)) + 
     geom_line(size = .7) +
     geom_ma(aes(colour = 'MM3'), ma_fun = SMA, n = 3, size = 1, show.legend = TRUE) + # media movel com ordem 3
     geom_ma(aes(colour = 'MM5'), ma_fun = SMA, n = 5, size = 1, show.legend = TRUE) + # media movel com ordem 5
     geom_ma(aes(colour = 'MM12'), ma_fun = SMA, n = 12, size = 1, show.legend = TRUE) + # media movel com ordem 12
     labs(title = "Predição do setor de Alimento", subtitle = "média móvel simples", x = "", y = "") +
     scale_colour_manual(name = "", values = c('MM3' = "red",
                                               'MM5' = "blue",
                                               'MM12' = "darkgreen"), labels = c("MMS(12)",
                                                                                 "MMS(3)",
                                                                                 "MMS(5)")) +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           legend.position = "bottom")
     


empregoSE %>% 
     filter(setor == "Metalurgia") %>% 
     ggplot(aes(x = mes, y = valores)) + 
     geom_line(size = .7) +
     geom_ma(aes(colour = 'MM3'), ma_fun = SMA, n = 3, size = 1, show.legend = TRUE) + # media movel com ordem 3
     geom_ma(aes(colour = 'MM5'), ma_fun = SMA, n = 5, size = 1, show.legend = TRUE) + # media movel com ordem 5
     geom_ma(aes(colour = 'MM12'), ma_fun = SMA, n = 12, size = 1, show.legend = TRUE) + # media movel com ordem 12
     labs(title = "Predição do setor de Metalurgia", subtitle = "média móvel simples", x = "", y = "") +
     scale_colour_manual(name = "", values = c('MM3' = "red",
                                               'MM5' = "blue",
                                               'MM12' = "darkgreen"), labels = c("MMS(12)",
                                                                                 "MMS(3)",
                                                                                 "MMS(5)")) +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           legend.position = "bottom")


# 1. alisamento exponencial Simples ANN
# ajusta a série em função de três parâmetros (três letras: ver ajuda)
# valor de alpha próximo de 1: o peso está quase todo na última observação
# AIC, AICc e BIC: servem para comparação com outros modelos. Quanto menor, melhor.

ets(serie2, "ANN")

ANN <- empregoSE %>% 
     group_by(setor, mes) %>% 
     summarise(valores = sum(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            ajusteETS = map(.x = serie,
                            .f = ets,
                            model = "ANN"),
            tidy = map(ajusteETS, sw_tidy),
            glance = map(ajusteETS, sw_glance),
            augment = map(ajusteETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ajusteETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            previsaoETS = map(ajusteETS, forecast, h = 16, levels = c(85,90)),
            sweep = map(previsaoETS, sw_sweep, fitted = TRUE, timetk_idx = TRUE))

ANN %>% unnest(tidy)
ANN %>% unnest(glance)
ANN %>% unnest(augment)
ANN %>% unnest(decomp)

ANN %>% unnest(sweep) %>% 
     ggplot(aes(x = index, y = valores, colour = key, group = setor)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line(size = 1.3) +
     labs(title = "Previsão para os setores da economia",
          subtitle = "(Previsão de 16 meses pelo modelo de Alisamento Exponencial Simples - ANN)", x = "", y = "") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           axis.text.x = element_text(angle = 0, hjust = 0.5),
           legend.position = "bottom") +
     facet_wrap(~ setor, scales = "free_y", ncol = 3)

autoplot(forecast(ets(serie4, "ANN"), h=16,levels=c(85,90))) # AAN para metalurgia


# empregoSE %>% 
#      filter(setor == "Comercio") %>% 
#      mutate(Mes = as_date(as.yearmon(mes))) %>% 
#      group_by(setor, Mes) %>% 
#      summarise(total = sum(valores)) %>% 
#      nest() %>% 
#      mutate(series = map(.x = data,
#                          .f = tk_ts,
#                          select = -Mes,
#                          start = 2003,
#                          freq = 12),
#             ajusteETS = map(series, ets),
#             previsao = map(ajusteETS, forecast, h = 16),
#             sweep = map(previsao, sw_sweep, fitted = TRUE, timetk_idx = TRUE)) %>% 
#      unnest(sweep) %>% 
#      ggplot(aes(x = index, y = total, colour = key, group = setor)) +
#      geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
#                  fill = "#D5DBFF", colour = NA, size = 0) +
#      geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
#                  fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
#      geom_line(size = 1.3) +
#      labs(title = "Previsão para Comércio",
#           subtitle = "(Previsão pelo modelo ETS)", x = "", y = "") +
#      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#      scale_colour_tq() +
#      scale_fill_tq() + 
#      theme(plot.title = element_text(size = 16, hjust = 0.5),
#            plot.subtitle = element_text(size = 13, hjust = 0.5),
#            axis.text.x = element_text(angle = 0, hjust = 0.5),
#            legend.position = "bottom")




# 2. Alisamento exponencial Duplo "AAN"
AAN <- empregoSE %>% 
     group_by(setor, mes) %>% 
     summarise(valores = sum(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            ajusteETS = map(.x = serie,
                            .f = ets,
                            model = "AAN"),
            tidy = map(ajusteETS, sw_tidy),
            glance = map(ajusteETS, sw_glance),
            augment = map(ajusteETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ajusteETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            previsaoETS = map(ajusteETS, forecast, h = 16, levels = c(85,90)),
            sweep = map(previsaoETS, sw_sweep, fitted = TRUE, timetk_idx = TRUE))

AAN %>% unnest(tidy)
AAN %>% unnest(glance)
AAN %>% unnest(augment)
AAN %>% unnest(decomp)

AAN %>% unnest(sweep) %>% 
     ggplot(aes(x = index, y = valores, colour = key, group = setor)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line(size = 1.3) +
     labs(title = "Previsão para os setores da economia",
          subtitle = "(Previsão de 16 meses pelo modelo de Alisamento Exponencial Duplo - AAN)", x = "", y = "") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           axis.text.x = element_text(angle = 0, hjust = 0.5),
           legend.position = "bottom") +
     facet_wrap(~ setor, scales = "free_y", ncol = 3)



# 4. alisamento exponencial Holt-Winters Aditivo "AAA"
AAA <- empregoSE %>% 
     group_by(setor, mes) %>% 
     summarise(valores = sum(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            ajusteETS = map(.x = serie,
                            .f = ets,
                            model = "AAA"),
            tidy = map(ajusteETS, sw_tidy),
            glance = map(ajusteETS, sw_glance),
            augment = map(ajusteETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ajusteETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            previsaoETS = map(ajusteETS, forecast, h = 16, levels = c(85,90)),
            sweep = map(previsaoETS, sw_sweep, fitted = TRUE, timetk_idx = TRUE))

AAA %>% unnest(tidy)
AAA %>% unnest(glance)
AAA %>% unnest(augment)
AAA %>% unnest(decomp)

AAA %>% unnest(sweep) %>% 
     ggplot(aes(x = index, y = valores, colour = key, group = setor)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line(size = 1.3) +
     labs(title = "Previsão para os setores da economia",
          subtitle = "(Previsão de 16 meses pelo modelo de Alisamento Exponencial Holt-Winters Aditivo - AAA)", x = "", y = "") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           axis.text.x = element_text(angle = 0, hjust = 0.5),
           legend.position = "bottom") +
     facet_wrap(~ setor, scales = "free_y", ncol = 3)



# 5. alisamento exponencial - Holt-Winters Multiplicativo "MAM"
MAM <- empregoSE %>% 
     group_by(setor, mes) %>% 
     summarise(valores = sum(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            ajusteETS = map(.x = serie,
                            .f = ets,
                            model = "MAM"),
            tidy = map(ajusteETS, sw_tidy),
            glance = map(ajusteETS, sw_glance),
            augment = map(ajusteETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ajusteETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            previsaoETS = map(ajusteETS, forecast, h = 16, levels = c(85,90)),
            sweep = map(previsaoETS, sw_sweep, fitted = TRUE, timetk_idx = TRUE))

MAM %>% unnest(tidy)
MAM %>% unnest(glance)
MAM %>% unnest(augment)
MAM %>% unnest(decomp)

MAM %>% unnest(sweep) %>% 
     ggplot(aes(x = index, y = valores, colour = key, group = setor)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line(size = 1.3) +
     labs(title = "Previsão para os setores da economia",
          subtitle = "(Previsão de 16 meses pelo modelo de Alisamento Exponencial Holt-Winters Multiplicativo - MAM)", x = "", y = "") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           axis.text.x = element_text(angle = 0, hjust = 0.5),
           legend.position = "bottom") +
     facet_wrap(~ setor, scales = "free_y", ncol = 3)



# 6. Alisamento exponencial Holt-Winters Aditivo "ANA"
ANA <- empregoSE %>% 
     group_by(setor, mes) %>% 
     summarise(valores = sum(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            ajusteETS = map(.x = serie,
                            .f = ets,
                            model = "MAM"),
            tidy = map(ajusteETS, sw_tidy),
            glance = map(ajusteETS, sw_glance),
            augment = map(ajusteETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ajusteETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            previsaoETS = map(ajusteETS, forecast, h = 16, levels = c(85,90)),
            sweep = map(previsaoETS, sw_sweep, fitted = TRUE, timetk_idx = TRUE))

ANA %>% unnest(tidy)
ANA %>% unnest(glance)
ANA %>% unnest(augment)
ANA %>% unnest(decomp)

ANA %>% unnest(sweep) %>% 
     ggplot(aes(x = index, y = valores, colour = key, group = setor)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line(size = 1.3) +
     labs(title = "Previsão para os setores da economia",
          subtitle = "(Previsão de 16 meses pelo modelo de Alisamento Exponencial Holt-Winters Aditivo - ANA)", x = "", y = "") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           axis.text.x = element_text(angle = 0, hjust = 0.5),
           legend.position = "bottom") +
     facet_wrap(~ setor, scales = "free_y", ncol = 3)




# 7. alisamento exponencial Holt-Winters Multiplicativo "MNM"
MNM <- empregoSE %>% 
     group_by(setor, mes) %>% 
     summarise(valores = sum(valores)) %>% 
     nest() %>% 
     mutate(serie = map(.x = data,
                        .f = tk_ts,
                        select = -mes,
                        start = 2003,
                        freq = 12),
            ajusteETS = map(.x = serie,
                            .f = ets,
                            model = "MAM"),
            tidy = map(ajusteETS, sw_tidy),
            glance = map(ajusteETS, sw_glance),
            augment = map(ajusteETS, sw_augment, timetk_idx = TRUE, rename_index = "date"),
            decomp = map(ajusteETS, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date"),
            previsaoETS = map(ajusteETS, forecast, h = 16, levels = c(85,90)),
            sweep = map(previsaoETS, sw_sweep, fitted = TRUE, timetk_idx = TRUE))

MNM %>% unnest(tidy)
MNM %>% unnest(glance)
MNM %>% unnest(augment)
MNM %>% unnest(decomp)

MNM %>% unnest(sweep) %>% 
     ggplot(aes(x = index, y = valores, colour = key, group = setor)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line(size = 1.3) +
     labs(title = "Previsão para os setores da economia",
          subtitle = "(Previsão de 16 meses pelo modelo de Alisamento Exponencial Holt-Winters Multiplicativo - MNM)", x = "", y = "") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           plot.subtitle = element_text(size = 13, hjust = 0.5),
           axis.text.x = element_text(angle = 0, hjust = 0.5),
           legend.position = "bottom") +
     facet_wrap(~ setor, scales = "free_y", ncol = 3)



#comparando modelos
treino = window(serie2, 2003, c(2005, 12))
teste  = window(serie2, 2006, c(2007, 12))


# 8. alisamento exponencial  - holt - Winters - Aditivo
ets(treino, "AAA")

autoplot(ets(treino, "AAA")$residuals)
autoplot(ets(treino, "AAA")$fitted)

forecast(ets(treino, "AAA"), h=24)
print(forecast(ets(treino, "AAA"), h=24)$mean)
autoplot(forecast(ets(treino, "AAA"), h=24))



#alisamento exponencial  - holt - Winters - Multiplicativo
ets(treino, "MAM")

autoplot(ets(treino, "MAM")$residuals)
autoplot(ets(treino, "MAM")$fitted)

forecast(ets(treino, "MAM"), h=24)
print(forecast(ets(treino, "MAM"), h=24)$mean)
autoplot(forecast(ets(treino, "MAM"), h=24))


#################################################################################

plot(serie2)
lines(forecast(ets(treino, "AAA"), h=24)$mean, col="blue")
lines(forecast(ets(treino, "MAM"), h=24)$mean, col="red")
lines(teste, col="green")
legend("topleft",legend=c("AAA","MAM","Teste"), col = c("blue","red","green"), lty=1:2, cex=0.8)

accuracy(forecast(ets(treino, "AAA"), h=24),teste )
accuracy(forecast(ets(treino, "MAM"), h=24),teste )
