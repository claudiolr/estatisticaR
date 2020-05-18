library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)
library(lubridate)


# https://cran.rstudio.com/web/packages/sweep/vignettes/SW01_Forecasting_Time_Series_Groups.html


# Esse script roda um exemplo de análise de séries temporais a partir de diversos pacotes que funcionam em conjunto com o tidyverse
# Ele permite a geração de parâmetros e dados ajustados que facilitam a geração de gráficos
# O código é uma adaptação do que está no endereço acima, já que evita a criação excessiva de objetos


# Base de exemplo presente no pacote sweep
bike <- sweep::bike_sales


# Total de unidades de bicicletas vendidas por mês
bike %>% 
     mutate(month = month(order.date, label = TRUE),
            year = year(order.date)) %>% 
     group_by(year, month) %>% 
     summarise(total = sum(quantity))


# Gráfico de unidades de bicicletas vendidas por mês
bike %>% 
     mutate(month = month(order.date, label = TRUE),
            year = year(order.date)) %>% 
     group_by(year, month) %>% 
     summarise(total = sum(quantity)) %>% 
     ggplot(aes(x = month, y = total, group = year)) +
     geom_area(aes(fill = year), position = "stack") +
     scale_y_continuous() +
     theme_tq()



##### Abaixo estão os passos para geração dos dados para previsão
### 1. Agrupa os dados por mÊs
bike %>% 
     mutate(order.month = as_date(as.yearmon(order.date))) %>% 
     group_by(category.secondary, order.month) %>% 
     summarise(total = sum(quantity))



### 2. Agrupa os dados por mÊs e categoria de bicicleta por meio da função nest()
# (cada categoria de bicicleta é um data frame na lista de data frames)
bike %>% 
     mutate(order.month = as_date(as.yearmon(order.date))) %>% 
     group_by(category.secondary, order.month) %>% 
     summarise(total = sum(quantity)) %>% 
     nest()



### 3. Força os vetores para o tipo time series(ts), criando a coluna data.ts
# observe que a função nest cria uma coluna chamata data, que é um vetor de listas (list-column)
# Essa list-column é passada no parâmetro .x
# para o parâmetro que pede a função é passada a função tk_ts, que força aquele elemento para uma série temporal (ts)
# a função select descarta a coluna order.month para evitar um monte de mensagens de aviso
bike %>% 
     mutate(order.month = as_date(as.yearmon(order.date))) %>% 
     group_by(category.secondary, order.month) %>% 
     summarise(total = sum(quantity)) %>% 
     nest() %>% 
     mutate(data.ts = map(.x = data,
                          .f = tk_ts,
                          select = -order.month,
                          start = 2011,
                          freq = 12))



### 4. Modela cada série temporal pelo modelo ETS (error, trend, seasonal), do pacote forecast
# a coluna a ser modelada é data.ts, e a nova coluna será fit.ets
# o resultado segue sendo um vetor de listas (list-column)
ajustado <- 
bike %>% 
     mutate(order.month = as_date(as.yearmon(order.date))) %>% 
     group_by(category.secondary, order.month) %>% 
     summarise(total = sum(quantity)) %>% 
     nest() %>% 
     mutate(data.ts = map(.x = data,
                          .f = tk_ts,
                          select = -order.month,
                          start = 2011,
                          freq = 12),
            fit.ets = map(data.ts, ets))



### 5. Verificação do modelo a partir de funções "arrumadoras" (tidiers) do pacote sweep
# 5.1. a função sw_tidy() retorna os parâmetros dos modelos para vetor da lista de vetores
bike %>% 
     mutate(order.month = as_date(as.yearmon(order.date))) %>% 
     group_by(category.secondary, order.month) %>% 
     summarise(total = sum(quantity)) %>% 
     nest() %>% 
     mutate(data.ts = map(.x = data,
                          .f = tk_ts,
                          select = -order.month,
                          start = 2011,
                          freq = 12),
            fit.ets = map(data.ts, ets),
            tidy = map(fit.ets, sw_tidy)) %>% 
     unnest(tidy) %>% 
     pivot_wider(names_from = category.secondary, values_from = estimate)
     


# 5.2. a função sw_glance() retorna a acurácia dos modelos
bike %>% 
     mutate(order.month = as_date(as.yearmon(order.date))) %>% 
     group_by(category.secondary, order.month) %>% 
     summarise(total = sum(quantity)) %>% 
     nest() %>% 
     mutate(data.ts = map(.x = data,
                          .f = tk_ts,
                          select = -order.month,
                          start = 2011,
                          freq = 12),
            fit.ets = map(data.ts, ets),
            glance = map(fit.ets, sw_glance)) %>% 
     unnest(glance)



# 5.3. a função sw_augment retorna os valores residuais e ajustados aumentados
# em seguida está a plotagem dos resíduos
bike %>% 
     mutate(order.month = as_date(as.yearmon(order.date))) %>% 
     group_by(category.secondary, order.month) %>% 
     summarise(total = sum(quantity)) %>% 
     nest() %>% 
     mutate(data.ts = map(.x = data,
                          .f = tk_ts,
                          select = -order.month,
                          start = 2011,
                          freq = 12),
            fit.ets = map(data.ts, ets),
            augment = map(fit.ets, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>% 
     unnest(augment)



bike %>% 
     mutate(order.month = as_date(as.yearmon(order.date))) %>% 
     group_by(category.secondary, order.month) %>% 
     summarise(total = sum(quantity)) %>% 
     nest() %>% 
     mutate(data.ts = map(.x = data,
                          .f = tk_ts,
                          select = -order.month,
                          start = 2011,
                          freq = 12),
            fit.ets = map(data.ts, ets),
            augment = map(fit.ets, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>% 
     unnest(augment) %>% 
     ggplot(aes(x = date, y = .resid, group = category.secondary)) +
     geom_hline(yintercept = 0, color = "grey40") +
     geom_line(color = palette_light()[[2]]) +
     geom_smooth(method = "loess") +
     facet_wrap( ~ category.secondary, scale = "free_y", ncol = 3) +
     scale_x_date(date_labels = "%Y")



# 5.6. a função sq_tidy_decomp decompõe os resultados dos modelos
bike %>% 
     mutate(order.month = as_date(as.yearmon(order.date))) %>% 
     group_by(category.secondary, order.month) %>% 
     summarise(total = sum(quantity)) %>% 
     nest() %>% 
     mutate(data.ts = map(.x = data,
                          .f = tk_ts,
                          select = -order.month,
                          start = 2011,
                          freq = 12),
            fit.ets = map(data.ts, ets),
            decomp = map(fit.ets, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date")) %>% 
     unnest(decomp)



### 6. Previsão dos modelos com o pacote forecast::
# h = 12 é período de previsão
bike %>% 
     mutate(order.month = as_date(as.yearmon(order.date))) %>% 
     group_by(category.secondary, order.month) %>% 
     summarise(total = sum(quantity)) %>% 
     nest() %>% 
     mutate(data.ts = map(.x = data,
                          .f = tk_ts,
                          select = -order.month,
                          start = 2011,
                          freq = 12),
            fit.ets = map(data.ts, ets),
            fcast.ets = map(fit.ets, forecast, h = 12))


### 7. Obter um data frame de previsões arrumado (tidy) a partir da função sw_sweep
# o argumento fitted define se os valores ajustados aparecem (TRUE) ou não (FALSE) no resultado
bike %>% 
     mutate(order.month = as_date(as.yearmon(order.date))) %>% 
     group_by(category.secondary, order.month) %>% 
     summarise(total = sum(quantity)) %>% 
     nest() %>% 
     mutate(data.ts = map(.x = data,
                          .f = tk_ts,
                          select = -order.month,
                          start = 2011,
                          freq = 12),
            fit.ets = map(data.ts, ets),
            fcast.ets = map(fit.ets, forecast, h = 12),
            sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>% 
     unnest(sweep)



bike %>% 
     mutate(order.month = as_date(as.yearmon(order.date))) %>% 
     group_by(category.secondary, order.month) %>% 
     summarise(total = sum(quantity)) %>% 
     nest() %>% 
     mutate(data.ts = map(.x = data,
                          .f = tk_ts,
                          select = -order.month,
                          start = 2011,
                          freq = 12),
            fit.ets = map(data.ts, ets),
            fcast.ets = map(fit.ets, forecast, h = 12),
            sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>% 
     unnest(sweep) %>% 
     ggplot(aes(x = index, y = total, colour = key, group = category.secondary)) +
     geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                 fill = "#D5DBFF", colour = NA, size = 0) +
     geom_ribbon(aes(ymin = lo.80, ymax = hi.80),
                 fill = "#596DD5", colour = NA, size = 0, alpha = 0.8) +
     geom_line() +
     labs(title = "Quantidade de bicicletas vendidas por categoria secundária",
          subtitle = "Previsão pelo modelo ETS", x = "", y = "Unidades") +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     scale_colour_tq() +
     scale_fill_tq() + 
     facet_wrap(~ category.secondary, scales = "free_y", ncol = 3) +
     theme_tq() +
     theme(plot.title = element_text(size = 16, hjust = 0.5),
           axis.text.x = element_text(angle = 45, hjust = 1))
     
     
