library(RODBC)
library(tidyverse)
library(forecast)
library(prophet)
library(tsibble)
library(fable)
library(modeltime.ensemble)
library(tidymodels)
library(timetk)

dremio_host <- Sys.getenv("endereco")
dremio_port <- Sys.getenv("port")
dremio_uid <- Sys.getenv("uid")
dremio_pwd <- Sys.getenv("datalake")


channel <- odbcDriverConnect(sprintf("DRIVER=Dremio Connector;
                                     HOST=%s;
                                     PORT=%s;
                                     UID=%s;
                                     PWD=%s;
                                     AUTHENTICATIONTYPE=Basic Authentication;
                                     CONNECTIONTYPE=Direct", 
                                     dremio_host, 
                                     dremio_port, 
                                     dremio_uid, 
                                     dremio_pwd))


query <- 'SELECT * FROM "@daniel"."nascidos_uf_def_preliminar_ajustado"'


nascidos <- sqlQuery(channel, 
                        query, 
                        as.is = TRUE) |> 
               janitor::clean_names()

nascidos$qtd <- as.numeric(nascidos$qtd)

nascidos <- nascidos |> 
                  mutate(mes = substr(competen, 1, 2),
                         ano = substr(competen, 3, 6),
                         competen = ymd(paste0(ano, "-", 
                                               mes, "-01"))) |> 
            filter(competen < "2024-07-01")

nascidos |> 
  filter(uf_sigla == "AM") |> 
  ggplot(aes(x = competen, y = qtd)) + 
  geom_line()

# previsoes_nv para um estado apenas  ------------------------------------------------------------

estado <- "RR"

nascidos_uf <- nascidos |> 
                  filter(uf_sigla == estado)

nascidos_uf |>
  plot_time_series(competen, qtd,
                   .smooth = FALSE)

training_data <- nascidos_uf |> filter(competen < "2022-01-01")
testing_data <- nascidos_uf |> filter(competen >= "2022-01-01")

# Modelos -----------------------------------------------------------------

# modelo 1 - arima

modelo_1 <- 
  arima_reg() |>
  set_engine(engine = "auto_arima") |>
  fit(qtd ~ competen, 
      data = training_data)

# Model 2 - arima boost

modelo_2 <- arima_boost(
  min_n = 2,
  learn_rate = 0.015) |>
  set_engine(engine = "auto_arima_xgboost") |>
  fit(qtd ~ competen + 
            as.numeric(competen) + 
            factor(month(competen, 
                         label = TRUE), 
                   ordered = F),
      data = training_data)

# Modelo 3 - ETS

modelo_3 <- 
  exp_smoothing() |>
  set_engine(engine = "ets") |>
  fit(qtd ~ competen, data = training_data)

# Modelo 4 - Prophet

modelo_4 <- 
  prophet_reg() |>
  set_engine(engine = "prophet") |>
  fit(qtd ~ competen, data = training_data)

# Modelo 5 - Regressão

modelo_5 <- 
  linear_reg() |>
  set_engine("lm") |>
  fit(qtd ~ as.numeric(competen) + 
            factor(month(competen, 
                         label = TRUE), 
             ordered = FALSE),
      data = training_data)

# Modelo 6 - MARS

model_spec_mars <- 
  mars(mode = "regression") |>
  set_engine("earth") 

recipe_spec <- recipe(qtd ~ competen, data = training_data) |>
  step_date(competen, features = "month", ordinal = FALSE) |>
  step_mutate(date_num = as.numeric(competen)) |>
  step_normalize(date_num) |>
  step_rm(competen)

modelo_6 <- workflow() |>
  add_recipe(recipe_spec) |>
  add_model(model_spec_mars) |>
  fit(training_data)

# modelo 7 - Prophet boost

modelo_7 <- 
    exp_smoothing() |> 
    set_engine(engine = "theta") |> 
    fit(qtd ~ competen, data = training_data)

# modelo 8 - Arima STLM

modelo_8 <- seasonal_reg() |> 
              set_engine("stlm_arima") |> 
              fit(qtd ~ competen, data = training_data)

# modelo 9 - NNetar_reg 

modelo_9 <- nnetar_reg() |> 
              set_engine("nnetar") |> 
              fit(qtd ~ competen, data = training_data)


models_tbl <- 
  modeltime_table(
        modelo_1, modelo_2, modelo_3,
        modelo_4, modelo_5, modelo_6,
        modelo_7, modelo_8, modelo_9)

models_tbl

calibration_tbl <- 
  models_tbl |>
  modeltime_calibrate(new_data = testing_data)

calibration_tbl |>
  modeltime_accuracy() |> 
  table_modeltime_accuracy()

melhores_modelos <- 
  calibration_tbl |>
  modeltime_accuracy() |> 
  filter(.model_id %in% c(2, 3, 5))

# AM, GO foi ETS, Prophet e LM
# MS foi ETS, ARIMA, prophet
# RR foi ets, arima com xgboost, lm 

write.csv(melhores_modelos, 
          file = paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/melhores_modelos/melhores_modelos_",estado,".csv"))

id <- melhores_modelos$.model_id

comp_best_models <- 
  calibration_tbl |>
  modeltime_forecast(
    new_data    = testing_data,
    actual_data = nascidos_uf
  ) |>  
  filter(.index >= "2022-01-01") |> 
  filter(.model_id %in% id | is.na(.model_id)) 


a <- 
  comp_best_models |> 
  ggplot(aes(x = .index, 
             y = .value, 
             col = .model_desc)) + 
  geom_line(size = 1) + 
  theme_minimal() + 
  xlab("Ano") + ylab("Total") +
  ylim(0, max(comp_best_models$.value) * 1.10) + 
  ggtitle(paste0("Comparação entre melhores projeções vs valores reais - UF: ",
                  estado))

a

ggsave(plot = a,
       filename = paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecao_vs_teste/proj_teste_",
       estado,".jpeg"), 
       dpi = 500, height = 5, width = 8)

refit_tbl <- 
  calibration_tbl |>
  modeltime_refit(data = nascidos_uf)

b <- refit_tbl |>
  filter(.model_id %in% id) |> 
  modeltime_forecast(h = "8 years", 
                     actual_data = nascidos_uf) |> 
  filter(year(.index) < 2031) |> 
  ggplot(aes(x = .index, 
             y = .value, 
             col = .model_desc)) +
  geom_line() + 
  geom_smooth(method = "loess", 
              se = FALSE,
              size = 0.5) +
  theme_minimal() + 
  ylim(0, max(comp_best_models$.value) * 1.50) + 
  xlab("Ano") + ylab("Total") +
  ggtitle(paste0("Projeção dos três melhores modelos - UF: ",
                 estado))

b

ggsave(plot = b, 
       filename = paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecoes/",estado,
       "_top_3_proj.jpeg"), 
       dpi = 500, height = 5, width = 8)


# Criando o ensemble dos melhores

ensemble_fit <- 
  models_tbl |>
  filter(.model_id %in% id) |> 
  ensemble_weighted(loadings = c(1, 1, 3),
                    scale_loadings = TRUE)

ensemble_fit

calibration_tbl_ensemble <- 
  modeltime_table(
  ensemble_fit) |>
  modeltime_calibrate(testing_data, 
                      quiet = FALSE)

# Forecast vs Test Set
calibration_tbl_ensemble |>
  modeltime_forecast(
    new_data    = testing_data,
    actual_data = training_data
  ) |> 
  ggplot(aes(x = .index, 
             y = .value, 
             col = .model_desc)) +
  geom_line() + 
  theme_minimal() + 
  ylim(0, max(comp_best_models$.value) * 1.50) + 
  xlab("Ano") + ylab("Total") +
  ggtitle(paste0("Projeção dos melhores modelos - UF: ",
                 estado))

refit_tbl_ensemble <- 
  calibration_tbl_ensemble |>
  modeltime_refit(data = nascidos_uf)

c <- refit_tbl_ensemble |>
  modeltime_forecast(h = "8 years", 
                     actual_data = nascidos_uf) |> 
  filter(year(.index) < 2031) |> 
  ggplot(aes(x = .index, 
             y = .value, 
             col = .model_desc)) +
  geom_line() + 
  geom_smooth(method = "loess",
              col = "blue", 
              se = FALSE,
              size = 0.5) +
  theme_minimal() + 
  ylim(0, max(comp_best_models$.value) * 1.50) + 
  xlab("Ano") + ylab("Total") +
  ggtitle(paste0("Projeção dos melhores modelos - UF: ",
                 estado)) 
c
ggsave(plot = c, 
       filename = paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecoes/",estado,"_proj_emsemble.jpeg"), 
       dpi = 500, height = 5, width = 8)


projecao_ensemble <- refit_tbl_ensemble |>
  modeltime_forecast(h = "9 years", 
                     actual_data = nascidos_uf)

write.csv(projecao_ensemble,
          paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecoes/",estado,"_projecao.csv"))


proj_ano <- projecao_ensemble |>
  mutate(ano = year(.index)) |> 
  group_by(ano) |> 
  summarise(qtd = sum(.value)) 

d <- proj_ano |> 
  mutate(.key = if_else(ano >= 2024, "Previsão", "Real")) |>
  filter(ano <= 2030) |> 
  ggplot(aes(x = ano, y = qtd, col = .key)) + 
  geom_line(aes(group = 1)) + 
  geom_point() +
  theme_minimal() +
  ylim(0, max(proj_ano$qtd) * 1.10) + 
  xlab("Ano") + ylab("Quantidade") + 
  ggtitle(paste0("Projeção em valores anuais - UF: ",
                 estado))
d
ggsave(plot = d, 
       filename = paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecoes/",estado,
                         "_proj_emsemble_anual.jpeg"), 
       dpi = 500, height = 5, width = 8)


# Criando uma funcao ------------------------------------------------------

fazer_projecao <- function(estado){
  
  nascidos_uf <- nascidos |> 
    filter(uf_sigla == estado)
  
  nascidos_uf |>
    plot_time_series(competen, qtd,
                     .smooth = FALSE)
  
  training_data <- nascidos_uf |> filter(competen < "2022-01-01")
  testing_data <- nascidos_uf |> filter(competen >= "2022-01-01")
  
  # modelo 1 - arima
  
  modelo_1 <- 
    arima_reg() |>
    set_engine(engine = "auto_arima") |>
    fit(qtd ~ competen, 
        data = training_data)
  
  # Model 2 - arima boost
  
  modelo_2 <- arima_boost(
    min_n = 2,
    learn_rate = 0.015) |>
    set_engine(engine = "auto_arima_xgboost") |>
    fit(qtd ~ competen + 
          as.numeric(competen) + 
          factor(month(competen, 
                       label = TRUE), 
                 ordered = F),
        data = training_data)
  
  # Modelo 3 - ETS
  
  modelo_3 <- 
    exp_smoothing() |>
    set_engine(engine = "ets") |>
    fit(qtd ~ competen, data = training_data)
  
  # Modelo 4 - Prophet
  
  modelo_4 <- 
    prophet_reg() |>
    set_engine(engine = "prophet") |>
    fit(qtd ~ competen, data = training_data)
  
  # Modelo 5 - Regressão
  
  modelo_5 <- 
    linear_reg() |>
    set_engine("lm") |>
    fit(qtd ~ as.numeric(competen) + 
          factor(month(competen, 
                       label = TRUE), 
                 ordered = FALSE),
        data = training_data)
  
  # Modelo 6 - MARS
  
  model_spec_mars <- 
    mars(mode = "regression") |>
    set_engine("earth") 
  
  recipe_spec <- recipe(qtd ~ competen, data = training_data) |>
    step_date(competen, features = "month", ordinal = FALSE) |>
    step_mutate(date_num = as.numeric(competen)) |>
    step_normalize(date_num) |>
    step_rm(competen)
  
  modelo_6 <- workflow() |>
    add_recipe(recipe_spec) |>
    add_model(model_spec_mars) |>
    fit(training_data)
  
  # modelo 7 - Prophet boost
  
  modelo_7 <- 
    exp_smoothing() |> 
    set_engine(engine = "theta") |> 
    fit(qtd ~ competen, data = training_data)
  
  # modelo 8 - Arima STLM
  
  modelo_8 <- seasonal_reg() |> 
    set_engine("stlm_arima") |> 
    fit(qtd ~ competen, data = training_data)
  
  # modelo 9 - NNetar_reg 
  
  modelo_9 <- nnetar_reg() |> 
    set_engine("nnetar") |> 
    fit(qtd ~ competen, data = training_data)
  
  
  models_tbl <- 
    modeltime_table(
      modelo_1, modelo_2, modelo_3,
      modelo_4, modelo_5, modelo_6,
      modelo_7, modelo_8, modelo_9)
  
  models_tbl
  
  calibration_tbl <- 
    models_tbl |>
    modeltime_calibrate(new_data = testing_data)
  
  calibration_tbl |>
    modeltime_accuracy() |> 
    table_modeltime_accuracy()
  
  melhores_modelos <- 
    calibration_tbl |>
    modeltime_accuracy() |> 
    slice_min(mape, n = 3)
  
  write.csv(melhores_modelos, 
            file = paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/melhores_modelos/melhores_modelos_",estado,".csv"))
  
  id <- melhores_modelos$.model_id
  
  comp_best_models <- 
    calibration_tbl |>
    modeltime_forecast(
      new_data    = testing_data,
      actual_data = nascidos_uf
    ) |>  
    filter(.index >= "2022-01-01") |> 
    filter(.model_id %in% id | is.na(.model_id)) 
  
  
  a <- 
    comp_best_models |> 
    ggplot(aes(x = .index, 
               y = .value, 
               col = .model_desc)) + 
    geom_line(size = 1) + 
    theme_minimal() + 
    xlab("Ano") + ylab("Total") +
    ylim(0, max(comp_best_models$.value) * 1.10) + 
    ggtitle(paste0("Comparação entre melhores projeções vs valores reais - UF: ",
                   estado))
  
  ggsave(plot = a,
         filename = paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecao_vs_teste/proj_teste_",
                           estado,".jpeg"), 
         dpi = 500, height = 5, width = 8)
  
  refit_tbl <- 
    calibration_tbl |>
    modeltime_refit(data = nascidos_uf)
  
  b <- refit_tbl |>
    filter(.model_id %in% id) |> 
    modeltime_forecast(h = "8 years", 
                       actual_data = nascidos_uf) |> 
    filter(year(.index) < 2031) |> 
    ggplot(aes(x = .index, 
               y = .value, 
               col = .model_desc)) +
    geom_line() + 
    geom_smooth(method = "loess", 
                se = FALSE,
                size = 0.5) +
    theme_minimal() + 
    ylim(0, max(comp_best_models$.value) * 1.50) + 
    xlab("Ano") + ylab("Total") +
    ggtitle(paste0("Projeção dos três melhores modelos - UF: ",
                   estado))
  
  ggsave(plot = b, 
         filename = paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecoes/",estado,
                           "_top_3_proj.jpeg"), 
         dpi = 500, height = 5, width = 8)
  
  
  # Criando o ensemble dos melhores
  
  ensemble_fit <- 
    models_tbl |>
    filter(.model_id %in% id) |> 
    ensemble_weighted(loadings = c(1, 3, 1),
                      scale_loadings = TRUE)
  
  ensemble_fit
  
  calibration_tbl_ensemble <- 
    modeltime_table(
      ensemble_fit) |>
    modeltime_calibrate(testing_data, 
                        quiet = FALSE)
  
  # Forecast vs Test Set
  calibration_tbl_ensemble |>
    modeltime_forecast(
      new_data    = testing_data,
      actual_data = training_data
    ) |> 
    ggplot(aes(x = .index, 
               y = .value, 
               col = .model_desc)) +
    geom_line() + 
    theme_minimal() + 
    ylim(0, max(comp_best_models$.value) * 1.50) + 
    xlab("Ano") + ylab("Total") +
    ggtitle(paste0("Projeção dos melhores modelos - UF: ",
                   estado))
  
  refit_tbl_ensemble <- 
    calibration_tbl_ensemble |>
    modeltime_refit(data = nascidos_uf)
  
  c <- refit_tbl_ensemble |>
    modeltime_forecast(h = "8 years", 
                       actual_data = nascidos_uf) |> 
    filter(year(.index) < 2031) |> 
    ggplot(aes(x = .index, 
               y = .value, 
               col = .model_desc)) +
    geom_line() + 
    geom_smooth(method = "loess",
                col = "blue", 
                se = FALSE,
                size = 0.5) +
    theme_minimal() + 
    ylim(0, max(comp_best_models$.value) * 1.50) + 
    xlab("Ano") + ylab("Total") +
    ggtitle(paste0("Projeção dos melhores modelos - UF: ",
                   estado)) 
  
  ggsave(plot = c, 
         filename = paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecoes/",estado,"_proj_emsemble.jpeg"), 
         dpi = 500, height = 5, width = 8)
  
  
  projecao_ensemble <- refit_tbl_ensemble |>
    modeltime_forecast(h = "9 years", 
                       actual_data = nascidos_uf)
  
  write.csv(projecao_ensemble,
            paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecoes/",estado,"_projecao.csv"))
  
  
  proj_ano <- projecao_ensemble |>
    mutate(ano = year(.index)) |> 
    group_by(ano) |> 
    summarise(qtd = sum(.value)) 
  
  d <- proj_ano |> 
    mutate(.key = if_else(ano >= 2024, "Previsão", "Real")) |>
    filter(ano <= 2030) |> 
    ggplot(aes(x = ano, y = qtd, col = .key)) + 
    geom_line(aes(group = 1)) + 
    geom_point() +
    theme_minimal() +
    ylim(0, max(proj_ano$qtd) * 1.10) + 
    xlab("Ano") + ylab("Quantidade") + 
    ggtitle(paste0("Projeção em valores anuais - UF: ",
                   estado))
  
  ggsave(plot = d, 
         filename = paste0("~/GitHub/materno_infantil/04_analises_UF_cap3/01_output_projecoes/projecoes/",estado,
                           "_proj_emsemble_anual.jpeg"), 
         dpi = 500, height = 5, width = 8)
  
}

uf <- unique(nascidos$uf_sigla)

for (estado in uf) {
  fazer_projecao(estado = estado)
}

