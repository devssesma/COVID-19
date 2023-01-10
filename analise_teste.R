




# Carregando dados --------------------------------------------------------

monitoramento <- read.csv2('casos_positivos-20230110132327401479.csv') |> 
  janitor::clean_names()



# Pre processamento de dados ----------------------------------------------

monitoramento <- monitoramento |> 
  dplyr::mutate(datanot = lubridate::dmy(data_notificacao),
                data_sintomas = lubridate::dmy(data_inicio_sintomas),
                datanot = dplyr::coalesce(datanot, data_sintomas),
                data_hospital = lubridate::dmy(data_hospitalizacao),
                dataobito = lubridate::dmy(data_obito),
                data_pub_obito = lubridate::dmy(data_publicacao_obito),
                semana_epi = lubridate::epiweek(datanot),
                ano = lubridate::year(datanot),
                mes = lubridate::month(datanot, label = TRUE))



# Analise 1 ---------------------------------------------------------------

monitoramento |> 
  dplyr::group_by(datanot) |> 
  dplyr::summarise(count = dplyr::n()) |> 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = datanot, y = count)) +
  ggplot2::theme_bw()

# -------

notific <- monitoramento |> 
  dplyr::group_by(datanot) |> 
  dplyr::summarise(count = dplyr::n()) |> 
  tidyr::drop_na(datanot)

sint <- monitoramento |> 
  dplyr::group_by(data_sintomas) |> 
  dplyr::summarise(count = dplyr::n()) |> 
  tidyr::drop_na(data_sintomas)


notific_xts <- xts::xts(x = notific$count, order.by = notific$datanot)
sint_xts <- xts::xts(x = sint$count, order.by = sint$data_sintomas)
 
not_sint <- cbind(notific_xts, sint_xts)

dygraphs::dygraph(not_sint) |> 
  dygraphs::dySeries("notific_xts", label = "Notificados") |> 
  dygraphs::dySeries("sint_xts", label = "Data dos sintomas") |> 
  dygraphs::dyOptions(stackedGraph = TRUE) 





































































