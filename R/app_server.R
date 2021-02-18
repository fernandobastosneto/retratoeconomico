#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
library(magrittr)
library(tmap)

number_si <- htmlwidgets::JS("
    function(n) {
      if (Math.abs(n) < 1000) return n;
      if (Math.abs(n) >= 1000 && Math.abs(n) < 999999) return +(n / 1e3).toFixed(1) + 'K';
      if (Math.abs(n) >= 1000000 && Math.abs(n) < 999999999) return +(n / 1e6).toFixed(1) + 'M';
      if (Math.abs(n) >= 1000000000 && Math.abs(n) < 999999999999) return +(n / 1e9).toFixed(1) + 'B';
      if (Math.abs(n) >= 1e12) return +(n / 1e12).toFixed(1) + 'T';
    }")


app_server <- function( input, output, session ) {

  # Funções Comércio Bilateral ----

  output$corrente <- echarts4r::renderEcharts4r({
    pais <- input$pais

    barao::comerciobr_dados_corrente(input$pais, "anual") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(co_ano = as.factor(co_ano)) %>%
      tidyr::pivot_wider(names_from = trade_flow, values_from = value) %>%
      # tidyr::pivot_longer(Exportações:Importações, names_to = "trade_flow", values_to = "value") %>%
      # dplyr::group_by(Exportações) %>%
      echarts4r::e_charts(co_ano) %>%
      echarts4r::e_area(serie = Saldo, smooth = TRUE) %>%
      echarts4r::e_line(serie = Exportações, smooth = T) %>%
      echarts4r::e_line(serie = Importações, smooth = T) %>%
      echarts4r::e_y_axis(formatter = number_si) %>%
      echarts4r::e_tooltip(trigger = "axis") %>%
      echarts4r::e_legend(right = 0)
  })

  output$fator_exp <- echarts4r::renderEcharts4r({

    pais <- input$pais

    comerciobr::fator_df %>%
      dplyr::left_join(comerciobr::dic_paises) %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::filter(path == "EXP") %>%
      dplyr::left_join(comerciobr::dic_ncm_fator) %>%
      dplyr::select(-c(co_pais, no_pais, co_ncm, co_fat_agreg)) %>%
      dplyr::distinct_all() %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = no_fat_agreg, values_from = value) %>%
      janitor::clean_names() %>%
      dplyr::rename(Básicos = produtos_basicos,
                    `Semi-manufaturados` = produtos_semimanufaturados,
                    Manufaturados = produtos_manufaturados) %>%
      # dplyr::group_by(path) %>%
      echarts4r::e_charts(co_ano) %>%
      echarts4r::e_river(Básicos) %>%
      echarts4r::e_river(`Semi-manufaturados`) %>%
      echarts4r::e_river(Manufaturados) %>%
      echarts4r::e_tooltip(trigger = "axis") %>%
      echarts4r::e_legend(right = 0, orient = 'vertical')

  })

  output$fator_imp <- echarts4r::renderEcharts4r({

    pais <- input$pais

    comerciobr::fator_df %>%
      dplyr::left_join(comerciobr::dic_paises) %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::filter(path == "IMP") %>%
      dplyr::left_join(comerciobr::dic_ncm_fator) %>%
      dplyr::select(-c(co_pais, no_pais, co_ncm, co_fat_agreg)) %>%
      dplyr::distinct_all() %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = no_fat_agreg, values_from = value) %>%
      janitor::clean_names() %>%
      dplyr::rename(Básicos = produtos_basicos,
                    `Semi-manufaturados` = produtos_semimanufaturados,
                    Manufaturados = produtos_manufaturados) %>%
      # dplyr::group_by(path) %>%
      echarts4r::e_charts(co_ano) %>%
      echarts4r::e_river(Básicos) %>%
      echarts4r::e_river(`Semi-manufaturados`) %>%
      echarts4r::e_river(Manufaturados) %>%
      echarts4r::e_tooltip(trigger = "axis") %>%
      echarts4r::e_legend(right = 0, orient = 'vertical')

  })

  output$produtos_exp <- echarts4r::renderEcharts4r({

    barao::comerciobr_dados_produtos(input$pais, "anual") %>%
      dplyr::ungroup() %>%
      # dplyr::filter(co_ano == max(co_ano)) %>%
      dplyr::mutate(pct = value/total) %>%
      dplyr::mutate(co_ano = as.factor(co_ano)) %>%
      dplyr::filter(path == "EXP") %>%
      dplyr::mutate(no_sh4_por = dplyr::case_when(
        stringr::str_length(no_sh4_por) > 30 ~ paste0(stringr::str_sub(no_sh4_por, 1, 28), ".."),
        TRUE ~ no_sh4_por)) %>%
      # tidyr::pivot_wider(names_from = trade_flow, values_from = value) %>%
      dplyr::group_by(no_sh4_por) %>%
      # dplyr::ungroup()
      echarts4r::e_charts(co_ano) %>%
      echarts4r::e_y_axis(formatter = number_si) %>%
      echarts4r::e_line(value) %>%
      echarts4r::e_legend(right = 0) %>%
      echarts4r::e_tooltip()

  })
  output$produtos_imp <- echarts4r::renderEcharts4r({

    barao::comerciobr_dados_produtos(input$pais, "anual") %>%
      dplyr::ungroup() %>%
      # dplyr::filter(co_ano == max(co_ano)) %>%
      dplyr::mutate(pct = value/total) %>%
      dplyr::mutate(co_ano = as.factor(co_ano)) %>%
      dplyr::filter(path == "IMP") %>%
      dplyr::mutate(no_sh4_por = dplyr::case_when(
        stringr::str_length(no_sh4_por) > 30 ~
          paste0(stringr::str_sub(no_sh4_por, 1, 28), ".."),
        TRUE ~ no_sh4_por)) %>%
      # tidyr::pivot_wider(names_from = trade_flow, values_from = value) %>%
      dplyr::group_by(no_sh4_por) %>%
      # dplyr::ungroup()
      echarts4r::e_charts(co_ano) %>%
      echarts4r::e_line(value) %>%
      echarts4r::e_y_axis(formatter = number_si) %>%
      echarts4r::e_legend(right = 0) %>%
      echarts4r::e_tooltip()
  })

  output$paises_exp <- echarts4r::renderEcharts4r({

    barao::comerciobr_dados_paises(input$pais, "anual") %>%
      dplyr::ungroup() %>%
      dplyr::filter(co_ano <= max(co_ano)-1) %>%
      dplyr::arrange((co_ano)) %>%
      dplyr::mutate(co_ano = as.factor(co_ano)) %>%
      dplyr::filter(path == "EXP") %>%
      dplyr::group_by(no_pais) %>%
      echarts4r::e_charts(co_ano) %>%
      echarts4r::e_line(value) %>%
      echarts4r::e_y_axis(formatter = number_si) %>%
      echarts4r::e_legend(right = 0) %>%
      echarts4r::e_tooltip()
  })

  output$paises_imp <- echarts4r::renderEcharts4r({

    barao::comerciobr_dados_paises(input$pais, "anual") %>%
      dplyr::ungroup() %>%
      dplyr::filter(co_ano <= max(co_ano)-1) %>%
      dplyr::arrange((co_ano)) %>%
      dplyr::mutate(co_ano = as.factor(co_ano)) %>%
      dplyr::filter(path == "IMP") %>%
      dplyr::group_by(no_pais) %>%
      echarts4r::e_charts(co_ano) %>%
      echarts4r::e_line(value) %>%
      echarts4r::e_y_axis(formatter = number_si) %>%
      echarts4r::e_legend(right = 0) %>%
      echarts4r::e_tooltip()
  })

  # Funções Investimentos -----

  output$plot1 <- echarts4r::renderEcharts4r({

    investimentos::bc_idp_pais_controlador %>%
      dplyr::filter(discriminacao == input$pais) %>%
      echarts4r::e_charts(ano) %>%
      echarts4r::e_river(value) %>%
      echarts4r::e_tooltip(trigger = "axis")
      # echarts4r::

  })

  output$plot2 <- echarts4r::renderEcharts4r({

    investimentos::bc_idp_pais_provisorio %>%
      dplyr::filter(pais == input$pais) %>%
      echarts4r::e_charts(ano) %>%
      echarts4r::e_line(value) %>%
      echarts4r::e_y_axis(formatter = number_si) %>%
      echarts4r::e_tooltip()
    # echarts4r::

  })

  output$plot3 <- echarts4r::renderEcharts4r({

    pais_filter <- comerciobr::dic_paises %>%
      dplyr::select(-co_pais) %>%
      dplyr::mutate(no_pais_low = abjutils::rm_accent(no_pais)) %>%
      dplyr::mutate(no_pais_low = stringr::str_to_lower(no_pais_low)) %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::pull(no_pais_low)


    investimentos::bndes_posembarque_bens_paises %>%
      dplyr::filter(paises == pais_filter) %>%
      echarts4r::e_charts(ano) %>%
      echarts4r::e_line(value) %>%
      echarts4r::e_y_axis(formatter = number_si) %>%
      echarts4r::e_tooltip()
    })

  output$plot4 <- echarts4r::renderEcharts4r({

    pais_filter <- comerciobr::dic_paises %>%
      dplyr::select(-co_pais) %>%
      dplyr::mutate(no_pais_low = abjutils::rm_accent(no_pais)) %>%
      dplyr::mutate(no_pais_low = stringr::str_to_lower(no_pais_low)) %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::pull(no_pais_low)


    investimentos::bndes_posembarque_bens_operacoes %>%
      dplyr::filter(paises == pais_filter) %>%
      echarts4r::e_charts(ano) %>%
      echarts4r::e_line(value) %>%
      echarts4r::e_y_axis(formatter = number_si) %>%
      echarts4r::e_tooltip()
  })

  #  Comércio Mundo ----

  output$correntemundo <- echarts4r::renderEcharts4r({

    data(World, package = "tmap")

    pais <- comerciomundo::dic_comtrade_mdic %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::pull(id)

    comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code != 3 & trade_flow_code != 4) %>%
      dplyr::group_by(year, trade_flow_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(trade_flow_code = dplyr::case_when(
        trade_flow_code == 1 ~ "Importações",
        trade_flow_code == 2 ~ "Exportações"
      )) %>%
      dplyr::mutate(year = as.factor(year)) %>%
      tidyr::pivot_wider(names_from = trade_flow_code, values_from = value) %>%
      dplyr::mutate(Saldo = Exportações - Importações) %>%
      # tidyr::pivot_longer(2:tidyselect::last_col(), names_to = "trade_flow_code", values_to = "value") %>%
      # dplyr::group_by(trade_flow_code) %>%
      echarts4r::e_charts(year) %>%
      echarts4r::e_line(Exportações) %>%
      echarts4r::e_line(Importações) %>%
      echarts4r::e_area(Saldo) %>%
      echarts4r::e_y_axis(formatter = number_si) %>%
      echarts4r::e_tooltip()

  })

  output$saldomundo <- tmap::renderTmap({

    pais <- comerciomundo::dic_comtrade_mdic %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::pull(id)

    saldo <- reactive({comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::group_by(partner_code, trade_flow_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      tidyr::pivot_wider(names_from = trade_flow_code,
                         values_from = value,
                         values_fill = 0) %>%
      dplyr::rename(imp = `1`, exp = `2`) %>%
      dplyr::mutate(saldo = exp - imp) %>%
      dplyr::select(partner_code, saldo) %>%
      dplyr::rename(id = partner_code) %>%
      dplyr::left_join(comerciomundo::dic_comtrade_mdic) %>%
      tidyr::drop_na() %>%
      dplyr::select(no_pais, co_pais_isoa3, saldo) %>%
      dplyr::rename(iso_a3 = co_pais_isoa3) %>%
      dplyr::ungroup()
      })


    World_pais <- reactive({World %>%
      dplyr::left_join(saldo())})

    tmap::tmap_mode("view")

    tmap::tm_shape(World_pais()) +
      tmap::tm_polygons("saldo")


  })

  output$parceirosmundo_exp <- echarts4r::renderEcharts4r({

    pais <- comerciomundo::dic_comtrade_mdic %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::pull(id)

    top5paises <- comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 2) %>%
      dplyr::group_by(partner_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::pull(partner_code)

    dic_partners <- comerciomundo::dic_partners %>%
      dplyr::mutate(id = as.numeric(id)) %>%
      tidyr::drop_na() %>%
      dplyr::rename(partner_code = id)

    comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 2) %>%
      dplyr::group_by(partner_code, year) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(partner_code %in% top5paises) %>%
      dplyr::left_join(dic_partners) %>%
      dplyr::group_by(text) %>%
      echarts4r::e_charts(year) %>%
      echarts4r::e_river(value) %>%
      echarts4r::e_tooltip(trigger = "axis") %>%
      echarts4r::e_legend(
        right = 0)

  })

  output$parceirosmundo_imp <- echarts4r::renderEcharts4r({

    pais <- comerciomundo::dic_comtrade_mdic %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::pull(id)

    top5paises <- comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 1) %>%
      dplyr::group_by(partner_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::pull(partner_code)

    dic_partners <- comerciomundo::dic_partners %>%
      dplyr::mutate(id = as.numeric(id)) %>%
      tidyr::drop_na() %>%
      dplyr::rename(partner_code = id)

    comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 1) %>%
      dplyr::group_by(partner_code, year) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(partner_code %in% top5paises) %>%
      dplyr::left_join(dic_partners) %>%
      dplyr::group_by(text) %>%
      echarts4r::e_charts(year) %>%
      echarts4r::e_river(value) %>%
      echarts4r::e_tooltip(trigger = "axis") %>%
      echarts4r::e_legend(
        right = 0)

  })

  output$produtosmundo_exp <- echarts4r::renderEcharts4r({

    pais <- comerciomundo::dic_comtrade_mdic %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::pull(id)

    sh2 <- comerciobr::dic_sh6_sh2 %>%
      dplyr::select(CO_SH2, NO_SH2_POR) %>%
      dplyr::rename(commodity_code = CO_SH2) %>%
      dplyr::filter(stringr::str_length(commodity_code) == 2) %>%
      dplyr::distinct()

    top5produtos <- comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 2) %>%
      dplyr::group_by(commodity_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::pull(commodity_code)

    comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 2) %>%
      dplyr::group_by(commodity_code, year) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(commodity_code %in% top5produtos)  %>%
      dplyr::left_join(sh2) %>%
      dplyr::mutate(NO_SH2_POR = dplyr::case_when(stringr::str_length(NO_SH2_POR) > 30 ~ paste0(stringr::str_sub(NO_SH2_POR, 1, 18), ".."),
                                                  TRUE ~ NO_SH2_POR)) %>%
      dplyr::group_by(NO_SH2_POR) %>%
      echarts4r::e_charts(year) %>%
      echarts4r::e_river(value) %>%
      echarts4r::e_tooltip(trigger = "axis") %>%
      echarts4r::e_legend(
        right = 0)

  })

  output$produtosmundo_imp <- echarts4r::renderEcharts4r({

    pais <- comerciomundo::dic_comtrade_mdic %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::pull(id)

    sh2 <- comerciobr::dic_sh6_sh2 %>%
      dplyr::select(CO_SH2, NO_SH2_POR) %>%
      dplyr::rename(commodity_code = CO_SH2) %>%
      dplyr::filter(stringr::str_length(commodity_code) == 2) %>%
      dplyr::distinct()

    top5produtos <- comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 1) %>%
      dplyr::group_by(commodity_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::pull(commodity_code)

    comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 1) %>%
      dplyr::group_by(commodity_code, year) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(commodity_code %in% top5produtos)  %>%
      dplyr::left_join(sh2) %>%
      dplyr::mutate(NO_SH2_POR = dplyr::case_when(stringr::str_length(NO_SH2_POR) > 30 ~ paste0(stringr::str_sub(NO_SH2_POR, 1, 18), ".."),
                                                  TRUE ~ NO_SH2_POR)) %>%
      dplyr::group_by(NO_SH2_POR) %>%
      echarts4r::e_charts(year) %>%
      echarts4r::e_river(value) %>%
      echarts4r::e_tooltip(trigger = "axis") %>%
      echarts4r::e_legend(
        right = 0)

  })

  output$prodpaismundo_exp <- echarts4r::renderEcharts4r({

    pais <- comerciomundo::dic_comtrade_mdic %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::pull(id)

    top5paises <- comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 2) %>%
      dplyr::group_by(partner_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::pull(partner_code)

    sh2 <- comerciobr::dic_sh6_sh2 %>%
      dplyr::select(CO_SH2, NO_SH2_POR) %>%
      dplyr::rename(commodity_code = CO_SH2) %>%
      dplyr::filter(stringr::str_length(commodity_code) == 2) %>%
      dplyr::distinct()

    dic_partners <- comerciomundo::dic_partners %>%
      dplyr::mutate(id = as.numeric(id)) %>%
      tidyr::drop_na() %>%
      dplyr::rename(partner_code = id)

    nomes_paises <- comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 2) %>%
      dplyr::group_by(partner_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::rename(id = partner_code) %>%
      dplyr::left_join(comerciomundo::dic_comtrade_mdic) %>%
      dplyr::arrange(no_pais) %>%
      dplyr::pull(no_pais)

    nomes_paises_lista <- lapply(seq_along(nomes_paises), function(i) {
      return(
        list(
          text = nomes_paises[[i]],
          top = 0,
          left = 0)
      )
    })

    comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 1) %>%
      dplyr::filter(partner_code %in% top5paises) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::group_by(partner_code, commodity_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::group_by(partner_code) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::left_join(sh2) %>%
      dplyr::rename(id = partner_code) %>%
      dplyr::left_join(comerciomundo::dic_comtrade_mdic) %>%
      dplyr::mutate(NO_SH2_POR = dplyr::case_when(stringr::str_length(NO_SH2_POR) > 30 ~ paste0(stringr::str_sub(NO_SH2_POR, 1, 18), ".."),
                                                  TRUE ~ NO_SH2_POR)) %>%
      dplyr::group_by(no_pais) %>%
      echarts4r::e_charts(NO_SH2_POR, timeline = T) %>%
      echarts4r::e_bar(value) %>%
      echarts4r::e_tooltip(trigger = "axis") %>%
      echarts4r::e_legend(show = F) %>%
      echarts4r::e_grid(left = "25%", bottom = "25%") %>%
      echarts4r::e_flip_coords() %>%
      echarts4r::e_y_axis(axisLabel = list(
        fontSize = 8
      )) %>%
      echarts4r::e_x_axis(formatter = number_si) %>%
      echarts4r::e_timeline_opts(
        axis_type = "category",
        label = list(fontSize = 4,
                     interval = 0
        ),
        progress = list(label =
                          list(fontSize = 5)
        )
      ) %>%
      echarts4r::e_timeline_serie(
        title = nomes_paises_lista
      ) %>%
      echarts4r::e_title(text = "") %>%
      echarts4r::e_title(
        subtext = "Fonte: Comtrade-ONU",
        left = "right", bottom = 10)

  })

  output$prodpaismundo_imp <- echarts4r::renderEcharts4r({


    pais <- comerciomundo::dic_comtrade_mdic %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::pull(id)

    top5paises <- comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 1) %>%
      dplyr::group_by(partner_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::pull(partner_code)

    sh2 <- comerciobr::dic_sh6_sh2 %>%
      dplyr::select(CO_SH2, NO_SH2_POR) %>%
      dplyr::rename(commodity_code = CO_SH2) %>%
      dplyr::filter(stringr::str_length(commodity_code) == 2) %>%
      dplyr::distinct()

    dic_partners <- comerciomundo::dic_partners %>%
      dplyr::mutate(id = as.numeric(id)) %>%
      tidyr::drop_na() %>%
      dplyr::rename(partner_code = id)

    nomes_paises <- comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 1) %>%
      dplyr::group_by(partner_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::rename(id = partner_code) %>%
      dplyr::left_join(comerciomundo::dic_comtrade_mdic) %>%
      dplyr::arrange(no_pais) %>%
      dplyr::pull(no_pais)

    nomes_paises_lista <- lapply(seq_along(nomes_paises), function(i) {
      return(
        list(
          text = nomes_paises[[i]],
          top = 0,
          left = 0)
      )
    })

    comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 1) %>%
      dplyr::filter(partner_code %in% top5paises) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::group_by(partner_code, commodity_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::group_by(partner_code) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::left_join(sh2) %>%
      dplyr::rename(id = partner_code) %>%
      dplyr::left_join(comerciomundo::dic_comtrade_mdic) %>%
      dplyr::mutate(NO_SH2_POR = dplyr::case_when(stringr::str_length(NO_SH2_POR) > 30 ~ paste0(stringr::str_sub(NO_SH2_POR, 1, 18), ".."),
                                                  TRUE ~ NO_SH2_POR)) %>%
      dplyr::group_by(no_pais) %>%
      echarts4r::e_charts(NO_SH2_POR, timeline = T) %>%
      echarts4r::e_bar(value) %>%
      echarts4r::e_tooltip(trigger = "axis") %>%
      echarts4r::e_legend(show = F) %>%
      echarts4r::e_grid(left = "25%", bottom = "25%") %>%
      echarts4r::e_flip_coords() %>%
      echarts4r::e_y_axis(axisLabel = list(
        fontSize = 8
      )) %>%
      echarts4r::e_x_axis(formatter = number_si) %>%
      echarts4r::e_timeline_opts(
        axis_type = "category",
        label = list(fontSize = 4,
                     interval = 0
        ),
        progress = list(label =
                          list(fontSize = 5)
        )
      ) %>%
      echarts4r::e_timeline_serie(
        title = nomes_paises_lista
      ) %>%
      echarts4r::e_title(text = "") %>%
      echarts4r::e_title(
        subtext = "Fonte: Comtrade-ONU",
        left = "right", bottom = 10)


  })

  output$paisprodmundo_exp <- echarts4r::renderEcharts4r({

    pais <- comerciomundo::dic_comtrade_mdic %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::pull(id)

    top5produtos <- comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 1) %>%
      dplyr::group_by(commodity_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::pull(commodity_code)

    sh2 <- comerciobr::dic_sh6_sh2 %>%
      dplyr::select(CO_SH2, NO_SH2_POR) %>%
      dplyr::rename(commodity_code = CO_SH2) %>%
      dplyr::filter(stringr::str_length(commodity_code) == 2) %>%
      dplyr::distinct()

    dic_partners <- comerciomundo::dic_partners %>%
      dplyr::mutate(id = as.numeric(id)) %>%
      tidyr::drop_na() %>%
      dplyr::rename(partner_code = id)

    nomes_produtos <- comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 1) %>%
      dplyr::group_by(commodity_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::left_join(sh2) %>%
      dplyr::arrange(NO_SH2_POR) %>%
      dplyr::mutate(NO_SH2_POR = dplyr::case_when(stringr::str_length(NO_SH2_POR) > 40 ~  paste0(stringr::str_sub(NO_SH2_POR, 1, 38), ".."),
                                                  TRUE ~ NO_SH2_POR)) %>%
      dplyr::pull(NO_SH2_POR)

    nomes_produtos_lista <- lapply(seq_along(nomes_produtos), function(i) {
      return(
        list(
          text = nomes_produtos[[i]],
          top = 0,
          left = 0)
      )
    })

    comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 1) %>%
      dplyr::filter(commodity_code %in% top5produtos) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::group_by(partner_code, commodity_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::group_by(commodity_code) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::left_join(sh2) %>%
      dplyr::left_join(dic_partners) %>%
      dplyr::mutate(NO_SH2_POR = dplyr::case_when(stringr::str_length(NO_SH2_POR) > 20 ~ paste0(stringr::str_sub(NO_SH2_POR, 1, 18), ".."),
                                                  TRUE ~ NO_SH2_POR)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(NO_SH2_POR) %>%
      dplyr::group_by(NO_SH2_POR) %>%
      echarts4r::e_charts(text, timeline = T, bottom = "5%",
                          axisType = 'value') %>%
      # timelineStyle = list(fontSize = 12)) %>%
      echarts4r::e_bar(value) %>%
      echarts4r::e_tooltip(trigger = "axis") %>%
      echarts4r::e_legend(show = F) %>%
      echarts4r::e_grid(left = "25%", bottom = "25%") %>%
      echarts4r::e_flip_coords() %>%
      echarts4r::e_y_axis(axisLabel = list(
        fontSize = 8
      )) %>%
      echarts4r::e_x_axis(formatter = number_si) %>%
      # echarts4r::e_title(subtext = "",
      # left = "right", bottom = 0) %>%
      echarts4r::e_timeline_opts(
        axis_type = "category",
        label = list(fontSize = 4,
                     interval = 0
        ),
        progress = list(label =
                          list(fontSize = 5)
        )
      ) %>%
      echarts4r::e_timeline_serie(
        title = nomes_produtos_lista
      ) %>%
      echarts4r::e_title(text = "") %>%
      echarts4r::e_title(
        subtext = "Fonte: Comtrade-ONU",
        left = "right", bottom = 10)

  })


  output$paisprodmundo_imp <- echarts4r::renderEcharts4r({

    pais <- comerciomundo::dic_comtrade_mdic %>%
      dplyr::filter(no_pais == input$pais) %>%
      dplyr::pull(id)

    top5produtos <- comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 1) %>%
      dplyr::group_by(commodity_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::pull(commodity_code)

    sh2 <- comerciobr::dic_sh6_sh2 %>%
      dplyr::select(CO_SH2, NO_SH2_POR) %>%
      dplyr::rename(commodity_code = CO_SH2) %>%
      dplyr::filter(stringr::str_length(commodity_code) == 2) %>%
      dplyr::distinct()

    dic_partners <- comerciomundo::dic_partners %>%
      dplyr::mutate(id = as.numeric(id)) %>%
      tidyr::drop_na() %>%
      dplyr::rename(partner_code = id)

    nomes_produtos <- comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == pais) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 2) %>%
      dplyr::group_by(commodity_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::left_join(sh2) %>%
      dplyr::arrange(NO_SH2_POR) %>%
      dplyr::mutate(NO_SH2_POR = dplyr::case_when(stringr::str_length(NO_SH2_POR) > 40 ~  paste0(stringr::str_sub(NO_SH2_POR, 1, 38), ".."),
                                                  TRUE ~ NO_SH2_POR)) %>%
      dplyr::pull(NO_SH2_POR)

    nomes_produtos_lista <- lapply(seq_along(nomes_produtos), function(i) {
      return(
        list(
          text = nomes_produtos[[i]],
          top = 0,
          left = 0)
      )
    })

    comerciomundo::comtrade %>%
      dplyr::filter(reporter_code == 818) %>%
      dplyr::filter(partner_code != 0) %>%
      dplyr::filter(trade_flow_code == 2) %>%
      dplyr::filter(commodity_code %in% top5produtos) %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::group_by(partner_code, commodity_code) %>%
      dplyr::summarise(value = sum(trade_value_us)) %>%
      dplyr::group_by(commodity_code) %>%
      dplyr::slice_max(value, n = 5) %>%
      dplyr::left_join(sh2) %>%
      dplyr::left_join(dic_partners) %>%
      dplyr::mutate(NO_SH2_POR = dplyr::case_when(stringr::str_length(NO_SH2_POR) > 20 ~ paste0(stringr::str_sub(NO_SH2_POR, 1, 18), ".."),
                                                  TRUE ~ NO_SH2_POR)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(NO_SH2_POR) %>%
      dplyr::group_by(NO_SH2_POR) %>%
      echarts4r::e_charts(text, timeline = T, bottom = "5%",
                          axisType = 'value') %>%
      echarts4r::e_bar(value) %>%
      echarts4r::e_tooltip(trigger = "axis") %>%
      echarts4r::e_legend(show = F) %>%
      echarts4r::e_grid(left = "25%", bottom = "25%") %>%
      echarts4r::e_flip_coords() %>%
      echarts4r::e_y_axis(axisLabel = list(
        fontSize = 8
      )) %>%
      echarts4r::e_x_axis(formatter = number_si) %>%
      echarts4r::e_timeline_opts(
        axis_type = "category",
        label = list(fontSize = 4,
                     interval = 0
        ),
        progress = list(label =
                          list(fontSize = 5)
        )
      ) %>%
      echarts4r::e_timeline_serie(
        title = nomes_produtos_lista
      ) %>%
      echarts4r::e_title(text = "") %>%
      echarts4r::e_title(
        subtext = "Fonte: Comtrade-ONU",
        left = "right", bottom = 10)

  })


}




# saldo <- comerciomundo::comtrade %>%
#   dplyr::filter(reporter_code == 156) %>%
#   dplyr::filter(year == max(year)) %>%
#   dplyr::filter(partner_code != 0) %>%
#   dplyr::group_by(partner_code, trade_flow_code) %>%
#   dplyr::summarise(value = sum(trade_value_us)) %>%
#   tidyr::pivot_wider(names_from = trade_flow_code,
#                      values_from = value,
#                      values_fill = 0) %>%
#   dplyr::rename(imp = `1`, exp = `2`) %>%
#   dplyr::mutate(saldo = exp - imp) %>%
#   dplyr::select(partner_code, saldo) %>%
#   dplyr::rename(id = partner_code) %>%
#   dplyr::left_join(comerciomundo::dic_comtrade_mdic) %>%
#   tidyr::drop_na() %>%
#   dplyr::select(no_pais, co_pais_isoa3, saldo) %>%
#   dplyr::rename(iso_a3 = co_pais_isoa3) %>%
#   dplyr::ungroup()
#
# data(World, package = "tmap")
#
# World <- World %>%
#   dplyr::left_join(saldo)
#
# tmap::tmap_mode("view")
#
#
# tmap::tm_shape(World) +
#      tmap::tm_polygons("saldo")
#
# #
# #
# pais <- comerciomundo::dic_comtrade_mdic %>%
#   dplyr::filter(no_pais == "Angola") %>%
#   dplyr::pull(id)
#
# top5produtos <- comerciomundo::comtrade %>%
#   dplyr::filter(reporter_code == pais) %>%
#   dplyr::filter(year == max(year)) %>%
#   dplyr::filter(partner_code != 0) %>%
#   dplyr::filter(trade_flow_code == 1) %>%
#   dplyr::group_by(commodity_code) %>%
#   dplyr::summarise(value = sum(trade_value_us)) %>%
#   dplyr::slice_max(value, n = 5) %>%
#   dplyr::pull(commodity_code)
#
# sh2 <- comerciobr::dic_sh6_sh2 %>%
#   dplyr::select(CO_SH2, NO_SH2_POR) %>%
#   dplyr::rename(commodity_code = CO_SH2) %>%
#   dplyr::filter(stringr::str_length(commodity_code) == 2) %>%
#   dplyr::distinct()
#
# dic_partners <- comerciomundo::dic_partners %>%
#   dplyr::mutate(id = as.numeric(id)) %>%
#   tidyr::drop_na() %>%
#   dplyr::rename(partner_code = id)
#
# nomes_produtos <- comerciomundo::comtrade %>%
#   dplyr::filter(year == max(year)) %>%
#   dplyr::filter(reporter_code == 818) %>%
#   dplyr::filter(partner_code != 0) %>%
#   dplyr::filter(trade_flow_code == 1) %>%
#   dplyr::group_by(commodity_code) %>%
#   dplyr::summarise(value = sum(trade_value_us)) %>%
#   dplyr::slice_max(value, n = 5) %>%
#   dplyr::left_join(sh2) %>%
#   dplyr::arrange(NO_SH2_POR) %>%
#   dplyr::mutate(NO_SH2_POR = dplyr::case_when(stringr::str_length(NO_SH2_POR) > 40 ~  paste0(stringr::str_sub(NO_SH2_POR, 1, 38), ".."),
#                                               TRUE ~ NO_SH2_POR)) %>%
#   dplyr::pull(NO_SH2_POR)
#
# nomes_produtos_lista <- lapply(seq_along(nomes_produtos), function(i) {
#   return(
#     list(
#       text = nomes_produtos[[i]],
#       top = 0,
#       left = 0)
#   )
# })
#
# comerciomundo::comtrade %>%
#   dplyr::filter(reporter_code == 818) %>%
#   dplyr::filter(partner_code != 0) %>%
#   dplyr::filter(trade_flow_code == 1) %>%
#   dplyr::filter(commodity_code %in% top5produtos) %>%
#   dplyr::filter(year == max(year)) %>%
#   dplyr::group_by(partner_code, commodity_code) %>%
#   dplyr::summarise(value = sum(trade_value_us)) %>%
#   dplyr::group_by(commodity_code) %>%
#   dplyr::slice_max(value, n = 5) %>%
#   dplyr::left_join(sh2) %>%
#   dplyr::left_join(dic_partners) %>%
#   dplyr::mutate(NO_SH2_POR = dplyr::case_when(stringr::str_length(NO_SH2_POR) > 20 ~ paste0(stringr::str_sub(NO_SH2_POR, 1, 18), ".."),
#                                               TRUE ~ NO_SH2_POR)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(NO_SH2_POR) %>%
#   dplyr::group_by(NO_SH2_POR) %>%
#   echarts4r::e_charts(text, timeline = T, bottom = "5%",
#                       axisType = 'value') %>%
#   # timelineStyle = list(fontSize = 12)) %>%
#   echarts4r::e_bar(value) %>%
#   echarts4r::e_tooltip(trigger = "axis") %>%
#   echarts4r::e_legend(show = F) %>%
#   echarts4r::e_grid(left = "25%", bottom = "25%") %>%
#   echarts4r::e_flip_coords() %>%
#   echarts4r::e_y_axis(axisLabel = list(
#     fontSize = 8
#   )) %>%
#   echarts4r::e_x_axis(formatter = number_si) %>%
#   # echarts4r::e_title(subtext = "",
#   # left = "right", bottom = 0) %>%
#   echarts4r::e_timeline_opts(
#     axis_type = "category",
#     label = list(fontSize = 4,
#                  interval = 0
#     ),
#     progress = list(label =
#                       list(fontSize = 5)
#     )
#   ) %>%
#   echarts4r::e_timeline_serie(
#     title = nomes_produtos_lista
#   ) %>%
#   echarts4r::e_title(text = "") %>%
#   echarts4r::e_title(
#     subtext = "Fonte: Comtrade-ONU",
#     left = "right", bottom = 10)
#
