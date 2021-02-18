#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
retirar <- c("Não Definido", "Não Declarados", "Sem Informação",
             "Bancos Centrais", "Organizações Internacionais", "Bancos Centrais",
             "A Designar", "Provisão de Navios e Aeronaves", "Zona do Canal do Panamá",
             "União das Repúblicas Socialistas Soviéticas", "Tchecoslováquia",
             "Território Antártico Britânico", "Território da Alta Comissão do Pacífico Ocidental",
             "Território Britânico do Oceano Índico", "Terras Austrais Francesas","Iugoslávia",
            "Iêmen Democrático", "Internação na Zona Franca de Manaus", "Alboran-Perejil, Ilhas")
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "Retrato Econômico"),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "tabs",
          selectInput("pais", "Países",
                      choices = comerciobr::dic_paises$no_pais[!comerciobr::dic_paises$no_pais %in% retirar],
                      selected = "China"),
          shinydashboard::menuItem("Comércio", icon = icon("bar-chart-o"),
            shinydashboard::menuSubItem("Comércio Brasil-País", tabName = "comerciobr"),
            shinydashboard::menuSubItem("Comércio País-Mundo", tabName = "comerciomundo")),
          # shinydashboard::menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          shinydashboard::menuItem("Investimentos", icon = icon("th"), tabName = "investimentos")
          )
        ),
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem("dashboard", "Dashboard tab content"),
          shinydashboard::tabItem("investimentos",
                                  h2("Dados de Investimentos teste"),
                                  fluidRow(
                                    shinydashboard::box(title = "Dados de Estoque",
                                                      echarts4r::echarts4rOutput("plot1", height = 300)),
                                    shinydashboard::box(title = "Dados de Fluxo",
                                                      echarts4r::echarts4rOutput("plot2", height = 300))
                                    )
                                  # fluidRow(
                                    # shinydashboard::box(title = "BNDES",
                                                        # echarts4r::echarts4rOutput("plot3", height = 300)),
                                    # shinydashboard::box(title = "BNDES-2",
                                                        # echarts4r::echarts4rOutput("plot4", height = 300))
                                  # )
          ),
          shinydashboard::tabItem("comerciobr",
                                  fluidRow(
                                    shinydashboard::box(title = "Fluxo de Comércio", echarts4r::echarts4rOutput("corrente", height = 300)),
                                    shinydashboard::tabBox(title = "Composição",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("fator_exp", height = 300)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("fator_imp", height = 300))
                                                           )
                                    ),
                                  fluidRow(
                                    shinydashboard::tabBox(title = "Produtos",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("produtos_exp", height = 300)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("produtos_imp", height = 300))
                                                           ),
                                    shinydashboard::tabBox(title = "Países",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("paises_exp", height = 300)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("paises_imp", height = 300))
                                                           )
                                    )
                                  ),
          shinydashboard::tabItem("comerciomundo",
                                  fluidRow(
                                    shinydashboard::box(title = "Fluxo de Comércio", echarts4r::echarts4rOutput("correntemundo", height = 300)),
                                    shinydashboard::box(title = "Saldos Comerciais", tmap::tmapOutput("saldomundo", height = 400))
                                    ),
                                  fluidRow(
                                    shinydashboard::tabBox(title = "Produtos",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("produtosmundo_exp", height = 300)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("produtosmundo_imp", height = 400))
                                                           ),
                                    shinydashboard::tabBox(title = "Países",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("parceirosmundo_exp", height = 300)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("parceirosmundo_imp", height = 300))
                                                           )
                                    ),
                                  fluidRow(
                                    shinydashboard::tabBox(title = "Destinos e origens dos principais produtos",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("prodpaismundo_exp", height = 500)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("prodpaismundo_imp", height = 500))
                                    ),
                                    shinydashboard::tabBox(title = "Produtos dos principais países",
                                                           tabPanel("exp", echarts4r::echarts4rOutput("paisprodmundo_exp", height = 500)),
                                                           tabPanel("imp", echarts4r::echarts4rOutput("paisprodmundo_imp", height = 500))
                                    )
                                  )

          ))
        )
      )
  )
  }

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'retratoeconomico'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

