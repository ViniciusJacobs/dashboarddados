# dashboarddados
What the Package Does (One Line, Title Case)

<!-- README.md is generated from README.Rmd. Please edit that file -->

### Dashboard Previdência (code).

``` r
library(shiny)
library(shinydashboard)
#> Warning: package 'shinydashboard' was built under R version 4.1.1
library(tidyverse)
library(shinydashboard)
library(DT)
library(leaflet)
library(sf)
library(viridis)
library(ps)
```

``` r

ui <- dashboardPage(
  dashboardHeader(title = "Previdência"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Números da Entidade de Prev.", tabName = "info"),
      menuItem("Mapa Dist", tabName = "map_dist"),
      menuItem("Folha", tabName = "Folha"),
      menuItem("Descrição", tabName = "Descricao")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "info",
        h2("Números dos Planos de Benefícios"),
        br(),
        fluidRow(
          box(
            width = 12,
            selectInput(
              inputId = "plano_benef",
              label = "Selecione o Plano de Benefícios",
              choices = c("Todos Planos","Plano A","Plano PREV B", "Plano Novo C", "Plano Mig D", "Plano Antigo E"),
              width = "30%"
            ),
          ),
          infoBoxOutput(
            outputId = "ativos_plan",
            width = 4
          ),
          infoBoxOutput(
            outputId = "autopat_plan",
            width = 4
          ),
          infoBoxOutput(
            outputId = "bpd_plan",
            width = 4
          ),
          infoBoxOutput(
            outputId = "assis_plan",
            width = 4
          ),
          infoBoxOutput(
            outputId = "pens_plan",
            width = 4)
        ),
        fluidRow(
          box(width = 6,
              title = "Distribuição",
              footer = "*Distribuição por plano de beneficio",
              status = "info", solidHeader = TRUE, collapsible = TRUE,
              plotOutput("distribuicao", height = 300)
          )
          )
      ),
      tabItem(
        tabName = "map_dist",
        h2("Mapa - Distribuição por Estado"),
        br(),
        fluidPage(
          box(
            width = 5,
            selectInput(
              inputId = "mapa_dist",
              label = "Selecione plano",
              choices = c("Todos Planos","Plano A","Plano PREV B", "Plano Novo C", "Plano Mig D", "Plano Antigo E"),
              width = "40%"
            )
          )
        ),
        fluidRow(
          box(leafletOutput("mapa"))
        )
      ),
        tabItem(
        tabName = "Folha",
        h2("Folha Salários e Benefícios"),
        br(),
        fluidPage(
        box(
          width = 5,
          selectInput(
            inputId = "folha_plano",
            label = "Selecione plano",
            choices = c("Plano A","Plano PREV B", "Plano Novo C", "Plano Mig D", "Plano Antigo E"),
            width = "40%"
          )
        ),
        box(
          width = 5,
          selectInput(
            inputId = "assis_pensi",
            label = "Selecione por tipo de benefício",
            choices = c("Assistido","Pensionista"),
            width = "40%"
          )
        ),
          sliderInput(
          width = "45%",
          inputId = "beneficio",
          label = "Itervalo de Benefício",
          min = 0,
          max = 13500,
          value = c(1000, 12000),
          step = 500,
          sep = ""
        )
        ),
          fluidRow(

          box(
            width = 6,
            title = "Benefício (Assistidos ou Pensionistas)",
            solidHeader = TRUE,
            status = "info", collapsible = TRUE,
            plotOutput("beneficio")
            ),
            box(
              width = 6,
              title = "Salários (Ativos)",
              solidHeader = TRUE,
              status = "info", collapsible = TRUE,
              plotOutput("salario")
            )
          )
        ),
        tabItem(
        tabName = "Descricao",
        h2("Base de Dados da análise"),
        br(),
        fluidRow(
          box(width = 12,
              title = "Informacoes-Base",
              status = "info", solidHeader = TRUE, collapsible = TRUE,
              dataTableOutput("Desc", height = 500)
         )
        )
       )
      )
    )
  )


server <- function(input, output, session) {

  prev <- read_rds("../appvf/previ_shiny.rds")
  tab_prev <- read_rds("../appvf/tab_prev.rds")
  tab_estados <- geobr::read_state()

  output$ativos_plan <- renderInfoBox({

    if(input$plano_benef == "Todos Planos"){
        v1 <-  prev %>%
        filter(Situacao_Plano == "Ativo")
    }else{
      v1 <-  prev %>%
        filter(Situacao_Plano == "Ativo"& Plano == input$plano_benef)
    }

    ativos_plan <- nrow(v1)

    infoBox(
      title = "Ativos",
      value = ativos_plan,
      icon = icon("user-friends"),
      fill = TRUE
    )
  })

  output$autopat_plan <- renderInfoBox({

    if(input$plano_benef == "Todos Planos"){
      v2 <-  prev %>%
        filter(Situacao_Plano == "Autopatrocinado")
    }else{
      v2 <-  prev %>%
      filter(Situacao_Plano == "Autopatrocinado"& Plano == input$plano_benef)
    }

    autopat_plan <- nrow(v2)

    infoBox(
      title = "Autopatrocinados",
      value = autopat_plan,
      icon = icon("user-friends"),
      fill = TRUE
    )
  })

  output$bpd_plan <- renderInfoBox({

    if(input$plano_benef == "Todos Planos"){
      v3 <-  prev %>%
        filter(Situacao_Plano == "BPD")
    }else{
      v3 <-  prev %>%
        filter(Situacao_Plano == "BPD"& Plano == input$plano_benef)
    }

    bpd_plan <- nrow(v3)

    infoBox(
      title = "BPDs",
      value = bpd_plan,
      icon = icon("user-friends"),
      fill = TRUE
    )
  })

  output$assis_plan <- renderInfoBox({

    if(input$plano_benef == "Todos Planos"){
      v4 <-  prev %>%
        filter(Situacao_Plano == "Assistido")
    }else{
      v4 <-  prev %>%
        filter(Situacao_Plano == "Assistido"& Plano == input$plano_benef)
    }

      assis_plan <- nrow(v4)

    infoBox(
      title = "Assistidos",
      value = assis_plan,
      icon = icon("user-friends"),
      fill = TRUE
    )
  })

  output$pens_plan <- renderInfoBox({

    if(input$plano_benef == "Todos Planos"){
      v5 <-  prev %>%
        filter(Situacao_Plano == "Pensionista")
    }else{
      v5 <-  prev %>%
        filter(Situacao_Plano == "Pensionista"& Plano == input$plano_benef)
    }
    pens_plan <- nrow(v5)

    infoBox(
      title = "Pensionistas",
      value = pens_plan,
      icon = icon("user-friends"),
      fill = TRUE
    )
  })



  output$distribuicao <- renderPlot({

    if(input$plano_benef == "Todos Planos"){
      grafico2 <- prev %>%
        group_by(Situacao_Plano) %>%
        count() %>%
        arrange(desc(n))
    }else{
      grafico2 <- prev %>%
        dplyr::filter(Plano == input$plano_benef) %>%
        group_by(Situacao_Plano) %>%
        count() %>%
        arrange(desc(n))
    }

    ggplot(grafico2) +
      aes(x = reorder(Situacao_Plano, n), weight = n, fill = Situacao_Plano) +
      geom_bar(width = 0.75) +
      labs(x = "Situacao do Participante", y = "Quantidade")+ #title = "") +
      coord_flip() +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$salario <- renderPlot({
    prev %>%
      filter(Plano %in% input$folha_plano) %>%
      filter(Situacao_Plano %in% "Ativo") %>%
      ggplot() +
      aes(x = Educacao, y = salario, fill = Educacao) +
      geom_boxplot() +
      scale_fill_hue() +
      labs(x = "Escolaridade", y = "Salario", title = "Dist. Salarios Ativos ", subtitle = "Nivel de Escolaridade", fill = "Escolaridade") +
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))

  })

  output$salario <- renderPlot({

    prev %>%
      filter(Plano %in% input$folha_plano) %>%
      filter(Situacao_Plano %in% "Ativo") %>%
      ggplot() +
      aes(x = Educacao, y = salario, fill = Educacao) +
      geom_boxplot() +
      scale_fill_hue() +
      labs(x = "Escolaridade", y = "Salario", title = "Dist. Salarios Ativos ", subtitle = "Nivel de Escolaridade", fill = "Escolaridade") +
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))

  })


  output$beneficio <- renderPlot({

      prev2 <- prev %>%
        filter(Situacao_Plano == "Assistido"|Situacao_Plano == "Pensionista") %>%
        mutate(benef2 = (beneficio + judicial))

      prev2 %>%
        filter(Plano %in% input$folha_plano) %>%
        filter(Situacao_Plano %in% input$assis_pensi) %>%
        filter(benef2 %in% input$beneficio[1]:input$beneficio[2]) %>%
        ggplot() +
        aes(x = benef2, colour = Plano) +
        geom_histogram(bins = 54L, fill = "#0c4c8a") +
        scale_color_viridis_d(option = "viridis") +
        labs(x = "Beneficio", y = "Quantidade de Beneficios", title = "Distribuicao dos beneficios") +
        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5))

  })





  output$Desc <- renderDataTable({

    datatable(prev[,-3], options = list(searching = FALSE,pageLength = 10,
                                     lengthMenu = c(5, 10, 15, 20),
                                     scrollX = T),
              style = "default")

  })





  output$mapa <- renderLeaflet({

    if(input$mapa_dist == "Todos Planos"){
      tab_prev2 <- tab_prev %>%
        filter(!is.na(Plano)) %>%
        group_by(Estado) %>%
        count() %>%
        arrange(desc(n))
    }else{
      tab_prev2 <- tab_prev %>%
        filter(Plano == input$mapa_dist) %>%
        group_by(Estado) %>%
        count() %>%
        arrange(desc(n))
    }

    color_fun <- colorNumeric(
      viridis::viridis(nrow(tab_prev2)),
      domain = range(tab_prev2$n)
    )

    tab_estados %>%
      inner_join(tab_prev2 , by = c("abbrev_state" = "Estado")) %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(
        weight = .5,
        color = "black",
        opacity = .9,
        fillOpacity = .6,
        fillColor = ~color_fun(n),
        label = ~(n)

      )

  })






}

shinyApp(ui, server)

```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
