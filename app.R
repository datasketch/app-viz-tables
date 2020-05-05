library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(dsmodules)
library(htmlwidgets)
library(hotr)
library(tidyverse)
library(homodatum)
library(reactable)

# Internacionalización
# Arreglar código
# Problema con parámetro width que depende de full width
# Mirar los parámetros de sortable...
# TODAS: títulos de las secciones (estílos), nombres secciónes, color input (título, estílos),
# unidades (width, height pixeles)

ui <- panelsPage(panel(title = "Upload Data", 
                       width = 200,
                       body = tableInputUI("initial_data",
                                           choices = list("Sample data" = "sampleData",
                                                          "Copy & paste" = "pasted",
                                                          "CSV/XLS Upload" = "fileUpload",
                                                          "Google sheets" = "googleSheets"),
                                           selected = "sampleData")),
                 panel(title = "Dataset",
                       width = 300,
                       body = uiOutput("data_preview")),
                 panel(title = "Options",
                       width = 250,
                       body = uiOutput("controls")),
                 panel(title = "Viz",
                       can_collapse = FALSE,
                       body = div(reactableOutput("result"),
                                  shinypanels::modal(id = "test",
                                                     title = "Download table",
                                                     dsmodules::downloadHtmlwidgetUI("download_data_button", "Download HTML"))),
                       footer = shinypanels::modalButton(label = "Download table", modal_id = "test")))


server <- function(input, output, session) {
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  output_parmesan("controls", 
                  parmesan = parmesan,
                  input = input,
                  output = output)
  
  inputData <- callModule(tableInput, 
                          "initial_data",
                          sampleFile = list("Emission per capita C02" = "data/sampleData/emisiones_c02.csv", 
                                            "South America population" = "data/sampleData/poblacion.csv"))
  
  output$data_preview <- renderUI({
    req(inputData())
    suppressWarnings(hotr("hotr_input", data = inputData(), order = NULL, options = list(height = 470), enableCTypes = FALSE))
  })
  
  dt <- reactive({
    req(input$hotr_input)
    hotr_table(input$hotr_input)
  })
  
  cols_nms <- reactive({
    req(dt())
    names(dt())
  })
  
  rctbl <- reactive({
    req(dt())
    
    sl <- NULL
    if (sum(input$selection) > 0) 
      sl <- "multiple"
    
    st <- paste0("color: ", input$color, "; font-family: ", input$font_family, "; font-size: ", input$font_size, "px;")
      
    reactable(dt(),
              
              height = input$height,
              fullWidth = ifelse(input$full_width == "full_width", TRUE, FALSE),
              width = ifelse(input$full_width == "full_width", "auto", input$width),
              wrap = input$wrap,
              resizable = input$resizable,

              outlined = input$outlined,
              bordered = ifelse(!input$outlined, FALSE, input$bordered),
              borderless = !input$borderless,
              striped = input$striped,
              compact = input$compact,
              highlight = input$highlight,

              pagination = input$show_pagination,
              showPagination = input$show_pagination,
              showPageInfo = ifelse(input$show_pagination, input$show_page_info, FALSE),
              showPageSizeOptions = ifelse(input$show_pagination, input$page_size_control, FALSE),
              paginationType = input$page_type,
              defaultPageSize = input$page_size,

              # sortable = input$sortable,
              # showSortIcon = input$sortable,
              # showSortable =  input$sortable,
              sortable = input$sortable,
              showSortIcon = TRUE,
              showSortable =  TRUE,
              groupBy = input$group_by,
              filterable = input$filterable,
              searchable = input$searchable,
              selection = sl,

              pageSizeOptions = seq(5, nrow(inputData()), 5),
              
              style = st
              )
  })
  
  # renderizando reactable
  output$result <- renderReactable({
    session$sendCustomMessage("setButtonState", c("none", "download_data_button-downloadHtmlwidget"))
    req(rctbl())
    rctbl()
  })

  # descargas
  callModule(downloadHtmlwidget, "download_data_button", widget = reactive(rctbl()), name = "table")
    
}


shinyApp(ui, server)