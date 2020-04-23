library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(dsmodules)
library(hotr)
library(tidyverse)
library(homodatum)
library(reactable)

ui <- panelsPage(panel(title = "Upload Data", 
                       width = 400,
                       body = tableInputUI("initial_data",
                                           choices = list("Sample data" = "sampleData",
                                                          "Copy & paste" = "pasted",
                                                          "CSV/XLS Upload" = "fileUpload",
                                                          "Google sheets" = "googleSheets"),
                                           selected = "sampleData")),
                 panel(title = "Dataset",
                       width = 400,
                       body = uiOutput("data_preview")),
                 panel(title = "Options",
                       width = 400,
                       body = div(uiOutput("controls0"),
                                  uiOutput("controls1"),
                                  uiOutput("controls2"),
                                  uiOutput("controls3"))),
                 panel(title = "Viz",
                       can_collapse = FALSE,
                       body = div(reactableOutput("result"),
                                  shinypanels::modal(id = "test",
                                                     title = "Download plot",
                                                     dsmodules::downloadHtmlwidgetUI("download_data_button", "Descarga")),
                                  shinypanels::modalButton(label = "Download table", modal_id = "test"))))


config_path <- "parmesan"
# Reactive part
input_ids <- parmesan_input_ids(section = NULL, config_path = "parmesan")
input_ids_values <- lapply(input_ids, function(i) {NA})
names(input_ids_values) <- input_ids


server <- function(input, output, session) {
  
  react_env <- new.env()
  
  inputData <- callModule(tableInput, 
                          "initial_data",
                          sampleFile = list("Emission per capita C02" = "data/sampleData/emisiones_c02.csv", 
                                            "South America population" = "data/sampleData/poblacion.csv"))
  
  # renderizando los parámetros
  output$controls0 <- renderUI({
    parmesan_render_ui(sections = "Styles", config_path = config_path, input = input, env = react_env)
  })
  
  output$controls1 <- renderUI({
    parmesan_render_ui(sections = "Size", config_path = config_path, input = input, env = react_env)
  })
  
  output$controls2 <- renderUI({
    parmesan_render_ui(sections = "Sort, group, filter", config_path = config_path, input = input, env = react_env)
  })
  
  output$controls3 <- renderUI({
    parmesan_render_ui(sections = "Pagination", config_path = config_path, input = input, env = react_env)
  })
  
  output$data_preview <- renderUI({
    req(inputData())
    suppressWarnings(hotr("hotr_input", data = inputData(), order = NULL, options = list(height = 470), enableCTypes = FALSE))
  })
  
  dt <- reactive({
    req(input$hotr_input)
    hotr_table(input$hotr_input)
  }, env = react_env)
  
  cols_nms <- reactive({
    req(dt())
    names(dt())
  }, env = react_env)
  
  rctbl <- reactive({
    req(dt())
    # fw <- ifelse(input$full_width_tbl == "full_width", TRUE, FALSE)
    # wd <- ifelse(input$full_width_tbl == "full_width", "auto", input$width_tbl)
    
    sl <- NULL
    if (sum(input$selection) > 0) 
      sl <- "multiple"
      
    reactable(dt(),
              
              height = input$height,
              # fullWidth = fw,
              # width = wd,
              wrap = input$wrap,
              resizable = input$resizable,
              
              outlined = input$outlined,
              bordered = input$bordered,
              borderless = !input$borderless,
              striped = input$striped,
              compact = input$compact,
              highlight = input$highlight,
              
              # pagination = input$pagination,
              showPagination = input$pagination,
              # showPageInfo = input$show_page_info,
              # showPageSizeOptions = input$page_size_control,
              # paginationType = input$page_type,
              # defaultPageSize = input$page_size,
              
              sortable = input$sortable,
              # showSortIcon = input$show_sort_icon,
              # showSortable =  input$show_sortable,
              groupBy = input$group_by,
              filterable = input$filterable,
              searchable = input$searchable,
              selection = sl,
              
              pageSizeOptions = seq(5, nrow(inputData()), 5))
  })
  
  # renderizando reactable
  output$result <- renderReactable({
    rctbl()
  })

  # descargas
  callModule(downloadHtmlwidget, "download_data_button", widget = rctbl(), name = "table")
    
}


shinyApp(ui, server)