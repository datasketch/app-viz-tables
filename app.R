library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(dsmodules)
library(htmlwidgets)
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
                       body = uiOutput("controls")),
                 panel(title = "Viz",
                       can_collapse = FALSE,
                       body = div(reactableOutput("result"),
                                  shinypanels::modal(id = "test",
                                                     title = "Download plot",
                                                     dsmodules::downloadHtmlwidgetUI("download_data_button", "Descarga"))),
                       footer = shinypanels::modalButton(label = "Download table", modal_id = "test")))


server <- function(input, output, session) {
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_env <- new.env()
  parmesan_input <- parmesan_watch(input, parmesan)
  output_parmesan("#controls", 
                  parmesan = parmesan,
                  input = input,
                  output = output,
                  env = parmesan_env)
  
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
    fw <- ifelse(input$full_width == "full_width", TRUE, FALSE)
    wd <- ifelse(input$full_width == "full_width", "auto", input$width)
    # sw_i <- ifelse(is.null(input$show_sort_icon), FALSE, input$show_sort_icon)
    # sw_s <- ifelse(is.null(input$show_sortable), FALSE, input$show_sortable)
    
    sl <- NULL
    if (sum(input$selection) > 0) 
      sl <- "multiple"
    
      
    reactable(dt(),
              
              height = input$height,
              fullWidth = fw,
              width = wd,
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
              showPageInfo = input$show_page_info,
              showPageSizeOptions = input$page_size_control,
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

              pageSizeOptions = seq(5, nrow(inputData()), 5)
              )
  })
  
  # renderizando reactable
  output$result <- renderReactable({
    req(rctbl())
    rctbl()
  })

  # descargas
  callModule(downloadHtmlwidget, "download_data_button", widget = rctbl(), name = "table")
    
}


shinyApp(ui, server)