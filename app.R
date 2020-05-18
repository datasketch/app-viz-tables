library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
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

ui <- panelsPage(useShi18ny(),
                 panel(title = ui_("upload_data"),
                       width = 200,
                       body = uiOutput("table_input")),
                 panel(title = ui_("dataset"),
                       width = 300,
                       body = uiOutput("data_preview")),
                 panel(title = ui_("options"),
                       width = 250,
                       color = "chardonnay",
                       body = uiOutput("controls")),
                 panel(title = ui_("viz"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  reactableOutput("result"),
                                  shinypanels::modal(id = "test",
                                                     title = ui_("download_table"),
                                                     # dsmodules::downloadHtmlwidgetUI("download_data_button", "Download HTML"))),
                                                     uiOutput("modal"))),
                       footer = shinypanels::modalButton(label = "Download table", modal_id = "test")))


server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = TRUE)
  observeEvent(lang(), {
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })  
  
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data",
                 choices = choices,
                 # selected is important for inputs not be re-initialized
                 selected = ifelse(is.null(input$`initial_data-tableInput`), "sampleData", input$`initial_data-tableInput`))
  })
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices"))})
  output_parmesan("controls", 
                  parmesan = parmesan_lang,
                  input = input,
                  output = output)
  # dsmodules::downloadHtmlwidgetUI("download_data_button", "Download HTML"))),
  
  output$modal <- renderUI({
    dw <- i_("download", lang())#Download HTML
    downloadHtmlwidgetUI("download_data_button", paste(dw, "HTML"))
  })
  
  labels <- reactive({
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFile = list("Iris" = "data/sampleData/iris.csv",
                           "Emission per capita C02" = "data/sampleData/emisiones_c02.csv"),
         pasteLabel = i_("paste", lang()), pasteValue = "", pastePlaceholder = i_("paste_pl", lang()), pasteRows = 5, 
         uploadLabel = i_("upload_lb", lang()), uploadButtonLabel = i_("upload_bt_lb", lang()), uploadPlaceholder = i_("upload_pl", lang()),
         googleSheetLabel = i_("google_sh_lb", lang()), googleSheetValue = "", googleSheetPlaceholder = i_("google_sh_pl", lang()),
         googleSheetPageLabel = i_("google_sh_pg_lb", lang()))
  })
  
  
  inputData <- eventReactive(labels(), {
    do.call(callModule, c(tableInput,
                          "initial_data",
                          labels()))
  })
  
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

              pageSizeOptions = seq(5, nrow(dt()), 5),
              
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