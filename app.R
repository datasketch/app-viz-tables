library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(dspins)
library(hotr)
library(tidyverse)
library(reactable)
library(shinycustomloader)
library(homodatum)

# unidades (width, height pixeles)

ui <- panelsPage(useShi18ny(),
                 showDebug(),
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
                       title_plugin = uiOutput("download"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  withLoader(uiOutput("result"), type = "image", loader = "loading_gris.gif"))))



server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt_BR"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})  
  
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data",
                 label = "",
                 choices = choices,
                 # selected is important for inputs not be re-initialized
                 selected = ifelse(is.null(input$`initial_data-tableInput`), "sampleData", input$`initial_data-tableInput`))
  })
  
  labels <- reactive({
    sm_f <- i_(c("sample_ch_0", "sample_ch_1"), lang())
    names(sm_f) <- i_(c("sample_ch_nm_0", "sample_ch_nm_1"), lang())
    
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFile = sm_f,
         
         pasteLabel = i_("paste", lang()),
         pasteValue = "", 
         pastePlaceholder = i_("paste_pl", lang()), 
         pasteRows = 5, 
         
         uploadLabel = i_("upload_lb", lang()), 
         uploadButtonLabel = i_("upload_bt_lb", lang()),
         uploadPlaceholder = i_("upload_pl", lang()),
         
         googleSheetLabel = i_("google_sh_lb", lang()), 
         googleSheetValue = "",
         googleSheetPlaceholder = i_("google_sh_pl", lang()),
         googleSheetPageLabel = i_("google_sh_pg_lb", lang()))
  })
  
  inputData <- eventReactive(list(labels(), input$`initial_data-tableInput`), {
    do.call(tableInputServer, c("initial_data", labels()))
  })
  
  output$data_preview <- renderUI({
    req(inputData())
    suppressWarnings(hotr("hotr_input", data = inputData(), order = NULL, options = list(height = "86vh"), enableCTypes = FALSE))
  })
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices", "text"))})
  output_parmesan("controls", 
                  parmesan = parmesan_lang,
                  input = input,
                  output = output,
                  env = environment())
  
  observeEvent(lang(), {
    ch0 <- as.character(parmesan$pagination$inputs[[5]]$input_params$choices)
    names(ch0) <- i_(ch0, lang())
    ch1 <- as.character(parmesan$size$inputs[[3]]$input_params$choices)
    names(ch1) <- i_(ch1, lang())
    print("RRET")
    updateRadioButtons(session, "page_type", choices = ch0, selected = input$page_type)
    updateRadioButtons(session, "full_width", choices = ch1, selected = input$full_width)
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
    pt <- "numbers"
    if (!is.null(input$page_type)) {
      pt <- input$page_type 
    }
    if (sum(input$hyperlink) > 0) {
      hlink <- function(value, index, name) {
        if (!is.null(dt()[index, name])) {
          if (grepl("^www\\.|^http(s|)://", dt()[index, name])) {
            shiny::tags$a(href = value, target = "_blank", value)
          } else {
            value
          }
        }
      }
    } else {
      hlink <- NULL
    }
    st <- paste0("color: ", input$color, "; font-family: ", input$font_family, "; font-size: ", input$font_size, "px;")# font-weight: bold;")
    safe_reactable <- purrr::safely(reactable)
    reactable::reactable(dt(),
                   
                   height = input$height,
                   fullWidth = ifelse(input$full_width == "full_width", TRUE, FALSE),
                   width = ifelse(input$full_width == "full_width", "auto", input$width_l),
                   wrap = input$wrap,
                   resizable = input$resizable,
                   
                   outlined = input$outlined,
                   bordered = ifelse(!input$outlined, FALSE, input$bordered),
                   borderless = !input$borderless,
                   striped = input$striped,
                   compact = input$compact,
                   highlight = input$highlight,
                   
                   pagination = input  $show_pagination,
                   showPagination = input$show_pagination,
                   showPageInfo = ifelse(input$show_pagination, input$show_page_info, FALSE),
                   showPageSizeOptions = ifelse(input$show_pagination, input$page_size_control, FALSE),
                   paginationType = pt,
                   defaultPageSize = input$page_size,
                   
                   # showSortIcon = ifelse(is.null(input$show_sort_icon), FALSE, input$show_sort_icon),
                   # showSortable =  ifelse(is.null(input$show_sort_icon), FALSE, input$show_sort_icon),
                   sortable = input$sortable,
                   showSortIcon = TRUE,
                   showSortable =  TRUE,
                   groupBy = input$group_by,
                   filterable = input$filterable,
                   searchable = input$searchable,
                   selection = sl,
                   
                   pageSizeOptions = seq(5, nrow(dt()), 5),
                   
                   defaultColDef = colDef(cell = hlink,
                                          headerStyle = list(background = input$color_header,
                                                             color = input$font_color_header)),
                   style = st)
  })
  
  output$result <- renderUI({
    res <- rctbl()
    # if (is.null(res$result)) {
    #   infomessage(p(res$error$message))
    # } else {
      reactableOutput("result_table", height = "61vh")
    # }
  })
  
  output$result_table <- renderReactable({
    req(rctbl())
    rctbl()
  })
  
  output$download <- renderUI({
    
    downloadDsUI("download_data_button",
                 display = "dropdown",
                 formats = c("html"),
                 dropdownWidth = 170,
                 modalFormatChoices = c("HTML" = "html", "PNG" = "png"),
                 text = i_("download", lang()), 
                 dropdownLabel = i_("download", lang()), 
                 getLinkLabel = i_("get_link", lang()), 
                 modalTitle = i_("get_link", lang()), 
                 modalButtonLabel = i_("gl_save", lang()), 
                 modalLinkLabel = i_("gl_url", lang()), 
                 modalIframeLabel = i_("gl_iframe", lang()),
                 nameLabel = i_("gl_name", lang()),
                 descriptionLabel = i_("gl_description", lang()),
                 sourceLabel = i_("gl_source", lang()),
                 sourceTitleLabel = i_("gl_source_name", lang()),
                 sourcePathLabel = i_("gl_source_path", lang()),
                 licenseLabel = i_("gl_license", lang()),
                 tagsLabel = i_("gl_tags", lang()),
                 tagsPlaceholderLabel = i_("gl_type_tags", lang()),
                 categoryLabel = i_("gl_category", lang()),
                 categoryChoicesLabels = i_("gl_no_category", lang())
    )
  })
  
  par <- list(user_name = "test", org_name = NULL)
  url_par <- reactive({
    url_params(par, session)
  })


  observe({
    req(rctbl())
    # browser()
    user_name <- url_par()$inputs$user_name
    org_name <- url_par()$inputs$org_name
    if (is.null(user_name) & is.null(user_name)) return()
    downloadDsServer(id = "download_data_button",
                     element = reactive(rctbl()),
                     formats = c("html"),
                     errorMessage = i_("gl_error", lang()),
                     type = "dsviz",
                     user_name = user_name,
                     org_name = org_name)

  })
  
}


shinyApp(ui, server)