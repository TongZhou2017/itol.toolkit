#' Complex HTML text
#'
#' Interactively combine columns by HTML styles and record workflow as
#' reproducible code.
#'
#' When you're done, the code performing this operation will be emitted
#' at the cursor position.
#'
#' @importFrom colourpicker colourInput
#' @import dplyr
#' @import miniUI
#' @import shiny
#' @import rstudioapi
#' @importFrom stringr str_replace_all
#' @export
complex_html_text <- function() {
  stableColumnLayout <- function(...) {
    dots <- list(...)
    n <- length(dots)
    width <- 12 / n
    class <- sprintf("col-xs-%s col-md-%s", width, width)
    fluidRow(
      lapply(dots, function(el) {
        div(class = class, el)
      })
    )
  }


  mem_names     <- ls(envir = .GlobalEnv)
  napply <- function(names, fn){
    sapply(names, function(x) fn(get(x)))
  }
  obj.mode  <- napply(mem_names, mode)
  obj.class <- napply(mem_names, function(x) as.character(class(x))[1])
  obj.type  <- ifelse(is.na(obj.class), obj.mode, obj.class)

  dataset_names <- names(obj.type[obj.type == "data.frame"])

  # Generate UI for the gadget.
  ui <- miniPage(
    gadgetTitleBar("Complex HTML text"),

    miniContentPanel(
      br(),
      selectInput("dataset","Select dataset",dataset_names),
      br(),
      uiOutput('uiOutpt'),
      br(),
      actionButton("add", "Add column"),
      br(),
      h4("Render preview"),
      uiOutput("preview")
    )
  )


  # Server code for the gadget.
  server <- function(input, output, session) {
    features <- reactiveValues(renderd=c(1))
    data <- reactive({
      get(input$dataset)
    })
    dataset <- reactive({
      req(input$dataset)
      data()
    })
    observeEvent(dataset(), {
      choices <- names(dataset())
      updateSelectInput(inputId = "column", choices = choices)
    })


    reactiveData <- reactive({

      # Collect inputs.
      dataString <- input$dataset
      nn <- max(features$renderd)
      cols <- list()
      for (i in 1:nn) {
        try(cols[[i]] <- input[[paste0("column",i)]],silent = T)
      }
      styles <- list()
      for (i in 1:nn) {
        try(styles[[i]] <- input[[paste0("style",i)]],silent = T)
      }
      subsups <- list()
      for (i in 1:nn) {
        try(subsups[[i]] <- input[[paste0("subsup",i)]],silent = T)
      }
      sizes <- list()
      for (i in 1:nn) {
        try(sizes[[i]] <- input[[paste0("size",i)]],silent = T)
      }
      colors <- list()
      for (i in 1:nn) {
        try(colors[[i]] <- input[[paste0("color",i)]],silent = T)
      }
      fonts <- list()
      for (i in 1:nn) {
        try(fonts[[i]] <- input[[paste0("font",i)]],silent = T)
      }
      finds <- list()
      for (i in 1:nn) {
        try(finds[[i]] <- input[[paste0("find",i)]],silent = T)
      }
      replaces <- list()
      for (i in 1:nn) {
        try(replaces[[i]] <- input[[paste0("replace",i)]],silent = T)
      }
      seps <- list()
      for (i in 1:nn) {
        try(seps[[i]] <- input[[paste0("sep",i)]],silent = T)
      }

      data <- get(dataString, envir = .GlobalEnv)

      example_string <- ""
      html_fonts <- list()
      itol_command_head <- paste0(input$dataset," <- ",input$dataset," %>% mutate(new_label = paste0(")
      itol_command_body <- ""
      for (i in 1:nn) {
        content <- data[,cols[[i]]][1]
        if("bold" %in% styles[[i]]){
          if("italic" %in% styles[[i]]){
            html_style <- c("<i><b>","</b></i>")
          }else{
            html_style <- c("<b>","</b>")
          }
        }else{
          if("italic" %in% styles[[i]]){
            html_style <- c("<i>","</i>")
          }else{
            html_style <- c("<n>","</n>")
          }
        }
        html_subsup <- c("","")
        if(subsups[[i]] == "sub"){
          html_subsup <- c("<sub>","</sub>")
        }
        if(subsups[[i]] == "sup"){
          html_subsup <- c("<sup>","</sup>")
        }
        html_size <- c("","")
        if(sizes[[i]] == 1){
          html_size <- c("","")
        }else{
          html_size <- c(paste0("<font size='",sizes[[i]],"'>"),"</font>")
        }
        html_color <- c("","")
        if(colors[[i]] == "#000000"){
          html_color <- c("","")
        }else{
          html_color <- c(paste0("<font color='",colors[[i]],"'>"),"</font>")
        }
        html_font <- c("","")
        if(fonts[[i]] == "Arial"){ # diff in iTOL
          html_font <- c("","")
        }else{
          html_font <- c(paste0("<font face='",fonts[[i]],"'>"),"</font>")
        }
        if(paste0(finds[[i]],replaces[[i]]) != ""){
          content <- stringr::str_replace_all(content,finds[[i]],replaces[[i]])
        }

        try(example_string <- paste0(example_string,seps[[i]],html_font[1],html_color[1],html_size[1],html_subsup[1],html_style[1],content,html_style[2],html_subsup[2],html_size[2],html_color[2],html_font[2]),silent = T)

        if(seps[[1]] == " "){
          seps[[1]] = ""
        }
        itol_command_col <- cols[[i]]
        if(paste0(finds[[i]],replaces[[i]]) != ""){
          itol_command_col <- paste0("stringr::str_replace_all(",cols[[i]],',"',finds[[i]],'", "',replaces[[i]],'")')
        }
        # head
        if(html_subsup[1] %in% c("","non")){
          if("bold" %in% styles[[i]]){
            if("italic" %in% styles[[i]]){
              html_head <- "<bi"
              html_tail <- "</bi>"
            }else{
              html_head <- "<b"
              html_tail <- "</b>"
            }
          }else{
            if("italic" %in% styles[[i]]){
              html_head <- "<i"
              html_tail <- "</i>"
            }else{
              html_head <- "<n"
              html_tail <- "</n>"
            }
          }
        }else{
          html_head <- paste0("<",subsups[[i]])
          html_tail <- paste0("</",subsups[[i]],">")
        }
        # font
        if(paste0(html_font[1],html_size[1],html_color[1]) == ""){
          html_fonts[[i]] <- ">"
        }else{
          html_fonts[[i]] <- paste0(html_font[1],html_size[1],html_color[1])
          html_fonts[[i]] <- stringr::str_remove_all(html_fonts[[i]], '<font')
          html_fonts[[i]] <- stringr::str_replace(html_fonts[[i]], "face","font")
          html_fonts[[i]] <- stringr::str_remove_all(html_fonts[[i]], '>')
          html_fonts[[i]] <- paste0(html_fonts[[i]],">")
        }
        itol_command_body <- paste0(itol_command_body,paste0('"',html_head,'", ','"',html_fonts[[i]],'", ','"',seps[[i]],'", ',itol_command_col,', "',html_tail,'"'),sep = ", ")

      }
      itol_command_body <- stringr::str_remove(itol_command_body, ", $")
      itol_command_tail <- paste0(")) %>% select(",names(dataset())[1],",new_label)")
      itol_command <- paste0(itol_command_head,itol_command_body,itol_command_tail)


      list(example_string,itol_command)
    })

    output$preview <- renderUI({
      data <- reactiveData()[[1]]
      HTML(data)
    })

    observeEvent(input$add,{
      features$renderd <- c(features$renderd, max(features$renderd)+1)
    })

    observe({
      output$uiOutpt <- renderUI({
        nn <- max(features$renderd)
        cols <- list()
        for (i in 1:nn) {
          try(cols[[i]] <- input[[paste0("column",i)]],silent = T)
        }
        try(cols <- append(cols,""),silent = T)

        styles <- list()
        for (i in 1:nn) {
          if(is.null(input[[paste0("style",i)]])){
            try(styles[[i]] <- "non",silent = T)
          }else{
            try(styles[[i]] <- input[[paste0("style",i)]],silent = T)
          }
        }
        try(styles <- append(styles,""),silent = T)

        subsups <- list()
        for (i in 1:nn) {
          if(is.null(input[[paste0("subsup",i)]])){
            try(subsups[[i]] <- "non",silent = T)
          }else{
            try(subsups[[i]] <- input[[paste0("subsup",i)]],silent = T)
          }
        }
        try(subsups <- append(subsups,""),silent = T)

        sizes <- list()
        for (i in 1:nn) {
          if(is.null(input[[paste0("size",i)]])){
            try(sizes[[i]] <- 1,silent = T)
          }else{
            try(sizes[[i]] <- input[[paste0("size",i)]],silent = T)
          }
        }
        try(sizes <- append(sizes,""),silent = T)

        colors <- list()
        for (i in 1:nn) {
          if(is.null(input[[paste0("color",i)]])){
            try(colors[[i]] <- "#000000",silent = T)
          }else{
            try(colors[[i]] <- input[[paste0("color",i)]],silent = T)
          }
        }
        try(colors <- append(colors,""),silent = T)

        fonts <- list()
        for (i in 1:nn) {
          if(is.null(input[[paste0("font",i)]])){
            try(fonts[[i]] <- "Arial",silent = T)
          }else{
            try(fonts[[i]] <- input[[paste0("font",i)]],silent = T)
          }
        }
        try(fonts <- append(fonts,""),silent = T)

        finds <- list()
        for (i in 1:nn) {
          if(is.null(input[[paste0("find",i)]])){
            try(finds[[i]] <- "",silent = T)
          }else{
            try(finds[[i]] <- input[[paste0("find",i)]],silent = T)
          }
        }
        try(finds <- append(finds,""),silent = T)

        replaces <- list()
        for (i in 1:nn) {
          if(is.null(input[[paste0("replace",i)]])){
            try(replaces[[i]] <- "",silent = T)
          }else{
            try(replaces[[i]] <- input[[paste0("replace",i)]],silent = T)
          }
        }
        try(replaces <- append(replaces,""),silent = T)

        seps <- list()
        for (i in 1:nn) {
          if(is.null(input[[paste0("sep",i)]])){
            try(seps[[i]] <- " ",silent = T)
          }else{
            try(seps[[i]] <- input[[paste0("sep",i)]],silent = T)
          }
        }
        try(seps <- append(seps,""),silent = T)

        rows <- lapply(features$renderd,function(i){
          list(
            stableColumnLayout(
              selectInput(paste0('column',i),
                          label = paste0("Select column ",i),
                          choices =names(dataset()),
                          selected = cols[[i]],
                          width = 150),
              checkboxGroupInput(paste0("style",i),
                                 "Styles",
                                 choices = c("non","bold","italic"),
                                 selected = styles[[i]],
                                 inline = T,
                                 width = 200),
              radioButtons(paste0("subsup",i),
                           "Sub/Sup",
                           choices = c("non","sub","sup"),
                           selected = subsups[[i]],
                           inline = T,
                           width = 250),
              numericInput(paste0("size",i),
                           "Font size",
                           value = sizes[[i]],
                           width = 100)
            ),
            stableColumnLayout(
              colourpicker::colourInput(paste0("color",i),
                                        "Font color",
                                        value = colors[[i]]),
              selectInput(paste0("font",i),
                          "Font type",
                          choices = c("Arial",
                                      "Verdana",
                                      "Courier",
                                      "Courier New",
                                      "Times New Roman",
                                      "Georgia",
                                      "Impact",
                                      "Monotype Corsiva"),
                          selected = fonts[[i]],
                          width = 150),
              textInput(paste0("find",i),
                        "Find",
                        value = finds[[i]],
                        width = 150),
              textInput(paste0("replace",i),
                        "Replace",
                        value = replaces[[i]],
                        width = 150)
            ),
            textInput(paste0("sep",i),
                      "Prefix",
                      value = seps[[i]],
                      width = 100)
          )
        })
        try(do.call(shiny::tagList,rows),silent = T)

      })
    })

    observeEvent(input$done, {
      command <- reactiveData()[[2]]
      rstudioapi::insertText(command)
      stopApp()
    })
  }

  # Use a modal dialog as a viewr.
  viewer <- dialogViewer("complex_html_text", width = 800, height = 1200)
  runGadget(ui, server, viewer = viewer)

}
