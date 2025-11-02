#' Complex HTML text
#'
#' Interactively combine columns by HTML styles and record workflow as
#' reproducible code.
#'
#' When you're done, the code performing this operation will be emitted
#' at the cursor position.
#'
#' @importFrom colourpicker colourInput
#' @importFrom magrittr %>%
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

#' Binary Data Conversion
#'
#' Interactively convert data to binary format (-1, 0, 1) using range sliders.
#' Provides three pairs of sliders for negative, zero, and positive ranges
#' with automatic constraint handling to prevent overlaps.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr all_of select
#' @import miniUI
#' @import shiny
#' @import rstudioapi
#' @import ggplot2
#' @importFrom data.table data.table
#' @export
binary_data_conversion <- function() {

  # Get available data frames and data.tables from global environment
  mem_names <- ls(envir = .GlobalEnv)
  napply <- function(names, fn){
    sapply(names, function(x) fn(get(x)))
  }
  obj.mode  <- napply(mem_names, mode)
  obj.class <- napply(mem_names, function(x) as.character(class(x))[1])
  obj.type  <- ifelse(is.na(obj.class), obj.mode, obj.class)

  # Support both data.frame and data.table
  dataset_names <- names(obj.type[obj.type %in% c("data.frame", "data.table")])

  # Generate UI for the gadget
  ui <- miniPage(
    gadgetTitleBar("Binary Data Conversion"),

    miniContentPanel(
      # Add some padding and better spacing
      style = "padding: 20px;",
      br(),

      # Dataset selection and preview in horizontal layout
      fluidRow(
        column(2,
          h4("Select dataset", style = "font-size: 18px; font-weight: bold; margin-bottom: 10px;"),
          selectInput("dataset", "", dataset_names)
        ),
        column(10,
          h4("Data Preview", style = "font-size: 18px; font-weight: bold; margin-bottom: 10px;"),
          div(style = "max-height: 200px; overflow-y: auto;",
              tableOutput("dataPreview")
          )
        )
      ),

      # Status message
      div(id = "statusMessage", style = "margin: 5px 0; padding: 5px; background-color: #f8f9fa; border-radius: 3px; font-size: 12px; color: #666;"),
      br(),

      # Data distribution visualization
      h4("Data Distribution", style = "font-size: 18px; font-weight: bold; margin-bottom: 10px;"),
      div(style = "margin-bottom: 15px;",
          plotOutput("dataDistribution", height = "300px")
      ),
      br(),

      # Range controls
      h4("Conversion Range Settings", style = "font-size: 18px; font-weight: bold; margin-bottom: 10px;"),

      # Data range info
      div(id = "dataRangeInfo", style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;"),

      # Range controls in horizontal layout
      fluidRow(
        # Negative range (left)
        column(4,
          wellPanel(
            h5("Omitted (-> -1)", style = "color: #d32f2f; font-weight: bold; margin-bottom: 10px; font-size: 14px;"),
            sliderInput("neg_range", "Range",
                       min = 0, max = 1, value = c(0, 0), step = 0.01,
                       width = "100%", ticks = TRUE),
            fluidRow(
              column(6, numericInput("neg_min_input", "Min:", value = 0, step = 0.01, width = "100%")),
              column(6, numericInput("neg_max_input", "Max:", value = 0, step = 0.01, width = "100%"))
            )
          )
        ),
        # Zero range (middle)
        column(4,
          wellPanel(
            h5("Outline (-> 0)", style = "color: #1976d2; font-weight: bold; margin-bottom: 10px; font-size: 14px;"),
            sliderInput("zero_range", "Range",
                       min = 0, max = 1, value = c(0, 1), step = 0.01,
                       width = "100%", ticks = TRUE),
            fluidRow(
              column(6, numericInput("zero_min_input", "Min:", value = 0, step = 0.01, width = "100%")),
              column(6, numericInput("zero_max_input", "Max:", value = 1, step = 0.01, width = "100%"))
            ),
            fluidRow(
              column(6, checkboxInput("zero_lower_inclusive", "Lower inclusive", value = FALSE)),
              column(6, checkboxInput("zero_upper_inclusive", "Upper inclusive", value = FALSE))
            )
          )
        ),
        # Positive range (right)
        column(4,
          wellPanel(
            h5("Filled (-> 1)", style = "color: #388e3c; font-weight: bold; margin-bottom: 10px; font-size: 14px;"),
            sliderInput("pos_range", "Range",
                       min = 0, max = 1, value = c(1, 1), step = 0.01,
                       width = "100%", ticks = TRUE),
            fluidRow(
              column(6, numericInput("pos_min_input", "Min:", value = 1, step = 0.01, width = "100%")),
              column(6, numericInput("pos_max_input", "Max:", value = 1, step = 0.01, width = "100%"))
            )
          )
        )
      ),


      # Preview results and summary in horizontal layout
      fluidRow(
        column(4,
          h4("Conversion Summary", style = "font-size: 18px; font-weight: bold; margin-bottom: 10px;"),
          div(style = "max-height: 250px; overflow-y: auto;",
              tableOutput("conversionSummary")
          )
        ),
        column(8,
          h4("Conversion Preview", style = "font-size: 18px; font-weight: bold; margin-bottom: 10px;"),
          div(style = "max-height: 250px; overflow-y: auto;",
              tableOutput("conversionPreview")
          )
        )
      ),

      # Range validation
      div(id = "rangeValidation", style = "margin-top: 10px;")
    )
  )

  # Server code for the gadget
  server <- function(input, output, session) {

    # Reactive data
    data <- reactive({
      req(input$dataset)
      get(input$dataset, envir = .GlobalEnv)
    })

    # Get all columns except the first one (ID column)
    numericColumns <- reactive({
      req(data())

      # Get all columns except the first one
      all_cols <- names(data())
      if (length(all_cols) > 1) {
        return(all_cols[-1])  # Exclude first column (ID)
      } else {
        return(character(0))
      }
    })

    # Calculate data range for sliders
    dataRange <- reactive({
      req(data())

      # Get all columns except first (ID)
      all_cols <- names(data())
      if (length(all_cols) <= 1) return(c(0, 1))

      # Get data columns (excluding first column)
      data_cols <- all_cols[-1]

      # Filter for numeric columns only
      numeric_cols_filtered <- data_cols[sapply(data_cols, function(col) {
        is.numeric(data()[[col]])
      })]

      if (length(numeric_cols_filtered) == 0) return(c(0, 1))

      # Get numeric data
      tryCatch({
        # Handle both data.frame and data.table
        if (inherits(data(), "data.table")) {
          numeric_data <- data()[, ..numeric_cols_filtered]
        } else {
          numeric_data <- data()[, numeric_cols_filtered, drop = FALSE]
        }

        if (ncol(numeric_data) == 0) return(c(0, 1))

        if (nrow(numeric_data) == 0) return(c(0, 1))

        # Calculate min and max across all numeric columns
        all_values <- unlist(numeric_data)
        all_values <- all_values[!is.na(all_values)]

        if (length(all_values) == 0) return(c(0, 1))

        min_val <- min(all_values)
        max_val <- max(all_values)

        # Use exact data range without padding
        return(c(min_val, max_val))
      }, error = function(e) {
        return(c(0, 1))
      })
    })


    # Update slider ranges when data or columns change
    observeEvent(dataRange(), {
      range_vals <- dataRange()
      min_range <- range_vals[1]
      max_range <- range_vals[2]

      # Calculate step size and adjust min/max precision based on data
      # First, determine the precision of the original data
      all_cols <- names(data())
      if (length(all_cols) > 1) {
        data_cols <- all_cols[-1]
        numeric_cols <- data_cols[sapply(data_cols, function(col) {
          is.numeric(data()[[col]])
        })]

        if (length(numeric_cols) > 0) {
          # Get all numeric values to determine precision
          # Handle both data.frame and data.table
          if (inherits(data(), "data.table")) {
            all_values <- unlist(data()[, ..numeric_cols])
          } else {
            all_values <- unlist(data()[, numeric_cols, drop = FALSE])
          }
          all_values <- all_values[!is.na(all_values)]

          if (length(all_values) > 0) {
            # Determine decimal places by looking at the data
            decimal_places <- 0
            for (val in all_values) {
              if (val != round(val)) {
                # Count decimal places
                val_str <- as.character(val)
                if (grepl("\\.", val_str)) {
                  dec_part <- strsplit(val_str, "\\.")[[1]][2]
                  decimal_places <- max(decimal_places, nchar(dec_part))
                }
              }
            }

            # Set step size based on decimal places
            # Use a smaller step size for better precision
            if (decimal_places == 0) {
              step_size <- 0.01
            } else if (decimal_places == 1) {
              step_size <- 0.01
            } else if (decimal_places == 2) {
              step_size <- 0.001
            } else if (decimal_places == 3) {
              step_size <- 0.0001
            } else {
              step_size <- 0.00001
            }

            # Don't round or buffer - use exact data range
            # This ensures the slider values match the actual data values
          } else {
            step_size <- 0.01
          }
        } else {
          step_size <- 0.01
        }
      } else {
        step_size <- 0.01
      }

      # Fallback step size calculation if precision detection failed
      if (is.null(step_size) || is.na(step_size)) {
        range_diff <- max_range - min_range
        if (range_diff <= 0) {
          step_size <- 0.01
        } else if (range_diff <= 1) {
          step_size <- 0.001
        } else if (range_diff <= 10) {
          step_size <- 0.01
        } else if (range_diff <= 100) {
          step_size <- 0.1
        } else {
          step_size <- 1
        }
      }

      # Ensure step size is not too small
      if (step_size < 0.0001) step_size <- 0.0001

      # Update all sliders with new range
      updateSliderInput(session, "neg_range",
                       min = min_range, max = max_range, step = step_size,
                       value = c(min_range, min_range))
      updateSliderInput(session, "zero_range",
                       min = min_range, max = max_range, step = step_size,
                       value = c(min_range, max_range))
      updateSliderInput(session, "pos_range",
                       min = min_range, max = max_range, step = step_size,
                       value = c(max_range, max_range))

      # Update numeric inputs with new range
      updateNumericInput(session, "neg_min_input", value = min_range, min = min_range, max = max_range, step = step_size)
      updateNumericInput(session, "neg_max_input", value = min_range, min = min_range, max = max_range, step = step_size)
      updateNumericInput(session, "zero_min_input", value = min_range, min = min_range, max = max_range, step = step_size)
      updateNumericInput(session, "zero_max_input", value = max_range, min = min_range, max = max_range, step = step_size)
      updateNumericInput(session, "pos_min_input", value = max_range, min = min_range, max = max_range, step = step_size)
      updateNumericInput(session, "pos_max_input", value = max_range, min = min_range, max = max_range, step = step_size)
    })

    # Also update sliders when dataset changes (redundant but ensures immediate update)
    observeEvent(input$dataset, {
      # This will trigger dataRange() recalculation and slider updates
      # The main observeEvent(dataRange(), ...) above will handle the actual updates
    })


    # Display data range information
    output$dataRangeInfo <- renderUI({
      req(dataRange(), numericColumns())
      range_vals <- dataRange()
      min_range <- range_vals[1]
      max_range <- range_vals[2]
      numeric_cols <- numericColumns()

      if (length(numeric_cols) > 0) {
        HTML(paste0(
          "<strong>Data range:</strong> [",
          round(min_range, 3), ", ", round(max_range, 3), "] ",
          "<br><small>Slider ranges automatically adjusted based on data</small>"
        ))
      } else {
        HTML("<strong>Data has only one column, cannot convert</strong>")
      }
    })

    # Data preview
    output$dataPreview <- renderTable({
      req(data())
      head(data(), 10)
    })

    # Status message display
    output$statusMessage <- renderUI({
      req(data())
      numeric_cols <- numericColumns()
      if (length(numeric_cols) == 0) {
        HTML('<span style="color: #d32f2f;">(!) No numeric columns found for conversion</span>')
      } else {
        HTML('<span style="color: #388e3c;">(OK) Ready for conversion</span>')
      }
    })

    # Data distribution visualization
    output$dataDistribution <- renderPlot({
      req(data(), numericColumns())

      # Suppress warnings for this plot
      suppressWarnings({
        numeric_cols <- numericColumns()
        if (length(numeric_cols) == 0) return(NULL)

        # Get numeric data
        if (inherits(data(), "data.table")) {
          numeric_data <- data()[, ..numeric_cols]
        } else {
          numeric_data <- data()[, numeric_cols, drop = FALSE]
        }

        # Create plot data
        plot_data <- data.frame()
        for (col in numeric_cols) {
          col_data <- numeric_data[[col]]
          # Ensure data is numeric with proper handling
          col_data <- suppressWarnings(as.numeric(col_data))
          col_data_clean <- col_data[!is.na(col_data)]
          if (length(col_data_clean) > 0) {
            plot_data <- rbind(plot_data,
                              data.frame(
                                value = col_data_clean,
                                column = col,
                                stringsAsFactors = FALSE
                              ))
          }
        }

        if (nrow(plot_data) == 0) return(NULL)

        # Ensure value column is numeric
        plot_data$value <- suppressWarnings(as.numeric(plot_data$value))

      # Calculate density for all columns combined
      all_values <- plot_data$value

      # Create base plot
      p <- ggplot(plot_data, aes(x = value)) +
        geom_density(color = "#d32f2f", size = 1.2) +
        labs(
          title = "Data Distribution (All Numeric Columns)",
          x = "Value",
          y = "Density"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.grid.minor = element_blank()
        )

      # Add vertical lines for current range settings if available
      if (!is.null(input$neg_range) && !is.null(input$zero_range) && !is.null(input$pos_range)) {
        neg_max <- input$neg_range[2]
        zero_min <- input$zero_range[1]
        zero_max <- input$zero_range[2]
        pos_min <- input$pos_range[1]

        p <- p +
          # Add transparent background rectangles for each range
          annotate("rect", xmin = input$neg_range[1], xmax = neg_max, ymin = -Inf, ymax = Inf,
                   fill = "#d32f2f", alpha = 0.1) +
          annotate("rect", xmin = zero_min, xmax = zero_max, ymin = -Inf, ymax = Inf,
                   fill = "#1976d2", alpha = 0.1) +
          annotate("rect", xmin = pos_min, xmax = input$pos_range[2], ymin = -Inf, ymax = Inf,
                   fill = "#388e3c", alpha = 0.1) +
          # Add range labels
          annotate("text", x = (input$neg_range[1] + neg_max) / 2, y = Inf, label = "Omitted Range", color = "#d32f2f",
                   vjust = 1.5, size = 3.5) +
          annotate("text", x = (zero_min + zero_max) / 2, y = Inf, label = "Outline Range",
                   color = "#1976d2", vjust = 1.5, size = 3.5) +
          annotate("text", x = (pos_min + input$pos_range[2]) / 2, y = Inf, label = "Filled Range", color = "#388e3c",
                   vjust = 1.5, size = 3.5)
      }

      p
      }) # End suppressWarnings
    }, res = 100)

    # Sync numeric inputs to sliders
    observeEvent(input$neg_min_input, {
      updateSliderInput(session, "neg_range", value = c(input$neg_min_input, input$neg_range[2]))
    })

    observeEvent(input$neg_max_input, {
      updateSliderInput(session, "neg_range", value = c(input$neg_range[1], input$neg_max_input))
    })

    observeEvent(input$zero_min_input, {
      updateSliderInput(session, "zero_range", value = c(input$zero_min_input, input$zero_range[2]))
    })

    observeEvent(input$zero_max_input, {
      updateSliderInput(session, "zero_range", value = c(input$zero_range[1], input$zero_max_input))
    })

    observeEvent(input$pos_min_input, {
      updateSliderInput(session, "pos_range", value = c(input$pos_min_input, input$pos_range[2]))
    })

    observeEvent(input$pos_max_input, {
      updateSliderInput(session, "pos_range", value = c(input$pos_range[1], input$pos_max_input))
    })

    # Sync sliders to numeric inputs
    observeEvent(input$neg_range, {
      updateNumericInput(session, "neg_min_input", value = input$neg_range[1])
      updateNumericInput(session, "neg_max_input", value = input$neg_range[2])
    })

    observeEvent(input$zero_range, {
      updateNumericInput(session, "zero_min_input", value = input$zero_range[1])
      updateNumericInput(session, "zero_max_input", value = input$zero_range[2])
    })

    observeEvent(input$pos_range, {
      updateNumericInput(session, "pos_min_input", value = input$pos_range[1])
      updateNumericInput(session, "pos_max_input", value = input$pos_range[2])
    })

    # Range constraint logic with zero gap (using inclusive/exclusive boundaries)
    observeEvent(input$neg_range, {
      neg_min <- input$neg_range[1]
      neg_max <- input$neg_range[2]

      # Constrain zero range - zero gap adjustment
      if (neg_max >= input$zero_range[1]) {
        new_zero_min <- neg_max  # Zero gap - touching boundaries
        updateSliderInput(session, "zero_range",
                         value = c(new_zero_min, input$zero_range[2]))
      }
    })

    observeEvent(input$zero_range, {
      zero_min <- input$zero_range[1]
      zero_max <- input$zero_range[2]

      # Constrain negative range - zero gap adjustment
      if (zero_min <= input$neg_range[2]) {
        new_neg_max <- zero_min  # Zero gap - touching boundaries
        updateSliderInput(session, "neg_range",
                         value = c(input$neg_range[1], new_neg_max))
      }

      # Constrain positive range - zero gap adjustment
      if (zero_max >= input$pos_range[1]) {
        new_pos_min <- zero_max  # Zero gap - touching boundaries
        updateSliderInput(session, "pos_range",
                         value = c(new_pos_min, input$pos_range[2]))
      }
    })

    observeEvent(input$pos_range, {
      pos_min <- input$pos_range[1]
      pos_max <- input$pos_range[2]

      # Constrain zero range - zero gap adjustment
      if (pos_min <= input$zero_range[2]) {
        new_zero_max <- pos_min  # Zero gap - touching boundaries
        updateSliderInput(session, "zero_range",
                         value = c(input$zero_range[1], new_zero_max))
      }
    })

    # Range validation
    observe({
      req(input$neg_range, input$zero_range, input$pos_range)

      neg_max <- input$neg_range[2]
      zero_min <- input$zero_range[1]
      zero_max <- input$zero_range[2]
      pos_min <- input$pos_range[1]

      validation_msg <- ""
      validation_class <- "alert-success"

      if (neg_max >= zero_min) {
        validation_msg <- "Warning: Negative and zero ranges overlap!"
        validation_class <- "alert-danger"
      } else if (zero_max >= pos_min) {
        validation_msg <- "Warning: Zero and positive ranges overlap!"
        validation_class <- "alert-danger"
      } else {
        validation_msg <- "Range settings are valid (OK)"
        validation_class <- "alert-success"
      }

      output$rangeValidation <- renderUI({
        div(class = paste("alert", validation_class), validation_msg)
      })
    })

    # Conversion preview
    output$conversionPreview <- renderTable({
      req(data(), numericColumns())

      numeric_cols <- numericColumns()
      if (length(numeric_cols) == 0) return(NULL)

      # Create conversion parameters
      neg_range <- input$neg_range
      zero_range <- input$zero_range
      pos_range <- input$pos_range

      # Apply conversion
      tryCatch({
        # Handle both data.frame and data.table
        if (inherits(data(), "data.table")) {
          # For data.table, use data.table syntax
          selected_data <- data()[, c(names(data())[1], numeric_cols), with = FALSE]
        } else {
          # For data.frame, use dplyr syntax
          selected_data <- data() %>% select(all_of(c(names(data())[1], numeric_cols)))
        }

        converted_data <- convert_to_binary(
          selected_data,
          force_convert = FALSE,
          verbose = FALSE,
          negative_range = neg_range,
          zero_range = zero_range,
          positive_range = pos_range,
          lower_inclusive = input$zero_lower_inclusive,
          upper_inclusive = input$zero_upper_inclusive,
          auto_detect = FALSE
        )
        head(converted_data, 10)
      }, error = function(e) {
        data.frame(Error = paste("Conversion failed:", e$message))
      })
    })

    # Conversion summary
    output$conversionSummary <- renderTable({
      req(data(), numericColumns())

      numeric_cols <- numericColumns()
      if (length(numeric_cols) == 0) return(data.frame(Message = "No numeric columns found"))

      # Create conversion parameters
      neg_range <- input$neg_range
      zero_range <- input$zero_range
      pos_range <- input$pos_range

      # Apply conversion and capture summary
      tryCatch({
        # Handle both data.frame and data.table
        if (inherits(data(), "data.table")) {
          # For data.table, use data.table syntax
          selected_data <- data()[, c(names(data())[1], numeric_cols), with = FALSE]
        } else {
          # For data.frame, use dplyr syntax
          selected_data <- data() %>% select(all_of(c(names(data())[1], numeric_cols)))
        }

        # Capture conversion summary using sink to suppress console output
        # Create a temporary connection to capture messages
        capture_connection <- textConnection("capture_output", "w", local = TRUE)
        sink(capture_connection, type = "message", split = FALSE)

        tryCatch({
          result <- convert_to_binary(
            selected_data,
            force_convert = FALSE,
            verbose = TRUE,
            negative_range = neg_range,
            zero_range = zero_range,
            positive_range = pos_range,
            lower_inclusive = input$zero_lower_inclusive,
            upper_inclusive = input$zero_upper_inclusive,
            auto_detect = FALSE
          )
        }, finally = {
          sink(NULL, type = "message")
          close(capture_connection)
        })

        # Get the conversion summary table from the result attribute
        summary_table <- attr(result, "conversion_summary")
        if (!is.null(summary_table)) {
          # Convert data.table to data.frame for renderTable
          as.data.frame(summary_table)
        } else {
          data.frame(Message = "No conversion summary available")
        }
      }, error = function(e) {
        data.frame(Error = paste("Conversion failed:", e$message))
      })
    })

    # Generate conversion code
    observeEvent(input$done, {
      req(numericColumns())

      numeric_cols <- numericColumns()
      if (length(numeric_cols) == 0) return()

      # Build conversion parameters
      neg_range_str <- paste0("c(", input$neg_range[1], ", ", input$neg_range[2], ")")
      zero_range_str <- paste0("c(", input$zero_range[1], ", ", input$zero_range[2], ")")
      pos_range_str <- paste0("c(", input$pos_range[1], ", ", input$pos_range[2], ")")

      # Generate code based on data type
      if (inherits(data(), "data.table")) {
        # For data.table, use data.table syntax
        data_selection <- paste0(input$dataset, "[, c(\"", names(data())[1], "\", \"", paste(numeric_cols, collapse = '", "'), "\"), with = FALSE]")
      } else {
        # For data.frame, use dplyr syntax
        data_selection <- paste0(input$dataset, " %>% select(", names(data())[1], ", ", paste(numeric_cols, collapse = ", "), ")")
      }

      code <- paste0(
        "# Binary data conversion\n",
        "converted_data <- convert_to_binary(\n",
        "  data = ", data_selection, ",\n",
        "  force_convert = FALSE,\n",
        "  verbose = TRUE,\n",
        "  negative_range = ", neg_range_str, ",\n",
        "  zero_range = ", zero_range_str, ",\n",
        "  positive_range = ", pos_range_str, ",\n",
        "  lower_inclusive = ", input$zero_lower_inclusive, ",\n",
        "  upper_inclusive = ", input$zero_upper_inclusive, ",\n",
        "  auto_detect = FALSE\n",
        ")"
      )

      rstudioapi::insertText(code)
      stopApp()
    })
  }

  # Use browser as viewer to avoid RStudio display issues
  viewer <- browserViewer()
  runGadget(ui, server, viewer = viewer)
}
