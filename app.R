if(!require("shiny")){
  install.packages("shiny")
  library("shiny")
}
if(!require("shinydashboard")){
  install.packages("shinydashboard")
  library("shinydashboard")
}
if(!require("shinyWidgets")){
  install.packages("shinyWidgets")
  library("shinyWidgets")
}
if(!require("ggplot2")){
  install.packages("ggplot2")
  library("ggplot2")
}
if(!require("readxl")){
  install.packages("readxl")
  library("readxl")
}
if(!require("knitr")){
  install.packages("knitr")
  library("knitr")
}
if(!require("shinyjs")){
  install.packages("shinyjs")
  library("shinyjs")
}
if(!require("shinyAce")){
  install.packages("shinyAce")
  library("shinyAce")
}
if(!require("reshape2")){
  install.packages("reshape2")
  library("reshape2")
}

headerUI = dropdownMenu(messageItem(from = "Admin",
                                    message = "Welcome to Physio HBR!"),
                        type = "messages", badgeStatus = "primary")

sidebarUI = div(
  sidebarMenu(
    br(),
    downloadBttn(outputId = "sample_data", label = "Download Sample Data", style = "simple"),
    h4(HTML("<hr>&nbsp&nbsp"), "Upload Data"),
    menuItem("Data Preparation",
             br(),
             fileInput(inputId = "data_file", label = "Upload Data File", 
                       multiple = FALSE, accept = c(".csv", ".xlsx")),
             sliderInput("factor_selection", label = "Factor Range", min = 1, 
                         max = 100, value = c(1, 2)),
             sliderInput("numeric_selection", label = "Numeric Range", min = 1, 
                         max = 100, value = c(1, 2))),
    h4(HTML("<hr>&nbsp&nbsp"), "Analysis"),
    menuItem("Covariance Heatmap", tabName = "heatmap_menu"),
    menuItem("Covariance Diff Heatmap", tabName = "diff_heatmap_menu"),
    menuItem("Boxplot", tabName = "boxplot_menu"),
    menuItem("Regression Plot", tabName = "regression_menu")
  ),
  h4(HTML("<hr>&nbsp&nbsp"), "Advanced"),
  sidebarMenu(
    menuItem("Developer Mode", tabName = "developer_menu"),
    menuItem("Upload Macros", tabName = "macro_menu")
  ),
  h4(HTML("<hr>&nbsp&nbsp"), "About"),
  sidebarMenu(
    menuItem("About PhysioHBR", tabName = "about_menu")
  )
)

bodyUI = div(
  box(width = 12,collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE,
      uiOutput("header_title"),
      tableOutput("data_summary"),
      verbatimTextOutput("factor_data_names"),
      verbatimTextOutput("numeric_data_names"),
      uiOutput("show_hide_summary"),
      br(),
      dataTableOutput("data_view")
  ),
  tabItems(
    tabItem("heatmap_menu", uiOutput("heatmap_ui")),
    tabItem("diff_heatmap_menu", uiOutput("diff_heatmap_ui")),
    tabItem("boxplot_menu", uiOutput("boxplot_ui")),
    tabItem("regression_menu", uiOutput("regression_ui")),
    tabItem("developer_menu", uiOutput("developer_ui")),
    tabItem("macro_menu", uiOutput("macro_ui")),
    tabItem("about_menu", uiOutput("about_ui"))
  )
)

ui = dashboardPage(
  header = dashboardHeader(headerUI, title = "Physio HBR", titleWidth = 300),
  sidebar = dashboardSidebar(sidebarUI, width = 300),
  body = dashboardBody(bodyUI)
)

server = function(input, output, session) {
  
  observeEvent(input$data_file, {
    output$heatmap_ui = renderUI({
      fluidRow(
        column(8, actionBttn(inputId = "plot_heatmap", label = "Plot Heatmap", 
                             block = TRUE, color = "royal", style = "fill"))
      )
    })
  })
  
  observeEvent(input$data_file, {
    output$diff_heatmap_ui = renderUI({
      fluidRow(
        column(8, actionBttn(inputId = "plot_diff_heatmap", label = "Plot Diff Heatmap", 
                             block = TRUE, color = "success", style = "fill"))
      )
    })
  })
  
  observeEvent(input$data_file, {
    output$boxplot_ui = renderUI({
      fluidRow(
        column(8, actionBttn(inputId = "plot_boxplot", label = "Plot Boxplot", 
                             block = TRUE, color = "primary", style = "fill"))
      )
    })
  })
  
  observeEvent(input$data_file, {
    output$regression_ui = renderUI({
      fluidRow(
        column(8, actionBttn(inputId = "plot_regression", label = "Plot Regression Plot", 
                             block = TRUE, color = "danger", style = "fill"))
      )
    })
  })
  
  output$developer_ui = renderUI({
    initial_value = "
    ### Use dff to refer to the data frame uploaded
    list(
    data_summary = summary(dff)
    )
    "
    box(width = 12, solidHeader = TRUE, title = "Terminal",
        aceEditor("code", mode = "r", height = "200px", value = initial_value),
        fluidRow(
          column(4, actionBttn(inputId = "eval_code", label = "Analyse", 
                               block = TRUE, style = "fill", color = "royal"))),
        hr(),
        verbatimTextOutput("terminal_output")
    )
  })
  
  output$terminal_output = renderPrint({
    input$eval_code
    eval(parse(text = isolate(input$code)))
  })
  
  output$macro_ui = renderUI({
    init_value = "
    Fill in the description of the macro uploaded.
    E.g:
    \"\"\"
    This Macro is used to run multiple linear regressions on uploaded data files..
    Function X and Function Y are used for ...
    This Macro is composed by WG 2019 and IX 2019 ... 
    \"\"\"
    "
    box(
      fileInput(inputId = "uploaded_macro", label = "Upload Macro", multiple = TRUE),
      hr(),
      h4("Macro Descriotion"),
      textAreaInput(inputId = "macro_description", label = NULL, placeholder = init_value,
                    resize = "both", width = "300", height = "300px"),
      actionBttn(inputId = "submit_macro", label = "Submit Macro", style = "fill",
                 block = TRUE, color = "primary")
    )
  })
  
  output$about_ui = renderUI({
    box(width = 12,
        includeMarkdown("README.md")
    )
  })
  
  #####################################################
  
  observeEvent(input$data_file, {
    output$header_title = renderUI({
      fluidRow(
        box(width = 12,
            h4(tags$strong(paste0("Uploaded Table: ", input$data_file$name))),
            h5(paste0("Upload Time: ", as.character(Sys.time())))
        )
      )
    })
  })
  
  observeEvent(input$data_file, {
    output$show_hide_summary = renderUI({
      div(hr(),
          fluidRow(
            column(width = 4,actionBttn(inputId = "show_summary_button", label = "Show Table",
                                        style = "fill", block = TRUE, color = "success")),
            column(width = 4,actionBttn(inputId = "hide_summary_button", label = "Hide Table",
                                        style = "fill", block = TRUE, color = "warning"))
          ))
    })
  })
  
  observeEvent(input$show_summary_button, {
    output$data_view = renderDataTable({
      req(input$data_file)
      return(dff)
    }, options = list(pageLength = 10))
  })
  
  observeEvent(input$hide_summary_button, {
    output$data_view = renderDataTable({
      NULL
    }, options = list(pageLength = 10))
  })
  
  observeEvent(input$data_file, {
    tryCatch({
      df = as.data.frame(read_excel(input$data_file$datapath))
    },
    error = function(e) {
      tryCatch({
        df = as.data.frame(read.csv(input$data_file$datapath))
      },
      error = function(ee){
        stop(safeError(ee))
      })
    }
    )
    updateNumericInput(session, "factor_selection", max = dim(df)[2])
    updateNumericInput(session, "numeric_selection", max = dim(df)[2])
  })
  
  output$data_summary = renderTable({
    req(input$data_file)
    tryCatch({
      dff <<- as.data.frame(read_excel(input$data_file$datapath))
    },
    error = function(e) {
      tryCatch({
        dff <<- as.data.frame(read.csv(input$data_file$datapath))
      },
      error = function(ee){
        stop(safeError(ee))
        
      })
    })
    data.frame(Data_Size = paste(round(input$data_file$size * 10^(-6),3), "MB"),
               Observation = dim(dff)[1],
               Feature = dim(dff)[2])
  })
  
  output$sample_data = downloadHandler(
    filename = function(){
      "sample_data_sheet.csv"
    },
    content = function(con){
      data_download = as.data.frame(read_excel(path = "www/sample_data_sheet.xlsx"))
      write.csv(data_download, con, quote = FALSE, row.names = FALSE)
    }
  )
  
  observeEvent(input$data_file, {
    output$factor_data_names = renderText({
      paste("Factor Variables:", 
            paste(names(dff)[seq(input$factor_selection[1], input$factor_selection[2])], collapse = ", "))
    })
    
    output$numeric_data_names = renderText({
      paste("Numeric Variables",
            paste(names(dff)[seq(input$numeric_selection[1], input$numeric_selection[2])], collapse = ", "))
    })
  })
  
  observeEvent(input$plot_heatmap, {
    print(2)
  })
  
  observeEvent(input$plot_diff_heatmap, {
    print(1)
  })
  
  observeEvent(input$plot_boxplot, {
    
  })
  
  observeEvent(input$plot_regression, {
    
  })
  
  onSessionEnded(function(){stopApp()})
}

shinyApp(ui, server)

