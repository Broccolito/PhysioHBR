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

headerUI = dropdownMenu(messageItem(from = "Admin",
                                    message = "Welcome to Physio HBR!"),
                        type = "messages", badgeStatus = "primary")

sidebarUI = div(
  br(),
  downloadBttn(outputId = "sample_data", label = "Download Sample Data", style = "simple"),
  fileInput(inputId = "data_file", label = "Upload Data File", 
            multiple = FALSE, accept = c(".csv", ".xlsx")),
  sliderInput("factor_selection", label = "Factor Range", min = 1, 
              max = 100, value = c(1, 2)),
  sliderInput("numeric_selection", label = "Numeric Range", min = 1, 
              max = 100, value = c(1, 2)),
  h4(HTML("<hr>&nbsp&nbsp"), "Analysis"),
  sidebarMenu(
    menuItem("Covariance Heatmap", tabName = "heatmap_menu"),
    menuItem("Covariance Diff Heatmap", tabName = "diff_heatmap_menu"),
    menuItem("Boxplot", tabName = "boxplot_menu"),
    menuItem("Regression Plot", "regression_menu")
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
  uiOutput("header_title"),
  tableOutput("data_summary"), 
  verbatimTextOutput("factor_data_names"),
  verbatimTextOutput("numeric_data_names"),
  uiOutput("show_hide_summary"),
  br(),
  dataTableOutput("data_view"),
  tabItems(
    tabItem("heatmap_menu",
            div(p("Dashboard tab content"))
    ),
    tabItem("diff_heatmap_menu",
            "Widgets tab content"
    ),
    tabItem("boxplot_menu",
            "Sub-item 1 tab content"
    ),
    tabItem("regression_menu",
            "Sub-item 2 tab content"
    ),
    tabItem("developer_menu",
            "Sub-item 2 tab content"
    ),
    tabItem("macro_menu",
            "Sub-item 2 tab content"
    ),
    tabItem("about_menu",
            "Sub-item 2 tab content"
    )
  )
)

ui = dashboardPage(
  header = dashboardHeader(headerUI, title = "Physio HBR", titleWidth = 300),
  sidebar = dashboardSidebar(sidebarUI, width = 300),
  body = dashboardBody(bodyUI)
)

server = function(input, output, session) {
  
  observeEvent(input$data_file, {
    output$header_title = renderUI({
      fluidRow(
        box(
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
            column(width = 4,actionBttn(inputId = "show_summary_button", label = "Show Data Summary",
                                        style = "fill", block = TRUE, color = "success")),
            column(width = 4,actionBttn(inputId = "hide_summary_button", label = "Hide Data Summary",
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
  
  onSessionEnded(function(){stopApp()})
}

shinyApp(ui, server)

