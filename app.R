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

headerUI = dropdownMenu(type = "messages", badgeStatus = "primary")

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
  actionBttn("plot_cov_heatmap", label = "Cov Heatmap", 
             block = FALSE, style = "float"),
  actionBttn("plot_cov_diff_heatmap", label = "Diff Heatmap",
             block = FALSE, style = "material-flat")
)

bodyUI = div(
  tableOutput("data_summary"),
  verbatimTextOutput("factor_data_names"),
  verbatimTextOutput("numeric_data_names")
)

ui = dashboardPage(
  header = dashboardHeader(headerUI, title = "Physio HBR", titleWidth = 300),
  sidebar = dashboardSidebar(sidebarUI, width = 300),
  body = dashboardBody(bodyUI)
)

server = function(input, output, session) {
  
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
    }
    )
    data.frame(Data_Name = input$data_file$name,
               Upload_Time = as.character(Sys.time()),
               Data_Size = paste(round(input$data_file$size * 10^(-6),3), "MB"),
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

