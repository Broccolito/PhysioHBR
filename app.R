library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(reshape2)
library(ggplot2)
library(dplyr)

ui = dashboardPagePlus(
  skin = "black",
  header = dashboardHeaderPlus(title = "Physio HBR", titleWidth = 400),
  sidebar = dashboardSidebar(
    sidebarMenu(
      
      menuItem("Analysis",div(
        fileInput(inputId = "data_file_path",
                  label = "Input Data (.csv required)", 
                  accept = c(".csv")),
        actionBttn(inputId ="read_file", label = "Read File", 
                   size = "md", style = "fill", block = FALSE),
        br(),
        sliderInput(inputId = "p_cutoff", label = "Significant P Value",
                    min = 1e-20, max = 1, value = 0.05),
        actionBttn(inputId ="run_analysis", label = "Run Analysis", 
                   size = "md", style = "fill", block = FALSE),
        br()
      ), startExpanded = TRUE),
      
      menuItem("Settings",div(
        
      )),
      
      menuItem("Developers",div(
        
      ))
      
    ),
    width = 400),
  
  body = dashboardBody(
    tabsetPanel(type = "tabs",
                tabPanel("Data Table", uiOutput("data_display_ui")),
                tabPanel("Statistics", uiOutput("test_stats_display_ui")),
                tabPanel("Heatmap", fluidPage(
                  br(),
                  boxPlus(collapsible = FALSE, width = 6,height = NULL,
                          closable = FALSE,
                          fluidRow(
                            column(3, actionButton(inputId = "plot_heatmaps", label = "Plot Heatmaps")),
                            column(9, sliderInput(inputId = "heatmap_picture_size",
                                                  label = "Picture Size (px)", min = 200, max = 1500,
                                                  value = 650))
                          )
                  ),
                  boxPlus(collapsible = FALSE, width = 6,height = NULL,
                          closable = FALSE,
                          fluidRow(
                            column(3, actionButton(inputId = "save_all_heatmap_plots", label = "Save Heatmaps")),
                            column(4, sliderInput(inputId = "heatmap_width",
                                                  label = "Saved Graphics Width (in)", min = 5, max = 15,
                                                  value = 7)),
                            column(4, sliderInput(inputId = "heatmap_height",
                                                  label = "Saved Graphics Height (in)", min = 5, max = 15,
                                                  value = 7))
                          )
                  ),
                  uiOutput("heatmap_display_ui")
                )),
                tabPanel("Boxplot", fluidPage(
                  br(),
                  boxPlus(collapsible = FALSE, width = 6,height = NULL,
                          closable = FALSE,
                          fluidRow(
                            column(3, actionButton(inputId = "plot_boxplots", label = "Plot Boxplots")),
                            column(9, sliderInput(inputId = "boxplot_picture_size",
                                                  label = "Picture Size (px)", min = 200, max = 1500,
                                                  value = 650))
                          )
                  ),
                  boxPlus(collapsible = FALSE, width = 6,height = NULL,
                          closable = FALSE,
                          fluidRow(
                            column(3, actionButton(inputId = "save_all_boxplots", label = "Save Boxplots")),
                            column(4, sliderInput(inputId = "boxplot_width",
                                                  label = "Saved Graphics Width (in)", min = 5, max = 15,
                                                  value = 7)),
                            column(4, sliderInput(inputId = "boxplot_height",
                                                  label = "Saved Graphics Height (in)", min = 5, max = 15,
                                                  value = 7))
                          )
                  ),
                  boxPlus(collapsible = FALSE, width = 12,height = NULL,
                          closable = FALSE, align = "center",
                          uiOutput("boxplot_display_ui")
                  )
                )),
                tabPanel("Regression", fluidPage(
                  br(),
                  boxPlus(collapsible = FALSE, width = 6,height = NULL,
                          closable = FALSE,
                          fluidRow(
                            column(3, actionButton(inputId = "plot_regression_plot", label = "Plot Regression Plots")),
                            column(9, sliderInput(inputId = "regression_plot_picture_size",
                                                  label = "Picture Size (px)", min = 200, max = 1500,
                                                  value = 650))
                          )
                  ),
                  boxPlus(collapsible = FALSE, width = 6,height = NULL,
                          closable = FALSE,
                          fluidRow(
                            column(3, actionButton(inputId = "save_all_regression_plots", label = "Save Regression Plots")),
                            column(4, sliderInput(inputId = "regression_plot_width",
                                                  label = "Saved Graphics Width (in)", min = 5, max = 15,
                                                  value = 7)),
                            column(4, sliderInput(inputId = "regression_plot_height",
                                                  label = "Saved Graphics Height (in)", min = 5, max = 15,
                                                  value = 7))
                          )
                  ),
                  boxPlus(collapsible = FALSE, width = 12,height = NULL,
                          closable = FALSE, align = "center",
                          uiOutput("regression_plot_display_ui")
                  )
                ))
    )
    
  )
)

server = function(input, output, session){
  
  observeEvent(input$plot_regression_plot,{
    showModal(modalDialog(title = "Displaying Significant Regression Plots ...",
                          "Rendering regression plots showing significant correlations ... "))
    
    max_plots = dim(regression_result)[1]
    
    output$regression_plot_display_ui = renderUI({
      regression_plot_output_list = lapply(1:max_plots, function(i){
        regressionplotname = paste("regression_plot", i, sep="")
        plotOutput(regressionplotname, height = input$regression_plot_picture_size,
                   width = input$regression_plot_picture_size)
      })
      do.call(tagList, regression_plot_output_list)
    })
    
    for(i in 1:max_plots){
      x_name = as.character(regression_result$x[i])
      y_name = as.character(regression_result$y[i])
      x = as.numeric(as.character(data_file_numeric[[x_name]]))
      y = as.numeric(as.character(data_file_numeric[[y_name]]))
      xy = data.frame(x = x, y = y) %>%
        na.omit()
      fig = ggplot(data = xy, aes(x = x, y = y)) + 
        geom_smooth(method = "lm",color = "black",formula = "y~x") +
        geom_point(size = 2) + 
        geom_point(size = 2) + 
        xlab(x_name) + 
        ylab(y_name) +
        ggtitle(label = "", subtitle = paste0("P = ",round(regression_result$p_value[i],3),
                                              "; Effect Size = ", round(regression_result$effect_size,3),
                                              "; N = ",regression_result$n)) +
        theme_bw()
      assign(paste0("regression_fig",i),fig)
    }
    
    for (i in 1:max_plots){
      local({
        local_i = i
        regressionplotname = paste("regression_plot", local_i, sep="")
        output[[regressionplotname]] = renderPlot({
          get(paste0("regression_fig",local_i))
        })
      })
    }
    
    removeModal()
    
    
    
  })
  
  observeEvent(input$save_all_boxplots,{
    
    showModal(modalDialog(title = "Saving Significant Boxplots ...",
                          "Writing significant boxplots to png files ... \n
                          This process may take a while, dependent on the number of figures to export."))
    
    max_plots = dim(ttest_anova_result)[1]
    for(i in 1:max_plots){
      x_name = as.character(ttest_anova_result$x[i])
      y_name = as.character(ttest_anova_result$y[i])
      x = data_file_factor[[x_name]]
      y = as.numeric(as.character(data_file_numeric[[y_name]]))
      xy = data.frame(x = x, y = y) %>%
        na.omit()
      fig = ggplot(data = xy, aes(x = x, y = y)) + 
        geom_boxplot() + 
        geom_point(size = 2) + 
        xlab(x_name) + 
        ylab(y_name) +
        ggtitle(label = "", subtitle = paste0("P = ",round(ttest_anova_result$p_value[i],3),
                                              "; N = ", ttest_anova_result$n_numeric[i])) + 
        theme_bw() + 
        theme(plot.margin = margin(1, 1, 1, 1, "cm")) + 
        ggsave(filename = paste0("output/Boxplot ",y_name,
                                 " Vs. ",x_name,
                                 gsub(":",".",date()),".png"),
               device = "png", dpi = 1200,
               width = input$boxplot_width,
               height = input$boxplot_height)
    }
    
    removeModal()
    
  })
  
  observeEvent(input$plot_boxplots,{
    
    showModal(modalDialog(title = "Displaying Significant Boxplots ...",
                          "Rendering boxplots showing significant differences ... "))
    
    max_plots = dim(ttest_anova_result)[1]
    
    output$boxplot_display_ui = renderUI({
      boxplot_output_list = lapply(1:max_plots, function(i){
        boxplotname = paste("boxplot_plot", i, sep="")
        plotOutput(boxplotname, height = input$boxplot_picture_size,
                   width = input$boxplot_picture_size)
      })
      do.call(tagList, boxplot_output_list)
    })
    for(i in 1:max_plots){
      x_name = as.character(ttest_anova_result$x[i])
      y_name = as.character(ttest_anova_result$y[i])
      x = data_file_factor[[x_name]]
      y = as.numeric(as.character(data_file_numeric[[y_name]]))
      xy = data.frame(x = x, y = y) %>%
        na.omit()
      fig = ggplot(data = xy, aes(x = x, y = y)) + 
        geom_boxplot() + 
        geom_point(size = 2) + 
        xlab(x_name) + 
        ylab(y_name) +
        ggtitle(label = "", subtitle = paste0("P = ",round(ttest_anova_result$p_value[i],3),
                                              "; N = ", ttest_anova_result$n_numeric[i])) + 
        theme_bw() + 
        theme(plot.margin = margin(1, 1, 1, 1, "cm"))
      assign(paste0("ttest_anova_fig",i),fig)
    }
    
    for (i in 1:max_plots){
      local({
        local_i = i
        boxplotname = paste("boxplot_plot", local_i, sep="")
        output[[boxplotname]] = renderPlot({
          get(paste0("ttest_anova_fig",local_i))
        })
      })
    }
    
    removeModal()
    
  })
  
  plot_cov = function(df, title = "Correlation Matrix"){
    cormat = round(cor(df,use="na.or.complete"),2)
    melted_cormat = melt(cormat)
    get_lower_tri=function(cormat){
      cormat[upper.tri(cormat)] = NA
      return(cormat)
    }
    get_upper_tri = function(cormat){
      cormat[lower.tri(cormat)]= NA
      return(cormat)
    }
    upper_tri = get_upper_tri(cormat)
    melted_cormat = melt(upper_tri, na.rm = TRUE)
    reorder_cormat = function(cormat){
      dd = as.dist((1-cormat)/2)
      hc = hclust(dd)
      cormat = cormat[hc$order, hc$order]
    }
    upper_tri = get_upper_tri(cormat)
    melted_cormat = melt(upper_tri, na.rm = TRUE)
    ggheatmap = ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson Correlation") +
      theme_minimal()+ # minimal theme
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1))+
      coord_fixed()
    plot_output = ggheatmap + 
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                   title.position = "top", title.hjust = 0.5)) + 
      ggtitle(title)
    return(plot_output)
  }
  
  plot_p_cov = function(df, title = "Significant Positive (Red) and Negative (Blue) Correlations"){
    n = dim(df)[1]
    cormat = cor(df,use="na.or.complete")
    t_value_mat = cormat/((1-cormat^2)/(n-2))^0.5
    t_value_mat[t_value_mat <= -1.644854] = -1000
    t_value_mat[t_value_mat >= 1.644854] = 1000
    t_value_mat[t_value_mat > -1.644854 & t_value_mat < 1.644854] = 0
    p_value_mat = pnorm(t_value_mat)
    p_value_mat = round((p_value_mat - 0.5) * 2, 0)
    cormat = p_value_mat
    melted_cormat <- melt(cormat)
    # Get lower triangle of the correlation matrix
    get_lower_tri = function(cormat){
      cormat[upper.tri(cormat)] <- NA
      return(cormat)
    }
    # Get upper triangle of the correlation matrix
    get_upper_tri = function(cormat){
      cormat[lower.tri(cormat)]<- NA
      return(cormat)
    }
    upper_tri = get_upper_tri(cormat)
    # Melt the correlation matrix
    melted_cormat = melt(upper_tri, na.rm = TRUE)
    reorder_cormat = function(cormat){
      # Use correlation between variables as distance
      dd = as.dist((1-cormat)/2)
      hc = hclust(dd)
      cormat = cormat[hc$order, hc$order]
    }
    # Reorder the correlation matrix
    # cormat <- reorder_cormat(cormat)
    upper_tri = get_upper_tri(cormat)
    # Melt the correlation matrix
    melted_cormat = melt(upper_tri, na.rm = TRUE)
    # Create a ggheatmap
    ggheatmap = ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1)) +
      coord_fixed()
    plot_output = ggheatmap + 
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                   title.position = "top", title.hjust = 0.5)) + 
      ggtitle(title)
    return(plot_output)
  }
  
  observeEvent(input$plot_heatmaps,{
    output$heatmap_display_ui = renderUI({
      div(
        boxPlus(collapsible = FALSE, width = 12,height = input$heatmap_picture_size,
                closable = FALSE,
                plotOutput("correlation_heatmap_plot", height = input$heatmap_picture_size*0.9),
                actionButton(inputId = "save_correlation_heatmap_plot",label = "Save Plot")
        ),
        boxPlus(collapsible = FALSE, width = 12,height = input$heatmap_picture_size,
                closable = FALSE,
                plotOutput("significant_heatmap_plot", height = input$heatmap_picture_size*0.9),
                actionButton(inputId = "save_significant_heatmap_plot",label = "Save Plot")
        )
      )
    })
  })
  
  observeEvent(input$save_all_heatmap_plots,{
    
    showModal(modalDialog(title = "Saving heatmap plots ...",
                          "Writing heatmap plots to png files ... "))
    
    data_file_numeric = data_file_numeric %>% 
      mutate_all(type.convert) %>% 
      mutate_if(is.factor, as.numeric)
    plot_cov(df = data_file_numeric) + 
      ggsave(filename = paste0("output/correlation heatmap plot",gsub(":",".",date()),".png"),
             device = "png", dpi=1200,
             width = input$heatmap_width,
             height = input$heatmap_height)
    
    data_file_numeric = data_file_numeric %>% 
      mutate_all(type.convert) %>% 
      mutate_if(is.factor, as.numeric)
    plot_p_cov(df = data_file_numeric) + 
      ggsave(filename = paste0("output/significant heatmap plot",gsub(":",".",date()),".png"),
             device = "png", dpi=1200,
             width = input$heatmap_width,
             height = input$heatmap_height)
    
    removeModal()
    
  })
  
  observeEvent(input$save_correlation_heatmap_plot,{
    
    showModal(modalDialog(title = "Saving correlation heatmap plot ...",
                          "Writing the heatmap plot to png files ... "))
    
    data_file_numeric = data_file_numeric %>% 
      mutate_all(type.convert) %>% 
      mutate_if(is.factor, as.numeric)
    plot_cov(df = data_file_numeric) + 
      ggsave(filename = paste0("output/correlation heatmap plot",gsub(":",".",date()),".png"),
             device = "png", dpi=1200,
             width = input$heatmap_width,
             height = input$heatmap_height)
    
    removeModal()
    
  })
  
  observeEvent(input$save_significant_heatmap_plot,{
    
    showModal(modalDialog(title = "Saving significant correlation plot ...",
                          "Writing the heatmap plot to png files ... "))
    
    data_file_numeric = data_file_numeric %>% 
      mutate_all(type.convert) %>% 
      mutate_if(is.factor, as.numeric)
    plot_p_cov(df = data_file_numeric) + 
      ggsave(filename = paste0("output/significant heatmap plot",gsub(":",".",date()),".png"),
             device = "png", dpi=1200,
             width = input$heatmap_width,
             height = input$heatmap_height)
    
    removeModal()
    
  })
  
  observeEvent(input$plot_heatmaps,{
    output$correlation_heatmap_plot = renderPlot({
      data_file_numeric = data_file_numeric %>% 
        mutate_all(type.convert) %>% 
        mutate_if(is.factor, as.numeric)
      plot_cov(df = data_file_numeric)
    })
  })
  
  observeEvent(input$plot_heatmaps,{
    output$significant_heatmap_plot = renderPlot({
      data_file_numeric = data_file_numeric %>% 
        mutate_all(type.convert) %>% 
        mutate_if(is.factor, as.numeric)
      plot_p_cov(df = data_file_numeric)
    })
  })
  
  
  
  observeEvent(input$save_ttest_anova_table,{
    showModal(modalDialog(title = "Saving Files ...",
                          "Writing Table to a csv file"))
    filename = paste("T test ANOVA Statistics ",gsub(":",".",date()),".csv", sep = "")
    filepath = paste0("output/",filename)
    write.csv(regression_result, 
              file = filepath,
              quote = FALSE,
              row.names = FALSE)
    removeModal()
  })
  
  observeEvent(input$save_regression_table,{
    showModal(modalDialog(title = "Saving Files ...",
                          "Writing Table to a csv file"))
    filename = paste("/Regression Statistics ",gsub(":",".",date()),".csv", sep = "")
    filepath = paste0("output/",filename)
    write.csv(regression_result, 
              file = filepath,
              quote = FALSE,
              row.names = FALSE)
    removeModal()
  })
  
  observeEvent(input$run_analysis,{
    output$test_stats_display_ui = renderUI({
      div(
        fluidRow(
          column(6,
                 div(
                   hr(),
                   h4("Regression Statistics"),
                   tableOutput("regression_table_output"),
                   actionButton(inputId = "save_regression_table",
                                label = "Save Table")
                 )
          ),
          column(6,
                 div(
                   hr(),
                   h4("T test and ANOVA Statistics"),
                   tableOutput("ttest_anova_table_output"),
                   actionButton(inputId = "save_ttest_anova_table",
                                label = "Save Table")
                 )
          )
        )
      )
    })
  })
  
  observeEvent(input$run_analysis,{
    
    showModal(
      modalDialog(title = "Data Processing ... ", 
                  "Statistics Tables will be displayed once the analysis is finished ... "
      )
    )
    
    p_cutoff = input$p_cutoff
    
    l_res_mat = vector()
    data_file_numeric_name = names(data_file_numeric) 
    for(i in 1:dim(data_file_numeric)[2]){
      for(j in 1:dim(data_file_numeric)[2]){
        if(j > i){
          try({
            x_name = data_file_numeric_name[i]
            y_name = data_file_numeric_name[j]
            xy = data.frame(x = as.numeric(data_file_numeric[,i]),
                            y = as.numeric(data_file_numeric[,j])) %>%
              na.omit()
            l = summary(lm(formula = y~x, data = xy))
            effsize = l$coefficients[2,1]
            p_value = l$coefficients[2,4]
            n = dim(xy)[1]
            l_res = data.frame(x = x_name, y = y_name, n = n,
                               effect_size = effsize, p_value = p_value)
            l_res_mat = rbind.data.frame(l_res_mat, l_res)
          }, silent = TRUE)
        }
      }
    }
    l_res_mat = arrange(l_res_mat, p_value) %>%
      filter(p_value <= p_cutoff)
    
    f_res_mat = vector()
    data_file_factor_name = names(data_file_factor)
    data_file_numeric_name = names(data_file_numeric)
    for(i in 1:dim(data_file_factor)[2]){
      for(j in 1:dim(data_file_numeric)[2]){
        try({
          x_name = data_file_factor_name[i]
          y_name = data_file_numeric_name[j]
          xy = data.frame(x = as.factor(data_file_factor[,i]),
                          y = as.numeric(data_file_numeric[,j])) %>%
            na.omit()
          n_factor = length(unique(xy$x))
          n_numeric = length(unique(xy$y))
          if(n_factor <= 2){
            ttest = t.test(formula = y ~ x, data = xy)
            p_value = ttest$p.value
            effsize = diff(ttest$estimate)
            method = "T test"
            f_res = data.frame(x = x_name, y = y_name, 
                               n_factor = n_factor, n_numeric = n_numeric,
                               effect_size = effsize, p_value = p_value,
                               method = method)
          }else{
            anv = aov(formula = y ~ x, data = xy)
            p_value = summary(anv)[[1]][["Pr(>F)"]][1]
            effsize = NA
            method = "ANOVA"
            f_res = data.frame(x = x_name, y = y_name, 
                               n_factor = n_factor, n_numeric = n_numeric,
                               effect_size = effsize, p_value = p_value,
                               method = method)
          }
          f_res_mat = rbind.data.frame(f_res_mat, f_res)
        })
      }
    }
    rownames(f_res_mat) = NULL
    f_res_mat = arrange(f_res_mat, p_value) %>%
      filter(p_value <= p_cutoff)
    
    output$regression_table_output = renderTable({
      assign("regression_result",l_res_mat,envir = .GlobalEnv)
      l_res_mat
    })
    
    output$ttest_anova_table_output = renderTable({
      assign("ttest_anova_result",f_res_mat,envir = .GlobalEnv)
      f_res_mat
    })
    
    removeModal()
    
  })
  
  observeEvent(input$read_file,{
    output$data_display_ui = renderUI({
      div(
        hr(),
        h4("Subject ID list"),
        tableOutput("id_data_table_output"),
        hr(),
        h4("Factor data head"),
        tableOutput("factor_data_table_output"),
        hr(),
        h4("Numeric data head"),
        tableOutput("numeric_data_table_output"),
        hr(),
        h4("Character data head"),
        tableOutput("character_data_table_output"),
        hr()
      )
    })
  })
  
  data_file = NULL
  observeEvent(input$read_file,{
    if(!is.null(input$data_file_path)){
      data_file = read.csv(input$data_file_path$datapath)
    }
    data_file_data_type = data_file[1,]
    data_file = data_file[-1,]
    
    data_file_id <<- data_file[,1,drop=FALSE]
    data_file_numeric <<- data_file[,which(data_file_data_type=="numeric"),drop=FALSE]
    data_file_character <<- data_file[,which(data_file_data_type=="character"),drop=FALSE]
    data_file_factor <<- data_file[,which(data_file_data_type=="factor"),drop=FALSE]
    
    output$id_data_table_output = renderTable(colnames = FALSE,{
      table_dimension = dim(data_file_id)
      data_file_id_display = data_file_id
      if(table_dimension[1]>10){
        data_file_id_display_1 = as.character(data_file_id_display[c(1:4),])
        data_file_id_display_2 = as.character(data_file_id_display[(table_dimension[1]-3):table_dimension[1],])
        data_file_id_display = c(data_file_id_display_1," ... ",data_file_id_display_2)
      }else{
        data_file_id_display = as.character(data_file_id_display)
      }
      data_file_id_display = t(data.frame(id = data_file_id_display))
      colnames(data_file_id_display) = NULL
      data_file_id_display
    })
    
    output$factor_data_table_output = renderTable({
      table_dimension = dim(data_file_factor)
      data_file_factor_display = data_file_factor
      if(table_dimension[1] >= 5){
        data_file_factor_display = data_file_factor_display[1:5,]
      }
      if(table_dimension[2] >= 15){
        data_file_factor_display = data_file_factor_display[,1:15]
      }
      data_file_factor_display
    })
    
    output$numeric_data_table_output = renderTable({
      table_dimension = dim(data_file_numeric)
      data_file_numeric_display = data_file_numeric
      if(table_dimension[1] >= 5){
        data_file_numeric_display = data_file_numeric_display[1:5,]
      }
      if(table_dimension[2] >= 15){
        data_file_numeric_display = data_file_numeric_display[,1:15]
      }
      data_file_numeric_display
    })
    
    output$character_data_table_output = renderTable({
      table_dimension = dim(data_file_character)
      data_file_character_display = data_file_character
      if(table_dimension[1] >= 5){
        data_file_character_display = head(data_file_character_display,5)
      }
      if(table_dimension[2] >= 15){
        data_file_character_display = data_file_character_display[,1:15,drop=FALSE]
      }
      data_file_character_display
    })
    
  })
  
  
  
  
  onSessionEnded(function(){
    stopApp()
  })
}

shinyApp(ui, server)