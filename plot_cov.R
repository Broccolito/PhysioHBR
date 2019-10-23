generate_save_heatmap = function(factor_variable, numeric_variable, data){
  wk_dir = "image_output/heatmap"
  if(!exists(wk_dir)){
    dir.create(wk_dir)
  }
  plot_cov = function(df, title = "plot title"){
    
    cormat = round(cor(df),2)
    
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
  plot_cov(df = data[,numeric_variable], title = "Overall Correlation Heatmap")
  ggsave(filename = paste0(wk_dir, "/Overall Correlation Heatmap.png"), device = "png")
  
  zip::zipr("www/heatmap.zip", paste0(wk_dir, "/", list.files(wk_dir)))
}

