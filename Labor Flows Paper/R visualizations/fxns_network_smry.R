################################################################################
#                                                                              #
#                                                                              #
#   Functions for creating visualizations specific to Summary Network results  #
#                                                                              #
#                                                                              #
#   Zach Modig                                                                 #
#   November 2021                                                              #
#                                                                              # 
################################################################################





## A function to create a plot with some column in the network summary data.frame
#
#
#
plotStatistic_networkSummary = function(stat,        # Statistic of interest (column name)
                                        xbar,        # Mean or median data frame
                                        stdev = NULL # If this is passed, a ribbon will be created
                                        ){
  
  #### Input Checking ####
  
  
  
  #### Begin Function ####
  
  df_stat = xbar[, c("Sector", "Time Step", stat)]
  cnames = colnames(df_stat)
  
  if (!is.null(stdev)){
    
    df_sd = stdev[, c("Sector", "Time Step", stat)]
    df_stat = cbind(df_stat, df_sd[, stat])
    colnames(df_stat) = c(cnames, "sd")
    df_stat$plus_sd = df_stat[[stat]] + df_stat$sd
    df_stat$minus_sd = df_stat[[stat]] - df_stat$sd
    
    
    ## Add an additional "fake" column that is Location + Sector to make the ribboning work
    df_stat$ribbon = factor(paste("ribbon", df_stat$Sector))
  }
  
  
  ## Create base plot
  plt = ggplot(df_stat, aes(x = `Time Step`, y = .data[[stat]], color = Sector)) + 
    geom_line(aes(linetype = Sector)) +
    # geom_line() + 
    # facet_grid(Sector ~ .) + # Note need to figure out how to ribbon on two variables...may need a dummy column?
    theme_minimal() # update this with my theme 
  
  
  ## If a standard deviation was passed, add the ribbon
  if (!is.null(stdev)){
    
    plt = plt + 
      geom_ribbon(aes(y = .data[[stat]], ymin = minus_sd, ymax = plus_sd, 
                      fill = ribbon), 
                  color = NA, 
                  alpha = .2, show.legend = FALSE) +
      scale_fill_manual(values = rep("#808080", length(unique(df_stat$ribbon))))
    
    ## Note - for colored ribbons create facetted plots and set fill and color inisde of the aes call above 
  }
  
  
  return(plt)
}


