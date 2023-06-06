################################################################################
#                                                                              #
#                                                                              #
#   Functions for creating visualizations specific to labor results            #
#                                                                              #
#                                                                              #
#   Zach Modig                                                                 #
#   November 2021                                                              #
#                                                                              # 
################################################################################



## A function to create a plot with some column in the location data frame
#
#
#
plotStatistic_location = function(stat,        # Statistic of interest (column name)
                                  xbar,        # Mean or median data frame
                                  stdev = NULL # If this is passed, a ribbon will be created
                                  ){
  
  #### Input Checking ####
  
  
  
  #### Begin Function ####
  
  df_stat = xbar[, c("Name", "Sector", "Time Step", stat)]
  cnames = colnames(df_stat)
  
  if (!is.null(stdev)){
    
    df_sd = stdev[, c("Name", "Sector", "Time Step", stat)]
    df_stat = cbind(df_stat, df_sd[, stat])
    colnames(df_stat) = c(cnames, "sd")
    df_stat$plus_sd = df_stat[[stat]] + df_stat$sd
    df_stat$minus_sd = df_stat[[stat]] - df_stat$sd
    
    
    ## Add an additional "fake" column that is Location + Sector to make the ribboning work
    df_stat$ribbon = factor(paste(df_stat$Name, df_stat$Sector))
  }
  
  colnames(df_stat)[colnames(df_stat) == "Name"] = "Location"
  
  
  ## Create base plot
  plt = ggplot(df_stat, aes(x = `Time Step`, y = .data[[stat]], color = Location)) + 
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





## Plot the components of utility
#
#
#   Assumes location data frame for mean utility values   
#
#
plotUtilityComponents = function(xbar,        # Mean or median data frame
                                 stdev = NULL # If this is passed, a ribbon will be created
                                 ){
  
  #### Input Checking ####
  
  
  
  #### Begin Function ####
  
  ## Add a location preference 
  xbar$Location = xbar$Name
  
  
  ## Get the data of interest and melt to a new data frame
  xbar = reshape2::melt(xbar, id.vars = c("Time Step", "Sector", "Location"), 
                        measure.vars = c("rent", "Ammenities", "mean utility", "mean wage", "Current Location Preference"))
  
  
  if (!is.null(stdev)){
    
    stdev$Location = stdev$Name
    stdev = reshape2::melt(stdev, id.vars = c("Time Step", "Sector", "Location"), 
                           measure.vars = c("rent", "Ammenities", "mean utility", "mean wage", "Current Location Preference"))
    
    xbar$plus_sd = xbar$value + stdev$value
    xbar$minus_sd = xbar$value - stdev$value
    
    xbar$ribbon = xbar$variable
  }
  
  
  ## Create base plot
  plt = ggplot(xbar, aes(x = `Time Step`, y = value, color = variable)) + 
          geom_line(aes(linetype = variable)) +
          # geom_line() + 
          facet_grid(Sector ~ Location) + # Note need to figure out how to ribbon on two variables...may need a dummy column?
          theme_minimal()
  
  
  ## If requested, add error bars
  if (!is.null(stdev)){
    
    plt = plt + geom_ribbon(aes(y = value, ymin = minus_sd, ymax = plus_sd, 
                                fill = ribbon), 
                            color = NA, 
                            alpha = .2, show.legend = FALSE) +
                  scale_fill_manual(values = rep("#808080", length(unique(xbar$ribbon))))
  }
  
  
  return(plt)
}




## Compare Statistic by Remote...
#
#   Plot the output of collapseMeanRemote_firm()
#
plotStatistic_location_byRemote = function(stat_name,    # Statistic of interest (column name)
                                           xbar,         # Mean or median data frame
                                           stdev = NULL  # If this is passed, a ribbon will be created
                                           ){
  
  #### Input Checking ####
  
  
  
  #### Begin Function ####
  
  df_stat = xbar[, c("Location", "Remote Allowed", "Sector", "Time Step", stat_name)]
  
  cnames = colnames(df_stat)
  if (!is.null(stdev)){
    
    
    df_stat = cbind(df_stat, stdev[, stat_name])
    colnames(df_stat) = c(cnames, "sd")
    df_stat$plus_sd = df_stat[[stat_name]] + df_stat$sd
    df_stat$minus_sd = df_stat[[stat_name]] - df_stat$sd
    
    
    ## Add an additional "fake" column that is Location + Sector to make the ribboning work
    df_stat$ribbon = factor(paste(df_stat$Location, df_stat$Sector, df_stat$`Remote Allowed`))
  }
  
  
  ## Remove any "missing" rows (i.e one of the combinations doesn't exist)
  idx = is.nan(df_stat[[stat_name]]) 
  if (!is.null(stdev)){
    idx = idx & is.na(df_stat$sd)
  }
  df_stat = df_stat[!idx, ]
  
  
  ## Create base plot
  plt = ggplot(df_stat, aes(x = `Time Step`, y = .data[[stat_name]], color = Location)) + 
          geom_line(aes(linetype = Sector)) +
          facet_grid(`Remote Allowed` ~ .) + 
          # geom_line() + 
          # facet_grid(Sector ~ .) + # Note need to figure out how to ribbon on two variables...may need a dummy column?
          theme_minimal() # update this with my theme 
  
  
  ## If a standard deviation was passed, add the ribbon
  if (!is.null(stdev)){
    
    plt = plt + 
            geom_ribbon(aes(y = .data[[stat_name]], ymin = minus_sd, ymax = plus_sd, 
                            fill = ribbon), 
                        color = NA, 
                        alpha = .2, show.legend = FALSE) +
            scale_fill_manual(values = rep("#808080", length(unique(df_stat$ribbon))))
    
    ## Note - for colored ribbons create facetted plots and set fill and color inside of the aes call above 
  }
  
  
  return(plt)
}






