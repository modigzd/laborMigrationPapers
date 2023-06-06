################################################################################
#                                                                              #
#                                                                              #
#   Functions for creating visualizations specific to firm results             #
#                                                                              #
#                                                                              #
#   Zach Modig                                                                 #
#   November 2021                                                              #
#                                                                              # 
################################################################################




## Plot Mean Firm Statistics
#
#
#
plotStatistic_MeanFirm = function(stat,        # Statistic of interest (column name)
                                  xbar,        # Mean or median data frame
                                  stdev = NULL # If this is passed, a ribbon will be created
                                  ){
  
  #### Input Checking ####
  
  
  
  #### Begin Function ####
  
  df_stat = xbar[, c("Sector", "Location", "Time Step", stat)]
  cnames = colnames(df_stat)
  
  if (!is.null(stdev)){
    
    df_sd = stdev[, c("Sector", "Location", "Time Step", stat)]
    df_stat = cbind(df_stat, df_sd[, stat])
    colnames(df_stat) = c(cnames, "sd")
    df_stat$plus_sd = df_stat[[stat]] + df_stat$sd
    df_stat$minus_sd = df_stat[[stat]] - df_stat$sd
    
    
    ## Add an additional "fake" column that is Location + Sector to make the ribboning work
    df_stat$ribbon = factor(paste(df_stat$Location, df_stat$Sector))
  }
  
  
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



## Plot the Wage along with the components
#
#   Plots mean wages along with their components by location and sector
#
#   Note that the values are scaled by some constant labor elasticity...should
#   capture this in python eventually
#
#
plotMeanWageComponents = function(xbar,         # Mean or median data frame
                                  stdev = NULL, # If this is passed, a ribbon will be created
                                  alpha = .75   # Labor Elasticity
                                  ){
  #### Input Checking ####
  
  
  
  #### Begin Function ####
  
  ## Add a location preference 
  xbar$Location = xbar$Name
  
  
  ## Get the data of interest and melt to a new data frame
  xbar = reshape2::melt(xbar, id.vars = c("Time Step", "Sector", "Location"), 
                        measure.vars = c("Firm Mean Wage", "Firm Mean Capital", "Firm Mean Workers", "Location Productivity Shift", "Firm Mean Productivity Shift"))
  
  
  ## N and K should be raised to the appropriate power
  xbar$value[xbar$variable != "Firm Mean Workers"] = xbar$value[xbar$variable == "Firm Mean Workers"]^(alpha - 1)
  xbar$value[xbar$variable == "Firm Mean Capital"] = xbar$value[xbar$variable == "Firm Mean Capital"]^(1 - alpha)
  
  ## Re-scale the appropriate variables since they seem to have gotten all askew
  xbar$value[xbar$variable == "Firm Mean Wage"] = exp(xbar$value[xbar$variable == "Firm Mean Wage"])
  
  xbar$value[xbar$variable != "Firm Mean Wage"] = xbar$value[xbar$variable != "Firm Mean Wage"] * alpha
  
  if (!is.null(stdev)){
    
    stdev$Location = stdev$Name
    stdev = reshape2::melt(stdev, id.vars = c("Time Step", "Sector", "Location"), 
                           measure.vars = c("Firm Mean Wage", "Firm Mean Capital", "Firm Mean Workers", "Location Productivity Shift", "Firm Mean Productivity Shift"))
    
    xbar$plus_sd = xbar$value + stdev$value
    xbar$minus_sd = xbar$value - stdev$value
    
    xbar$ribbon = xbar$variable
  }
  
  
  ## Rename the variables
  var_names = as.character(xbar$variable)
  var_names[var_names == "Firm Mean Wage"] = "Wage"
  var_names[var_names == "Firm Mean Capital"] = "K"
  var_names[var_names == "Firm Mean Workers"] = "N"
  var_names[var_names == "Location Productivity Shift"] = "X_l"
  var_names[var_names == "Firm Mean Productivity Shift"] = "X_f"
  
  xbar$variable = as.factor(var_names)
  
  
  ## Apparently, right now this wage is somehow 
  
  
  ## Create the base plot
  plt = ggplot(xbar, aes(x = `Time Step`, y = value, color = variable)) + 
          geom_line(aes(linetype = variable)) +
          # geom_line() + 
          facet_grid(Sector ~ Location) + # Note need to figure out how to ribbon on two variables...may need a dummy column?
          theme_minimal()
  
  
  ## If If requested, add error bars
  if (!is.null(stdev)){
    
    plt = plt + geom_ribbon(aes(y = value, ymin = minus_sd, ymax = plus_sd, 
                                fill = ribbon), 
                            color = NA, 
                            alpha = .2, show.legend = FALSE) +
      scale_fill_manual(values = rep("#808080", length(unique(xbar$ribbon))))
  }
  
  
  return(plt)
}



## Plot the Profit Components
#
#
#
#
plotMeanProfitComponents = function(xbar,        # Mean or median data frame
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
}





## Get Remote Mean values
#
#   For firms, in order to effectively visualize policy choice of remote allowed
#   via remote or not, need to collapse the raw firm values into mean groups 
#   by remote tolerance
#
#   This function does that and returns mean as well as standard deviation of 
#   each grouping per time step
#
#
collapseMeanRemote_firm = function(firm_dfs, stat_name){
  
  #### Input Checking ####
  
  
  
  #### Begin Function ####
  
  
  ## First, collapse to mean remote/no remote
  new_dfs = list()
  for (ii in 1:length(firm_dfs)){
    
    df_new = internal.collapseFirmMean_oneSim(firm_dfs[[ii]], stat_name)
    
    new_dfs[[ii]] = df_new
  }
  
  
  ## Get all of the time steps, Locations, and Sectors across all inputs
  time_step = sort(unique(unlist(lapply(new_dfs, function(x) x$`Time Step`))))
  locations = unique(unlist(lapply(new_dfs, function(x) x$Location)))
  sectors = unique(unlist(lapply(new_dfs, function(x) x$Sector)))
  r_allowed = unique(unlist(lapply(new_dfs, function(x) x$`Remote Allowed`)))
  
  
  ## There is no guarantee the rows will be aligned, so create new outputs individually
  xbar = NULL
  stdev = NULL
  for (ts in time_step){
    
    for (l in locations){
      
      for (s in sectors){
        
        for (r in r_allowed){
          
          tmp_all = NULL
          for (ii in 1:length(new_dfs)){
            
            idx = (new_dfs[[ii]]$`Time Step` == ts) & (new_dfs[[ii]]$Location == l) & (new_dfs[[ii]]$Sector == s) & (new_dfs[[ii]]$`Remote Allowed` == r)
            if (sum(idx) > 0){
              
              tmp_all = as.data.frame(rbind(tmp_all, new_dfs[[ii]][idx, c("Sector", "Time Step", stat_name, "Remote Allowed", "Location")]))
            }
          }
          
          if (!is.null(tmp_all)){
            
            xbar = as.data.frame(rbind(xbar, c(s, ts, mean(tmp_all[[stat_name]], na.rm = TRUE), r, l)))
            stdev = as.data.frame(rbind(stdev, c(s, ts, sd(tmp_all[[stat_name]], na.rm = TRUE), r, l)))
          }
        }
      }
    }
  }
  colnames(xbar) = c("Sector", "Time Step", stat_name, "Remote Allowed", "Location")
  colnames(stdev) = c("Sector", "Time Step", stat_name, "Remote Allowed", "Location")
  
  
  ## Make sure the numeric columns get re-cast to numeric since rbind forces matrix conversion which does not support mixed data types
  xbar[[stat_name]] = as.numeric(xbar[[stat_name]])
  xbar$`Time Step` = as.numeric(xbar$`Time Step`)
  
  stdev[[stat_name]] = as.numeric(stdev[[stat_name]])
  stdev$`Time Step` = as.numeric(stdev$`Time Step`)
  
  
  ## Package for export
  out = list("Mean" = xbar,
             "StDev" = stdev)
  
  
  return(out)
}



## Helper function for collapse Firm Mean
#
#
internal.collapseFirmMean_oneSim = function(df, stat_name){
  
  #### Check Inputs ####
  
  
  #### Begin Function ####
  
  colnames(df) = gsub("\\.", " ", colnames(df))
  df$`Remote Allowed` = "No Remote"
  fnames_remote = unique(df$`Firm Name`[df$`Remote Tolerance` > 0])        # Gets the names of the firms that eventually get remote dynamics for full pre/post shock comparison
  df$`Remote Allowed`[df$`Firm Name` %in% fnames_remote] = "Remote Allowed"
  
  
  df_stat = df[, c("Sector", "Time Step", stat_name, "Remote Allowed", "Location")]
  
  ## Get the mean of each Remote option within the time step, Location, and sector
  df_new = NULL
  for (ts in unique(df_stat$`Time Step`)){
    
    for (l in unique(df_stat$Location[df_stat$`Time Step` == ts])){
      
      for (s in unique(df_stat$Sector[(df_stat$`Time Step` == ts) & (df_stat$Location == l)])){
        
        
        ## Get Indices and add calculations, if they exist
        # idy = 
        
        
        ## Get the data of interest
        tmp_y = mean(df_stat[[stat_name]][(df_stat$`Time Step` == ts) & (df_stat$Location == l) & (df_stat$Sector == s) & (df_stat$`Remote Allowed` == "Remote Allowed")], na.rm = TRUE)
        tmp_n = mean(df_stat[[stat_name]][(df_stat$`Time Step` == ts) & (df_stat$Location == l) & (df_stat$Sector == s) & (df_stat$`Remote Allowed` == "No Remote")], na.rm = TRUE)
        
        
        ## Perform the appropriate calculation
        df_new = as.data.frame(rbind(df_new, c(s, ts, tmp_y, "Remote Allowed", l)))
        df_new = as.data.frame(rbind(df_new, c(s, ts, tmp_n, "No Remote", l)))
      }
    }
  }
  colnames(df_new) = colnames(df_stat)
  df_new[[stat_name]] = as.numeric(df_new[[stat_name]])
  df_new$`Time Step` = as.numeric(df_new$`Time Step`)
  
  ## Set NaN values as NAs...just in case?
  # df_new[is.nan(df_new)] = NA
  
  
  return(df_new)
}






## Get Remote Mean values for all statistics of interest
#
#   For firms, in order to effectively visualize policy choice of remote allowed
#   via remote or not, need to collapse the raw firm values into mean groups 
#   by remote tolerance
#
#   This function does that and returns mean as well as standard deviation of 
#   each grouping per time step
#
#
collapseMeanRemote_firm_all = function(firm_dfs){
  
  #### Input Checking ####
  
  
  
  #### Begin Function ####
  
  
  ## First, collapse to mean remote/no remote
  new_dfs = list()
  for (ii in 1:length(firm_dfs)){
    
    df_new = internal.collapseFirmMean_oneSim_all(firm_dfs[[ii]])
    
    new_dfs[[ii]] = df_new
  }
  
  
  stat_int = c("Total Workers", "Remote Tolerance", "Profit", "Hiring Quota", "Wage", "Location Mean Wage",
               "Remote Workers", "Preferred Location", "Co Located", "Node Triangles", "Node Degree", "Node V Shapes",
               "Clustering", "Degree Centrality", "Eigenvector Centrality", "Betweenness Centrality", "VoteRank")
  
  
  ## Get all of the time steps, Locations, and Sectors across all inputs
  time_step = sort(unique(unlist(lapply(new_dfs, function(x) x$`Time Step`))))
  locations = unique(unlist(lapply(new_dfs, function(x) x$Location)))
  sectors = unique(unlist(lapply(new_dfs, function(x) x$Sector)))
  r_allowed = unique(unlist(lapply(new_dfs, function(x) x$`Remote Allowed`)))
  
  
  ## There is no guarantee the rows will be aligned, so create new outputs individually
  xbar = NULL
  stdev = NULL
  for (ts in time_step){
    
    for (l in locations){
      
      for (s in sectors){
        
        for (r in r_allowed){
          
          tmp_all = NULL
          for (ii in 1:length(new_dfs)){
            
            idx = (new_dfs[[ii]]$`Time Step` == ts) & (new_dfs[[ii]]$Location == l) & (new_dfs[[ii]]$Sector == s) & (new_dfs[[ii]]$`Remote Allowed` == r)
            if (sum(idx) > 0){
              
              tmp_all = as.data.frame(rbind(tmp_all, new_dfs[[ii]][idx, c("Sector", "Time Step", stat_int, "Remote Allowed", "Location")]))
            }
          }
          
          ## Need to fix the columns that get converted to character...
          if (!is.null(tmp_all)){
            
            ## Update teh columns converted to a character
            tmp_all[, stat_int] = apply(tmp_all[, stat_int, drop = FALSE], 2, as.numeric)
            
            
            ## Do the calculations
            xbar = as.data.frame(rbind(xbar, c(s, ts, colMeans(tmp_all[, stat_int, drop = FALSE], na.rm = TRUE), r, l)))
            stdev = as.data.frame(rbind(stdev, c(s, ts, apply(tmp_all[, stat_int, drop = FALSE], 2, sd, na.rm = TRUE), r, l)))
          }
        }
      }
    }
  }
  colnames(xbar) = c("Sector", "Time Step", stat_int, "Remote Allowed", "Location")
  colnames(stdev) = c("Sector", "Time Step", stat_int, "Remote Allowed", "Location")
  
  
  ## Make sure the numeric columns get re-cast to numeric since rbind forces matrix conversion which does not support mixed data types
  to_num = c(stat_int, "Time Step")
  xbar[, to_num] = apply(xbar[, to_num], 2, as.numeric)
  stdev[, to_num] = apply(stdev[, to_num], 2, as.numeric)
  
  
  ## Package for export
  out = list("Mean" = xbar,
             "StDev" = stdev)
  
  
  return(out)
}






## Helper function to collapse all Firm Mean metrics
#
#
#   Note that a similar thing could be done by Sector, Location, etc 
#   (though most of that is already handled by the location summary metrics)
#
#
internal.collapseFirmMean_oneSim_all = function(df){
  
  #### Input Checking ####
  
  
  
  #### Begin Function ####
  
  colnames(df) = gsub("\\.", " ", colnames(df))
  df$`Remote Allowed` = "No Remote"
  fnames_remote = unique(df$`Firm Name`[df$`Remote Tolerance` > 0])        # Gets the names of the firms that eventually get remote dynamics for full pre/post shock comparison
  df$`Remote Allowed`[df$`Firm Name` %in% fnames_remote] = "Remote Allowed"
  
  
  ## Define the statistics of interest
  stat_int = c("Total Workers", "Remote Tolerance", "Profit", "Hiring Quota", "Wage", "Location Mean Wage",
               "Remote Workers", "Preferred Location", "Co Located", "Node Triangles", "Node Degree", "Node V Shapes",
               "Clustering", "Degree Centrality", "Eigenvector Centrality", "Betweenness Centrality", "VoteRank")
  
  
  ## Get the mean of each Remote option within the time step, Location, and sector
  df_new = NULL
  for (ts in unique(df$`Time Step`)){
    
    for (l in unique(df$Location[df$`Time Step` == ts])){
      
      for (s in unique(df$Sector[(df$`Time Step` == ts) & (df$Location == l)])){
        
        
        ## Get Indices and add calculations, if they exist
        idy = (df$`Time Step` == ts) & (df$Location == l) & (df$Sector == s) & (df$`Remote Allowed` == "Remote Allowed")
        if (sum(idy) > 0){
          
          tmp_y = colMeans(df[idy, stat_int, drop = FALSE], na.rm = TRUE)
          df_new = as.data.frame(rbind(df_new, c(s, ts, tmp_y, "Remote Allowed", l)))
        }
        
        idn = (df$`Time Step` == ts) & (df$Location == l) & (df$Sector == s) & (df$`Remote Allowed` == "No Remote")
        if (sum(idn) > 0){
          
          tmp_n = colMeans(df[idn, stat_int, drop = FALSE], na.rm = TRUE)
          df_new = as.data.frame(rbind(df_new, c(s, ts, tmp_n, "No Remote", l)))
        }
      }
    }
  }
  ## Update the column names
  colnames(df_new) = c("Sector", "Time Step", stat_int, "Remote Allowed", "Location")
  df[, stat_int] = apply(df[, stat_int], 2, as.numeric)
  
  
  return(df_new)
}




