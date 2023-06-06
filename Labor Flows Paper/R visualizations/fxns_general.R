################################################################################
#                                                                              #
#                                                                              #
#   Functions for creating visualizations specific to creating general         #
#   intermediate results, such as simulation aggregations                      #
#                                                                              #
#   Zach Modig                                                                 #
#   November 2021                                                              #
#                                                                              # 
################################################################################



## Eventually I'll write results to SQL. Until then, reading from saved csv outputs


## Read in location data and create mean, median, and stdev data.frames
#
#
#
getLocationData = function(fpath # File path to the results of interest
                           ){
  
  #### Input Checking ####
  
  
  #### Begin Function ####
  
  loc = list()
  fnames = list.files(fpath)
  n_runs = suppressWarnings(max(as.numeric(gsub(".*results_([0-9]+)\\.csv", "\\1", fnames)), na.rm = TRUE))
  for (ii in 0:n_runs){
    
    ## Read in each file to a list element
    loc[[ii + 1]] = read.csv(paste(fpath, paste0("location_results_", ii, ".csv"), sep = "/"))
  }
  
  
  ## Create data.frames with mean, median, and standard deviation results where applicable
  loc_mean = data.frame(matrix(data = NA, nrow = nrow(loc[[1]]), ncol = ncol(loc[[1]])))
  colnames(loc_mean) = colnames(loc[[1]])
  loc_median = loc_mean
  loc_std = loc_mean
  cnames = colnames(loc_mean)
  for (cn in cnames){
    
    
    tmp = do.call('cbind', lapply(loc, function(x) x[, cn, drop=FALSE]))
    
    if (cn %in% c("Name", "Sector")){
      
      nm = apply(tmp, 1, unique) # Gets the unique elements in each row
      
      loc_mean[, cn] = nm        
      loc_std[, cn] = nm
      loc_mean[, cn] = nm
      
    } else {
      
      loc_mean[, cn] = apply(tmp, 1, mean)
      loc_std[, cn] = apply(tmp, 1, sd)
      loc_median[, cn] = apply(tmp, 1, median)
    }
  }
  
  
  ## Finally, remove the "X" column and make the names pretty
  loc_mean = loc_mean[, 2:ncol(loc_mean)]
  loc_std = loc_std[, 2:ncol(loc_std)]
  loc_median = loc_median[, 2:ncol(loc_median)]
  cnames = gsub("\\.", " ", cnames[2:length(cnames)])
  colnames(loc_mean) = cnames
  colnames(loc_std) = cnames
  colnames(loc_mean) = cnames
  
  
  ## Add a column to store total workers by sector and total workers in the model
  loc_mean$`Total Workers Sector` = NA
  loc_mean$`Total Workers Model` = NA
  loc_std$`Total Workers Sector` = NA
  loc_std$`Total Workers Model` = NA
  loc_median$`Total Workers Sector` = NA
  loc_median$`Total Workers Model` = NA
  
  
  ## Get the total number of workers in each sector at time step 1, and add that 
  ## value to a column for easy "percentage" calculations
  tmp =  loc_mean[loc_mean[, "Time Step"] == 0, ]
  s_names = unique(tmp$Sector)
  tot_labor = rep(NA, length(s_names))
  names(tot_labor) = s_names
  for (s in s_names){
    
    tot_labor[s] = sum(tmp$`N workers`[tmp$Sector == s])
    
    ## Add the sector totals to the outputs
    loc_mean$`Total Workers Sector`[loc_mean$Sector == s] = tot_labor[s]
    loc_std$`Total Workers Sector`[loc_std$Sector == s] = tot_labor[s]
    loc_median$`Total Workers Sector`[loc_median$Sector == s] = tot_labor[s]
  }
  
  
  ## And finally, the total workers in the model
  loc_mean$`Total Workers Model` = sum(tot_labor)
  loc_std$`Total Workers Model` = sum(tot_labor)
  loc_median$`Total Workers Model` = sum(tot_labor)
  
  
  out = list("Mean" = loc_mean,
             "Std" = loc_std,
             "Median" = loc_median)
  
  
  return(out)
}



## Read in firm data and create mean, median, and stdev data.frames
#
#
#   Note that this will be useful for some things, but perhaps not others
#   Therefore, also return the list of all of the individual data
#
#
getFirmData = function(fpath # File path to the results of interest
                       ){
  
  #### Input Checking ####
  
  
  #### Begin Function ####
  
  firm = list()
  fnames = list.files(fpath)
  n_runs = suppressWarnings(max(as.numeric(gsub(".*results_([0-9]+)\\.csv", "\\1", fnames)), na.rm = TRUE))
  for (ii in 0:n_runs){
    
    ## Read in each file to a list element
    firm[[ii + 1]] = read.csv(paste(fpath, paste0("firm_results_", ii, ".csv"), sep = "/"))
  }
  
  
  ## Create mean/median/stdev for each sector/location/time step
  sectors = unique(firm[[1]]$Sector)
  locations = unique(firm[[1]]$Location)
  time_steps = unique(firm[[1]]$Time.Step)
  n_comb = length(sectors) * length(locations) * length(time_steps) # Total number of combinatorics
  
  ## Instantiate an output data.frame
  df_mean = data.frame(matrix(data = NA, nrow = n_comb, ncol = ncol(firm[[1]])))
  cnames = colnames(firm[[1]])
  colnames(df_mean) = cnames
  df_std = df_mean
  df_median = df_mean
  
  
  ## For each location/sector/time step, get the appropriate firm moments
  rn = 1
  for (ts in time_steps){
    for (loc in locations){
      for (sector in sectors){
        
        
        tmp = do.call('rbind', lapply(firm, function(x) x[x$Location == loc & x$Sector == sector & x$Time.Step == ts, , drop=FALSE]))
        df_mean[rn, ] = lapply(tmp, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else -999)
        df_std[rn, ] = lapply(tmp, function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else -999)
        df_median[rn, ] = lapply(tmp, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else -999)
        
        df_mean[rn, c("Sector", "Location")] = c(sector, loc)
        df_std[rn, c("Sector", "Location")] = c(sector, loc)
        df_median[rn, c("Sector", "Location")] = c(sector, loc)
        
        ## Go to the next row
        rn = rn + 1
      }
    }
  }
  
  
  
  ## Finally, remove the "X" column and make the names pretty
  df_mean = df_mean[, 2:ncol(df_mean)]
  df_std = df_std[, 2:ncol(df_std)]
  df_median = df_median[, 2:ncol(df_median)]
  cnames = gsub("\\.", " ", cnames[2:length(cnames)])
  colnames(df_mean) = cnames
  colnames(df_std) = cnames
  colnames(df_mean) = cnames
  
  
  out = list("Raw" = firm,
             "Mean" = df_mean,
             "Std" = df_std,
             "Median" = df_median)
  
  return(out)
}





## Read in summary Network Results
#
#
#
#
getNetworkSummaryData = function(fpath
                          ){
  
  #### Input Checking ####
  
  
  #### Begin Function ####
  
  net = list()
  fnames = list.files(fpath)
  n_runs = suppressWarnings(max(as.numeric(gsub(".*results_([0-9]+)\\.csv", "\\1", fnames)), na.rm = TRUE))
  for (ii in 0:n_runs){
    
    ## Read in each file to a list element
    net[[ii + 1]] = read.csv(paste(fpath, paste0("network_results_", ii, ".csv"), sep = "/"))
  }
  
  
  ## Create data.frames with mean, median, and standard deviation results where applicable
  net_mean = data.frame(matrix(data = NA, nrow = nrow(net[[1]]), ncol = ncol(net[[1]])))
  colnames(net_mean) = colnames(net[[1]])
  net_median = net_mean
  net_std = net_mean
  cnames = colnames(net_mean)
  for (cn in cnames){
    
    
    tmp = do.call('cbind', lapply(net, function(x) x[, cn, drop=FALSE]))
    
    if (cn %in% c("Name", "Sector")){
      
      nm = apply(tmp, 1, unique) # Gets the unique elements in each row
      
      net_mean[, cn] = nm        
      net_std[, cn] = nm
      net_mean[, cn] = nm
      
    } else {
      
      net_mean[, cn] = apply(tmp, 1, mean)
      net_std[, cn] = apply(tmp, 1, sd)
      net_median[, cn] = apply(tmp, 1, median)
    }
  }
  
  
  ## Finally, remove the "X" column and make the names pretty
  net_mean = net_mean[, 2:ncol(net_mean)]
  net_std = net_std[, 2:ncol(net_std)]
  net_median = net_median[, 2:ncol(net_median)]
  cnames = gsub("\\.", " ", cnames[2:length(cnames)])
  colnames(net_mean) = cnames
  colnames(net_std) = cnames
  colnames(net_mean) = cnames
  
  
  out = list("Mean" = net_mean,
             "Std" = net_std,
             "Median" = net_median)
  
  
  return(out)
}






## Get WFH Comparisons per location and sector
#
#   Returns a list of tables: Location_Sector
#
#   Only a single time step is accepted   
#
#
getWFHImpactTable = function(df_dock, 
                             df_wfh, 
                             time_step = NULL # If Null, this will default to the last time step
                             ){
  
  #### Input Checking ####
  
  if (is.null(time_step)){
    
    time_step = max(df_dock$`Time Step`)
  }
  
  
  
  #### Begin Function ####
  
  ## Subset data
  df_dock = df_dock[df_dock$`Time Step` == time_step, ]
  df_wfh = df_wfh[df_wfh$`Time Step` == time_step, ]
  
  
  locs = unique(df_dock$Name)
  sectors = unique(df_dock$Sector)
  
  metrics = c("Employment Rate", "Employed", "Total", "Real Wage", "Nominal Wage", "Employed Real Wage", "Employed Nominal Wage", "Remote Workers", "Preferred Location", "Preferred Location Rate", "Co-Located Rate",
              "Firm Real Wage", "Firm Nominal Wage")
  init_NA = rep(NA, length(metrics))
  
  
  tbls = list()
  for (loc in locs){
    for (sector in sectors){
      
      tbl = data.frame("Metric" = metrics, "Docked" = init_NA, "WFH" = init_NA, "WFH_Docked_Ratio" = init_NA)
      
      idd = df_dock$Name == loc & df_dock$Sector == sector
      idw = df_wfh$Name == loc & df_wfh$Sector == sector
      
      tbl[1, c("Docked", "WFH")] = c(df_dock$`N Employed`[idd] / df_dock$`N workers`[idd], df_wfh$`N Employed`[idw] / df_wfh$`N workers`[idw])
      tbl[2, c("Docked", "WFH")] = c(df_dock$`N Employed`[idd], df_wfh$`N Employed`[idw])
      tbl[3, c("Docked", "WFH")] = c(df_dock$`N workers`[idd], df_wfh$`N workers`[idw])
      tbl[4, c("Docked", "WFH")] = c(df_dock$`mean real wage`[idd], df_wfh$`mean real wage`[idw])
      tbl[5, c("Docked", "WFH")] = c(df_dock$`mean wage`[idd], df_wfh$`mean wage`[idw])
      tbl[6, c("Docked", "WFH")] = c(df_dock$`employed mean real wage`[idd], df_wfh$`employed mean real wage`[idw])
      tbl[7, c("Docked", "WFH")] = c(df_dock$`employed mean wage`[idd], df_wfh$`employed mean wage`[idw])
      tbl[8, c("Docked", "WFH")] = c(df_dock$`remote workers`[idd], df_wfh$`remote workers`[idw])
      tbl[9, c("Docked", "WFH")] = c(df_dock$`preferred location`[idd], df_wfh$`preferred location`[idw])
      tbl[10, c("Docked", "WFH")] = c(df_dock$`preferred location`[idd] / df_dock$`N workers`[idd], df_wfh$`preferred location`[idw] / df_wfh$`N workers`[idw])
      tbl[11, c("Docked", "WFH")] = c(df_dock$`Firm Mean Co Located`[idd] / df_dock$`N workers`[idd], df_wfh$`Firm Mean Co Located`[idw] / df_wfh$`N workers`[idw])
      tbl[12, c("Docked", "WFH")] = c(df_dock$`Firm Mean Wage`[idd] / df_dock$rent[idd], df_wfh$`Firm Mean Wage`[idw] / df_wfh$rent[idw])
      tbl[13, c("Docked", "WFH")] = c(df_dock$`Firm Mean Wage`[idd], df_wfh$`Firm Mean Wage`[idw])
      
      tbl[, "WFH_Docked_Ratio"] = tbl[, "WFH"] / tbl[, "Docked"]
      
      tbls[[paste(loc, sector, sep = "_")]] = tbl
    }
  }
  
  
  return(tbls)
}





