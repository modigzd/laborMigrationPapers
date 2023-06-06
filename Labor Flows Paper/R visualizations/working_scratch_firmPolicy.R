
## Create some initial visualizations for the firm policy section


## Temp script for inspecting some base results
library(ggplot2)


## Note - the mean wage should possibly be by firm and not total mean (or maybe median)
##        this would tell us if a single firm is skewing the results, which seems likely



## Load visualization functions
lpath = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/R visualizations"
source(paste(lpath, "fxns_firm.R", sep = "/"))
source(paste(lpath, "fxns_general.R", sep = "/"))
source(paste(lpath, "fxns_labor.R", sep = "/"))
source(paste(lpath, "fxns_network_smry.R", sep = "/"))



## Point to the correct location and get the data
base_path = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/Results"
run_path = "3_1_WFH_extension/3_1_3_firm_policy/Case_1"   # s = inf
# run_path = "3_1_WFH_extension/3_1_3_firm_policy/Case_1" # s = 0


fpath = paste(base_path, run_path, sep = "/")

## Get Firm Data
df_firms = getFirmData(fpath)

## Get Location Data
loc_df = getLocationData(fpath)

loc_mean = loc_df$Mean
loc_sd = loc_df$Std



## Split Firm results into two sets
df_WFH = list()
df_noWFH = list()
df_large = list()
for (ii in 1:length(df_firms$Raw)){
  
  ## Get the WFH firms...
  tmp = data.frame(df_firms$Raw[[ii]])
  fids_wfh = unique(tmp$Firm.Name[tmp$Remote.Tolerance>0])
  idx = tmp$Firm.Name %in% fids_wfh
  
  
  ## Get the biggest 10 prior to the shock
  maxp = sort(tmp$Profit[tmp$Time.Step==200], decreasing = TRUE)[1:10]
  fidsL = tmp$Firm.Name[(tmp$Profit %in% maxp) & (tmp$Time.Step==200)]
  idL = tmp$Firm.Name %in% fidsL
  
  
  df_WFH[[ii]] = df_firms$Raw[[ii]][idx, ]
  df_noWFH[[ii]] = df_firms$Raw[[ii]][!(idx | idL), ]
  df_large[[ii]] = df_firms$Raw[[ii]][idL, ]
  
  
  ## Set all values that are numeric as numeric...since apparently R and python are having a disagreement
  names_ignore = c("X", "Location", "Sector", "Firm.Name")
  cnames_to_change = colnames(df_WFH[[1]])
  cnames_to_change = cnames_to_change[!(cnames_to_change %in% names_ignore)]
  for (cn in cnames_to_change){
    
    df_WFH[[ii]][, cn] = as.numeric(df_WFH[[ii]][, cn])
    df_noWFH[[ii]][, cn] = as.numeric(df_noWFH[[ii]][, cn])
    df_large[[ii]][, cn] = as.numeric(df_large[[ii]][, cn])
  }
}



## Construct a table pre v post shock, A, B, Total, and WFH vs non-WFH averages (split this into biggest 10 as well)

## First, create a function that gets a data.frame of the mean firm sizes by time step
xbar = list()
stdev = list()



## Wrap the internal.getMeanFirmStats function to get mean/standard deviation by simulation 


getMeanFirmStats = function(df_list, firm_type_label){
  
  ## First pass mean
  df_list = lapply(df_list, internal.getMeanFirmStats, firm_label = firm_type_label)
  
  cnames_ignore = c("Location", "Sector", "Firm Name")
  cnames_mean = colnames(df_list[[1]])
  cnames_mean = cnames_mean[!(cnames_mean %in% cnames_ignore)]
  
  ## Now, get xbar and mean by time step
  xbar = list()
  stdev = list()
  cnt = 1
  for (loc in unique(df_list[[1]]$Location)){
    
    tmp_mean = NULL
    tmp_stdev = NULL
    for (ts in unique(df_list[[1]]$`Time Step`)){
      
      tmp = do.call('rbind', lapply(df_list, function(x) x[(x$`Time Step` == ts) & (x$Location == loc), ]))
      
      tmp_mean = cbind(tmp_mean, apply(tmp[, cnames_mean], 2, mean, na.rm = TRUE))
      tmp_stdev = cbind(tmp_stdev, apply(tmp[, cnames_mean], 2, sd, na.rm = TRUE))
    }
    
    tmp_mean = data.frame(tmp_mean)
    tmp_stdev = data.frame((tmp_stdev))
    
    tmp_mean$Location = loc
    tmp_stdev$Location = loc
    
    tmp_mean$Sector = "Sector 1"
    tmp_stdev$Sector = "Sector 1"
    
    tmp_mean$`Firm Name` = firm_type_label
    tmp_stdev$`Firm Name` = firm_type_label
    
    xbar[[cnt]] = tmp_mean
    stdev[[cnt]] = tmp_stdev
    
    cnt = cnt + 1
  }
  
  xbar = do.call('rbind', xbar)
  stdev = do.call('rbind', stdev)
  
  
  out = list("xbar" = xbar, 
             "stdev" = stdev)
  
  
  return(out)
}



## Get the Mean Stats for a single Raw Firm element
internal.getMeanFirmStats = function(df, firm_label){
  
  names_ignore = c("X", "Location", "Sector", "Firm.Name")
  names_mean = colnames(df)
  names_mean = names_mean[!(names_mean %in% names_ignore)]
  
  
  all_time_steps = unique(df$Time.Step)
  outA = NULL
  outB = NULL
  outT = NULL
  cnt = 1
  n_firmsA = rep(NA, length(all_time_steps))
  n_firmsB = n_firmsA
  n_firmsT = n_firmsA
  cnt = 1
  for (ii in 1:length(all_time_steps)){
    
    tmp = data.frame(df[df$Time.Step == all_time_steps[ii], ])
    idA = tmp$Location == "Location A"
    idB = tmp$Location == "Location B"
    
    outA = rbind(outA, apply(tmp[idA, names_mean], 2, mean, na.rm = TRUE))
    outB = rbind(outB, apply(tmp[idB, names_mean], 2, mean, na.rm = TRUE))
    outT = rbind(outT, apply(tmp[, names_mean], 2, mean, na.rm = TRUE))
    
    ## Capture the number of firms...
    n_firmsA[cnt] = sum(idA)
    n_firmsB[cnt] = sum(idB)
    n_firmsT[cnt] = sum(idA | idB)
    
    cnt = cnt + 1
  }
  
  outA = data.frame(outA)
  outB = data.frame(outB)
  outT = data.frame(outT)
  
  outA$Location = "Location A"
  outB$Location = "Location B"
  outT$Location = "Total"
  
  outA$`N Firms` = n_firmsA
  outB$`N Firms` = n_firmsB
  outT$`N Firms` = n_firmsT
  
  out = do.call('rbind', list(outA, outB, outT))
  
  out$Sector = "Sector 1"
  out$Firm.Name =firm_label
  
  out = out[order(out$Time.Step), ]
  rownames(out) = NULL
  
  
  ## Clean up some column names
  colnames(out) = gsub("\\.", " ", colnames(out))
  
  ## Get rows with NA and set everything except Time Step and the rest of the ignore names as 0
  idNA = rowSums(is.na(out)) > 0
  # if (any(idNA)){
  #   
  #   names_zero
  # }
  
  ## Actually, for now, just remove them...
  out = out[!idNA, ]
  
  
  return(out)
}




### This all needs to be fixed above...


noWFH_mean = getMeanFirmStats(df_noWFH, "No WFH")
WFH_mean = getMeanFirmStats(df_noWFH, "WFH")
noWFHLarge_mean = getMeanFirmStats(df_noWFH, "No WFH - Large")

## These need a t() and also they have xbar and stdev elements


## Then a cowplot...







