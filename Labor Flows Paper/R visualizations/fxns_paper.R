## Functions for generating all plots and tables that could be used within the
## Paper across simulations



## Create All Final plots 
#
#   Creates the final set of plots for a given WFH/Docked comparison
#
#
createFinalPlots = function(df_loc_docked, # Saved output containing mean/standard deviations of docked data
                            df_loc_wfh     # Saved output containing mean/standard deviations of corresponding wfh data
                            ){
  
  #### Input Checking ####
  
  
  
  
  #### Begin Function ####
  
  ## Sub-function ##
  ## Creates the standardized plot for calling in a loop, below
  subfxn_Plot = function(var_name,        # Variable to be plotted
                         df_mean,         # Data.frame containing mean values
                         df_sd    = NULL, # Data.frame containing sd values 
                         y_lab    = NULL  # Optional label for the y axis
                         ){
    
    
    df_plt = df_mean[, c(var_name, "Time Step", "model", "Name", "Sector")]
    df_plt$ribbon = factor(paste(df_plt$Name, df_plt$Sector))
    
    colnames(df_plt) = c(var_name, "Time Step", "model", "Location", "Sector", "ribbon")
    
    
    if (!is.null(df_sd)){
      
      df_plt$var_plus = df_plt[, var_name] + df_sd[, var_name]
      df_plt$var_minus = df_plt[, var_name] - df_sd[, var_name]
    }
    
    
    plt = ggplot(df_plt, aes(x = `Time Step`, y = !!rlang::sym(var_name), color = Location)) +
            geom_line(aes(linetype = Sector)) + 
            facet_grid(model ~ .) +
            theme_minimal_hgrid() + 
            geom_vline(xintercept = 200, linetype = "dashed", alpha = .3) +
            xlab(NULL) +
            theme(axis.text.x = element_blank()) +
            scale_color_manual(values = c("firebrick", "steelblue"))
    
    if (!is.null(y_lab)){
      
      plt = plt + ylab(y_lab)
    }
    
    if (!is.null(df_sd)){
      
      plt = plt + 
              geom_ribbon(aes(ymin = var_minus, ymax = var_plus, fill = ribbon), alpha = .2, color = NA, show.legend = FALSE) +
              scale_fill_manual(values =  rep("#808080", length(unique(df_plt$ribbon)))) 
        
    }
    
    
    return(plt)
  }
  
  
  ## Get data and appropriately append it
  loc_mean_docked = df_loc_docked$Mean
  loc_mean_docked$model = "Docked"
  
  loc_sd_docked = df_loc_docked$Std
  loc_sd_docked$model = "Docked"
  
  loc_mean_wfh = df_loc_wfh$Mean
  loc_mean_wfh$model = "WFH"
  
  loc_sd_wfh = df_loc_wfh$Std
  loc_sd_wfh$model = "WFH"
  
  
  ## Store the output as one big data frame
  loc_mean = rbind(loc_mean_docked, loc_mean_wfh)
  loc_sd = rbind(loc_sd_docked, loc_sd_wfh)
  
  ## And add the employment rate
  loc_mean$`employment rate` = loc_mean$`N Employed` / loc_mean$`N workers`
  loc_sd$`employment rate` = loc_sd$`N Employed` / loc_sd$`N workers`
  
  
  all_vars = c("rent", "Total Firms", "Firm Mean Profit", "mean wage", "mean real wage", "employed mean wage", "employed mean real wage", "employment rate", "remote workers")
  y_labs = c("Rent", "Total Firms", "Mean Profit", "Wage", "Wage", "Wage", "Wage", "Employment Rate", "Remote Workers")
  out = list()
  for (ii in 1:length(all_vars)){
    
    if (all_vars[ii] == "employment rate"){
      
      tmp = subfxn_Plot(all_vars[ii], loc_mean, df_sd = NULL, y_lab = y_labs[ii])
      
    } else {
      
      tmp = subfxn_Plot(all_vars[ii], loc_mean, loc_sd, y_labs[ii])
    }
    
    out[[all_vars[ii]]] = tmp
  }
  
  
  return(out)
}





## Creates pre to post wage ratio tables
#
#
getPre2PostWageRatio = function(df,        # Data frame of interest (usually from getLocationData())
                                colname,   # Wage column of interest
                                new_name,  # What should the new column be called?
                                time_comp  # Time step to grab the wage comparison (should be last time before shock)
                                ){
  
  #### Input Checking ####
  
  
  #### Begin Function ####
  
  ## Note - can't divide standard deviations, so this is only calculated for the mean
  
  
  
  ## Get the ratio as new wage over the comp wage on a per location and per sector basis
  ## This gives as an appropriate looking gain/lost relative to the pre-shock equilibrium wage
  locs = unique(df$Mean$Name)
  sectors = unique(df$Mean$Sector)
  for (loc in locs){
    for (sector in sectors){
      
      idx = (df$Mean$Name == loc) & (df$Mean$Sector == sector)
      
      ## Get the vector of appropriate wages for the indices of interest
      tmp = df$Mean[[colname]][idx]
      comp_val = tmp[time_comp]
      
      tmp[1:time_comp] = 1
      id_shock = (time_comp + 1):length(tmp)
      tmp[id_shock] = comp_val / tmp[id_shock]
      
      df$Mean[idx, new_name] = tmp
    }
  }
  
  
  return(df)
}




## Creates Firm plots
#
#
createFirmPlots = function(loc_mean_docked, 
                           loc_sd_docked,
                           loc_mean_wfh,
                           loc_sd_wfh
                           ){
  
  ## Mean Workers Plot
  plt1 = plotStatistic_location("Firm Mean Workers", loc_mean_docked, loc_sd_docked) + ylab("Docked")
  plt2 = plotStatistic_location("Firm Mean Workers", loc_mean_wfh, loc_sd_wfh) + ylab("WFH")
  
  ttl = ggdraw() + draw_label("Mean Workers per Firm", x = .15, size = 12) + theme_minimal()
  
  plt_mean_workers = plot_grid(ttl,
                               plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                               ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  
  ## Mean Profit Plot
  plt1 = plotStatistic_location("Firm Mean Profit", loc_mean_docked, loc_sd_docked)
  plt2 = plotStatistic_location("Firm Mean Profit", loc_mean_wfh, loc_sd_wfh)
  
  ttl = ggdraw() + draw_label("Mean Profit per Firm", x = .15, size = 12) + theme_minimal()
  
  plt_mean_profit = plot_grid(ttl,
                              plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                              ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  
  ## Mean Unfulfilled Hiring Quota
  plt1 = plotStatistic_location("Firm Mean Hiring Quota unfulfilled", loc_mean_docked, loc_sd_docked)
  plt2 = plotStatistic_location("Firm Mean Hiring Quota unfulfilled", loc_mean_wfh, loc_sd_wfh)
  
  ttl = ggdraw() + draw_label("Mean Unfulfilled Hiring Quota", x = .15, size = 12) + theme_minimal()
  
  plt_hiring_quota = plot_grid(ttl,
                               plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                               ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  
  ## This is only the Firm Wage average
  plt1 = plotStatistic_location("Firm Mean Wage", loc_mean_docked, loc_sd_docked) + ggtitle("Docked")
  plt2 = plotStatistic_location("Firm Mean Wage", loc_mean_wfh, loc_sd_wfh) + ggtitle("WFH")
  
  ttl = ggdraw() + draw_label("Firm Mean Nominal Wage", x = .15, size = 12) + theme_minimal()
  
  plt_nom_wage_firms = plot_grid(ttl,
                                 plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                                 ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  ## Mean Real Wage 
  ## Note - this should probably be plotted as a percent difference due to the rent calculation being crazy different with higher values of k
  
  loc_mean_docked$`Mean Real Wage` = loc_mean_docked$`Firm Mean Real Wage`
  loc_sd_docked$`Mean Real Wage` = loc_sd_docked$`Firm Mean Real Wage`
  
  # loc_mean_wfh$`Mean Real Wage` = loc_mean_wfh$`Firm Mean Wage` / loc_mean_wfh$rent
  # loc_sd_wfh$`Mean Real Wage` = loc_sd_wfh$`Firm Mean Wage` / loc_sd_wfh$rent
  loc_mean_wfh$`Mean Real Wage` = loc_mean_wfh$`Firm Mean Real Wage`
  loc_sd_wfh$`Mean Real Wage` = loc_sd_wfh$`Firm Mean Real Wage` 
  
  
  ttl = ggdraw() + draw_label("Firm Mean Real Wage", x = .15, size = 12) + theme_minimal()
  
  plt1 = plotStatistic_location("Mean Real Wage", loc_mean_docked, loc_sd_docked) + ggtitle("Docked") # Don't show the bands here because the SD really throws off the scale
  plt2 = plotStatistic_location("Mean Real Wage", loc_mean_wfh, loc_sd_wfh) + ggtitle("WFH")
  
  plt_real_wage_firms = plot_grid(ttl,
                                  plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                                  ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  
  out = list("Mean Workers" = plt_mean_workers,
             "Mean Profit" = plt_mean_profit,
             "Hiring Quota" = plt_hiring_quota,
             "Nominal Wage" = plt_nom_wage_firms,
             "Real Wage" = plt_real_wage_firms
             )
  
  return(out)
}



## Create Labor/Location plots
#
#
createLaborPlots = function(loc_mean_docked, 
                            loc_sd_docked,
                            loc_mean_wfh,
                            loc_sd_wfh
                            ){
  
  ## Remote Workers
  plt1 = plotStatistic_location("remote workers", loc_mean_docked, loc_sd_docked) + ggtitle("Docked")
  plt2 = plotStatistic_location("remote workers", loc_mean_wfh, loc_sd_wfh) + ggtitle("WFH")
  
  ttl = ggdraw() + draw_label("Remote Workers", x = .15, size = 12) + theme_minimal()
  
  plt_remote_workers = plot_grid(ttl,
                                 plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                                 ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  
  ## This is includes unemployed...
  ## Mean Nominal Wage
  plt1 = plotStatistic_location("mean wage", loc_mean_docked, loc_sd_docked) + ggtitle("Docked")
  plt2 = plotStatistic_location("mean wage", loc_mean_wfh, loc_sd_wfh) + ggtitle("WFH")
  
  ttl = ggdraw() + draw_label("Nominal Wage", x = .15, size = 12) + theme_minimal()
  
  plt_nom_wage_workers_wUnemp = plot_grid(ttl,
                                          plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                                          ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  ## Mean Real Wage 
  ## Note - this should probably be plotted as a percent difference due to the rent calculation being crazy different with higher values of k
  # loc_mean_docked$`Mean Real Wage wUnemp` = loc_mean_docked$`mean wage` / loc_mean_docked$rent
  # loc_sd_docked$`Mean Real Wage wUnemp` = loc_sd_docked$`mean wage` / loc_sd_docked$rent
  loc_mean_docked$`Mean Real Wage wUnemp` = loc_mean_docked$`mean real wage` 
  loc_sd_docked$`Mean Real Wage wUnemp` = loc_sd_docked$`mean real wage`
  
  # loc_mean_wfh$`Mean Real Wage wUnemp` = loc_mean_wfh$`mean wage` / loc_mean_wfh$rent
  # loc_sd_wfh$`Mean Real Wage wUnemp` = loc_sd_wfh$`mean wage` / loc_sd_wfh$rent
  loc_mean_wfh$`Mean Real Wage wUnemp` = loc_mean_wfh$`mean real wage` 
  loc_sd_wfh$`Mean Real Wage wUnemp` = loc_sd_wfh$`mean real wage` 
  
  ttl = ggdraw() + draw_label("Real Wage", x = .15, size = 12) + theme_minimal()
  
  # plt1 = plotStatistic_location("Mean Real Wage", loc_mean_docked, loc_sd_docked) + ggtitle("Docked")
  # plt2 = plotStatistic_location("Mean Real Wage", loc_mean_wfh, loc_sd_wfh) + ggtitle("Docked")
  plt1 = plotStatistic_location("Mean Real Wage wUnemp", loc_mean_docked, loc_sd_docked) + ggtitle("Docked") # Don't show the bands here because the SD really throws off the scale
  plt2 = plotStatistic_location("Mean Real Wage wUnemp", loc_mean_wfh, loc_sd_wfh) + ggtitle("WFH")
  
  plt_real_wage_workers_wUnemp = plot_grid(ttl,
                                           plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                                           ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  
  
  ## This does not include unemployed
  plt1 = plotStatistic_location("employed mean wage", loc_mean_docked, loc_sd_docked) + ggtitle("Docked")
  plt2 = plotStatistic_location("employed mean wage", loc_mean_wfh, loc_sd_wfh) + ggtitle("WFH")
  
  ttl = ggdraw() + draw_label("Nominal Wage (excl unemployed)", x = .15, size = 12) + theme_minimal()
  
  plt_nom_wage_workers = plot_grid(ttl,
                                   plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                                   ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  ## Mean Real Wage 
  ## Note - this should probably be plotted as a percent difference due to the rent calculation being crazy different with higher values of k
  
  loc_mean_docked$`Mean Real Wage` = loc_mean_docked$`employed mean real wage`
  loc_sd_docked$`Mean Real Wage` = loc_sd_docked$`employed mean real wage`
  
  loc_mean_wfh$`Mean Real Wage` = loc_mean_wfh$`employed mean real wage`
  loc_sd_wfh$`Mean Real Wage` = loc_sd_wfh$`employed mean real wage` 
  
  
  ttl = ggdraw() + draw_label("Real Wage (excl unemployed)", x = .15, size = 12) + theme_minimal()
  
  # plt1 = plotStatistic_location("Mean Real Wage", loc_mean_docked, loc_sd_docked) + ggtitle("Docked")
  # plt2 = plotStatistic_location("Mean Real Wage", loc_mean_wfh, loc_sd_wfh) + ggtitle("Docked")
  plt1 = plotStatistic_location("Mean Real Wage", loc_mean_docked, loc_sd_docked) + ggtitle("Docked") # Don't show the bands here because the SD really throws off the scale
  plt2 = plotStatistic_location("Mean Real Wage", loc_mean_wfh, loc_sd_wfh) + ggtitle("WFH")
  
  plt_real_wage_workers = plot_grid(ttl,
                                    plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                                    ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  
  
  
  ## Rent
  plt1 = plotStatistic_location("rent", loc_mean_docked, loc_sd_docked) + ggtitle("Docked")
  plt2 = plotStatistic_location("rent", loc_mean_wfh, loc_sd_wfh) + ggtitle("WFH")
  
  ttl = ggdraw() + draw_label("Rent", x = .15, size = 12) + theme_minimal()
  
  plt_rent = plot_grid(ttl,
                       plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                       ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  
  
  
  ## Preferred Location 
  plt1 = plotStatistic_location("preferred location", loc_mean_docked, loc_sd_docked) + ggtitle("Docked")
  plt2 = plotStatistic_location("preferred location", loc_mean_wfh, loc_sd_wfh) + ggtitle("WFH")
  
  ttl = ggdraw() + draw_label("Preferred Location", x = .15, size = 12) + theme_minimal()
  
  plt_pref_loc = plot_grid(ttl,
                           plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                           ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  
  
  ## Proportion of workers in each location 
  loc_mean_docked$`Proportion Workers` = loc_mean_docked$`N workers` / loc_mean_docked$`Total Workers Sector`
  loc_mean_wfh$`Proportion Workers` = loc_mean_wfh$`N workers` / loc_mean_wfh$`Total Workers Sector`
  
  plt1 = plotStatistic_location("Proportion Workers", loc_mean_docked, NULL) + ggtitle("Docked")
  plt2 = plotStatistic_location("Proportion Workers", loc_mean_wfh, NULL) + ggtitle("WFH")
  
  ttl = ggdraw() + draw_label("Proportion Workers", x = .15, size = 12) + theme_minimal()
  
  plt_N_prop = plot_grid(ttl,
                         plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                         ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  ## Total Workers in each location (by Sector)
  plt1 = plotStatistic_location("N workers", loc_mean_docked, loc_sd_docked) + ggtitle("Docked")
  plt2 = plotStatistic_location("N workers", loc_mean_wfh, loc_sd_wfh) + ggtitle("WFH")
  
  ttl = ggdraw() + draw_label("N Workers", x = .15, size = 12) + theme_minimal()
  
  
  
  plt_N_tot = plot_grid(ttl,
                        plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                        ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  
  
  
  ## Employment Rate
  loc_mean_docked$`Employment Rate` = loc_mean_docked$`N Employed` / loc_mean_docked$`N workers`
  loc_sd_docked$`Employment Rate` = loc_sd_docked$`N Employed` / loc_sd_docked$`N workers`
  
  loc_mean_wfh$`Employment Rate` = loc_mean_wfh$`N Employed` / loc_mean_wfh$`N workers`
  loc_sd_wfh$`Employment Rate` = loc_sd_wfh$`N Employed` / loc_sd_wfh$`N workers`
  
  
  # plt1 = plotStatistic_location("Employment Rate", loc_mean_docked, loc_sd_docked) + ggtitle("Docked")
  # plt2 = plotStatistic_location("Employment Rate", loc_mean_wfh, loc_sd_wfh) + ggtitle("WFH")
  plt1 = plotStatistic_location("Employment Rate", loc_mean_docked) + ggtitle("Docked")
  plt2 = plotStatistic_location("Employment Rate", loc_mean_wfh) + ggtitle("WFH")
  
  
  ttl = ggdraw() + draw_label("Employment Rate", x = .15, size = 12) + theme_minimal()
  
  plt_emp_rate = plot_grid(ttl,
                           plot_grid(plt1, plt2, rel_heights = c(1, 1), align = 'hv', ncol = 1, nrow = 2),
                           ncol = 1, nrow = 2, rel_heights = c(.15, 1))
  
  
  out = list("Remote Workers" = plt_remote_workers,
             "Nominal Wage" = plt_nom_wage_workers,
             "Real Wage" = plt_real_wage_workers,
             "Nominal Wage w Unemployed" = plt_nom_wage_workers_wUnemp,
             "Real Wage w Unemployed" = plt_real_wage_workers_wUnemp,
             "Rent" = plt_rent,
             "Preferred Location" = plt_pref_loc,
             "Total Workers" = plt_N_tot,
             "Proportion Workers" = plt_N_prop,
             "Unemployment Rate" = plt_emp_rate
             )
  
  
  return(out)
}




## Calculate pre2post wage ratio - return Kable
#
#
pre2postWageTable = function(loc_mean_docked, 
                             loc_mean_wfh
                             ){
  
  ## Self wage pre and post shock
  n = length(unique(loc_mean_docked$Name)) * length(unique(loc_mean_docked$Sector)) # Note that this only works for 1 sector with 2 locations and 2 and 2 (need to remember how to do n choose k for more...)
  self_real_wage_docked = rep(NA, n)
  self_nominal_wage_docked = self_real_wage_docked
  self_real_wage_wfh = self_real_wage_docked
  self_nominal_wage_wfh = self_real_wage_docked
  cnt = 1
  cnames = rep(NA, n)
  for (loc in unique(loc_mean_docked$Name)){
    for (sector in unique(loc_mean_docked$Sector)){
      
      idl = loc_mean_docked$Name == loc
      ids = loc_mean_docked$Sector == sector
      
      ## Including the unemployed
      tmp = loc_mean_docked$`mean real wage`[idl & ids]
      self_real_wage_docked[cnt] = tmp[600] / tmp[200]
      
      tmp = loc_mean_docked$`mean wage`[idl & ids]
      self_nominal_wage_docked[cnt] = tmp[600] / tmp[200]
      
      
      idl = loc_mean_wfh$Name == loc
      ids = loc_mean_wfh$Sector == sector
      
      tmp = loc_mean_wfh$`mean real wage`[idl & ids]
      self_real_wage_wfh[cnt] = tmp[600] / tmp[200]
      
      
      tmp = loc_mean_wfh$`mean wage`[idl & ids]
      self_nominal_wage_wfh[cnt] = tmp[600] / tmp[200]
      
      cnames[cnt] = paste0(loc, " - ", sector)
      cnt = cnt + 1
    }
  }
  
  
  self_real_wage_pre2post = matrix(data = c(self_real_wage_docked, self_real_wage_wfh), nrow = 2, ncol = n, byrow = TRUE)
  colnames(self_real_wage_pre2post) = cnames
  rownames(self_real_wage_pre2post) = c("Docked", "WFH")
  
  self_nominal_wage_pre2post = matrix(data = c(self_nominal_wage_docked, self_nominal_wage_wfh), nrow = 2, ncol = n, byrow = TRUE)
  colnames(self_nominal_wage_pre2post) = cnames
  rownames(self_nominal_wage_pre2post) = c("Docked", "WFH")
  
  
  ## Kableize these tables for ease of viewing
  tbl_wage_pre2post = kable(round(rbind(self_real_wage_pre2post, self_nominal_wage_pre2post), 2), 
                            row.names = TRUE, align = 'c',
                            caption = "Change in Wages pre-shock to post-shock") %>%
    kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
    pack_rows(group_label = "Real", start_row = 1, end_row = 2) %>%
    pack_rows(group_label = "Nominal", start_row = 3, end_row = 4)
  
  return(tbl_wage_pre2post)
}



## Location/Sector Post Comp Table
#
#
pre2postLocationComp = function(loc_mean_docked, 
                                loc_mean_wfh
                                ){
  
  ## Loc and Sector post shock
  n = length(unique(loc_mean_docked$Sector))
  comp_nominal_wage_docked = rep(NA, n)
  comp_real_wage_docked = comp_nominal_wage_docked
  comp_real_wage_wfh = comp_nominal_wage_docked
  comp_nominal_wage_wfh = comp_nominal_wage_docked
  cnt = 1
  cnames = rep(NA, n)
  for (sector in unique(loc_mean_docked$Sector)){
    
    cnames[cnt] = paste0("A/B - ", sector)
    
    idx = loc_mean_docked$Sector == sector
    idA = loc_mean_docked$Name == "Location A"
    idB = loc_mean_docked$Name == "Location B"
    idT = loc_mean_docked$`Time Step` == 600
    
    comp_nominal_wage_docked[cnt] = loc_mean_docked$`mean wage`[idx & idA & idT] / loc_mean_docked$`mean wage`[idx & idB & idT]
    comp_real_wage_docked[cnt] = loc_mean_docked$`mean real wage`[idx & idA & idT] / loc_mean_docked$`mean real wage`[idx & idB & idT]
    
    
    idx = loc_mean_wfh$Sector == sector
    idA = loc_mean_wfh$Name == "Location A"
    idB = loc_mean_wfh$Name == "Location B"
    idT = loc_mean_wfh$`Time Step` == 600
    
    comp_nominal_wage_wfh[cnt] = loc_mean_wfh$`mean wage`[idx & idA & idT] / loc_mean_wfh$`mean wage`[idx & idB & idT]
    comp_real_wage_wfh[cnt] = loc_mean_wfh$`mean real wage`[idx & idA & idT] / loc_mean_wfh$`mean real wage`[idx & idB & idT]
    
    cnt = cnt + 1
  }
  
  comp_real_wage_loc = matrix(data = c(comp_real_wage_docked, comp_real_wage_wfh), nrow = 2, ncol = n, byrow = TRUE)
  colnames(comp_real_wage_loc) = cnames
  rownames(comp_real_wage_loc) = c("Docked", "WFH")
  
  comp_nominal_wage_loc= matrix(data = c(comp_nominal_wage_docked, comp_nominal_wage_wfh), nrow = 2, ncol = n, byrow = TRUE)
  colnames(comp_nominal_wage_loc) = cnames
  rownames(comp_nominal_wage_loc) = c("Docked", "WFH")
  
  ## Kableize these tables for ease of viewing
  tbl_wage_loc_comp = kable(round(rbind(comp_real_wage_loc, comp_nominal_wage_loc), 2), 
                            row.names = TRUE, align = 'c',
                            caption = "Post-shock Wage Ratio by Location") %>%
    kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
    pack_rows(group_label = "Real", start_row = 1, end_row = 2) %>%
    pack_rows(group_label = "Nominal", start_row = 3, end_row = 4)
  
  
  return(tbl_wage_loc_comp)
}



## Create histograms of firm sizes
#
#
firmHistComp = function(df_firms_docked, 
                        df_firms_wfh
                        ){
  
  ## Histograms of firm sizes (compare these side-by-side)
  df_hist = do.call('rbind', df_firms_docked$Raw)
  colnames(df_hist) = gsub("\\.", " ", colnames(df_hist))
  df_hist = df_hist[df_hist$`Time Step` %in% c(0, 100, 199, 225, 245, 599),]
  hist_docked = ggplot(df_hist, aes(x = `Total Workers`, fill = Location)) + 
    geom_histogram(bins = 45) +
    facet_grid(`Time Step` ~ Sector) +
    theme_minimal() +
    ggtitle("Firm Size - Docked")
  
  df_hist = do.call('rbind', df_firms_wfh$Raw)
  colnames(df_hist) = gsub("\\.", " ", colnames(df_hist))
  df_hist = df_hist[df_hist$`Time Step` %in% c(0, 100, 199, 225, 245, 599),]
  hist_wfh = ggplot(df_hist, aes(x = `Total Workers`, fill = Location)) + 
    geom_histogram(bins = 45) +
    facet_grid(`Time Step` ~ Sector) +
    theme_minimal() +
    ggtitle("Firm Size - WFH")
  
  
  plt_hist = plot_grid(hist_docked, hist_wfh, ncol = 2, align = 'hv')
  
  
  return(plt_hist)
}


## Create Network Plots
#
#
networkPlots = function(net_df_docked,
                        net_df_wfh
                        ){
  
  net_df_docked = getNetworkSummaryData(paste(base_path, docked_path, sep = "/"))
  net_df_wfh = getNetworkSummaryData(paste(base_path, wfh_path, sep = "/"))
  
  
  plt_net_docked = cowplot::plot_grid(plotStatistic_networkSummary("Total Edges", net_df_docked$Mean, net_df_docked$Std) + ggtitle("Total Edges"),
                                      plotStatistic_networkSummary("Global Clustering", net_df_docked$Mean, net_df_docked$Std) + ggtitle("Global Clustering"),
                                      plotStatistic_networkSummary("Density", net_df_docked$Mean, net_df_docked$Std) + ggtitle("Density"),
                                      ncol = 1, nrow = 3, align = 'hv')
  
  
  plt_net_wfh = cowplot::plot_grid(plotStatistic_networkSummary("Total Edges", net_df_wfh$Mean, net_df_wfh$Std) + ggtitle("Total Edges"),
                                   plotStatistic_networkSummary("Global Clustering", net_df_wfh$Mean, net_df_wfh$Std) + ggtitle("Global Clustering"),
                                   plotStatistic_networkSummary("Density", net_df_wfh$Mean, net_df_wfh$Std) + ggtitle("Density"),
                                   ncol = 1, nrow = 3, align = 'hv')
  
  out = list("Docked" = plt_net_docked,
             "WFH" = plt_net_wfh)
  
  
  return(out)
}

