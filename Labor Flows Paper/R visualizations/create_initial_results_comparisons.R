## A script to call the initial visualization/comparison .Rmd file and create 
## appropriate outputs


## Where should the files be saved?
save_path = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/Results/Initial_results"


## Where is the base path the files should be read from?
base_path = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/Results"


## Define the docked path and wfh path pairs as a named list, where the list
## names will be the save name of the file

comps = list("3_1_2_WFH_100" = c("3_1_Homogenous_Labor/3_1_2_labor_demand_shock", "3_1_WFH_extension/3_1_2_labor_demand_shock/Case_1"),
             "3_1_2_WFH_50" = c("3_1_Homogenous_Labor/3_1_2_labor_demand_shock", "3_1_WFH_extension/3_1_2_labor_demand_shock/Case_2"),
             "3_1_3_s_inf" = c("3_1_Homogenous_Labor/3_1_3_incidence/Case_1", "3_1_WFH_extension/3_1_3_incidence/Case_1"),
             "3_1_3_s_0" = c("3_1_Homogenous_Labor/3_1_3_incidence/Case_2", "3_1_WFH_extension/3_1_3_incidence/Case_2"),
             "3_1_3_kb_inf" = c("3_1_Homogenous_Labor/3_1_3_incidence/Case_3", "3_1_WFH_extension/3_1_3_incidence/Case_3"),
             "3_1_3_kb_0" = c("3_1_Homogenous_Labor/3_1_3_incidence/Case_4", "3_1_WFH_extension/3_1_3_incidence/Case_4"),
             # "3_1_4" = c()
             "3_2_2_lowk_even" = c("3_2_Heterogenous_Labor/3_2_2_labor_demand_shock/Case_1", "3_2_WFH_extension/3_2_2_labor_demand_shock/Case_1"),
             "3_2_2_highk_even" = c("3_2_Heterogenous_Labor/3_2_2_labor_demand_shock/Case_2", "3_2_WFH_extension/3_2_2_labor_demand_shock/Case_2"),
             "3_2_2_lowk_uneven" = c("3_2_Heterogenous_Labor/3_2_2_labor_demand_shock/Case_3", "3_2_WFH_extension/3_2_2_labor_demand_shock/Case_3"),
             "3_2_2_highk_uneven" = c("3_2_Heterogenous_Labor/3_2_2_labor_demand_shock/Case_4", "3_2_WFH_extension/3_2_2_labor_demand_shock/Case_4")#,
             # "3_2_3_lowk_even" = c("3_2_Heterogenous_Labor/3_2_3_incidence/Case_1", "3_2_WFH_extension/3_2_3_incidence/Case_1"),
             # "3_2_3_highk_even" = c("3_2_Heterogenous_Labor/3_2_3_incidence/Case_2", "3_2_WFH_extension/3_2_3_incidence/Case_2"),
             # "3_2_3_lowk_uneven" = c("3_2_Heterogenous_Labor/3_2_3_incidence/Case_3", "3_2_WFH_extension/3_2_3_incidence/Case_3"),
             # "3_2_3_highk_uneven" = c("3_2_Heterogenous_Labor/3_2_3_incidence/Case_4", "3_2_WFH_extension/3_2_3_incidence/Case_4")
             )


## And create some longer descriptions for the top of the document
desc = list("3_1_2_WFH_100" = c("Section 3.1.2 - Homogenous Labor, Labor Demand Shock. 100% WFH comparison"),
            "3_1_2_WFH_50" = c("Section 3.1.2 - Homogenous Labor, Labor Demand Shock. 50% WFH comparison"),
            "3_1_3_s_inf" = c("Section 3.1.3 - Homogenous Labor, Incidence. 100% WFH Comparison. Labor is perfectly immobile (s=inf)"),
            "3_1_3_s_0" = c("Section 3.1.3 - Homogenous Labor, Incidence. 100% WFH Comparison. Labor is perfectly mobile (s=0)"),
            "3_1_3_kb_inf" = c("Section 3.1.3 - Homogenous Labor, Incidence. 100% WFH Comparison. Housing Supply is fixed (kb=inf)"),
            "3_1_3_kb_0" = c("Section 3.1.3 - Homogenous Labor, Incidence. 100% WFH Comparison. Housing supply is infinitely elastic (kb=0)"),
            "3_2_2_lowk_even" = c("Section 3.2.2 - Heterogenous Labor, Labor Demand Shock. Low housing elasticity, even number of skilled/unskilled workers"),
            "3_2_2_highk_even" = c("Section 3.2.2 - Heterogenous Labor, Labor Demand Shock. High housing elasticity, even number of skilled/unskilled workers"),
            "3_2_2_lowk_uneven" = c("Section 3.2.2 - Heterogenous Labor, Labor Demand Shock. Low housing elasticity, more unskilled than skilled workers"),
            "3_2_2_highk_uneven" = c("Section 3.2.2 - Heterogenous Labor, Labor Demand Shock. High housing elasticity, more unskilled than skilled workers")#,
            # "3_2_3_lowk_even" = c("Section 3.2.3 - Heterogenous Labor, Labor Demand Shock. Low housing elasticity, even number of skilled/unskilled workers. High WFH tolerance for skilled, Low WFH tolerance for unskilled"),
            # "3_2_3_highk_even" = c("Section 3.2.3 - Heterogenous Labor, Labor Demand Shock. High housing elasticity, even number of skilled/unskilled workers. High WFH tolerance for skilled, Low WFH tolerance for unskilled"),
            # "3_2_3_lowk_uneven" = c("Section 3.2.3 - Heterogenous Labor, Labor Demand Shock. Low housing elasticity, more unskilled than skilled workers. High WFH tolerance for skilled, Low WFH tolerance for unskilled"),
            # "3_2_3_highk_uneven" = c("Section 3.2.3 - Heterogenous Labor, Labor Demand Shock. High housing elasticity, more unskilled than skilled workers. High WFH tolerance for skilled, Low WFH tolerance for unskilled")
            )



## For each item in the comps/desc list, read in the appropriate data and create the comparisons
cnt_loop = 1 # It seems like this is sharing an environment with the Markdown document, which also uses cnt (so it gets over-written, which is a fun thing R is doing)
for (nm in names(comps)){
  
  print(paste0("Creating ", nm, " comparison (", cnt_loop, " of ", length(comps), ")"))
  
  ## The base_path gets passed to the file. The docked_path and wfh_path variables must also be specified
  docked_path = comps[[nm]][1]
  wfh_path = comps[[nm]][2]
  
  
  quick_description = desc[[nm]]
  
  
  ## Create the markdown document
  rmarkdown::render(input = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/R visualizations/initial_results.Rmd",
                    output_format = "html_document",
                    output_file = paste0(nm, ".html"),
                    output_dir = save_path,
                    quiet = TRUE)
  
  
  cnt_loop = cnt_loop + 1
}


## Create wfh/docked comparison tables
lpath = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/R visualizations"
source(paste(lpath, "fxns_firm.R", sep = "/"))
source(paste(lpath, "fxns_general.R", sep = "/"))
source(paste(lpath, "fxns_labor.R", sep = "/"))
source(paste(lpath, "fxns_network_smry.R", sep = "/"))

wfh_docked_impact_tables = list()
cnt_loop = 1 # It seems like this is sharing an environment with the Markdown document, which also uses cnt (so it gets over-written, which is a fun thing R is doing)
for (nm in names(comps)){
  
  print(paste0("Creating ", nm, " tables (", cnt_loop, " of ", length(comps), ")"))
  
  ## The base_path gets passed to the file. The docked_path and wfh_path variables must also be specified
  docked_path = comps[[nm]][1]
  wfh_path = comps[[nm]][2]
  
  
  ## Import docked data
  df_loc_docked = getLocationData(paste(base_path, docked_path, sep = "/"))
  loc_mean_docked = df_loc_docked$Mean
  
  ## Import WFH data
  df_loc_wfh = getLocationData(paste(base_path, wfh_path, sep = "/"))
  loc_mean_wfh = df_loc_wfh$Mean
  
  tbls = getWFHImpactTable(df_dock = loc_mean_docked, df_wfh = loc_mean_wfh)
  
  wfh_docked_impact_tables[[nm]] = tbls
  
  cnt_loop = cnt_loop + 1
}


## Save the results
save(wfh_docked_impact_tables, file = paste(save_path, "WFH_impact_tables.RData", sep = "/"))


## Create table comparisons 
rmarkdown::render(input = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/R visualizations/wfh_v_docked.Rmd",
                  output_format = "html_document",
                  output_file = paste0("WFH_v_Docked_tables.html"),
                  output_dir = save_path,
                  quiet = TRUE)



## Create Results and save them as an RData file
library(knitr)
library(kableExtra)
library(ggplot2)
library(cowplot)

lpath = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/R visualizations"
source(paste(lpath, "fxns_firm.R", sep = "/"))
source(paste(lpath, "fxns_general.R", sep = "/"))
source(paste(lpath, "fxns_labor.R", sep = "/"))
source(paste(lpath, "fxns_network_smry.R", sep = "/"))
source(paste(lpath, "fxns_paper.R", sep = "/"))


all_results = list()
cnt_loop = 1 # It seems like this is sharing an environment with the Markdown document, which also uses cnt (so it gets over-written, which is a fun thing R is doing)
ptm = Sys.time()
for (nm in names(comps)){
  
  print(paste0("Creating ", nm, " comparison (", cnt_loop, " of ", length(comps), ")"))
  
  ## The base_path gets passed to the file. The docked_path and wfh_path variables must also be specified
  docked_path = comps[[nm]][1]
  wfh_path = comps[[nm]][2]
  
  ## Import docked data
  df_firms_docked = getFirmData(paste(base_path, docked_path, sep = "/"))
  df_loc_docked = getLocationData(paste(base_path, docked_path, sep = "/"))
  loc_mean_docked = df_loc_docked$Mean
  loc_sd_docked = df_loc_docked$Std
  
  
  
  ## Import WFH data
  df_firms_wfh = getFirmData(paste(base_path, wfh_path, sep = "/"))
  df_loc_wfh = getLocationData(paste(base_path, wfh_path, sep = "/"))
  loc_mean_wfh = df_loc_wfh$Mean
  loc_sd_wfh = df_loc_wfh$Std
  
  
  ## Create firm comparisons
  tmp_firm = createFirmPlots(loc_mean_docked, loc_sd_docked, loc_mean_wfh, loc_sd_wfh)
  
  
  ## Create Location/Labor Plots 
  tmp_loc = createLaborPlots(loc_mean_docked, loc_sd_docked, loc_mean_wfh, loc_sd_wfh)
  
  
  ## Wage ratio and Location Comp
  tmp_wage_tbl = pre2postWageTable(loc_mean_docked, loc_mean_wfh)
  tmp_loc_tbl = pre2postLocationComp(loc_mean_docked, loc_mean_wfh)
  
  
  ## Histogram of firm sizes
  tmp_hist = firmHistComp(df_firms_docked, df_firms_wfh)
  
  
  ## Network Plots
  tmp_net = networkPlots(net_df_docked, net_df_wfh)
  
  
  all_results[[nm]] = list("Firm Comparisons" = tmp_firm,
                           "Labor Comparisons" = tmp_loc,
                           "Wage Ratio Table" = tmp_wage_tbl,
                           "Location Comp Table" = tmp_loc_tbl,
                           "Firm Size Hist" = tmp_hist,
                           "Network Plot" = tmp_net
                           )
  
  if (cnt_loop == 1){
    
    print("Approximate total time: ")
    approx_t = (Sys.time() - ptm) * length(comps)
  }
  
  cnt_loop = cnt_loop + 1
}

print("Actual Total Time: ")
Sys.time() - ptm

## Save the results
save(all_results, file = paste(save_path, "All_Plots.RData", sep = "/"))





