## A script to call the initial visualization/comparison .Rmd file and create 
## appropriate outputs


## Where should the files be saved?
save_path = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/Results/Initial_results"


## Where is the base path the files should be read from?
base_path = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/Results"


## Define the docked path and wfh path pairs as a named list, where the list
## names will be the save name of the file

comps = list("3_1_2_WFH_100" = c("3_1_Homogenous_Labor/3_1_2_labor_demand_shock", "3_1_WFH_extension/3_1_2_labor_demand_shock/Case_1"),
             "3_1_3_s_inf" = c("3_1_Homogenous_Labor/3_1_3_incidence/Case_1", "3_1_WFH_extension/3_1_3_incidence/Case_1"),
             "3_1_3_s_0" = c("3_1_Homogenous_Labor/3_1_3_incidence/Case_2", "3_1_WFH_extension/3_1_3_incidence/Case_2"),
             "3_1_3_kb_inf" = c("3_1_Homogenous_Labor/3_1_3_incidence/Case_3", "3_1_WFH_extension/3_1_3_incidence/Case_3"),
             "3_1_3_kb_0" = c("3_1_Homogenous_Labor/3_1_3_incidence/Case_4", "3_1_WFH_extension/3_1_3_incidence/Case_4"),
             "3_2_2_lowk_even" = c("3_2_Heterogenous_Labor/3_2_2_labor_demand_shock/Case_1", "3_2_WFH_extension/3_2_2_labor_demand_shock/Case_1"),
             "3_2_2_highk_even" = c("3_2_Heterogenous_Labor/3_2_2_labor_demand_shock/Case_2", "3_2_WFH_extension/3_2_2_labor_demand_shock/Case_2"),
             "3_2_2_lowk_uneven" = c("3_2_Heterogenous_Labor/3_2_2_labor_demand_shock/Case_3", "3_2_WFH_extension/3_2_2_labor_demand_shock/Case_3"),
             "3_2_2_highk_uneven" = c("3_2_Heterogenous_Labor/3_2_2_labor_demand_shock/Case_4", "3_2_WFH_extension/3_2_2_labor_demand_shock/Case_4")
             )


## And create some longer descriptions for the top of the document
desc = list("3_1_2_WFH_100" = c("Section 3.1.2 - Homogenous Labor, Labor Demand Shock. 100% WFH comparison"),
            "3_1_3_s_inf" = c("Section 3.1.3 - Homogenous Labor, Incidence. 100% WFH Comparison. Labor is perfectly immobile (s=inf)"),
            "3_1_3_s_0" = c("Section 3.1.3 - Homogenous Labor, Incidence. 100% WFH Comparison. Labor is perfectly mobile (s=0)"),
            "3_1_3_kb_inf" = c("Section 3.1.3 - Homogenous Labor, Incidence. 100% WFH Comparison. Housing Supply is fixed (kb=inf)"),
            "3_1_3_kb_0" = c("Section 3.1.3 - Homogenous Labor, Incidence. 100% WFH Comparison. Housing supply is infinitely elastic (kb=0)"),
            "3_2_2_lowk_even" = c("Section 3.2.2 - Heterogenous Labor, Labor Demand Shock. Low housing elasticity, even number of skilled/unskilled workers"),
            "3_2_2_highk_even" = c("Section 3.2.2 - Heterogenous Labor, Labor Demand Shock. High housing elasticity, even number of skilled/unskilled workers"),
            "3_2_2_lowk_uneven" = c("Section 3.2.2 - Heterogenous Labor, Labor Demand Shock. Low housing elasticity, more unskilled than skilled workers"),
            "3_2_2_highk_uneven" = c("Section 3.2.2 - Heterogenous Labor, Labor Demand Shock. High housing elasticity, more unskilled than skilled workers")
            )



## Create wfh/docked comparison tables
lpath = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/R visualizations"
source(paste(lpath, "fxns_firm.R", sep = "/"))
source(paste(lpath, "fxns_paper.R", sep = "/"))
source(paste(lpath, "fxns_general.R", sep = "/"))
source(paste(lpath, "fxns_labor.R", sep = "/"))
source(paste(lpath, "fxns_network_smry.R", sep = "/"))
library(ggplot2)
library(cowplot)


wfh_docked_impact_tables = list()
final_plots = list()
cnt_loop = 1 # It seems like this is sharing an environment with the Markdown document, which also uses cnt (so it gets over-written, which is a fun thing R is doing)
ptm = Sys.time()
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
  
  
  final_plots[[nm]] = createFinalPlots(df_loc_docked, df_loc_wfh)
  
  
  if (cnt_loop == 1){
    
    print("Approximate total time: ")
    approx_t = (Sys.time() - ptm) * length(comps)
    print(approx_t)
  }
  
  cnt_loop = cnt_loop + 1
}

print("Actual Total Time: ")
print(Sys.time() - ptm)

## Save the results
save(wfh_docked_impact_tables, file = paste(save_path, "WFH_impact_tables.RData", sep = "/"))
save(final_plots, file = paste(save_path, "Final_plots.RData", sep = "/"))








