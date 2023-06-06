

## Temp script for inspecting some base results
library(ggplot2)


## Load visualization functions
lpath = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/R visualizations"
source(paste(lpath, "fxns_firm.R", sep = "/"))
source(paste(lpath, "fxns_general.R", sep = "/"))
source(paste(lpath, "fxns_labor.R", sep = "/"))
source(paste(lpath, "fxns_network_smry.R", sep = "/"))



## Point to the correct location and get the data
base_path = "C:/Users/zachm/Documents/George Mason/Projects/Labor Flows Paper/Results"
# run_path = "Tech_Restaurant_2L_firm_policy"
# run_fpath = "3_1_Homogenous_Labor/3_1_2_labor_demand_shock"
# run_fpath = "3_1_Homogenous_Labor/3_1_3_incidence/case1"
# run_fpath = "3_1_Homogenous_Labor/3_1_3_incidence/case2"
run_path = "3_2_Heterogenous_Labor/3_2_2_labor_demand_shock/Case_1"


fpath = paste(base_path, run_path, sep = "/")

## Get Firm Data
df_firms = getFirmData(fpath)

## Get Location Data
loc_df = getLocationData(fpath)

loc_mean = loc_df$Mean
loc_sd = loc_df$Std


## Check firm size versus wage...
ggplot(df_firms$Raw[[1]][df_firms$Raw[[1]]$Productivity.Shift == 0, ], aes(x = Total.Workers, y = Wage, color = Sector)) + 
  geom_point() +
  theme_minimal() +
  facet_grid(. ~ Location)
## Weird set of firms with 0 wage and more than 0 workers...but the rest of the profit looks fine...

## But the profit goes way down for more than a few workers...
ggplot(df_firms$Raw[[1]][df_firms$Raw[[1]]$Productivity.Shift == 0, ], aes(x = Total.Workers, y = Profit, color = Sector)) + 
  geom_point() +
  theme_minimal() +
  facet_grid(. ~ Location)

## See line 144 in firms.py. Looks like profit scales with the number of workers to the h (fractional value)
## But the max of the location mean wage and the firm wage scales the cost times total N
## I think, here, the units are off. The wage is already scaled by the number of workers...





plotUtilityComponents(loc_mean, loc_sd) + ggtitle("Mean Utility Components")
# plotMeanWageComponents(loc_mean, loc_sd, alpha = .75) + ggtitle("Mean Wage Components") 


## Create Base Location Plots
plotStatistic_location("Firm Mean Workers", loc_mean, loc_sd) + ggtitle("Mean Workers per Firm")
# plotStatistic_location("Firm Mean Production", loc_mean, loc_sd) + ggtitle("Mean Production of Firms")
plotStatistic_location("Firm Mean Profit", loc_mean, loc_sd) + ggtitle("Mean Profit of Firms")
plotStatistic_location("remote workers", loc_mean, loc_sd) + ggtitle("Remote Workers")
plotStatistic_location("mean wage", loc_mean, loc_sd) + ggtitle("Mean Nominal Wage")
plotStatistic_location("mean utility", loc_mean, loc_sd) + ggtitle("Mean Utility")
plotStatistic_location("rent", loc_mean, loc_sd) + ggtitle("Rent")

plotStatistic_location("preferred location", loc_mean, loc_sd)
plotStatistic_location("N Employed", loc_mean, loc_sd)

plotStatistic_location("Firm Mean Hiring Quota unfulfilled", loc_mean, loc_sd) + ggtitle("Mean Unfulfilled Hiring Quota")

## A look at wages zoomed in:
plotStatistic_location("mean wage", loc_mean, loc_sd) + ggtitle("Mean Wage") + scale_y_continuous(limits = c(-.5, 2))
plotStatistic_location("mean utility", loc_mean, loc_sd) + ggtitle("Mean Utility") + scale_y_continuous(limits = c(-1.5, 1))


## Calculate and plot employment rate per sector
loc_mean$`Sector Employment Rate` = loc_mean$`N Employed` / loc_mean$`Total Workers Sector`
loc_sd$`Sector Employment Rate` = loc_sd$`N Employed` / loc_sd$`Total Workers Sector`
plotStatistic_location("Sector Employment Rate", loc_mean, loc_sd)

plotStatistic_location("Total Firms", loc_mean, loc_sd)


## Normalized rent - this is actually Real Wage
loc_mean$`Mean Real Wage` = loc_mean$`mean wage` / loc_mean$rent
loc_sd$`Mean Real Wage` = loc_sd$`mean wage` / loc_sd$rent
plotStatistic_location("Mean Real Wage", loc_mean, loc_sd) + scale_y_continuous(limits = c(0, 2.5))


## Not sure if this is correct or not...
loc_mean$`Employment Rate` = loc_mean$`N Employed` / loc_mean$`N workers`
loc_sd$`Employment Rate` = loc_sd$`N Employed` / loc_sd$`N workers`
plotStatistic_location("Employment Rate", loc_mean, loc_sd) + scale_y_continuous(limits = c(.55, 1.05))


## On a per-firm basis
## Could just scale 1 and 3 by Workers per firm...
plt1 = plotStatistic_location("Firm Mean Remote Workers", loc_mean, loc_sd) + ggtitle("Mean Remote Workers per Firm")
plt2 = plotStatistic_location("Firm Mean Workers", loc_mean, loc_sd) + ggtitle("Mean Workers per Firm")
plt3 = plotStatistic_location("Firm Mean Worker Preferred Loc", loc_mean, loc_sd) + ggtitle("Mean Workers in Preferred Location per Firm")
cowplot::plot_grid(plt1, plt2, plt3, nrow = 3, ncol = 1, align = 'hv')


## Display Remote workers and workers in preferred location per firm as a percentage of the firm workers
tmp_mean = loc_mean
tmp_sd = loc_sd
tmp_mean$remote_workers_percentage = tmp_mean$`Firm Mean Remote Workers` / tmp_mean$`Firm Mean Workers` 
tmp_mean$co_located_percentage = tmp_mean$`Firm Mean Co Located` / tmp_mean$`Firm Mean Workers`
tmp_mean$preferred_loc_percentage = tmp_mean$`Firm Mean Worker Preferred Loc` / tmp_mean$`Firm Mean Workers`

tmp_sd$remote_workers_percentage = tmp_sd$`Firm Mean Remote Workers` / tmp_sd$`Firm Mean Workers`
tmp_sd$co_located_percentage = tmp_sd$`Firm Mean Co Located` / tmp_sd$`Firm Mean Workers`
tmp_sd$preferred_loc_percentage = tmp_sd$`Firm Mean Worker Preferred Loc` / tmp_sd$`Firm Mean Workers`


plt1 = plotStatistic_location("remote_workers_percentage", tmp_mean, tmp_sd) + ggtitle("Mean Remote Workers Per Firm (Percentage)") + scale_y_continuous(limits = c(-.05, 1.05))
plt2 = plotStatistic_location("co_located_percentage", tmp_mean, tmp_sd) + ggtitle("Mean Workers Co-Located Per Firm (Percentage)") + scale_y_continuous(limits = c(-.05, 1.05))
plt3 = plotStatistic_location("preferred_loc_percentage", tmp_mean, tmp_sd) + ggtitle("Mean Workers in Preferred Location Per Firm (Percentage)") + scale_y_continuous(limits = c(-.05, 1.05))
cowplot::plot_grid(plt1, plt2, plt3, nrow = 3, ncol = 1, align = 'hv')


## No standard deviation (because its quote wide and probably not properly scaled)
plt1 = plotStatistic_location("remote_workers_percentage", tmp_mean) + ggtitle("Mean Remote Workers Per Firm (Percentage)") + scale_y_continuous(limits = c(-.1, 1.1))
plt2 = plotStatistic_location("co_located_percentage", tmp_mean) + ggtitle("Mean Workers Co-Located Per Firm (Percentage)") + scale_y_continuous(limits = c(-.1, 1.1))
plt3 = plotStatistic_location("preferred_loc_percentage", tmp_mean) + ggtitle("Mean Workers in Preferred Location Per Firm (Percentage)") + scale_y_continuous(limits = c(-.1, 1.1))
cowplot::plot_grid(plt1, plt2, plt3, nrow = 3, ncol = 1, align = 'hv')



plotStatistic_location("Firm Mean Remote Workers", loc_mean, loc_sd) + ggtitle("Mean Remote Workers per Firm")



# ## Get total employment rate
# empl_mean = NULL
# empl_sd = NULL
# for (ii in unique(loc_mean$`Time Step`)){
#   
#   empl_mean = rbind(empl_mean, colSums(loc_mean[loc_mean$`Time Step` == ii, c("N Employed", "N workers")]))
# }
# empl_mean = as.data.frame(empl_mean)



## Get Firm Data
df_firms = getFirmData(fpath)


## Produce the Firms histogram
df_hist = do.call('rbind', df_firms$Raw)
colnames(df_hist) = gsub("\\.", " ", colnames(df_hist))

# stat_name = "Total Workers"
# time_step = unique(df_hist$`Time Step`)
# locations = unique(df_hist$Location)
# sectors = unique(df_hist$Sector)
# n = length(time_step) * length(locations) * length(sectors)
# df_hist_mean = matrix(data = NA, ncol = 4, nrow = n)
# cnt = 1
# for (ts in time_step){
#   for (l in locations){
#     for (s in sectors){
#       
#       idx = (df_hist$`Time Step` == ts) & (df_hist$Location == l) & (df_hist$Sector == s)
#       if (length(idx) > 0){
#         
#         df_hist_mean[cnt, ] = c(l, s, ts, mean(df_hist[[stat_name]], rm.na = TRUE))
#         
#         cnt = cnt + 1
#       }
#     }
#   }
# }
# idNA = rowSums(is.na(df_hist_mean)) == ncol(df_hist_mean)
# df_hist_mean = df_hist_mean[!idNA, ]
# 
# colnames(df_hist_mean) = c("Location", "Sector", "Time Step", stat_name)
# df_hist_mean = as.data.frame(df_hist_mean)
# df_hist_mean$`Time Step` = as.numeric(df_hist_mean$`Time Step`)
# df_hist_mean[[stat_name]] = as.numeric(df_hist_mean[[stat_name]])
# 
# df_plt = df_hist_mean[df_hist_mean$`Time Step` %in% c(0, 50, 99, 145, 200), ]
# ggplot(df_plt, aes(x = `Total Workers`, fill = Sector)) +
#   geom_histogram(bins = 45) +
#   facet_grid(`Time Step` ~ Location) +
#   theme_minimal()




df_hist = df_hist[df_hist$`Time Step` %in% c(0, 50, 99, 145, 200),]
df_hist = df_hist[df_hist$`Time Step` %in% c(0, 100, 199, 245, 400),]
ggplot(df_hist, aes(x = `Total Workers`, fill = Sector)) + 
  geom_histogram(bins = 45) +
  facet_grid(`Time Step` ~ Location) +
  theme_minimal()





## Pull in Global Network data and create a quick comparison
net_df = getNetworkSummaryData(fpath)


cowplot::plot_grid(#plotStatistic_networkSummary("Total Nodes", net_df$Mean, net_df$Std) + ggtitle("Total Nodes"),
                   plotStatistic_networkSummary("Total Edges", net_df$Mean, net_df$Std) + ggtitle("Total Edges"),
                   plotStatistic_networkSummary("Global Clustering", net_df$Mean, net_df$Std) + ggtitle("Global Clustering"),
                   plotStatistic_networkSummary("Density", net_df$Mean, net_df$Std) + ggtitle("Density"),
                   ncol = 1, nrow = 3, align = 'hv')





## Plot some mean firm stats
plotStatistic_MeanFirm("Node Degree", df_firms$Mean, df_firms$Std)
plotStatistic_MeanFirm("Degree Centrality", df_firms$Mean, df_firms$Std)
plotStatistic_MeanFirm("Clustering", df_firms$Mean, df_firms$Std)

plotStatistic_MeanFirm("Total Workers", df_firms$Mean, df_firms$Std)



## Hmm, maybe in collapse Mean remote, put the following into the same exported data.frame (that way it only needs to be run once):
# Total Workers, Remote Tolerance, Profit, Hiring Quota, Wage, Location Mean Wage, Remote Workers,
# Preferred Location, Co Located, Node Triangles, Node Degree, Node V Shapes, Node Clustering, VoteRank, 
# Eigenvector Centrality, Betweeness Centrality


## Check out remote tolerance policy implications
df = collapseMeanRemote_firm(df_firms$Raw, "Wage")
plotStatistic_location_byRemote("Wage", df$Mean, df$StDev) + ggtitle("Remote Work Policy Impacts\nFirm Mean Wage")

df = collapseMeanRemote_firm(df_firms$Raw, "Total Workers")
plotStatistic_location_byRemote("Total Workers", df$Mean, df$StDev) + ggtitle("Remote Work Policy Impacts\nFirm Mean Workers")


## Better approach:
df = collapseMeanRemote_firm_all(df_firms$Raw) # This gets all of the columns above
plotStatistic_location_byRemote("Wage", df$Mean, df$StDev)




