# -*- coding: utf-8 -*-
"""
Run a 2 location, 2 sector model with a remote working shock at 100 steps

@author: zachm
"""

import sys
import random
import numpy as np
import time
# import pandas as pd

## Point to the location where the Game and Agent class definitions are
sys.path.insert(0, r'C:\Users\zachm\Documents\George Mason\Projects\Labor Flow Python Package\src')


import laborFlow as lf

"""
Some notes on number of firms/number of workers

There are roughly 157 million workers in the US economy and 11 million firms

(old)
n_firms = 150
n_workers = 5000

"""
n_labor = 1570
n_firms = 110

firm_search = .005
labor_search = .05

## Set parameters in a dictionary - write these to a log file after writing results



## To Do: store edge list of each network as an output (maybe as a string with some common sep)

## Set base seed
seed = 202111
write_path = r'C:\Users\zachm\Documents\George Mason\Projects\Labor Flows Paper\Results\2L_4S'

start = time.time()

n_steps = 100 # How many steps should be in the model pre and post shock?
for ii in range(50):
    
    
    print(ii)
    
    new_seed = seed + ii
    random.seed(new_seed)
    np.random.seed(new_seed)
    
    
    test = lf.Economy()
    
    ## Add Housing
    test.addLocation(housing_elasticity = .5, local_ammenities = np.log(1), productivity_shifter = 3, location_ID = "Location 1")
    test.addLocation(housing_elasticity = .5, local_ammenities = np.log(2), productivity_shifter = 1, location_ID = "Location 2")
    
    
    ### Add firms in two sectors
    test.addAllFirms(n_firms = n_firms, 
                     loc_pr = {'Location 1': .3, 'Location 2': .7},
                     sec_pr = {'Sector 1': .3, 'Sector 2': .3, 'Sector 3': .2, 'Sector 4': .2},
                     # remote_prefs = {'Sector 1': .2, 'Sector 2': .5}
                     remote_prefs = None
                     )


    ## Add workers
    test.addLabor(n_workers = n_labor,
                  location_prefs = {'Location 1': [-1, 1], 'Location 2': [0, 1]}, 
                  location_prop = {'Location 1': .6, 'Location 2': .4},
                  sector_prop = {'Sector 1': .3, 'Sector 2': .3, 'Sector 3': .2, 'Sector 4': .2},
                  remote_prefs = [.8, .8, .8, .8],
                  # remote_prefs = None,
                  unemployment_rate = .05,
                  firm_eta = .8)
    
    
    stats_loc, stats_firm, stats_labor, stats_network = test.simulate(steps = n_steps,
                                                                      alpha = labor_search, beta = firm_search) 


    ## Update remote tolerances for workers
    for f in test.Firms.values():
        
        ## Get the tolerance from a gaussian ditribution based on sector
        if f.sector == 'Sector 1':
            
            new_tol = np.random.normal(.1, .075)
        
        elif f.sector == 'Sector 2':
            
            new_tol = np.random.normal(.5, .075)
            
        elif f.sector == 'Sector 3':
            
            new_tol = np.random.normal(.9, .075)
        
        
        ## Bound the values between 0 and 1 and udpate the firm
        new_tol = min(max(0, new_tol), 1)    
        f.updateRemotePref(new_tol)
            


    ## Simulate the last 50 steps
    s_stats_loc, s_stats_firm, s_stats_labor, s_stats_network = test.simulate(steps = n_steps, is_shock = True, time_offset = n_steps,
                                                                              alpha = labor_search, beta = firm_search)

    
    ## Update the "steps" and bind the shocked data frames to the initial data frames
    s_stats_loc['Time Step'] = s_stats_loc['Time Step']
    s_stats_firm['Time Step'] = s_stats_firm['Time Step']
    # s_stats_labor['Time Step'] = s_stats_labor['Time Step']
    s_stats_network['Time Step'] = s_stats_network['Time Step']


    ## Need to drop the first step 

    stats_loc = stats_loc.append(s_stats_loc, ignore_index = True)
    stats_firm = stats_firm.append(s_stats_firm, ignore_index = True)
    # stats_labor = stats_labor.append(s_stats_labor, ignore_index = True)
    stats_network = stats_network.append(s_stats_network, ignore_index = True)


    ## Write these files to csvs in the results folder for looking at in R
    stats_loc.to_csv(write_path + r'\location_results_' + str(ii) + '.csv')
    stats_firm.to_csv(write_path + r'\firm_results_' + str(ii) + '.csv')
    # stats_labor.to_csv(fpath + r'\labor_results.csv') # These are pretty big, let's avoid writing them...
    stats_network.to_csv(write_path + r'\network_results_' + str(ii) + '.csv')
    
    
    ## Instead of writing the whole firm stats to csv, just get the mean results per sector and location, per time step and write those
    # stats_firm.to_csv(write_path + r'\firm_results_' + str(ii) + '.csv')
    
    

end = time.time()
print(end - start)
