# -*- coding: utf-8 -*-
"""
Replicate Section 3.1.3: Incidence: Who Benefits From the Productivity Increase

The focus here is on s=0 and s=inf (agent preferences for certain locations) and the impact
of firm policy. In particular, what happens when the 10 or so smallest firms decide to 
allow remote work, while the rest of the firms remain against it?


@author: zachm
"""


import sys
import random
import numpy as np
import time
import pandas as pd

## Make sure the labor-flows-paper environment is activated
## Then, we can monitor what version of the code was being used...

## Until then:
## Point to the location where the Game and Agent class definitions are
sys.path.insert(0, r'C:\Users\zachm\Documents\George Mason\Projects\Labor Flow Python Package\src')


import laborFlow as lf

"""
Some notes on number of firms/number of workers

There are roughly 157 million workers in the US economy and 11 million firms


"""
n_labor = 1570 #15700
n_firms = 110  #1100

firm_search = .01 # .005
labor_search = .05

entrepenuer = .05 # By default in the model, this is .05...and it controls whether a worker will try to start a new firm or not
                  # A current issue is that a worker will move to any location to start a new firm and won't evaluate their utility...

## Set parameters in a dictionary - write these to a log file after writing results



## To Do: store edge list of each network as an output (maybe as a string with some common sep)

## Set base seed
seed = 202207
write_path = r'C:\Users\zachm\Documents\George Mason\Projects\Labor Flows Paper\Results\3_1_WFH_extension\3_1_3_firm_policy'

start = time.time()


n_steps_pre = 200  # How many steps should be in the model pre shock?
n_steps_post = 400 # How many steps should be in the model post shock?

runs = 50     # How many simulation runs should be done for each world?

"""
World 1: labor is completely immobile (in Morettie, s=inf. Here, I set preferences for all workers in two steps)
"""
print('Running Case 1: Labor is Perfectly Immobile')
for ii in range(runs):   
    
    print(ii)
    
    new_seed = seed + ii
    random.seed(new_seed)
    np.random.seed(new_seed)

    world = lf.Economy()
    
    ## Add Locations
    world.addLocation(housing_elasticity = .5, local_ammenities = np.log(4), productivity_shifter = {'Sector 1': 6}, location_ID = "Location A")
    world.addLocation(housing_elasticity = .5, local_ammenities = np.log(4), productivity_shifter = {'Sector 1': 6}, location_ID = "Location B")
    
    
    ### Add firms in one sectors
    world.addAllFirms(n_firms = n_firms, 
                      loc_pr = {'Location A': .7, 'Location B': .3},
                      sec_pr = {'Sector 1': 1},
                      # remote_prefs = {'Sector 1': .2, 'Sector 2': .5}
                      remote_prefs = None                      
                      )

    
    ### Split the workers in half, setting their mobility such that they won't/can't migrate
    ## City A
    world.addLabor(n_workers = (int(np.floor(n_labor / 2))),
                   location_prefs = {'Location A': [100, 100], 'Location B': [0, 0]}, 
                   location_prop = {'Location A': .6, 'Location B': .4},
                   sector_prop = {'Sector 1': 1},
                   remote_prefs = [.8, .8],
                   # remote_prefs = None,
                   unemployment_rate = .05,
                   firm_eta = .8)

    ## City B
    world.addLabor(n_workers = (int(np.floor(n_labor / 2))),
                   location_prefs = {'Location A': [0, 0], 'Location B': [100, 100]}, 
                   location_prop = {'Location A': .6, 'Location B': .4},
                   sector_prop = {'Sector 1': 1},
                   remote_prefs = [.8, .8],
                   # remote_prefs = None,
                   unemployment_rate = .05,
                   firm_eta = .8)



    stats_loc, stats_firm, stats_labor, stats_network = world.simulate(steps = n_steps_pre,
                                                                       alpha = labor_search, 
                                                                       beta = firm_search,
                                                                       start_firm = entrepenuer) 


    ## Update remote tolerances for workers
    f_p = {k: f.profit for k, f in world.Firms.items()}                       # Put just the profits in a dictionary
    f_change = [f[0] for f in sorted(f_p.items(), key = lambda p: p[1])[:10]] # Gets the keys (i.e. firm names) for the first 10 firms with the smallest profits
    for f in f_change:

        new_tol = np.random.normal(.9, .075)
        world.Firms[f].updateRemotePref(new_tol)


    ## Shock technology in city b
    p_shock = 10
    world.Locations['Location B'].Productivity['Sector 1'] = p_shock        

    print('Shocking Productivity in Location B: X_b = ' + str(int(p_shock)))

    ## Simulate the last n steps
    s_stats_loc, s_stats_firm, s_stats_labor, s_stats_network = world.simulate(steps = n_steps_post, 
                                                                               is_shock = True, 
                                                                               time_offset = n_steps_pre,
                                                                               alpha = labor_search, 
                                                                               beta = firm_search,
                                                                               start_firm = entrepenuer)

    
    ## Update the "steps" and bind the shocked data frames to the initial data frames
    s_stats_loc['Time Step'] = s_stats_loc['Time Step']
    s_stats_firm['Time Step'] = s_stats_firm['Time Step']
    # s_stats_labor['Time Step'] = s_stats_labor['Time Step']
    s_stats_network['Time Step'] = s_stats_network['Time Step']


    ## Need to drop the first step 
    stats_loc = pd.concat([stats_loc, s_stats_loc], ignore_index = True)
    stats_firm = pd.concat([stats_firm, s_stats_firm], ignore_index = True)
    # stats_labor = pd.concat([stats_labor, s_stats_labor], ignore_index = True)
    stats_network = pd.concat([stats_network, s_stats_network], ignore_index = True)


    ## Write these files to csvs in the results folder for looking at in R
    stats_loc.to_csv(write_path + r'\Case_1' + r'\location_results_' + str(ii) + '.csv')
    stats_firm.to_csv(write_path + r'\Case_1' + r'\firm_results_' + str(ii) + '.csv')
    # stats_labor.to_csv(write_path + r'\Case_1' + r'\labor_results_' + str(ii) + '.csv') # These are pretty big, let's avoid writing them...
    stats_network.to_csv(write_path + r'\Case_1' + r'\network_results_' + str(ii) + '.csv')


"""
World 2: labor is perfectly mobile (s=0)
"""
print('Running Case 2: Labor is Perfectly Mobile')
for ii in range(runs):   
    
    print(ii)
    
    new_seed = seed + ii
    random.seed(new_seed)
    np.random.seed(new_seed)

    world = lf.Economy()
    
    ## Add Locations
    world.addLocation(housing_elasticity = .5, local_ammenities = np.log(4), productivity_shifter = {'Sector 1': 6}, location_ID = "Location A")
    world.addLocation(housing_elasticity = .5, local_ammenities = np.log(4), productivity_shifter = {'Sector 1': 6}, location_ID = "Location B")
    
    
    ### Add firms in one sectors
    world.addAllFirms(n_firms = n_firms, 
                      loc_pr = {'Location A': .7, 'Location B': .3},
                      sec_pr = {'Sector 1': 1},
                      # remote_prefs = {'Sector 1': .2, 'Sector 2': .5}
                      remote_prefs = None                      
                      )

    
    ### Split the workers in half, setting their mobility such that they migrate fully
    world.addLabor(n_workers = n_labor,
                   location_prefs = {'Location A': [0, 0], 'Location B': [0, 0]}, 
                   location_prop = {'Location A': .6, 'Location B': .4},
                   sector_prop = {'Sector 1': 1},
                   remote_prefs = [.8, .8],
                   # remote_prefs = None,
                   unemployment_rate = .05,
                   firm_eta = .8)
    


    stats_loc, stats_firm, stats_labor, stats_network = world.simulate(steps = n_steps_pre,
                                                                       alpha = labor_search, 
                                                                       beta = firm_search,
                                                                       start_firm = entrepenuer) 


    ## Update remote tolerances for workers
    f_p = {k: f.profit for k, f in world.Firms.items()}                       # Put just the profits in a dictionary
    f_change = [f[0] for f in sorted(f_p.items(), key = lambda p: p[1])[:10]] # Gets the keys (i.e. firm names) for the first 10 firms with the smallest profits
    for f in f_change:

        new_tol = np.random.normal(.9, .075)
        world.Firms[f].updateRemotePref(new_tol)


    ## Shock technology in city b
    p_shock = 10
    world.Locations['Location B'].Productivity['Sector 1'] = p_shock        

    print('Shocking Productivity in Location B: X_b = ' + str(int(p_shock)))

    ## Simulate the last n steps
    s_stats_loc, s_stats_firm, s_stats_labor, s_stats_network = world.simulate(steps = n_steps_post, 
                                                                               is_shock = True, 
                                                                               time_offset = n_steps_pre,
                                                                               alpha = labor_search, 
                                                                               beta = firm_search,
                                                                               start_firm = entrepenuer)

    
    ## Update the "steps" and bind the shocked data frames to the initial data frames
    s_stats_loc['Time Step'] = s_stats_loc['Time Step']
    s_stats_firm['Time Step'] = s_stats_firm['Time Step']
    # s_stats_labor['Time Step'] = s_stats_labor['Time Step']
    s_stats_network['Time Step'] = s_stats_network['Time Step']


    ## Need to drop the first step 
    stats_loc = pd.concat([stats_loc, s_stats_loc], ignore_index = True)
    stats_firm = pd.concat([stats_firm, s_stats_firm], ignore_index = True)
    # stats_labor = pd.concat([stats_labor, s_stats_labor], ignore_index = True)
    stats_network = pd.concat([stats_network, s_stats_network], ignore_index = True)


    ## Write these files to csvs in the results folder for looking at in R
    stats_loc.to_csv(write_path + r'\Case_2' + r'\location_results_' + str(ii) + '.csv')
    stats_firm.to_csv(write_path + r'\Case_2' + r'\firm_results_' + str(ii) + '.csv')
    # stats_labor.to_csv(write_path + r'\Case_2' + r'\labor_results_' + str(ii) + '.csv') # These are pretty big, let's avoid writing them...
    stats_network.to_csv(write_path + r'\Case_2' + r'\network_results_' + str(ii) + '.csv')






end = time.time()
print(end - start)

