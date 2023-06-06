# -*- coding: utf-8 -*-
"""
Replicate Section 3.2.3: Incidence: Changes in Wage and Utility Inequality
                         for Heterogenous Labor (2 sectors)

Here, run to equilibrium, then shock a location for skilled workers only:
    - Increase X_c 
    - Increase X_ic as an extension (potentially)

There are two sectors: skilled and unskilled
    - Two cases:
        - Even split
        - Unbalanced (more unskilled than skilled)


Moretti notes that differences between labor supply elasticities can also drive outcomes...
Also notes the mobility differences 
    - Can test this by changing location preference for unskilled only
        - Should see worse gains in real wage
    - Similarly, low mobility for skilled labor has the same effect

p26: "The difference in nominal wages between skilled and unksilled workers increases nationwide more than the difference in utility between skilled and unskilled workers"

Further, following Moretti, assume the two cities are identifical in period 1. Additionally:
    - set k_a/k_b equal 
        - Initially set a low value (.5)
        - Same run with a slightly higher value (2)
    - set idiosyncratic preferences relatively equal
    - set Ammeinities equal as well

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

## Set parameters in a dictionary - write these to a log file after writing results



## To Do: store edge list of each network as an output (maybe as a string with some common sep)

## Set base seed
seed = 202207
write_path = r'C:\Users\zachm\Documents\George Mason\Projects\Labor Flows Paper\Results\3_2_Heterogenous_Labor\3_2_3_incidence'

start = time.time()

n_steps_pre = 200  # How many steps should be in the model pre shock?
n_steps_post = 400 # How many steps should be in the model post shock?

runs = 50     # How many simulation runs should be done for each world?

"""
Case 1: Low K, even skilled/unskilled populations
"""
for ii in range(runs):   
    
    print(ii)
    
    new_seed = seed + ii
    random.seed(new_seed)
    np.random.seed(new_seed)
    
    
    world = lf.Economy()
    
    ## Add Locations
    world.addLocation(housing_elasticity = .5, local_ammenities = np.log(4), productivity_shifter = {'skilled': 6, 'unskilled': 6}, location_ID = "Location A")
    world.addLocation(housing_elasticity = .5, local_ammenities = np.log(4), productivity_shifter = {'skilled': 6, 'unskilled': 6}, location_ID = "Location B")
    
    
    ### Add firms in one sectors
    world.addAllFirms(n_firms = n_firms, 
                      loc_pr = {'Location A': .7, 'Location B': .3},
                      sec_pr = {'skilled': .5, "unskilled": .5},
                      # remote_prefs = {'Sector 1': .2, 'Sector 2': .5}
                      remote_prefs = None
                      )


    ## Add some firm-specific technology/productivity shift...
    ## This would perhaps be more instructive if selected from a random uniform/random normal distribution...
    for f in world.Firms.values():
        f.productivity_shift = 2.5 # note that 3 gave me decent dynamics, as did 2


    ## Add workers
    world.addLabor(n_workers = n_labor,
                   location_prefs = {'Location A': [0, 1.5], 'Location B': [-1.5, 1.5]}, 
                   location_prop = {'Location A': .7, 'Location B': .3},
                   sector_prop = {'skilled': .5, "unskilled": .5},
                   remote_prefs = [.8, .8],
                   # remote_prefs = None,
                   unemployment_rate = .05,
                   firm_eta = .8)
    
    
    stats_loc, stats_firm, stats_labor, stats_network = world.simulate(steps = n_steps_pre,
                                                                       alpha = labor_search, beta = firm_search) 


    ## Update remote tolerances for workers
    """ for f in test.Firms.values():
        
        ## Get the tolerance from a gaussian ditribution based on sector
        if f.sector == 'Sector 1':
            
            new_tol = np.random.normal(.1, .075)
        
        elif f.sector == 'Sector 2':
            
            new_tol = np.random.normal(.9, .075)
        
        
        ## Bound the values between 0 and 1 and udpate the firm
        new_tol = min(max(0, new_tol), 1)    
        f.updateRemotePref(new_tol) """

    ## Shock technology in city b
    p_shock = 8
    world.Locations['Location B'].Productivity['skilled'] = p_shock        

    print('Shocking Productivity in Location B: X_b = ' + str(int(p_shock)))

    ## Simulate the last n steps
    s_stats_loc, s_stats_firm, s_stats_labor, s_stats_network = world.simulate(steps = n_steps_post, is_shock = True, time_offset = n_steps_pre,
                                                                               alpha = labor_search, beta = firm_search)

    
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
    # stats_labor.to_csv(fpath + r'\Case_1' + r'\labor_results_' + str(ii) + '.csv') # These are pretty big, let's avoid writing them...
    stats_network.to_csv(write_path + r'\Case_1' + r'\network_results_' + str(ii) + '.csv')
    
    
    ## Instead of writing the whole firm stats to csv, just get the mean results per sector and location, per time step and write those
    # stats_firm.to_csv(write_path + r'\firm_results_' + str(ii) + '.csv')


"""
Case 2: Low K, uneven skilled/unskilled populations
"""
for ii in range(runs):   
    
    print(ii)
    
    new_seed = seed + ii
    random.seed(new_seed)
    np.random.seed(new_seed)
    
    
    world = lf.Economy()
    
    ## Add Locations
    world.addLocation(housing_elasticity = .5, local_ammenities = np.log(4), productivity_shifter = {'skilled': 6, 'unskilled': 6}, location_ID = "Location A")
    world.addLocation(housing_elasticity = .5, local_ammenities = np.log(4), productivity_shifter = {'skilled': 6, 'unskilled': 6}, location_ID = "Location B")
    
    
    ### Add firms in one sectors
    world.addAllFirms(n_firms = n_firms, 
                      loc_pr = {'Location A': .7, 'Location B': .3},
                      sec_pr = {'skilled': .5, "unskilled": .5},
                      # remote_prefs = {'Sector 1': .2, 'Sector 2': .5}
                      remote_prefs = None
                      )


    ## Add some firm-specific technology/productivity shift...
    ## This would perhaps be more instructive if selected from a random uniform/random normal distribution...
    for f in world.Firms.values():
        f.productivity_shift = 2.5 # note that 3 gave me decent dynamics, as did 2


    ## Add workers
    world.addLabor(n_workers = n_labor,
                   location_prefs = {'Location A': [0, 1.5], 'Location B': [-1.5, 1.5]}, 
                   location_prop = {'Location A': .7, 'Location B': .3},
                   sector_prop = {'skilled': .3, "unskilled": .7},
                   remote_prefs = [.8, .8],
                   # remote_prefs = None,
                   unemployment_rate = .05,
                   firm_eta = .8)
    
    
    stats_loc, stats_firm, stats_labor, stats_network = world.simulate(steps = n_steps_pre,
                                                                       alpha = labor_search, beta = firm_search) 


    ## Update remote tolerances for workers
    """ for f in test.Firms.values():
        
        ## Get the tolerance from a gaussian ditribution based on sector
        if f.sector == 'Sector 1':
            
            new_tol = np.random.normal(.1, .075)
        
        elif f.sector == 'Sector 2':
            
            new_tol = np.random.normal(.9, .075)
        
        
        ## Bound the values between 0 and 1 and udpate the firm
        new_tol = min(max(0, new_tol), 1)    
        f.updateRemotePref(new_tol) """

    ## Shock technology in city b
    p_shock = 8
    world.Locations['Location B'].Productivity['skilled'] = p_shock        

    print('Shocking Productivity in Location B: X_b = ' + str(int(p_shock)))

    ## Simulate the last n steps
    s_stats_loc, s_stats_firm, s_stats_labor, s_stats_network = world.simulate(steps = n_steps_post, is_shock = True, time_offset = n_steps_pre,
                                                                               alpha = labor_search, beta = firm_search)

    
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
    # stats_labor.to_csv(fpath + r'\Case_2' + r'\labor_results_' + str(ii) + '.csv') # These are pretty big, let's avoid writing them...
    stats_network.to_csv(write_path + r'\Case_2' + r'\network_results_' + str(ii) + '.csv')
    
    
    ## Instead of writing the whole firm stats to csv, just get the mean results per sector and location, per time step and write those
    # stats_firm.to_csv(write_path + r'\firm_results_' + str(ii) + '.csv')


"""
Case 3: High K, even skilled/unskilled populations
"""
for ii in range(runs):   
    
    print(ii)
    
    new_seed = seed + ii
    random.seed(new_seed)
    np.random.seed(new_seed)
    
    
    world = lf.Economy()
    
    ## Add Locations
    world.addLocation(housing_elasticity = 2, local_ammenities = np.log(4), productivity_shifter = {'skilled': 6, 'unskilled': 6}, location_ID = "Location A")
    world.addLocation(housing_elasticity = 2, local_ammenities = np.log(4), productivity_shifter = {'skilled': 6, 'unskilled': 6}, location_ID = "Location B")
    
    
    ### Add firms in one sectors
    world.addAllFirms(n_firms = n_firms, 
                      loc_pr = {'Location A': .7, 'Location B': .3},
                      sec_pr = {'skilled': .5, "unskilled": .5},
                      # remote_prefs = {'Sector 1': .2, 'Sector 2': .5}
                      remote_prefs = None
                      )


    ## Add some firm-specific technology/productivity shift...
    ## This would perhaps be more instructive if selected from a random uniform/random normal distribution...
    for f in world.Firms.values():
        f.productivity_shift = 2.5 # note that 3 gave me decent dynamics, as did 2


    ## Add workers
    world.addLabor(n_workers = n_labor,
                   location_prefs = {'Location A': [0, 1.5], 'Location B': [-1.5, 1.5]}, 
                   location_prop = {'Location A': .7, 'Location B': .3},
                   sector_prop = {'skilled': .5, "unskilled": .5},
                   remote_prefs = [.8, .8],
                   # remote_prefs = None,
                   unemployment_rate = .05,
                   firm_eta = .8)
    
    
    stats_loc, stats_firm, stats_labor, stats_network = world.simulate(steps = n_steps_pre,
                                                                       alpha = labor_search, beta = firm_search) 


    ## Update remote tolerances for workers
    """ for f in test.Firms.values():
        
        ## Get the tolerance from a gaussian ditribution based on sector
        if f.sector == 'Sector 1':
            
            new_tol = np.random.normal(.1, .075)
        
        elif f.sector == 'Sector 2':
            
            new_tol = np.random.normal(.9, .075)
        
        
        ## Bound the values between 0 and 1 and udpate the firm
        new_tol = min(max(0, new_tol), 1)    
        f.updateRemotePref(new_tol) """

    ## Shock technology in city b
    p_shock = 8
    world.Locations['Location B'].Productivity['skilled'] = p_shock        

    print('Shocking Productivity in Location B: X_b = ' + str(int(p_shock)))

    ## Simulate the last n steps
    s_stats_loc, s_stats_firm, s_stats_labor, s_stats_network = world.simulate(steps = n_steps_post, is_shock = True, time_offset = n_steps_pre,
                                                                               alpha = labor_search, beta = firm_search)

    
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
    stats_loc.to_csv(write_path + r'\Case_3' + r'\location_results_' + str(ii) + '.csv')
    stats_firm.to_csv(write_path + r'\Case_3' + r'\firm_results_' + str(ii) + '.csv')
    # stats_labor.to_csv(fpath + r'\Case_3' + r'\labor_results_' + str(ii) + '.csv') # These are pretty big, let's avoid writing them...
    stats_network.to_csv(write_path + r'\Case_3' + r'\network_results_' + str(ii) + '.csv')
    
    
    ## Instead of writing the whole firm stats to csv, just get the mean results per sector and location, per time step and write those
    # stats_firm.to_csv(write_path + r'\firm_results_' + str(ii) + '.csv')


"""
Case 4: high K, uneven skilled/unskilled populations
"""
for ii in range(runs):   
    
    print(ii)
    
    new_seed = seed + ii
    random.seed(new_seed)
    np.random.seed(new_seed)
    
    
    world = lf.Economy()
    
    ## Add Locations
    world.addLocation(housing_elasticity = 2, local_ammenities = np.log(4), productivity_shifter = {'skilled': 6, 'unskilled': 6}, location_ID = "Location A")
    world.addLocation(housing_elasticity = 2, local_ammenities = np.log(4), productivity_shifter = {'skilled': 6, 'unskilled': 6}, location_ID = "Location B")
    
    
    ### Add firms in one sectors
    world.addAllFirms(n_firms = n_firms, 
                      loc_pr = {'Location A': .7, 'Location B': .3},
                      sec_pr = {'skilled': .5, "unskilled": .5},
                      # remote_prefs = {'Sector 1': .2, 'Sector 2': .5}
                      remote_prefs = None
                      )


    ## Add some firm-specific technology/productivity shift...
    ## This would perhaps be more instructive if selected from a random uniform/random normal distribution...
    for f in world.Firms.values():
        f.productivity_shift = 2.5 # note that 3 gave me decent dynamics, as did 2


    ## Add workers
    world.addLabor(n_workers = n_labor,
                   location_prefs = {'Location A': [0, 1.5], 'Location B': [-1.5, 1.5]}, 
                   location_prop = {'Location A': .7, 'Location B': .3},
                   sector_prop = {'skilled': .3, "unskilled": .7},
                   remote_prefs = [.8, .8],
                   # remote_prefs = None,
                   unemployment_rate = .05,
                   firm_eta = .8)
    
    
    stats_loc, stats_firm, stats_labor, stats_network = world.simulate(steps = n_steps_pre,
                                                                       alpha = labor_search, beta = firm_search) 


    ## Update remote tolerances for workers
    """ for f in test.Firms.values():
        
        ## Get the tolerance from a gaussian ditribution based on sector
        if f.sector == 'Sector 1':
            
            new_tol = np.random.normal(.1, .075)
        
        elif f.sector == 'Sector 2':
            
            new_tol = np.random.normal(.9, .075)
        
        
        ## Bound the values between 0 and 1 and udpate the firm
        new_tol = min(max(0, new_tol), 1)    
        f.updateRemotePref(new_tol) """

    ## Shock technology in city b
    p_shock = 8
    world.Locations['Location B'].Productivity['skilled'] = p_shock        

    print('Shocking Productivity in Location B: X_b = ' + str(int(p_shock)))

    ## Simulate the last n steps
    s_stats_loc, s_stats_firm, s_stats_labor, s_stats_network = world.simulate(steps = n_steps_post, is_shock = True, time_offset = n_steps_pre,
                                                                               alpha = labor_search, beta = firm_search)

    
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
    stats_loc.to_csv(write_path + r'\Case_4' + r'\location_results_' + str(ii) + '.csv')
    stats_firm.to_csv(write_path + r'\Case_4' + r'\firm_results_' + str(ii) + '.csv')
    # stats_labor.to_csv(fpath + r'\Case_4' + r'\labor_results_' + str(ii) + '.csv') # These are pretty big, let's avoid writing them...
    stats_network.to_csv(write_path + r'\Case_4' + r'\network_results_' + str(ii) + '.csv')
    
    
    ## Instead of writing the whole firm stats to csv, just get the mean results per sector and location, per time step and write those
    # stats_firm.to_csv(write_path + r'\firm_results_' + str(ii) + '.csv')