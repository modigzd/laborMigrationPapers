"""
Run multiple scripts in a row

This is necessary if changes are made to the underlying package (seems to happen often...) 
"""


## Setting up a logging file
import logging

log_file = r'C:\Users\zachm\Documents\George Mason\Projects\Labor Flows Paper\Results\Run_All_Log.txt'

logging.basicConfig(filename = log_file,
                    filemode = 'a',
                    format = '%(asctime)s,%(msecs)d %(name)s %(levelname)s %(message)s',
                    datefmt = '%H:%M:%S',
                    level = logging.DEBUG
                    )


import time


base_path = r'C:\Users\zachm\Documents\George Mason\Projects\Labor Flows Paper\Python Scripts'

start = time.time()
"""
Section 3.1 Scripts
"""
rel_path = r'\3_1_Homogenous_Labor'

logging.info("Begin Section 3.1")

logging.info("Running Section 3.1.2: Labor Demand Shock")
exec(open(base_path + rel_path + r'\3_1_2_labor_demand_shock.py').read())

logging.info("Running Section 3.1.3: Incidence")
exec(open(base_path + rel_path + r'\3_1_3_incidence.py').read())
# exec(open(base_path + rel_path + r'\3_1_4_labor_supply_shock.py').read())

## WFH
rel_path = r'\3_1_WFH_extension'

logging.info("Begin 3.1: WFH Extension")


logging.info("Running Section 3.1.2: Labor Demand Shock (WFH)")
exec(open(base_path + rel_path + r'\3_1_2_labor_demand_shock.py').read())

logging.info("Running Section 3.1.3: Incidence (WFH)")
exec(open(base_path + rel_path + r'\3_1_3_incidence.py').read())

logging.info("Running Section 3.1.3: Firm Policy (WFH)")
exec(open(base_path + rel_path + r'\3_1_3_firm_policy.py').read())


end = time.time()
print('Section 1 Total Time: ')
print(end - start)


start2 = time.time()
"""
Section 3.2 Scripts
"""
rel_path = r'\3_2_Heterogenous_Labor'
logging.info("Begin Section 3.2")


logging.info("Running Section 3.2.2: Labor Demand Shock")
exec(open(base_path + rel_path + r'\3_2_2_labor_demand_shock.py').read())
# exec(open(base_path + rel_path + r'\3_2_2_with_firm_technology.py').read())
# logging.info("Running Section 3.2.3: Incidence")
# exec(open(base_path + rel_path + r'\3_2_3_incidence.py').read())

## WFH
rel_path = r'\3_2_WFH_extension'

logging.info("Begin Section 3.2 - WFH Extension")

logging.info("Running Section 3.2.2: Labor Demand Shock (WFH)")
exec(open(base_path + rel_path + r'\3_2_2_labor_demand_shock.py').read())
# exec(open(base_path + rel_path + r'\3_2_2_with_firm_technology.py').read())
# logging.info("Running Section 3.2.2: Incidence (WFH)")
# exec(open(base_path + rel_path + r'\3_2_3_incidence.py').read())


end = time.time()
print('Section 2 Total Time: ')
print(end - start2)


print("Overall Time: ")
print(end - start)
