#### PAT VERSION 2.7 ####
## To run PAT as a standalone python script lines 18-57 should be uncommented and input parameters specified. In additon line 1790 should be uncommented.
## To run PAT as a GUI, lines 18-57 should be commented along with line 1790. Inputs are specified in the GUI.

# Notes:
# *** FORMATION DECLINE WARNING ***
# When running formation decline no additional formation (in the TPEZ) or decline (APEZ, TPEZ, WPEZ) are being calculated outside of PWC. This may result in an underprediction of daughter compounds. Please email Jerrett Fowler at fowler.jerrett@epa.gov for more information.

### ENVIRONMENTAL SET-UP, DO NOT MODIFY ###
import os
import pandas as pd
import numpy as np
import datetime as dt
from functools import reduce

### USER INPUTS ####

#input_dir = os.path.join(r"C:\Users\gdykes\OneDrive - Environmental Protection Agency (EPA)\Desktop\Test_10")  # directory where PAT input files are located
#output_dir = os.path.join(input_dir, "GED_test_10_c_1182022")  # directory for all output files
#if not os.path.exists(output_dir):
#    os.mkdir(output_dir)
#make_figs = False  # True if you want to create figures
#batch_run = True # True if you are doing a batch run
#batch_file = os.path.join(input_dir, "PAT Batch Run_florylpicoxamid.csv")  # if doing batch run, name of input file
#use_pwc_batch = False  # use this flag to let program know that you will be using the PWC batch file for inputs (i.e you ran an external batch PWC file)
#use_pwc_internal_batch = True # let the program know if you used an internal batch run in PWC
#scen_dir = "C:/Users/gdykes/OneDrive - Environmental Protection Agency (EPA)/Templates, formatting, & guidance/Old scenarios"  # if using PWC external batch or PWC internal batch, location of ESA scenarios
#sdf_file = os.path.join(input_dir, "sdf.csv")  # file for spray drift info, normally you don't have to change
#endpoints = os.path.join(input_dir, "PAT plant endpoints Florylpicoxamid_v2.csv")  # file name with plant endpoints
#tsum = ""  # PAT terrestrial summary file, used for batch runs
#tsum30 = ""  # PAT terrestrial 30 year summary file, used for batch runs
#ssum = ""  # PAT semiaquatic summary file, used for batch runs
#ssum30 = ""  # PAT terrestrial 30 year summary file, used for batch runs
#rq_sum_saq = "" # RQ summary file for wetland
#rq_sum_ter = "" # RQ summary file for terrestrial
#rq_sum_aq = "" # RQ summary file for aqueous
#options = "t"  # PAT options = t = terrestrial, a = aquatics, s = semiaquatic, tsa = all, order of the letters doesn't matter
#esa = False  # flag if doing esa or fifra run - fifra - 1 in 10, esa 1 in 15
#save_terr = False  # save daily terrestrial results? files are typically 22 MB
#save_saq = False  # save daily wetlands runs? files are typically 1 MB

## if doing a single run, file names in following variables
#run_name = "test6"  # name of PAT output file
#zts_file = os.path.join(input_dir, "IAcornstd_+0_Holly_QAQC_internal.zts") # name of zts file
#pwc_input = os.path.join(input_dir, "Holly_QAQC_pond.PWC") # name of .PWC file
#semi_csv = os.path.join(input_dir, "Holly_QAQC_wetland_IAcornstd_Custom_Parent_daily.csv")  # Need to make sure this input matches the formation decline chemical product you are running
#aq_csv = os.path.join(input_dir,"Holly_QAQC_pond_IAcornstd_Pond_Parent_daily.csv")  # Need to make sure this input matches the formation decline chemical product you are running
#app_method = "Ground_Fine_to_Medium/Coarse_High_Boom_90th" # application method for spray drift. Must match one of the columns in the sdf.csv file
#fd = 0  # Formation decline variable: input 0 for parent, input 1 for daughter and 2 for grand-daughter. If no fd, set to 0
#buffer = 0  # buffer distance, in meters, if applicable

def resettable(f):
    import copy

    def __init_and_copy__(self, *args, **kwargs):
        f(self, *args)
        self.__original_dict__ = copy.deepcopy(self.__dict__)

        def reset(o = self):
            o.__dict__ = o.__original_dict__

        self.reset = reset

    return __init_and_copy__

class PlantEndpoints(object):
    def __init__(self, type="", level="", plant=0, effect=0, indicator="", val=0, mrid=""):
        self.type = type
        self.level = level
        self.plant = plant
        self.effect = effect
        self.indicator = indicator
        self.val = val
        self.mrid = mrid

class PWCInputs(object):
    @resettable
    def __init__(self, chem_name="", soil_t12=1e8, foliar_t12=1e8, washoff=0.5, Kd=0, num_apps=1, app_days=[], app_months=[],
                 app_rate=[], scenario="", app_method="Ground_Very_Fine_to_Fine_Low_Boom_90th", fd_run=0):
        self.chem_name = chem_name
        self.soil_t12 = soil_t12
        self.foliar_t12 = foliar_t12
        self.washoff = washoff
        self.Kd = Kd
        self.num_apps = num_apps
        self.app_days = app_days
        self.app_months = app_months
        self.app_rates = app_rate
        self.scenario = scenario
        self.app_method = app_method
        self.fd_run = fd_run

class SemiPWCInputs(object):
    @resettable
    def __init__(self, depth=0.15, volume_sed=1500, bulk_density=0, porosity=0.5):
        self.depth = depth
        self.volume_sed = volume_sed
        self.bulk_density = bulk_density
        self.porosity = porosity
        self.volume_water = volume_sed * porosity

class hydro_inputs:
    @resettable
    def __init__(self, max_hold_cap=0, min_hold_cap=0, avail_cap=0, bulk_density=0, soil_mass=0):
        self.max_hold_cap = max_hold_cap
        self.min_hold_cap = min_hold_cap
        self.avail_cap = avail_cap
        self.bulk_density = bulk_density
        self.soil_mass = soil_mass

class geo_inputs:
    @resettable
    def __init__(self, sf_width=30, side_length=316.228, depth=15, pez_cap=0, eff_area = 0):
        self.sf_width = sf_width
        self.side_length = side_length
        self.depth = depth
        self.pez_area = sf_width * side_length
        self.pez_volume = sf_width * side_length * depth/100
        self.pez_cap = pez_cap
        self.eff_area = eff_area

# summary output file
# opts 1 and 2 are for 1-in-15 year estimates
# opts 3 and 4 are for all 30 years of values
class OutputTable(object):
    def __init__(self, out_dir, out_file, opt):
        # Initialize path
        self.dir = out_dir
        self.base = out_file
        self.path = os.path.join(self.dir, self.base)

        # Initialize header
        if opt == 1:
            self.header = ["Line", "Batch Run ID", "Scenario", "HUC2", "Bin", "EEC runoff only (lb/A)",
                           "EEC runoff+drift_15 m (lb/A)"]
        elif opt == 2:
            self.header = ["Line", "Batch Run ID", "Scenario", "HUC2", "Bin", "EEC (ug/L)", "EEC (lb/A)"]
        elif opt == 3:
            self.header = ["Line", "Batch Run ID", "Scenario", "HUC2", "Bin"] + \
                ["{}_Year {}".format(scen, yr) for scen in ("EEC runoff only (lb/A)", "EEC runoff+drift_15 m (lb/A)") \
                 for yr in range(1,31)]
        else:
            self.header = ["Line", "Batch Run ID", "Scenario", "HUC2", "Bin"] + \
                          ["{}_Year {}".format(scen, yr) for scen in ("EEC (ug/L)", "EEC (lb/A)") for yr in range(1, 31)]

        # Initialize file
        self.initialize_table()

    def initialize_table(self):
        # Create output directory if it does not exist
        if not os.path.exists(self.dir):
            os.mkdir(self.dir)

        # Create file and write header
        with open(self.path, 'w') as f:
            f.write(",".join(self.header) + "\n")

    def update_table(self, line, run_id, yr, scen, huc, bin, val1, val2):
        """ Insert run results into table """

        if yr == 0:
            out_data = [line, run_id, scen, huc, bin, "{:.2E}".format(val1), "{:.2E}".format(val2)]
        else:
            # modified 2/4/22 - cap - designed to accommodate those scenarios that have less than 30 years
            # If you don't have 32 years of data
            if len(val1) != 32:
                out_data = [line, run_id, scen, huc, bin]
                dd = yr - 1961
                # If the year is 1961
                if dd == 0:
                    # For all the values but the last two, add the formatted val to out_data
                    for xx in range(0, len(val1) - 2):
                        tval = format(val1[xx], ".2E")
                        out_data += [tval]
                    # For values between less than 2 of all the values and 30
                    for xx in range(len(val1) - 2, 30):
                        # The outdata is blank
                        out_data += [" "]
                    # For all values but the last two, fromat and add the value to out_data
                    for xx in range(0, len(val2) - 2):
                        tval = format(val2[xx], ".2E")
                        out_data += [tval]
                    # For values between less than 2 of all the values and 30, the outdata is blank
                    for xx in range(len(val2) - 2, 30):
                        out_data += [" "]
                # If you do have 32 years of data
                else:
                    # For all the extra years of data beyond 1961
                    for xx in range(0, dd):
                        # out_data is blank
                        out_data += [" "]
                    # For the extra years of data up to 30 years
                    for xx in range(dd, 30):
                        tval = format(val1[xx - dd], ".2E")
                        out_data += [tval]
                    # For each extra year of data
                    for xx in range(0, dd):
                        out_data += [" "]
                    # For the extra years of data up to 30 years
                    for xx in range(dd, 30):
                        tval = format(val2[xx - dd], ".2E")
                        out_data += [tval]
            # If you do have 30 years of data
            else:
                out_data = [line, run_id, scen, huc, bin] + ["%.2E" % member for member in list(val1[:-2])] + \
                           ["%.2E" % member2 for member2 in list(val2[:-2])]

        # Update file
        with open(self.path, 'a') as f:
            f.write(",".join(map(str, out_data)) + "\n")

# Wetland RQ table (concatenates all RQ results for the WPEZ from all scenarios)
class OutputTableRQ_saq(object):
    def __init__(self, out_dir, out_file):
        # Initialize path
        self.dir = out_dir
        self.base = out_file
        self.path = os.path.join(self.dir, self.base)

        # Initialize header
        self.header = ["SE/VV/AQ", "Species", "Wetlands EEC", "Non-listed Wetlands RQ", "Listed Wetlands RQ", "Scenario"]

        # Initialize file
        self.initialize_table()

    def initialize_table(self):
        # Create output directory if it does not exist
        if not os.path.exists(self.dir):
            os.mkdir(self.dir)

        # Create file and write header
        with open(self.path, 'w') as f:
            f.write(",".join(self.header) + "\n")

    def update_table(self, rq_df):
        """ Insert run results into table """
        # Update file
        rq_df.to_csv(self.path, mode='a', header = False, index=False)

# Aqueous RQ table (concatenates all RQ results for the APEZ from all scenarios)
class OutputTableRQ_aq(object):
    def __init__(self, out_dir, out_file):
        # Initialize path
        self.dir = out_dir
        self.base = out_file
        self.path = os.path.join(self.dir, self.base)

        # Initialize header
        self.header = ["AQ", "Species", "Aquatic EEC", "Non-listed Aquatic RQ", "Listed Aquatic RQ", "Scenario"]

        # Initialize file
        self.initialize_table()

    def initialize_table(self):
        # Create output directory if it does not exist
        if not os.path.exists(self.dir):
            os.mkdir(self.dir)

        # Create file and write header
        with open(self.path, 'w') as f:
            f.write(",".join(self.header) + "\n")

    def update_table(self, rq_df):
        """ Insert run results into table """
        # Update file
        rq_df.to_csv(self.path, mode='a', header = False, index=False)

# Terrestrial RQ table (concatenates all RQ results for the TPEZ from all scenarios)
class OutputTableRQ_ter(object):
    def __init__(self, out_dir, out_file):
        # Initialize path
        self.dir = out_dir
        self.base = out_file
        self.path = os.path.join(self.dir, self.base)

        # Initialize header
        # TPEZ average may be 15m from edge of field
        # Use TPEZ average to compare to terrestrial plants in the wpez
        self.header = ["SE/VV", "Species", "Runoff EEC (lb/A)", "Runoff Non-listed RQ", "Runoff Listed RQ",
                       "Outer Edge EEC (lb/A)", "Outer Edge Non-listed RQ", "Outer Edge Listed RQ", "Scenario"]

        # Initialize file
        self.initialize_table()

    def initialize_table(self):
        # Create output directory if it does not exist
        if not os.path.exists(self.dir):
            os.mkdir(self.dir)

        # Create file and write header
        with open(self.path, 'w') as f:
            f.write(",".join(self.header) + "\n")

    def update_table(self, rq_df):
        """ Insert run results into table """
        # Update file
        rq_df.to_csv(self.path, mode='a', header = False, index=False)

# Function to format TPEZ RQs into a format for risk assessments
# def format_terrestrial_plant(df2):
#     df_out = []
#     endpoint_types = ["VV_Dicot", "SE_Dicot", "VV_Monocot", "SE_Monocot"]
#     for etype in endpoint_types:
#         # filter
#         b = df2[df2["endpoint_type"] == etype]
#         b2 = b[["Scenario", "Outer Edge EEC (lb/A)", "Outer Edge Non-listed RQ", "Outer Edge Listed RQ"]]

#         # re-name columns
#         new_names = [(i, i + "_" + etype) for i in b2.iloc[:, 2:].columns.values]
#         b2.rename(columns=dict(new_names), inplace=True)

#         # append to df_out
#         df_out.append(b2)

#     # merge all dfs
#     merged_df = reduce(lambda l, r: pd.merge(l, r, on=['Scenario', 'Outer Edge EEC (lb/A)'], how='outer'), df_out)

#     return merged_df

# # Organize and save data for TPEZ RQ Summary
# def make_terrestrial_RQ_summary(input_dir, output_dir, rq_sum_ter):
#     try:
#         # Read in the wetland summary RQ file
#         terrestrial_summaryRQ = pd.read_csv(os.path.join(input_dir, rq_sum_ter.path))

#         # filter out non-definitive endpoints
#         #if isnumeric(terrestrial_summaryRQ[]) != True:

    
#         # Read in the plant info file
#         plant_class = pd.read_csv(os.path.join(input_dir, "Plant_classification_2.csv"))
    
#         # merge summary EECs with plant info
#         terrestrial_summaryRQ_plant = terrestrial_summaryRQ.merge(plant_class, how="left", on="Species")
    
#         # Drop plants not in the tpez
#         tpez_plants = terrestrial_summaryRQ_plant[terrestrial_summaryRQ_plant["TPEZ"] == "Yes"]
    
#         # group by scenario, endpoint type, cotelydons & get max RQ
#         df1 = tpez_plants.groupby(["Scenario", "SE/VV", "Monocot_or_Dicot"]).max()
    
#         df1.reset_index(inplace=True)
    
#         # concat endpoint type & cotelydons
#         df1["endpoint_type"] = df1["SE/VV"] + "_" + df1["Monocot_or_Dicot"]
    
#         # drop meaningless columns
#         df2 = df1[["Scenario", "endpoint_type", "Outer Edge EEC (lb/A)", "Outer Edge Non-listed RQ", "Outer Edge Listed RQ"]]
    
#         # format plant
#         df = format_terrestrial_plant(df2)
    
#         # outfile
#         outfile = os.path.join(output_dir, "PAT TPEZ RQ Summary.csv")
    
#         # Save out
#         df.to_csv(path_or_buf=outfile, index=False)
#     except:
#         print("Could not make a TPEZ Summary RQ file. Please refer to the terrestrial RQ summary ALL file for RQs.")

# Format APEZ data into table that's close to suitable for risk assessment
# def format_aquatic_plant(df2):
#     df_out = []
#     endpoint_types = ["AQ_Non-Vascular", "AQ_Vascular"]
#     for etype in endpoint_types:
#         # filter
#         b = df2[df2["endpoint_type"] == etype]
#         b2 = b[["Scenario", "Aquatic EEC", "Non-listed Aquatic RQ", "Listed Aquatic RQ"]]
#         # re-name columns
#         new_names = [(i, i + "_" + etype) for i in b2.iloc[:, 2:].columns.values]
#         b2.rename(columns=dict(new_names), inplace=True)

#         # append to df_out
#         df_out.append(b2)

#     # merge all dfs
#     merged_df = reduce(lambda l, r: pd.merge(l, r, on=['Scenario', 'Aquatic EEC'], how='outer'), df_out)

#     return merged_df

# # Create the APEZ RQ summary
# def make_aquatic_RQ_summary(input_dir, output_dir, rq_sum_aq):
#     try:
#         # Read in the aquatic summary RQ file
#         aquatic_summaryRQ = pd.read_csv(os.path.join(output_dir, rq_sum_aq.path))
    
#         # Read in the plant info file
#         plant_class = pd.read_csv(os.path.join(input_dir, "Plant_classification_2.csv"))
    
#         # merge summary EECs with plant info
#         aquatic_summaryRQ_plant = aquatic_summaryRQ.merge(plant_class, how="left", on="Species")
    
#         # Drop plants not in the tpez
#         apez_plants = aquatic_summaryRQ_plant[aquatic_summaryRQ_plant["APEZ"] == "Yes"]
    
#         # group by scenario, endpoint type, cotelydons & get max RQ
#         df1 = apez_plants.groupby(["Scenario", "AQ", "Monocot_or_Dicot"]).max()
    
#         df1.reset_index(inplace=True)
    
#         # concat endpoint type & cotelydons
#         df1["endpoint_type"] = df1["AQ"] + "_" + df1["Monocot_or_Dicot"]
    
#         # drop meaningless columns
#         df2 = df1[["Scenario", "endpoint_type", "Aquatic EEC", "Non-listed Aquatic RQ", "Listed Aquatic RQ"]]
    
#         # format plant
#         df = format_aquatic_plant(df2)
    
#         # outfile
#         outfile = os.path.join(output_dir, "PAT APEZ RQ Summary.csv")
    
#         # Save out
#         df.to_csv(path_or_buf=outfile, index=False)
#     except:
#         print("Could not make an APEZ Summary RQ file. Please refer to the aquatic RQ summary ALL file for RQs.")

# Format WPEZ RQ data for TERRESTRIAL plants
# def format_wetland_plant_ter(df2):
#     df_out = []
#     endpoint_types = ["VV_Dicot", "SE_Dicot", "VV_Monocot", "SE_Monocot"]
#     for etype in endpoint_types:
#         # filter
#         b = df2[df2["endpoint_type"] == etype]
#         b2 = b[["Scenario", "Wetlands EEC", "Non-listed Wetlands RQ", "Listed Wetlands RQ"]]

#         # cols = df.columns[~df.columns.str.contains('col1|col2')]
#         # df.rename(columns = dict(zip(cols, cols + '_' + endpoint_type)), inplace=True)

#         new_names = [(i, i + "_" + etype) for i in b2.iloc[:, 2:].columns.values]
#         b2.rename(columns=dict(new_names), inplace=True)

#         # rename columns
#         # df.columns = np.where(df.columns.isin(df2.V1), 'DMR_' + df.columns, df.columns)
#         # append to df_out
#         df_out.append(b2)

#     # merge all dfs
#     merged_df = reduce(lambda l, r: pd.merge(l, r, on=['Scenario', 'Wetlands EEC'], how='outer'), df_out)

#     return merged_df

# # Format data for WPEZ RQ summary, for AQUEOUS PLANTS
# def format_wetland_plant_aq(df2):
#     df_out = []
#     endpoint_types = ["AQ_Non-Vascular", "AQ_Vascular"]
#     for etype in endpoint_types:
#         # filter
#         b = df2[df2["endpoint_type"] == etype]
#         b2 = b[["Scenario", "Wetlands EEC", "Non-listed Wetlands RQ", "Listed Wetlands RQ"]]

#         # cols = df.columns[~df.columns.str.contains('col1|col2')]
#         # df.rename(columns = dict(zip(cols, cols + '_' + endpoint_type)), inplace=True)

#         new_names = [(i, i + "_" + etype) for i in b2.iloc[:, 2:].columns.values]
#         b2.rename(columns=dict(new_names), inplace=True)

#         # rename columns
#         # df.columns = np.where(df.columns.isin(df2.V1), 'DMR_' + df.columns, df.columns)
#         # append to df_out
#         df_out.append(b2)

#     # merge all dfs
#     merged_df = reduce(lambda l, r: pd.merge(l, r, on=['Scenario', 'Wetlands EEC'], how='outer'), df_out)

#     return merged_df

# # Retrieve and format data for WPEZ RQ summary, for AQUEOUS PLANTS
# def make_wetland_RQ_summary(input_dir, output_dir, rq_sum_saq, t_or_a):
#     try:
#         # Read in the wetland summary RQ file
#         wetland_summaryRQ = pd.read_csv(os.path.join(input_dir, rq_sum_saq.path))
        
#         # Split out operators for non-definitive endpoints
#         wetland_summaryRQ['non-definitive operator_listed'] = wetland_summaryRQ['Listed Wetlands RQ'].str.extract('^(<)').fillna('')
#         wetland_summaryRQ['non-definitive operator_nonlisted'] = wetland_summaryRQ['Non-listed Wetlands RQ'].str.extract('^(<)').fillna('')

#         # Read in the plant info file
#         plant_class = pd.read_csv(os.path.join(input_dir, "Plant_classification_2.csv"))
    
#         # merge summary EECs with plant info
#         wetland_summaryRQ_plant = wetland_summaryRQ.merge(plant_class, how="left", on="Species")
    
#         # Drop plants not in the wpez
#         wpez_plants= wetland_summaryRQ_plant[(wetland_summaryRQ_plant["WPEZ"] == "Yes") & (wetland_summaryRQ_plant[t_or_a] == "Yes")]
    
#         # group by scenario, endpoint type, cotelydons & get max RQ
#         df1 = wpez_plants.groupby(["Scenario", "SE/VV/AQ", "Monocot_or_Dicot"]).max()
#         #df1 = wpez_plants.groupby(["Scenario", "SE/VV/AQ", "Monocot_or_Dicot"]).apply(lambda x: x.max(skipna=False))
    
#         df1.reset_index(drop=True, inplace=True)
    
#         # concat endpoint type & cotelydons
#         df1["endpoint_type"] = df1["SE/VV/AQ"] + "_" + df1["Monocot_or_Dicot"]
    
#         # drop meaningless columns
#         df2 = df1[["Scenario", "endpoint_type", "Wetlands EEC", "Non-listed Wetlands RQ", "Listed Wetlands RQ"]]
    
#         if t_or_a == "TPEZ":            # if we're looking at the terrestrial plants, format for WPEZ terrestrial plants (TPEZ might be confusing here, it doesn't have anything to do wiht the TPEZ just terrestrial plants. Same with APEZ below).
#             #format
#             df = format_wetland_plant_ter(df2)
#             # outfile
#             outfile = os.path.join(output_dir, "PAT WPEZ Terrestrial RQ Summary.csv")
#         elif t_or_a == "APEZ":          # if we're looking at the aquatic plants, format for WPEZ aquatic plants
#             #format
#             df = format_wetland_plant_aq(df2)
#             # outfile
#             outfile = os.path.join(output_dir, "PAT WPEZ Aquatic RQ Summary.csv")
#         # Save out
#         df.to_csv(path_or_buf=outfile, index=False)
#     except:
#         print("Could not make a WPEZ Summary RQ file. Please refer to the wetland RQ summary ALL file for RQs.")

# get inputs from OLD, SINGLE PWC run (SWI file, versions prior to PWC v 2.0)
def get_pwc_inputs(pwc_inputs, fd): #pass fd variable here to
    pwci = PWCInputs()
    pwci.reset()
    hi = hydro_inputs()
    hi.reset()
    d_abs = "True"

    with open(pwc_inputs) as ts:
        str1 = ts.readline().rstrip('\n')           # Read line 1
        str1 = ts.readline().rstrip('\n')           # Read line 2
        pwci.chem_name = str1                       # Chemical name is line 2
        str1 = ts.readline().rstrip('\n')           # Read line 3
        pwci.fd_run = int(str1) - 1                 # Fd variable is line 3, offset by 1

        str2 = ts.readline().rstrip('\n')           # Read line 4
        str1 = ts.readline().rstrip('\n')           # Read line 5
        tstr = str1.split(",")                      # Split line 5 by comma

        pwci.Kd = float(tstr[fd])                   # Kd is line 5, indexed by degredate of interest

        if str2 == "True":                          # If line 4 is true
            pwci.Kd *= 0.01                         # Convert Koc to Kd

        for n in range (6, 14):                     # For lines 6-13
            str1 = ts.readline().rstrip('\n')       # Read lines 6-13
        tstr = str1.split(",")                      # Split line 13 by comma

        pwci.soil_t12 = float(tstr[fd])             # Soil half-life is line 13, indexed by fd
        if pwci.soil_t12 == 0:                      # If the soil half-life is stable
            pwci.soil_t12 = 1e8                     # Set the soil half-life to 1e8

        str1 = ts.readline().rstrip('\n')           # Read line 14
        str1 = ts.readline().rstrip('\n')           # Read line 15
        tstr = str1.split(",")                      # Separate line 15 by comma

 #       if tstr[fd] == '0' or np.isnan(tstr[fd]):
        if tstr[fd] == '0' or tstr[fd] == '':       # If line 15 is 0 or blank
            pwci.foliar_t12 = 1e8                   # Set foliar half-life at 1e8
        else:
            pwci.foliar_t12 = float(tstr[fd])       # Otherwise line 15 is foliar half-life

        for n in  range(16, 30):                    # For line 16-29
            str1 = ts.readline().rstrip('\n')       # Read line 16-29
        pwci.num_apps = int(str1)                   # Number of applications is line 29

        str1 = ts.readline().rstrip('\n')           # Read line 30
        tstr = str1.split(",")                      # Split line 30 by comma
        for i in range(0,pwci.num_apps):            # For each application 
            if tstr[i] != "":                       # If the value indexed for line 30 isnt blank
                pwci.app_days.append(int(tstr[i]))  # The application day is line 30
            else:
                break

        str1 = ts.readline().rstrip('\n')           # Read line 31
        if pwci.app_days:                           # if app_days = True
            tstr = str1.split(",")                  # Split line 31 by comma
            for i in range(0,pwci.num_apps):        # For each application
                pwci.app_months.append(int(tstr[i]))# Line 31 is application month

        str1 = ts.readline().rstrip('\n')           # Read line 32
        tstr = str1.split(",")                      # Split line 32 by comma
        for i in range(0,pwci.num_apps):            # For each application
            pwci.app_rates.append(float(tstr[i]))   # Append the rate from line 32

        for n in  range(32, 43):                    # For lines 32-42
            str1 = ts.readline().rstrip('\n')       # Read lines 32-42

        tstr = str1.split(",")                      # Separate line 42 by comma
        str1 = ts.readline().rstrip('\n')           # Read line 43
        d_abs = str1                                # Absolute dates? in line 43

        for n in range(44, 49):                     # For lines 44-48
            str1 = ts.readline().rstrip('\n')       # Read lines 44-48
        pwci.scenario = str1                        # The scenario is line 48

        for n in range(50, 77):                     # For lines 50-76 (Probably not actually becaues line 49 has not been read yet, I do not have a SWI file to verify)
            str1 = ts.readline().rstrip('\n')       # Read lines 50-76

        i=78                                        # Set i = 78
        if d_abs == "False":                        # If you're working with relative dates
            e_day = int(str1)                       # The emergence date is line 76
            str1 = ts.readline().rstrip('\n')       # Read line 77
            e_month = int(str1)                     # The emergence month is line 77
            for i in range(0,pwci.num_apps):        # For each application
                dt2 = dt.date(1960, e_month, e_day) + pd.DateOffset(days=int(tstr[i]))  # Get the absolute date from the e_month and e_day
                pwci.app_months[i] = dt2.month      # Get the app month from dt2
                pwci.app_days[i] = dt2.day          # Get the app day from dt2
            i=79                                    # Set i =79

        for n in range(i, 103):                     # For lines 79-102 (Does this match up as intended)
            str1 = ts.readline().rstrip('\n')       # Read lines 79-102

        tstr = str1.split(",")                      # Split line 102
        hi.bulk_density = float(tstr[0])            # bulk density is line 102 column 0 
        str1 = ts.readline().rstrip('\n')           # Read line 103
        tstr = str1.split(",")                      # Split line 103
        hi.max_hold_cap = float(tstr[0])            # max holding capacity is line 103 column 0
        str1 = ts.readline().rstrip('\n')           # Read line 104
        tstr = str1.split(",")                      # Split line 104
        hi.min_hold_cap = float(tstr[0])            # minimum holding capacity is line 104 column 0
        hi.avail_cap = hi.max_hold_cap - hi.min_hold_cap    # Calculate available holding capacity

    ts.close()
    return hi, pwci


# get inputs from single PWC run (PWC v 2.0)
def get_pwc_inputs_single(pwc_inputs, fd):
    pwci = PWCInputs()
    pwci.reset()
    hi = hydro_inputs()
    hi.reset()
    d_abs = "True"
    # fd = ""

    with open(pwc_inputs) as ts:
        str1 = ts.readline().rstrip('\n')   # Read the PWC header (line 1)
        str1 = ts.readline().rstrip('\n')   # Read the chemical name (line 2)
        pwci.chem_name = str1               # Set the chemical name
        str1 = ts.readline().rstrip('\n')   # Read the fd variable (line 3)
        pwci.fd_run = int(str1) - 1         # Set the fd variable 

        str2 = ts.readline().rstrip('\n')   # Read the variable (line 4)
        str1 = ts.readline().rstrip('\n')   # Read line 5
        tstr = str1.split(",")  # pass df here and pull the appropriate value, split line 5 by comma 

        pwci.Kd = float(tstr[fd])
        if str2 == "True":              # If the Koc flag is true
            pwci.Kd *= 0.01             # Multiply by 0.01 to get a true Kd

        for n in range (6, 14):             # For lines 6-13...
            str1 = ts.readline().rstrip('\n')   # Read the line
        tstr = str1.split(",")              # Split the line (13) by comma

        pwci.soil_t12 = float(tstr[fd])     # Line 13 = soil halflife
        if pwci.soil_t12 == 0:              # If stable (i.e. 0 as input)
            pwci.soil_t12 = 1e8             # Replace with 1e8

        str1 = ts.readline().rstrip('\n')   # Read line 14
        str1 = ts.readline().rstrip('\n')   # Read line 15
        tstr = str1.split(",")              # Split line 15 by comma

        if tstr[fd] == '0' or tstr[fd] == '':  # JDF - added if else statements breaking out parent and degradate values
            pwci.foliar_t12 = 1e8           # Line 15 is foliar half life
        else:
            pwci.foliar_t12 = float(tstr[fd])

        for n in  range(16, 30):            # For lines 16-29...
            str1 = ts.readline().rstrip('\n')   # Read the line
        pwci.num_apps = int(str1)           # number of apps = line 29 as an integer

        str1 = ts.readline().rstrip('\n')   # For line 30
        abs_days = str1.split(",")          # Split line 30 by comma

        str1 = ts.readline().rstrip('\n')   # For line 31
        abs_months = str1.split(",")        # Split line 31 by comma

        str1 = ts.readline().rstrip('\n')   # Read line 32
        tstr = str1.split(",")              # Split 32 by comma
        for i in range(0,pwci.num_apps):
            pwci.app_rates.append(float(tstr[i]))   # Set app rate for each app

        for n in  range(33, 44):            # For line 33-43...
            str1 = ts.readline().rstrip('\n')   # Read line

        tstr0 = str1.split(",")             # Split line 43 by comma
        str1 = ts.readline().rstrip('\n')   # Read line 44
        tstr = str1.split(",")              # Split line 44 by comma
        d_abs = tstr[0]                     # absolute dates T/F from line 44 column 0
        emh = int(tstr[1])                  # emh (emergence maturity harvest) from line 44 column 1

        for n in range(45, 50):             # for line 45 to 49...
            str1 = ts.readline().rstrip('\n')   # read line
        pwci.scenario = str1                # scenario is line 49

        for n in range(50, 81):                 # for line 50 to 80...
            str1 = ts.readline().rstrip('\n')   # read line

        if d_abs == "False":                # If absolute date == False, i.e. relative dates...
            pwci.app_months.clear()
            pwci.app_days.clear()
            tstr2 = str1.split(",")         # Split line 80 by comma
            if emh == 1:                    # if relative to emergence
                e_day = int(tstr2[0])       # day is line 80 column 0
                e_month = int(tstr2[1])     # month is line 80 column 1
            elif emh == 2:                  # if relative to maturity
                e_day = int(tstr2[2])
                e_month = int(tstr2[3])
            else:                           # if relative to harvest
                e_day = int(tstr2[4])
                e_month = int(tstr2[5])

            for i in range(0,pwci.num_apps):    # For however many applications there were
                dt2 = dt.date(1960, e_month, e_day) + pd.DateOffset(days=int(tstr0[i]))     # make date from month and day + offset
                pwci.app_months.append(dt2.month)      # The month of dt2 is from the pwci.app_months
                pwci.app_days.append(dt2.day)          # The day of dt2 is from the pwci.app_days

        elif d_abs == "True":
            pwci.app_months.clear()  # clear save absolute months
            pwci.app_days.clear()   # clear save absolute days
            for i in range(0, pwci.num_apps):  # For 0 through the number of apps
                pwci.app_days.append(int(abs_days[i]))  # append the value to the application days

            for i in range(0, pwci.num_apps):  # Then for all possible application
                pwci.app_months.append(int(abs_months[i]))  # Append the value to the months

        for n in range(81, 103):
            str1 = ts.readline().rstrip('\n')   # For lines 81- 102...

        tstr = str1.split(",")                  # split line 102 by comma
        hi.bulk_density = float(tstr[0])        # bulk density is 102 column 0
        str1 = ts.readline().rstrip('\n')       # read line 103
        tstr = str1.split(",")                  # split line 104
        hi.max_hold_cap = float(tstr[0])        # max holding capacity is line 104 column 0
        str1 = ts.readline().rstrip('\n')       # read line 105
        tstr = str1.split(",")                  # split line 105
        hi.min_hold_cap = float(tstr[0])        # min holding capacity is line 105 column 0
        hi.avail_cap = hi.max_hold_cap - hi.min_hold_cap # available holding capacity is max - min

    ts.close()
    return hi, pwci

# get inputs from internal batch PWC run (PWC v 2.0)
def get_pwc_inputs_internal(pwc_inputs, fd, scen_name, scen_dir):
    pwci = PWCInputs()
    pwci.reset()
    hi = hydro_inputs()
    hi.reset()
    
    #pwci.vals.clear()
    #pwci.app_rates.clear()

    with open(pwc_inputs) as ts:
        str1 = ts.readline().rstrip('\n')   # Read the PWC header (line 1)
        str1 = ts.readline().rstrip('\n')   # Read the chemical name (line 2)
        pwci.chem_name = str1               # Set the chemical name
        str1 = ts.readline().rstrip('\n')   # Read the fd variable (line 3)
        pwci.fd_run = int(str1) - 1         # Set the fd variable 

        str2 = ts.readline().rstrip('\n')   # Read the variable (line 4)
        str1 = ts.readline().rstrip('\n')   # Read line 5
        tstr = str1.split(",")  # pass df here and pull the appropriate value, split line 5 by comma 

        pwci.Kd = float(tstr[fd])           #Line 5 is Kd
        if str2 == "True":                  # Line 4 is Koc flag, if true
            pwci.Kd *= 0.01                 # convert Koc to Kd

        for n in range (6, 14):             # For lines 6-13...
            str1 = ts.readline().rstrip('\n')   # Read the line
        tstr = str1.split(",")              # Split the line (13) by comma

        pwci.soil_t12 = float(tstr[fd])     # Line 13 = soil halflife
        if pwci.soil_t12 == 0:              # If soil half-life is stable
            pwci.soil_t12 = 1e8             # Replace with 1e8

        str1 = ts.readline().rstrip('\n')   # Read line 14
        str1 = ts.readline().rstrip('\n')   # Read line 15
        tstr = str1.split(",")              # Split line 15 by comma

        if tstr[fd] == '0' or tstr[fd] == '':  # JDF - added if else statements breaking out parent and degradate values
                pwci.foliar_t12 = 1e8          # Line 15 is foliar half life, replace with 1e8 if stable
        else:
            pwci.foliar_t12 = float(tstr[fd])

        for n in  range(16, 30):            # For lines 16-29...
            str1 = ts.readline().rstrip('\n')   # Read the line
        pwci.num_apps = int(str1)           # number of apps = line 29 as an integer

        str1 = ts.readline().rstrip('\n')   # For line 30
        abs_days = str1.split(",")              # Split line 30 by comma

        str1 = ts.readline().rstrip('\n')   # For line 31
        abs_months = str1.split(",")          # Split line 31 by comma

        str1 = ts.readline().rstrip('\n')   # Read line 32
        tstr = str1.split(",")              # Split 32 by comma
        #pwci.app_rates.clear()
        for i in range(0,pwci.num_apps):
            pwci.app_rates.append(float(tstr[i]))   # Set app rate for each app

        for n in range(33, 44):            # For line 33-43...
            str1 = ts.readline().rstrip('\n')   # Read line

        days_to_emh = str1.split(",")       # Split line 43 by comma
        str1 = ts.readline().rstrip('\n')   # Read line 44
        tstr = str1.split(",")              # Split line 44 by comma
        d_abs = tstr[0]                     # absolute dates T/F from line 44 column 0
        emh = int(tstr[1])                  # emh (emergence maturity harvest) from line 44 column 1
        if scen_name[-3:] == 'scn':         # if the scenario has scn at the end,
            pwci.scenario = scen_name[:-4]  # the pwci.scenario = everything but the .scn
        else:
            pwci.scenario = scen_name[:-5] 
        if d_abs == "False":                                            # If absolute date == False, i.e. relative dates...
            pwci.app_months.clear()                                     # clear save absolute months
            pwci.app_days.clear()                                       # clear saved absolute days  
            if scen_name[-3:] == 'scn':   # PWC 2.0 scenario,  # open scenario and get relevant info
                with open(os.path.join(scen_dir,scen_name)) as scenario_file:
                    for i in range(0, 27):                              # for lines 0-27
                        str1 = scenario_file.readline().rstrip('\n')    # read line 27
                    em_day = scenario_file.readline().rstrip('\n')      # line 28 is emergence day
                    em_month = scenario_file.readline().rstrip('\n')    # line 29 is emergence month
                    m_day = scenario_file.readline().rstrip('\n')       # line 30 is maturity day
                    m_month = scenario_file.readline().rstrip('\n')     # line 31 is maturity month
                    h_day = scenario_file.readline().rstrip('\n')       # line 32 is harvest day
                    h_month = scenario_file.readline().rstrip('\n')     # line 33 is harvest month
                    if emh == 1:                                    # if relative to emergence
                        e_day = int(em_day)                         # day is em_day
                        e_month = int(em_month)                     # month is em_month
                    elif emh == 2:                                  # if relative to maturity
                        e_day = int(m_day)                          
                        e_month = int(m_month)
                    else:                                           # if relative to harvest
                        e_day = int(h_day)
                        e_month = int(h_month)

                    for i in range(0, pwci.num_apps):               # For all possible applications
                        dt2 = dt.date(1960, e_month, e_day) + pd.DateOffset(days=int(days_to_emh[i]))   # Calculate date from relative date
                        pwci.app_months.append(dt2.month)           # Set app date month
                        pwci.app_days.append(dt2.day)               # Det app date day

                    for n in range(34, 54):
                        str1 = scenario_file.readline().rstrip('\n')

                    tstr = str1.split(",")                                  # split line 53 by comma
                    hi.bulk_density = float(tstr[0])                        # bulk density is line 53 column 0
                    tstr = scenario_file.readline().rstrip('\n').split(",") # read line 54 and split by comma
                    hi.max_hold_cap = float(tstr[0])                        # max holding capacity is line 54 column 0
                    tstr = scenario_file.readline().rstrip('\n').split(",") # read line 55 and split by comma
                    hi.min_hold_cap = float(tstr[0])                        # min holding capacity in line 56 column 0
                    hi.avail_cap = hi.max_hold_cap - hi.min_hold_cap        # calc available holding capacity
                scenario_file.close()     
            else:                       # old PWC scenario
                print("Old PWC scenarios are not supported for this method of PAT. Please contact the PAT developers for more information.")
        
        else:
            pwci.app_months.clear()             # clear save absolute months
            pwci.app_days.clear()               # clear saved absolute days
            for i in range(0,pwci.num_apps):    # For 0 through the number of apps
                pwci.app_days.append(int(abs_days[i]))  # append the value to the application days

            for i in range(0,pwci.num_apps):    # Then for all possible application
                pwci.app_months.append(int(abs_months[i]))    # Append the value to the months
            
            if scen_name[-3:] == 'scn':                                     # if it's a PWC 2.0 scenario
                with open(os.path.join(scen_dir,scen_name)) as scenario_file: # open scenario and get relevant info
                    for i in range(0, 53):                                  # for lines 0-53
                        str1 = scenario_file.readline().rstrip('\n')        # read line
                    tstr = str1.split(",")                                  # split line 53 by comma
                    hi.bulk_density = float(tstr[0])                        # bulk density is line 53 column 0
                    tstr = scenario_file.readline().rstrip('\n').split(",") # read line 54 and split by comma
                    hi.max_hold_cap = float(tstr[0])                        # max holding capacity is line 54 column 0
                    tstr = scenario_file.readline().rstrip('\n').split(",") # read line 55 and split by comma
                    hi.min_hold_cap = float(tstr[0])                        # min holding capacity in line 56 column 0
                    hi.avail_cap = hi.max_hold_cap - hi.min_hold_cap        # calc available holding capacity
                scenario_file.close()
            else:
                print("Old PWC scenarios are not supported for this method of PAT. Please contact the PAT developers for more information.")

    ts.close()
    
    return hi, pwci

# get inputs from PWC csv file that serves as an input file in a PWC batch run
# as PWC external batch file can't be used currently for fd, not included here
def get_pwc_inputs2(pwc_inputs, input_dir, scen_dir, options):
    pwci = PWCInputs()
    pwci.reset()
    hi = hydro_inputs()
    hi.reset()
    zts_file = ""
    aq_file = ""
    sa_file = ""
    #sstr = ""

    # zts file is Run Name + _bin + ".zts"
    if "t" in options:
        zts_file = os.path.join(input_dir, pwc_inputs[0, 1] + "_10.zts") # aq file is Run Name + _7 + _Scenario Name + "_Custom_Parent_daily.csv"
    scen_name = pwc_inputs[0, 21]
    if scen_name[-1:] == '2': # check if using old scenario (.scn) or new ones (.scn2)
        pwci.scenario = scen_name[:-5]
    else:
        pwci.scenario = scen_name[:-4]

    if "a" in options:
        aq_file = os.path.join(input_dir, pwc_inputs[0, 1] + "_7_" + pwci.scenario + "_Custom_Parent_daily.csv")

    # need to find way to link semiaquatic runs (bin 11) to aq runs - look for same file name extension, using bin 10
    if "s" in options:
        str1 = pwc_inputs[0, 1] + "_10_" + pwci.scenario + "_Custom_Parent_daily.csv"
        files = os.listdir(input_dir)
        if files:
            lst = [i for i in files if i.endswith(str1)]
            if len(lst) > 0:
                sa_file = os.path.join(input_dir, lst[0])
            else:
                print("Semiaquatic file does not exist")
        else:
            print("Directory is empty")

    # pwci.Kd from SoprtionCoefficient(mL/g) and kocflag
    pwci.Kd = pwc_inputs[0, 2]
    if pwc_inputs[0, 3] == True:
        pwci.Kd *= 0.01

    # pwci.soil_t12 from SoilHalflife(days)
    pwci.soil_t12 = pwc_inputs[0, 11]
    if pwci.soil_t12 == 0:
        pwci.soil_t12 = 1e8

    # pwci.foliar_t12 from FoliarHalflife(day)
    if pwc_inputs[0, 13] == 0 or np.isnan(pwc_inputs[0, 13]):
        pwci.foliar_t12 = 1e8
    else:
        pwci.foliar_t12 = pwc_inputs[0, 13]

    # pwci.num_apps from NumberofApplications
    pwci.num_apps = int(pwc_inputs[0, 74])
    # absolute days?
    abs_rel = pwc_inputs[0,75]

    # pwci.app_days, app_months, and app_rates from different columns
    if len(pwci.app_days) > 0:
        pwci.app_days.clear()
        pwci.app_months.clear()
        pwci.app_rates.clear()
        
    # For each application in the number of apps, append from the days column of the batch file for each app
    for i in range(0, pwci.num_apps):
        pwci.app_days.append(int(pwc_inputs[0, i * 8 + 77]))
        # If its an absolute date also append the month from the month column of the batch file for each app
        if abs_rel == True:
            pwci.app_months.append(int(pwc_inputs[0, i * 8 + 78]))
        # If it's a relatve day also append the month from the month column of the batch file for each app (but it should be blank so don't treat it as an integer)
        else:
            pwci.app_months.append(pwc_inputs[0, i * 8 + 78])
        # Get the app rate from the batch file
        pwci.app_rates.append(pwc_inputs[0, i * 8 + 79])


    # open scenario and get relevant info
    if scen_name[-1:] == '2':   # PWC 2.0 scenario
        with open(os.path.join(scen_dir,scen_name)) as ts:
            for i in range(0, 32):                                              # For the first 31 lines of the scenario line
                str1 = ts.readline().rstrip('\n')                               # Read line 31
            tstr = str1.split(",")                                              # Separate by comma

            if abs_rel == False:                                                # If it's a relative date
                if pwc_inputs[0, 76] == 1:                                      # And if the date is relative to emergence ( = 1)
                    e_day = int(tstr[0])                                        # Line 31 column 0 is the emergence day
                    e_month = int(tstr[1])                                      # Line 31 column 1 is the emergence month
                elif pwc_inputs[0, 76] == 2:                                    # If it relative to mauturity (=2)
                    e_day = int(tstr[2])                                        # Line 31 column 2 is the maturity day
                    e_month = int(tstr[3])                                      # Line 31 column 3 is the maturity month
                else:                                                           # And if the date is relative to harvest (=3)
                    e_day = int(tstr[4])                                        # Line 31 column 4 is the harvest day
                    e_month = int(tstr[5])                                      # Line 31 column 5 is the harvest month

                for i in range(0, pwci.num_apps):                               # For each application
                    rday = pwci.app_days[i]                                     # rday is the days relative to the event 
                    dt2 = dt.date(1960, e_month, e_day) + pd.DateOffset(days=rday)  # Calculate the abs app date from the relative days and the date that was relative to
                    pwci.app_months[i] = dt2.month                              # Get the app month from the calculated date
                    pwci.app_days[i] = dt2.day                                  # Get the app day from the calculated date

            for n in range(32, 54):                                             # Read lines 32- 53
                str1 = ts.readline().rstrip('\n')                               # Read line 53

            tstr = str1.split(",")                                              # Split line 53 by comma
            hi.bulk_density = float(tstr[0])                                    # Bulk density is line 53 column 0
            tstr = ts.readline().rstrip('\n').split(",")                        # Read line 54, split by comma
            hi.max_hold_cap = float(tstr[0])                                    # Maximum holding capacity is line 54 column 0
            tstr = ts.readline().rstrip('\n').split(",")                        # Read line 55, split by comma
            hi.min_hold_cap = float(tstr[0])                                    # Minimum holding capacity line 55 column 0
            hi.avail_cap = hi.max_hold_cap - hi.min_hold_cap                    # Calculate available holding capacity from the min and max
    else:                                                   # old PWC scenario
        with open(os.path.join(scen_dir,scen_name)) as ts:
            for i in range(0,28):                                               # Read lines 0-27
                str1 = ts.readline().rstrip('\n')                               # Read line 27

            if abs_rel == False:                                                # If its a relative date
                if pwc_inputs[0, 76] == 1:                                      # If the relative date is relative to emergence
                    e_day = int(str1)                                           # Emergence date is line 27 
                    str1 = ts.readline().rstrip('\n')                           # Read line 28
                    e_month = int(str1)                                         # Emergence month is line 28
                    for i in range(0,4):                                        # Read the next 4 lines (line 29, 30, 31, 32)
                        str1 = ts.readline().rstrip('\n')                       # Read line 32
                elif pwc_inputs[0, 76] == 2:                                    # If the relative date is relative to maturity
                    for i in range(0,2):                                        # Read the next two lines (line 33, 34)
                        str1 = ts.readline().rstrip('\n')                       # Read line 34
                    e_day = int(str1)                                           # Maturity date is line 34
                    str1 = ts.readline().rstrip('\n')                           # REad line 35
                    e_month = int(str1)                                         # Maturity month is line 35
                    for i in range(0, 2):                                       # Read the next two lines (line 36, 37)
                        str1 = ts.readline().rstrip('\n')                       # Read line 37
                else:                                                           # If the relative date is relative to harvest
                    for i in range(0,4):                                        # Read the next 4 lines (line 38, 39, 40, 41)
                        str1 = ts.readline().rstrip('\n')                       # Read line 41
                    e_day = int(str1)                                           # the harvest day is line 41
                    str1 = ts.readline().rstrip('\n')                           # REad line 42
                    e_month = int(str1)                                         # The harvest month is line 42

                for i in range(0, pwci.num_apps):                               # For each application
                    rday = pwci.app_days[i]                                     # rday is the days relative to the event
                    dt2 = dt.date(1960, e_month, e_day) + pd.DateOffset(days=rday)  # Calculate the absolute date from the relative days and the event date
                    pwci.app_months[i] = dt2.month                              # Get the month from the calculated date
                    pwci.app_days[i] = dt2.day                                  # Get the day from the calculated date

            else:                                                               # For an absolute date
                for n in range(0, 5):                                           # Read the next 5 lines (line 28, 29, 30, 31, 32)
                    str1 = ts.readline().rstrip('\n')                           # Read line 32

            for n in range(0, 20):                                              # Read the next 20 lines (lines?? # A little confused here because it wouldn't match up to be the same line number between the relative and absolute dates and it looks like it should?)
                str1 = ts.readline().rstrip('\n')

            tstr = str1.split(",")
            hi.bulk_density = float(tstr[0])
            tstr = ts.readline().rstrip('\n').split(",")
            hi.max_hold_cap = float(tstr[0])
            tstr = ts.readline().rstrip('\n').split(",")
            hi.min_hold_cap = float(tstr[0])
            hi.avail_cap = hi.max_hold_cap - hi.min_hold_cap

    ts.close()

    return hi, pwci, zts_file, aq_file, sa_file


# develop spray drift values
def get_sdf(app_method, buffer, sdf_file):
    # Read in the spray drift fraction file
    table = pd.read_csv(sdf_file)
    sdf = []
    # For each 1 m interval up to the maximum 30 m distance [?]
    for n in range(0,30):
        sdf.append(n)
        sdf[n] = 0
        # If there is spray drift
        if app_method != "None":
            # Convert feet to meters
            dist1 = round((n + buffer)*3.2808, 4)
            dist2 = round((n + 1 + buffer) * 3.2808, 4)
            # If the further distance is less than 997 m
            if dist2 < 997:
                # bot = get the index from the table where the Dist_ft is greater than dist1
                bot = table[table["Dist_(ft)"] > dist1].index[0]
                # tp = get the index from the table where the Dist_ft is greater than dist2
                tp = table[table["Dist_(ft)"] > dist2].index[0]

                if bot == tp:
                # get average deposition between dist1 and measurement just above dist1
                    a = (table.loc[bot, app_method] - table.loc[bot - 1, app_method]) / (table.loc[bot, "Dist_(ft)"] -                                                                         table.loc[bot - 1, "Dist_(ft)"])
                    b = table.loc[bot, app_method] - a * table.loc[bot, "Dist_(ft)"]
                    sdf[n] = (a * dist1 + b + a * dist2 + b)/2
                else:
                # get average deposition between dist1 and measurement just above dist1
                    a = (table.loc[bot,app_method] - table.loc[bot-1,app_method]) / (table.loc[bot,"Dist_(ft)"] -
                                                                             table.loc[bot-1,"Dist_(ft)"])
                    b = table.loc[bot,app_method] - a * table.loc[bot,"Dist_(ft)"]
                    dep = a * dist1 + b
                    sdf[n] += (table.loc[bot, app_method] + dep)/2 * (table.loc[bot,"Dist_(ft)"] - dist1)

                    # get average deposition between measurements up to the measurement just below dist2
                    i = bot + 1
                    while table.loc[i, "Dist_(ft)"] < dist2:
                        sdf[n] += ((table.loc[i, app_method] + table.loc[i-1, app_method]) / 2 *
                                   (table.loc[i, "Dist_(ft)"] - table.loc[i-1, "Dist_(ft)"]))
                        i += 1

                    #  get average deposition between measurement just below dist2 and dist2
                    a = (table.loc[i,app_method] - table.loc[i-1,app_method]) / (table.loc[i,"Dist_(ft)"] -
                                                                                 table.loc[i-1,"Dist_(ft)"])
                    b = table.loc[i,app_method] - a * table.loc[i,"Dist_(ft)"]
                    dep = a * dist2 + b
                    sdf[n] += (dep + table.loc[i-1, app_method])/2 * (dist2 - table.loc[i-1,"Dist_(ft)"])

                    # get average deposition
                    sdf[n] /= (dist2 - dist1)
    return sdf


# estimate the 1-in-10 year and 1-in-15 year values
# modified 2/4/22, cap - allows for use with Python versions before 3
def one_in_ten(vals, col_name):
    rslt1 = 0
    rslt2 = 0

    for n in range(0, 2):
        if n == 0:  # 1-in-10 yr estimate (1/10 = 0.1)
            cnt = int(np.floor(0.1 * (len(vals) + 1)))
        else:       # 1-in-15 yr estimate (1/15 = 0.067)
            cnt = int(np.floor(0.067 * (len(vals) + 1)))
        # Get the n largest values based on the cnt
        s = vals.nlargest(cnt+1, col_name)
        # 1-in-10 yr estimate
        # a1 is the ratio of the cnt to all the values +1
        a1 = float(cnt)/float(len(vals)+1)
        # a2 is the ratio of the cnt +1 to all the values + 1
        a2 = float(cnt+1)/float(len(vals)+1)
        # Slope
        slp = (s.values[cnt] - s.values[cnt-1]) / (a2-a1)
        # Intercept
        intcpt = s.values[cnt] - slp * a2
        if n == 0:  # 1-in-10 yr estimate
            rslt1 = slp * 0.1 + intcpt
        else:       # 1-in-15 yr estimate
            rslt2 = slp * 0.067 + intcpt

    return rslt1, rslt2


# retrieve the huc number from scenario name
def get_huc(scenario):
    n = scenario.find("ESA") + 3
    m = len(scenario) - n
    huc = scenario[-m:]
    return huc

# terrestrial plant calculations
def pat_terr(zts_file, run_name, hi, gi, pwci, sdf, make_figs, plant_edpt, esa, output_dir, line, tsum, tsum30, save_ter, fd, rq_sum_ter):
    print("Terrestrial inputs...")
    print("pwci")
    print(vars(pwci))
    print("hi")
    print(vars(hi))
    print("gi")
    print(vars(gi))
    print("zts")
    print(zts_file)
    print("run name")
    print(run_name)
    print("line")
    print(line)
    # get zts file
    # JDF: Added xtra colmumn names to account for additional columns from f&d analysis
    if pwci.fd_run == 0:     # no formation decline
        col_names = ["year", "month", "day", "RUNF0", "ESLS0", "RFLX1", "EFLX1", "DCON1", "INFL0", "PRCP0"]
    elif pwci.fd_run == 1:   # formation decline with parent-daughter
        col_names = ["year", "month", "day", "RUNF0", "ESLS0", "RFLX1", "EFLX1", "RFLX2", "EFLX2",
                     "DCON1", "DCON2", "INFL0", "PRCP0"]
    else:               # formation decline with parent-daughter-granddaughter
        col_names = ["year", "month", "day", "RUNF0", "ESLS0", "RFLX1", "EFLX1", "RFLX2", "EFLX2", "RFLX3",
                     "EFLX3", "DCON1", "DCON2", "DCON3",  "INFL0", "PRCP0"]
    
    # Read in zts file
    table = pd.read_csv(zts_file, header = None, names = col_names, skiprows = 3, delim_whitespace = True)
    # If there is a precipitation column...
    if ~np.isnan(table.loc[0, "PRCP0"]):
        # Drop the columns for dissolved
        table.drop(["DCON1", "INFL0"], axis=1, inplace=True)

        # If you're looking at a degradate other than parent for formation decline (fd)
        if pwci.fd_run > 0:
            # If you're looking at the daughter
            if pwci.fd_run == 1:
                # Drop the daughter pesticide concentration in porewater
                table.drop(["DCON2"], axis=1, inplace=True)
                #Otherwise
            else:
                # Drop the daughter and grand-daughters in porewater
                table.drop(["DCON2", "DCON3"], axis=1, inplace=True)
            # If you're looking at parent
            if fd == 0:  
                # Drop the pesticide in runoff and erosion for the daughter and granddaughters
                table.drop(["RFLX2", "EFLX2", "RFLX3", "EFLX3"], axis=1, inplace=True, errors='ignore')   #if fd = 0 drop FLX2 and FLX3
            # If you're looking at daughter
            elif fd == 1:
                # Drop the pesticide in runoff and erosion for the parent and granddaughter
                table.drop(["RFLX1", "EFLX1", "RFLX3", "EFLX3"], axis=1, inplace=True, errors='ignore')
            # If you're looking at granddaughter
            elif fd == 2:
                # Drop the pesticide in runoff and erosion for the parent and daughter
                table.drop(["RFLX1", "EFLX1", "RFLX2", "EFLX2"], axis=1, inplace=True)
            else:
                print("No formation decline input provided ... ")

        # add columns to table and initialize the parameters
        # add parameter to indicate if an application occurred on the date or not, 1 = application occurred
        table["m_d"] = table["month"].astype(str) + "_" + table["day"].astype(str)
        table["app event"] = 0
        # For all the applications
        for n in range(0, pwci.num_apps):
            # make an application date by stringing together the app month and app day from pwc inputs
            str1 = str(pwci.app_months[n]) + "_" + str(pwci.app_days[n])
            # Find where the app dates match in the table in the app event column, then + one for that date
            table.loc[table["m_d"] == str1, "app event"] = n + 1
        # Drop the month_day column used to match application dates
        table.drop(["m_d"], axis=1, inplace=True)
        
        # Rename things so they are more human readable, do some calculations
        table["Runoff Depth cm"] = table["RUNF0"]
        # Convert runoff mass for the degradate of concern (g/cm2 -> kg/ha)
        table["Runoff Mass kg/ha"] = table["RFLX"+str(fd+1)]*100000 # table RLFX str df  -- also pass variable to sfq procedure
        # Convert erosion mass for degradate of concern (g/cm2 -> kg/ha)
        table["Erosion Mass kg/ha"] = table["EFLX"+str(fd+1)]*100000
        table["Precipitation cm"] = table["PRCP0"]
        # Calculate runoff volume from runoff depth (Equation 8 of user manual)
        table["Runoff Volume m3/d"] = table["Runoff Depth cm"] / 100 * 10 * 10000
        # Calculate precipitation volume (Equation 9)
        table["Precip Volume m3/d"] = table["Precipitation cm"] / 100 * gi.pez_area
        # Set the initial soil water content at 0 
        table["Soil water content m3/m3"] = 0.0
        # Set the daily volume + infiltration to 0 
        table["Runoff+Infil m3/d"] = 0.0
        # Convert mass in kg/ha to kg by multiplying by 10 (assume a 10 ha field-- Section 3.1, figure 2)
        table["mass runon kg"] = table["Runoff Mass kg/ha"] * 10
        # Convert erosion mass in kg/ha to kg by multiplying by 10 (assume a 10 ha field-- Section 3.1, figure 2)
        table["mass sediment kg"] = table["Erosion Mass kg/ha"] * 10
        
        # Create a column in the table for foliar spray at distances from 0-30 m from the edge of field
        for n in range(0, 30):
            nstr = "foliar spray kg " + str(n) + "_" + str(n + 1)
            table[nstr] = 0
        
        # Create columns for variables to be calculated, set with 0
        table["foliar washoff kg"] = 0
        table["K1 d-1"] = 0
        table["K2 kg/m3/d"] = 0
        table["mass tpez water kg"] = 0
        table["mass tpez soil kg"] = 0

        # Create a column for the foliar mass remaining at distances from 0-30 m from the edge of the field
        for n in range(0, 30):
            nstr = "foliar mass remain kg " + str(n) + "_" + str(n + 1)
            table[nstr] = 0
        # Create a column for runoff eec in kg/ha
        table["runoff eec kg/ha"] = 0
        
        # Create a column for the concentration from spray + runoff for each distance from 0-30 m from the edge of field
        for n in range(0, 30):
            nstr = "spray+runoff eec kg/ha " + str(n) + "_" + str(n + 1)
            table[nstr] = 0
        
        # Put all the column names into a variable
        column_names = table.columns
        # Put all the tabel values into an array variable
        tab2 = table.values
        
        # insert daily calculations
        for n in range(0,len(table)):
            print("Terrestrial model, processing day {}".format(n))
            
        # Soil water content (Section 4.1.2.3)
            # If the runoff volume + the precipitation volume = 0
            if tab2[n, 13] + tab2[n, 14] == 0:
                # The the minimum holding capacity is set as the soil water content
                tab2[n, 15] = hi.min_hold_cap
                # And the runoff + infiltration is set as 0
                tab2[n, 16] = 0.0
            # otherwise...
            else:
                # If its the first day of the time series
                if n == 0:
                    # "prev_day" variable is set as the maximum holding capacity
                    prev_day = hi.max_hold_cap
                # Otherwise
                else:
                    # prev_day is set as the previous days soil water content
                    prev_day = tab2[n-1, 15]
                # Add to prev_day the runoff volumne and the precipitation volume, and divide that by the waterholding capacity for the compartment
                prev_day += (tab2[n, 13] + tab2[n, 14])/gi.pez_cap
                # If the the prev_day minus the maximum waterholding capacity is greater than 0
                if prev_day - hi.max_hold_cap > 0:
                    # Set the maximum waterholding capacity as the soil water content
                    tab2[n, 15] = hi.max_hold_cap
                else:
                    # Otherwise set the soil water content as the prev_day volumne
                    tab2[n, 15] = prev_day
                    
                # Runoff and Infiltration (Equation 10)
                # Runoff and infiltration = (the runoff volume + the precipitation) - (the soil watercontent for the day - the soil water content of the previous day) * the waterholding capacity for the compartment
                tab2[n, 16] = (tab2[n, 13] + tab2[n, 14] - (tab2[n, 15] - tab2[n-1, 15]) * gi.pez_cap)
                # If the absolute value of the runoff plus infiltration is less than 1e-14
                if abs(tab2[n, 16]) < 1e-14:
                    # Set the runoff and infiltration to 0
                    tab2[n, 16] = 0

            # foliar mass due to spray (8.1.3)
            foliar_sum = 0
            for i in range(0, 30):
                # If it's not a day with an application event
                if tab2[n, 8] == 0:
                    # If it's the first day of the time series
                    if n == 0:
                        # Set the foliar spray as 0 for each interval from 0-30 m
                        tab2[n, i + 19] = 0
                    # If it's not the first day of the time series
                    else:
                        # The foliar spray for each interval is foliar mass remaining of the previous day (Equation 29)
                        tab2[n, i + 19] = tab2[n-1, i + 54] * np.exp(-1 * np.log(2) / pwci.foliar_t12)
                # If it's a day with an application event
                else:
                    # If it's the first day of the time series
                    if n == 0:
                         # Set the foliar spray as 0 for each interval from 0-30 m
                        tab2[n, i + 19] = 0
                    # If it's not the first day of the time series
                    else:
                        # ar (application rate) = the application rate indexed in pwci.app_rates
                        ar = pwci.app_rates[int(tab2[n, 8]) - 1]
                        # Foliar spray for a given distance range = (Equation 29 and 30)
                        tab2[n, i + 19] = (tab2[n - 1, i + 54] * np.exp(-1 * np.log(2) / pwci.foliar_t12) + ar * sdf[i] * gi.side_length * 1 / 10000)
                foliar_sum += tab2[n, i + 19]
            
            # If the [precipitation isn't 0]
            if tab2[n, 12] != 0:
                # the foliar washoff (Equation 31)
                tab2[n, 49] = (foliar_sum * (1 - np.exp(-1 * tab2[n, 12] * pwci.washoff)))

            # calculate K1 and K2. denom is used in both (Equation 20)
            denom = (tab2[n, 15] + pwci.Kd * hi.bulk_density) * gi.pez_volume
            # Calculate K1 (Equation 20)
            tab2[n, 50] = tab2[n, 16] / denom + np.log(2) / pwci.soil_t12
            # Calculate K2 (Equation 28)
            tab2[n, 51] = tab2[n, 17] + tab2[n, 18]
            if n != 0:
                # Equation 28 and 19
                tab2[n, 51] += (tab2[n-1, 49] + tab2[n-1, 52] + tab2[n-1, 53])
                tab2[n, 51] /= denom

            tab2[n, 52] = (tab2[n, 51] * np.exp(-1 * tab2[n, 50]) * tab2[n, 15] * gi.pez_volume)

            tab2[n, 53] = (tab2[n, 52] * pwci.Kd / 1000 * hi.soil_mass / (tab2[n, 15] * gi.pez_volume))

            # calculate runoff EEC
            tab2[n, 84] = ((tab2[n, 52] + tab2[n, 53]) / (gi.pez_area/10000))

            # calculate remaining foliar mass and EECs
            for i in range(0, 30):
                if tab2[n, 12] == 0:
                    tab2[n, i + 54] = tab2[n, i + 19]
                else:
                    tab2[n, i + 54] = tab2[n, i + 19] * np.exp(-1 * tab2[n, 12] * pwci.washoff)
                tab2[n, i + 85] = tab2[n, 84] + tab2[n, i + 54] / (gi.side_length * 1 / 10000)

        ter_table = pd.DataFrame(data=tab2, columns=column_names)
        # create summary file
        numyrs = len(ter_table['year'].unique())
        col_names = np.empty(shape=[32], dtype='object')
        col_names[0] = 'year'
        col_names[1] = "peak runoff lb/A"
        for n in range(0, 30):
            col_names[n + 2] = "peak eec " + str(n) + "_" + str(n + 1) + " m lb/A"

        sum_tab2 = np.empty(shape=[numyrs,32])
        n=0
        for yr in ter_table["year"].unique():
            table_sum = ter_table[ter_table["year"] == yr]
            print("Processing year {}...".format(int(yr)))
            # Analyze the data
            sum_tab2[n, 0] = int(yr)
            sum_tab2[n, 1] = np.max(table_sum["runoff eec kg/ha"])/1.12
            for i in range(0, 30):
                nstr2 = "spray+runoff eec kg/ha " + str(i) + "_" + str(i + 1)
                sum_tab2[n, i + 2] = np.max(table_sum[nstr2]) / 1.12
            n += 1

        # Get the 1-in-10 year values
        sum_stat = np.empty(shape=[2,32])
        sum_stat[0, 0] = 99
        sum_stat[1, 0] = 99
        sum_stat[0, 1], sum_stat[1, 1] = one_in_ten(pd.DataFrame(sum_tab2[:, 1],columns=["runoff eec kg/ha"]),
                                                    "runoff eec kg/ha")
        for i in range(0, 30):
            nstr2 = "spray+runoff eec kg/ha " + str(i) + "_" + str(i + 1)
            sum_stat[0, i + 2], sum_stat[1, i + 2] = one_in_ten(pd.DataFrame(sum_tab2[:, i + 2],columns=[nstr2]), nstr2)

        sum_tab2 = np.append(sum_tab2,sum_stat,axis=0)

        # create terrestrial EEC file and EEC summary file with results
        if save_ter:
            print("Saving terrestrial data for run {} ...".format(run_name))
            ter_table.to_csv(os.path.join(output_dir,(run_name + "_terr_results.csv")), index=False)

        # create an index to use with summary file
        indx = np.empty(numyrs+2, dtype='object')
        for i in range(0, numyrs):
            indx[i] = str(sum_tab2[i, 0])
        indx[numyrs] = "1-in-10 yr"
        indx[numyrs+1] = "1-in-15 yr"

        sum_data = pd.DataFrame(data=sum_tab2, columns=col_names, index=indx)
        sum_data.drop(["year"], axis=1, inplace=True)
        sum_data.index.name = "Year"
        print("Saving terrestrial summary data for run {} ...".format(run_name))
        sum_data.to_csv(os.path.join(output_dir,(run_name + "_terr_summary.csv")))

        # get huc number
        if esa:
            huc = get_huc(pwci.scenario)
        else:
            huc = "99"
        # save data to summary files - currently saves edge of field+spray drift estimate
        if line != -1:      # if -1, then not a batch file run
            if esa:
                tsum.update_table(line, run_name, 0, pwci.scenario, huc, 0, sum_tab2[numyrs+1, 1], sum_tab2[numyrs+1, 16])
            else:
                tsum.update_table(line, run_name, 0, pwci.scenario, huc, 0, sum_tab2[numyrs, 1], sum_tab2[numyrs, 16])
            # modified 2/4/22, cap - allows for summarizing runs with scenarios with less than 30 years
            tsum30.update_table(line, run_name, int(float(indx[0])), pwci.scenario, huc, 0, sum_tab2[:, 1], sum_tab2[:, 16])

        # create terrestrial RQ tables
        rq_colnames = ["SE/VV", "Species", "Runoff EEC (lb/A)", "Runoff Non-listed RQ", "Runoff Listed RQ","Outer Edge EEC (lb/A)", "Outer Edge Non-listed RQ", "Outer Edge Listed RQ", "Scenario"]
        # determine unique crops
        rq_tab = np.empty(shape=[len(plant_edpt), 9], dtype='object')
        for i in range(0,len(plant_edpt)):
            rq_tab[i, 0] = plant_edpt.iloc[i, 0]
            rq_tab[i, 1] = plant_edpt.iloc[i, 1]
            if esa:
                rq_tab[i, 2] = sum_tab2[numyrs+1, 1]          # these are the 1-in-15 yr values for ESA
                rq_tab[i, 5] = sum_tab2[numyrs+1, 31]
            else:
                rq_tab[i, 2] = sum_tab2[numyrs, 1]          # these are the 1-in-10 yr values, for QC against Excel version
                rq_tab[i, 5] = sum_tab2[numyrs, 31]
            for z in range(0, 2):
                if ~np.isnan(plant_edpt.iloc[i, (z * 5 + 5)]):
                    for t in  range (0, 2):
                        rq = np.round(rq_tab[i, (t * 3 + 2)] / plant_edpt.iloc[i, (z * 5 + 5)], 3)
                        if rq < 0.01:
                            rq_tab[i, (t * 3 + 3 + z)] = "< 0.01"
                        elif plant_edpt.iloc[i, (z * 5 + 4)] == "<":
                            rq_tab[i, (t * 3 + 3 + z)] = "> " + str(np.round(rq, 2))
                        elif plant_edpt.iloc[i, (z * 5 + 4)] == ">":
                            rq_tab[i, (t * 3 + 3 + z)] = "< " + str(np.round(rq, 2))
                        else:
                            rq_tab[i, (t * 3 + 3 + z)] = str(np.round(rq, 2))
                else:
                    rq_tab[i, (3 + z)] = "NA"
                    rq_tab[i, (6 + z)] = "NA"
            rq_tab[i, 8] = run_name

        print("Saving terrestrial RQ summary for run {} ...".format( run_name))
        pd.DataFrame(data=rq_tab, columns=rq_colnames).to_csv(os.path.join(output_dir, (run_name + "_terr_rqs.csv")),
                                                              index=False)
        # Take data from rq_tab, make it a dataframe
        rq_df = pd.DataFrame(data=rq_tab, columns=rq_colnames)

        # Append data to the summary dataframe
        if line != -1:  # -1, not running a batch file
            rq_sum_ter.update_table(rq_df)

        # create figures
#        if make_figs:
            #dts = np.empty(shape=[len(ter_table)], dtype='object')
            #for n in range(0,len(ter_table)):
            #    dts[n] = str(1900+int(tab2[n,0])) + "-" + str(int(tab2[n, 1])) + "-" + str(int(tab2[n,2]))
            #converted_dates = pltdt.datestr2num(dts)
            #plt.figure(num=1,figsize=(10.0,7.0))

            #plt.subplot(211)
            #plt.plot_date(converted_dates, tab2[:, 85], "b-")       # near edge of TPEZ
            #plt.plot_date(converted_dates, tab2[:, 114], "r-")      # far edge of TPEZ
            #plt.title("Concentrations Over Time in TPEZ, Drift + Runoff", fontsize=8)
            #plt.xlabel("Year", fontsize='x-small')
            #plt.ylabel("Concentration (lb/A)", fontsize='x-small')
            #plt.xticks(fontsize='x-small')
            #plt.yticks(fontsize='x-small')
            #plt.legend(("Near-edge", "Far-edge"), loc='best', fontsize='xx-small', ncol = 2)

            #plt.subplot(212)
            #plt.plot_date(converted_dates, tab2[:, 84], "b-")       # runoff across TPEZ
            #plt.title("Concentrations Over Time in TPEZ, Runoff only", fontsize=8)
            #plt.xlabel("Year", fontsize='x-small')
            #plt.ylabel("Concentration (lb/A)", fontsize='x-small')
            #plt.xticks(fontsize='x-small')
            #plt.yticks(fontsize='x-small')
            #plt.legend("Across_TPEZ", loc='best', fontsize='xx-small', ncol = 1)

            #plt.tight_layout()
            #plt.savefig(os.path.join(output_dir,(run_name + "terr_figs.pdf")), format="PDF")
    else:
        print("Precipitation data not contained in ZTS file, cannot calculate TPEZ EECs")

# new subroutine, 2/4/22, cap - this gets the first year of the runs from the time series for saq and aq runs
def get_start_date(csvfile):
    with open(csvfile) as satable:
        for n in range(0, 5):
            txt = satable.readline()
    satable.close()
    dat = int(txt[7:12])-1
    dy = pd.datetime.strptime("1/1/1900", '%m/%d/%Y').date()
    dy2 = dy + pd.DateOffset(days=dat)
    return dy2

def pat_semiaq(semi_csv, run_name, pwci, sai, make_figs, plant_edpt, esa, output_dir, line, ssum, ssum30, save_saq, rq_sum_saq):
    # print("Wetland inputs...")
    # print(vars(pwci))
    # print(vars(sai))
    col_names = ["depth m", "avg aq water kg/m3", "avg aq benthic kg/m3", "peak aq water kg/m3"]
    dy = get_start_date(semi_csv)   # added 2/4/22, cap - to get start dates from files that don't start in 1961
    
    # Read in the daily csv file for the wetland
    satable = pd.read_csv(semi_csv, header = None, index_col = False, names = col_names, skiprows = 5, delim_whitespace = False)
    # add date and year columns
    satable["Date"] = ""
    satable["Year"] = 0
    satable["avg aq water ug/L"] = 0
    satable["EEC lb/A"] = 0
    indx_order = [4, 5, 0, 1, 2, 3, 6, 7]
    satable = satable[[satable.columns[i] for i in indx_order]]

    col_names = satable.columns
    tab2 = satable.values

    for n in range(0,len(satable)):
        print("Semi-aquatic model, processing day {}".format(n))
        # insert date and year
        dat = dy + pd.DateOffset(days=n+1)
        tab2[n, 0] = str(dat.month) + "/" + str(dat.day) + "/" + str(dat.year)
        tab2[n, 1] = int(dat.year)
        
        # If the depth is greater than 0.005 m
        if tab2[n, 2] >= 0.005:
            # Convert average water concentration from kg/m3 to ug/L
            tab2[n, 6] = tab2[n, 3] * 1000 * 1e6 / 1000
            # EEC in kg (Equation 38)
            tab2[n, 7] = tab2[n, 3] * 10000 * tab2[n, 2]
        # Equations 35 and 36
        tab2[n, 7] += tab2[n, 4] * ((pwci.Kd/1000) * sai.bulk_density * sai.volume_sed + sai.volume_water)
        # Equation 34 conversion from kg to lb/A, assuming waterbody of 10,000 m2
        tab2[n, 7] = tab2[n, 7] / 10000 * 10000 / 1.12

    satable2 = pd.DataFrame(data=tab2, columns=col_names)

    # create summary file
    numyrs = len(satable2['Year'].unique())
    col_names2 = np.empty(shape=[3], dtype='object')
    col_names2[0] = 'Year'
    col_names2[1] = "EEC (ug/L)"
    col_names2[2] = "EEC (lb/A)"

    sum_tab2 = np.empty(shape=[numyrs,3])
    n=0
    for yr in satable2["Year"].unique():
        table_sum = satable2[satable2["Year"] == yr]
        print("Processing year {} ...".format(yr))
        # Analyze the data
        sum_tab2[n, 0] = yr
        sum_tab2[n, 1] = np.max(table_sum["avg aq water ug/L"])
        sum_tab2[n, 2] = np.max(table_sum["EEC lb/A"])
        n += 1
        
    # Get the 1-in-10 year values
    sum_stat = np.empty(shape=[2,3])
    sum_stat[0, 0] = 99
    sum_stat[1, 0] = 99
    # Calculate 1-in-10 year averages
    sum_stat[0, 1], sum_stat[1, 1] = one_in_ten(pd.DataFrame(sum_tab2[:, 1],columns=["avg aq water ug/L"]),
                                                "avg aq water ug/L")
    sum_stat[0, 2], sum_stat[1, 2] = one_in_ten(pd.DataFrame(sum_tab2[:, 2],columns=["EEC lb/A"]), "EEC lb/A")

    sum_tab2 = np.append(sum_tab2,sum_stat,axis=0)

    # create semi aquatic file and summary file with results
    if save_saq:
        print("Saving wetlands data for run {} ...".format(run_name))
        satable2.to_csv(os.path.join(output_dir,(run_name + "_semiaq_results.csv")), index=False)

    # create an index to use with summary file
    indx = np.empty(shape=[numyrs+2], dtype='object')
    # for i in range(0, 30):
    for i in range(0, numyrs):
        indx[i] = str(sum_tab2[i, 0])
    indx[numyrs] = "1-in-10 yr"
    indx[numyrs+1] = "1-in-15 yr"

    sum_data = pd.DataFrame(data=sum_tab2, columns=col_names2, index=indx)
    sum_data.drop(["Year"], axis=1, inplace=True)
    sum_data.index.name = "Year"
    print("Saving wetlands summary data for run {} ...".format(run_name))
    sum_data.to_csv(os.path.join(output_dir,(run_name + "_semiaq_summary.csv")))

    # get huc number
    if esa:
        huc = get_huc(pwci.scenario)
    else:
        huc = "99"

    # save data to summary files
    if line != -1:      # -1, not running a batch file
        if esa:
            ssum.update_table(line, run_name, 0, pwci.scenario, huc, 1, sum_tab2[numyrs+1,1], sum_tab2[numyrs+1,2])
        else:
            ssum.update_table(line, run_name, 0, pwci.scenario, huc, 1, sum_tab2[numyrs, 1], sum_tab2[numyrs, 2])
        # modified 2/4/22, cap - to accommodate runs with less than 30 years
        ssum30.update_table(line, run_name, int(float(indx[0])), pwci.scenario, huc, 1, sum_tab2[:, 1], sum_tab2[:, 2])

    # create semiaquatic RQ tables
    rq_colnames = ["SE/VV/AQ", "Species", "Wetlands EEC", "Non-listed Wetlands RQ", "Listed Wetlands RQ", "Scenario"]
    # determine unique crops
    rq_tab = np.empty(shape=[len(plant_edpt), 6], dtype='object')
    for i in range(0,len(plant_edpt)):
        rq_tab[i, 0] = plant_edpt.iloc[i, 0]
        rq_tab[i, 1] = plant_edpt.iloc[i, 1]
        if esa:
            t = numyrs+1
        else:
            t = numyrs
        if rq_tab[i, 0] == "AQ":
            rq_tab[i, 2] = sum_tab2[t, 1]
        else:
            rq_tab[i, 2] = sum_tab2[t, 2]
        for z in range(0, 2):
            if ~np.isnan(plant_edpt.iloc[i, (z * 5 + 5)]):
                rq = np.round(rq_tab[i, 2] / plant_edpt.iloc[i, (z * 5 + 5)], 3)
                if rq < 0.01:
                    rq_tab[i, (3 + z)] = "< 0.01"
                elif plant_edpt.iloc[i, (z * 5 + 4)] == "<":
                    rq_tab[i, (3 + z)] = "> " + str(np.round(rq, 2))
                elif plant_edpt.iloc[i, (z * 5 + 4)] == ">":
                    rq_tab[i, (3 + z)] = "< " + str(np.round(rq, 2))
                else:
                    rq_tab[i, (3 + z)] = str(np.round(rq, 2))
            else:
                rq_tab[i, (3 + z)] = "NA"
        rq_tab[i,5]=run_name

    print("Saving wetlands RQ summary for run {} ...".format(run_name))
    pd.DataFrame(data=rq_tab, columns=rq_colnames).to_csv(os.path.join(output_dir, (run_name + "_semiaq_rqs.csv")),
                                                          index=False)
    # Take data from rq_tab, make it a dataframe
    rq_df = pd.DataFrame(data=rq_tab, columns=rq_colnames)

    # Append data to the summary dataframe
    if line != -1:  # -1, not running a batch file
        rq_sum_saq.update_table(rq_df)


    # create figures
    #if make_figs:
    #    dts = np.empty(shape=[len(satable)], dtype='object')
    #    for n in range(0, len(satable)):
    #        dts[n] = str(1900 + int(tab2[n, 0])) + "-" + str(int(tab2[n, 1])) + "-" + str(int(tab2[n, 2]))
    #    converted_dates = pltdt.datestr2num(dts)
    #    plt.figure(num=1, figsize=(10.0, 7.0))


def pat_aq(aq_csv, run_name, make_figs, plant_edpt, esa, output_dir, line, rq_sum_aq): ## Add in line as parameter to work with summary RQ file, and rq_sum_aq
    col_names = ["depth m", "avg aq water kg/m3", "avg aq benthic kg/m3", "peak aq water kg/m3"]
    dy = get_start_date(aq_csv)     # added 2/4/22, cap - to deal with start dates that aren't 1/1/196
    # Read in the daily values for the pond
    aqtable = pd.read_csv(aq_csv, header = None, index_col = False, names = col_names, skiprows = 5,
                          delim_whitespace = False)
    # add date and year columns
    aqtable["Date"] = ""
    aqtable["Year"] = 0
    aqtable["avg aq water ug/L"] = 0
    indx_order = [4, 5, 0, 1, 2, 3, 6]
    aqtable = aqtable[[aqtable.columns[i] for i in indx_order]]

    col_names = aqtable.columns
    tab2 = aqtable.values

    # dy = pd.datetime.strptime("12/31/1960",'%m/%d/%Y').date()     removed 2/4/22, cap - dealt with above
    for n in range(0,len(aqtable)):
        print("Aquatic model, processing day {}".format(n))
        # insert date and year
        dat = dy + pd.DateOffset(days=n+1)
        tab2[n, 0] = str(dat.month) + "/" + str(dat.day) + "/" + str(dat.year)
        tab2[n, 1] = int(dat.year)
        tab2[n, 6] = tab2[n, 3] * 1000 * 1e6 / 1000

    aqtable2 = pd.DataFrame(data=tab2, columns=col_names)

    # create summary file
    numyrs = len(aqtable2['Year'].unique())
    col_names2 = np.empty(shape=[2], dtype='object')
    col_names2[0] = 'Year'
    col_names2[1] = "EEC (ug/L)"

    sum_tab2 = np.empty(shape=[numyrs,2])
    n=0
    for yr in aqtable2["Year"].unique():
        table_sum = aqtable2[aqtable2["Year"] == yr]
        print("Processing year {} ...".format(yr))
        # Analyze the data
        sum_tab2[n, 0] = yr
        sum_tab2[n, 1] = np.max(table_sum["avg aq water ug/L"])
        n += 1
    

    sum_stat = np.empty(shape=[2,2])
    sum_stat[0, 0] = 99
    sum_stat[1, 0] = 99
    # Calculate 1-in-10 year values
    sum_stat[0, 1], sum_stat[1, 1] = one_in_ten(pd.DataFrame(sum_tab2[:, 1],columns=["avg aq water ug/L"]),
                                                "avg aq water ug/L")

    sum_tab2 = np.append(sum_tab2,sum_stat,axis=0)

    # create aquatic file and summary file with results
    print("Saving aquatic data for run {} ...".format(run_name))
    aqtable2.to_csv(os.path.join(output_dir,(run_name + "_aq_results.csv")), index=False)

    # create an index to use with summary file
    indx = np.empty(shape=[numyrs+2], dtype='object')
    for i in range(0, numyrs):
        indx[i] = str(sum_tab2[i, 0])
    indx[numyrs] = "1-in-10 yr"
    indx[numyrs+1] = "1-in-15 yr"

    sum_data = pd.DataFrame(data=sum_tab2, columns=col_names2, index=indx)
    sum_data.drop(["Year"], axis=1, inplace=True)
    sum_data.index.name = "Year"
    print("Saving aquatic summary data for run {} ...".format(run_name))
    sum_data.to_csv(os.path.join(output_dir,(run_name + "_aq_summary.csv")))

    # create aquatic RQ tables
    rq_colnames = ["AQ", "Species", "Aquatic EEC", "Non-listed Aquatic RQ", "Listed Aquatic RQ", "Scenario"]
    # determine unique crops
    rq_tab = np.empty(shape=[len(plant_edpt), 6], dtype='object')
    for i in range(0, len(plant_edpt)):
        rq_tab[i, 0] = plant_edpt.iloc[i, 0]
        rq_tab[i, 1] = plant_edpt.iloc[i, 1]
        if esa:
            t = numyrs+1
        else:
            t = numyrs
        rq_tab[i, 2] = sum_tab2[t, 1]
        for z in range(0, 2):
            if ~np.isnan(plant_edpt.iloc[i, (z * 5 + 5)]):
                rq = np.round(rq_tab[i, 2] / plant_edpt.iloc[i, (z * 5 + 5)], 3)
                if rq < 0.01:
                    rq_tab[i, (3 + z)] = "< 0.01"
                elif plant_edpt.iloc[i, (z * 5 + 4)] == "<":
                    rq_tab[i, (3 + z)] = "> " + str(np.round(rq, 2))
                elif plant_edpt.iloc[i, (z * 5 + 4)] == ">":
                    rq_tab[i, (3 + z)] = "< " + str(np.round(rq, 2))
                else:
                    rq_tab[i, (3 + z)] = str(np.round(rq, 2))
            else:
                rq_tab[i, (3 + z)] = "NA"
        rq_tab[i, 5] = run_name

    print("Saving aquatic RQ summary for run {} ...".format(run_name))
    pd.DataFrame(data=rq_tab, columns=rq_colnames).to_csv(os.path.join(output_dir, (run_name + "_aq_rqs.csv")),
                                                          index=False)
    # Take data from rq_tab, make it a dataframe
    rq_df = pd.DataFrame(data=rq_tab, columns=rq_colnames)

    # Append data to the summary dataframe
    if line != -1:  # -1, not running a batch file
        rq_sum_aq.update_table(rq_df)

    # create figures
#    if make_figs:
#        dts = np.empty(shape=[len(aqtable)], dtype='object')
#        for n in range(0, len(aqtable)):
#            dts[n] = str(1900 + int(tab2[n, 0])) + "-" + str(int(tab2[n, 1])) + "-" + str(int(tab2[n, 2]))
#        converted_dates = pltdt.datestr2num(dts)
#        plt.figure(num=1, figsize=(10.0, 7.0))


def main(input_dir, output_dir, batch_run, make_figs, batch_file, use_pwc_batch, scen_dir, sdf_file, endpoints, run_name, zts_file, pwc_input, semi_csv, aq_csv, app_method, options, esa, buffer, save_terr, save_saq, fd, use_pwc_internal_batch):
    tic = dt.datetime.now()
    print("Start time: %s" % (tic))
    if not os.path.exists(output_dir):
        os.mkdir(output_dir)
    tsum = ""                                       # PAT terrestrial summary file, used for batch runs
    tsum30 = ""                                     # PAT terrestrial 30 year summary file, used for batch runs
    ssum = ""                                       # PAT semiaquatic summary file, used for batch runs
    ssum30 = ""                                     # PAT terrestrial 30 year summary file, used for batch runs
    rq_sum_ter = ""                                 # TPEZ RQ Summary file
    rq_sum_saq = ""                                 # WPEZ RQ Summary file
    rq_sum_aq = ""                                  # APEZ RQ Summary file

    # next set of classes are designed to store inputs for PAT run
    gi = geo_inputs()
    hi = hydro_inputs()
    pwci = PWCInputs()
    sai = SemiPWCInputs()

    """ All path and parameter variables are set before this line. The rest is program functionality """

    # get plant endpoints
    plant_edpt = pd.read_csv(endpoints, index_col = False)

    # check if batch run; if so, grab info for runs
    if batch_run:
        # Read in the batch file to batch_inputs
        batch_inputs = pd.read_csv(batch_file, index_col=False)
        if "t" in options:
            # Set file names for summary files
            tsum = OutputTable(output_dir, "PAT Terrestrial Summary.csv", 1)
            tsum30 = OutputTable(output_dir, "PAT Terrestrial Summary 30.csv", 3)
            rq_sum_ter = OutputTableRQ_ter(output_dir, "PAT Terrestrial RQ summary.csv")
        if "s" in options:
            # Set file names for summary files
            ssum = OutputTable(output_dir, "PAT Semiaquatic Summary.csv", 2)
            ssum30 = OutputTable(output_dir, "PAT Semiaquatic Summary 30.csv", 4)
            rq_sum_saq = OutputTableRQ_saq(output_dir, "PAT Semiaquatic RQ Summary.csv")
        if "a" in options:
            # Set file names for summary files
            rq_sum_aq = OutputTableRQ_aq(output_dir, "PAT Aquatic RQ Summary.csv")

        for n in range(0, len(batch_inputs)):
            # Get the run_name from the Run ID column in the batch file
            run_name = batch_inputs.loc[n,"Run ID"]
            # Get the buffer distance
            buffer = batch_inputs.loc[n, "Buffer (m)"]
            # Get the formation decline information
            fd = int(batch_inputs.loc[n, "fd"])
            # Get the scenario name
            scen_name = batch_inputs.loc[n,"PWC Scenario File"]
            # If it's a PWC external batch file
            if use_pwc_batch == True:
                # Set pwc_input as the PWC external batch file
                pwc_input = os.path.join(input_dir, batch_inputs.loc[n, "PWC Input File"])
                pwc_vals = pd.read_csv(pwc_input, index_col=False)
                # Parse inputs as specified in the get_pwc_inputs2 function for pwc external batch files
                hi, pwci, zts_file, aq_csv, semi_csv = get_pwc_inputs2(pwc_vals[pwc_vals["Run Name"] == run_name].values,
                                                                       input_dir, scen_dir, options)
            # If you're using an internal PWC batch run
            elif use_pwc_internal_batch == True: # add in the option to read inputs as needed for an internal batch function
                # get inputs from the pat batch file
                pwc_input = os.path.join(input_dir, batch_inputs.loc[n,"PWC Input File"])
                # You can't use old PWC scenarios
                if pwc_input[-3:] == "swi":
                    print("Unsupported PWC file type")
                # For the regular .PWC scenarios use the get_pwc_inputs_internal file to get input information
                elif pwc_input[-3:] == "PWC":
                    hi, pwci = get_pwc_inputs_internal(pwc_input, fd, scen_name, scen_dir)

            # Otherwise you did a single PWC run
            else:
                # Get inputs from the pat batch file
                pwc_input = os.path.join(input_dir, batch_inputs.loc[n,"PWC Input File"])
                # For SWI files use the get_pwc_inputs function
                if pwc_input[-3:] == "swi":
                    hi, pwci = get_pwc_inputs(pwc_input)
                # For PWC files use the get_pwc_inputs_single function
                elif pwc_input[-3:] == "PWC":
                    hi, pwci = get_pwc_inputs_single(pwc_input, fd)

            # Calculate the soil mass [kg] in TPEZ, assuming soil bulk density same as treated field
            # soil mass [kg]  = volume of TPEZ [m3] x bulk density [g/cm3] x 1 kg/1000 g x 1000 cm3/L x 1000 L/m3
            hi.soil_mass = gi.pez_volume * hi.bulk_density * 1000
            # Calculate PEZ water holding capacity [m3] in TPEZ
            # gi.pez_cap [m3] = available capacity [m3/m3] x TPEZ volume [m3])
            gi.pez_cap = hi.avail_cap * gi.pez_volume
            # Calculate the bulk density [kg/m3] of the WPEZ, assuming bulk density is the same as the field
            # sai.bulk_density [kg/m3] = bulk density [g/cm3] x 1 kg/1000 g x 1000 cm3/L x 1000 L/m3
            sai.bulk_density = hi.bulk_density * 1000
            # open spray drift deposition file and develop deposition values for first 30 m
            pwci.app_method = batch_inputs.loc[n, "Spray DSD"]
            sdf = get_sdf(pwci.app_method, buffer, sdf_file)

            if "t" in options:
                # get zts file and generate terrestrial outputs
                if not use_pwc_batch:
                    zts_file = os.path.join(input_dir, batch_inputs.loc[n,"PWC ZTS File Name"])
                print("Conducting terrestrial analysis for {}".format(zts_file))
                edpt = plant_edpt[(plant_edpt["Type"] == "SE") | (plant_edpt["Type"] == "VV")]
                pat_terr(zts_file, run_name, hi, gi, pwci, sdf, make_figs, edpt, esa, output_dir, n+1, tsum, tsum30,
                         save_terr, fd, rq_sum_ter)
                #make_terrestrial_RQ_summary(input_dir, output_dir, rq_sum_ter)

            if "s" in options:
                # get PWC semiaquatic csv file
                if not use_pwc_batch:
                    semi_csv = os.path.join(input_dir, batch_inputs.loc[n,"PWC Semi-Aquatic File"])
                print("Conducting semi-aquatic analysis for {}".format(semi_csv))
                pat_semiaq(semi_csv, run_name, pwci, sai, make_figs, plant_edpt, esa, output_dir, n+1, ssum, ssum30,
                           save_saq, rq_sum_saq)
                #make_wetland_RQ_summary(input_dir, output_dir, rq_sum_saq, "TPEZ")
                #make_wetland_RQ_summary(input_dir, output_dir, rq_sum_saq, "APEZ")

            if "a" in options:
                # get PWC aquatic csv file
                if not use_pwc_batch:
                    aq_csv = os.path.join(input_dir, batch_inputs.loc[n,"PWC Aquatic File"])
                print("Conducting aquatic analysis for {}".format(aq_csv))
                edpt = plant_edpt[plant_edpt["Type"] == "AQ"]
                pat_aq(aq_csv, run_name, make_figs, edpt, esa, output_dir, n+1, rq_sum_aq)
                #make_aquatic_RQ_summary(input_dir, output_dir, rq_sum_aq)
    # For a PAT single run
    else:
        # get pwc inputs
        #run_name = ""
        # For an old SWI file, use the get_pwc_inputs function
        if pwc_input[-3:] == "swi":
            hi, pwci = get_pwc_inputs(pwc_input, fd)
        # For a new PWC file, use the get_pwc_inputs_single function
        elif pwc_input[-3:] == "PWC":
            hi, pwci = get_pwc_inputs_single(pwc_input, fd)
        # Calculate the soil mass [kg] in TPEZ, assuming soil bulk density same as treated field
        # soil mass [kg]  = volume of TPEZ [m3] x bulk density [g/cm3] x 1 kg/1000 g x 1000 cm3/L x 1000 L/m3
        hi.soil_mass = gi.pez_volume * hi.bulk_density * 1000
        # Calculate PEZ water holding capacity [m3] in TPEZ
        # gi.pez_cap [m3] = available capacity [m3/m3] x TPEZ volume [m3])
        gi.pez_cap = hi.avail_cap * gi.pez_volume
        # Calculate the bulk density [kg/m3] of the WPEZ, assuming bulk density is the same as the field
        # sai.bulk_density [kg/m3] = bulk density [g/cm3] x 1 kg/1000 g x 1000 cm3/L x 1000 L/m3
        sai.bulk_density = hi.bulk_density * 1000

        # open spray drift deposition file and develop deposition values for first 30 m
        if app_method != "":
            pwci.app_method = app_method
        sdf = get_sdf(pwci.app_method, buffer, sdf_file)

        if "t" in options:
            # get zts file and generate terrestrial outputs
            print("Conducting terrestrial analysis for {}".format(zts_file))
            edpt = plant_edpt[(plant_edpt["Type"] == "SE") | (plant_edpt["Type"] == "VV")]
            pat_terr(zts_file, run_name, hi, gi, pwci, sdf, make_figs, edpt, esa, output_dir, -1, tsum, tsum30,
                     save_terr, fd, rq_sum_ter)

        if "s" in options:
            # get PWC semiaquatic csv file and generate wetland output
            print("Conducting semi-aquatic analysis for {}".format(semi_csv))
            pat_semiaq(semi_csv, run_name, pwci, sai, make_figs, plant_edpt, esa, output_dir, -1, ssum, ssum30,
                       save_saq, rq_sum_saq)

        if "a" in options:
            # get PWC aquatic csv file and generate aquatic output
            print("Conducting aquatic analysis for {}".format(aq_csv))
            edpt = plant_edpt[plant_edpt["Type"] == "AQ"]
            pat_aq(aq_csv, run_name, make_figs, edpt, esa, output_dir, -1, rq_sum_aq)
    toc = dt.datetime.now()
    print("End time: %s" % (toc))
    print("Execution time: %s" % (toc - tic))

# main(input_dir, output_dir, batch_run, make_figs, batch_file, use_pwc_batch, scen_dir, sdf_file, endpoints, run_name, zts_file, pwc_input, semi_csv, aq_csv, app_method, options, esa, buffer, save_terr, save_saq, fd, use_pwc_internal_batch)
