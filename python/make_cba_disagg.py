#%% Import libraries
import os
import timeit
import pymrio
import create_accounts_funs as fns
import pandas as pd

if __name__ == "__main__":
    start = timeit.default_timer()
    print("Start at: {}".format(start))
    
    #%% Define years to be used
    y_considered = list(range(2000, 2020))

    #%% Define the parts of the IO table to be used
    gwp_col = \
        'GHG emissions (GWP100) | Problem oriented approach: ' + \
            'baseline (CML, 2001) | GWP100 (IPCC, 2007)'
    impacts_of_interest = ['Value Added', 'Employment', gwp_col]

    #%% Read in the IO-table
    io_tables = fns.setup_data(y_considered)

    #%% Get the CBA on the country level, at least for the EU countries
    year_frames = []

    # It is generally recommended to calculate MRIO accounts with the highest detail possible and aggregated the results afterwards (post-aggregation - see for example Steen-Olsen et al 2014, Stadler et al 2014 or Koning et al 2015.
    #%% Completely global
    for current_y in io_tables.keys():
        print(current_y)
        current_iot = io_tables[current_y]
        current_iot.calc_all()
        # Imports
        gwp_imp = current_iot.impacts.D_imp_reg[
            current_iot.impacts.D_imp_reg.index.isin(
                [current_iot.impacts.D_imp_reg.index[3]])]
        gwp_imp_wide = gwp_imp.stack().reset_index(name="value").pivot(
                    index=["level_1"], columns="impact", values="value")
        gwp_imp_wide.index.name = None
        gwp_imp_wide["year"] = current_y
        gwp_imp_wide.reset_index(inplace=True)
        gwp_imp_wide.rename(
            columns={gwp_col: "GWP_import", "index": "country"}, 
            inplace=True)
        
        # Exports
        gwp_exp = current_iot.impacts.D_exp_reg[
            current_iot.impacts.D_exp_reg.index.isin(
                [current_iot.impacts.D_exp_reg.index[3]])]
        gwp_exp_wide = gwp_exp.stack().reset_index(name="value").pivot(
                    index=["level_1"], columns="impact", values="value")
        gwp_exp_wide.index.name = None
        gwp_exp_wide["year"] = current_y
        gwp_exp_wide.reset_index(inplace=True)
        gwp_exp_wide.rename(
            columns={gwp_col: "GWP_export", "index": "country"}, 
            inplace=True)
        
        gwp_imp_exp_wide = gwp_imp_wide.merge(gwp_exp_wide, 
            left_on=['country', 'year'], 
            right_on=['country', 'year'])

        year_frames.append(gwp_imp_exp_wide)
        
    #%% Aggregate frames and save them
    full_frame = pd.concat(year_frames)
    csv_file = os.path.join("data", "cba_disagg.csv")
    full_frame.to_csv(csv_file)
    
    print("Saved CBA accounts to: ", csv_file)
    runtime = (timeit.default_timer() - start)/60    
    complete_mins = int(runtime)
    incomplete_mins = round((runtime % 1) * 60, 2)
    print("Finished. Total runtime: {}:{} minutes".format(
        complete_mins, incomplete_mins))
    exit(0)
