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

    #%% Get the PBA on the country level, at least for the EU countries
    year_frames = []
    for y in y_considered:
        print(y)
        io_disagg = io_tables[y]
        io_disagg.calc_all()

        data = io_disagg.impacts.D_pba_reg
        gwp_col = \
                'GHG emissions (GWP100) | Problem oriented approach: ' + \
                    'baseline (CML, 2001) | GWP100 (IPCC, 2007)'
                    
        data = data[data.index.isin(impacts_of_interest)]

        data_wide = data.stack().reset_index(name="value").pivot(
            index=["level_1"], columns="impact", values="value")
        data_wide.index.name = None
        data_wide["year"] = y
        data_wide["type"] = "pba"
        data_wide.reset_index(inplace=True)
        data_wide.rename(
            columns={gwp_col: "GWP", "Value Added": "ValueAdded", "index": "country"}, 
            inplace=True)
        year_frames.append(data_wide)

    #%% Aggregate frames and save them
    full_frame = pd.concat(year_frames)
    csv_file = os.path.join("data", "pba_disagg.csv")
    full_frame.to_csv(csv_file)
    
    print("Saved PBA accounts to: ", csv_file)
    runtime = timeit.default_timer() - start    
    print("Finished. Total runtime: {} minutes".format(
        round(runtime/60, 2)))
    exit(0)
