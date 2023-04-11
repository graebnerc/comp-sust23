import os
import pymrio
import pandas as pd

#%% Read in data
def setup_data(years, interactive=False):    
    io_tables = {}
    for y in years:
        print("Parsing year ", y, "...", sep="", end="")
        tab_name = "IOT_" + str(y) + "_ixi.zip"
        if interactive:
            io_tables[int(y)] = pymrio.parse_exiobase3(
                os.path.join("..", "data", "exiobase_raw", tab_name))
        else:
            io_tables[int(y)] = pymrio.parse_exiobase3(
                os.path.join("data", "exiobase_raw", tab_name))
        print("finished.")
    return io_tables

def make_long(data, col_interest, year, type_account):
    """Transforms impact account into long format

    Parameters
    ----------
    data : pd.DataFrame
        Account taken from EXIOBASE impact extension
    col_interest : list
        Names of the columns of interest
    year : int
        Year of the observations
    type_account : str
        Type of accounts: 'cba', 'pba', 'imp', 'exp'

    Returns
    -------
    pd.DataFrame
        Long version with sufficient information for further processing
    """
    gwp_col = \
        'GHG emissions (GWP100) | Problem oriented approach: ' + \
            'baseline (CML, 2001) | GWP100 (IPCC, 2007)'
    data = data[data.index.isin(col_interest)]
    data = data.stack().reset_index().melt(
        id_vars=["impact", "sector"], var_name="country")
    data["year"] = year
    data["type"] = type_account
    data = data.pivot_table(
        index=['country', "sector", 'year', "type"], 
        columns="impact", values="value")
    data.rename(
        columns={gwp_col: "GWP", "Value Added": "ValueAdded"}, 
        inplace=True)
    data.reset_index(inplace=True)
    return data

def save_pba_accounts(io_tables, impacts_of_interest):
    pba_accounts = [make_long(
        data=io_tables[y].impacts.D_pba, 
        col_interest=impacts_of_interest, 
        year=y, type_account="pba") for y in io_tables.keys()]

    full_pba_accounts = pd.concat(pba_accounts)
    csv_file = os.path.join("data", "tidy", "pba_eu.csv")
    full_pba_accounts.to_csv(csv_file)
    print("Saved PBA accounts to: ", csv_file)
