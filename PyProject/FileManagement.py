import json
from collections import OrderedDict
import os
import os.path
import pandas as pd


# user to set file path and set file names if different from the default
dirListing = 'c:\\personal\\IMM\\'

filelist = ['households_out.csv', 'trips_out.csv', 'sample_veh_proportions_gghm_zone_csd.csv',
            'dtype_primarymode_prob.json', 'dtype_stationchoice.json', 'dtype_egressmode_prob.json',
            'dtype_households.json', 'dtype_trips.json', 'dtype_tripsprocessed.json']


########################################################################################################################
# FUNCTIONS that are used for cleaning the datasets and getting them ready for sampling

# Function to check existence of files

def file_existence(filepath, filename):

    """
    This function checks if a file exists for a given user directory
    :param filepath:
    :param filename
    :return: boolean
    """
    try:
        f = open(os.path.join(filepath, filename))
    except IOError as e:
        raise IOError("%s: %s" % (dirListing, e.strerror))

    return True


# function for setting market segments

def market_segment(df):

    """
    This function takes a dataframe and assigns the market segment to it that is used in the GGHM.
    :param df
    :return dataframe with market segment
    """
    if {'hhinc', 'auto_suff'}.issubset(df.columns):

        # create segments
        df.loc[(df['hhinc'] <= 60000) & (df['auto_suff'] == 0), 'market_seg'] = 0
        df.loc[(df['hhinc'] > 60000) & (df['auto_suff'] == 0), 'market_seg'] = 1
        df.loc[(df['hhinc'] <= 60000) & (df['auto_suff'] == 1), 'market_seg'] = 2
        df.loc[(df['hhinc'] > 60000) & (df['auto_suff'] == 1), 'market_seg'] = 3
        df.loc[(df['hhinc'] <= 60000) & (df['auto_suff'] == 2), 'market_seg'] = 4
        df.loc[(df['hhinc'] > 60000) & (df['auto_suff'] == 2), 'market_seg'] = 5
        # set dtype
        df['market_seg'] = df['market_seg'].astype('int8')
    else:
        print("The requisite fields are not there to run the function")

    return df


# function for concatenating dataframes by columns

def concat_df(df1, df2, num):

    """
    A function to concatenate two dataframes by columns
    :param dataframe 1
    :param dataframe 2
    :param axis i.e 0 for row and 1 for column
    :return concatenated df
    """
    # once sampled, now concatenate the information back to the household dataframe
    df1.reset_index(drop=True, inplace=True)
    df2.reset_index(drop=True, inplace=True)
    df1 = pd.concat([df1, df2], axis=num)

    return df1


########################################################################################################################
# Check if files noted in the filelist exist in the path specified by the user. If not, then raise error.

for file in filelist:

    check_existence = file_existence(dirListing, file)

########################################################################################################################
# Batch in the dtype definitions that are stored as json files
with open(os.path.join(dirListing, 'dtype_primarymode_prob.json')) as json_file:
    dtype_primarymode_prob = json.load(json_file, object_pairs_hook=OrderedDict)

with open(os.path.join(dirListing, 'dtype_stationchoice.json')) as json_file:
    dtype_stationchoice = json.load(json_file, object_pairs_hook=OrderedDict)

with open(os.path.join(dirListing, 'dtype_egressmode_prob.json')) as json_file:
    dtype_egressmode_prob = json.load(json_file, object_pairs_hook=OrderedDict)

with open(os.path.join(dirListing, 'dtype_households.json')) as json_file:
    dtype_hh = json.load(json_file, object_pairs_hook=OrderedDict)

with open(os.path.join(dirListing, 'dtype_trips.json')) as json_file:
    dtype_trips = json.load(json_file, object_pairs_hook=OrderedDict)

with open(os.path.join(dirListing, 'dtype_tripsprocessed.json')) as json_file:
    dtype_tripsprocessed = json.load(json_file, object_pairs_hook=OrderedDict)


########################################################################################################################
# Read in all the necessary files and reset the dtypes to optimize memory. Also, modify the vehicle type df to make it
# work with Cheval. The veh_type file is melted and a market_segment is added to the # end of it. Adding the market
# segment is helpful later on for slicing and attaching the appropriate probabilities # to the households file and
# passing it to Cheval.

# bring in the GGHMV4's Trips_out and Household file and attach a market segment to each household
hh = pd.read_csv(os.path.join(dirListing, "households_out.csv"))
hh = market_segment(hh)   # function application
trips = pd.read_csv(os.path.join(dirListing, "trips_out.csv"))

# set dtypes for the household and trips dataframe to reduce memory requirements
for key, value in dtype_trips.items():
    trips[key] = trips[key].astype(value)

for key, value in dtype_hh.items():
    hh[key] = hh[key].astype(value)

#
# bring in Vehicle Type file. Bryce's file has a certain structure (wide format) that Bill prefers for MLOGIT.
veh_type = pd.read_csv(os.path.join(dirListing, "sample_veh_proportions_gghm_zone_csd.csv"))
veh_type.rename(columns={'ggh_zone': 'taz'}, inplace=True)
veh_type = pd.melt(veh_type, id_vars=['taz']).sort_values(['taz', 'variable'])

# attach the market segment to the veh_type. Also add in an integer based market seg.
# The vehicle type is part of the variable column but starts after the second underscore
# thus it is stripped out into a column of its own.
veh_type['mseg1'] = veh_type['variable'].str.split('_').str[0]
veh_type['mseg2'] = veh_type['variable'].str.split('_').str[1]
veh_type['mseg'] = veh_type['mseg1'] + '_' + veh_type['mseg2']

# add in integer based market segment category
veh_type.loc[(veh_type['mseg'] == 'nocar_low'), 'market_seg'] = 0
veh_type.loc[(veh_type['mseg'] == 'nocar_high'), 'market_seg'] = 1
veh_type.loc[(veh_type['mseg'] == 'insuff_low'), 'market_seg'] = 2
veh_type.loc[(veh_type['mseg'] == 'insuff_high'), 'market_seg'] = 3
veh_type.loc[(veh_type['mseg'] == 'suff_low'), 'market_seg'] = 4
veh_type.loc[(veh_type['mseg'] == 'suff_high'), 'market_seg'] = 5
veh_type['market_seg'] = veh_type['market_seg'].astype('int8')

# extract the vehicle type.
veh_type['vtype1'] = veh_type['variable'].str.split('_').str[2]
veh_type['vtype2'] = veh_type['variable'].str.split('_').str[3]
veh_type['vtype'] = veh_type['vtype1'] + '_' + veh_type['vtype2']

# drop unncessary columns
columns = ['mseg1', 'mseg2', 'mseg', 'vtype1', 'vtype2']
veh_type.drop(columns, inplace=True, axis=1)

print("")