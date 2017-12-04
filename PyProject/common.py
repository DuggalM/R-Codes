import pandas as pd
import os
from collections import OrderedDict
import json
import logging

def setupLogger():
    logger = logging.getLogger("super_model")
    logger.setLevel(logging.INFO)
    fh = logging.FileHandler(r"c:/personal/model.log")
    fh.setFormatter(logging.Formatter("%(asctime)s - %(name)s - %(funcName)s - %(levelname)s - %(message)s"))


# function to concatenate two dfs
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
    df1 = df1.loc[:,~df1.columns.duplicated()]
    df2 = df2.loc[:,~df2.columns.duplicated()]
    df1 = pd.concat([df1, df2], axis=num)

    return df1

def market_segment(df):

    """
    This function takes a dataframe and assigns the market segment to it that is used in the GGHM.
    :param df: dataframe that needs to be assigned the market segment
    :return df: with market segment attached
    :raises KeyError: some columns are missing in order to undertake the market segment calculation

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
        raise KeyError ("The requisite fields are not there to run the function")

    return df

# Method to bring in the various dtype definitions
def dtype_defintions( filepath, filenames):

    """
    This function batches in the dtype definition JSON file for each of the file in the filelist.

    :param filepath: directory in which the user has saved all the requisite files noted in control_parameters.py
    :param filenames: JSON filename that needs to be evaluated
    :return: variable names of each dtype file
    """

    # Batch in the dtype definitions that are stored as json files
    dataFrameDtype = {}

    for filename in filenames:
        with open(os.path.join(filepath, filename)) as json_file:
            dataFrameDtype[filename] = json.load(json_file, object_pairs_hook=OrderedDict)

    return  dataFrameDtype


    # Function to check existence of files
def file_existence(filepath, filename):

    """
    This function checks if a file exists for a given user directory
    :param: filepath: directory in which the user has saved all the requisite files noted in control_parameters.py
    :param: filename: filename that needs to be checked for its presence
    :return: boolean
    """
    try:
        f = open(os.path.join(filepath, filename))
    except IOError as e:
        print("%s: %s : %s" % (filepath, filename, e.strerror))
        return False

    return True