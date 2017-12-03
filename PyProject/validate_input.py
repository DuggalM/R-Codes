import json
from collections import OrderedDict
import os
import os.path

# This class validates the inputs and also carries out a minor data wrangling operation i.e. assigns a numeric
# integer to the market segment based on income and auto sufficiency

class EarlyValidation(object):

    def __init__(self):
        pass

    # Function to check existence of files
    def file_existence(self, filepath, filename):

        """
        This function checks if a file exists for a given user directory
        :param: filepath:
        :param: filename:
        :return: boolean
        """
        try:
            f = open(os.path.join(filepath, filename))
        except IOError as e:
            raise IOError("%s: %s" % (filepath, e.strerror))

        return True

    # Method to bring in the various dtype definitions
    def dtype_defintions(self, filepath, filenames):

        """
        This function batches in the dtype definition JSON file for each of the file in the filelist.

        :param filepath:
        :param filenames:
        :return: variable names of each dtype file
        """

        # Batch in the dtype definitions that are stored as json files
        with open(os.path.join(filepath, filenames[3])) as json_file:
            dtype_primarymode_prob = json.load(json_file, object_pairs_hook=OrderedDict)

        with open(os.path.join(filepath, filenames[4])) as json_file:
            dtype_stationchoice = json.load(json_file, object_pairs_hook=OrderedDict)

        with open(os.path.join(filepath, filenames[5])) as json_file:
            dtype_egressmode_prob = json.load(json_file, object_pairs_hook=OrderedDict)

        with open(os.path.join(filepath, filenames[6])) as json_file:
            dtype_hh = json.load(json_file, object_pairs_hook=OrderedDict)

        with open(os.path.join(filepath, filenames[7])) as json_file:
            dtype_trips = json.load(json_file, object_pairs_hook=OrderedDict)

        with open(os.path.join(filepath, filenames[8])) as json_file:
            dtype_tripsprocessed = json.load(json_file, object_pairs_hook=OrderedDict)

        return dtype_primarymode_prob, dtype_stationchoice, dtype_egressmode_prob, dtype_hh, \
               dtype_trips, dtype_tripsprocessed

    # Function to assign market segment integer value
    def market_segment(self, df):

        """
        This function takes a dataframe and assigns the market segment to it that is used in the GGHM.
        :param df:
        :return dataframe with market segment:
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

