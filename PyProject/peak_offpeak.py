# -*- coding: utf-8 -*-
"""
Created on Monday Dec  11 17:07:02 2017

@author: MZD
"""

# import packages
import os
import pandas as pd
import numpy as np
import Mlogit_Probe_EarlyValid as mprobe_valid
import common

class PeakOffpeak(object):

    """


    """

    def __init__(self):

        pass

    def peak_consistency(self, df_array):
        """

        :param df_array:
        :return:
        """

        nrows, ncols = df_array.shape

        for row in range(nrows):

            if ((df_array[row][5] == 'home') & (df_array[row][6] in ['work', 'school', 'university'])):
                loop = 0
                start_peak_flag = df_array[row][21]

            if ((df_array[row][6] == 'home')):
                if (loop == 0):
                    df_array[row][21] = start_peak_flag
                    loop = 1
                    start_peak_flag = None

        return df_array


    def run(self, trips_hhold_df, peak_consistency):
        """
        This function creates a peak flag for every record in the trips_hhold dataframe. This is needed to ensure that
        MLOGIT produces the correct probabilities by time period and O-D pair. If the peak_consistency flag is
        set to 1 then the function ensures consistency in choosing the peak-off peak flag for the outbound and inbound
        trip of the mandatory tours. This option also increases the run times by around 35 minutes as every row needs to
        be evaluated.
        :param trips_hhold_df: the trips_hhold df that needs a peak flag
        :param peak_consistency: if activated to 1 then consistency between the outbound and inbound trips of the
        mandatory tour are maintained.
        :return: trips_hhold df with peak flag
        """

        # The trips_out file contains a peak hour factor column that decides whether a trip is sampled
        # in the peak or off-peak period. In order to discretely select the peak records and vice-versa
        # an uniform random number generator is run and the values are attached to the trips_out file.
        # If the (1-peak_factor) value in the record is greater than that of the random value than
        # the record is in the off-peak and vice-versa.

        np.random.seed(mprobe_valid.seed)
        random = pd.DataFrame(np.random.uniform(size=len(trips_hhold_df)))
        random.columns = ['rnum']

        # attach the random number generator and calculate peak_flag. A value of 1 in this flag
        # means that this is a peak period trip record.
        trips_hhold_df = common.concat_df(trips_hhold_df, random, 1)
        trips_hhold_df['peak_flag'] = np.where(trips_hhold_df['rnum'] <= trips_hhold_df['peak_factor'], 1, 0)
        trips_hhold_df[['peak_flag']] = trips_hhold_df[['peak_flag']].astype('int8')


        mprobe_valid.logger.info("Return the trips and household dataframe combined with each record that has a home end"
                                 "in it tagged as to whether it starts in the peak (1) or off-peak period (0). The rest"
                                 "of the records are populated with a dummy value of 10 as their time period will be "
                                 "determined by the destination choice model of the GGHMV4")

        # GGHMV4 carries out mode choice for the HBW, HBS, and HBU trips at the PA level. This essentially means that
        # the return trip of that tour must also lie in the same peak period that the home based trip was in.

        if peak_consistency == 1:
            mprobe_valid.logger.info("Peak consistency flag set to %s." % peak_consistency)

            # set default values
            loop = 0
            start_peak_flag = 0

            # convert the trips_hhold dataframe and run the peak consistency function. Once completed make the array
            # back to a dataframe and set column names
            trips_hhold_df_array = trips_hhold_df.values
            trips_hhold_df_array = self.peak_consistency(trips_hhold_df_array)

            trips_hhold_df_array = pd.DataFrame(trips_hhold_df_array)
            trips_hhold_df_array.columns = trips_hhold_df.columns

            # reset the trips_hhold_df dataframe
            trips_hhold_df = trips_hhold_df_array

        mprobe_valid.logger.info("Peak flag populated")
        return trips_hhold_df