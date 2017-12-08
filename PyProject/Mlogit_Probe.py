# -*- coding: utf-8 -*-
"""
Created on Thu Dec  7 18:07:02 2017

@author: MZD
"""

# import packages
import os
import pandas as pd
import numpy as np
from balsa.matrices import read_mdf, to_mdf, to_fortran, read_fortran_square, read_fortran_rectangle
import Mlogit_Probe_EarlyValid as mprobe_valid
import common


class MandatoryOd(object):
    
    """
    
    """
    
    def __init__(self, seed):

        self.seed = seed

        # check if files exist. If not, then stop program
        if not self.validation():
            mprobe_valid.logger.info("validataion failed")
            exit(0)
        else:
            mprobe_valid.logger.info("validataion success")

        # batch requisite json file for DTYPE
        self.dataFrameDtype = common.dtype_defintions(mprobe_valid.dirListing_abm,
                                                      mprobe_valid.EarlyValidFiles_MlogitProbe.getJSONFileList())
    
    def validation(self):
        
        """
    
        """
        _errorMessage = ""
        
        # Validation checks for files, both the JSON DTYPE and csv
        EarlyValidFiles = mprobe_valid.EarlyValidFiles_MlogitProbe
        fileList = EarlyValidFiles.getJSONFileList()
        fileList.extend(EarlyValidFiles.getCSVFileList())
    
        # Check if the trips_out, hholds_out, and JSON files noted in the filelist exist in the path specified
        # by the user. If not, then raise error.
        for file in (fileList):
    
            check_existence = common.file_existence(mprobe_valid.dirListing_abm, file)
            mprobe_valid.logger.info("%s necessary to run the program found in the directory" % file)
    
            if not common.file_existence(mprobe_valid.dirListing_abm, file) :
                mprobe_valid.logger.info("%s necessary to run the program not found in the directory" %file)
                return False
    
        return True
    

    def identify_peak(self, trips_hhold_df):
        """


        :return:
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
        trips_hhold_df['peak_flag'] = np.where((1 - trips_hhold_df['peak_factor']) > trips_hhold_df['rnum'], 0, 1)
        trips_hhold_df[['peak_flag']] = trips_hhold_df[['peak_flag']].astype('int8')  # save some memory
        mprobe_valid.logger.info("Return the trips and household dataframe combined with each record tagged as to whether"
                                 "it starts in the peak (1) or off-peak period (0)")
        return trips_hhold_df

      
    def run(self, mand_purposes, education):
        """


        :return:
        """
        # bring in the GGHMV4's household and trip list files and attach a market segment to each household
        mprobe_valid.logger.info("Batch in the household and trips list files from a gghm run")
        hh = pd.read_csv(os.path.join(mprobe_valid.dirListing_abm, mprobe_valid.EarlyValidFiles_MlogitProbe.HOUSE_HOLDS_OUT))
        trips = pd.read_csv(os.path.join(mprobe_valid.dirListing_abm, mprobe_valid.EarlyValidFiles_MlogitProbe.TRIPS_OUT))
        hh = common.market_segment(hh)  # tag each household by the market segment it belongs to

        # set dtypes for the household and trips dataframe to reduce memory requirements
        for key, value in self.dataFrameDtype[mprobe_valid.EarlyValidFiles_MlogitProbe.DTYPE_TRIPS].items():
            trips[key] = trips[key].astype(value)

        for key, value in self.dataFrameDtype[mprobe_valid.EarlyValidFiles_MlogitProbe.DTYPE_HOUSEHOLDS].items():
            hh[key] = hh[key].astype(value)

        # Merge the hholds info to the trips. By doing so, we can bring in a bunch of household attributes
        # including income, dwelling type, size, number of vehicles, and auto_sufficiency. Add in an integer
        # definition for one of six market segments.
        trips_hhold = pd.merge(trips, hh, how='left', left_on='hhid', right_on='hhid')
        trips_hhold = self.identify_peak(trips_hhold)

        # batch in ggh zone numbers and add in two columns for i and j zones
        ggh = pd.read_csv(os.path.join(mprobe_valid.dirListing_abm, mprobe_valid.EarlyValidFiles_MlogitProbe.GGH_EQUIV))
        ggh['key'] = 0
        # make a copy of the df and create a square matrix
        ggh1 = ggh
        ggh2 = pd.merge(ggh1, ggh, how='left', on='key')

        # generate the matrices desired by MLOGIT
        mprobe_valid.logger.info("Start evaluating the mandatory purposes")
        for purpose in mand_purposes:

            mprobe_valid.logger.info("Evaluating the %s purpose" % purpose)

            # because the school and university purposes don't have any market segmentation, set it to 0.
            if purpose in education:
                mand_only = trips_hhold.loc[(trips_hhold['purpose'] == purpose)]
                mand_only['market_seg'] = 0  # set this to a defauly market segment of 0
            else:
                mand_only = trips_hhold.loc[(trips_hhold['purpose'] == purpose)]

                # now loop over the peak periods
            for peak in range(0, 2):
                mprobe_valid.logger.info("Start evaluating the peak_flag %s" % peak)

                timeperiod_df = mand_only.loc[mand_only['peak_flag'] == peak]
                timeperiod_df = timeperiod_df.groupby(['taz_i', 'taz_j', 'purpose', 'market_seg']).size().reset_index(
                    name='freq')

                # now loop over the segments
                for segment in timeperiod_df['market_seg'].unique():
                    mprobe_valid.logger.info("Start evaluating the segment %s" % segment)
                    # create filename and then groupby
                    # only keep relevant cols and set a flag
                    # Merge the ggh zones and the trip list and convert to wide format

                    fname = purpose + "_" + str(segment)
                    df_hbw = timeperiod_df.loc[timeperiod_df['market_seg'] == segment]
                    df_hbw = df_hbw[['taz_i', 'taz_j']]
                    df_hbw['probflag'] = 1

                    # this merge is not necessary, but I am being on the safe side and bringing in the equiv file we have in TRESO-code
                    df_hbw1 = pd.merge(ggh2, df_hbw, how="left", left_on=['ggh_zone_x', 'ggh_zone_y'],
                                       right_on=['taz_i', 'taz_j'])
                    df_hbw2 = df_hbw1.pivot_table(index='ggh_zone_x', columns='ggh_zone_y', values='probflag',
                                                  fill_value=0)

                    mprobe_valid.logger.info("Saving file to the requisite Fortran format")
                    to_fortran(df_hbw2, os.path.join(mprobe_valid.dirListing_abm, fname +
                                                             ' peak_flag ' + str(peak) + '.bin'), n_columns = 4000)



