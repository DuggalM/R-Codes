# -*- coding: utf-8 -*-
"""
Created on Thu Dec  7 18:07:02 2017

@author: MZD
"""

# import packages
import os
import pandas as pd
from balsa.matrices import to_fortran
import Mlogit_Probe_EarlyValid as mprobe_valid
import common


class MandatoryFortranOd(object):
    
    """
    
    """
    
    def __init__(self, seed):

        self.seed = seed

        # check if files exist. If not, then stop program
        if not self.validation():
            mprobe_valid.logger.info("validation failed")
            exit(0)
        else:
            mprobe_valid.logger.info("validation success")

        # batch requisite json file for DTYPE. This covers the household and trip dataframes only
        self.dataFrameDtype = common.dtype_defintions(mprobe_valid.dirListing_abm,
                                                      mprobe_valid.EarlyValidFiles_MlogitProbe.getJSONFileList())

    #TODO add validation of trip purposes by time period as well
    def validation(self):
        
        """
        This function validates if the files are available in the folder
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


    def run(self, trips_hhold, mand_purposes, education):
        """

        :param mand_purposes: A list of mandatory trip purposes for which MLOGIT inputs need to be created
        :param education: the list of educational (school, university) trip purposes
        :return: saves Fortran ready binary files for Mlogit
        """

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
                mand_only['market_seg'] = 0  # set this to a default market segment of 0
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
                    # only keep relevant cols and set a flag
                    # Merge the ggh zones and the trip list and convert to wide format

                    fname = purpose + "_" + str(segment)
                    df_hbw = timeperiod_df.loc[timeperiod_df['market_seg'] == segment]
                    df_hbw = df_hbw[['taz_i', 'taz_j']]
                    df_hbw['probflag'] = 1

                    # this merge is necessary to make a square matrix
                    df_hbw1 = pd.merge(ggh2, df_hbw, how="left", left_on=['ggh_zone_x', 'ggh_zone_y'],
                                       right_on=['taz_i', 'taz_j'])
                    df_hbw2 = df_hbw1.pivot_table(index='ggh_zone_x', columns='ggh_zone_y', values='probflag',
                                                  fill_value=0)

                    mprobe_valid.logger.info("Saving file to the requisite Fortran format")
                    to_fortran(df_hbw2, os.path.join(mprobe_valid.dirListing_abm, fname +
                                                             ' peak_flag ' + str(peak) + '.bin'), n_columns = 4000)

        return trips_hhold



