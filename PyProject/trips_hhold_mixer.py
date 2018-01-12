# -*- coding: utf-8 -*-
"""
Created on Thu Dec  10 18:07:02 2017

@author: MZD
"""

# import packages
import os
import pandas as pd
import Mlogit_Probe_EarlyValid as mprobe_valid
import common

class TripsHhold(object):


    def __init__(self):

        # batch requisite json file for DTYPE
        mprobe_valid.logger.info("Get DTYPE definitions for the various files")
        self.dataFrameDtype = common.dtype_defintions(mprobe_valid.dirListing_abm,
                                                      mprobe_valid.EarlyValidFiles_MlogitProbe.getJSONFileList())

    def mixer_run(self):

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

        return trips_hhold
