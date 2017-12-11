# -*- coding: utf-8 -*-
"""
Created on Thu Dec  7 18:07:02 2017

@author: MZD
"""


import os
import logging

#%% Set the directory structure and other user controls
dirListing_abm = 'c:\\personal\\IMM\\'
dirListing_peaktrips = 'c:\\personal\\IMM\\Other Trips\\peak'
dirListing_offpeaktrips = 'c:\\personal\\IMM\\Other Trips\\offpeak'

# set random seed to be used
seed = 12345
logname = "mlogit_probe.log"


def setupLogger(model_name):
    # remove pre existing log
    if os.path.exists(os.path.join(dirListing_abm, logname)):
        os.remove(os.path.join(dirListing_abm, logname))

    logger = logging.getLogger(model_name)
    logger.setLevel(logging.INFO)
    fh = logging.FileHandler(os.path.join(dirListing_abm, logname))
    fh.setLevel(logging.INFO)
    fh.setFormatter(logging.Formatter("%(asctime)s - %(name)s - %(funcName)s - %(levelname)s - %(message)s"))
    logger.addHandler(fh)

    return logger

logger = setupLogger("MLOGIT PROBE")

# Early validation of files. 
# This includes checking for the ABM components as well as the necessary trips
# from the GGHM run.

class EarlyValidFiles_MlogitProbe(object):
    ####################### CSV ############################
    HOUSE_HOLDS_OUT = 'households_out.csv'
    TRIPS_OUT      = 'trips_out.csv'
    GGH_EQUIV      = 'GGH_zones.csv'

    ####################### JSON ###########################
    DTYPE_HOUSEHOLDS = 'dtype_households.json'
    DTYPE_TRIPS      = 'dtype_trips.json'

    ####################### PEAK ############################
    HBM_PEAK_MSEG1 = 'trips_peak_all_modes_hbm_nocar_low.bin'
    HBM_PEAK_MSEG2 = 'trips_peak_all_modes_hbm_nocar_high.bin'
    HBM_PEAK_MSEG3 = 'trips_peak_all_modes_hbm_insuff_low.bin'
    HBM_PEAK_MSEG4 = 'trips_peak_all_modes_hbm_insuff_high.bin'
    HBM_PEAK_MSEG5 = 'trips_peak_all_modes_hbm_suff_low.bin'
    HBM_PEAK_MSEG6 = 'trips_peak_all_modes_hbm_suff_high.bin'
    
    HBO_PEAK_MSEG1 = 'trips_peak_all_modes_hbo_nocar_low.bin'
    HBO_PEAK_MSEG2 = 'trips_peak_all_modes_hbo_nocar_high.bin'
    HBO_PEAK_MSEG3 = 'trips_peak_all_modes_hbo_insuff_low.bin'
    HBO_PEAK_MSEG4 = 'trips_peak_all_modes_hbo_insuff_high.bin'
    HBO_PEAK_MSEG5 = 'trips_peak_all_modes_hbo_suff_low.bin'
    HBO_PEAK_MSEG6 = 'trips_peak_all_modes_hbo_suff_high.bin'
    
    WBO_PEAK_MSEG1 = 'trips_peak_all_modes_wbo_nocar_low.bin'
    WBO_PEAK_MSEG2 = 'trips_peak_all_modes_wbo_nocar_high.bin'
    WBO_PEAK_MSEG3 = 'trips_peak_all_modes_wbo_insuff_low.bin'
    WBO_PEAK_MSEG4 = 'trips_peak_all_modes_wbo_insuff_high.bin'
    WBO_PEAK_MSEG5 = 'trips_peak_all_modes_wbo_suff_low.bin'
    WBO_PEAK_MSEG6 = 'trips_peak_all_modes_wbo_suff_high.bin'
    
    NHB       = 'trips_peak_all_modes_nhb_all_segments.bin'

    ###################### OFF-PEAK ##########################
    HBM_OFFPEAK_MSEG1 = 'trips_offpeak_all_modes_hbm_nocar_low.bin'
    HBM_PEAK_MSEG2 = 'trips_peak_all_modes_hbm_nocar_high.bin'
    HBM_PEAK_MSEG3 = 'trips_peak_all_modes_hbm_insuff_low.bin'
    HBM_PEAK_MSEG4 = 'trips_peak_all_modes_hbm_insuff_high.bin'
    HBM_PEAK_MSEG5 = 'trips_peak_all_modes_hbm_suff_low.bin'
    HBM_PEAK_MSEG6 = 'trips_peak_all_modes_hbm_suff_high.bin'

    HBO_PEAK_MSEG1 = 'trips_peak_all_modes_hbo_nocar_low.bin'
    HBO_PEAK_MSEG2 = 'trips_peak_all_modes_hbo_nocar_high.bin'
    HBO_PEAK_MSEG3 = 'trips_peak_all_modes_hbo_insuff_low.bin'
    HBO_PEAK_MSEG4 = 'trips_peak_all_modes_hbo_insuff_high.bin'
    HBO_PEAK_MSEG5 = 'trips_peak_all_modes_hbo_suff_low.bin'
    HBO_PEAK_MSEG6 = 'trips_peak_all_modes_hbo_suff_high.bin'

    WBO_PEAK_MSEG1 = 'trips_peak_all_modes_wbo_nocar_low.bin'
    WBO_PEAK_MSEG2 = 'trips_peak_all_modes_wbo_nocar_high.bin'
    WBO_PEAK_MSEG3 = 'trips_peak_all_modes_wbo_insuff_low.bin'
    WBO_PEAK_MSEG4 = 'trips_peak_all_modes_wbo_insuff_high.bin'
    WBO_PEAK_MSEG5 = 'trips_peak_all_modes_wbo_suff_low.bin'
    WBO_PEAK_MSEG6 = 'trips_peak_all_modes_wbo_suff_high.bin'

    NHB = 'trips_peak_all_modes_nhb_all_segments.bin'
    


    @classmethod
    def getBINFileList(cls):
        return [
            cls.HBM_PEAK_MSEG1, cls.HBM_PEAK_MSEG2, cls.HBM_PEAK_MSEG3,
            cls.HBM_PEAK_MSEG4, cls.HBM_PEAK_MSEG5, cls.HBM_PEAK_MSEG6,
            cls.HBO_PEAK_MSEG1, cls.HBO_PEAK_MSEG2, cls.HBO_PEAK_MSEG3,
            cls.HBO_PEAK_MSEG4, cls.HBO_PEAK_MSEG5, cls.HBO_PEAK_MSEG6,
            cls.WBO_PEAK_MSEG1, cls.WBO_PEAK_MSEG2, cls.WBO_PEAK_MSEG3,
            cls.WBO_PEAK_MSEG4, cls.WBO_PEAK_MSEG5, cls.WBO_PEAK_MSEG6,
            cls.NHB_PEAK]

    @classmethod
    def getCSVFileList(cls):
        return [cls.HOUSE_HOLDS_OUT, cls.TRIPS_OUT, cls.GGH_EQUIV]
    
    @classmethod
    def getJSONFileList(cls):
        return [cls.DTYPE_HOUSEHOLDS, cls.DTYPE_TRIPS]

