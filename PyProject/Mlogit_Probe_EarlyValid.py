# -*- coding: utf-8 -*-
"""
Created on Thu Dec  7 18:07:02 2017

@author: MZD
"""


import os
import logging

#%% Set the directory structure and other user controls
dirListing_abm = 'c:\\personal\\IMM\\'
dirListing_othertrips = 'c:\\personal\\IMM\\Other Trips\\'

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
    HBM_PEAK_MSEG0 = 'trips_peak_all_modes_hbm_nocar_low.bin'
    HBM_PEAK_MSEG1 = 'trips_peak_all_modes_hbm_nocar_high.bin'
    HBM_PEAK_MSEG2 = 'trips_peak_all_modes_hbm_insuff_low.bin'
    HBM_PEAK_MSEG3 = 'trips_peak_all_modes_hbm_insuff_high.bin'
    HBM_PEAK_MSEG4 = 'trips_peak_all_modes_hbm_suff_low.bin'
    HBM_PEAK_MSEG5 = 'trips_peak_all_modes_hbm_suff_high.bin'
    
    HBO_PEAK_MSEG0 = 'trips_peak_all_modes_hbo_nocar_low.bin'
    HBO_PEAK_MSEG1 = 'trips_peak_all_modes_hbo_nocar_high.bin'
    HBO_PEAK_MSEG2 = 'trips_peak_all_modes_hbo_insuff_low.bin'
    HBO_PEAK_MSEG3 = 'trips_peak_all_modes_hbo_insuff_high.bin'
    HBO_PEAK_MSEG4 = 'trips_peak_all_modes_hbo_suff_low.bin'
    HBO_PEAK_MSEG5 = 'trips_peak_all_modes_hbo_suff_high.bin'
    
    WBO_PEAK_MSEG0 = 'trips_peak_all_modes_wbo_nocar_low.bin'
    WBO_PEAK_MSEG1 = 'trips_peak_all_modes_wbo_nocar_high.bin'
    WBO_PEAK_MSEG2 = 'trips_peak_all_modes_wbo_insuff_low.bin'
    WBO_PEAK_MSEG3 = 'trips_peak_all_modes_wbo_insuff_high.bin'
    WBO_PEAK_MSEG4 = 'trips_peak_all_modes_wbo_suff_low.bin'
    WBO_PEAK_MSEG5 = 'trips_peak_all_modes_wbo_suff_high.bin'

    HBE_PEAK_MSEG0 = 'trips_peak_all_modes_hbe_nocar_low.bin'
    HBE_PEAK_MSEG1 = 'trips_peak_all_modes_hbe_nocar_high.bin'
    HBE_PEAK_MSEG2 = 'trips_peak_all_modes_hbe_insuff_low.bin'
    HBE_PEAK_MSEG3 = 'trips_peak_all_modes_hbe_insuff_high.bin'
    HBE_PEAK_MSEG4 = 'trips_peak_all_modes_hbe_suff_low.bin'
    HBE_PEAK_MSEG5 = 'trips_peak_all_modes_hbe_suff_high.bin'
    
    NHB_PEAK       = 'trips_peak_all_modes_nhb_all_segments.bin'

    ###################### OFF-PEAK ##########################
    HBM_OFFPEAK_MSEG0 = 'trips_offpeak_all_modes_hbm_nocar_low.bin'
    HBM_OFFPEAK_MSEG1 = 'trips_offpeak_all_modes_hbm_nocar_high.bin'
    HBM_OFFPEAK_MSEG2 = 'trips_offpeak_all_modes_hbm_insuff_low.bin'
    HBM_OFFPEAK_MSEG3 = 'trips_offpeak_all_modes_hbm_insuff_high.bin'
    HBM_OFFPEAK_MSEG4 = 'trips_offpeak_all_modes_hbm_suff_low.bin'
    HBM_OFFPEAK_MSEG5 = 'trips_offpeak_all_modes_hbm_suff_high.bin'

    HBO_OFFPEAK_MSEG0 = 'trips_offpeak_all_modes_hbo_nocar_low.bin'
    HBO_OFFPEAK_MSEG1 = 'trips_offpeak_all_modes_hbo_nocar_high.bin'
    HBO_OFFPEAK_MSEG2 = 'trips_offpeak_all_modes_hbo_insuff_low.bin'
    HBO_OFFPEAK_MSEG3 = 'trips_offpeak_all_modes_hbo_insuff_high.bin'
    HBO_OFFPEAK_MSEG4 = 'trips_offpeak_all_modes_hbo_suff_low.bin'
    HBO_OFFPEAK_MSEG5 = 'trips_offpeak_all_modes_hbo_suff_high.bin'

    WBO_OFFPEAK_MSEG0 = 'trips_offpeak_all_modes_wbo_nocar_low.bin'
    WBO_OFFPEAK_MSEG1 = 'trips_offpeak_all_modes_wbo_nocar_high.bin'
    WBO_OFFPEAK_MSEG2 = 'trips_offpeak_all_modes_wbo_insuff_low.bin'
    WBO_OFFPEAK_MSEG3 = 'trips_offpeak_all_modes_wbo_insuff_high.bin'
    WBO_OFFPEAK_MSEG4 = 'trips_offpeak_all_modes_wbo_suff_low.bin'
    WBO_OFFPEAK_MSEG5 = 'trips_offpeak_all_modes_wbo_suff_high.bin'

    HBE_OFFPEAK_MSEG0 = 'trips_offpeak_all_modes_hbe_nocar_low.bin'
    HBE_OFFPEAK_MSEG1 = 'trips_offpeak_all_modes_hbe_nocar_high.bin'
    HBE_OFFPEAK_MSEG2 = 'trips_offpeak_all_modes_hbe_insuff_low.bin'
    HBE_OFFPEAK_MSEG3 = 'trips_offpeak_all_modes_hbe_insuff_high.bin'
    HBE_OFFPEAK_MSEG4 = 'trips_offpeak_all_modes_hbe_suff_low.bin'
    HBE_OFFPEAK_MSEG5 = 'trips_offpeak_all_modes_hbe_suff_high.bin'

    NHB_OFFPEAK = 'trips_offpeak_all_modes_nhb_all_segments.bin'
    

    @classmethod
    def getBINFileList(cls):
        return [
            cls.HBM_PEAK_MSEG0, cls.HBM_PEAK_MSEG1, cls.HBM_PEAK_MSEG2,
            cls.HBM_PEAK_MSEG3, cls.HBM_PEAK_MSEG4, cls.HBM_PEAK_MSEG5,
            cls.HBO_PEAK_MSEG0, cls.HBO_PEAK_MSEG1, cls.HBO_PEAK_MSEG2,
            cls.HBO_PEAK_MSEG3, cls.HBO_PEAK_MSEG4, cls.HBO_PEAK_MSEG5,
            cls.WBO_PEAK_MSEG0, cls.WBO_PEAK_MSEG1, cls.WBO_PEAK_MSEG2,
            cls.WBO_PEAK_MSEG3, cls.WBO_PEAK_MSEG4, cls.WBO_PEAK_MSEG5,
            cls.HBE_PEAK_MSEG0, cls.HBE_PEAK_MSEG1, cls.HBE_PEAK_MSEG2,
            cls.HBE_PEAK_MSEG3, cls.HBE_PEAK_MSEG4, cls.HBE_PEAK_MSEG5,
            cls.NHB_PEAK,
            cls.HBM_OFFPEAK_MSEG0, cls.HBM_OFFPEAK_MSEG1, cls.HBM_OFFPEAK_MSEG2,
            cls.HBM_OFFPEAK_MSEG3, cls.HBM_OFFPEAK_MSEG4, cls.HBM_OFFPEAK_MSEG5,
            cls.HBO_OFFPEAK_MSEG0, cls.HBO_OFFPEAK_MSEG1, cls.HBO_OFFPEAK_MSEG2,
            cls.HBO_OFFPEAK_MSEG3, cls.HBO_OFFPEAK_MSEG4, cls.HBO_OFFPEAK_MSEG5,
            cls.WBO_OFFPEAK_MSEG0, cls.WBO_OFFPEAK_MSEG1, cls.WBO_OFFPEAK_MSEG2,
            cls.WBO_OFFPEAK_MSEG3, cls.WBO_OFFPEAK_MSEG4, cls.WBO_OFFPEAK_MSEG5,
            cls.HBE_OFFPEAK_MSEG0, cls.HBE_OFFPEAK_MSEG1, cls.HBE_OFFPEAK_MSEG2,
            cls.HBE_OFFPEAK_MSEG3, cls.HBE_OFFPEAK_MSEG4, cls.HBE_OFFPEAK_MSEG5,
            cls.NHB_OFFPEAK
        ]

    @classmethod
    def getCSVFileList(cls):
        return [cls.HOUSE_HOLDS_OUT, cls.TRIPS_OUT, cls.GGH_EQUIV]
    
    @classmethod
    def getJSONFileList(cls):
        return [cls.DTYPE_HOUSEHOLDS, cls.DTYPE_TRIPS]

    # dictionary of nonmandatory datafarmes by time period
    dict_othertrips_names = {
    # PEAK PERIOD
    'HBM_0_1': 'trips_peak_all_modes_hbm_nocar_low.bin',
    'HBM_1_1': 'trips_peak_all_modes_hbm_nocar_high.bin',
    'HBM_2_1': 'trips_peak_all_modes_hbm_insuff_low.bin',
    'HBM_3_1': 'trips_peak_all_modes_hbm_insuff_high.bin',
    'HBM_4_1': 'trips_peak_all_modes_hbm_suff_low.bin',
    'HBM_5_1': 'trips_peak_all_modes_hbm_suff_high.bin',
    'HBO_0_1': 'trips_peak_all_modes_hbo_nocar_low.bin',
    'HBO_1_1': 'trips_peak_all_modes_hbo_nocar_high.bin',
    'HBO_2_1': 'trips_peak_all_modes_hbo_insuff_low.bin',
    'HBO_3_1': 'trips_peak_all_modes_hbo_insuff_high.bin',
    'HBO_4_1': 'trips_peak_all_modes_hbo_suff_low.bin',
    'HBO_5_1': 'trips_peak_all_modes_hbo_suff_high.bin',
    'NHB_0_1': 'trips_peak_all_modes_nhb_all_segments.bin',
    'NHB_1_1': 'trips_peak_all_modes_nhb_all_segments.bin',
    'NHB_2_1': 'trips_peak_all_modes_nhb_all_segments.bin',
    'NHB_3_1': 'trips_peak_all_modes_nhb_all_segments.bin',
    'NHB_4_1': 'trips_peak_all_modes_nhb_all_segments.bin',
    'NHB_5_1': 'trips_peak_all_modes_nhb_all_segments.bin',
    'WBO_0_1': 'trips_peak_all_modes_wbo_nocar_low.bin',
    'WBO_1_1': 'trips_peak_all_modes_wbo_nocar_high.bin',
    'WBO_2_1': 'trips_peak_all_modes_wbo_insuff_low.bin',
    'WBO_3_1': 'trips_peak_all_modes_wbo_insuff_high.bin',
    'WBO_4_1': 'trips_peak_all_modes_wbo_suff_low.bin',
    'WBO_5_1': 'trips_peak_all_modes_wbo_suff_high.bin',
    'HBE_0_1': 'trips_peak_all_modes_hbe_nocar_low.bin',
    'HBE_1_1': 'trips_peak_all_modes_hbe_nocar_high.bin',
    'HBE_2_1': 'trips_peak_all_modes_hbe_insuff_low.bin',
    'HBE_3_1': 'trips_peak_all_modes_hbe_insuff_high.bin',
    'HBE_4_1': 'trips_peak_all_modes_hbe_suff_low.bin',
    'HBE_5_1': 'trips_peak_all_modes_hbe_suff_high.bin',

    # OFFPEAK PERIOD
    'HBM_0_0': 'trips_offpeak_all_modes_hbm_nocar_low.bin',
    'HBM_1_0': 'trips_offpeak_all_modes_hbm_nocar_high.bin',
    'HBM_2_0': 'trips_offpeak_all_modes_hbm_insuff_low.bin',
    'HBM_3_0': 'trips_offpeak_all_modes_hbm_insuff_high.bin',
    'HBM_4_0': 'trips_offpeak_all_modes_hbm_suff_low.bin',
    'HBM_5_0': 'trips_offpeak_all_modes_hbm_suff_high.bin',
    'HBO_0_0': 'trips_offpeak_all_modes_hbo_nocar_low.bin',
    'HBO_1_0': 'trips_offpeak_all_modes_hbo_nocar_high.bin',
    'HBO_2_0': 'trips_offpeak_all_modes_hbo_insuff_low.bin',
    'HBO_3_0': 'trips_offpeak_all_modes_hbo_insuff_high.bin',
    'HBO_4_0': 'trips_offpeak_all_modes_hbo_suff_low.bin',
    'HBO_5_0': 'trips_offpeak_all_modes_hbo_suff_high.bin',
    'NHB_0_0': 'trips_offpeak_all_modes_nhb_all_segments.bin',
    'NHB_1_0': 'trips_offpeak_all_modes_nhb_all_segments.bin',
    'NHB_2_0': 'trips_offpeak_all_modes_nhb_all_segments.bin',
    'NHB_3_0': 'trips_offpeak_all_modes_nhb_all_segments.bin',
    'NHB_4_0': 'trips_offpeak_all_modes_nhb_all_segments.bin',
    'NHB_5_0': 'trips_offpeak_all_modes_nhb_all_segments.bin',
    'WBO_0_0': 'trips_offpeak_all_modes_wbo_nocar_low.bin',
    'WBO_1_0': 'trips_offpeak_all_modes_wbo_nocar_high.bin',
    'WBO_2_0': 'trips_offpeak_all_modes_wbo_insuff_low.bin',
    'WBO_3_0': 'trips_offpeak_all_modes_wbo_insuff_high.bin',
    'WBO_4_0': 'trips_offpeak_all_modes_wbo_suff_low.bin',
    'WBO_5_0': 'trips_offpeak_all_modes_wbo_suff_high.bin',
    'HBE_0_0': 'trips_offpeak_all_modes_hbe_nocar_low.bin',
    'HBE_1_0': 'trips_offpeak_all_modes_hbe_nocar_high.bin',
    'HBE_2_0': 'trips_offpeak_all_modes_hbe_insuff_low.bin',
    'HBE_3_0': 'trips_offpeak_all_modes_hbe_insuff_high.bin',
    'HBE_4_0': 'trips_offpeak_all_modes_hbe_suff_low.bin',
    'HBE_5_0': 'trips_offpeak_all_modes_hbe_suff_high.bin'
    }

    # dictionary of Column DTYPES of the non-mandatory dataframe
    dict_nonmandatory_dtype = {
        'origin': 'int16',
        'destination': 'int16',
        'trips': 'float32',
        'mseg': 'int16',
        'wholetrips': 'int16',
        'period': 'category'
    }
