# This file contains all the user defined controls like directory listing, filelists, binary lists etc.
# All modifications to customize a run should be made here.
########################################################################################################################
# user to set file path and set file names if different from the default
dirListing = 'c:\\personal\\IMM\\'

# set random seed to be used
seed = 12345
logname = "discretization.log"

# set chunk parameter. This should be the same as in MLogit for outputting probabilities and equals the number of
# files for each trip purpose and time period in the binary probability list
chunk = 1


########################################################################################################################
# EARLY VALIDATION FILES
# files that the validate_input module needs to check for and stop the code if they don't exist

class EarlyValidFiles(object):
    HOUSE_HOLDS_OUT = 'households_out.csv'
    TRIPS_OUT = 'trips_out.csv'
    SAMPLE_VEH_PROPS = 'sample_veh_proportions_gghm_zone_csd.csv'
    DTYPE_ELEMENTAL_PROB = 'dtype_primarymode_prob.json'
    DTYPE_STATION_CHOICE = 'dtype_stationchoice.json'
    DTYPE_EGRESS_PROB = 'dtype_egressmode_prob.json'
    DTYPE_TRIPS_PROCESSED = 'dtype_tripsprocessed.json'
    DTYPE_HOUSEHOLDS = 'dtype_households.json'
    DTYPE_TRIPS = 'dtype_trips.json'

    @classmethod
    def getJSONFileList(cls):
        return [
            cls.DTYPE_EGRESS_PROB, cls.DTYPE_ELEMENTAL_PROB, cls.DTYPE_HOUSEHOLDS,
            cls.DTYPE_STATION_CHOICE, cls.DTYPE_TRIPS_PROCESSED, cls.DTYPE_TRIPS]

    @classmethod
    def getCSVFileList(cls):
        return [cls.HOUSE_HOLDS_OUT, cls.TRIPS_OUT, cls.SAMPLE_VEH_PROPS]


########################################################################################################################
# BINARY PROBABILITIES
# Bill's binary probability and access/egress station ids split into chunks. There must be:
# 7 (trip purposes) X 3 (primary mode elemental probabilities) X 2 (time periods) X 10 (chunks) = 420 files.
# If the user changes the number of chunks option in MLOGIT than the appropriate file names need to be added in the list
# below.

# TODO-mausam add in the remaining trip purposes as well

binary_dict = {
    'hbw': [('hbw_mode_peak_bin', ['hbw_peak_tresodat_%s.bin' % str(i) for i in range(1, chunk + 1)]),
            ('hbw_mode_offpeak_bin', ['hbw_offpeak_tresodat_%s.bin' % str(i) for i in range(1, chunk + 1)]),
            ('hbw_stn_peak_bin', ['hbw_peak_tresosta_%s.bin' % str(i) for i in range(1, chunk + 1)]),
            ('hbw_stn_offpeak_bin', ['hbw_offpeak_tresosta_%s.bin' % str(i) for i in range(1, chunk + 1)]),
            ('hbw_egg_peak_bin', ['hbw_peak_tresoegr_%s.bin' % str(i) for i in range(1, chunk + 1)]),
            ('hbw_egg_offpeak_bin', ['hbw_offpeak_tresoegr_%s.bin' % str(i) for i in range(1, chunk + 1)])],

    'hbs': [('hbs_mode_peak_bin', ['hbs_peak_tresodat_%s.bin' % str(i) for i in range(1, chunk + 1)]),
            ('hbs_mode_offpeak_bin', ['hbs_offpeak_tresosta_%s.bin' % str(i) for i in range(1, chunk + 1)]),
            ('hbs_stn_peak_bin', ['hbs_peak_tresosta_%s.bin' % str(i) for i in range(1, chunk + 1)]),
            ('hbs_stn_offpeak_bin', ['hbs_offpeak_tresosta_%s.bin' % str(i) for i in range(1, chunk + 1)]),
            ('hbs_egg_peak_bin', ['hbs_peak_tresoegr_%s.bin' % str(i) for i in range(1, chunk + 1)]),
            ('hbs_egg_offpeak_bin', ['hbs_offpeak_tresoegr_%s.bin' % str(i) for i in range(1, chunk + 1)])],
}
