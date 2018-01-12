# -*- coding: utf-8 -*-
"""
Created on Thu Dec  7 18:07:02 2017

@author: MZD
"""

import control_parameters
from Mlogit_Probe_Mandatory import MandatoryFortranOd
import Mlogit_Probe_EarlyValid as mprobe_valid
from trips_hhold_mixer import TripsHhold
from peak_offpeak import PeakOffpeak
from Mlogit_Probe_NonMandatory import NonMandatoryFortranOd


def main():
    mprobe_valid.logger.info("Processing Start")

    # the sequence in which the code is called is:
    # step 0: set seed from the control parameters.py
    # step 1: join the hhold information to the trips data
    # step 2: create peak/offpeak flag for each trip record
    # step 3: Run the mandatory trip purposes and get them ready for MLOGIT to generate probabilities
    # step 4: Run the non-mandatory trip purposes next for MLOGIT to generate probabilities

    mprobe_valid.logger.info("Get see from the control_parameters file")
    seed = control_parameters.seed
    prng = control_parameters.prng
    exploration_thold = control_parameters.chaos_monkey

    ####################################################################################################################
    # run the trips_hhold mixer and also tag each record with the peakoffpeak flag
    mprobe_valid.logger.info("Run the trips_hhold mixer and create the peak_offpeak flags")
    trips_hhold = TripsHhold().mixer_run()
    trips_hhold = PeakOffpeak().run(trips_hhold, 0)

    ####################################################################################################################
    # mprobe_valid.logger.info("Run Mlogit_Probe for the mandatory trip purposes")
    # mand_purposes = ['HBW', 'HBS', 'HBU']
    # education = ['HBS', 'HBU']
    # mand_prob_pairs = MandatoryFortranOd(seed).run(trips_hhold, mand_purposes, education)
    # mand_prob_pairs.to_csv(r'c:/personal/imm/mand')
    # ####################################################################################################################
    mprobe_valid.logger.info("Run Mlogit_Probe for the Non-Mandatory trip purposes")
    nonmandatory_purposes = ['HBO', 'HBM', 'WBO', 'NHB', 'HBE']

    # TODO remove the first line below for a full operational model
    trips_hhold = trips_hhold.loc[(trips_hhold['hhid'] > 0) & (trips_hhold['hhid'] < 10000)]
    nonmand_prob_pairs = NonMandatoryFortranOd(prng, exploration_thold).run(trips_hhold, nonmandatory_purposes)
    nonmand_prob_pairs.to_csv(r'c:/personal/imm/foo_nonmand.csv', index = False)




    mprobe_valid.logger.info("Processing Ended")

    print("")
if __name__ == "__main__":
    main()

