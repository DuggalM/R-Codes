# -*- coding: utf-8 -*-
"""
Created on Thu Dec  7 18:07:02 2017

@author: MZD
"""

import control_parameters
from Mlogit_Probe import MandatoryOd
import Mlogit_Probe_EarlyValid as mprobe_valid
import logging


def main():
    mprobe_valid.logger.info("Processing Start")
    seed = control_parameters.seed

    mprobe_valid.logger.info("Run Mlogit_Probe for the mandatory trip purposes")
    mand_purposes = ['HBW', 'HBS', 'HBU']
    education = ['HBS', 'HBU']
    mand_prob_pairs = MandatoryOd(seed).run(mand_purposes, education)




    mprobe_valid.logger.info("Processing Ended")

    print("")
if __name__ == "__main__":
    main()

