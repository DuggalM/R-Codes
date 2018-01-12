# -*- coding: utf-8 -*-
"""
Created on Thu Nov 30 18:07:02 2017

@author: MZD
"""

from vehicle_sampling import VehicleSampling
import control_parameters
from discretization import ModeSampling
import common


def main():
    common.logger.info("Processing Start")
    # the sequence in which the various classes are called is the following:
    # step 0: set seed from the control parameters.py
    # step 1: call and run VehicleSampling class
    # step 2: run peak and then off-peak mode sampling for each trip purpose
    seed = control_parameters.seed

    ####################################################################################################################
    common.logger.info("Sample and attach vehicle type to the households")
    trips_vehtype = VehicleSampling(seed).run()
    trips_vehtype.to_csv(r"c:/personal/imm/foo_nonmand_veh.csv", index = False)

    #TODO set market segm to start from 0 in Bill's prob files
    # ####################################################################################################################
    # common.logger.info("Sample and attach elemental modes to the peak HBW trip records")
    # mand_sample_pk = ModeSampling(seed).run("PEAK", "HBW", trips_vehtype)
    # mand_sample_pk.iloc[0:25000].to_csv(r"c:/personal/imm/foo.csv")

    # common.logger.info("Sample and attach elemental modes to the off-peak HBW trip records")
    # mand_sample_offpk = ModeSampling(seed).run("OFF_PEAK", "HBW", trips_vehtype)

    ####################################################################################################################

    #TODO repeat HBU and HBS mandatory trip purposes and then add the non-mand


    common.logger.info("Processing Ended")

    print("")
if __name__ == "__main__":
    main()

