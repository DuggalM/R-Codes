from vehicle_sampling import VehicleSampling
import control_parameters
from mode_sampling_mandatory import MandatoryModeSampling
import pandas as pd
import numpy as np
import os
from mode_sampling_mandatory import MandatoryModeSampling
import common
import logging


def main():
    common.logger.info("Processing Start")
    # the sequence in which the various classes are called is the following:
    # step 0: set control parameters as needed
    # step 1: call and run VehicleSampling class
    # step 2:
    seed = control_parameters.seed
    common.logger.info("Sample and attach vehicle type to the households")
    trips_vehtype = VehicleSampling(seed).run()
    common.logger.info("Sample and attach elemental modes to the peak HBW trip records")
    mand_sample = MandatoryModeSampling(seed).run("PEAK", "HBW", trips_vehtype)
    # mand_sample.iloc[1:100].to_csv(r"c:/personal/IMM/mandatory_discretized.csv")
    common.logger.info("Sample and attach elemental modes to the off-peak HBW trip records")
    non_mand_sample = MandatoryModeSampling(seed).run("OFF_PEAK", "HBW", trips_vehtype)
    # non_mand_sample.iloc[1:100].to_csv(r"c:/personal/IMM/non_mandatory_discretized.csv")

    common.logger.info("Processing Ended")

    print("")
if __name__ == "__main__":
    main()

