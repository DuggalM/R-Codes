from vehicle_sampling import VehicleSampling
import control_parameters
from mode_sampling_mandatory import MandatoryModeSampling
import pandas as pd
import numpy as np
import os
from mode_sampling_mandatory import MandatoryModeSampling
import common
import logging

common.setupLogger()
logger = logging.getLogger("super_model")

def main():
    logger.info("Processing Start")
    # the sequence in which the various classes are called is the following:
    # step 0: set control parameters as needed
    # step 1: call and run VehicleSampling class
    # step 2:
    seed = control_parameters.seed
    trips_vehtype = VehicleSampling(seed).run()
    mand_sample = MandatoryModeSampling(seed).run("PEAK", "HBW", trips_vehtype)

    logger.info("Processing Ended")

if __name__ == "__main__":
    main()

