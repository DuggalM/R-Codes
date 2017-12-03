from vehicle_sampling import VehicleSampling
import control_parameters
from mode_sampling import MandatoryModeSampling
import pandas as pd
import numpy as np
import os
from mode_sampling import MandatoryModeSampling


def main():

    # the sequence in which the various classes are called is the following:
    # step 0: set control parameters as needed
    # step 1: call and run VehicleSampling class
    # step 2:
    seed = control_parameters.seed
    trips_vehtype = VehicleSampling(seed).run()






if __name__ == "__main__":
    main()

