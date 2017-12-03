import pandas as pd
import os
import control_parameters
from validate_input import EarlyValidation
from veh_sampler import VehicleSampling
from binary_file_mgmt import BinaryDataFormat

########################################################################################################################
# EARLY VALIDATION
########################################################################################################################

# Start with validating inputs
validate = EarlyValidation()

# Check if files noted in the filelist exist in the path specified by the user. If not, then raise error.
for file in control_parameters.early_valid_filelist:
    check_existence = validate.file_existence(control_parameters.dirListing, file)

# check if all the trip purpose and time period files are available in the user defined directory
for binaryfile in control_parameters.binary_dict:

    if len(binaryfile[1]) != control_parameters.chunk:
        print("The number of chunks in the binary file list don't match the chunk variable set in the control "
              "parameters file")
        break
    else:
        print("The number of binary files across the three probability definitions matches the number of chunks")


# Now grab the Json files for each of the dtypes
dtype_primarymode_prob, dtype_stationchoice, dtype_egressmode_prob, \
dtype_hh, dtype_trips, dtype_tripsprocessed = validate.dtype_defintions(control_parameters.dirListing,
                                                                        control_parameters.early_valid_filelist)

########################################################################################################################

def main():

    ####################################################################################################################
    # DATA BATCHING IN AND MUNGING FOR VEHICLE SAMPLING
    ####################################################################################################################
    ####################################################################################################################

    # bring in the GGHMV4's Trips_out and Household file and attach a market segment to each household
    hh = pd.read_csv(os.path.join(control_parameters.dirListing, "households_out.csv"))
    hh = validate.market_segment(hh)   # function application
    trips = pd.read_csv(os.path.join(control_parameters.dirListing, "trips_out.csv"))

    # set dtypes for the household and trips dataframe to reduce memory requirements
    for key, value in dtype_trips.items():
        trips[key] = trips[key].astype(value)

    for key, value in dtype_hh.items():
        hh[key] = hh[key].astype(value)

    # bring in Vehicle Type file. Bryce's file has a certain structure (wide format) that Bill prefers for MLOGIT.
    veh_type = pd.read_csv(os.path.join(control_parameters.dirListing, "sample_veh_proportions_gghm_zone_csd.csv"))
    veh_type.rename(columns={'ggh_zone': 'taz'}, inplace=True)
    veh_type = pd.melt(veh_type, id_vars=['taz']).sort_values(['taz', 'variable'])

    # attach the market segment to the veh_type. Also add in an integer based market seg.
    # The vehicle type is part of the variable column but starts after the second underscore
    # thus it is stripped out into a column of its own.
    veh_type['mseg1'] = veh_type['variable'].str.split('_').str[0]
    veh_type['mseg2'] = veh_type['variable'].str.split('_').str[1]
    veh_type['mseg'] = veh_type['mseg1'] + '_' + veh_type['mseg2']

    # add in integer based market segment category
    veh_type.loc[(veh_type['mseg'] == 'nocar_low'), 'market_seg'] = 0
    veh_type.loc[(veh_type['mseg'] == 'nocar_high'), 'market_seg'] = 1
    veh_type.loc[(veh_type['mseg'] == 'insuff_low'), 'market_seg'] = 2
    veh_type.loc[(veh_type['mseg'] == 'insuff_high'), 'market_seg'] = 3
    veh_type.loc[(veh_type['mseg'] == 'suff_low'), 'market_seg'] = 4
    veh_type.loc[(veh_type['mseg'] == 'suff_high'), 'market_seg'] = 5
    veh_type['market_seg'] = veh_type['market_seg'].astype('int8')

    # extract the vehicle type and drop unncessary columns
    veh_type['vtype1'] = veh_type['variable'].str.split('_').str[2]
    veh_type['vtype2'] = veh_type['variable'].str.split('_').str[3]
    veh_type['vtype'] = veh_type['vtype1'] + '_' + veh_type['vtype2']
    columns = ['mseg1', 'mseg2', 'mseg', 'vtype1', 'vtype2']
    veh_type.drop(columns, inplace=True, axis=1)

    ####################################################################################################################
    # RUN VEHICLE SAMPLING

    # invoke the class and run the vehicle sampling method. Once done,
    # concatenate the information back to the household dataframe
    veh_sampled = VehicleSampling()
    hh = veh_sampled.assign_vehtype(hh, veh_type, control_parameters.seed)

    ####################################################################################################################
    # DATA BATCHING IN AND MUNGING FOR PRIMARY/STATION/EGRESS MODE SAMPLING
    ####################################################################################################################
    ####################################################################################################################

    # Now bring in the binary files by purpose. Files are spread across probabilities of the main mode, the access and
    # egress station for any of the relevant primary modes, and finally probabilities of the egress mode from a station.
    # Further the probabilities are also broken down by peak vs off-peak.

    # get the binary class
    bd_format = BinaryDataFormat()

    # set dictionary to receive the various chunks for the trip purpose and file in question
    # hbw_mode_prob = {}
    #
    # for file in control_parameters.hbw_mode_peak_bin:
    #     hbw_mode_prob[file] = bd_format.bring_binfiles(control_parameters.dirListing, file, dtype_primarymode_prob)

    # concatenate the probabilities
    # hbw_mode_prob = pd.concat(hbw_mode_prob.values(), ignore_index=True)

    print("")

if __name__ == "__main__":
    main()



