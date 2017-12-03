# Import packages
import pandas as pd
from balsa.cheval import sample_from_weights
from common import concat_df


class VehicleSampling(object):


    def __init__(self):
        pass

    def assign_vehtype(self, hh_file, vehtype_file, seed):

        """
        This function takes the household file and processed vehicle type file and samples a vehicle to attach to each
        household.
        :param hh_file:
        :param vehtype_file:
        :return: sample_df
        """

        # join the vehicle probabilities to the households so that we can sample from them. It is easy enough
        # to attach the probabilities because we know the market segment of each household.
        hh_vehprob = pd.merge(hh_file, vehtype_file, left_on=['taz', 'market_seg'],
                              right_on=['taz', 'market_seg'], how='left')

        # now unstack and get it ready for Cheval
        hh_vehprob = hh_vehprob.pivot(index='hhid', columns='vtype', values='value')

        # Sample a vehicle type using Cheval
        sample_df = pd.DataFrame(sample_from_weights(hh_vehprob, randomizer=seed, astype='category', n_threads=3))
        sample_df.columns = ['hh_veh_type']

        hh_file = concat_df(hh_file, sample_df, 1)

        return hh_file







