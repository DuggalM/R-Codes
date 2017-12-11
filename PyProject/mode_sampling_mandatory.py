# -*- coding: utf-8 -*-
"""
Created on Thu Nov 30 18:07:02 2017

@author: MZD
"""

# Import packages
import pandas as pd
import numpy as np
from balsa.cheval import sample_from_weights
import control_parameters
import common
from control_parameters import EarlyValidFiles
import os
import logging

# logger = logging.getLogger("super_model")
#TODO the return mode of the mandatory tour purpose should be the same as the outgoing mode sampled. This will also
#TODO require swtiching the egress and access modes

class MandatoryModeSampling(object):



    def __init__(self, seed):

        self.seed = seed



    def vehtype_prob(self, purp_prob, veh_segment):
        """
        This function takes the elemental probabilities and slices it in to the relevant dfs for each vehicle type

        :param self:
        :param purp_prob: binary probability file
        :param veh_segment: vehicle segment being sliced. int format
        :return: slice probability dataframe coressponding to the vehicle type in question
        """
        # get column indices to slice
        segment = int(float(veh_segment))

        # get the number of columns in the purpose dataframe and divide by four to get index positions
        val = purp_prob.shape[1]
        val = int(float(val / 4))

        # if the number of columns are even numbered
        if val % 2 == 0:

            s_pos = segment * val
            e_pos = segment * val + val
            col_ind = list(range(s_pos, e_pos))
            lvl1 = purp_prob.iloc[:, col_ind].reset_index()
            lvl1.set_index('index', inplace=True)

            return lvl1
        else:
            common.logger.error('The columns are not even numbered. The slicing of the purpose dataframe is wrong')


    def prob_df_longtermchoice(self, trips_df, purpose, df_prob, veh_segment):
        """
        This function takes the mandatory trip purpose records and appends the appropriate Veh_Segment probability,
        and then expands the dataframe which equals to the number of trips that need to be sampled between an O-D pair
        to get it ready for sampling via Cheval.

        :param self:
        :param trips_df: The trips dataframe
        :param purpose: trip purpose
        :param df_prob: binary probability file to sample from
        :param veh_segment: vehicle segment
        :return: expanded dataframe of binary probabilities, trips dataframe by purpose and vehicle type,
        """
        # group and get the count of trips for each unique pair that need to be sampled from Bill's probability file
        # Only keep those records where both the origin and destination are known.
        # get the dataframe for the purpose and veh_segment
        gr = trips_df.loc[(trips_df['purpose'] == purpose) & (trips_df['vseg'] == veh_segment)]
        gr_purp = gr.groupby(['taz_i', 'taz_j', 'market_seg', 'vseg']).size().reset_index(name='counts')
        gr_purp = gr_purp.loc[(gr_purp['taz_i'] > 0) & (gr_purp['taz_j'] > 0)]

        # Join the counts to the probability file generated by Bill.
        # Also, expand the dataframe as that allows using Cheval
        # The DROPNA is required because all those records that could not be
        # matched with Bill's peak probabilities, automatically belong to off-peak as well as only
        # belong to the chunk in question
        df_prob1 = pd.merge(df_prob, gr_purp, left_on=(['Production Zone', 'Destination Zone', 'Market Segment']),
                            right_on=(['taz_i', 'taz_j', 'market_seg']), how='left').dropna()

        # expand the dataframe based on the counts
        df_prob1['counts'] = df_prob1['counts'].astype(int)
        df_prob1 = df_prob1.loc[np.repeat(df_prob1.index.values, df_prob1['counts'])]

        # set the prod/att/market segments as a multi-index. This allows us to use Cheval for sampling.
        df_prob1.set_index(['Production Zone', 'Destination Zone', 'Market Segment'], inplace=True)

        return df_prob1, gr


    def elemental_mode(self, mand_prob_l1, veh_segment, mand_prob_append, purpose, trips):
        """
        This function samples

        :param self:
        :param mand_prob_l1:
        :param veh_segment:
        :param mand_prob_append:
        :param purpose:
        :param trips:
        :return:
        """

        # null dataframe
        mand_level1 = pd.DataFrame()
        sampled_df = pd.DataFrame()
        mand_l1 = pd.DataFrame()
        collect_df = {}

        # generate the appropriate df for sampling by veh_type
        # and then attach the production, destination, and market segment
        mand_l1 = self.vehtype_prob(mand_prob_l1, veh_segment)  # get the columns of prob that will be sampled
        mand_level1 = common.concat_df(mand_prob_append, mand_l1, 1)  # concat function

        # Now prepare the level1 file for sampling and only keep relevant columns
        mand_level2, df_join = self.prob_df_longtermchoice(trips, purpose, mand_level1, veh_segment)
        mand_level2 = mand_level2.iloc[:, 0:52]
        mand_level2 = mand_level2.loc[(mand_level2 != 0).any(axis=1)] # get rid of rows that are zero all the way

        if len(mand_level2) >0:
            common.logger.info("Start sampling of the elemental mode for vehicle segment %s. This is initiated "
                               "provided the binary probability file has records with "
                               "non-zero probabilities." %veh_segment)
            # sample using Cheval
            sampled_df = pd.DataFrame(sample_from_weights(mand_level2, randomizer=self.seed,
                                                          astype='category', n_threads=1, n_draws=1)).reset_index()
            sampled_df.columns = ['Production Zone', 'Destination Zone', 'Market Segment', 'Mode']

            # create  flag to help select the records in the trips dataframe. Creating the flag allows us to select
            # exactly the same number of rows even in the trips dataframe that match the sampled_df in length
            # Also sort the df to ensure that we don't end up concatenating the wrong o-ds
            sampled_df['flag'] = sampled_df['Production Zone'].astype(str) + sampled_df['Destination Zone'].astype(str) + \
                                 sampled_df['Market Segment'].astype(str)
            sampled_df = sampled_df.sort_values(['Production Zone', 'Destination Zone', 'Market Segment'])
            list_un = sampled_df['flag'].unique().tolist()

            # select from trips dataframe recores that corresspond to the sampled df using the flag. Once again sort
            # to ensure proper concatenation
            df_join = df_join.loc[(df_join['flag'].isin(list_un))]
            df_join = df_join.sort_values(['taz_i', 'taz_j', 'market_seg'])

            # concatenate the data. Concatenate is needed as
            # the flag is yet not unique and a merge will result in a larger dataframe than what we started with
            collect_df[veh_segment] = common.concat_df(df_join, sampled_df, 1)

        if len(collect_df) > 0:
            common.logger.info("Concatenate the dictionary of dataframes by vehicle segment")
            # now make one dataframe across vehicle segments
            mand_mode = pd.concat(collect_df.values(), ignore_index=True)
            mand_mode['PrimaryMode'] = mand_mode['Mode'].map(lambda x: str(x)[10:])

            return mand_mode

        # return empty df
        mand_mode = pd.DataFrame()
        common.logger.info("Returning an empty dataframe because there were no elemental probabilities in the "
                           "i-j pairs for the vehicle segment %s " %veh_segment)
        return mand_mode


    def access_egress_station(self, mand_st_acc, mand_st_egg, mand_mode):
        """

        :param mand_st_acc:
        :param mand_st_egg:
        :param mand_mode:
        :return:
        """
        mand_st_acc_unstack = mand_st_acc.melt(id_vars=['Production Zone', 'Destination Zone'])
        mand_st_acc_unstack.columns = ['Production Zone', 'Destination Zone', 'PrimaryMode', 'AccessZone']

        # strip off the unnecessary text and space
        mand_st_acc_unstack['PrimaryMode'] = mand_st_acc_unstack['PrimaryMode'].str[:-7]

        # PROCESS EGRESS STATION
        mand_st_egg_unstack = mand_st_egg.melt(id_vars=['Production Zone', 'Destination Zone'])
        mand_st_egg_unstack.columns = ['Production Zone', 'Destination Zone', 'PrimaryMode', 'EgressZone']

        # strip off the unnecessary text and space
        mand_st_egg_unstack['PrimaryMode'] = mand_st_egg_unstack['PrimaryMode'].str[:-7]

        # join the access and egress stations based on the primary mode chosen and also add in egressflag column
        mand_mode = mand_mode.merge(mand_st_acc_unstack,
                                    on=['Production Zone', 'Destination Zone', 'PrimaryMode'],
                                    how='left').merge(mand_st_egg_unstack,
                                                      on=['Production Zone', 'Destination Zone', 'PrimaryMode'],
                                                      how='left')
        mand_mode['egressflag'] = np.where(mand_mode['EgressZone'] > 0,
                                           mand_mode['Production Zone'].astype(str) + mand_mode['Destination Zone'].astype(
                                               str) + mand_mode['PrimaryMode'].astype(str), np.NAN)

        return mand_mode


    def egress_prob(self, mand_mode, mand_eg_prob):
        """

        :param mand_mode:
        :param mand_eg_prob:
        :return:
        """

        egg_df = mand_mode.loc[mand_mode['EgressZone'] > 0]
        cols = ['Production Zone', 'Destination Zone']
        egg_df[cols] = egg_df[cols].astype(int)

        # groupby and get the number of draws for each unique O-D pair that has an egress zone
        egg_df_gr = egg_df.groupby(['Production Zone', 'Destination Zone', 'PrimaryMode']).size().\
            reset_index(name='counts')

        # get column names and melt the dataframe on the production and destination zones
        # and then add in columns for defining the primary and egress modes
        melt_df = pd.melt(mand_eg_prob, id_vars=['Production Zone', 'Destination Zone'])
        melt_df['PrimaryMode'] = melt_df['variable'].str[26:]
        melt_df['EgressMode'] = melt_df['variable'].str[21:25]
        melt_df.drop('variable', axis=1, inplace=True)

        # get rid of any non-uniqueness and get it read for joining
        melt_df = melt_df.pivot_table(index=['Production Zone', 'Destination Zone', 'PrimaryMode'],
                                      columns='EgressMode', values='value').reset_index()

        # The melted df is now joined back to the group dataframe
        # so that the grouped df can be expanded by the counts and contains the egress probabilities as well.
        egg_df_gr1 = pd.merge(egg_df_gr, melt_df, on=['Production Zone', 'Destination Zone', 'PrimaryMode'], how='left')
        egg_df_gr1 = egg_df_gr1.loc[np.repeat(egg_df_gr1.index.values, egg_df_gr1['counts'])]

        # Now make the df back to a wide format and ready for sampling. ALso, Bill does not explicitly compute bus
        # probabilities, which are computed by subtracting Uber and Walk from 1.
        egg_df_gr1.set_index(['Production Zone', 'Destination Zone', 'PrimaryMode'], inplace=True)
        egg_df_gr1.drop('counts', axis=1, inplace=True)
        egg_df_gr1['Bus'] = 1 - (egg_df_gr1['Uber'] + egg_df_gr1['Walk'])

        # ' sample egress mode
        sampled_df_eg = pd.DataFrame(sample_from_weights(egg_df_gr1, randomizer = self.seed,
                                                         astype='category', n_threads=3, n_draws=1)).reset_index()

        egg_df_gr1 = common.concat_df(egg_df_gr1, sampled_df_eg, 1)
        egg_df_gr1.rename(columns={egg_df_gr1.columns[-1]: "EgressMode"}, inplace=True)

        # assign egress mode
        cols = [0, 1, 2]
        egg_df_gr1.drop(egg_df_gr1.columns[cols], axis=1, inplace=True)

        # like before we need a flag to join the information back to hbw_mode df. We also sort the dfs before concatenating
        egg_df_gr1['egressflag'] = egg_df_gr1['Production Zone'].astype(str) + \
                                   egg_df_gr1['Destination Zone'].astype(str) + \
                                   egg_df_gr1['PrimaryMode'].astype(str)
        egg_df_gr1 = egg_df_gr1.sort_values(['Production Zone', 'Destination Zone', 'egressflag'])

        # create unique list for selection
        list_un_eg = egg_df_gr1['egressflag'].unique().tolist()

        # get temp dataframe to do the assigning of the egress mode and this will then be later integrated with the
        # chunk being processed
        temp_df = mand_mode
        temp_df['egressflag'] = np.where(temp_df['EgressZone'] > 0,
                                         temp_df['Production Zone'].astype(str) + \
                                         temp_df['Destination Zone'].astype(str) + \
                                         temp_df['PrimaryMode'].astype(str), np.NAN)
        temp_df = temp_df.loc[(temp_df['egressflag'].isin(list_un_eg))].sort_values(
            ['Production Zone', 'Destination Zone', 'egressflag'])

        # concatenate the dfs
        temp_df = common.concat_df(temp_df, egg_df_gr1, 1)

        # remove the egress records from the hbw_mode chunk and replace them with with the temp dfs. One will
        # need to get rid of duplicated columns as well
        mand_mode = mand_mode.loc[mand_mode['egressflag'].isnull()]
        mand_mode = common.concat_df(mand_mode, temp_df, 0)

        return mand_mode


    def _getPeak_OffPeakFile(self, peak_offpeak, purpose):

        trip_bin = []
        stn_bin = []
        egg_bin = []
        if peak_offpeak.upper() == 'PEAK':
            trip_bin = control_parameters.binary_dict[purpose.lower()][0][1]
            stn_bin = control_parameters.binary_dict[purpose.lower()][2][1]
            egg_bin = control_parameters.binary_dict[purpose.lower()][4][1]
        elif peak_offpeak.upper() == 'OFF_PEAK':
            trip_bin = control_parameters.binary_dict[purpose.lower()][1][1]
            stn_bin = control_parameters.binary_dict[purpose.lower()][3][1]
            egg_bin = control_parameters.binary_dict[purpose.lower()][5][1]
        else:
            common.logger.error("Invalid time period definition")
        logging.info("bin file name", trip_bin, stn_bin, egg_bin)
        return   trip_bin, stn_bin, egg_bin



    def run(self, peak_offpeak, purpose, trips_vehtype):


        #hbw_trip_bin, hbw_stn_bin, hbw_egg_bin =
        # batch requisite json file for DTYPE
        common.logger.info("Batch in the DTYPE definitions")
        dataFrameDtype = common.dtype_defintions(control_parameters.dirListing,
                                                      EarlyValidFiles.getJSONFileList())

        hbw_trip_bin, hbw_stn_bin, hbw_egg_bin = self._getPeak_OffPeakFile(peak_offpeak, purpose)

                # dictionary of dfs to collect chunk df

        hbw_mode_allchunks = {}

        # With the data batched in it is time to now create access and egress station files for the trip purposes.
        # Need to first split the files by creating an acess and egress index
        access_indx = list(range(0, 14)) + list(range(26, 38)) + list(range(51, 63)) + list(range(75, 77))
        egress_indx = list(range(0, 2)) + list(range(14, 26)) + list(range(38, 51)) + list(range(63, 75))

        for chunk_prob, chunk_station, chunk_egress in zip(hbw_trip_bin, hbw_stn_bin, hbw_egg_bin):

            hbw_prob = pd.DataFrame(np.fromfile(os.path.join(control_parameters.dirListing, chunk_prob),
                                                dtype=[tuple(t) for t in
                                                       dataFrameDtype[EarlyValidFiles.DTYPE_ELEMENTAL_PROB]]))

            hbw_stn = pd.DataFrame(np.fromfile(os.path.join(control_parameters.dirListing, chunk_station),
                                               dtype=[tuple(t) for t in
                                                      dataFrameDtype[EarlyValidFiles.DTYPE_STATION_CHOICE]]))

            hbw_eg_prob = pd.DataFrame(np.fromfile(os.path.join(control_parameters.dirListing, chunk_egress),
                                                   dtype=[tuple(t) for t in
                                                          dataFrameDtype[EarlyValidFiles.DTYPE_EGRESS_PROB]]))


            # for work get the columns that represent the access and egress stations as they are mixed in Bill's file
            hbw_st_acc = hbw_stn.iloc[:, access_indx]
            hbw_st_egg = hbw_stn.iloc[:, egress_indx]

            # get the relevant columns to start the process
            hbw_prob_l1 = hbw_prob.iloc[:, 3:]
            hbw_prob_append = hbw_prob.iloc[:, 0:3]

            # The structure of the egress probability file is such that it requires some adjustments. Specifically,
            # the columns need to get appended as a row. Get the first two columns and then drop all the primary mode
            # columns as we only need the access mode and its probabilities in the df for Cheval
            # first_cols = [0, 1]
            s_pos = list(range(2, hbw_eg_prob.shape[1], 3))
            hbw_eg_prob.drop(hbw_eg_prob.columns[s_pos], axis=1, inplace=True)

            # run for the vehicle segments
            hbw_modes = {}
            common.logger.info("Start running vehicle segments for each chunk %s" %chunk_prob)
            for veh_segment in range(0, 4):

                hbw_mode = self.elemental_mode(hbw_prob_l1, veh_segment, hbw_prob_append, purpose, trips_vehtype)
                if len(hbw_mode)>0:

                    hbw_mode = self.access_egress_station(hbw_st_acc, hbw_st_egg, hbw_mode)
                    hbw_mode = self.egress_prob(hbw_mode, hbw_eg_prob)
                    hbw_modes[veh_segment] = hbw_mode

            if len(hbw_modes)>0:
                hbw_mode_allchunks[chunk_prob] = pd.concat(hbw_modes.values(), ignore_index=True)
                return pd.concat(hbw_mode_allchunks.values(), ignore_index=True)

        # if the probability file and consequently the remaining files are empty then return an empty dataframe
        hbw_mode_allchunks = pd.DataFrame()
        return hbw_mode_allchunks

