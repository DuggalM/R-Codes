import pandas as pd
import numpy as np
import os
from validate_input import EarlyValidation

class BinaryDataFormat(object):

    def __init__(self):
        pass

    def bring_binfiles(self, filepath, filename, dtype_json):

        """
        This function batches in a binary file and sets the appropriate dtype format for the columns
        :param filepath:
        :param filename:
        :param dtype_json:
        :return:
        """

        df = pd.DataFrame(np.fromfile(os.path.join(filepath, filename),
                                                              dtype=[tuple(t) for t in dtype_json]))
        return df

