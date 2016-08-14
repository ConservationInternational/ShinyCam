
import pandas as pd
import numpy as np

if __name__ == '__main__':

    import pandas as pd
    import numpy as np
    import datetime

    marin = pd.read_csv('../data/intermediate/Max_Animals_MARIN_data.csv', index_col=0)
    marin['Sampling Period'] = marin['Sampling Period'].apply(lambda x: str(x).replace('.', '-'))
    marin['Year'] = marin['Sampling Period'].apply(lambda x: x[0:4])

    marin['Sampling Period'] = marin['Sampling Period'].apply(lambda x:
                                                              datetime.datetime.strptime(x, '%Y-%m'))

    lat_long = pd.read_csv('../data/intermediate/Marin_Merged.csv', index_col=0)
    lat_long = lat_long[['Deployment.Location.ID', 'Latitude.Resolution', 'Longitude.Resolution']]
    lat_long = lat_long.rename(columns = {'Deployment.Location.ID':'Deployment Location ID',
                     'Latitude.Resolution': 'Latitude',
                     'Longitude.Resolution': 'Longitude'})
    lat_long = lat_long.drop_duplicates('Deployment Location ID').reset_index()
    del lat_long['index']

    marin = pd.merge(marin, lat_long, on=['Deployment Location ID'], how='left')
    marin['Data Source'] = 'MARIN'

    rate = pd.read_csv('../data/intermediate/marin_trap_nights.csv')

    rate['Month'] = rate['Month'].apply(lambda x: datetime.datetime.strptime(x, '%Y-%m'))

    marin = pd.merge(marin, rate, left_on= ['Deployment Location ID', 'Sampling Period'], how = 'inner',
             right_on = ['Deployment.Location.ID', 'Month'])
    marin.head()

    marin['Sampling Period'].value_counts()

    marin['trap_days_per_100'] = marin['Trapnights']/100.
    marin['Rate Of Detection'] = marin['Number of Animals']/marin['trap_days_per_100']
    marin['Sampling Period'] = marin['Sampling Period'].apply(lambda x: x.month)
    marin['Sampling Type'] = 'Monthly'

    marin.rename(columns = {'Latitude' : 'Latitude Resolution',
                            'Longitude' :'Longitude Resolution',
                           'Project.ID' : 'Project ID'}, inplace = True)

    marin = marin[['Data Source', 'Project ID', 'Deployment Location ID', 'Latitude Resolution', 'Longitude Resolution',
    'Sampling Type', 'Sampling Period', 'Year', 'Genus', 'Species', 'Rate Of Detection']]

    marin.to_csv('../data/processed/rate_of_detection_MARIN.csv', index=False)
