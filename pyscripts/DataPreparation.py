
import pandas as pd
import numpy as np

if __name__ == '__main__':

    team = pd.read_csv('Max_Animals_TEAM_data.csv', index_col=0)
    team['Sampling Period'] = team['Sampling Period'].apply(lambda x: str(x).replace('.', '-'))
    team['Year'] = team['Sampling Period'].apply(lambda x: x[0:4])
    
    lat_long = pd.read_csv('../data/Terrestrial_Vertebrate.csv', index_col=0)
    lat_long = lat_long[['Site Name', 'Latitude', 'Longitude']]
    lat_long = lat_long.drop_duplicates('Site Name').reset_index()
    del lat_long['ID']
    
    team = pd.merge(team, lat_long, on=['Site Name'], how='left')
    team['Sampling Type'] = 'Annual'
    team['Data Source'] = 'TEAM'
    
    rate = pd.read_csv('../data/team_trap_days.csv')
    rate['Sampling.Period'] = rate['Sampling.Period'].apply(lambda x: str(x).replace('.', '-'))

    team = pd.merge(team, rate, left_on= ['Site Name', 'Sampling Unit Name', 'Sampling Period'], how = 'inner',
             right_on = ['Project.ID', 'Deployment.Location.ID', 'Sampling.Period'])
    team['trap_days_per_100'] = team['trap_days']/100.
    team['Rate Of Detection'] = team['Number of Animals']/team['trap_days_per_100']
    del team['trap_days_per_100']
    del team['trap_days']

    team.to_csv('rate_of_detection.csv', index=False)
