
import pandas as pd
import numpy as np

if __name__ == '__main__':

    team = pd.read_csv('../data/intermediate/Max_Animals_TEAM_data.csv', index_col=0)
    team['Sampling Period'] = team['Sampling Period'].apply(lambda x: str(x).replace('.', '-'))
    team['Year'] = team['Sampling Period'].apply(lambda x: x[0:4])

    lat_long = pd.read_csv('../data/original/Terrestrial_Vertebrate.csv', index_col=0)
    lat_long = lat_long[['Site Name', 'Sampling Unit Name' , 'Latitude', 'Longitude']]
    lat_long = lat_long.drop_duplicates(['Sampling Unit Name', 'Site Name']).reset_index()

    team = pd.merge(team, lat_long, on=['Sampling Unit Name', 'Site Name'], how='left')
    team['Sampling Type'] = 'Annual'
    team['Data Source'] = 'TEAM'

    rate = pd.read_csv('../data/intermediate/team_dataset_trapnights.csv')
    rate['Sampling.Period'] = rate['Sampling.Period'].apply(lambda x: str(x).replace('.', '-'))

    team = pd.merge(team, rate, left_on= ['Sampling Unit Name', 'Sampling Period'], how = 'inner',
             right_on = ['Deployment.Location.ID', 'Sampling.Period'])
    team['trap_nights_per_100'] = team['trapnights']/100.
    team['Rate Of Detection'] = team['Number of Animals']/team['trap_nights_per_100']
    team['Sampling Period'] = 1
    
    team.rename(columns = {'Site Name': 'Project ID', 'Latitude' : 'Latitude Resolution',
                        'Longitude' :'Longitude Resolution', 'Deployment.Location.ID' :'Deployment Location ID'}, inplace = True)

    team = team[['Data Source', 'Project ID', 'Deployment Location ID', 'Latitude Resolution', 'Longitude Resolution', 
    'Sampling Type', 'Sampling Period', 'Year', 'Genus', 'Species', 'Rate Of Detection']]

    team.to_csv('../data/processed/team_rate_of_detection.csv', index=False)
