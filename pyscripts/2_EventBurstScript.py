
import pandas as pd
import numpy as np
from datetime import datetime
from datetime import timedelta


def fn(df, threshold):
    return pd.DataFrame(df.groupby(['Sampling Period', 'Site Name', 'Sampling Unit Name', 'Species']).apply(lambda x: x[x['Time Between Photos'] < threshold]['Number of Animals'].max()), columns = ['Number of Animals']).reset_index()

def fn_marin(df, threshold):
    return pd.DataFrame(df.groupby(['Sampling Period', 'Project ID_x', 'Deployment Location ID', 'Species']).apply(lambda x: x[x['Time Between Photos'] < threshold]['Count'].max()), columns = ['Number of Animals']).reset_index()
if __name__ == '__main__':
    team = pd.read_csv('../data/original/Terrestrial_Vertebrate.csv', index_col=0)
    team = team.sort_values(['Site Name', 'Sampling Unit Name', 'Species', 'Photo Time'])
    team = team.reset_index()

    team['NEW_PHOTO_TIME'] = team['Photo Date'] + ' ' + team['Photo Time']
    team['NEW_PHOTO_TIME'] = pd.to_datetime(team['NEW_PHOTO_TIME'])
    team['Time Between Photos'] = team['NEW_PHOTO_TIME'] - team['NEW_PHOTO_TIME'].shift()
    team['Time Between Photos'].fillna(timedelta(0), inplace=True)
    team['Species'].dropna(inplace=True)
    team['Time Between Photos'] = team['Time Between Photos'].astype('timedelta64[s]')
    team['Time Between Photos'] = team['Time Between Photos'].apply(lambda x: 0 if x > 100 else x)

    team_new = fn(team, 3)
    team_new.to_csv('Max_Animals_TEAM_data.csv')

    marin = pd.read_csv('../data/Marin_Merged.csv', index_col=0)

    marin['NEW_PHOTO_TIME'] = pd.to_datetime(marin['Date_Time Captured'])
    marin['Time Between Photos'] = marin['NEW_PHOTO_TIME'] - marin['NEW_PHOTO_TIME'].shift()
    marin['Time Between Photos'].fillna(timedelta(0), inplace=True)
    marin = marin[marin['Species'] != 'unknown']
    marin['Species'].dropna(inplace=True)
    marin['Time Between Photos'] = marin['Time Between Photos'].astype('timedelta64[s]')
    marin['Time Between Photos'] = marin['Time Between Photos'].apply(lambda x: 0 if x > 100 else x)
    marin['Date_Time Captured'].dropna(inplace=True)
    marin['Sampling Period'] = marin['Date_Time Captured'].apply(lambda x: x[0:7])

    marin_new = fn_marin(marin, 3)
    marin_new.to_csv('../data/intermediate/Max_Animals_MARIN_data.csv')
