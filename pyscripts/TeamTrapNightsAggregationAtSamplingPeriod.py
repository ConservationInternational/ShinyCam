import pandas as pd, numpy as np, datetime, math, calendar

team = pd.read_csv('../data/StandardizedData/Terrestrial_Vertebrate_Cols_Edited.csv')

#columns of the marin dataset
team.columns

# Number of locations 
len(set(team['Deployment.Location.ID']))

# Number of projects
len(set(team['Project.ID']))

# SELECT the projections that we actually need
team_simple = team[['Project.ID',
'Deployment.Location.ID',
'Camera.Deployment.Begin.Date',
'Camera.Deployment.End.Date',
'Sampling.Period']]


team_simple_per_location = team_simple.groupby(['Project.ID', 'Deployment.Location.ID', 'Camera.Deployment.Begin.Date', 'Camera.Deployment.End.Date','Sampling.Period'])
team_simple_per_location

team_simple_per_location = team_simple_per_location.count().reset_index()

team_simple_per_location['Camera.Deployment.Begin.Date_datetime'] = pd.to_datetime(team_simple_per_location['Camera.Deployment.Begin.Date'])
team_simple_per_location['Camera.Deployment.End.Date_datetime'] = pd.to_datetime(team_simple_per_location['Camera.Deployment.End.Date'])

team_simple_per_location['trapnights'] = (team_simple_per_location['Camera.Deployment.End.Date_datetime'] 
                                          - 
        team_simple_per_location['Camera.Deployment.Begin.Date_datetime']).astype(pd.Timedelta).apply(lambda l: max(1, l.days))

result = team_simple_per_location[['Project.ID',
'Deployment.Location.ID',
'Sampling.Period',
'trapnights']]


result.to_csv('../data/team_trap_days.csv')

