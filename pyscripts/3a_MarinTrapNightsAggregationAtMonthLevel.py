#export ipython as script

import pandas as pd, numpy as np, datetime, math, calendar

#dataset
marin = pd.read_csv('../data/StandardizedData/Marin_Merged.csv')
team = pd.read_csv('../data/StandardizedData/Terrestrial_Vertebrate_Cols_Edited.csv')

#columns of the marin dataset
marin.columns

# Number of locations 

len(set(marin['Deployment.Location.ID']))

# Number of projects

len(set(marin['Project.ID']))

# SELECT the projections that we actually need
marin_simple = marin[['Project.ID',
'Deployment.Location.ID',
'Camera.Deployment.Begin.Date',
'Camera.Deployment.End.Date']]


marin_simple_per_location = marin_simple.groupby(['Project.ID', 'Deployment.Location.ID', 'Camera.Deployment.Begin.Date', 'Camera.Deployment.End.Date'])
marin_simple_per_location

marin_simple_per_location = marin_simple_per_location.count().reset_index()

marin_simple_per_location['Camera.Deployment.Begin.Date_datetime'] = marin_simple_per_location['Camera.Deployment.Begin.Date'].apply(lambda x: 
                                                                    datetime.datetime.strptime(x, '%Y-%m-%d'))

marin_simple_per_location['Camera.Deployment.End.Date_datetime'] = marin_simple_per_location['Camera.Deployment.End.Date'].apply(lambda x: 
                                                                    datetime.datetime.strptime(x, '%Y-%m-%d'))

marin_simple_per_location['trapnights'] = (marin_simple_per_location['Camera.Deployment.End.Date_datetime'] 
                                          - 
        marin_simple_per_location['Camera.Deployment.Begin.Date_datetime']).astype(pd.Timedelta).apply(lambda l: l.days)

marin_simple_per_location.head()

#Generate a trap night count for each unique {ProjectId + DeploymentId + Month}

from dateutil.relativedelta import relativedelta

def getDaysInMonth(date):
    '''
    For a specific day d, it will return the number of days between d and the end of the month
    '''
    endDate = date + relativedelta(day=1, months=+1, days=-1)
    return (endDate - date).days + 1

def genCountByMonth(start, end):
    '''
    Given a date range, it will return a dictionary of tuples for (month, trapnight counts)
    '''
    totalcount = (end - start).days
    results = {}
    while totalcount > 0:
        daysInCurrentMonth = min(totalcount,getDaysInMonth(start))
        if(start.month in results.keys()):
            results[str(start.year)+'-'+str(start.month)] += daysInCurrentMonth
        else:
            results[str(start.year)+'-'+str(start.month)] = daysInCurrentMonth
        totalcount -= daysInCurrentMonth
        start += relativedelta(day=1, months=+1)
    return results

print genCountByMonth(marin_simple_per_location['Camera.Deployment.Begin.Date_datetime'][0], marin_simple_per_location['Camera.Deployment.End.Date_datetime'][0])

df = pd.DataFrame(columns=['Project.ID', 'Deployment.Location.ID', 'Month', 'Trapnights'])
y=0
for i in range(0,len(marin_simple_per_location)):
    projectId = marin_simple_per_location['Project.ID'][i]
    locationId = marin_simple_per_location['Deployment.Location.ID'][i]
    data = genCountByMonth(marin_simple_per_location['Camera.Deployment.Begin.Date_datetime'][i], marin_simple_per_location['Camera.Deployment.End.Date_datetime'][i])
    for month,trapnights in data.iteritems():
        df.loc[y] = [projectId, locationId, month, trapnights]
        y+=1

#final output -- trap night count for each unique {ProjectId + DeploymentId + Month}
df = df.groupby(['Project.ID', 'Deployment.Location.ID','Month']).sum().reset_index()
df

df.to_csv('marin_dataset_trapnights.csv')

