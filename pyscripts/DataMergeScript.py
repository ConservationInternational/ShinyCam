import pandas as pd
import numpy as np

if __name__ == '__main__':
	cameras = pd.read_csv('../data/ChedaJewel-Cheda-and-Jewel-GGNRA-Cameras.csv')
	deployments = pd.read_csv('../data/ChedaJewel-Cheda-and-Jewel-GGNRA-Deployments.csv')
	images = pd.read_csv('../data/ChedaJewel-Cheda-and-Jewel-GGNRA-Images.csv')
	team = pd.read_csv('../data/Terrestrial_Vertebrate.csv')


	merged = pd.merge(deployments, cameras, on='Camera ID', how='left')
	merged = pd.merge(merged, images, on='Deployment ID', how='left')
	merged['Genus Species'].fillna('unknown unknown', inplace=True)
	merged['Genus'] = merged['Genus Species'].apply(lambda x: x.split()[0])
	merged['Species'] = merged['Genus Species'].apply(lambda x: x.split()[1])
	del merged['Genus Species']

	merged_cols = ['Project ID_x', 'Deployment Location ID', 'Image ID', 'Location', 'Photo Type', 
	               'Photo Type Identified By', 'Genus', 'Species', 'Count', 'Latitude Resolution', 
	               'Longitude Resolution', 'Camera Deployment Begin Date', 
	               'Camera Deployment End Date', 'Camera ID']
	colnames_team = ['Site Name', 'Sampling Unit Name', 'Raw Name', 'Photo ID URL', 'Photo Type',
	                'Person Identifying the Photo', 'Genus', 'Species', 'Number of Animals', 
	                'Latitude', 'Longitude', 'Camera Start Date and Time', 
	                'Camera End Date and Time', 'Camera Serial Number'] 

	team['Camera Start Date and Time'] = pd.to_datetime(team['Camera Start Date and Time'])
	team['Camera End Date and Time'] = pd.to_datetime(team['Camera End Date and Time'])
	team[colnames_team].columns = merged_cols

	team[colnames_team].to_csv('Terrestrial_Vertebrate_Cols_Edited.csv', index=False)

	merged.columns = [u'Project ID', u'Deployment ID', u'Event', u'Array Name',
	       u'Deployment Location ID', u'Longitude Resolution',
	       u'Latitude Resolution', u'Camera Deployment Begin Date',
	       u'Camera Deployment End Date', u'Bait Type', u'Bait Description',
	       u'Feature Type', u'Feature Type methodology', u'Camera ID',
	       u'Quiet Period Setting', u'Restriction on access',
	       u'Camera Failure Details', u'Project ID_y', u'Make', u'Model',
	       u'Serial Number', u'Year Purchased', u'Project ID', u'Image ID',
	       u'Location', u'Photo Type', u'Photo Type Identified by', u'Uncertainty',
	       u'IUCN Identification Number', u'Date_Time Captured', u'Age', u'Sex',
	       u'Individual ID', u'Count', u'Animal recognizable (Y/N)',
	       u'individual Animal notes', u'Genus', u'Species']
	del merged['Project ID_y']
	merged['Camera Deployment Begin Date'] = pd.to_datetime(merged['Camera Deployment Begin Date'])
	merged['Camera Deployment End Date'] = pd.to_datetime(merged['Camera Deployment End Date'])
	merged['Date_Time Captured'] = pd.to_datetime(merged['Date_Time Captured'])
	merged.to_csv('Merged.csv', index=False)