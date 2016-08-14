import csv
import random

# set up sample data
data_source = 'TEAM'
subprojects = [
    ('Yasuni', [
        ('123ABC', -0.6, -76.4),
        ('456DEF', -4.2, -81.5),
    ]),
    ('Pasoh Forest Reserve', [
        ('789LMN', 20.4, -66.6),
        ('000XYZ', 21.6, -65.9),
    ]),
]
species_sets = [
    ('Macaca', 'nemestrina', 50, 100),
    ('Cuniculus', 'paca', 0, 5),
]
samplings = [
    ('annual', 1),
    ('seasonal', 4),
    ('monthly', 12),
]
years = [2013, 2014, 2015]

# set up output file
writer = csv.writer(open('sample_data_to_frontend.csv', 'w'))

# print header
header = [
    'Data Source', 'Project ID',
    'Deployment Location ID', 'Latitude Resolution', 'Longitude Resolution',
    'Sampling Type', 'Sampling Period', 'Year',
    'Genus', 'Species', 'Rate Of Detection',
]
writer.writerow(header)

# loop over variables
for site, locs in subprojects:
    for deploy_id, lat, lon in locs:
        for year in years:
            for sampling_type, n_sampling_periods in samplings:
                for i_sampling_period in range(n_sampling_periods):
                    for genus, species, low_rate, high_rate in species_sets:
                        # set up row
                        row = [
                            data_source, site,
                            deploy_id, lat, lon,
                            sampling_type, i_sampling_period+1, year,
                            genus, species, random.randint(low_rate, high_rate),
                        ]

                        # print row
                        writer.writerow(row)
