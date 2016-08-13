project_dir = '/Users/simonaaksman/Desktop/Git/datadive_201608_ci'
data = '/data/'
rscripts = '/rscripts/'
setwd(paste0(project_dir, rscripts))

vertibrates = read.csv(paste0(project_dir, data, '/processed/'))

