# Rates of Detection app

## Goal
Show "rates of detection" for camera traps on a map, showing additional graphs and data where appropriate. Give user control over:
- Data source
- Species (and Guild/Conservation Status)
- Time 

## Needs
In order to run, this app needs the following in the `data` directory:
- a csv called `rate_of_detection` with the following column names:

```"Sampling.Period", "Site.Name", "Sampling.Unit.Name", "Species", "Number.of.Animals", "Year", "Project.ID"             "Deployment.Location.ID", "Latitude", "Longitude", "Genus", "Sampling.Type", "Data.Source", "Rate.Of.Detection"```

- a csv called `taxonomy_scientific_name_20160813.csv` mapping species to guild, and
- a csv called `taxonomy_red_list_status_20160813.csv` mapping species to their conservation status.

## To run
To run this app from RStudio, open server.R or ui.R and click 'Run App'. 

To run from the command line, do ```R -e "shiny::runApp('~/path/to/this/directory')"```
