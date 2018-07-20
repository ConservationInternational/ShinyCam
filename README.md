# ShinyCam Project

#####  See the [project wiki](https://github.com/ConservationInternational/ShinyCam/wiki) for additional information regarding project background and how to contribute. We will post a live example on a server shortly. <!--Please look [here](http://analytics.teamnetwork.org/efegraus/ShinyCam/ShinyApps/LeafletApp/) for version of the ShinyCam app currently in production.-->

## Goals
The goal of ShinyCam is to have a basic analytics package that enables users with camera trap data to easily see both operational and scientificly based analytics. We are using the Rshiny framework for a number of reasons because it is written in R which is beocoming more utilized in the ecological and conservation communities. We want this to be user driven and answer the problems most pressing to folks doing camera trapping. We will also use this as an exploratory tool as we build cloud-based analytics that will be available Wildlife Insights. 

### Scope
We are building this applicaiton to work at the level of a camera trap project. A project may be 5 cameras run over a summer for a master thesis or it could be hundreds of cameras put out in a particular season over many years in a protected area.  Because Rshiny will run locally there are machine limitiations to what is possible.

### Server-side
Rshiny and ShinyCam can run on a server. If you are interested in doing this please let us know. We generally feel at this tmie (summer 2018) we need to build out the software some more, fix some bugs and make things more effiicent. 

How can Conservation International better respond to and implement solutions for new data analytic requests from our partners in the wildlife monitoring community?

![alt tag](https://github.com/ConservationInternational/ShinyCam/blob/master/shinycam.gif)

## Code
### Directory Structure
Here is the structure for our repo. Note that the `data` folder will not exist on Github - please match this directory structure on your machine and populate `data/original` with the raw data:

```.
├── ShinCam
|   |-- Archive
|   |--rscripts
|   |  |- DataSourceTransformScripts
|   ├── ShinyApps
|   | ├── LeafletApp
|   |   ├── data - either the folder containing data, or symlink /path/to/data
|   |    |- data
|   |    |- processed
|   |    |- raw_dataprep
|   |    |- Shapefiles
|   |   ├── server.R
|   |   └── ui.R
|   ├── scripts
|   
└──
```

**Installation and Data File Processing:** Data processing and calculation of relevant metrics is carried out using scripts found in the `rscripts` folder. Such calculations were previously performed using Python scripts in the `pyscripts` folder, but these scripts will soon be deprecated. 

**Application Frontend:** The user interface of the app is created using R and the Shiny web framework - corresponding scripts for which are found in `LeafletApp` folder. See [README](https://github.com/ConservationInternational/ShinyCam/tree/master/ShinyApps/LeafletApp) file in `LeafletApp` folder for more information.

R Setup
-----
- Make sure you have `R` installed.  
- Clone this repo: `git clone https://github.com/ConservationInternational/ShinyCam.git`
- Populate the data folder as in [Google drive](https://drive.google.com/folderview?id=0BzoemeOsgjRIb2R1ZWo5YjBCRHc&usp=sharing).
- Enter and run the following in your R console to install packages neccessary to run app:

```
install.packages("shiny")
install.packages("raster")
install.packages("leaflet")
install.packages("RColorBrewer")
install.packages("scales")
install.packages("lattice")
install.packages("dplyr")
install.packages("gstat")
install.packages("sp")
install.packages("intervals")
install.packages("data.table")
install.packages("KernSmooth")
install.packages("rgdal")
```

To run this app from RStudio, open server.R or ui.R and click 'Run App'.

To run from the command line, do R -e "shiny::runApp('~/path/to/this/directory')"
