# Conservation International ShinyCam Project

##### Please look [here](http://analytics.teamnetwork.org/efegraus/ShinyCam/ShinyApps/LeafletApp/) for version of the ShinyCam app currently in production. 

## Goals

### Burning Question:

How can Conservation International better respond to and implement solutions for new data analytic requests from our partners in the wildlife monitoring community?

### Background:

The Tropical Ecology Assessment and Monitoring (TEAM) Network is the first and largest global-scale conservation observation network on the planet —essentially an early warning system for life on earth. Conservation International launched TEAM in 2002 as part of an innovative partnership with the Smithsonian Institution, Wildlife Conservation Society, and more than 80 local, academic and government partners. It is now one of the most robust datasets on the health of tropical forests in the world, with more than 1,000 camera traps capturing almost 3 million images, ~8 million weather observations and 70,000 forest trees measurements. These measurements come from 17 protected areas (TEAM Sites) that serve as our data collection and monitoring hubs. Please look [here](https://vimeo.com/93627505) for a video introduction to TEAM.

We have developed standard wildlife monitoring protocols and flexible data management systems that enable our growing network of global partners to organize and share camera trap images (meta data). To process the data, including assigning species names, we have also developed software to expedite the work for our scientists. This includes data management, data processing and analysis. Our flagship analytical product, the Wildlife Picture Index, uses TEAM data to monitor the health and status of over 500 species populations around the world (using the +2.6 million camera trap images plus 8 million climate measurements). The Wildlife Picture Index is our only analytical tool right now. It addresses a critical conservation question by calculating “occupancy” (i.e. how many animals of a given species are in a given area) and trends over time (i.e. is the population decreasing, increasing or stable).

## Code
### Directory Structure
Here is the structure for our repo. Note that the `data` folder will not exist on Github - please match this directory structure on your machine and populate `data/original` with the raw data:

```.
├── team_ci
|   ├── data
|   | ├── processed
|   | ├── intermediate
|   | └── original
|   ├── ShinyApps
|   | ├── LeafletApp
|   |   ├── data - either the folder containing data, or symlink /path/to/data
|   |   ├── server.R
|   |   └── ui.R
|   ├── rscripts
|   ├── pyscripts
|   └── notebooks
└──
```

Python Setup (OS X and Linux ONLY)
-----
- First make sure you have ```python, pip``` and ```virtualenv``` installed
- ```git clone git@github.com:DataKind-SF/datadive_201608_ci.git```
- Populate your data folder as in [Google drive](https://drive.google.com/folderview?id=0BzoemeOsgjRIb2R1ZWo5YjBCRHc&usp=sharing).
- ```make python```
- To start jupyter notebook run ```make notebook```
- Python scripts are documented in `pyscripts/README.md`

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

Shiny app Setup
------------
See the documentation in `ShinyApps/LeafletApp/README.md`.
