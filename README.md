# Goals

### Burning Question:

How can Conservation International better respond to and implement solutions for new data analytic requests from our partners in the wildlife monitoring community?

### Background:

The Tropical Ecology Assessment and Monitoring (TEAM) Network is the first and largest global-scale conservation observation network on the planet —essentially an early warning system for life on earth. Conservation International launched TEAM in 2002 as part of an innovative partnership with the Smithsonian Institution, Wildlife Conservation Society, and more than 80 local, academic and government partners. It is now one of the most robust datasets on the health of tropical forests in the world, with more than 1,000 camera traps capturing almost 3 million images, ~8 million weather observations and 70,000 forest trees measurements. These measurements come from 17 protected areas (TEAM Sites) that serve as our data collection and monitoring hubs. Please look [here](https://vimeo.com/93627505) for a video introduction to TEAM.

We have developed standard wildlife monitoring protocols and flexible data management systems that enable our growing network of global partners to organize and share camera trap images (meta data). To process the data, including assigning species names, we have also developed software to expedite the work for our scientists. This includes data management, data processing and analysis. Our flagship analytical product, the Wildlife Picture Index, uses TEAM data to monitor the health and status of over 500 species populations around the world (using the +2.6 million camera trap images plus 8 million climate measurements). The Wildlife Picture Index is our only analytical tool right now. It addresses a critical conservation question by calculating “occupancy” (i.e. how many animals of a given species are in a given area) and trends over time (i.e. is the population decreasing, increasing or stable).

# Code
## Directory Structure
Here is the structure for our repo:
```.
├── team_ci
|   ├── data
|   | ├── code
|   | |   └── code for processing data
|   | ├── processed
|   | └── original
|   ├── ShinyApps
|   | ├── ExampleShinyApp
|   |   ├── data - symlink /path/to/data
|   |   ├── server.R
|   |   └── ui.R
|   ├── rscripts
|   ├── pyscripts
|   └── notebooks
└──
```


### ExampleShinyApp
This is a small example app/intro to Shiny in R, using our data. Given a user-selected camera serial number, we show a bar chart of the animals that this camera has seen. Note that this is not one of the items as requested by CI; rather, it serves as an intro to our data and to Shiny.

To use this app, please populate the /data folder as it is in Google drive and create symlinks as in the above structure. Directions to run the app can be found [here](http://shiny.rstudio.com/articles/running.html). Please freel free to contact [Erin](mailto:erin.g.craig@gmail.com) with questions.

Python Setup (OS X and Linux ONLY)
-----
- First make sure you have ```python, pip``` and ```virtualenv``` installed
- ```git clone git@github.com:DataKind-SF/team_ci.git```
- ```make python```
- To start jupyter notebook run ```make notebook```

R Setup
-----
- TODO
