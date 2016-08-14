library(dplyr)
library(ggplot2)

createTimeStamp = function(Year, samplingPeriod){
    timeString = paste(Year, samplingPeriod, "01", sep = "-")
    as.Date(timeString, "%Y-%m-%d")
}

cameraID = "123ABC"
groupingID = "Species"
samplingFrequency = "monthly"


selectedData =
    read.csv("data/TEAM_data.csv", stringsAsFactors = FALSE)

## Create time stamp
timeStampData =
    selectedData %>%
    subset(Sampling.Type == samplingFrequency) %>%
    mutate(., timeStamp = createTimeStamp(Year = Year,
                                          samplingPeriod = Sampling.Period))

cameraData =
    timeStampData %>%
    subset(., Deployment.Location.ID == cameraID)


plotts = function(full_data, camera_data, time, group, rate, addSmoother = FALSE,
                  facet = FALSE){
    ## This function plots the camera specific rate of detection
    grouping_formula =
        sapply(c(time, group), . %>% {as.formula(paste0('~', .))})

    averageData =
        full_data %>%
        group_by_(.dots = grouping_formula) %>%
        summarise(., mean_rate = mean(Rate.Of.Detection))

    if(!facet){
        tsplot = ggplot() +
            geom_line(data = camera_data,
                      aes_string(x = time, y = rate, col = group), lwd = 2) +
            geom_line(data = averageData,
                      aes_string(x = time, y = "mean_rate", col = group)) +
            theme(legend.position="top") +
            xlab("") +
            ylab("")
    }else {
    ## This enables facet
        tsplot = ggplot() +
            geom_line(data = camera_data,
                      aes_string(x = time, y = rate), lwd = 2) +
            geom_line(data = averageData,
                      aes_string(x = time, y = "mean_rate")) +
            facet_grid(as.formula(paste0("~", group))) +
            theme(legend.position="top") +
            xlab("") +
            ylab("")
    }
    if(addSmoother){
        tsplot =
            tsplot +
            geom_smooth(data = full_data,
                        aes_string(x = time, y = rate, col = group))
    }
    tsplot
}


## plotts(full_data = timeStampData, camera_data = cameraData,
##        group = groupingID, time = "timeStamp", rate = "Rate.Of.Detection",
##        addSmoother = FALSE)


plotAggTs = function(full_data, time, rate,
                     addSmoother = FALSE){
    ## This function plots the total sum of detection rate
    grouping_formula =
        sapply(c(time), . %>% {as.formula(paste0('~', .))})

    averageData =
        full_data %>%
        group_by_(.dots = grouping_formula) %>%
        summarise(., sum_rate = sum(Rate.Of.Detection))


    tsplot = ggplot() +
        geom_line(data = averageData,
                  aes_string(x = time, y = "sum_rate")) +
        theme(legend.position="top") +
        xlab("") +
        ylab("")

    if(addSmoother){
        tsplot =
            tsplot +
            geom_smooth(data = full_data,
                        aes_string(x = time, y = rate))
    }
    tsplot
}


## plotAggTs(full_data = timeStampData,
##           time = "timeStamp", rate = "Rate.Of.Detection",
##           addSmoother = FALSE)
