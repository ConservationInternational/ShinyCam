library(dplyr)
library(ggplot2)
library(reshape2)

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


plotAggTs = function(full_data, time, rate, addSmoother = FALSE){
    ## This function plots the total sum of detection rate
    grouping_formula =
        sapply(c(time), . %>% {as.formula(paste0('~', .))})

    sumData <<-
        full_data %>%
        group_by_(.dots = grouping_formula) %>%
        summarise(., sum_rate = sum(Rate.Of.Detection))

    tsplot =
        ggplot(data = sumData, aes(x = timeStamp, y = sum_rate)) +
        geom_line() +
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


## plotAggTs(full_data = timeStampData, time = "timeStamp",
##           rate = "Rate.Of.Detection", addSmoother = FALSE)


plotDecomposeTs = function(full_data, time, rate){
    grouping_formula =
        sapply(c(time), . %>% {as.formula(paste0('~', .))})

    sumData =
        full_data %>%
        group_by_(.dots = grouping_formula) %>%
        summarise(., sum_rate = sum(Rate.Of.Detection))

    sum.ts = ts(sumData$sum_rate, freq = 12)
    sum.df = cbind(timeStamp = sumData[[time]],
                   data.frame(stl(sum.ts, s.window = "periodic")[[1]]))
    sum_normalised.df = melt(sum.df, id.var = time)

    decompose_ts =
        ggplot(data = sum_normalised.df,
               aes(x = timeStamp, y = value)) +
        facet_grid(variable ~ .) +
        geom_line() +
        theme(legend.position="top") +
        xlab("") +
        ylab("")

    decompose_ts
}


## plotDecomposeTs(full_data = timeStampData, time = "timeStamp",
##                 rate = "Rate.Of.Detection")
