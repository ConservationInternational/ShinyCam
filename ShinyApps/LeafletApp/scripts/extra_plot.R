library(dplyr)
library(ggplot2)
library(reshape2)

## createTimeStamp = function(Year, samplingPeriod){
##     timeString = paste(Year, samplingPeriod, "01", sep = "-")
##     as.Date(timeString, "%Y-%m-%d")
## }


createTimeStamp = function(samplingPeriod){
    timeString = paste(samplingPeriod, "01", sep = "-")
    as.Date(timeString, "%Y-%m-%d")
}


cameraID = "CT-VB-1-1"
groupingID = "Species"
samplingFrequency = "Annual"


selectedData =
    read.csv("data/rate_of_detection.csv", stringsAsFactors = FALSE)

## HACK (Michael): Cleaning the data
selectedData =
    selectedData %>%
    subset(., Rate.Of.Detection >= 0 & Rate.Of.Detection < Inf) %>%
    subset(., Genus %in% head(unique(.$Genus)))

## Create time stamp
timeStampData =
    selectedData %>%
    subset(Sampling.Type == samplingFrequency) %>%
    mutate(., timeStamp = createTimeStamp(samplingPeriod = Sampling.Period))

cameraData =
    timeStampData %>%
    subset(., Deployment.Location.ID == cameraID)


plotCameraBenchmark = function(full_data,
                               camera_data,
                               time,
                               group,
                               rate,
                               addSmoother = FALSE,
                               facet = FALSE){

    ## This function plots the camera specific rate of detection
    grouping_formula =
        sapply(c(time, group), . %>% {as.formula(paste0('~', .))})

    averageData =
        full_data %>%
        subset(., .[[group]] %in% unique(camera_data[[group]])) %>%
        group_by_(.dots = grouping_formula) %>%
        summarise(., mean_rate = mean(Rate.Of.Detection))

    dotdash = "dotdash"
    solid = "solid"

    if(!facet){
        tsplot = ggplot() +
            geom_line(data = camera_data,
                      aes_string(x = time, y = rate, col = group,
                                 linetype = dotdash)) +
            geom_line(data = averageData,
                      aes_string(x = time, y = "mean_rate", col = group,
                                 linetype = solid)) +
            theme(legend.position="top") +
            scale_linetype_manual(name = "Line type",
                                  values = c("dotdash" = "dotdash",
                                             "solid" = "solid"),
                                  labels = c("Camera", "Overall")) +
            xlab("") +
            ylab("")
    }else {
        ## This enables facet
        tsplot =
            ggplot() +
            geom_line(data = camera_data,
                      aes_string(x = time, y = rate,
                                 linetype = dotdash)) +
            geom_line(data = averageData,
                      aes_string(x = time, y = "mean_rate",
                                 linetype = solid)) +
            facet_grid(as.formula(paste0(group, "~ .")), scales = "free") +
            theme(legend.position="top") +
            scale_linetype_manual(name = "Line type",
                                  values = c("dotdash" = "dotdash",
                                             "solid" = "solid"),
                                  labels = c("camera","average")) +
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

## ## This plot should be plotted only if the number of class is less than 5.
## plotCameraBenchmark(full_data = timeStampData, camera_data = cameraData,
##                     group = groupingID, time = "timeStamp",
##                     rate = "Rate.Of.Detection", addSmoother = FALSE,
##                     facet = TRUE)


plotTotalTs = function(full_data, time, rate, addSmoother = FALSE, aggFUN = sum){
    ## This function plots the total sum of detection rate
    grouping_formula =
        sapply(c(time), . %>% {as.formula(paste0('~', .))})

    sumData <<-
        full_data %>%
        group_by_(.dots = grouping_formula) %>%
        summarise(., agg_rate = aggFUN(Rate.Of.Detection))

    tsplot =
        ggplot(data = sumData, aes(x = timestamp, y = agg_rate)) +
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

## ## Seems to make no sense of plotting the aggregated count, as it increases with
## ## the number of project.
## plotTotalTs(full_data = timeStampData, time = "timeStamp",
##             rate = "Rate.Of.Detection", addSmoother = FALSE,
##             aggFUN = mean)


## Need to account for data which only has annual data, this should depends on
## the sampling frequency filter.
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


## plotDecomposeTs(full_data = timeStampData,
##                 time = "timeStamp",
##                 rate = "Rate.Of.Detection")


## Top species at camera and top species overall.

groupTopFive = function(data, group, rate){
    grouping_formula = as.formula(paste0("~", group))

    ## Select the top 5 by detection rate
    topFiveData =
        data %>%
        group_by_(.dots = grouping_formula) %>%
        summarise(avg = mean(Rate.Of.Detection)) %>%
        arrange(., desc(avg)) %>%
        head(., 5)

    ## This is to sort the graph in order
    topFiveData[[group]] =
        factor(topFiveData[[group]], levels = rev(topFiveData[[group]]))

    ## Plot the data
    top5Plot =
        ggplot(data = topFiveData,
               aes_string(x = group, y = "avg")) +
        geom_bar(stat = "identity") +
        coord_flip()

    top5Plot
}

## This plot can be performed on full data, or camera data
## groupTopFive(selectedData, "Genus", "Rate.Of.Detection")




health_timeseries = function(data, group, rate, year){

    splittedData = split(data, data[[group]])


    appendCoef = function(formula, data){
        coefs = coef(lm(formula, data))
        data$intercept = coefs[1]
        data$trend = coefs[2]
        data$col = with(data, ifelse(trend >= 0, "green", "red"))
        data
    }
    form = as.formula(paste0(rate, " ~ ", year))

    estimatedData = lapply(splittedData, FUN = appendCoef, formula = form)
    print(str(estimatedData))
    combinedData = unsplit(estimatedData, data[[group]])

    health_ts =
        ggplot(data = combinedData,
               aes_string(x = year, y = rate, group = group)) +
        geom_line(stat = "smooth", method = "lm",
                  aes(color = col), alpha = 0.2, lwd = 2,
                  se = FALSE) +
        scale_colour_manual("Group Trend",
                            values = c("red" = "red", "green" = "green"),
                            labels = c("Decreasing","Increasing")) +
        scale_y_log10() +
        xlab("") +
        ylab("")
    health_ts
}

## health_timeseries(timeStampData, "Species", "Rate.Of.Detection", "Year")
## health_timeseries(timeStampData, "Genus", "Rate.Of.Detection", "Year")

