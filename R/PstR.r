#' @import dplyr
library(dplyr)

#' @import tidyverse
library(tidyverse)

#' @import stR
library(stR)

#' @import tsibble
library(tsibble)

#' @import distributional
library(distributional)

#' @import httpgd
library(httpgd)

# see which packages i really need


setClass(
    "Model", 
    slots=list(
        data = "data.frame" , 
        predictors = "list"
        ),
    prototype = list(
        data = data.frame(),
        predictors = list()
  )
    )


#' Inizialize the wrapper object for stR
#'
#' @param .Object is the reference of the object in the method
#'
#' @return The updated object.
#' @export
setGeneric("initialize",function(.Object,...){
    standardGeneric("initialize")
})
setMethod("initialize", "Model", function(.Object, data = data.frame(), predictors = list()) {
  .Object@data <- data
  .Object@predictors <- predictors
  return(.Object)
})


#' Load the dataframe from a specific location
#'
#' @param .Object is the reference of the object in the method
#' @param pathDF contains the path to the dataframe
#'
#' @return The new str object with the dataframe updated
#' @export
setGeneric("loadDF",function(.Object,pathDF){
    standardGeneric("loadDF")
})
setMethod("loadDF","Model",function(.Object,pathDF) {
    df <- read.csv(pathDF)
    .Object@data = df
    return(.Object)
})


#' Get the dataframe saved in the model
#'
#' @param .Object is the reference of the object in the method
#'
#' @return The dataframe 
#' @export
setGeneric("getDF",function(.Object){
    standardGeneric("getDF")
})
setMethod("getDF","Model",function(.Object) {
    return(.Object@data)
})



#' Set the new dataframe in the object
#'
#' @param .Object is the reference of the object in the method
#' @param newDF the path to a new dataframe.
#'
#' @return The stR object with the new dataframe.
#' @export
setGeneric("updateDF",function(.Object,newDF){
    standardGeneric("updateDF")
})
setMethod("updateDF","Model",function(.Object,newDF){
    .Object@data = newDF
    return(.Object)
})



#' Make the left join
#'
#' @param .Object is the reference of the object in the method
#' @param pathFile specific the path that contains a dataframe to be joined
#' @param nameColumn specific the name of the equal name of column to do the left join
#'
#' @return The new stR wrapper with the dataframe updated 
#' @export
setGeneric("leftJ",function(.Object,pathFile,nameColumn){
    standardGeneric("leftJ")
})
# da sperimentare
setMethod("leftJ","Model",function(.Object,pathFile,nameColumn){
    .Object$getDF = .Object$getDF %>%
    left_join(
      read_csv(pathFile) %>%
      mutate(
              DateTime= as.POSIXct(DateTime),       
              Data = TRUE
        ),
      by = c("DateTime")
    )%>%
    replace_na(list(Data = FALSE)
    )%>%
    mutate_at(
            vars(Data), 
            list(
                new_col = nameColumn)
    )
})


#' Add the predictor trend in the wrapper
#'
#' @param .Object is the reference of the object in the method
#'
#' @return Return the updated object.
#' @export
setGeneric("addPredictorTrend",function(.Object,name,lambdas,len){
    standardGeneric("addPredictorTrend")
})
setMethod("addPredictorTrend", "Model",function(.Object,name,lambdas,len) {
    Predictor <- list(
        name = name,
        data = rep(1, NROW(.Object@data)),
        times = .Object@data$Time,
        seasons = rep(1, NROW(.Object@data)),
        timeKnots = seq(from = first(.Object@data$Time), to = last(.Object@data$Time), length.out = len), # time knots have (time,season)
        seasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0))),
        lambdas = lambdas
    )
    .Object@predictors[[length(.Object@predictors)+1]] = Predictor
    return(.Object)
})

#' Add a predictor for a covariate
#'
#' @param .Object is the reference of the object in the method
#'
#' @return Return the updated object.
#' @export
setGeneric("addPredictorCovariate",function(.Object,name,lambdas,len,nameCovariate){
    standardGeneric("addPredictorCovariate")
})
setMethod("addPredictorCovariate", "Model",function(.Object,name,lambdas,len,nameCovariate) {
    Predictor <- list(
        name = name,
        data = rep(1, NROW(.Object@data[[nameCovariate]])), 
        times = .Object@data$Time,
         seasons = rep(1, NROW(.Object@data)),
        timeKnots = seq(from = first(.Object@data$Time), to = last(.Object@data$Time), length.out = len), # time knots have (time,season)
        seasonalStructure = list(segments = list(c(0, 1)), sKnots = list(c(1, 0))),
        lambdas = lambdas
    )
    .Object@predictors[[length(.Object@predictors)+1]] = Predictor
    return(.Object)

})


#' Add a predicttor for a seasonality
#'
#' @param .Object is the reference of the object in the method
#' @param split What to split on.
#'
#' @return Return the updated object.
#' @export
setGeneric("addPredictorSeasonality",function(.Object,name,nameSeasonality,segments,sKnots,lambdas,len){
    standardGeneric("addPredictorSeasonality")
})
setMethod("addPredictorSeasonality","Model",function(.Object,name,nameSeasonality,segments,sKnots,lambdas,len) {
    Predictor <- list(
        name = name,
        data = rep(1, NROW(.Object@data)),
        times = .Object@data$Time,
        seasons = .Object@data[[nameSeasonality]], # capire il nome della stagionalità come inserirlo in maniera randomica nameSeasonality
        timeKnots = seq(from = first(.Object@data$Time), to = last(.Object@data$Time), length.out = len),
        seasonalStructure = list(
            segments = segments, 
            sKnots = sKnots
        ),
        lambdas = lambdas
    )
    #' gapCV = 24*7, nel nostro caso una settimana
    #' nFold = 5
    #' confidence = 0.95, per le prove non conviene settare l'intervallo di confidenza
    .Object@predictors[[length(.Object@predictors)+1]] = Predictor
    return(.Object)

})




#' Save the model in the RDS format
#'
#' @param .Object is the reference of the object in the method
#' @param name the name of the model saved
#'
#' @return Nothing
#' @export
setGeneric("saveModel",function(model.str,name){
    standardGeneric("saveModel")
})
setMethod("saveModel","Model",function(model.str,name) {
    home = "/home/vitoc/allData/Rmodels/" # starting path defined somewhere
    saveRDS(model.str, paste0(home,name,".rds")) # path to save
})


#' Return the model from the wrapper
#'
#' @param .Object is the reference of the object in the method
#'
#' @return The str model.
#' @export
setGeneric("getModel",function(name){
    standardGeneric("getModel")
})
setMethod("getModel","Model",function(name = ""){
    # nome definito comme OxygenStr
    home = "/home/vitoc/allData/Rmodels/"
    model.str <- readRDS(paste0(home,"OxygenStr",".rds")) # path to save
    return(model.str)
})

#' Print and store the plot of beta parameters
#'
#' @param .Object is the reference of the object in the method
#'
#' @return Nothing.
#' @export
setGeneric("betaPlots",function(.Object){
    standardGeneric("betaPlots")
})
# If a predictor is a covariate, se segments and sKnots are the same
setMethod("betaPlots","Model",function(.Object) {
    model.str = .Object$getModel()
    # scegliere la directory dove fare i plot
    png("Plot.png")
    plot(model.str, xTime = .Object@data$Date)
    dev.off()
    for(i in 1:length(model.str$output$predictors)){
        png(paste0("ComponentBeta",i,".png"))
        set.seed(i)
        plotBeta(model.str, predictorN = i)
        dev.off()
    }

})

#' Train the model
#'
#' @param .Object is the reference of the object in the method
#'
#' @return The trained model.
#' @export
setGeneric("train",function(.Object,gapCV,nFold,value,trace){
    standardGeneric("train")
})
setMethod("train","Model",function(.Object,gapCV,nFold,value,trace) { # value contains the name of the signal
    print("I'm training the model")
    #print(.Object@data[[value]])
    model.str <- STR(
        data = .Object@data[[value]],
        predictors = .Object@predictors,
        gapCV = gapCV, # skippo ipoteticamente la lunghezza di una settimana
        nFold = nFold, # lenght of field for cross validation
        trace = trace
        #confidence = 0.95
        #solver = c("Matrix", "qr")
  )
  saveModel(model.str,"OxygenStr")
  print("Model saved")
})



#' The function that permit to exchange the data from R to Python
#'
#' @param .Object is the reference of the object in the method
#' @param split What to split on.
#'
#' @return A dataframe of the components of the model.
#' @export
setGeneric("exportPython",function(.Object){
    standardGeneric("exportPython")
})
setMethod("exportPython","Model",function(.Object){
    model.str = .Object$getModel()
    dfAppo = .Object@data
    for(i in seq_along(model.str$output$predictors)){
        dfAppo <- dfAppo %>%
        as_tsibble(index = DateTime,key=Time) %>% 
        mutate(
            Data = model.str$output$predictors[[i]]$data
        )%>%
        mutate_at(
            vars(Data), 
            list(
                new_col =.Object@predictors[i]$name)
        )

    }
    dfAppo <- dfAppo %>%
    as_tsibble(index = DateTime,key=Time) %>% 
    mutate(
        Remainder = model.str$output$random$data,
        FF = model.str$output$forecast$data,
    )

    return(dfAppo)
})
