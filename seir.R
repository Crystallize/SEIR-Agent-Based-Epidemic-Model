# SEIR Agent based model interfaced with NetLogo 5.2.0
# Coded by: Ryan Yan
# This script is meant to be run a machine with NetLogo 5.2.0 installed and runs either 
library(RNetLogo)
library(ggplot2)
library(reshape2)

nlpath <- "C:/Program Files (x86)/NetLogo 5.2.0"
modelpath <- "C:/Program Files (x86)/NetLogo 5.2.0/models/seir model.nlogo"
NLStart(nlpath, gui = TRUE)
NLLoadModel(modelpath)

runModel <- function(tickLimit = FALSE, plot = TRUE) {
    #This function runs the netlogo model - whichever is loaded, either until the stop conditions are
    #met in the model code in netlogo (currently this is when no infected or exposed exist), or until
    #tick number 150, depending on whether "tickLimit" is set to True or False. Data on the population
    #of each infective state (S E I R), will be recorded at each tick. If plot is set to True, then a simple
    #line chart will also be plotted in R. This function returns a numeric vector containing the susceptible
    #population data at each tick.
    
    NLCommand("setup")
    if(tickLimit) {
        popData <- NLDoReportWhile("ticks < 150 ", "goUntil", 
                                   c("count turtles with [ susceptible? ]", "count turtles with [ exposed? ]",
                                     "count turtles with [ infected? ]", "count turtles with [ recovered? ]", "ticks"), 
                                   as.data.frame = TRUE, 
                                   df.col.names = c("Susceptible", "Exposed", "Infected", "Recovered", "Ticks"))
    }
        
    else {
        popData <- NLDoReportWhile("not all? turtles [ not exposed? and not infected? ]", "go", 
                                   c("count turtles with [ susceptible? ]", "count turtles with [ exposed? ]",
                                     "count turtles with [ infected? ]", "count turtles with [ recovered? ]", "ticks"), 
                                   as.data.frame = TRUE, 
                                   df.col.names = c("Susceptible", "Exposed", "Infected", "Recovered", "Ticks"))
    }
        
    if(plot == TRUE) {
        plot(popData$Ticks, popData$Susceptible, type = "n", xlab = "Ticks", ylab = "Susceptible",
             main = "SEIR Populations Over Time", ylim = c(0, 150))
        
        lines(popData$Ticks, popData$Susceptible, col = "green")
        lines(popData$Ticks, popData$Exposed, col = "orange")
        lines(popData$Ticks, popData$Infected, col = "red")
        lines(popData$Ticks, popData$Recovered, col = "grey")
        legend("left", col = c("green", "orange", "red", "grey"), lwd = 1,
               legend = c("Susceptible", "Exposed", "Infected", "Recovered"), cex = 1)
    }
  
    popData$Susceptible
}

plotResults <- function(runs = 2) {
    #This function runs the model "runs" number of times and constructs a long-format data frame 
    #containing the susceptible population data at each tick from 1 to 150 from each run. It then plots
    #that data (tick vs. population) on one graph
    
    #creates labels
    cnames = rep(NA, runs)
    for (i in 1:runs) {
        cnames[i] <- paste("Run", i, sep = " ")
    }
    
    #Create data frame with NA values
    model1_results <- data.frame(x = rep(NA, runs), y = rep(NA, 150))
    
    #Fills data frame with data from runModel runs
    for (i in 1:runs) {
        model1_results[ , i] <- runModel(tickLimit = TRUE, plot = FALSE)
    }
    
    #Restructures to long-format and plots.
    colnames(model1_results) = cnames
    df <- melt(model1_results)
    df <- cbind(df, rep(1:150, runs))
    colnames(df) = c("Series", "Population", "Tick")
    plot <- ggplot(df, aes(Tick, Population, col = Series))
    plot <- plot + geom_smooth() + ggtitle("Susceptible Population Over Multiple Runs")
    plot <- plot + coord_cartesian(ylim = c(0, 175)) + theme(text = element_text(size = 15))
    plot
}


