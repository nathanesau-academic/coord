# Title: Coordination in R
# Author: Nathan Esau
# Last updated: July 24, 2016

# Datasets can potentially be very large, and passing them as arguments (copied) into 
# a function is very memory intensive - to get around this issue, an environment is used

#' @title Survey data environment
#' @description Project variables are stored are retrieved from this environment, such as the project directory
#' @details To see variables assigned by default use ls(envir=SurveyData)
#' @keywords internal
#' @export
SurveyData <- new.env()

#' @title Assign to SurveyData environment
#' @description Assigns or overrides an existing variable in the SurveyData environment
#' @param ObjectName The variable name to assign
#' @param ObjectValue The value to assign to the variable
#' @param DeparseObjectName true if ObjectName is a string and deparse needs to be called, false otherwise
#' @details By default, the value NA is assigned when unspecified
#' @keywords internal
#' @export
SurveyData.Assign <- function(ObjectName, ObjectValue = NA, DeparseObjectName = FALSE) 
{
  assign(x = ifelse(DeparseObjectName, ObjectName, deparse(substitute(ObjectName))), 
         value = ObjectValue, 
         envir = SurveyData)
}

#' @title Get from SurveyData environment
#' @description Gets the value of a variable defined in the SurveyData environment
#' @param ObjectName The variable name to get the value of
#' @param DeparseObjectName true if ObjectName is a string and deparse needs to be called, false otherwise
#' @details The ObjectName argument should be an object name, not a string
#' @keywords internal
#' @export
SurveyData.Get <- function(ObjectName, DeparseObjectName = FALSE)
{
  get(x = ifelse(DeparseObjectName, ObjectName, deparse(substitute(ObjectName))), 
      envir = SurveyData)
}

#' @title Calculate the cumulative burden
#' @description Calculates the cumulative burden for all units 
#' @param Burdens A list containing vectors of burdens for all units from each previous survey
#' @examples RunSimulation (FrameNames = rep(c("SampleFrame1","SampleFrame2"), 5),
#'                          DesignNames = rep(c("SampleDesign1", "SampleDesign2"), 5),
#'                          DatabaseNames = rep(c("SampleDatabase1", "SampleDatabase2"), 5),
#'                          BurdenFactors = rep(c(1, 5, 4, 3, 7), 2),
#'                          CoordinationMethods = c("Microstrata", "GlobalNegative", "CottonHesse", "None"))
#' lapply(SurveyData.Get (CumulativeBurden), CalculateCumulativeBurden)
#' @export
CalculateCumulativeBurden <- function(Burdens) 
{ 
  Reduce("+", Burdens)
}

#' @title Create a survey data frame
#' @description Builds a survey data frame containing the StrataID, RandomNumbers, Burdens 
#'              and UnitId vectors in the SurveyData environment
#' @param SurveyNumber The survey which information should be retrieved from
#' @keywords internal
#' @export
CreateSurveyFrame <- function(SurveyNumber) 
{
  data.frame(UnitID = SurveyData.Get(UnitID),
             StrataID = SurveyData.Get(StrataID)[[SurveyNumber]],
             RandomNumbers = SurveyData.Get(RandomNumbers)[[SurveyNumber]],
             Burdens = SurveyData.Get(Burdens)[[SurveyNumber]])
}

#' @title Update the survey lists
#' @description Update the RandomNumbers and Burdens in the SurveyData environment
#' @param Frame The frame which contains the RandomNumbers and Burdens vectors
#' @param SurveyNumber The survey for which the RandomNumbers and Burdens lists in the 
#'        SurveyData environment should be updated (usually equal to the current survey number)
#' @param UpdateRandomNumbers true if RandomNumbers should be updated, false otherwise
#' @keywords internal
#' @export
UpdateSurveyLists <- function(Frame, SurveyNumber = 1, UpdateRandomNumbers = TRUE) 
{
  RandomNumbers <- SurveyData.Get(RandomNumbers)
  Burdens <- SurveyData.Get(Burdens)
  
  if(UpdateRandomNumbers)
  {
    RandomNumbers[[SurveyNumber]] <- Frame$RandomNumbers
  }
  
  Burdens[[SurveyNumber]] <- Frame$Burdens
  
  SurveyData.Assign(RandomNumbers,RandomNumbers)
  SurveyData.Assign(Burdens,Burdens)
}

#' @title Gets the current survey sequence
#' @description The survey sequence will be different depending on the coordination CoordinationMethod
#' @param SurveyNumber The current survey which is being run
#' @param Coordination method Either "CottonHesse", "GlobalNegative" or "Microstrata".
#'        If something else is provided, such as "NoCoordination", no coordination is used
#' @examples GetSurveySequence (5, "GlobalNegative")
#' @export
GetSurveySequence <- function(SurveyNumber, CoordinationMethod) 
{
  SurveySequence <- 0 # no coordination
  
  if(CoordinationMethod=="CottonHesse")
  {
    SurveySequence <- SurveyNumber - 1
  }
  else if(CoordinationMethod=="GlobalNegative")
  {
    if(SurveyNumber > 1)
    {
      SurveySequence <- seq(SurveyNumber - 1, 1, -1)
    }
  }
  else if(CoordinationMethod=="Microstrata")
  {
    SurveySequence <- 1
  }
  
  SurveySequence
}

#' @title Run a survey with sample coordination
#' @description Coordinates a survey with previously RunSurvey surveys and selects units for the sample
#' @param FrameName The name of the R data frame containing the survey frame
#' @param DesignName The name of the R data frame containing the survey design
#' @param DatabaseName The name of the R data frame containing the survey database
#' @param SurveyNumber The current survey number
#' @param BurdenFactor The burden associated with the current survey
#' @param CoordinationMethod The coordination method to use (valid options are "GlobalNegative" and "CottonHesse")
#' @details Input: FrameName, DesignName and DatabaseName objects in SurveyData environment;
#'          Ouput: Strata1d, UnitID, RandomNumbers, Burdens, StrataSizes, NumberUnitsSelected,
#'                 Burdens in SurveyData environment
#' @examples RunSurvey ()
#' SurveyData.Get (Burdens)
#' @export 
RunSurvey <- function(FrameName = "SampleFrame1", DesignName = "SampleDesign1", 
                      DatabaseName = "SampleDatabase1", SurveyNumber = 1, BurdenFactor = 1, 
                      CoordinationMethod = "CottonHesse")
{
  # Get variables from SurveyData environment
  CurrentFrame <- SurveyData.Get(FrameName, DeparseObjectName = TRUE)
  CurrentDesign <- SurveyData.Get(DesignName, DeparseObjectName = TRUE)
  CurrentDatabase <- SurveyData.Get(DatabaseName, DeparseObjectName = TRUE)
  
  # Modify column names in case they aren't setup correctly
  colnames(CurrentFrame) <- c("UnitID", "StrataID")
  colnames(CurrentDesign) <- c("StrataID", "NumberUnitsSelected")
  colnames(CurrentDatabase) <- c("UnitID", "RandomNumbers") 
  
  # Update variables in SurveyData environment
  NumberUnits <- length(CurrentFrame$UnitID)
  
  StrataID <- list()
  RandomNumbers <- list()
  Burdens <- list()
  StrataSizes <- list()
  NumberUnitsSelected <- list ()
  
  if (SurveyNumber == 1)
  {
    StrataID <- list(CurrentFrame$StrataID)
    RandomNumbers <- list(CurrentDatabase$RandomNumbers)
    Burdens <- list(rep(0, NumberUnits))
    StrataSizes <- list(as.numeric(table(StrataID[[1]])))
    NumberUnitsSelected <- list(pmin(CurrentDesign$NumberUnitsSelected, StrataSizes[[1]]))
  }
  else # SurveyNumber > 1
  {
    StrataID <- SurveyData.Get(StrataID)
    RandomNumbers <- SurveyData.Get(RandomNumbers)
    Burdens <- SurveyData.Get(Burdens)
    StrataSizes <- SurveyData.Get(StrataSizes)
    NumberUnitsSelected <- SurveyData.Get(NumberUnitsSelected)
  }
  
  # UnitID is assumed to be the same in all frames
  UnitID <- CurrentFrame$UnitID
  
  if (SurveyNumber > 1)
  {
    StrataID[[SurveyNumber]] <- CurrentFrame$StrataID
    RandomNumbers[[SurveyNumber]] <- RandomNumbers[[SurveyNumber - 1]]
    Burdens[[SurveyNumber]] <- rep(0, NumberUnits)
    StrataSizes[[SurveyNumber]] <- as.numeric(table(StrataID[[SurveyNumber]]))
    NumberUnitsSelected[[SurveyNumber]] <- pmin(CurrentDesign$NumberUnitsSelected, 
                                                StrataSizes[[SurveyNumber]])
  }
  else
  {
    SurveyData.Assign (UnitID, UnitID)
  }
  
  SurveyData.Assign(StrataID, StrataID)
  SurveyData.Assign(RandomNumbers, RandomNumbers)
  SurveyData.Assign(Burdens, Burdens)
  SurveyData.Assign(StrataSizes, StrataSizes)
  SurveyData.Assign(NumberUnitsSelected, NumberUnitsSelected)
  
  Frame <- CreateSurveyFrame(SurveyNumber)
  
  if (SurveyNumber > 1)
  {
    # Permute Random Numbers
    Frame$StrataID <- rep("", length(UnitID)) # init microstrata
    SurveySequence <- GetSurveySequence(SurveyNumber, CoordinationMethod)
    
    if(!any(SurveySequence == 0))
    {
      for(Survey in SurveySequence) # GlobalNegative
      {
        Frame <- Frame[with(Frame, order(UnitID)), ] # required to update col
        Frame$StrataID <- paste0(Frame$StrataID, 
                                 SurveyData.Get(StrataID)[[Survey]]) # microstrat
        Frame$Burdens <- CalculateCumulativeBurden(SurveyData.Get(Burdens)[seq(Survey,
                                                                               SurveyNumber - 1)])
        Frame <- Frame[with(Frame, order(StrataID, Burdens, RandomNumbers)), ] # order cols
        
        Frame <- split(Frame, Frame$StrataID) # split into StrataID
        
        for(j in 1:length(Frame)) # sort within previous StrataID
        {
          Frame[[j]]$RandomNumbers <- sort(Frame[[j]]$RandomNumbers) # replace R with sorted R
        }
        
        Frame <- do.call(rbind,Frame) # rebind
      }
    }
    
    Frame$StrataID <- SurveyData.Get(StrataID)[[SurveyNumber]] # update StrataID
    Frame$Burdens <- rep(0, length(UnitID)) # update burdens
    
    if(any(SurveySequence == 0))
    {
      Frame$RandomNumbers <- runif(100) # generate new set of random numbers
    }
  }
  
  Frame <- Frame[with(Frame, order(StrataID,Burdens,RandomNumbers)),] # order cols
  Frame <- split(Frame, Frame$StrataID) # resplit into StrataID
  
  # Select Units
  for(Survey in 1:length(Frame))
  {
    NumberUnitsToSelect <- SurveyData.Get(NumberUnitsSelected)[[SurveyNumber]][Survey]
    
    # units at the "head" of the frame are selected
    SelectedUnits <- head(Frame[[Survey]], NumberUnitsToSelect)
    SelectedUnits$Burdens <- BurdenFactor
    
    NotSelectedUnits <- tail(Frame[[Survey]], -NumberUnitsToSelect)
    
    Frame[[Survey]] <- rbind(SelectedUnits, NotSelectedUnits)
  }
  
  Frame <- do.call(rbind,Frame) # rebind StrataID
  Frame <- Frame[with(Frame,order(UnitID)),] # reorder cols by UnitID
  
  # Update burdens and random numbers in lists
  UpdateSurveyLists(Frame, SurveyNumber, 
                    !any(GetSurveySequence(SurveyNumber, CoordinationMethod) == 0))
}

#' @title Run a sampling simulation
#' @description Runs a sampling simulation and produces the cumulative burden
#'              for each of the specified coordination methods
#' @param FrameNames The names of the R data frames containing the survey frames
#' @param DesignNames The names of the R data frames containing the survey designs
#' @param DatabaseNames The names of the R data frames containing the survey databases
#' @param BurdenFactors The burden associated with each survey
#' @param CoordinationMethods The coordination methods to use (valid options are "GlobalNegative", "Microstrata" and "CottonHesse")
#' @details Input: FrameNames, DesignNames and DatabaseNames are objects in SurveyData environment;
#'          Ouput: CumulativeBurden in SurveyData environment
#' @examples RunSimulation (FrameNames = rep(c("SampleFrame1","SampleFrame2"), 5),
#'                          DesignNames = rep(c("SampleDesign1", "SampleDesign2"), 5),
#'                          DatabaseNames = rep(c("SampleDatabase1", "SampleDatabase2"), 5),
#'                          BurdenFactors = rep(c(1, 5, 4, 3, 7), 2),
#'                          CoordinationMethods = c("Microstrata", "GlobalNegative", "CottonHesse", "None"))
#' @export
RunSimulation <- function(FrameNames, DesignNames, DatabaseNames, BurdenFactors, CoordinationMethods)
{
  SurveyData.Assign (CumulativeBurden, list ())
  NumberSurveys = length(FrameNames) # FrameNames, DesignNames, DatabaseNames need same length
    
  for (Method in 1:length(CoordinationMethods))
  {
    for (Survey in 1:NumberSurveys)
    { 
      RunSurvey (FrameName = FrameNames[Survey], 
                 DesignName = DesignNames[Survey], 
                 DatabaseName = DatabaseNames[Survey], 
                 SurveyNumber = Survey,
                 BurdenFactor = BurdenFactors[Survey],
                 CoordinationMethod = CoordinationMethods[Method])
    }
    
    CumulativeBurden <- SurveyData.Get (CumulativeBurden)
    CumulativeBurden[[Method]] <- SurveyData.Get (Burdens)
    SurveyData.Assign(CumulativeBurden, CumulativeBurden)
  }
  
  CumulativeBurden <- SurveyData.Get(CumulativeBurden)
  names(CumulativeBurden) <- CoordinationMethods
  SurveyData.Assign(CumulativeBurden, CumulativeBurden)
}

#' @title Compare cumulative burdens
#' @description Compare the cumulative burdens of coordination methods produced from a simulation
#' @param CumulativeBurden A list of cumulative burdens from the SurveyData environment
#' @examples RunSimulation (FrameNames = rep(c("SampleFrame1","SampleFrame2"), 5),
#'                          DesignNames = rep(c("SampleDesign1", "SampleDesign2"), 5),
#'                          DatabaseNames = rep(c("SampleDatabase1", "SampleDatabase2"), 5),
#'                          BurdenFactors = rep(c(1, 5, 4, 3, 7), 2),
#'                          CoordinationMethods = c("Microstrata", "GlobalNegative", "CottonHesse", "None"))
#' CompareBurdens (SurveyData.Get (CumulativeBurden))
#' @export
CompareBurdens <- function(CumulativeBurden)
{
  Cb <- lapply(CumulativeBurden, CalculateCumulativeBurden)
  xlim <- c(0, max(as.numeric(lapply(Cb,max))))
  ylim <- c(0, max(as.numeric(lapply(lapply(Cb, function(x) as.numeric(table(x))), max))))
  
  for(i in 1:length(Cb))
  {
    if(i == 1)
    {
      plot(table(Cb[[i]]), type = 'l', col = i, xlim = xlim, xaxt = "n",
           ylim = ylim, ylab = "Frequency", xlab = "Cumulative Burden")
    }
    else
    {
      lines(table(Cb[[i]]), col = i, type = 'l', xlim = xlim, ylim = ylim)
    }
  }
  
  axis(side = 1, at = seq(xlim[1], xlim[2], floor(sqrt(xlim[2]))), 
       labels = seq(xlim[1], xlim[2], floor(sqrt(xlim[2]))))
  
  legend('topright', legend = names(Cb),
         lty = rep(1, 4), col = 1:4)
}
