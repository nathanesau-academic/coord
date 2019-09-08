# Title: Coordination in R
# Author: Nathan Esau
# Last updated: July 24, 2016

set.seed(10)

#' @title [DATA] Sample Frame 1
#' @description Sample Frame 1. Contains two columns: UnitID, StratID.
#' @export
SampleFrame1 <- data.frame (UnitID = seq(1, 100, 1), 
                            StratID = c(rep(11, 20), rep(12, 80)))

#' @title [DATA] Sample Design 1
#' @description Sample Design 1. Contains two columns: StratID, NumberUnitsSelected.
#' @export
SampleDesign1 <- data.frame (StratID = c(11, 12),
                             NumberUnitsSelected = c(5,10))

#' @title [DATA] Sample Database 1
#' @description Sample Database 1. Contains two columns: UnitID, RandomNumbers.
#' @export
SampleDatabase1 <- data.frame (UnitID = seq(1, 100, 1),
                               RandomNumbers = runif(100))

#' @title [DATA] Sample Frame 2
#' @description Sample Frame 2. Contains two columns: UnitID, StratID.
#' @export
SampleFrame2 <- data.frame (UnitID = seq(1, 100, 1),
                            StratID = c(rep(21, 40), rep(22, 25), rep(23, 35)))

#' @title [DATA] Sample Design 2
#' @description Sample Design 2. Contains two columns: StratID, NumberUnitsSelected.
#' @export
SampleDesign2 <- data.frame(StratID = c(21, 22, 23), 
                            NumberUnitsSelected = c(15, 10, 12))

#' @title [DATA] Sample Database 2
#' @description Sample Database 2. Contains two columns: UnitID, RandomNumbers.
#' @export
SampleDatabase2 <- data.frame(UnitID = seq(1, 100, 1),
                              RandomNumbers = runif(100))

SurveyData.Assign(SampleFrame1, SampleFrame1)
SurveyData.Assign(SampleFrame2, SampleFrame2)
SurveyData.Assign(SampleDesign1, SampleDesign1)
SurveyData.Assign(SampleDesign2, SampleDesign2)
SurveyData.Assign(SampleDatabase1, SampleDatabase1)
SurveyData.Assign(SampleDatabase2, SampleDatabase2)
