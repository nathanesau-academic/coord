# coord
Sample coordination in R

## Instructions

1. Install the package using ``devtools::install_github("nathanesau-academic/coord")``.
2. Try out the sample script below to get started.

```R
set.seed(10)
SurveyData.Assign (projdir, "~/Documents/Jupyter/Coordination")
RunSimulation (FrameNames = rep(c("SampleFrame1","SampleFrame2"), 5),
               DesignNames = rep(c("SampleDesign1", "SampleDesign2"), 5),
               DatabaseNames = rep(c("SampleDatabase1", "SampleDatabase2"), 5),
               BurdenFactors = rep(c(1, 5, 4, 3, 7), 2),
               CoordinationMethods = c("Microstrata", "GlobalNegative", "Cotton-Hesse", "None"))
CompareBurdens (SurveyData.Get (CumulativeBurden))
```