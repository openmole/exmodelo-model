library(tidyverse)
library(rmarkdown)

wd <- getwd()

default_params <- list(
  input_variables=c("humanInformedRatio",
                    "humanInformProbability",
                    "humanFollowProbability"),
  prior_bounds=list(humanInformedRatio = c(0,1), 
                    humanInformProbability = c(0,1), 
                    humanFollowProbability = c(0,1)), 
  bins = list(humanInformedRatio = 20, 
              humanInformProbability = 20, 
              humanFollowProbability = 20),
  bandwidth=list(humanInformedRatio = 0.1, 
              humanInformProbability = 0.1, 
              humanFollowProbability = 0.1))

render("abc-report.Rmd", output_dir=wd, output_file="abc_rescuedDynamic.html",
  params = c(
    list(datafile= 
      str_c(wd, "../results/abc/rescuedDynamic/step872.csv", sep="/")),
    default_params))

render("abc-report.Rmd", output_dir=wd, output_file="abc_rescuedDynamic_highDim.html",
  params = c(
    list(datafile= 
      str_c(wd, "../results/abc/rescuedDynamic_highDim/step869.csv", sep="/")),
    default_params))

render("abc-report.Rmd", output_dir=wd, output_file="abc_totalRescued.html",
  params = c(
    list(datafile= 
      str_c(wd, "../results/abc/totalRescued/step542.csv", sep="/")),
    default_params))

render("abc-report.Rmd", output_dir=wd, output_file="abc_rescuedSummary.html",
  params = c(
    list(datafile= 
      str_c(wd, "../results/abc/rescuedSummary/step297.csv", sep="/")),
    default_params))
