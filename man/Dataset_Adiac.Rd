% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dataset_Adiac.R
\docType{data}
\name{Dataset_Adiac}
\alias{Dataset_Adiac}
\title{The automatic diatoms identification.}
\format{A dataset with 781 observations and a sequence length of 176, with a single sequence per row. 
\describe{
  The y data is the class label (1 or 0).
  
  The x data constructs time series sequences (numeric).
  
  The training dataset contains 390 observations.
  
  The testing dataset contains 391 observations.
}}
\source{
\url{http://timeseriesclassification.com/description.php?Dataset=Adiac}
}
\usage{
data(Dataset_Adiac)
}
\description{
The data is collected from a pilot study on automatic identification of 
diatoms (unicellular algae) from images.
}
\details{
The dataset originally had
37 classes. This built-in data sets one class as the positive class (class 1) and all others
are set to the negative class (class 0) to form a highly imbalanced dataset.
}
\keyword{datasets}
