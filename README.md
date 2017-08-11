# OSTSC
Over sampling for time series classification. This package balances the binary imbalance time series data by a combination of EPSO and ADASYN approaches.

# Installation
```java
library(devtools)
install_github("lweicdsor/OSTSC")
```
# Usage
library(OSTSC)

# Start the function
#### #loading data
data(synthetic_control_TRAIN)   
#### #create feature and label data 
label <- synthetic_control_TRAIN[, c(1)]
sample <- synthetic_control_TRAIN[, -1] 
#### #oversample the class 1 to the same amount of class 0
MyData <- OSTSC(sample, label, target_class = 1)
#### #print the feature data after oversampling
MyData$sample
#### #print the label data after oversampling
MyData$label
