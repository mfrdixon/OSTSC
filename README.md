# OSTSC
Over sampling for time series classification. This package balances the binary imbalance time series data by a combination of ESPO and ADASYN approaches. Windows & Linux system supported.

# Installation
```java
library(devtools)
install_github("lweicdsor/OSTSC")
```
# Usage
library(OSTSC)

# Start the function
#### #loading data
data(dataset_synthetic_control)  
#### #get split feature and label data 
train_label <- dataset_synthetic_control$train_y      

train_sample <- dataset_synthetic_control$train_x  
#### #oversample the class 1 to the same amount of class 0
MyData <- OSTSC(train_sample, train_label, target_class = 1)
#### #print the feature data after oversampling
MyData$sample
#### #print the label data after oversampling
MyData$label
