# OSTSC
Over sampling for time series classification. This package balances the binary imbalance time series data by a combination of ESPO and ADASYN approaches. Windows & Linux system supported.

# Installation
```java
library(devtools)
install_github("lweicdsor/OSTSC")
```
# Usage
library(OSTSC)

#### This is a simple example to show the usage. More complex examples are inside the vignettes.

#### #loading data
data(synthetic_control)  
#### #get feature and label data 
train_label <- synthetic_control$train_y      

train_sample <- synthetic_control$train_x  
#### #the first dimension of feature and label shall be the same
#### #the second dimention of feature is the time sequence length
dim(train_sample)
dim(train_label)
#### #check the imbalance of the data
table(train_label)
#### #oversample the class 1 to the same amount of class 0
MyData <- OSTSC(train_sample, train_label)
#### #store the feature data after oversampling
x <- MyData$sample
#### #store the label data after oversampling
y <- MyData$label
#### #check the imbalance of the data
table(y)
