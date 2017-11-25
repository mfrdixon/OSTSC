# OSTSC
Over sampling for time series classification. 

OSTSC implements oversampling for univariate, multimodal, time series classification. It has been tested in the Windows & Linux system. 

# Installation
```java
library(devtools)
install_github("lweicdsor/OSTSC")
```
# Usage
library(OSTSC)

#### Here is a simple example showing package usage. A tutorial with more complex examples is provided in the vignette. (https://github.com/lweicdsor/OSTSC/blob/master/inst/doc/Over_Sampling_for_Time_Series_Classification.pdf)

#### # loading data
data(Dataset_Synthetic_Control)  
#### # get feature and label data 
train.label <- Dataset_Synthetic_Control$train.y      

train.sample <- Dataset_Synthetic_Control$train.x  
#### # the first dimension of the feature set and labels should be the same
#### # the second dimension of feature set is the time sequence length
dim(train.sample)

dim(train.label)
#### # check the imbalance ratio of the labelled data
table(train.label)
#### # oversample the class 1 to the same amount as class 0
MyData <- OSTSC(train.sample, train.label)
#### # store the feature data after oversampling
x <- MyData$sample
#### # store the label data after oversampling
y <- MyData$label
#### # check the imbalance of the data
table(y)
