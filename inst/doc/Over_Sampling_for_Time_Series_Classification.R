## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE-----------------
 require("knitcitations")
 cleanbib()
 options("citation_format" = "pandoc")

## ---- echo=FALSE, message=FALSE------------------------------------------
require(OSTSC)
require(keras)
require(rio)
require(dummies)
require(pROC)
rfv <- local(get(load(url('https://github.com/lweicdsor/GSoC2017/raw/master/ResultForVignettes.rdata'))))

## ------------------------------------------------------------------------
data(Dataset_Synthetic_Control)

train.label <- Dataset_Synthetic_Control$train.y
train.sample <- Dataset_Synthetic_Control$train.x
test.label <- Dataset_Synthetic_Control$test.y
test.sample <- Dataset_Synthetic_Control$test.x

## ------------------------------------------------------------------------
dim(train.sample)

## ------------------------------------------------------------------------
table(train.label)

## ---- results='hide'-----------------------------------------------------
MyData <- OSTSC(train.sample, train.label, parallel = FALSE)
over.sample <- MyData$sample
over.label <- MyData$label

## ------------------------------------------------------------------------
table(over.label)

## ------------------------------------------------------------------------
dim(over.sample)

## ------------------------------------------------------------------------
data(Dataset_Adiac)

train.label <- Dataset_Adiac$train.y
train.sample <- Dataset_Adiac$train.x
test.label <- Dataset_Adiac$test.y
test.sample <- Dataset_Adiac$test.x

## ------------------------------------------------------------------------
dim(train.sample)

## ------------------------------------------------------------------------
table(train.label)

## ---- results='hide'-----------------------------------------------------
MyData <- OSTSC(train.sample, train.label, parallel = FALSE)
over.sample <- MyData$sample
over.label <- MyData$label

## ------------------------------------------------------------------------
table(over.label)

## ------------------------------------------------------------------------
data(Dataset_HFT300)

train.label <- Dataset_HFT300$y
train.sample <- Dataset_HFT300$x

## ------------------------------------------------------------------------
dim(train.sample)

## ------------------------------------------------------------------------
table(train.label)

## ---- results='hide'-----------------------------------------------------
MyData <- OSTSC(train.sample, train.label, parallel = FALSE)
over.sample <- MyData$sample
over.label <- MyData$label

## ------------------------------------------------------------------------
table(over.label)

## ------------------------------------------------------------------------
ElectricDevices <- Dataset_ElectricDevices()

train.label <- ElectricDevices$train.y
train.sample <- ElectricDevices$train.x
test.label <- ElectricDevices$test.y
test.sample <- ElectricDevices$test.x

## ------------------------------------------------------------------------
dim(train.sample)

## ------------------------------------------------------------------------
table(train.label)

## ---- results='hide'-----------------------------------------------------
MyData <- OSTSC(train.sample, train.label, parallel = FALSE)
over.sample <- MyData$sample
over.label <- MyData$label

## ------------------------------------------------------------------------
table(over.label)

## ---- eval = FALSE-------------------------------------------------------
#  library(keras)
#  train.y <- to_categorical(train.label)
#  test.y <- to_categorical(test.label)
#  train.x <- array(train.sample, dim = c(dim(train.sample),1))
#  test.x <- array(test.sample, dim = c(dim(test.sample),1))

## ---- eval = FALSE-------------------------------------------------------
#  model <- keras_model_sequential()
#  model %>%
#    layer_lstm(10, input_shape = c(dim(train.x)[2], dim(train.x)[3])) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_dense(dim(train.y)[2]) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_activation("softmax")
#  model %>% compile(
#    loss = "categorical_crossentropy",
#    optimizer = "adam",
#    metrics = "accuracy"
#  )
#  lstm.before <- model %>% fit(
#    x = train.x,
#    y = train.y,
#    validation_split = 0.2,
#    epochs = 20
#  )

## ---- echo = FALSE, message=FALSE----------------------------------------
pred.label <- as.vector(unlist(rfv$EDp1)) 
lstm.before <- rfv$EDh1 
score <- rfv$EDs1

## ---- fig.width = 5, fig.height = 3, fig.cap = "The loss and accuracy of the LSTM classifier trained on the unbalanced Electric Devices dataset. Both metrics are evaluated at the end of each epoch."----
plot(lstm.before)

## ---- eval = FALSE-------------------------------------------------------
#  score <- model %>% evaluate(test.x, test.y)

## ---- echo = FALSE-------------------------------------------------------
cat("The loss value is", unlist(score[1]), ".\n")
cat("The metric value (in this case 'accuracy') is", unlist(score[2]), ".\n")

## ---- eval = FALSE-------------------------------------------------------
#  over.y <- to_categorical(over.label)
#  over.x <- array(over.sample, dim = c(dim(over.sample),1))

## ---- eval = FALSE-------------------------------------------------------
#  model.over <- keras_model_sequential()
#  model.over %>%
#    layer_lstm(10, input_shape = c(dim(over.x)[2], dim(over.x)[3])) %>%
#    layer_dropout(rate = 0.1) %>%
#    layer_dense(dim(over.y)[2]) %>%
#    layer_dropout(rate = 0.1) %>%
#    layer_activation("softmax")
#  model.over %>% compile(
#    loss = "categorical_crossentropy",
#    optimizer = "adam",
#    metrics = "accuracy"
#  )
#  lstm.after <- model.over %>% fit(
#    x = over.x,
#    y = over.y,
#    validation_split = 0.2,
#    epochs = 20
#  )

## ---- echo = FALSE, message=FALSE----------------------------------------
pred.label.over <- as.vector(unlist(rfv$EDp2)) 
lstm.after <- rfv$EDh2 
score.over <- rfv$EDs2

## ---- fig.width = 5, fig.height = 3, fig.cap = "The loss and accuracy of the LSTM classifier trained on the oversampled Electric Devices dataset. Both metrics are evaluated at the end of each epoch."----
plot(lstm.after)

## ---- eval = FALSE-------------------------------------------------------
#  score.over <- model.over %>% evaluate(test.x, test.y)

## ---- echo = FALSE-------------------------------------------------------
cat("The loss value is", unlist(score.over[1]), ".\n")
cat("The metric value (in this case 'accuracy') is", unlist(score.over[2]), ".\n")

## ---- eval = FALSE-------------------------------------------------------
#  pred.label <- model %>% predict_classes(test.x)
#  pred.label.over <- model.over %>% predict_classes(test.x)

## ------------------------------------------------------------------------
cm.before <- table(test.label, pred.label)
cm.after <- table(test.label, pred.label.over)

## ---- echo = FALSE, fig.width = 4, fig.height = 2, fig.cap = "Normalized confusion matrix of LSTM applied to the Electric Devices dataset without oversampling."----
layout(matrix(c(1,1,1)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')

rect(150, 430, 240, 370, col='#3F97D0')
rect(250, 430, 340, 370, col='#F7AD50')
rect(150, 305, 240, 365, col='#F7AD50')
rect(250, 305, 340, 365, col='#3F97D0')
text(195, 435, '5', cex=1.1)
text(295, 435, '6', cex=1.1)
text(125, 370, 'True', cex=1.2, srt=90, font=2)
text(245, 450, 'Predicted', cex=1.2, font=2)
text(140, 400, '5', cex=1.1, srt=90)
text(140, 335, '6', cex=1.1, srt=90)

res <- as.numeric(cm.before)
sum1 <- res[1] + res[3]
sum2 <- res[2] + res[4] 
text(195, 400, round(res[1]/sum1, 4), cex=1.3, font=2, col='white')
text(195, 335, round(res[2]/sum2, 4), cex=1.3, font=2, col='white')
text(295, 400, round(res[3]/sum1, 4), cex=1.3, font=2, col='white')
text(295, 335, round(res[4]/sum2, 4), cex=1.3, font=2, col='white')

## ---- echo = FALSE, fig.width = 4, fig.height = 2, fig.cap = "Normalized confusion matrix of LSTM applied to the Electric Devices dataset with oversampling."----
layout(matrix(c(1,1,1)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')

rect(150, 430, 240, 370, col='#3F97D0')
rect(250, 430, 340, 370, col='#F7AD50')
rect(150, 305, 240, 365, col='#F7AD50')
rect(250, 305, 340, 365, col='#3F97D0')
text(195, 435, '5', cex=1.1)
text(295, 435, '6', cex=1.1)
text(125, 370, 'True', cex=1.2, srt=90, font=2)
text(245, 450, 'Predicted', cex=1.2, font=2)
text(140, 400, '5', cex=1.1, srt=90)
text(140, 335, '6', cex=1.1, srt=90)

res <- as.numeric(cm.after)
sum1 <- res[1] + res[3]
sum2 <- res[2] + res[4] 
text(195, 400, round(res[1]/sum1, 4), cex=1.3, font=2, col='white')
text(195, 335, round(res[2]/sum2, 4), cex=1.3, font=2, col='white')
text(295, 400, round(res[3]/sum1, 4), cex=1.3, font=2, col='white')
text(295, 335, round(res[4]/sum2, 4), cex=1.3, font=2, col='white')

## ---- echo = FALSE, warning = FALSE, message = FALSE, fig.width = 5, fig.height = 3, fig.cap = "Receiver operating characteristic (ROC) curves comparing the effect of oversampling on the performance of the LSTM applied to the Electric Devices dataset."----
library(pROC)
plot.roc(as.vector(test.label), pred.label, legacy.axes = TRUE, col = "blue", print.auc = TRUE,  
         print.auc.cex= .8, xlab = 'False Positive Rate', ylab = 'True Positive Rate') 
plot.roc(as.vector(test.label), pred.label.over, legacy.axes = TRUE, col = "red", print.auc = TRUE,   
         print.auc.y = .4, print.auc.cex= .8, add = TRUE)
legend("bottomright", legend=c("Without oversampling", "With oversampling"), 
       col=c("blue", "red"), lwd=2, cex= .6)

## ------------------------------------------------------------------------
ECG <- Dataset_ECG()

train.label <- ECG$train.y
train.sample <- ECG$train.x
test.label <- ECG$test.y
test.sample <- ECG$test.x

## ------------------------------------------------------------------------
dim(train.sample)

## ------------------------------------------------------------------------
table(train.label)

## ---- results='hide'-----------------------------------------------------
MyData <- OSTSC(train.sample, train.label, parallel = FALSE)
over.sample <- MyData$sample
over.label <- MyData$label

## ------------------------------------------------------------------------
table(over.label)

## ---- eval = FALSE-------------------------------------------------------
#  library(keras)
#  library(dummies)
#  train.y <- dummy(train.label)
#  test.y <- dummy(test.label)
#  train.x <- array(train.sample, dim = c(dim(train.sample),1))
#  test.x <- array(test.sample, dim = c(dim(test.sample),1))

## ---- eval = FALSE-------------------------------------------------------
#  model <- keras_model_sequential()
#  model %>%
#    layer_lstm(10, input_shape = c(dim(train.x)[2], dim(train.x)[3])) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_dense(dim(train.y)[2]) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_activation("softmax")
#  model %>% compile(
#    loss = "categorical_crossentropy",
#    optimizer = "adam",
#    metrics = "accuracy"
#  )
#  lstm.before <- model %>% fit(
#    x = train.x,
#    y = train.y,
#    validation_split = 0.2,
#    epochs = 20
#  )

## ---- echo = FALSE, message=FALSE----------------------------------------
pred.label <- as.vector(unlist(rfv$ECGp1)) 
lstm.before <- rfv$ECGh1 
score <- rfv$ECGs1

## ---- fig.width = 5, fig.height = 3, fig.cap = "The loss and accuracy of the LSTM classifier trained on the oversampled Electrocardiogram dataset. Both metrics are evaluated at the end of each epoch."----
plot(lstm.before)

## ---- eval = FALSE-------------------------------------------------------
#  score <- model %>% evaluate(test.x, test.y)

## ---- echo = FALSE-------------------------------------------------------
cat("The loss value is", unlist(score[1]), ".\n")
cat("The metric value (in this case 'accuracy') is", unlist(score[2]), ".\n")

## ---- eval = FALSE-------------------------------------------------------
#  over.y <- dummy(over.label)
#  over.x <- array(over.sample, dim = c(dim(over.sample),1))

## ---- eval = FALSE-------------------------------------------------------
#  model.over <- keras_model_sequential()
#  model.over %>%
#    layer_lstm(10, input_shape = c(dim(over.x)[2], dim(over.x)[3])) %>%
#    layer_dropout(rate = 0.1) %>%
#    layer_dense(dim(over.y)[2]) %>%
#    layer_dropout(rate = 0.1) %>%
#    layer_activation("softmax")
#  model.over %>% compile(
#    loss = "categorical_crossentropy",
#    optimizer = "adam",
#    metrics = "accuracy"
#  )
#  lstm.after <- model.over %>% fit(
#    x = over.x,
#    y = over.y,
#    validation_split = 0.2,
#    epochs = 20
#  )

## ---- echo = FALSE, message=FALSE----------------------------------------
pred.label.over <- as.vector(unlist(rfv$ECGp2)) 
lstm.after <- rfv$ECGh2 
score.over <- rfv$ECGs2

## ---- fig.width = 5, fig.height = 3, fig.cap = "The loss and accuracy of the LSTM classifier trained on the oversampled Electrocardiogram dataset. Both metrics are evaluated at the end of each epoch."----
plot(lstm.after)

## ---- eval = FALSE-------------------------------------------------------
#  score.over <- model.over %>% evaluate(test.x, test.y)

## ---- echo = FALSE-------------------------------------------------------
cat("The loss value is", unlist(score.over[1]), ".\n")
cat("The metric value (in this case 'accuracy') is", unlist(score.over[2]), ".\n")

## ---- eval = FALSE-------------------------------------------------------
#  pred.label <- model %>% predict_classes(test.x)
#  pred.label.over <- model.over %>% predict_classes(test.x)

## ------------------------------------------------------------------------
cm.before <- table(test.label, pred.label)
cm.after <- table(test.label, pred.label.over)

## ---- echo = FALSE, fig.width = 4, fig.height = 2, fig.cap = "Normalized confusion matrix of LSTM applied to the Electrocardiogram dataset without oversampling."----
layout(matrix(c(1,1,1)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(290, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')

rect(150, 430, 195, 400, col='#3F97D0')
rect(200, 430, 245, 400, col='#F7AD50')
rect(250, 430, 295, 400, col='#F7AD50')
rect(300, 430, 345, 400, col='#F7AD50')

rect(150, 395, 195, 365, col='#F7AD50')
rect(200, 395, 245, 365, col='#3F97D0')
rect(250, 395, 295, 365, col='#F7AD50')
rect(300, 395, 345, 365, col='#F7AD50')

rect(150, 360, 195, 330, col='#F7AD50')
rect(200, 360, 245, 330, col='#F7AD50')
rect(250, 360, 295, 330, col='#3F97D0')
rect(300, 360, 345, 330, col='#F7AD50')

rect(150, 325, 195, 295, col='#F7AD50')
rect(200, 325, 245, 295, col='#F7AD50')
rect(250, 325, 295, 295, col='#F7AD50')
rect(300, 325, 345, 295, col='#3F97D0')

text(172, 435, '1', cex=1.1)
text(222, 435, '3', cex=1.1)
text(272, 435, '4', cex=1.1)
text(322, 435, '5', cex=1.1)
text(140, 415, '1', cex=1.1, srt=90)
text(140, 380, '3', cex=1.1, srt=90)
text(140, 345, '4', cex=1.1, srt=90)
text(140, 310, '5', cex=1.1, srt=90)
text(120, 370, 'True', cex=1.2, srt=90, font=2)
text(240, 450, 'Predicted', cex=1.2, font=2)
  
res <- as.numeric(cm.before)
sum1 <- res[1] + res[5] + res[9] + res[13]
sum2 <- res[2] + res[6] + res[10] + res[14]
sum3 <- res[3] + res[7] + res[11] + res[15]
sum4 <- res[4] + res[8] + res[12] + res[16]
text(172, 415, round(res[1]/sum1, 4), cex=1.3, font=2, col='white')
text(172, 380, round(res[2]/sum2, 4), cex=1.3, font=2, col='white')
text(172, 345, round(res[3]/sum3, 4), cex=1.3, font=2, col='white')
text(172, 310, round(res[4]/sum4, 4), cex=1.3, font=2, col='white')
text(222, 415, round(res[5]/sum1, 4), cex=1.3, font=2, col='white')
text(222, 380, round(res[6]/sum2, 4), cex=1.3, font=2, col='white')
text(222, 345, round(res[7]/sum3, 4), cex=1.3, font=2, col='white')
text(222, 310, round(res[8]/sum4, 4), cex=1.3, font=2, col='white')
text(272, 415, round(res[9]/sum1, 4), cex=1.3, font=2, col='white')
text(272, 380, round(res[10]/sum2, 4), cex=1.3, font=2, col='white')
text(272, 345, round(res[11]/sum3, 4), cex=1.3, font=2, col='white')
text(272, 310, round(res[12]/sum4, 4), cex=1.3, font=2, col='white')
text(322, 415, round(res[13]/sum1, 4), cex=1.3, font=2, col='white')
text(322, 380, round(res[14]/sum2, 4), cex=1.3, font=2, col='white')
text(322, 345, round(res[15]/sum3, 4), cex=1.3, font=2, col='white')
text(322, 310, round(res[16]/sum4, 4), cex=1.3, font=2, col='white')

## ---- echo = FALSE, fig.width = 4, fig.height = 2, fig.cap = "Normalized confusion matrix of LSTM applied to the Electrocardiogram dataset with oversampling."----
layout(matrix(c(1,1,1)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(290, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')

rect(150, 430, 195, 400, col='#3F97D0')
rect(200, 430, 245, 400, col='#F7AD50')
rect(250, 430, 295, 400, col='#F7AD50')
rect(300, 430, 345, 400, col='#F7AD50')

rect(150, 395, 195, 365, col='#F7AD50')
rect(200, 395, 245, 365, col='#3F97D0')
rect(250, 395, 295, 365, col='#F7AD50')
rect(300, 395, 345, 365, col='#F7AD50')

rect(150, 360, 195, 330, col='#F7AD50')
rect(200, 360, 245, 330, col='#F7AD50')
rect(250, 360, 295, 330, col='#3F97D0')
rect(300, 360, 345, 330, col='#F7AD50')

rect(150, 325, 195, 295, col='#F7AD50')
rect(200, 325, 245, 295, col='#F7AD50')
rect(250, 325, 295, 295, col='#F7AD50')
rect(300, 325, 345, 295, col='#3F97D0')

text(172, 435, '1', cex=1.1)
text(222, 435, '3', cex=1.1)
text(272, 435, '4', cex=1.1)
text(322, 435, '5', cex=1.1)
text(140, 415, '1', cex=1.1, srt=90)
text(140, 380, '3', cex=1.1, srt=90)
text(140, 345, '4', cex=1.1, srt=90)
text(140, 310, '5', cex=1.1, srt=90)
text(120, 370, 'True', cex=1.2, srt=90, font=2)
text(240, 450, 'Predicted', cex=1.2, font=2)
  
res <- as.numeric(cm.after)
sum1 <- res[1] + res[5] + res[9] + res[13]
sum2 <- res[2] + res[6] + res[10] + res[14]
sum3 <- res[3] + res[7] + res[11] + res[15]
sum4 <- res[4] + res[8] + res[12] + res[16]
text(172, 415, round(res[1]/sum1, 4), cex=1.3, font=2, col='white')
text(172, 380, round(res[2]/sum2, 4), cex=1.3, font=2, col='white')
text(172, 345, round(res[3]/sum3, 4), cex=1.3, font=2, col='white')
text(172, 310, round(res[4]/sum4, 4), cex=1.3, font=2, col='white')
text(222, 415, round(res[5]/sum1, 4), cex=1.3, font=2, col='white')
text(222, 380, round(res[6]/sum2, 4), cex=1.3, font=2, col='white')
text(222, 345, round(res[7]/sum3, 4), cex=1.3, font=2, col='white')
text(222, 310, round(res[8]/sum4, 4), cex=1.3, font=2, col='white')
text(272, 415, round(res[9]/sum1, 4), cex=1.3, font=2, col='white')
text(272, 380, round(res[10]/sum2, 4), cex=1.3, font=2, col='white')
text(272, 345, round(res[11]/sum3, 4), cex=1.3, font=2, col='white')
text(272, 310, round(res[12]/sum4, 4), cex=1.3, font=2, col='white')
text(322, 415, round(res[13]/sum1, 4), cex=1.3, font=2, col='white')
text(322, 380, round(res[14]/sum2, 4), cex=1.3, font=2, col='white')
text(322, 345, round(res[15]/sum3, 4), cex=1.3, font=2, col='white')
text(322, 310, round(res[16]/sum4, 4), cex=1.3, font=2, col='white')

## ---- echo = FALSE, warning = FALSE, message = FALSE, fig.width = 5, fig.height = 3, fig.cap = "ROC curves of the LSTM applied to the Electrocardiogram dataset, with and without oversampling."----
library(pROC)
plot.roc(as.vector(test.label), pred.label, legacy.axes = TRUE, col = "blue", print.auc = TRUE,  
         print.auc.cex= .8, xlab = 'False Positive Rate', ylab = 'True Positive Rate')
plot.roc(as.vector(test.label), pred.label.over, legacy.axes = TRUE, col = "red", print.auc = TRUE,   
         print.auc.y = .4, print.auc.cex= .8, add = TRUE)
legend("bottomright", legend=c("Without oversampling", "With oversampling"), 
       col=c("blue", "red"), lwd=2, cex= .6)

## ------------------------------------------------------------------------
mhealth <- Dataset_MHEALTH()

train.label <- mhealth$train.y
train.sample <- mhealth$train.x
test.label <- mhealth$test.y
test.sample <- mhealth$test.x

## ------------------------------------------------------------------------
dim(train.sample)

## ------------------------------------------------------------------------
table(train.label)

## ---- results='hide'-----------------------------------------------------
MyData <- OSTSC(train.sample, train.label, parallel = FALSE)
over.sample <- MyData$sample
over.label <- MyData$label

## ------------------------------------------------------------------------
table(over.label)

## ---- echo=FALSE, message=FALSE------------------------------------------
pred.label <- as.vector(unlist(rfv$MHp1)) 
lstm.before <- rfv$MHh1 

## ---- fig.width = 5, fig.height = 3, fig.cap = "The loss and accuracy of the LSTM classifier trained on the MHEALTH dataset without oversampling. Both metrics are evaluated at the end of each epoch."----
plot(lstm.before)

## ---- echo=FALSE, message=FALSE------------------------------------------
pred.label.over <- as.vector(unlist(rfv$MHp2)) 
lstm.after <- rfv$MHh2  

## ---- fig.width = 5, fig.height = 3, fig.cap = "The loss and accuracy of the LSTM classifier trained on the MHEALTH dataset with oversampling. Both metrics are evaluated at the end of each epoch."----
plot(lstm.after)

## ---- echo = FALSE, fig.width = 4, fig.height = 2, fig.cap = "Normalized confusion matrix of LSTM applied to the MHEALTH dataset without oversampling."----
cm.before <- table(test.label, pred.label)
cm.after <- table(test.label, pred.label.over)

layout(matrix(c(1,1,1)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')

rect(150, 430, 240, 370, col='#3F97D0')
rect(250, 430, 340, 370, col='#F7AD50')
rect(150, 305, 240, 365, col='#F7AD50')
rect(250, 305, 340, 365, col='#3F97D0')
text(195, 435, '0', cex=1.1)
text(295, 435, '1', cex=1.1)
text(125, 370, 'True', cex=1.2, srt=90, font=2)
text(245, 450, 'Predicted', cex=1.2, font=2)
text(140, 400, '0', cex=1.1, srt=90)
text(140, 335, '1', cex=1.1, srt=90)

res <- as.numeric(cm.before)
sum1 <- res[1] + res[3]
sum2 <- res[2] + res[4] 
text(195, 400, round(res[1]/sum1, 4), cex=1.3, font=2, col='white')
text(195, 335, round(res[2]/sum2, 4), cex=1.3, font=2, col='white')
text(295, 400, round(res[3]/sum1, 4), cex=1.3, font=2, col='white')
text(295, 335, round(res[4]/sum2, 4), cex=1.3, font=2, col='white')

## ---- echo = FALSE, fig.width = 4, fig.height = 2, fig.cap = "Normalized confusion matrix of LSTM applied to the MHEALTH dataset with oversampling."----
layout(matrix(c(1,1,1)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')

rect(150, 430, 240, 370, col='#3F97D0')
rect(250, 430, 340, 370, col='#F7AD50')
rect(150, 305, 240, 365, col='#F7AD50')
rect(250, 305, 340, 365, col='#3F97D0')
text(195, 435, '0', cex=1.1)
text(295, 435, '1', cex=1.1)
text(125, 370, 'True', cex=1.2, srt=90, font=2)
text(245, 450, 'Predicted', cex=1.2, font=2)
text(140, 400, '0', cex=1.1, srt=90)
text(140, 335, '1', cex=1.1, srt=90)

res <- as.numeric(cm.after)
sum1 <- res[1] + res[3]
sum2 <- res[2] + res[4] 
text(195, 400, round(res[1]/sum1, 4), cex=1.3, font=2, col='white')
text(195, 335, round(res[2]/sum2, 4), cex=1.3, font=2, col='white')
text(295, 400, round(res[3]/sum1, 4), cex=1.3, font=2, col='white')
text(295, 335, round(res[4]/sum2, 4), cex=1.3, font=2, col='white')

## ---- echo = FALSE, warning = FALSE, message = FALSE, fig.width = 5, fig.height = 3, fig.cap = "ROC curves of the LSTM applied to the MHEALTH dataset, with and without oversampling."----
library(pROC)
plot.roc(as.vector(test.label), pred.label, legacy.axes = TRUE, col = "blue", 
         print.auc = TRUE, print.auc.cex= .8, xlab = 'False Positive Rate', 
         ylab = 'True Positive Rate')
plot.roc(as.vector(test.label), pred.label.over, legacy.axes = TRUE, col = "red", 
         print.auc = TRUE, print.auc.y = .4, print.auc.cex= .8, add = TRUE)
legend("bottomright", legend=c("Without Oversampling", "With Oversampling"), 
       col=c("blue", "red"), lwd=2, cex= .6)

## ------------------------------------------------------------------------
HFT <- Dataset_HFT()

label <- HFT$y
sample <- HFT$x
train.label <- label[1:15000]
train.sample <- sample[1:15000, ]
test.label <- label[15001:30000]
test.sample <- sample[15001:30000, ]

## ------------------------------------------------------------------------
table(train.label)

## ---- results='hide'-----------------------------------------------------
MyData <- OSTSC(train.sample, train.label, parallel = FALSE)
over.sample <- MyData$sample
over.label <- MyData$label

## ------------------------------------------------------------------------
table(over.label)

## ---- echo=FALSE, message=FALSE------------------------------------------
pred.label <- as.vector(unlist(rfv$HFTp1)) 
lstm.before <- rfv$HFTh1

## ---- fig.width = 5, fig.height = 3, fig.cap = "The loss and accuracy of the LSTM classifier trained on the HFT dataset without oversampling. Both metrics are evaluated at the end of each epoch."----
plot(lstm.before)

## ---- echo=FALSE, message=FALSE------------------------------------------
pred.label.over <- as.vector(unlist(rfv$HFTp2)) 
lstm.after <- rfv$HFTh2 

## ---- fig.width = 5, fig.height = 3, fig.cap = "The loss and accuracy of the LSTM classifier trained on the oversampled HFT dataset. Both metrics are evaluated at the end of each epoch."----
plot(lstm.after)

## ---- echo = FALSE, fig.width = 4, fig.height = 2, fig.cap = "Normalized confusion matrix of the LSTM applied to the HFT dataset without oversampling."----
cm.before <- table(test.label, pred.label)
cm.after <- table(test.label, pred.label.over)

layout(matrix(c(1,1,1)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')

rect(140, 430, 200, 390, col='#3F97D0')
rect(210, 430, 270, 390, col='#F7AD50')
rect(280, 430, 340, 390, col='#F7AD50')
rect(140, 345, 200, 385, col='#F7AD50')
rect(210, 345, 270, 385, col='#3F97D0')
rect(280, 345, 340, 385, col='#F7AD50')
rect(140, 300, 200, 340, col='#F7AD50')
rect(210, 300, 270, 340, col='#F7AD50')
rect(280, 300, 340, 340, col='#3F97D0')
text(170, 435, '-1', cex=1.1)
text(240, 435, '0', cex=1.1)
text(310, 435, '1', cex=1.1)
text(130, 410, '-1', cex=1.1, srt=90)
text(130, 365, '0', cex=1.1, srt=90)
text(130, 320, '1', cex=1.1, srt=90)
text(120, 370, 'True', cex=1.2, srt=90, font=2)
text(240, 450, 'Predicted', cex=1.2, font=2)
  
res <- as.numeric(cm.before)
sum1 <- res[1] + res[4] + res[7]
sum2 <- res[2] + res[5] + res[8]
sum3 <- res[3] + res[6] + res[9]
text(170, 410, round(res[1]/sum1, 4), cex=1.3, font=2, col='white')
text(170, 365, round(res[2]/sum2, 4), cex=1.3, font=2, col='white')
text(170, 320, round(res[3]/sum3, 4), cex=1.3, font=2, col='white')
text(240, 410, round(res[4]/sum1, 4), cex=1.3, font=2, col='white')
text(240, 365, round(res[5]/sum2, 4), cex=1.3, font=2, col='white')
text(240, 320, round(res[6]/sum3, 4), cex=1.3, font=2, col='white')
text(310, 410, round(res[7]/sum1, 4), cex=1.3, font=2, col='white')
text(310, 365, round(res[8]/sum2, 4), cex=1.3, font=2, col='white')
text(310, 320, round(res[9]/sum3, 4), cex=1.3, font=2, col='white')

## ---- echo = FALSE, fig.width = 4, fig.height = 2, fig.cap = "Normalized confusion matrix of the LSTM applied to the HFT dataset with oversampling."----
layout(matrix(c(1,1,1)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')

rect(140, 430, 200, 390, col='#3F97D0')
rect(210, 430, 270, 390, col='#F7AD50')
rect(280, 430, 340, 390, col='#F7AD50')
rect(140, 345, 200, 385, col='#F7AD50')
rect(210, 345, 270, 385, col='#3F97D0')
rect(280, 345, 340, 385, col='#F7AD50')
rect(140, 300, 200, 340, col='#F7AD50')
rect(210, 300, 270, 340, col='#F7AD50')
rect(280, 300, 340, 340, col='#3F97D0')
text(170, 435, '-1', cex=1.1)
text(240, 435, '0', cex=1.1)
text(310, 435, '1', cex=1.1)
text(130, 410, '-1', cex=1.1, srt=90)
text(130, 365, '0', cex=1.1, srt=90)
text(130, 320, '1', cex=1.1, srt=90)
text(120, 370, 'True', cex=1.2, srt=90, font=2)
text(240, 450, 'Predicted', cex=1.2, font=2)

res <- as.numeric(cm.after)
sum1 <- res[1] + res[4] + res[7]
sum2 <- res[2] + res[5] + res[8]
sum3 <- res[3] + res[6] + res[9]
text(170, 410, round(res[1]/sum1, 4), cex=1.3, font=2, col='white')
text(170, 365, round(res[2]/sum2, 4), cex=1.3, font=2, col='white')
text(170, 320, round(res[3]/sum3, 4), cex=1.3, font=2, col='white')
text(240, 410, round(res[4]/sum1, 4), cex=1.3, font=2, col='white')
text(240, 365, round(res[5]/sum2, 4), cex=1.3, font=2, col='white')
text(240, 320, round(res[6]/sum3, 4), cex=1.3, font=2, col='white')
text(310, 410, round(res[7]/sum1, 4), cex=1.3, font=2, col='white')
text(310, 365, round(res[8]/sum2, 4), cex=1.3, font=2, col='white')
text(310, 320, round(res[9]/sum3, 4), cex=1.3, font=2, col='white')

## ---- echo = FALSE, warning = FALSE, message = FALSE, fig.width = 5, fig.height = 3, fig.cap = "ROC curves of the LSTM applied to the HFT dataset with and without oversampling."----
library(pROC)
plot.roc(as.vector(test.label), pred.label, legacy.axes = TRUE, col = "blue", print.auc = TRUE,  
         print.auc.cex= .8, xlab = 'False Positive Rate', ylab = 'True Positive Rate')
plot.roc(as.vector(test.label), pred.label.over, legacy.axes = TRUE, col = "red", print.auc = TRUE,   
         print.auc.y = .4, print.auc.cex= .8, add = TRUE)
legend("bottomright", legend=c("Before Oversampling", "After Oversampling"), 
       col=c("blue", "red"), lwd=2, cex= .6)

## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE-----------------
#read.bibtex(file = "referenceOSTSC.bib")

