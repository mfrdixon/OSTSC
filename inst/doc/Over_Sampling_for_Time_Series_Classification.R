## ---- echo=FALSE, message=FALSE------------------------------------------
require(OSTSC)
require(keras)
require(rlist)
require(dummies)
require(pROC)

## ------------------------------------------------------------------------
library(OSTSC)
data(synthetic_control)

train_label <- synthetic_control$train_y
train_sample <- synthetic_control$train_x
test_label <- synthetic_control$test_y
test_sample <- synthetic_control$test_x

## ------------------------------------------------------------------------
table(train_label)

## ---- results='hide'-----------------------------------------------------
MyData <- OSTSC(train_sample, train_label, k = 4)
over_sample <- MyData$sample
over_label <- MyData$label

## ------------------------------------------------------------------------
table(over_label)

## ------------------------------------------------------------------------
library(keras)
train_y <- to_categorical(train_label)
test_y <- to_categorical(test_label)
train_x <- array(train_sample, dim = c(dim(train_sample),1)) 
test_x <- array(test_sample, dim = c(dim(test_sample),1)) 

## ---- eval=FALSE---------------------------------------------------------
#  model = keras_model_sequential()
#  model %>%
#    layer_lstm(10, input_shape = c(dim(train_x)[2], dim(train_x)[3])) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_dense(dim(train_y)[2]) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_activation("softmax")
#  model %>% compile(
#    loss = "categorical_crossentropy",
#    optimizer = "adam",
#    metrics = "accuracy"
#  )
#  lstm_before <- model %>% fit(
#    x = train_x,
#    y = train_y,
#    validation_split = 0.1,
#    epochs = 200
#  )
#  plot(lstm_before)

## ---- echo=FALSE, message=FALSE------------------------------------------
score <- rlist::list.load('/home/lwei/sc_before_score.rdata')
lstm_before <- rlist::list.load('/home/lwei/sc_before_e200.rdata')
pred_label <- rlist::list.load('/home/lwei/sc_before_predlabel.rdata')
pred_label <- as.vector(unlist(pred_label))
plot(lstm_before)

## ---- eval=FALSE---------------------------------------------------------
#  score <- model %>% evaluate(test_x, test_y)

## ---- echo=FALSE---------------------------------------------------------
cat("The loss value is", unlist(score[1]), ".\n")
cat("The metric value (in this case 'accuracy') is", unlist(score[2]), ".\n")

## ------------------------------------------------------------------------
over_y <- to_categorical(over_label)
over_x <- array(over_sample, dim = c(dim(over_sample),1)) 

## ---- eval=FALSE---------------------------------------------------------
#  model_over = keras_model_sequential()
#  model_over %>%
#    layer_lstm(10, input_shape = c(dim(over_x)[2], dim(over_x)[3])) %>%
#    layer_dropout(rate = 0.1) %>%
#    layer_dense(dim(over_y)[2]) %>%
#    layer_dropout(rate = 0.1) %>%
#    layer_activation("softmax")
#  model_over %>% compile(
#    loss = "categorical_crossentropy",
#    optimizer = "adam",
#    metrics = "accuracy"
#  )
#  lstm_after <- model_over %>% fit(
#    x = over_x,
#    y = over_y,
#    validation_split = 0.1,
#    epochs = 200
#  )
#  plot(lstm_after)

## ---- echo=FALSE, message=FALSE------------------------------------------
score_over <- rlist::list.load('/home/lwei/sc_after_score.rdata')
lstm_after <- rlist::list.load('/home/lwei/sc_after_e200.rdata')
pred_label_over <- rlist::list.load('/home/lwei/sc_after_predlabel.rdata')
pred_label_over <- as.vector(unlist(pred_label_over))
plot(lstm_after)

## ---- eval=FALSE---------------------------------------------------------
#  score_over <- model_over %>% evaluate(test_x, test_y)

## ---- echo=FALSE---------------------------------------------------------
cat("The loss value is", unlist(score_over[1]), ".\n")
cat("The metric value (in this case 'accuracy') is", unlist(score_over[2]), ".\n")

## ---- eval=FALSE---------------------------------------------------------
#  pred_label <- model %>% predict_classes(test_x)
#  pred_label_over <- model_over %>% predict_classes(test_x)

## ------------------------------------------------------------------------
cm_before <- table(test_label, pred_label)
cm_after <- table(test_label, pred_label_over)

## ---- echo=FALSE---------------------------------------------------------
layout(matrix(c(1,1,2)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
title('Normalized Confusion Matrix (before oversampling)', cex.main=1.75)

rect(150, 430, 240, 370, col='#3F97D0')
rect(250, 430, 340, 370, col='#F7AD50')
rect(150, 305, 240, 365, col='#F7AD50')
rect(250, 305, 340, 365, col='#3F97D0')
text(195, 435, '0', cex=1.2)
text(295, 435, '1', cex=1.2)
text(125, 370, 'True', cex=1.3, srt=90, font=2)
text(245, 450, 'Predicted', cex=1.3, font=2)
text(140, 400, '0', cex=1.2, srt=90)
text(140, 335, '1', cex=1.2, srt=90)

res <- as.numeric(cm_before)
sum1 <- res[1] + res[3]
sum2 <- res[2] + res[4] 
text(195, 400, round(res[1]/sum1, 4), cex=1.6, font=2, col='white')
text(195, 335, round(res[2]/sum2, 4), cex=1.6, font=2, col='white')
text(295, 400, round(res[3]/sum1, 4), cex=1.6, font=2, col='white')
text(295, 335, round(res[4]/sum2, 4), cex=1.6, font=2, col='white')

layout(matrix(c(1,1,2)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
title('Normalized Confusion Matrix (after oversampling)', cex.main=1.75)

rect(150, 430, 240, 370, col='#3F97D0')
rect(250, 430, 340, 370, col='#F7AD50')
rect(150, 305, 240, 365, col='#F7AD50')
rect(250, 305, 340, 365, col='#3F97D0')
text(195, 435, '0', cex=1.2)
text(295, 435, '1', cex=1.2)
text(125, 370, 'True', cex=1.3, srt=90, font=2)
text(245, 450, 'Predicted', cex=1.3, font=2)
text(140, 400, '0', cex=1.2, srt=90)
text(140, 335, '1', cex=1.2, srt=90)

res <- as.numeric(cm_after)
sum1 <- res[1] + res[3]
sum2 <- res[2] + res[4] 
text(195, 400, round(res[1]/sum1, 4), cex=1.6, font=2, col='white')
text(195, 335, round(res[2]/sum2, 4), cex=1.6, font=2, col='white')
text(295, 400, round(res[3]/sum1, 4), cex=1.6, font=2, col='white')
text(295, 335, round(res[4]/sum2, 4), cex=1.6, font=2, col='white')

## ------------------------------------------------------------------------
library(pROC)
plot.roc(test_label, pred_label, legacy.axes = TRUE, col = "blue", print.auc = TRUE,  
         print.auc.cex= .8, xlab = 'False Positive Rate', ylab = 'True Positive Rate', 
         main="ROC synthetic_control")
plot.roc(test_label, pred_label_over, legacy.axes = TRUE, col = "red", print.auc = TRUE,   
         print.auc.y = .4, print.auc.cex= .8, add = TRUE)
legend("bottomright", legend=c("Before Oversampling", "After Oversampling"), 
       col=c("blue", "red"), lwd=2, cex= .6)

## ------------------------------------------------------------------------
data(MHEALTH)

train_label <- MHEALTH$train_y
train_sample <- MHEALTH$train_x
test_label <- MHEALTH$test_y
test_sample <- MHEALTH$test_x

## ------------------------------------------------------------------------
table(train_label)

## ---- results='hide'-----------------------------------------------------
MyData <- OSTSC(train_sample, train_label)
over_sample <- MyData$sample
over_label <- MyData$label

table(over_label)

## ------------------------------------------------------------------------
library(keras)
train_y <- to_categorical(train_label)
test_y <- to_categorical(test_label)
train_x <- array(train_sample, dim = c(dim(train_sample),1)) 
test_x <- array(test_sample, dim = c(dim(test_sample),1)) 

## ---- eval=FALSE---------------------------------------------------------
#  model = keras_model_sequential()
#  model %>%
#    layer_lstm(10, input_shape = c(dim(train_x)[2], dim(train_x)[3])) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_dense(dim(train_y)[2]) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_activation("softmax")
#  model %>% compile(
#    loss = "categorical_crossentropy",
#    optimizer = "adam",
#    metrics = "accuracy"
#  )
#  lstm_before <- model %>% fit(
#    x = train_x,
#    y = train_y,
#    validation_split = 0.2,
#    epochs = 200
#  )
#  plot(lstm_before)

## ---- echo=FALSE, message=FALSE------------------------------------------
lstm_before <- rlist::list.load('/home/lwei/mhealth_before_e200.rdata')
pred_label <- rlist::list.load('/home/lwei/mhealth_before_predlabel.rdata')
pred_label <- as.vector(unlist(pred_label))
plot(lstm_before)

## ------------------------------------------------------------------------
over_y <- to_categorical(over_label)
over_x <- array(over_sample, dim = c(dim(over_sample),1)) 

## ---- eval=FALSE---------------------------------------------------------
#  model_over = keras_model_sequential()
#  model_over %>%
#    layer_lstm(10, input_shape = c(dim(over_x)[2], dim(over_x)[3])) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_dense(dim(over_y)[2]) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_activation("softmax")
#  model_over %>% compile(
#    loss = "categorical_crossentropy",
#    optimizer = "adam",
#    metrics = "accuracy"
#  )
#  lstm_after <- model_over %>% fit(
#    x = over_x,
#    y = over_y,
#    validation_split = 0.1,
#    epochs = 200
#  )
#  plot(lstm_after)

## ---- echo=FALSE, message=FALSE------------------------------------------
lstm_after <- rlist::list.load('/home/lwei/mhealth_after_e200.rdata')
pred_label_over <- rlist::list.load('/home/lwei/mhealth_after_predlabel.rdata')
pred_label_over <- as.vector(unlist(pred_label_over))
plot(lstm_after)

## ---- eval=FALSE---------------------------------------------------------
#  pred_label <- model %>% predict_classes(test_x)
#  pred_label_over <- model_over %>% predict_classes(test_x)

## ------------------------------------------------------------------------
cm_before <- table(test_label, pred_label)
cm_after <- table(test_label, pred_label_over)

## ---- echo=FALSE---------------------------------------------------------
layout(matrix(c(1,1,2)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
title('Normalized Confusion Matrix (before oversampling)', cex.main=1.75)

rect(150, 430, 240, 370, col='#3F97D0')
rect(250, 430, 340, 370, col='#F7AD50')
rect(150, 305, 240, 365, col='#F7AD50')
rect(250, 305, 340, 365, col='#3F97D0')
text(195, 435, '0', cex=1.2)
text(295, 435, '1', cex=1.2)
text(125, 370, 'True', cex=1.3, srt=90, font=2)
text(245, 450, 'Predicted', cex=1.3, font=2)
text(140, 400, '0', cex=1.2, srt=90)
text(140, 335, '1', cex=1.2, srt=90)

res <- as.numeric(cm_before)
sum1 <- res[1] + res[3]
sum2 <- res[2] + res[4] 
text(195, 400, round(res[1]/sum1, 4), cex=1.6, font=2, col='white')
text(195, 335, round(res[2]/sum2, 4), cex=1.6, font=2, col='white')
text(295, 400, round(res[3]/sum1, 4), cex=1.6, font=2, col='white')
text(295, 335, round(res[4]/sum2, 4), cex=1.6, font=2, col='white')

layout(matrix(c(1,1,2)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
title('Normalized Confusion Matrix (after oversampling)', cex.main=1.75)

rect(150, 430, 240, 370, col='#3F97D0')
rect(250, 430, 340, 370, col='#F7AD50')
rect(150, 305, 240, 365, col='#F7AD50')
rect(250, 305, 340, 365, col='#3F97D0')
text(195, 435, '0', cex=1.2)
text(295, 435, '1', cex=1.2)
text(125, 370, 'True', cex=1.3, srt=90, font=2)
text(245, 450, 'Predicted', cex=1.3, font=2)
text(140, 400, '0', cex=1.2, srt=90)
text(140, 335, '1', cex=1.2, srt=90)

res <- as.numeric(cm_after)
sum1 <- res[1] + res[3]
sum2 <- res[2] + res[4] 
text(195, 400, round(res[1]/sum1, 4), cex=1.6, font=2, col='white')
text(195, 335, round(res[2]/sum2, 4), cex=1.6, font=2, col='white')
text(295, 400, round(res[3]/sum1, 4), cex=1.6, font=2, col='white')
text(295, 335, round(res[4]/sum2, 4), cex=1.6, font=2, col='white')

## ---- warning=FALSE------------------------------------------------------
library(pROC)
plot.roc(as.vector(test_label), pred_label, legacy.axes = TRUE, col = "blue", 
         print.auc = TRUE, print.auc.cex= .8, xlab = 'False Positive Rate', 
         ylab = 'True Positive Rate', main="ROC MHEALTH")
plot.roc(as.vector(test_label), pred_label_over, legacy.axes = TRUE, col = "red", 
         print.auc = TRUE, print.auc.y = .4, print.auc.cex= .8, add = TRUE)
legend("bottomright", legend=c("Before Oversampling", "After Oversampling"), 
       col=c("blue", "red"), lwd=2, cex= .6)

## ------------------------------------------------------------------------
data(HFT)

label <- HFT$y
sample <- HFT$x
train_label <- label[1:15000]
train_sample <- sample[1:15000, ]
test_label <- label[15001:30000]
test_sample <- sample[15001:30000, ]

## ------------------------------------------------------------------------
table(train_label)

## ---- results='hide'-----------------------------------------------------
MyData <- OSTSC(train_sample, train_label)
over_sample <- MyData$sample
over_label <- MyData$label

## ------------------------------------------------------------------------
table(over_label)

## ------------------------------------------------------------------------
library(keras)
library(dummies)
train_y <- dummy(train_label)
test_y <- dummy(test_label)
train_x <- array(train_sample, dim = c(dim(train_sample),1)) 
test_x <- array(test_sample, dim = c(dim(test_sample),1)) 

## ---- eval=FALSE---------------------------------------------------------
#  model = keras_model_sequential()
#  model %>%
#    layer_lstm(10, input_shape = c(dim(train_x)[2], dim(train_x)[3])) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_dense(dim(train_y)[2]) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_activation("softmax")
#  model %>% compile(
#    loss = "categorical_crossentropy",
#    optimizer = "adam",
#    metrics = "accuracy"
#  )
#  lstm_before <- model %>% fit(
#    x = train_x,
#    y = train_y,
#    validation_split = 0.1,
#    epochs = 200
#  )
#  plot(lstm_before)

## ---- echo=FALSE, message=FALSE------------------------------------------
lstm_before <- rlist::list.load('/home/lwei/HFT_before_e200.rdata')
pred_label <- rlist::list.load('/home/lwei/HFT_before_predlabel.rdata')
pred_label <- as.vector(unlist(pred_label))
plot(lstm_before)

## ------------------------------------------------------------------------
over_y <- dummy(over_label)
over_x <- array(over_sample, dim = c(dim(over_sample),1)) 

## ---- eval=FALSE---------------------------------------------------------
#  model_over = keras_model_sequential()
#  model_over %>%
#    layer_lstm(10, input_shape = c(dim(over_x)[2], dim(over_x)[3])) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_dense(dim(over_y)[2]) %>%
#    layer_dropout(rate = 0.2) %>%
#    layer_activation("softmax")
#  model_over %>% compile(
#    loss = "categorical_crossentropy",
#    optimizer = "adam",
#    metrics = "accuracy"
#  )
#  lstm_after <- model_over %>% fit(
#    x = over_x,
#    y = over_y,
#    validation_split = 0.1,
#    epochs = 200
#  )
#  plot(lstm_after)

## ---- echo=FALSE, message=FALSE------------------------------------------
lstm_after <- rlist::list.load('/home/lwei/HFT_after_e200.rdata')
pred_label_over <- rlist::list.load('/home/lwei/HFT_after_predlabel.rdata')
pred_label_over <- as.vector(unlist(pred_label_over))
plot(lstm_after)

## ---- eval=FALSE---------------------------------------------------------
#  pred_label <- model %>% predict_classes(test_x)
#  pred_label_over <- model_over %>% predict_classes(test_x)

## ------------------------------------------------------------------------
cm_before <- table(test_label, pred_label)
cm_after <- table(test_label, pred_label_over)

## ---- echo=FALSE---------------------------------------------------------
layout(matrix(c(1,1,2)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
title('Normalized Confusion Matrix (before oversampling)', cex.main=1.75)

rect(140, 430, 200, 390, col='#3F97D0')
rect(210, 430, 270, 390, col='#F7AD50')
rect(280, 430, 340, 390, col='#F7AD50')
rect(140, 345, 200, 385, col='#F7AD50')
rect(210, 345, 270, 385, col='#3F97D0')
rect(280, 345, 340, 385, col='#F7AD50')
rect(140, 300, 200, 340, col='#F7AD50')
rect(210, 300, 270, 340, col='#F7AD50')
rect(280, 300, 340, 340, col='#3F97D0')
text(170, 435, '-1', cex=1.2)
text(240, 435, '0', cex=1.2)
text(310, 435, '1', cex=1.2)
text(130, 410, '-1', cex=1.2, srt=90)
text(130, 365, '0', cex=1.2, srt=90)
text(130, 320, '1', cex=1.2, srt=90)
text(120, 370, 'True', cex=1.3, srt=90, font=2)
text(240, 450, 'Predicted', cex=1.3, font=2)
  
res <- as.numeric(cm_before)
sum1 <- res[1] + res[4] + res[7]
sum2 <- res[2] + res[5] + res[8]
sum3 <- res[3] + res[6] + res[9]
text(170, 410, round(res[1]/sum1, 4), cex=1.6, font=2, col='white')
text(170, 365, round(res[2]/sum2, 4), cex=1.6, font=2, col='white')
text(170, 320, round(res[3]/sum3, 4), cex=1.6, font=2, col='white')
text(240, 410, round(res[4]/sum1, 4), cex=1.6, font=2, col='white')
text(240, 365, round(res[5]/sum2, 4), cex=1.6, font=2, col='white')
text(240, 320, round(res[6]/sum3, 4), cex=1.6, font=2, col='white')
text(310, 410, round(res[7]/sum1, 4), cex=1.6, font=2, col='white')
text(310, 365, round(res[8]/sum2, 4), cex=1.6, font=2, col='white')
text(310, 320, round(res[9]/sum3, 4), cex=1.6, font=2, col='white')

layout(matrix(c(1,1,2)))
par(mar=c(2,2,2,2))
plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
title('Normalized Confusion Matrix (after oversampling)', cex.main=1.75)

rect(140, 430, 200, 390, col='#3F97D0')
rect(210, 430, 270, 390, col='#F7AD50')
rect(280, 430, 340, 390, col='#F7AD50')
rect(140, 345, 200, 385, col='#F7AD50')
rect(210, 345, 270, 385, col='#3F97D0')
rect(280, 345, 340, 385, col='#F7AD50')
rect(140, 300, 200, 340, col='#F7AD50')
rect(210, 300, 270, 340, col='#F7AD50')
rect(280, 300, 340, 340, col='#3F97D0')
text(170, 435, '-1', cex=1.2)
text(240, 435, '0', cex=1.2)
text(310, 435, '1', cex=1.2)
text(130, 410, '-1', cex=1.2, srt=90)
text(130, 365, '0', cex=1.2, srt=90)
text(130, 320, '1', cex=1.2, srt=90)
text(120, 370, 'True', cex=1.3, srt=90, font=2)
text(240, 450, 'Predicted', cex=1.3, font=2)

res <- as.numeric(cm_after)
sum1 <- res[1] + res[4] + res[7]
sum2 <- res[2] + res[5] + res[8]
sum3 <- res[3] + res[6] + res[9]
text(170, 410, round(res[1]/sum1, 4), cex=1.6, font=2, col='white')
text(170, 365, round(res[2]/sum2, 4), cex=1.6, font=2, col='white')
text(170, 320, round(res[3]/sum3, 4), cex=1.6, font=2, col='white')
text(240, 410, round(res[4]/sum1, 4), cex=1.6, font=2, col='white')
text(240, 365, round(res[5]/sum2, 4), cex=1.6, font=2, col='white')
text(240, 320, round(res[6]/sum3, 4), cex=1.6, font=2, col='white')
text(310, 410, round(res[7]/sum1, 4), cex=1.6, font=2, col='white')
text(310, 365, round(res[8]/sum2, 4), cex=1.6, font=2, col='white')
text(310, 320, round(res[9]/sum3, 4), cex=1.6, font=2, col='white')

## ---- warning=FALSE------------------------------------------------------
library(pROC)
plot.roc(test_label, pred_label, legacy.axes = TRUE, col = "blue", print.auc = TRUE,  
         print.auc.cex= .8, xlab = 'False Positive Rate', ylab = 'True Positive Rate', 
         main="ROC HFT")
plot.roc(test_label, pred_label_over, legacy.axes = TRUE, col = "red", print.auc = TRUE,   
         print.auc.y = .4, print.auc.cex= .8, add = TRUE)
legend("bottomright", legend=c("Before Oversampling", "After Oversampling"), 
       col=c("blue", "red"), lwd=2, cex= .6)

