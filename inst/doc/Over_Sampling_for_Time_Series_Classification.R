## ---- echo=FALSE, message=FALSE------------------------------------------
require(OSTSC)
require(keras)
require(pROC)

## ------------------------------------------------------------------------
library(OSTSC)
data(dataset_synthetic_control)

train_label <- dataset_synthetic_control$train_y
train_sample <- dataset_synthetic_control$train_x
test_label <- dataset_synthetic_control$test_y
test_sample <- dataset_synthetic_control$test_x

## ------------------------------------------------------------------------
table(train_label)

## ---- results='hide'-----------------------------------------------------
MyData <- OSTSC(train_sample, train_label, target_class = 1)
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

## ---- message=FALSE------------------------------------------------------
model = keras_model_sequential()
model %>%
  layer_lstm(10, input_shape = c(60, 1)) %>%
  layer_dense(2) %>%
  layer_activation("softmax")
model %>% compile(
  loss = "categorical_crossentropy", 
  optimizer = "adam", 
  metrics = "accuracy"
)
lstm_before <- model %>% fit( 
  x = train_x, 
  y = train_y, 
  validation_split = 0.1,
  epochs = 20
)
plot(lstm_before)

## ---- message=FALSE------------------------------------------------------
score <- model %>% evaluate(test_x, test_y)

## ---- echo=FALSE---------------------------------------------------------
cat("The loss value is", unlist(score[1]), ".\n")
cat("The metric value (in this case 'accuracy') is", unlist(score[2]), ".\n")

## ------------------------------------------------------------------------
over_y <- to_categorical(over_label)
over_x <- array(over_sample, dim = c(dim(over_sample),1)) 

## ---- message=FALSE------------------------------------------------------
model_over = keras_model_sequential()
model_over %>%
  layer_lstm(10, input_shape = c(60, 1)) %>%
  layer_dense(2) %>%
  layer_activation("softmax")
model_over %>% compile(
  loss = "categorical_crossentropy", 
  optimizer = "adam", 
  metrics = "accuracy"
)
lstm_after <- model_over %>% fit( 
  x = over_x, 
  y = over_y, 
  validation_split = 0.1,
  epochs = 20
)
plot(lstm_after)

## ---- message=FALSE------------------------------------------------------
score_over <- model_over %>% evaluate(test_x, test_y)

## ---- echo=FALSE---------------------------------------------------------
cat("The loss value is", unlist(score_over[1]), ".\n")
cat("The metric value (in this case 'accuracy') is", unlist(score_over[2]), ".\n")

## ------------------------------------------------------------------------
pred_label <- model %>% predict_classes(test_x)
pred_label_over <- model_over %>% predict_classes(test_x)
cm_before <- table(test_label, pred_label)
cm_after <- table(test_label, pred_label_over)

## ---- echo=FALSE---------------------------------------------------------
cat("The confusion matrix before oversampling: \n") 
cm_before
cat("The confusion matrix after oversampling: \n") 
cm_after

## ------------------------------------------------------------------------
plot.roc(test_label, pred_label, legacy.axes = TRUE, col = "blue", print.auc = TRUE,  
         main="ROC", print.auc.cex= .8)
plot.roc(test_label, pred_label_over, legacy.axes = TRUE, col = "red", print.auc = TRUE,   
         print.auc.y = .4, print.auc.cex= .8, add = TRUE)
legend("bottomright", legend=c("Before Oversampling", "After Oversampling"), 
       col=c("blue", "red"), lwd=2, cex= .6)

