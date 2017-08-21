## ---- echo=FALSE, message=FALSE------------------------------------------
require(OSTSC)
require(mxnet)
require(caret)
require(e1071)
require(pROC)

## ------------------------------------------------------------------------
library(OSTSC)
data(synthetic_control_TRAIN)
data(synthetic_control_TEST)

train_label <- synthetic_control_TRAIN[, c(1)]
train_sample <- synthetic_control_TRAIN[, -1]
test_label <- synthetic_control_TEST[, c(1)]
test_sample <- synthetic_control_TEST[, -1]

## ------------------------------------------------------------------------
table(train_label)

## ---- results='hide'-----------------------------------------------------
MyData <- OSTSC(train_sample, train_label, target_class = 1, parallel = FALSE)
over_sample <- MyData$sample
over_label <- MyData$label

## ------------------------------------------------------------------------
table(over_label)

## ---- message=FALSE, warning=FALSE---------------------------------------
mx.set.seed(0)
model <- mx.mlp(as.matrix(train_sample), train_label, out_activation="softmax", 
                hidden_node=10, out_node=2, num.round=10, array.batch.size=15, 
                learning.rate=0.07, momentum=0.9, eval.metric=mx.metric.accuracy)
preds <- predict(model, as.matrix(test_sample))
pred_label <- max.col(t(preds))-1
cm <- table(pred_label, test_label)

## ---- message=FALSE, warning=FALSE---------------------------------------
mx.set.seed(0)
model_over <- mx.mlp(over_sample, over_label, out_activation="softmax", hidden_node=10, 
                     out_node=2, num.round=10, array.batch.size=15, learning.rate=0.07,  
                     momentum=0.9, eval.metric=mx.metric.accuracy)
preds_over <- predict(model_over, as.matrix(test_sample))
pred_label_over <- max.col(t(preds_over))-1
cm_over <- table(pred_label_over, test_label)

## ------------------------------------------------------------------------
fourfoldplot(cm, color = c("#FF0000", "#0000FF"), conf.level = 0, margin = 1, 
             main = "Confusion Matrix (Before Oversampling)")

## ---- echo=FALSE---------------------------------------------------------
cm

## ------------------------------------------------------------------------
fourfoldplot(cm_over, color = c("#FF0000", "#0000FF"), conf.level = 0, margin = 1, 
             main = "Confusion Matrix (After Oversampling)")

## ---- echo=FALSE---------------------------------------------------------
cm_over

## ------------------------------------------------------------------------
auc(roc(test_label, pred_label))

## ------------------------------------------------------------------------
auc(roc(test_label, pred_label_over))

## ------------------------------------------------------------------------
plot.roc(test_label, pred_label, legacy.axes = TRUE, col = "blue", print.auc = TRUE,  
         main="ROC", print.auc.cex= .8)
plot.roc(test_label, pred_label_over, legacy.axes = TRUE, col = "red", print.auc = TRUE,   
         print.auc.y = .4, print.auc.cex= .8, add = TRUE)
legend("bottomright", legend=c("Before Oversampling", "After Oversampling"), 
       col=c("blue", "red"), lwd=2, cex= .6)

