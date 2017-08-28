#' MHEALTH Dataset Data Set
#'
#' The MHEALTH (Mobile Health) dataset is devised to benchmark techniques dealing with human behavior 
#' analysis based on multimodal body sensing. comprises body motion and vital signs recordings for 
#' ten volunteers of diverse profile while performing several physical activities. Sensors placed on 
#' the subject's chest, right wrist and left ankle are used to measure the motion experienced by 
#' diverse body parts, namely, acceleration, rate of turn and magnetic field orientation. The sensor 
#' positioned on the chest also provides 2-lead ECG measurements, which can be potentially used for 
#' basic heart monitoring, checking for various arrhythmias or looking at the effects of exercise on 
#' the ECG.
#'
#' @format A time series data with multi-classes and multi-features. 
#' \describe{
#'   #Activities: 12 
#'   #Sensor devices: 3 
#'   #Subjects: 10 
#'   
#'   The activity set is listed in the following: 
#'   L1: Standing still (1 min) 
#'   L2: Sitting and relaxing (1 min) 
#'   L3: Lying down (1 min) 
#'   L4: Walking (1 min) 
#'   L5: Climbing stairs (1 min) 
#'   L6: Waist bends forward (20x) 
#'   L7: Frontal elevation of arms (20x) 
#'   L8: Knees bending (crouching) (20x) 
#'   L9: Cycling (1 min) 
#'   L10: Jogging (1 min) 
#'   L11: Running (1 min) 
#'   L12: Jump front & back (20x) 
#'   
#'   The meaning of each column is detailed next: 
#'   Column 1: acceleration from the chest sensor (X axis) 
#'   Column 2: acceleration from the chest sensor (Y axis) 
#'   Column 3: acceleration from the chest sensor (Z axis) 
#'   Column 4: electrocardiogram signal (lead 1) 
#'   Column 5: electrocardiogram signal (lead 2) 
#'   Column 6: acceleration from the left-ankle sensor (X axis) 
#'   Column 7: acceleration from the left-ankle sensor (Y axis) 
#'   Column 8: acceleration from the left-ankle sensor (Z axis) 
#'   Column 9: gyro from the left-ankle sensor (X axis) 
#'   Column 10: gyro from the left-ankle sensor (Y axis) 
#'   Column 11: gyro from the left-ankle sensor (Z axis) 
#'   Column 12: magnetometer from the left-ankle sensor (X axis) 
#'   Column 13: magnetometer from the left-ankle sensor (Y axis) 
#'   Column 14: magnetometer from the left-ankle sensor (Z axis) 
#'   Column 15: acceleration from the right-lower-arm sensor (X axis) 
#'   Column 16: acceleration from the right-lower-arm sensor (Y axis) 
#'   Column 17: acceleration from the right-lower-arm sensor (Z axis) 
#'   Column 18: gyro from the right-lower-arm sensor (X axis) 
#'   Column 19: gyro from the right-lower-arm sensor (Y axis) 
#'   Column 20: gyro from the right-lower-arm sensor (Z axis) 
#'   Column 21: magnetometer from the right-lower-arm sensor (X axis) 
#'   Column 22: magnetometer from the right-lower-arm sensor (Y axis) 
#'   Column 23: magnetometer from the right-lower-arm sensor (Z axis) 
#'   Column 24: Label (0 for the null class) 
#'   
#'   In this package, for a simple example displaying, only subject 1-5 and feature 5 (electrocardiogram 
#'   signal (lead 2)) are used, and the dataset is reformated to binary class. Class 11 is set as positive, 
#'   others as negative. The time series sequences length uses 30. Each sequence occurs in one line.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name MHEALTH
#' @usage data(MHEALTH)
#' @references Banos, O., Garcia, R., Holgado, J. A., Damas, M., Pomares, H., Rojas, I., Saez, A., Villalonga, C. 
#'             mHealthDroid: a novel framework for agile development of mobile health applications. 
#'             Proceedings of the 6th International Work-conference on Ambient Assisted Living an Active Ageing (IWAAL 2014), 
#'             Belfast, Northern Ireland, December 2-5, (2014).
#'             
#'             Banos, O., Villalonga, C., Garcia, R., Saez, A., Damas, M., Holgado, J. A., Lee, S., Pomares, H., Rojas, I. 
#'             Design, implementation and validation of a novel open framework for agile development of mobile health applications. 
#'             BioMedical Engineering OnLine, vol. 14, no. S2:S6, pp. 1-20 (2015).
#' @source \url{https://http://archive.ics.uci.edu/ml/datasets/mhealth+dataset}

dataset_MHEALTH <- function() {
  #load raw data
  mHealth_subject1 <- rio::import("https://github.com/lweicdsor/GSoC2017/raw/master/MHEALTH/mHealth_subject1.rda")
  mHealth_subject2 <- rio::import("https://github.com/lweicdsor/GSoC2017/raw/master/MHEALTH/mHealth_subject2.rda")
  mHealth_subject3 <- rio::import("https://github.com/lweicdsor/GSoC2017/raw/master/MHEALTH/mHealth_subject3.rda")
  mHealth_subject4 <- rio::import("https://github.com/lweicdsor/GSoC2017/raw/master/MHEALTH/mHealth_subject4.rda")
  mHealth_subject5 <- rio::import("https://github.com/lweicdsor/GSoC2017/raw/master/MHEALTH/mHealth_subject5.rda")
  mHealth_subject6 <- rio::import("https://github.com/lweicdsor/GSoC2017/raw/master/MHEALTH/mHealth_subject6.rda")
  mHealth_subject7 <- rio::import("https://github.com/lweicdsor/GSoC2017/raw/master/MHEALTH/mHealth_subject7.rda")
  mHealth_subject8 <- rio::import("https://github.com/lweicdsor/GSoC2017/raw/master/MHEALTH/mHealth_subject8.rda")
  mHealth_subject9 <- rio::import("https://github.com/lweicdsor/GSoC2017/raw/master/MHEALTH/mHealth_subject9.rda")
  mHealth_subject10 <- rio::import("https://github.com/lweicdsor/GSoC2017/raw/master/MHEALTH/mHealth_subject10.rda")
  # keep only feature 12
  mHealth_subject1 <- mHealth_subject1[, c(12, 24)]
  mHealth_subject2 <- mHealth_subject2[, c(12, 24)]
  mHealth_subject3 <- mHealth_subject3[, c(12, 24)]
  mHealth_subject4 <- mHealth_subject4[, c(12, 24)]
  mHealth_subject5 <- mHealth_subject5[, c(12, 24)]
  mHealth_subject6 <- mHealth_subject6[, c(12, 24)]
  mHealth_subject7 <- mHealth_subject7[, c(12, 24)]
  mHealth_subject8 <- mHealth_subject8[, c(12, 24)]
  mHealth_subject9 <- mHealth_subject9[, c(12, 24)]
  mHealth_subject10 <- mHealth_subject10[, c(12, 24)]
  # set class 11 as positive class
  p1 <- mHealth_subject1[which(mHealth_subject1[, c(2)] == 11), ][, c(1)]
  p2 <- mHealth_subject2[which(mHealth_subject2[, c(2)] == 11), ][, c(1)]
  p3 <- mHealth_subject3[which(mHealth_subject3[, c(2)] == 11), ][, c(1)]
  p4 <- mHealth_subject4[which(mHealth_subject4[, c(2)] == 11), ][, c(1)]
  p5 <- mHealth_subject5[which(mHealth_subject5[, c(2)] == 11), ][, c(1)]
  p6 <- mHealth_subject6[which(mHealth_subject6[, c(2)] == 11), ][, c(1)]
  p7 <- mHealth_subject7[which(mHealth_subject7[, c(2)] == 11), ][, c(1)]
  p8 <- mHealth_subject8[which(mHealth_subject8[, c(2)] == 11), ][, c(1)]
  p9 <- mHealth_subject9[which(mHealth_subject9[, c(2)] == 11), ][, c(1)]
  p10 <- mHealth_subject10[which(mHealth_subject10[, c(2)] == 11), ][, c(1)]
  # form time series sequences as length 30
  p1 <- matrix(p1[1:(30*round(length(p1)/30))], ncol=30, byrow=T)
  p2 <- matrix(p2[1:(30*round(length(p2)/30))], ncol=30, byrow=T)
  p3 <- matrix(p3[1:(30*round(length(p3)/30))], ncol=30, byrow=T)
  p4 <- matrix(p4[1:(30*round(length(p4)/30))], ncol=30, byrow=T)
  p5 <- matrix(p5[1:(30*round(length(p5)/30))], ncol=30, byrow=T)
  p6 <- matrix(p6[1:(30*round(length(p6)/30))], ncol=30, byrow=T)
  p7 <- matrix(p7[1:(30*round(length(p7)/30))], ncol=30, byrow=T)
  p8 <- matrix(p8[1:(30*round(length(p8)/30))], ncol=30, byrow=T)
  p9 <- matrix(p9[1:(30*round(length(p9)/30))], ncol=30, byrow=T)
  p10 <- matrix(p10[1:(30*round(length(p10)/30))],ncol=30, byrow=T)
  # p <- rbind(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
  # only use subject 1-5
  p <- rbind(p1, p2, p3, p4, p5)
  # set other classes as negative class
  n1 <- mHealth_subject1[which(mHealth_subject1[, c(2)] != 11), ][, c(1)]
  n2 <- mHealth_subject2[which(mHealth_subject2[, c(2)] != 11), ][, c(1)]
  n3 <- mHealth_subject3[which(mHealth_subject3[, c(2)] != 11), ][, c(1)]
  n4 <- mHealth_subject4[which(mHealth_subject4[, c(2)] != 11), ][, c(1)]
  n5 <- mHealth_subject5[which(mHealth_subject5[, c(2)] != 11), ][, c(1)]
  n6 <- mHealth_subject6[which(mHealth_subject6[, c(2)] != 11), ][, c(1)]
  n7 <- mHealth_subject7[which(mHealth_subject7[, c(2)] != 11), ][, c(1)]
  n8 <- mHealth_subject8[which(mHealth_subject8[, c(2)] != 11), ][, c(1)]
  n9 <- mHealth_subject9[which(mHealth_subject9[, c(2)] != 11), ][, c(1)]
  n10 <- mHealth_subject10[which(mHealth_subject10[, c(2)] != 11), ][, c(1)]
  # form sequences
  n1 <- matrix(n1[1:(30*round(length(n1)/30))], ncol=30, byrow=T)
  n2 <- matrix(n2[1:(30*round(length(n2)/30))], ncol=30, byrow=T)
  n3 <- matrix(n3[1:(30*round(length(n3)/30))], ncol=30, byrow=T)
  n4 <- matrix(n4[1:(30*round(length(n4)/30))], ncol=30, byrow=T)
  n5 <- matrix(n5[1:(30*round(length(n5)/30))], ncol=30, byrow=T)
  n6 <- matrix(n6[1:(30*round(length(n6)/30))], ncol=30, byrow=T)
  n7 <- matrix(n7[1:(30*round(length(n7)/30))], ncol=30, byrow=T)
  n8 <- matrix(n8[1:(30*round(length(n8)/30))], ncol=30, byrow=T)
  n9 <- matrix(n9[1:(30*round(length(n9)/30))], ncol=30, byrow=T)
  n10 <- matrix(n10[1:(30*round(length(n10)/30))], ncol=30, byrow=T)
  #n <- rbind(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10)
  # only use subject 1-5
  n <- rbind(n1, n2, n3, n4, n5)
  #remove NA
  p <- matrix(suppressWarnings(as.numeric(p)), nrow = nrow(p))
  p <- na.omit(p)
  n <- matrix(suppressWarnings(as.numeric(n)), nrow = nrow(n))
  n <- na.omit(n)
  #split training and testing data
  num_p <- round(nrow(p)/2)
  num_n <- round(nrow(n)/2)
  train_x <- rbind(p[1:num_p, ], n[1:num_n, ])
  test_x <- rbind(p[(num_p+1):nrow(p), ], n[(num_n+1):nrow(n), ])
  train_y <- rbind(matrix(1, num_p,1), matrix(0, num_n,1))
  test_y <- rbind(matrix(1, nrow(p)-num_p, 1), matrix(0, nrow(n)-num_n, 1))
  # form data
  MHEALTH <- list("train_x" = train_x, "train_y" = train_y, "test_x" = test_x, "test_y" = test_y)
  # save data
  devtools::use_data(MHEALTH)
}
