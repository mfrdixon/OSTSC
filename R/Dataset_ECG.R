#' @title The Electrocardiogram dataset.
#'
#' @description This dataset recorded heartbeats from the patient with severe congestive heart failure.
#' @details The dataset was pre-processed on extracting heartbeats sequences and setting class values from automated annotation. 
#'
#' @format A dataset with 5000 observations of sequence length 140, with a single sequence per row. 
#' \describe{
#'   The y data is labeled as {1,3,4,5}.
#'   
#'   The x data constructs time series sequences (numeric).
#' }
#' 
#' @references Goldberger AL, Amaral LAN, Glass L, Hausdorff JM, Ivanov PCh, Mark RG, Mietus JE, Moody GB, Peng CK, Stanley HE. 
#'             PhysioBank, PhysioToolkit, and PhysioNet: Components of a New Research Resource for Complex Physiologic Signals. 
#'             Circulation 101(23):e215-e220. [Circulation Electronic Pages; http://circ.ahajournals.org/content/101/23/e215.full]; 
#'             2000 (June 13). PMID: 10851218; doi: 10.1161/01.CIR.101.23.e215
#'             
#'             Chen, Y., Hao, Y., Rakthanmanon, T. et al. Data Min Knowl Disc (2015) 29: 1622. https://doi.org/10.1007/s10618-014-0388-4
#'             
#' @return ecg: the dataset ECG
#' @export Dataset_ECG

Dataset_ECG <- function(){
  ecg <- local(get(load(url('https://github.com/lweicdsor/GSoC2017/raw/master/ECG/Dataset_ECG.rdata'))))
  return (ecg)
}