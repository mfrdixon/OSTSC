#' @title The Electrical Devices dataset.
#'
#' @description This dataset is taken from research Powering the Nation. 
#' @details Powering the Nation collected behavioural data about how consumers use electricity within the home to help reduce the UK's carbon footprint. 
#'
#' @format A dataset with 5527 observations of sequence length 96, with a single sequence per row. 
#' \describe{
#'   The y data is labeled as {5,6}.
#'   
#'   The x data constructs time series sequences (numeric).
#' }
#' 
#' @references Lines J., Bagnall A., Caiger-Smith P., Anderson S. (2011) Classification of Household Devices by Electricity Usage Profiles. 
#'             In: Yin H., Wang W., Rayward-Smith V. (eds) Intelligent Data Engineering and Automated Learning - IDEAL 2011. IDEAL 2011. 
#'             Lecture Notes in Computer Science, vol 6936. Springer, Berlin, Heidelberg.
#'             
#' @return eds: the dataset ElectricalDevices
#' @export Dataset_ElectricalDevices

Dataset_ElectricalDevices <- function(){
  eds <- local(get(load(url('https://github.com/lweicdsor/GSoC2017/raw/master/ElectricDevices/Dataset_ElectricalDevices.rdata'))))
  return (eds)
}
