# metric for relative change
metric_relative <- function(x, y, z){

  if(sum(z) == 0){
    return(0)
  }

  if(is.numeric(x)){
    MLmetrics::MAPE(x[z], y[z])
  } else {
    sum(x[z] != y[z])/length(y[z])
  }

}