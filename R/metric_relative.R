# metric for relative change
# x is pred, y is true
metric_relative <- function(x, y, z){

  if(sum(z) == 0){
    return(0)
  }

  if(is.numeric(x)){
    mean(abs((y[z] - x[z])/y[z]))
  } else {
    sum(x[z] != y[z])/sum(z)
  }

}