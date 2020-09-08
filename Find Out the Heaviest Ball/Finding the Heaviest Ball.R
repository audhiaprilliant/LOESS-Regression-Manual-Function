# ===== FINDING THE HEAVIEST ONE =====

# BUILD 27 BALLS WITH 1 DIFFERENT WEIGHT
x = rep(x = 1,
        length.out = 27) # Build 27 balls whose weight is 1
x[sample(x = 1:length(x),
         size = 1)] = 1.3 # Choose 1 ball and assign it with weight 1.1

# ALGORITHM
recursive.function = function(data.split) {
  if (length(data.split) %% 3 == 0) {
    list.number = split(x = 1:length(data.split),
                        f = 1:3)
    if (sum(data.split[list.number[[1]]]) == sum(data.split[list.number[[2]]])) {
      recursive.function(data.split = data.split[list.number[[3]]])
    }
    else {
      if (sum(data.split[list.number[[1]]]) > sum(data.split[list.number[[2]]])) {
        recursive.function(data.split[list.number[[1]]])
      }
      else {
        recursive.function(data.split[list.number[[2]]])
      }
    }
  }
  else {
    return(data.split)
  }
}
choose.ball = function(data.ball) {
  number = recursive.function(data.split = data.ball)
  result.list = as.data.frame(list('Weight' = number,
                                   'Index' = which(data.ball == number)))
  return(result.list)
}

choose.ball(data.ball = x)