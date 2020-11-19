# This script is an implementation of the Stochastic Gradient Descent
# Copyright 2020 @LuanAdemi

activation_func <- function(x) {
  # the activation function (ReLU)
  if (x <= 1) {
    return(0)
  } else {
    return(x)
  }
}

d_activation_func <- function(x) {
  # the derivative of the activation function
  if (x <= 1) {
    return(0)
  } else {
    return(1)
  }
}

neuron <- function(x) {
  # our "neuron" 
  # or the function with the parameters W and B we want to fit to data 
  return(activation_func(W * x + B))
}

MSE <- function(x,y) {
  # the mean squared error function
  return(1/2 * (y-x)**2)
}

dMSE <- function(x,y) {
  # the derivative of the mean squared error function
  return(x-y)
}

SGDstep <- function() {
  # iterate through the data
  for (r in 1:nrow(data)){
    Y <- neuron(data[r,1])
    
    # calculate the partial derivatives
    dJdY <- dMSE(Y, data[r,2])
    
    dYdW <- data[r,1] * d_activation_func(W * data[r,1] + B)
    dYdB <- d_activation_func(W * data[r,1] + B)
    
    # adjust the gradients
    gW <<- gW + dJdY * dYdW
    gB <<- gB + dJdY * dYdB
    
  }
  # update the parameters
  W <<- W - alpha * gW
  B <<- B - alpha * gB
}
