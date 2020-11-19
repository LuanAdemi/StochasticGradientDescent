# import the SGD script
source('~/Schreibtisch/Programmieren/StochasticGradientDescent/scripts/functions/sgd.R')

# the data matrix
data = matrix(
  c(0, 1, 20, 5),
  nrow=2,
  ncol=2,
  byrow=TRUE
)

# the parameters
B <- 2
W <- 0

# the learning rate
alpha <- 0.001

# a vector for storing the loss values
loss = c()

# main loop
for(i in seq(1,2500, by=1)) {
  # the gradients
  gW <- 0
  gB <- 0
  SGDstep()
  loss <- append(loss, MSE(neuron(data[1,1]),data[1,2]))
}

# plot the loss curve
x <- 1:length(loss)
plot(x, loss, type="l")


