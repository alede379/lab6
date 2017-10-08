## ----include=FALSE-------------------------------------------------------
library(devtools)
install_github("alede379/lab6",force=TRUE)
library(lab6)

## ------------------------------------------------------------------------
set.seed(42)
n <- 2000
knapsack_objects <-data.frame(w=sample(1:4000, size = n, replace = TRUE),v=runif(n = n, 0, 10000))
system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))

## ------------------------------------------------------------------------
system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500,parallel = TRUE))

## ------------------------------------------------------------------------
system.time(knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500))

