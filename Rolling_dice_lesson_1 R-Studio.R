## first days, creating a dice rolling game. 
roll2<- function(bones = 1:6) {
  dice <- sample(bones, size =2, replace = TRUE)
  sum(dice) }
roll2()
## using qplot
library("ggplot2")
x <- c(1,2,3,4,5,6,7,8)
y <- c(-1,-2,-3,-4,-5,-6,-7,-8)
y=-1*y
x = x^3 
qplot(x,y)
# histogram
x = c(1,1,2,2,2,3,4,4,1)
qplot (x, binwidth = 1 )
qplot(x, binwidth= 1)

x3<- c(0,1,1,2,2,2,3,3,4)
qplot (x3, binwidth = 1)

## replicating functions or equations. replicate (# of times, bind width)
 replicate(3, 1+1)
 replicate(5, roll2())


 ## create roll function to follow book
roll <- function () {
  die <-1:6 
  dice <- sample(die, size =2, replace = TRUE)
  sum(dice)
}
roll()
## replicate 10,000 times roll() function. Assign output to variable to plot.
rolls<- replicate (10000, roll())
qplot( rolls, binwidth =1)

## access help page for sample
?sample
## make weighted dice game
roll2 <- function() {
  die <- 1:6
  dice <- sample(die, size =2, replace =TRUE, prob = c(1/8,1/8,1/8,1/8,1/8,3/8))
  sum(dice)
}
## must assign the output of the replication function a variable to plot it. 
# Otherwise you end up with a graph where the Y axis =1.
rolls<- replicate(10000, roll2())
qplot(rolls, binwidth =1)