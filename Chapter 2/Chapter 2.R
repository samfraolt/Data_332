
library(ggplot2)
x<- c (-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1)
y<- x^3
qplot(x,y)

# Create a histogram of x
qplot(x, geom="histogram")

x2<-c(1,2,2,2,3,3)
qplot(x2, binwidth = 1)

#Excercise 3.1
#Creating a Histogram x3
x3 <- c(0, 1, 1, 2, 2, 2, 3, 3, 4)

library(ggplot2)

# Create a histogram of x3 with binwidth = 1

x3<- c (0,1,1,2,2,2,3,3,4)
qplot(x3, binwidth = 1 )

replicate(3, 1+1)
replicate(10, roll())

#Exercise 3.2
roll<- function(){
  die<- 1:6
  dice<- sample(die, size =2, replace = TRUE)
  sum(dice)
}

roll<- function(){
  die<- 1:6
  dice<- sample(die, size = 2, replace = TRUE,
                prob = c(1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}

rolls<- replicate(10000, roll())
qplot(rolls, binwidth)