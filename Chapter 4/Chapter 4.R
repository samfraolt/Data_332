

setwd("C:\Users\samfr\OneDrive\Documents\Data_332\Chapter 4")

deck <- read.csv("deck.csv")

head(deck)

#Selecting Values 
#With index of positive integers 
deck[1, 1]

deck[1, c(1, 2, 3)]

deck[c(1, 1), c(1, 2, 3)]

deck[1:2, 1:2]

#with index of negative integers 
deck[-(2:52), 1:3]

#with zero
deck[0, 0]

#with blank spaces 
deck[1, ]

#with logical operators 
deck[1, c(TRUE, TRUE, FALSE)]

#with names
deck[1, c("face", "suit", "value")]

#Exercise 1
deal <- function(cards) {
  cards[1, ]
}

deal(deck)

deck2 <- deck[1:52, ]

deck3 <- deck[c(2, 1, 3:52), ]       #to shuffle the cards 

random <- sample(1:52, size = 52)
random

deck4 <- deck[random, ]
head(deck4)

#Exercise 2
shuffle <- function(cards) {
  random <- sample(1:52, size = 52)
  cards[random, ]
}

deck2 <- shuffle(deck)


deck3 <- deck[c(2, 1, 3:52), ]
head(deck3)

#Exercise 3
shuffle <- function(cards) {
  random <- sample(1:52, size = 52)
  cards[random, ]
}

deal(deck)

deal(deck2)

#Dollar sign and double bracket 
deck$value

mean(deck$value)

median(deck$value)

lst <- list(numbers = c(1, 2), logical = TRUE, strings = c("a", "b", "c"))
lst

lst[1]

sum(lst[1])

lst["numbers"]


