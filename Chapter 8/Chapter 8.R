#Chapter 8
num <- 1000000000
class(num) <- c("POSIXct", "POSIXt")
print(num)

#Attributes 
attributes(DECK)

row.names(DECK)

row.names(DECK) <- 101:152

levels(DECK) <- c("level 1", "level 2", "level 3")
attributes(DECK)


one_play <- play()
one_play

attributes(one_play)
attr(one_play, "symbols") <- c("B", "0", "B")
attr(one_play)

one_play

#Exercise 1
play <- function() {
  symbols <- get_symbols()
  prize <- score(symbols)
  attr(prize, "symbols") <- symbols
  prize
}

play()
two_play <- play()
two_play

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols)
}
three_play <- play()
three_play

slot_display <- function(prize){
  # extract symbols
  symbols <- attr(prize, "symbols")
  # collapse symbols into single string
  symbols <- paste(symbols, collapse = " ")
  # combine symbol with prize as a regular expression
  # \n is regular expression for new line (i.e. return or enter)
  string <- paste(symbols, prize, sep = "\n$")
  # display regular expression in console without quotes
  cat(string)
}
slot_display(one_play)

#Generic Functions 
print(pi)


#Exercise 2
play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = "slots")
}


#Classes 
methods(class = "factor")

play1 <- play()
play1

play2 <- play()
play2

c(play1, play2)

play1[1]




