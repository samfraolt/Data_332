#Chapter 10

abs_loop <- function(vec){
  for (i in 1:length(vec)) {
    if (vec[i] < 0) {
      vec[i] <- -vec[i]
    }
  }
  vec
}

abs_sets <- function(vec){
  negs <- vec < 0
  171
  vec[negs] <- vec[negs] * -1
  vec
}

#Exercise 1
system.time(abs(long))

#Vectorized code 
vec <- c(1, -2, 3, -4, 5, -6, 7, -8, 9, -10)
vec < 0

vec[vec < 0]

vec[vec < 0] * -1

#Exercise 2
vec[vec == "DD"]
## "DD"
vec[vec == "C"]
## "C"
vec[vec == "7"]
## "7"
vec[vec == "B"]
## "B"
vec[vec == "BB"]
## "BB"
vec[vec == "BBB"]
## "BBB"
vec[vec == "0"]
## "0"


#Writing loops faster 
system.time(
  output <- rep(NA, 1000000)
  for (i in 1:1000000) {
    output[i] <- i + 1
  }
)


system.time(
  output <- NA
  for (i in 1:1000000) {
    output[i] <- i + 1
  }
)



#Vectorized code in practice
winnings <- vector(length = 1000000)
for (i in 1:1000000) {
  winnings[i] <- play()
}
mean(winnings)

system.time(for (i in 1:1000000) {
  winnings[i] <- play()
})


get_many_symbols <- function(n) {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  vec <- sample(wheel, size = 3 * n, replace = TRUE,
                prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
  matrix(vec, ncol = 3)
}
get_many_symbols(5)
















