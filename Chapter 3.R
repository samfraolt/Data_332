die<- c(1,2,3,4,5,6)
die

five<- 5
is.vector(five)
int<- 1L
ext<- "ace"

int<- c(1L, 5L)
text<- c("ace","hearts")
sum(int)

sum(text)

#To check what type of object 

typeof(die)
#Creating an integer

int<- c(-1L, 2L, 4L)

typeof(int)

#Creating a character

text<- c("Hello", "World")

#To Create complex vectors

comp <- c(1 + 1i, 1 + 2i, 1+3i)

typeof(comp)

#Exercise 5.2

hand<- c("ace", "king", "queen", "jack", "ten")

typeof(hand)

dim(die)<- c(2,3)
dim(die)<- c(3,2)

#Matrices
m<- matrix(die, nrow= 2)

#Arrays

ar<- array(c(11:14, 21:24, 31:34), dim = c(2,2,3))
ar
#Exercise 5.3

hand1<- c("ace", "king","queen","jack", "ten", "spades", "spades", "spades", "spades", "spades")

matrix(hand1, nrow = 5)
matrix(hand1, ncol = 2)
dim(hand1) <- c(5,2)

#Dates and Times

now<- Sys.time()
now
#Exercise 5.4
card <- c("ace", "hearts", 1)
card

#Converting data from one type to another

as.character(1)
as.logical(1)
as.numeric(FALSE)

#Exercise 5.5
card<- list("ace", "hearts", 1)
card

#Data Frames
df<- data.frame( face= c("ace","two", "six"),
      suit = c("clubs", "clubs", "clubs"), value = c(1,2,3))
df

head(deck)

#Saving Data

write.csv( deck, file = "cards.csv", row.names = FALSE)