# Atomic vectors
die <- c(1,2,3,4,5,6)
die
is.vector(die)

#single number atomic vector
five <- 5
is.vector(five)
length(five)
length(die)

# Boolean. assign atomic vector of logics to a variable called logic.
# call logic, pass object through function "typeof" to see what type
# of data it contain.
logic<- c(TRUE, FALSE, TRUE)
logic
typeof(logic)

#Complex and Raw data
comp <- c(1 +1i, 1+2i, 1+3i)
comp
typeof(comp)

raw(3)
typeof(raw(3))

#vector of card names
hand <- c("ace", "king", "queen", "jack", "ten")
hand
typeof(hand)
## dimensions
names(die) <- c("One","two", "three", "four", "five", "six")
dim(die) <- c(6,1)
die
## Matrices, dimensions on steroids
m <- matrix(die, nrow=2)
m
## will fill by column by default, but can change to by row.
m<- matrix(die, nrow=2, byrow =TRUE)
m
# ?matrix will bring up help page for more arguments.
# Arrays
ar<- array(c(11:14, 21:24,31:34), dim = c(2,2,3))
   ar   
   
## matrix of cards
   hand<-c("ace", "king", "queen", "jack", "ten", "spades", "spades","spades", "spades", "spades")
   hand2 <- matrix(hand, nrow=5)
   hand2
   
## class
   attributes(die)
   class(die)
   class("hello")
   class(5)
#dates and time
now<- Sys.time()
now
typeof(now)
class(now)
unclass(now)

## 1 million seconds past Jan 1st 1970
mil <- 1000000
mil
class(mil) <- c("POSIXct", "POSIXt")
mil

# 5.5.2 factors, used for categorical data
gender <-factor(c("male", "female", "female", "male"))
typeof(gender)
attributes(gender)
unclass(gender)
gender
## R will try to convert char strings to factors, do not let it unless desired.
## to convert back to string:
as.character(gender)
## Lists combined other lists and R objects, not independent parts.
list1 <- list(100:130, "r", list(TRUE, FALSE))
list1
#I left the [1] notation in the output so you can see how it changes for lists. 
#The double-bracketed indexes tell you which element of the list is being displayed. 
#The single-bracket indexes tell you which subelement of an element is being displayed. 
card <- list("ace", "hearts", 1)
card

## data frame
df <- data.frame(face = c("ace","two","six"), suit = c("clubs", "clubs", "clubs"), value = c(1,2,3))
df
typeof(df)
class(df)
str(df)
#str means structure of an object
#to prevent R saving face as factors
df <- data.frame(face = c("ace", "two", "six"),  
                 suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3),
                 stringsAsFactors = FALSE)
df
str(df)
#Import data set saved in R_HandsonProg folder 
#save data frame as a new .csv file
write.csv(deck, file = "cards.csv", row.names=FALSE)
#find where working directory is
getwd()
#End Chapter 5. Start Chapter 6
#extract data, write df name followed by hard brackets
deck[1,2]
#Selects card in row 1, column 2.The opposite of Matrix function, which fill Column, row
#Select an entire row of data
deck[1,1:3]
#Select and entire column of data
deck[1:52,1]
# save returned values as new object.
new <- deck[1,c(1,2,3)]
new
deck[1,0]
deck[1,]
##use logic to select which rows/columns you want.
deck[1,c(T,T,F)]
# Returns row 1, column 1 (T), column 2 (t), does not return column 3 (F
#Exercise 6.1, deal a card
deal<- function(cards) {
  cards[1, ]
  }
deal(deck)
#deal(DIR_Test_PV)
##Pull info on which diets each DIRECT Participant Had.
#diet_type <- function(diet){
 # diet[,c(1,3)]
#}
#diet_type(DIR_Test_PV)
## pull with names instead of numbers
#diet_type2 <- function(diet){
  #diet[,c("ps_pid","anthtro_diet_visit2" )]
#}
#diet_type2(DIR_Test_PV)
## shuffle the deck, back to using deck df.
deck2 <- deck[1:52,]
head(deck2)
#ask for rows in a different order
deck3 <- deck[c(2,1,3:52), ]
head(deck3)
#generate a random set of integers to deal the deck using sample function
random <- sample(1:52, size = 52)
deck4<- deck[random, ]
head(deck4)



shuffle <- function(cards4) {
  random <- sample(1:52, size=52)
  deck[random, ]
  #cards4 <- deck[random, ]
  #head(cards4)
}
shuffle (deck)

deal23<- function(dealer) {
  dealer2<- shuffle(dealer)
  head(dealer2)
}
deal23(deck)
##6.4,$ and double brackets. $ selects column of data, also turns list into vector.
lst<- list(c(1,2), logical = TRUE, string= c("a", "b", "c"))
lst
lst[1]
lst$string
#use [[]] when no names attached to values
# using [] gives you one car of the train, [[]] gives you whats inside the car. 
lst[[1]]
# $ only returns numeric variables (doubles) as free numbers, not strings or other variables. 
# always just subsets  the list for string vars. 
lst[[3]]
lst$string
lst[3]
# for logicals, its the same. 
lst[2]
lst[[2]]

# Lesson 7, modifying values in cards.
deck2<- deck
#overwrite
vec <- c(0,0,0,0,0)
vec 
vec[1]
vec[1] <- 1000
vec
#replace multiple at once.
vec[c(1,3,5)]<- c(1,1,1)
vec
vec[4:5] <- vec[4:5] +1
vec
# create vectors that do not already exist. 
vec[6:7] <- 0
vec
#add new column called "new" to deck2 data frame.
deck2$new <- 1:52
head(deck2)
#remove using NULL
deck2$new <- NULL
head(deck2)
#reassign value of 14 to aces. Ace's appear every 13 cards.
deck2[c(13, 26, 39, 52), ]
deck2$value[c(13,26,39,52)] 
deck2$value[c(13,26,39,52)]<- 14
deck2$value[c(13,26,39,52)]
head(deck2)
deck2
shuffle<-function(cards) {
  random<- sample(1:52, size= 52)
  cards[random, ]
}
s_deck2<- shuffle(deck2)
head(s_deck2)
#7.02 Logical subsetting
a <- 1
b <- 4
c(1,2,3,4) %in% c(3,4,5)
a<b
a<=b
a>b
a>=b
a==b
a!=b
# make sure to us == to do logical sub-setting, not =.
deck2$face == "ace"
#use sum to quickly count the number of TRUE's in the previous vector. 
#change ace values to 14 using names.
deck3$value[deck3$face == "ace"] <- 14
shuffle(deck3)
s_deck3<- shuffle(deck3)
#see more values than head returns
print(s_deck3, n = 14)
#play Hearts. 
deck4 <- deck
deck4$value[deck4$suit == "hearts"] <- 1
deck4$value[deck4$suit != "hearts"] <- 0
deck4$suit == "hearts"
sum(deck4$suit == "hearts")
sum(deck4$suit!= "hearts")
head(deck4,13)
#queen of spades is = to 13. 
#find the queen of spades
deck4$face == "queen" & deck4$suit == "spades"
queenOfSpades <- deck4$face == "queen" & deck4$suit == "spades"
# Now use this var as an index to find the value of QoS.
deck4[queenOfSpades,]
deck4$value[queenOfSpades == TRUE] <- 13
head(deck4, 13)
#logical practice with tests. 
w<- c(-1,0,1)
x <- c(5,15)
y <- 'February'
z <- c("Monday", "Tuesday", "Friday")
#is w positive? Is 10<x<20? Is the object "y" = february? is every value in z a day of the week?
w>0
x>10 & x<20
y == "February"
days_of_week<- c("Monday","Tuesday", "Wednesday","Thursday", "Friday", "Saturday", "Sunday")
#uses boolean and logical operators. 
all(z %in% days_of_week)
# Black Jack
deck5<- deck
#find the face cards in one fell swoop.
face_cards<- deck5$face %in% c("king", "queen", "jack")
deck5[face_cards,]
#change their value
deck5$value[face_cards]<-10
head(deck5, 13)
deck5$face

#missing data
NA + 1
#can't assign NA to a new value, it is not a variable.
# == is a logical opperator that compares two things. 
NA = 1 
NA == 1
c(NA, 1:50)
#NAs propagate.
mean(c(NA, 1:50))
#use na.rm argument to remove them. 
mean(c(NA, 1:50), na.rm= TRUE)
# test to see if there are NAs in dataset.
is.na(NA)
vec <- c(1, 2, 3, NA)
is.na(vec)
#set all ace values to NA.
deck5$value[deck5$face == "ace"] <- NA

#Start at chapter 8
library(pryr)
parenvs(all=TRUE)
#8.2, working with environments.
parent.env(globalenv())
as.environment("package:stats")
globalenv()
baseenv()
emptyenv()
parent.env(globalenv())
#look up what is in an environment, or a bit about structure
ls(globalenv())
ls.str(globalenv())
#access deck from global env
head(globalenv()$deck, 3)
# assign an object to a particular environment. 
#give name, value, location.
assign("nobject", "R is fun", envir= globalenv())
assign("new", "Hello Global", envir = globalenv())
globalenv()$nobject
globalenv()$new
#8.3 Scoping rules
# global env is the one used at the command line, looks in parent folders if 
# item is not found in the global environment. continues up to the empty. 
#8.4, assignment
new
new <- "hello Active"
#R saves new values to the same name by overwriting the old value.
#R creates a new environment each time it evaluates a function, and then returns to the original environment when it's done.
#8.5, Evaluation.
show_env <- function(){
  list(ran.in = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}
#calling show_env will allow us to see the runtime environment created to run 
# the function, including the parent and which objects are in it. 
# Which environment R uses as the parent of the runtime environment. 
environment(show_env)
environment(parenvs)
#creating objects for show_env function. will be in the same environment that it is run in.
show_env <- function(){
  a <-1
  b <- 2
  c <- 3
  list(ran.in = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}
show_env()
# this is how r ensures that a function does not overwrite anything it shouldn't
foo <-"take me to your runtime"
show_env <- function(x = foo) {
  list(ran.in = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}
show_env()
#big kahuna, redefine deal to make it work.
deal <- function() {
  deck[1,]
}
# deal is saved in the  global environment, but deck is run in the RT environment.
#the RT environment finds deck in the parent env of the RT env, global. 
# remove the top card from deck. DECK is clean copy of deck. 
DECK <- deck
deck <- deck [-1,]
head(deck,3)
deal()
#create new deal function. Notice that R does not add the top card back
#it used deal from the global environment, not the RT one.
deal <- function(){
  card <- deck[1,]
  deck <- deck[-1,]
  card
}
deal()
#overwrite the deck, this time assigning deck to global env from inside RT. 
deal <- function(){
  card <- deck[1,]
  assign("deck", deck[-1,], envir = globalenv())
  card
}
deal()
# now it will remove a card  every time you deal it. -1 row. 
deal()
#shuffle function:
shuffle <- function(cards) {
  random <- sample(1:52, size = 52)
  cards[random, ]
}
shuffle(deck)
head(deck, 3)
a <- shuffle(deck)
head(a,3)
#exercise 8.3, rewrite shuffle so that it re-writes the copy of deck that lives
# in the global environment with a shuffled version of DECK.
shuffle <- function(){
  random<-sample(1:52, size =52)
  assign('deck',DECK[random,],envir= globalenv())
}
# now run shuffle and then deal
shuffle()
deal()
# create a RT environment to store deck
setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function(){
    card <- deck[1, ]
    assign("deck", deck[-1,], envir = globalenv())
  }
  SHUFFLE <- function(){
    random<- sample(1:52, size= 52, replace = FALSE)
    assign('deck', DECK[random,], envir = globalenv())
  }
}
# create a list to return SHUFFLE and DEAL to global env.
setup <- function(deck) {
  DECK <- deck
  # deal is a function we create to remove the top card and reset the deck.
  DEAL <- function(){
    card <- deck[1, ]
    assign("deck", deck[-1,], envir = globalenv())
  }
  SHUFFLE <- function(){
    random<- sample(1:52, size= 52, replace = FALSE)
    assign('deck', DECK[random,], envir = globalenv())
# the deck created here is still missing the first card, since order of opperations applies
# to assign deck again, we loop back to using DECK from the beginning of the function
# where DECK = deck, and deck has previously been re-defined globally as deck[-1,]
#look at DECK again, we are using the GLOBAL deck to set it. what we do to global deck affects RT DECK.
  }
#then create a list with the functions in the grand function
  list(deal = DEAL, shuffle = SHUFFLE)
}
setup(deck)
# now store the location of that list to a variable in the globalenv.
cards<- setup(deck)
# now use that location to find the contents of that list and store them in the global env.
deal <- cards$deal
shuffle <- cards$shuffle
#call deal/shuffle variable(s) and see what it's function is
deal
shuffle
environment(deal)
