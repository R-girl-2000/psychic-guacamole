# DD is diamonds, 7s, BBB = triple bars, BB = double bars, B = single bars, c = cherries, 0= 0
# items have different probs of being sampled. give those values when defining the sample function.
get_symbols<-function(){
  wheel<- c("DD", '7', 'BBB',"BB", "B", "c", '0')
  sample(wheel, size =3, replace = TRUE,
         prob = c(0.03, 0.03,0.06, 0.1, 0.25, 0.01, 0.52))
}
get_symbols()
# to make coding programs easier, break into sequential steps or parallel cases
play <- function(){
  symbols<-get_symbols()
  print(symbols)
  score(symbols)
}
# sequential program. first define symbols, then print them, then score. 
#paralell program:
symbols<- get_symbols()

--------------------------------------------------------------------------------
#If then statements
num <- -1 
if (num<0) {
  print("num is negative")
  print("dont worry, I'll fix it.")
  num <- num*-1
  print("now num is positive")
}
num
#else statements
a <- 3.14
dec <- a - trunc(a)
dec
if(dec >= .5){
  a <- trunc(a) + 1
} else {
  a <-trunc(a)
}
a
a <- 4.8
dec <- a - trunc(a)
dec
if(dec >= .5){
  a <- trunc(a) + 1
} else {
  a <-trunc(a)
}
a
a <- 1
b <- 1
------------------------------------------------------------------------------
#play slots framework
  
if(#case 1: all the same <1>)
  { prize<- #look up the prize <3>
} else if (#case 2: all bars <2> )
  { prize <- #assign $5 <4>
    } 
    else {
  #count cherries <5>
  prize <- # calculate a prize <7>
    }
#count diamonds <6>
#double prize if necessary <8>
}

------------------------------------------------------------------------------
#make test symbols value.
symbols <- c("7","7","7")
symbols


# case 1, all the same

symbols[1] == symbols[2] & symbols[2] == symbols[3]
same <- symbols[1] == symbols[2] && symbols[2] == symbols[3] # <1>
# <1> is done
if(same)
  { prize<- #look up the prize <3>
  } else if (#case 2: all bars <2> )
    { prize <- #assign $5 <4>
    } 
    else {
      #count cherries <5>
      prize <- # calculate a prize <7>
    }
    #count diamonds <6>
    #double prize if necessary <8>
    }
 -----------------------------------------------------------------------------   
#  case 2, all bars
symbols <- c("B", "BB","BBB")
all(symbols %in% c("B","BB","BBB"))
bars <- all(symbols %in% c("B","BB","BBB"))
# <2> is done
if(same)
{ prize<- #look up the prize <3>
} else if (bars #can also use all(bars))
  { prize <- #assign $5 <4>
  } 
  else {
    #count cherries <5>
    prize <- # calculate a prize <7>
  }
  #count diamonds <6>
  #double prize if necessary <8>
  }
--------------------------------------------------------------------------------

# task 3, calculate a prize for same, create a lookup table
# create a  new list and assign attributes to them. 
# task 4, assign 5 to all bars case 
payouts_same<- c("DD"= 100, '7'= 80, 'BBB' = 40,"BB" = 25, "B" = 10, 
                 "c" = 10, '0' = 0 )
payouts_same
# asking R to access stored attributes about "BB" in the payouts_same vector/df. 
payouts_same["BB"]
# when we look up "BB" in symbols vector, returns NA. No stored attributes
symbols["BB"]
# to remove name, use unaname() function.returns a copy without name. 
unname(payouts_same["BB"])
#subset based on first char of the symbols variable. 
symbols <- c("7","7","7")
unname(payouts_same[symbols[1]]) #<3>

if(same) { 
  payouts_same <- c("DD"= 100, '7'= 80, 'BBB' = 40,"BB" = 25, 
                    "B" = 10, "c" = 10, '0' = 0 )
  prize<- unname(payouts_same[symbols[1]])
} else if (all(bars)) { 
    prize <- 5 #<4> 
    } else {
             #count cherries <5>
             prize <- # calculate a prize <7>
           }
           #count diamonds <6>
           #double prize if necessary <8>
           # calculate score for three of a kind.
           score <- function(symbols){
             # calculate a prize
             prize
           }
----------------------------------------------------------------------------------         
# tasks 5 & 6, count cherries and diamonds
             
symbols <- c("c", "c", "0")
symbols == "c"  #or 
sum(symbols == "c") #<5>
#can do the same for counting diamonds <6>
sum(symbols == "DD")
if(same) { 
  payouts_same <- c("DD"= 100, '7'= 80, 'BBB' = 40,"BB" = 25, 
                    "B" = 10, "c" = 10, '0' = 0 )
  prize<- unname(payouts_same[symbols[1]])
} else if (all(bars)) { 
  prize <- 5 
} else {
  cherries <- sum(symbols == "c")
  prize <- # calculate a prize <7>
}
diamonds <- sum(symbols == "DD")
#double prize if necessary <8>
# calculate score for three of a kind.
}
--------------------------------------------------------------------------------
# task 7, prize based on cherries
# cherries prize is best made using sub-setting as well, rather than an if/else tree.

cherries_prize<- c(0,2,5) # cherries $, 1 cherry = $2
cherries +1 
cherries_prize[cherries +1]
symbols<- c('c','0', '0')
cherries <- sum(symbols == "c")
cherries_prize[cherries + 1 ] #<7>

#another way to write is:

prize<- c(0,2,5)[cherries +1]

#subsets the list 0,2,5 by the value generated by cherries +1, 
#since no zero place in the list (0 is first value)

symbols<- c("0","DD","c")
if(same) { 
  payouts_same <- c("DD"= 100, '7'= 80, 'BBB' = 40,"BB" = 25, 
                    "B" = 10, "c" = 10, '0' = 0 )
  prize<- unname(payouts_same[symbols[1]])
} else if (all(bars)) { 
  prize <- 5 
} else {
  cherries <- sum(symbols == "c")
  prize<- c(0,2,5)[cherries+1]
}
diamonds <- sum(symbols == "DD")
#double prize if necessary <8>
}
--------------------------------------------------------------------------------
# task 8, assign prize based on diamonds
# double prize for each diamond present.
symbols<- c("C", "DD", "0")
diamonds <- sum(symbols == "DD")
diamonds
prize <- prize*(c(1,2,4)[diamonds+1])
prize
#another option
prize* 2 ^ diamonds #<#8>

--------------------------------------------------------------------------------
#full program for scores

score<- function(symbols){
  #identify case
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B", "BB", "BBB")
  
  #get prize
  if(same) { 
  payouts_same <- c("DD"= 100, '7'= 80, 'BBB' = 40,"BB" = 25, 
                    "B" = 10, "c" = 10, '0' = 0 )
  prize<- unname(payouts_same[symbols[1]])
} else if (all(bars)) { 
  prize <- 5 
} else {
  cherries <- sum(symbols == "c")
  prize<- c(0,2,5)[cherries+1]
}
  #adjust for diamonds
diamonds <- sum(symbols == "DD")
 prize* diamonds ^ 2
}

--------------------------------------------------------------------------------

#play slots
play<- function(){
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}
play()


