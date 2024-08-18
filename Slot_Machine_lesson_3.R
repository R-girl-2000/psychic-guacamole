# DD is diamonds, 7s, BBB = triple bars, BB = double bars, B = single bars, c = cherries, 0= 0
# items have different probs of being sampled. give those values when defining the sample function.
get_symbols<-function(){
  wheel<- c("DD", '7', 'BBB',"BB", "b", "c", '0')
  sample(wheel, size =3, replace = TRUE,
         prob = c(0.03, 0.03,0.06, 0.1, 0.25, 0.01, 0.52))
}
get_symbols()
# to make coding programs easier, break into sequential steps or parallell cases/ 
play <- function(){
  symbols<-get_symbols()
  print(symbols)
  score(symbols)
}
# sequential program. first define symbols, then print them, then score. 
#paralell program:
symbols<- get_symbols()
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