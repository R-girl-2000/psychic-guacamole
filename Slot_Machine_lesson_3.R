# DD is diamonds, 7s, BBB = triple bars, BB = double bars, B = single bars, c = cherries, 0= 0
# items have different probs of being sampled. give those values when defining the sample function.
get_symbols<-function(){
  wheel<- c("DD", '7', 'BBB',"BB", "b", "c", '0')
  sample(wheel, size =3, replace = TRUE,
         prob = c(0.03, 0.03,0.06, 0.1, 0.25, 0.01, 0.52))
}
get_symbols()