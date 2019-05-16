#print full name
full.name<-function(first, last){
  sprintf("Hello %s %s", first, last) 
}

#print first name
first.name<-function(firstN){
  sprintf("Hello %s", firstN)
}

#print full name + additional arguments
extra.name<-function(first, last,...){
  argument <- list(...)
  print(argument)
  sprintf("Hello %s %s", first, last) 
}
extra.name("Jo", "Oo", "cat", "dog")