#The pourpose of this project is to develop a series of functions that 
##produce a fibonocci sequence with varying utility. This will include arguments that control 
###how many numbers are printed and stores them into a vector.

#Step 1 creates a fibonocci sequece which begins with one and then adds the previous number.
##It is desinged to stop when it exceeds 150

myfib <- function(){
 fib.a <- 1  
 fib.b<-1
 cat(fib.a,", ",fib.b,", ",sep = "")
 repeat{
   temp <- fib.a+fib.b
   fib.a <- fib.b
   fib.b <- temp
   cat(fib.b,", ", sep = "")
   if(fib.b>150){
     cat("BREAK NOW...")
     break
   }
 }
}

myfib()


#Step two creates a function that causes the sequence to stop once it reaches a certain number.

myfib2 <- function(thresh){
  fib.a <- 1
  fib.b <- 1
  cat(fib.a,", ",fib.b,", ",sep="")
  repeat{
    temp <- fib.a+fib.b
    fib.a <- fib.b
    fib.b <- temp
    cat(fib.b,", ",sep="").
    if(fib.b>thresh){
      cat("BREAK NOW...")
      break
    }
  }
}
myfib2(thresh = 1000) #calls the function.



#Step 3 creates a vector based off the fibonocci sequence and stores it

myfib3 <- function(thresh){
  fibseq <- c(1,1)
  counter <- 2
  repeat{
    fibseq <- c(fibseq,fibseq[counter-1]+fibseq[counter])
    counter <- counter+1
    if(fibseq[counter]>thresh){
      break
    }
  }
  return(fibseq)
} 

myfib3(150) #calls function



