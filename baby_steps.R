##Riddler express 3/24

#Your baby is learning to walk. The baby begins by holding onto a couch.
#Whenever she is next to the couch, there is a 25 percent chance that she will take a step forward
#and a 75 percent chance that she will stay clutching the couch.
#If the baby is one or more steps away from the couch, there's a 25 percent chance that she will take a step forward,
#a 25 percent chance she'll stay in place and a 50 percent chance she'll take one step back toward the couch.
#In the long run, what percent of the time does the baby choose to clutch the couch?

#Ben Finkelstein, Minneapolis, MN

baby_steps=function(t, plot=F, hist=F)
{
  if(t==1){return(1)} #trivial case of the first period (where the baby spends the whole time on the couch)
  else{
    couch <- 1
    distance <- 0
    d <- vector(length = (t-1))
    for(i in 1:(t-1)){
      d[i] <- distance
      if (distance==0){
        a <- sample(x=c(0,1),size=1,prob=c(.75,.25))
        distance <- distance+a
      }
      else {
        a <- sample(x=c(-1,0,1),size=1,prob=c(.5,.25,.25))
        distance <- distance+a
      }
      if (distance==0){couch <- couch +1}
    }
    if (plot==TRUE){plot(d, type ="h", col = 'blue', ylab = 'Distance from Couch', xlab = 'Time')}
    if (hist==TRUE){hist(d, main = "Histogram of Distance from Couch", xlab = "Distance")}
    return(couch/t)
  }
}

finite_test =function(t){
  #for use in short-run estimation
  test <- vector(length=10000)
  for(i in 1:10000){
    test[i]<-baby_steps(t)
  }
  return(mean(test))
}

#ans<-baby_steps(1000000)

