##Riddler express 3/31

# A sunny al fresco puzzle:
#
# On a lovely spring day, you and I agree to meet for a lunch picnic at the fountain
# in the center of our favorite park. We agree that we'll each arrive sometime from noon and 1 p.m.,
# and that whoever arrives first will wait up to 15 minutes for the other. If the other person
# doesn't show by then, the first person will abandon the plans and spend the day with a more
# punctual friend. If we both arrive at the fountain at an independently random time between noon
# and 1, what are the chances our picnic actually happens?

#Ben Finkelstein, Minneapolis, MN

picnic=function(t)
{
  set.seed(123)
  x<-runif(t)
  y<-runif(t)
  diff <- abs(x-y)
  lunch <- sum(diff<=.25)
  return(lunch/t)
}

#ans<-picnic(100000000)
