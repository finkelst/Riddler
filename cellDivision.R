cellDiv = function(prob)
{
  n=1
  p=prob

  while(n > 0 && n < 1000){
    n2 = sum(sample(x=c(0,2),size=n,prob=c(1-p,p), replace = TRUE))
    n = n2
  }
  if(n>0)
    return(1)
  else return(0)
}



results = data.frame()
for(j in 1:10000){
  for(i in 1:100){results[j,i] = cellDiv(i/100)}
}

totals = colSums(results)/100

plot(totals, xlab="Multiplication Probability (%)", ylab="Survival Probability (%)")


