# Riddler Classic 6/2/2017
# How many games would we expect to be needed to complete a best-of-seven series if
# each team has a 50 percent chance of winning each individual game?
# How about if one team has a 60 percent chance of winning each game? How about 70?
# Ben Finkelstein, Hui Xu
ET <- function(p){
  ET <- 4*(p^4+(1-p)^4) +
    5*choose(4,1)*(p^4*(1-p) + (1-p)^4*p) +
    6*choose(5,2)*(p^4*(1-p)^2 + (1-p)^4*p^2) +
    7*choose(6,3)*(p^4*(1-p)^3 + (1-p)^4*p^3)
  return(ET)
}
