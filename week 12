sim_rand_var=function(val, prob)
{
  if(sum(probs)!=1)
  {
    stop("The sum of the probabilities must be 1.")
  }
  
  U=runif(1)
  
  cum_probs=cumsum(prob)
  
  for(i in 1:length(cum_probs))
  {
    if(U<cum_probs[i])
    {
      return(val[i])
    }
  }
}

values = c(1, 2, 3, 4)
probs = c(0.1, 0.2, 0.4, 0.3)

result=sim_rand_var(values, probs)
result
