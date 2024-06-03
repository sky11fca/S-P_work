#1)

comp_confidence1 = function(PATH)
{
  data = read.csv(PATH)
  scores = data$probabilitati

  mean = mean(scores)
  size = length(scores)

  var = 92.16
  std_dev = sqrt(var)

  sem = std_dev/sqrt(size)

  z_95 = qnorm(0.975)
  z_99 = qnorm(0.995)

  ci_95 = c(mean - z_95*sem, mean + z_95*sem)
  ci_99 = c(mean - z_99*sem, mean + z_99*sem)
  
  return(list(
    mean=mean,
    ci_95=ci_95,
    ci_99=ci_99
  ))
}

comp_confidence1('~/Documents/R/Homework/probabilitati.csv')

#2)

comp_confidence2 = function(PATH)

{
  data = read.csv(PATH)
  scores = data$statistica
  
  mean = mean(scores)
  size = length(scores)
  
  s=sd(scores)
  s_mean = s/sqrt(size)
  
  t_95=qt(0.975, df=size-1)
  t_99=qt(0.995, df=size-1)
  
  E_95=t_95*s_mean
  E_99=t_99*s_mean
  
  ci_95 = c(mean-E_95, mean+E_95)
  ci_99 = c(mean-E_99, mean+E_99)

  return(list(
    mean=mean,
    ci_95=ci_95,
    ci_99=ci_99
  ))
}

comp_confidence2('~/Documents/R/Homework/statistica.csv')


#3)

nr_fail = 14
n=100
p_null=0.15

p_hat = nr_fail/n
z_stat = (p_hat-p_null)/sqrt(p_null*(1-p_null)/n)

z_crit1 = qnorm(0.99)
z_crit2 = qnorm(0.95)

if(z_stat>z_crit1)
{
  cat("Yes1")
}else
{
  cat("No1")
}

cat("\n")

if(z_stat>z_crit2)
{
  cat("Yes2")
}else
{
  cat("No2")
}
