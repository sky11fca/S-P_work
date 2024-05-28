# II.1)

z_confidence_interval = function(n, xn, s, alpha)
{
  z_score=qnorm(1-alpha/2)
  
  se=s/sqrt(n)
  
  margin_error = z_score*se
  
  lb=xn-margin_error
  ub=xn+margin_error
  
  return (c(lb, ub))
}

ci = z_confidence_interval(100, 50, 10, 0.05)

round(ci[1], 2)
round(ci[2], 2)

# II.6)

z_confidence_interval = function(filename, sigma, alpha)
{
  sample_data = scan(filename, what=numeric(), quiet=TRUE)
  
  n=length(sample_data)
  xn = mean(sample_data)
  
  z_score=qnorm(1-alpha/2)
  
  se=sigma/sqrt(n)
  
  margin_of_error = z_score*se
  
  lb=xn-margin_of_error
  ub=xn+margin_of_error
  
  return(c(lb, ub))
}

ci = z_confidence_interval("~/Documents/R/week 12/history.txt", 5, 0.05)

round(ci[1], 2)
round(ci[2], 2)

#III.1)

t_conf_interval = function(n, xn, s, alpha)
{
  t_score = qt(1-alpha/2, df=n-1)
  
  se = s/sqrt(n)
  
  margin_of_error = t_score * se
  
  lb=xn-margin_of_error
  ub=xn+margin_of_error
  
  return(c(lb, ub))
}

ci = t_conf_interval(30, 50, 10, 0.05)

round(ci[1], 2)
round(ci[2], 2)

#III.4)

t_conf_interval = function(filename, alpha)
{
  sample_data = scan(filename, what=numeric(), quiet=TRUE)
  
  n=length(sample_data)
  xn=mean(sample_data)
  s=sd(sample_data)
  
  t_score = qt(1-alpha/2, df=n-1)
  
  se = s/sqrt(n)
  
  margin_of_error = t_score * se
  
  lb=xn-margin_of_error
  ub=xn+margin_of_error
  
  return(c(lb, ub))
}

#95% confidence:

c1 = t_conf_interval('~/Documents/R/week 12/history.txt', 0.05)
round(c1[1], 2)
round(c1[2], 2)

#99% confidence

c2 = t_conf_interval('~/Documents/R/week 12/history.txt', 0.01)
round(c2[1], 2)
round(c2[2], 2)

#IV.2)

test_proportion_increase = function(sample_size, defect_count, null_prop, alpha)
{
  sample_prop=defect_count/sample_size
  
  std_err=sqrt(null_prop*(1-null_prop)/sample_size)
  
  z_statistic = (sample_prop-null_prop)/std_err
  
  crit_val=qnorm(1-alpha)
  
  if(z_statistic>=crit_val)
  {
    result = "Prop of defect components increased"
  }
  else
  {
    result = "No proof enought that defect components increased"
  }
  
  list(
    sample_prop = sample_prop,
    z_statistic = z_statistic,
    crit_val = crit_val,
    decision = result
  )
}

test_proportion_increase(150, 20, 0.10, 0.05)
