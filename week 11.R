#I.2

parabola_area = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, 0.5, 2);
    y = runif(1, 0, 1.125);
    if(y<=-2*x^2+5*x-2)
      N_C = N_C + 1;
  }
  
  rectangle_area=(2-0.5)*1.125;
  
  est_area=(N_C/N)*rectangle_area
  
  return(rectangle_area);
}

integration=function(x)
{
  -2*x^2+5*x-2
}

est_area=parabola_area(100000)
est_area

exact_area=integrate(integration, lower=0.5, upper=2)$value
exact_area

relative_err=abs(est_area-exact_area)/abs(exact_area)


#II.1 B

MC_integration_nr1 = function(N) {
  sum = 0;
  for(i in 1:N) {
    x = runif(1, 1, 4);
    sum = sum + exp(x);
  }
  return((4-1)*sum/N);
}


MC_integration_nr1(100000)

exact_val=51.87987

abs_err=abs(MC_integration_nr1(100000)-exact_val)
rel_err=abs_err/abs(exact_val)

#II.1 D

MC_improved_integration_nr2 = function(N) {
  sum = 0;
  for(i in 1:N) {
    x = rexp(1, 1);
    sum = sum + 1/(4*x^2-1);
  }
  return(sum/N);
}

MC_improved_integration_nr2(10000)

exact_area=ln(3/4)

abs_err=abs(MC_improved_integration_nr2(10000)-ln(3/4))
rel_err=abs_err/abs(ln(3/4))

#II.2

MC_improved_integration_Lambda = function(Lambda,N) {
  sum = 0;
  for(i in 1:N) {
    x = rexp(1, Lambda);
    sum = sum + exp(-2*x^2)/(Lambda*exp(-lambda*x));
  }
  return(sum/N);
}

MC_improved_integration_Lambda(3, 500000)

#3.2

l1=4
l2=12
p1=3/4
p2=1/4
N=10000

gen_serve_time=function(lambda)
{
  return (-log(runif(1))/lambda)
}

serve_time=numeric(N)
{
  for(i in 1:N)
  {
    if(runif(1)<=p1)
    {
      serve_time[i]=gen_serve_time(l1)
    }else
    {
      serve_time[i]=gen_serve_time(l2)
    }
  }
}

est_mean=mean(serve_time)
