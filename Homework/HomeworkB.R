#1

R=10
r=3

exact_vol=2*pi^2*R*r^2

box_vol=(2*(R+r))^2*(2*r)

in_torus=function(x1, x2, x3, R, r)
{
  return(x3^2+(sqrt(x1^2+x2^2)-R)^2<r^2)
}

MC_Torus=function(R, r, nr_samp)
{
  x1=runif(nr_samp, -R-r, R+r)
  x2=runif(nr_samp, -R-r, R+r)
  x3=runif(nr_samp, -r, r)
  
  points_in_torus=in_torus(x1, x2, x3, R, r)
  
  volume_est=sum(points_in_torus)/nr_samp*box_vol
}

sample_sizes <- c(10000, 20000, 50000)

results = data.frame(
  num_samples = sample_sizes,
  vol_estimate = numeric(length(sample_sizes)),
  relative_error = numeric(length(sample_sizes))
)

for (i in 1:length(sample_sizes)) {
  num_samples = sample_sizes[i]
  vol_estimate = MC_Torus(R, r, num_samples)
  relative_error = abs(vol_estimate - exact_vol) / exact_vol
  
  results$vol_estimate[i] = vol_estimate
  results$relative_error[i] = relative_error
}

print(results)

#2

in_triangle=function(x, y)
{
  return(y>=0 & y<=2*x & y<=6-3*x)
}

MC_AreaT=function(nample)
{
  x=runif(nample, 0, 2)
  y=runif(nample, 0, 2.4)
  
  points_in_triangle=in_triangle(x, y)
  
  rectangle_area=(2-0)*(2.4-0)
  est_area=sum(points_in_triangle)/nample*rectangle_area
  return(est_area)
}

MC_AreaT(20000)

#3

#a)

int_a=function(x)
{
  (2*x-1)/(x^2-x-6)
}

MC_int1=function(f, a, b, n)
{
  x_rand=runif(n, min=a, max=b)
  y_rand=int_a(x_rand)
  
  mean_y=mean(y_rand)
  
  est_int1=(b-a)*mean_y
  return(est_int1)
}

MC_int1(int_a, -1, 1, 1000000)
exact1=integrate(int_a, -1, 1)

#b)

int_b=function(x)
{
  (x+4)/(x-3)^(1/3)
}

MC_int2=function(f, a, b, n)
{
  x_rand=runif(n, min=a, max=b)
  y_rand=int_b(x_rand)
  
  mean_y=mean(y_rand)
  
  est_int1=(b-a)*mean_y
  return(est_int1)
}

MC_int2(int_b, 3, 11, 1000000)
exact3=integrate(int_b, 3, 11)

#c)
int_c=function(x)
{
  x*exp(-x^2)
}

MC_int3=function(f, a, n)
{
  x_rand=runif(n, min=a)
  y_rand=int_c(x_rand)
  
  mean_y=mean(y_rand)
  
  est_int1=-a*mean_y
  return(est_int1)
}

MC_int3(int_c, 0, 1000000)
exact2=integrate(int_c, 0, Inf)

#4)

#a)

years_to_target=function(UO, expect_new_users, q, target_users)
{
  years=0
  U=UO
  
  while(U<target_users)
  {
    U=U*(1-q)+expect_new_users
    years=years+1
  }
  
  return(years)
}

years_to_target(10000, 1000*0.25, 0.01, 15000)

#b)

expect_users=function(UO, expect_new_users, q, total_years)
{
  U=UO
  for(i in seq(1, total_years, by=1))
  {
    U=U*(1-q)+expect_new_users
  }
  
  return(U)
}

expect_users(10000, 1000*0.25, 0.01, 40+10/12)

#c)

mean_usr=expect_users(10000, 1000*0.25, 0.01, 40+10/12)
var_new_users=1000*0.25*(1-0.01)
var_users_leaving=100000*0.25*(1-0.01)
total_var=var_new_users+var_users_leaving

std_deviation=sqrt(total_var)

z_score=(15000-mean_usr)/std_deviation

prob=pnorm(z_score, lower.tail=FALSE)
prob

