#1)

#a)
compute_poisson=function(k, m, lambda)
{
  prob_poisson=dpois(k:m, lambda)
  return (prob_poisson)
}

compute_geometric=function(k, m, p)
{
  prob_geom=dgeom(k:m, p)
  return(prob_geom)
}

compute_binomial=function(k, m, n, p)
{
  prob_binom=dbinom(k:m, n, p)
}

#b)

plot_probs=function(prob, k, m)
{
  x=k:m
  plot(x=x, y=prob)
}

#c)


find_pos=function(lambda, threshold=1-10^6)
{
  k0=0
  cum_prob=0
  
  while(cum_prob<=threshold)
  {
    cum_prob=cum_prob+ppois(k0, lambda)
    k0=k0+1
  }
  return(k0-1)
}

#examples to run
#a)
p1=compute_poisson(3, 7, 0.3)
p1

p2=compute_geometric(3, 7, 1/2)
p2

p3=compute_binomial(3, 7, 10, 1/2)
p3
#B)
plot_probs(p1, 3, 7)
plot_probs(p2, 3, 7)
plot_probs(p3, 3, 7)

#C)

k0=find_pos(3)
k0



#2)
#a)

compute_freq=function(file_path)
{
  data=read.csv(file_path)
  
  P=data$P
  S=data$S
  
  abs_freqP=as.vector(table(P))
  abs_freqS=as.vector(table(S))
  
  rel_freqP=abs_freqP/length(P)
  rel_freqS=abs_freqS/length(S)
  
  expectP=mean(P)
  expectS=mean(S)
  
  return(list(
      abs_freqP=abs_freqP,
      rel_freqP=rel_freqP,
      expectP=expectP,
      abs_freqS=abs_freqS,
      rel_freqS=rel_freqS,
      expectS=expectS
    ))
}

compute_freq("~/Documents/R/Homework/note_PS.csv")

#b)

rm_outliers=function(file_path, sample)
{
  data=read.csv(file_path)
  samp=data[[sample]]
  
  Q1=quantile(samp, 0.25)
  Q3=quantile(samp, 0.75)
  
  IQR=Q3-Q1
  LB=Q1-1.5*IQR
  UB=Q3+1.5*IQR
  
  trimmed_samp=samp[samp>=LB & samp<=UB]
  
  breaks=seq(0, 10, by=1)
  freq_dist=cut(trimmed_samp, breaks=breaks, right=TRUE, include.lowest=TRUE)
  freq_table=as.vector(table(freq_dist))
  
  barplot(freq_table, names.arg=levels(freq_dist))
  
  return(trimmed_samp)
}

TP=rm_outliers("~/Documents/R/Homework/note_PS.csv", "P")
TS=rm_outliers("~/Documents/R/Homework/note_PS.csv", "S")

