#1 -> Las Vegas
#a).

generate_rand_perm = function(n)
{
  U=runif(n, min=0, max=1)
  perm=order(U)
  return(perm)
}

set.seed(123)
rand_perm = generate_rand_perm(10)
sorted = sort(rand_perm)
sorted

#b)

lex_comp = function(WI, WJ)
{
  LIJ=min(length(WI), length(WJ))
  
  for(l in 1:LIJ)
  {
    if(WI[l]<WJ[l])
    {
      return (TRUE)
    }
    else if(WI[l]>WJ[l])
    {
      return(FALSE)
    }
  }
  
  if(length(WI)==length(WJ))
  {
    return (sample(c(TRUE,FALSE), 1))
  }
  
  if(length(WI)<length(WJ))
  {
    while(length(WI)<length(WJ))
    {
      WI = c(WI, sample(c(0,1), 1))
      if(WI[length(WI)]<WJ[length(WI)]) return(TRUE)
      if(WI[length(WI)]>WJ[length(WI)]) return(FALSE)
    }
  }
  else
  {
    while(length(WI)>length(WJ))
    {
      WJ = c(WJ, sample(c(0,1), 1))
      if(WI[length(WJ)]<WJ[length(WJ)]) return(TRUE)
      if(WI[length(WJ)]>WJ[length(WJ)]) return(FALSE)
    }
  }
  
  return(sample(c(TRUE,FALSE), 1))
}

WI = c(1, 0, 1, 0)
WJ = c(1, 0, 1, 1)
lex_comp(WI, WJ)

#c)

quick_sort = function(words)
{
  if(length(words)<=1) return(words)
  
  pivot_index = sample(1:length(words), 1)
  pivot = words[[pivot_index]]
  less = list()
  great = list()
  
  for(i in 1:length(words))
  {
    if(i!=pivot_index)
    {
      if(lex_comp(words[[i]], pivot))
      {
        less = c(less, list(words[[i]]))
      }
      else
      {
        great = c(great, list(words[[i]]))
      }
    }
  }
  
  sort_less = quick_sort(less)
  sort_great = quick_sort(great)
  
  return(c(sort_less, list(pivot), sort_great))
}

words = list(c(1, 0, 1, 0),c(1, 0, 1, 1),c(0, 1, 0, 1),c(0, 0, 0, 1))
quick_sort(words)

words

#d)

gen_perm_QS = function(n, k)
{
  gen_bin_str = function(k)
  {
    return(sample(c(0, 1), k, replace=TRUE))
  }
  
  words = lapply(1:n, function(x) gen_bin_str(k))
  sort_words = quick_sort(words)
  
  perm = sapply(sort_words, function(x){which(sapply(words, function(y) all(y==x)))})
  
  return(perm)
}

set.seed(123)
gen_perm_QS(5, 4)

#2) -> MONTE CARLO
#a)

max_cut = function(graph)
{
  V = names(graph)
  n = floor(length(V)/2)
  
  A = sample(V, n)
  B = setdiff(V, A)
  
  cut_size = 0
  for(u in A)
  {
    for(v in graph[[u]])
    {
      if(v %in% B)
      {
        cut_size = cut_size+1
      }
    }
  }
  
  return(cut_size)
}

graph = list(
  '1' = c('2', '3'),
  '2' = c('1', '3', '4'),
  '3' = c('1', '2', '4'),
  '4' = c('2', '3')
)

max_cut(graph)

#b)

repeat_max_cut = function(graph, iter = 1000)
{
  best_cut = 0
  for(i in 1:iter)
  {
    cut_size = max_cut(graph)
    if(cut_size>best_cut)
    {
      best_cut = cut_size
    }
  }
  return(best_cut)
}

repeat_max_cut(graph, iter=1000)