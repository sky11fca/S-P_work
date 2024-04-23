#1)


#a)
poison_prob <- function(lambda, k, m)
{
  if (k>m)
  {
    stop("K must be les equal to m")
  }
  
  probs<-dpois(k:m, lambda)
  return (probs)
}

geometric_prob <- function(p, k, m)
{
  if (k>m)
  {
    stop("K must be les equal to m")
  }
  
  probs<-dgeom(k:m, p)
  return (probs)
}

binomial_prob <- function(n, p, k, m)
{
  if (k>m)
  {
    stop("K must be les equal to m")
  }
  
  probs<-dbinom(k:m, n, p)
  return (probs)
}


#b)

plot_probs <- function(probs, dist_name, k, m)
{
  plot(k:m, probs, type="h", lwd=10)
}

#c)

poisson_k0 <-function(lambda, alpha)
{
  k0<-qpois(1-alpha, lambda)
  return (k0)
}

#2)


#a)

comp_freq_n_exp <- function(file_name)
{
  data <- read.csv("file_name")
  
  sample_P <- data$P
  sample_S <- data$S
  
  freq_P <- as.vector(table(sample_P))/length(sample_P)
  freq_S <- as.vector(table(sample_S))/length(sample_S)
  
  exp_P <- sum(as.numeric(names(freq_P))*freq_P)
  exp_S <- sum(as.numeric(names(freq_S))*freq_S)
  
  return(list(freq_P = freq_P, freq_S = freq_S, exp_P = exp_P, exp_S = exp_S))
}

#b)

remove_and_plot <- function(file_name, sample_name) {
  data <- read.csv(file_name)
  
  sample <- data[[sample_name]]
  
  Q1 <- quantile(sample, 0.25)
  Q3 <- quantile(sample, 0.75)
  IQR <- Q3 - Q1
  
  low <- Q1 - 1.5 * IQR
  up<- Q3 + 1.5 * IQR
  
  trim <- sample[sample >= low & sample <= up]
  
  breaks <- seq(0, 10, by = 1)
  
  hist(sample, breaks = breaks, main = paste("Frequency Distribution of", sample_name),
       xlab = sample_name, ylab = "Freq", col = "lightblue", border = "black")
  
  hist(trimmed_sample, breaks = breaks, main = paste("Frequency Distribution of Trimmed", sample_name),
       xlab = sample_name, ylab = "Freq", col = "lightgreen", border = "black")
  
  legend("topright", legend = c("Normal", "Trimmed"), fill = c("lightblue", "lightgreen"))
  
  return(trimmed_sample)
}
