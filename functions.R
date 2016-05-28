
ParticleSwarmOptimzation <- function(f, n_g, n_p, c_1, c_2, dim_f = 1){
  P <- matrix(runif(n_p*dim_f),ncol=dim_f,nrow = n_p) #initial popltation
  v <- matrix(runif(n_p*dim_f),ncol=dim_f,nrow = n_p) #initial valosity
  Fvalue <- apply(P,1,f)
  
  Pbest <- P
  Fbest <- Fvalue
  Gbest <- min(Fvalue)
  gbest <- which.min(Fvalue)

  P_all <- P
  Pbest_all <- Pbest[gbest,]
  Gbest_all <- Gbest
  for(k in 1:n_g){
    u_1 <- matrix(runif(n_p*dim_f),ncol=dim_f)
    u_2 <- matrix(runif(n_p*dim_f),ncol=dim_f)
    Pbest_gbest <- matrix(Pbest[gbest,],ncol = dim_f,nrow = n_p,byrow = T)
    delta_v = c_1*u_1*(Pbest-P)+c_2*u_2*(Pbest_gbest-P)
    v <- v+delta_v
    P <- P+v
    Fvalue <- apply(P,1,f)
    
    if_statement <- Fvalue < Fbest
    Pbest[if_statement,] <- P[if_statement,]
    Fbest[if_statement] <- Fvalue[if_statement]
    
    if(min(Fvalue) < Gbest){
      Gbest <- min(Fvalue)
      gbest <- which.min(Fvalue)
    }
    
    P_all <- cbind(P_all,P)
    Pbest_all <- cbind(Pbest_all,Pbest[gbest,])
    Gbest_all <- c(Gbest_all,Gbest)
  }
  return(list(path = P_all, solutions = Pbest_all, value = Gbest_all))
}


ThresholdAccepting <- function(f, treshold, n_rounds, n_steps, dim_f = 1){
  x_c <- runif(dim_f)
  P_all <- x_c
  Pbest_all <- data.frame(x_c)
  Gbest_all <- f(x_c)
  for(r in 1:n_rounds){
    for(i in 1:n_steps){
      x_n <- rnorm(dim_f,mean = x_c) 
      delta = f(x_n) - f(x_c)
      P_all <- c(P_all,x_n)
      if( delta < treshold ){
        x_c = x_n
        Pbest_all <- cbind(Pbest_all,x_c)
        Gbest_all <- c(Gbest_all,f(x_c))
      }
      print(f(x_c))
    }
  }
  return(list(solutions = Pbest_all, value = Gbest_all))
}



AlgortihmsList <- list(ParticleSwarmOptimzation=c(n_g = 5, n_p=100, c_1=0.5, c_2 = 0, dim_f=1),
                   ThresholdAccepting=c(treshold=0.5, n_rounds=100, n_steps=5, dim_f = 1))





