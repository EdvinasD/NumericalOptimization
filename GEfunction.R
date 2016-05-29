GEsemi <- function(x, P, n, muta1 = 1, muta2 = 1, check.mutations=F) {
  # Finds optimal path between cities in given cordinates or distance matrix.
  #
  # Args:
  #   x:      if x is nxn matrix, then it is looked as distance matrix else 
  #           euclidean distances are calculated between x rows.
  #   P:      population size
  #   n:      number of itrations
  #   muta1:  probability of having mutation type 1
  #   muta2:  probability of having mutation type 2
  #   check.mutations: if mutations should be only performed if it decreases
  #           distance
  #
  # Returns:
  #   list of best path, and paths which led to that
  #   optimization path
  
  
  
  # checks if x is distance matrix or not
  # x <- data
  if (dim(x)[1] != dim(x)[2]) {
    x <- as.matrix(dist(x))
  }
  
  #number of cities
  n.cities <- nrow(x)
  
  x <- melt(x)
  x$key <- paste(x$Var1, x$Var2)
  
  # function to evaluate distance
  TotalDistance <- function(y,x) {
    sum(x[x$key %in% paste(y, c(y[-1], y[1])),"value"])
  }
  
  # initial population
  P.init <- t(sapply(1:P, function(x) sample(1:n.cities)))
  P.gen <- P.init
  path.best <- 1:n.cities
  best.dist <- TotalDistance(path.best,x)
  
  # number of iterations
  for (j in 1:n) {
    P.init <- P.gen
    
    # for each element in population
    for (i in 1:P) {
      # random neighbor is selected
      random.neightbor <- P.init[sample(c(1:P)[1:P!=i],1),]
      # main element 
      ele <- P.init[i, ]
      # which connection to replace with random neighbors connection
      connection <- sample(1:n.cities,1)
      starting <- if (connection == 1) n.cities else connection-1
      ending <- connection %% n.cities + 1
      
      # conetion in random neighbor
      sequance <- random.neightbor[
        which(random.neightbor==ele[(starting)]):
          which(random.neightbor==ele[(ending)])]
      
      # replacemanet process
      if (!ele[connection]%in%sequance & length(sequance)>2) {
        choose.from <- sequance[!sequance%in%ele[c(starting,connection,ending)]]
        if (length(choose.from) == 1){
          ele[which(ele%in%choose.from)] <- 
            ele[connection]
        } else {
          ele[sample(which(ele%in%choose.from),1)] <- 
            ele[connection]
        }
      }
      
      notinsequence <- which(!ele%in%sequance)
      
      if (length(sequance) == 2) {
        
        ele[c(starting,connection,ending)] <- 
          ele[c(connection,starting,ending)]
        new <- ele
      } else {
        
        if (starting == 1) {
          new <- c(sequance,
                   ele[intersect((ending+1):n.cities, notinsequence)])
        } else {
          if (ending %in% c(1,2)) {
            new <- c(ele[intersect((ending+1):(starting-1), notinsequence)],
                     sequance)
            
          } else {
            new <- c(ele[intersect(1:(starting-1), notinsequence)],
                     sequance,
                     ele[intersect((ending+1):n.cities, notinsequence)])
          }
        }
      }
      
      new.dist <- TotalDistance(new,x)
      
      # type 1 mutation where to conection is reversed 
      if(muta1 > runif(1)){
        mutated <- new
        which.mutate <- sample(2:n.cities,1)
        to.switch <- c(which.mutate,which.mutate-1)
        mutated[to.switch] <-  mutated[rev(to.switch)] 
        muta.dist <- TotalDistance(mutated,x)
        if(check.mutations){
          if(new.dist>muta.dist){
            new <- mutated
            new.dist <- muta.dist
          }
        }else{
          new <- mutated
          new.dist <- muta.dist
        }
        
      }
      
      # type 2 mutation where to cities are swiched in path
      if(muta2 > runif(1)){
        mutated <- new
        to.switch <- sample(1:n.cities,2)
        mutated[to.switch] <-  mutated[rev(to.switch)] 
        muta.dist <- TotalDistance(mutated,x)
        if(check.mutations){
          if(new.dist>muta.dist){
            new <- mutated
            new.dist <- muta.dist
          }
        }else{
          new <- mutated
          new.dist <- muta.dist
        }
      }
      
      # if new element is better then old one old one is replaced
      if(new.dist<TotalDistance(P.init[i, ],x)){
        P.gen[i, ] <- new
        if(new.dist<best.dist){
          best.dist <- new.dist
          path.best <- cbind(path.best,new)
          colnames(path.best)[length(colnames(path.best))] <- paste0("iter",j)
        }
      }
    }
  }
  return(path.best)
}