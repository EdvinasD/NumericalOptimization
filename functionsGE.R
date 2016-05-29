# reads tsp files which are in csv vector format and separated with newlines
readTSP.csv <- function(path) {
  data <- data.frame(read.csv(path))
  vec <- c(as.character(data[,1]))
  vec <- vec[gsub("\\d", "", vec) %>% gsub("[.]","",.) %>% gsub(" ","",.) == ""]
  vec <- gsub("^[ ]*","",vec) %>% gsub(" +"," ",.)
  vec <- strsplit(vec," ") %>% Reduce("rbind",.) %>% data.frame %>% 
    apply(1,as.numeric) %>% t
  vec[,-1]
}

# another type of data to read
readTSPdist <- function(x) {
  data <- data.frame(read.csv(x))
  data <- c(as.character(data[,1]))
  vec <- 
    data[gsub("\\d", "", data) %>% 
         gsub("[.]","",.) %>% gsub(" ","",.) == ""] %>% 
    gsub("^[ ]*","",.) %>% gsub(" +"," ",.) %>% 
    strsplit(" ") %>% unlist %>% as.numeric
  
  dim <- gsub("\\D","",data[grep("DIMENSION",data)]) %>% as.numeric
  mat <- diag(dim)
  mat[upper.tri(mat,diag = T)] <- vec
  mat <- t(mat)
  mat[upper.tri(mat,diag = T)] <- vec
  mat
}


