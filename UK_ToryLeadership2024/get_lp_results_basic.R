# This function is part of a project to analyse lpSolve (Linear Programming for R) to 
# analyse Conservative Leadership betting data. 
# get_lp_results_basic returns maximum and minimum probabilities for all possible states (cases).

get_lp_results_basic <- function(x){
  
  minmax <- x["minmax"] %>% as.character()
  case <- x["case"] %>% as.character()
  a_time <- x["a_time"] %>% as.numeric()
  
  
  # Creating const.mat
  
  const.mat <- 
    read.csv("constraints.csv", header = TRUE) %>% 
    select(-prob.case) %>% as.matrix() # constraints for min probabilities
  const.mat <- rbind(const.mat, const.mat)  # constraints for max probabilities
  const.mat <- rbind(const.mat, diag(6)) # constraints for probabilities > 0
  const.mat <- rbind(const.mat, diag(6)) # constraints for probabilities < 1
  const.mat <- rbind(const.mat, rep(1,6) %>% as.matrix() %>% t()) # probabilities summed up to 1
  
  
  # Creating const.dir
  
  const.dir <- c( 
    rep(">=", 6), # min probabilities
    rep("<=", 6), # max probabilities
    rep(">=", 6), # probabilities > 0 
    rep("<=", 6), # probabilities < 1
    "=") # probabilities summed up to 1
  
  
  # Creating const.rhs
  
  const.rhs <- 
    1/(read.csv("Betfair_odds.csv") %>% filter(time == a_time) %>% select(-time)) %>% 
    unlist() %>% as.vector() %>% unname() # max and min probabilities
  
  const.rhs <- c(const.rhs,
                 rep(0, 6), # probabilities > 0 
                 rep(1, 6), # probabilities < 1
                 1) # probabilities summed up to 1
  
  # Creating objective.in 
  
  objective.in <- rep(0,6)
  names(objective.in) <- c("CBJ", "BCJ", "CJB", "JCB", "BJC", "JBC")
  objective.in[case] <- 1 # picking up one of the cases
  
  data.frame(
    minmax = minmax,
    case = case, 
    time = a_time, 
    value = 
      lp(direction = minmax, objective.in, const.mat, const.dir, const.rhs, compute.sens=TRUE)$objval %>% 
      as.numeric()
  )
}