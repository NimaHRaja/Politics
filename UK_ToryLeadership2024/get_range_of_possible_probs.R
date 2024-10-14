# This function is part of a project to analyse lpSolve (Linear Programming for R) to 
# analyse Conservative Leadership betting data. 
# get_range_of_possible_probs returns gets a probability of head-to-head challenges and tests whether 
# the probability satisfies the market odds. It returns a 0/1 value.

get_range_of_possible_probs <- function(x){
  
  candidates <- x["candidates"] %>% as.character()
  a_time <- x["a_time"] %>% as.numeric()
  p <- x["prob"] %>% as.numeric()
  
  
  # Creating const.mat
  
  const.mat <- 
    read.csv("constraints.csv", header = TRUE) %>% 
    select(-prob.case) %>% as.matrix() # constraints for min probabilities
  const.mat <- rbind(const.mat, const.mat)  # constraints for max probabilities
  const.mat <- rbind(const.mat, diag(6)) # constraints for probabilities > 0
  const.mat <- rbind(const.mat, diag(6)) # constraints for probabilities < 1
  const.mat <- rbind(const.mat, rep(1,6) %>% as.matrix() %>% t()) # probabilities summed up to 1
  
  extra_cond <- rep(0,6) # Extra condition to set a pair of candidates head-to-head probabilities to a value
  if(candidates == "CB"){extra_cond[1] <- 1-p; extra_cond[2] <- -p;}
  if(candidates == "CJ"){extra_cond[3] <- 1-p; extra_cond[4] <- -p;}
  if(candidates == "BJ"){extra_cond[5] <- 1-p; extra_cond[6] <- -p;}
  
  names(extra_cond) <- c("CBJ", "BCJ", "CJB", "JCB", "BJC", "JBC")
  
  const.mat <- rbind(const.mat, extra_cond %>% as.matrix() %>% t())
  
  
  # Creating const.dir
  
  const.dir <- c( 
    rep(">=", 6), # min probabilities
    rep("<=", 6), # max probabilities
    rep(">=", 6), # probabilities > 0 
    rep("<=", 6), # probabilities < 1
    "=", # probabilities summed up to 1
    "=" # head-to-head probability value set
  ) 
  
  
  # Creating const.rhs
  
  const.rhs <- 
    1/(read.csv("Betfair_odds.csv") %>% filter(time == a_time) %>% select(-time)) %>% 
    unlist() %>% as.vector() %>% unname() # max and min probabilities
  
  const.rhs <- c(const.rhs,
                 rep(0, 6), # probabilities > 0 
                 rep(1, 6), # probabilities < 1
                 1, # probabilities summed up to 1
                 0 # head-to-head probability value set
  ) 
  
  data.frame(
    candidates = candidates,
    prob = p,
    time = a_time, 
    value = 
      lp(direction = "min", objective.in = c(1,1,1,1,1,1), 
         const.mat, const.dir, const.rhs, compute.sens=TRUE)$objval %>% 
      as.numeric()
  )
}