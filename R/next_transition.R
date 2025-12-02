next_transition <- function(state, K, t, alpha, cost, W){
  params <- time_distributions[[state]]
  possible_states <- transition_states[[state]]
  t <- sum(t, rgamma(1, shape = params['shape'], rate = params['rate']))
  fstate<-c('FailureState 1','FailureState 2', 'FailureState 3', 'FailureState 4')
  wstate<-c('WorkingState 1', 'WorkingState 2', 'WorkingState 3')
  
  if(state == 'WorkingState 4'){
    next_state <- 'FailureState 4'
    cost<-0
  }
  
  if (state %in% fstate){
    
    if (W-t> alpha){
      if (state %in% fstate[K:4]){
        next_state <- possible_states$state[2] #replace
        #cost <- runif(1, min=possible_states$cost[2]-10, max=possible_states$cost[2]+10)
        cost <- rnorm(1, mean = possible_states$cost[2], sd= 30)
      }
      if (state %in% fstate[1:(K-1)]){
        next_state <- possible_states$state[1] #repair
        #cost <- runif(1, min=possible_states$cost[1]-2, max=possible_states$cost[1]+2)
        cost <- rnorm(1, mean = possible_states$cost[1], sd= 20)
      }
    }else{
      
      next_state <- possible_states$state[1] #repair
      #cost <- runif(1, min=possible_states$cost[1]-2, max=possible_states$cost[1]+2)
      cost <- rnorm(1, mean = possible_states$cost[1], sd= 20)
    }
  }
  if (state %in% wstate){
    prob <- prob_transition[[state]]
    r<-runif(1,0,1)
    ifelse(r <= prob, next_state<-possible_states$state[1], next_state<-possible_states$state[2])
    cost<-0
  }
  
  out<-list(state = next_state, time = t, cost=cost)
  return(out)
}

