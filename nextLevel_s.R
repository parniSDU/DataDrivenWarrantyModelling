nextLevel_s<-function (proxel, delta, t, alpha, W, K){
  state <- proxel$State
  possible_states <- transition_states[[state]]
  
  fstate<-c('FailureState 1', 'FailureState 2', 'FailureState 3', 'FailureState 4')
  wstate<-c('WorkingState 1', 'WorkingState 2', 'WorkingState 3', 'WorkingState 4')
  
  
  if (state %in% fstate){
    
    if (W-t>= alpha){
      if (state %in% fstate[K:4]){
        next_state <- possible_states$state[2] #replace
        cost <- possible_states$cost[2]
        next_proxel <- data.frame(State = next_state, 
                                  ageInt = calageInt(next_state,proxel, delta), 
                                  Prob = proxel$Prob *1, 
                                  Cost = proxel$Prob * cost)
      }
      if (state %in% fstate[1:(K-1)]){
        ##
        tranProb <- calProb_s(state, proxel, delta) #minimal repair
        age <- sapply(tranProb$States, calageInt<- function (x, proxel, delta){  ifelse(proxel$State != x, ageint <- 0, ageint <- proxel$ageInt + delta)
          ageint
        }, proxel, delta)
        cost<-c(0,possible_states$cost[1])
        next_proxel <- data.frame(State= tranProb[,1], Prob = proxel$Prob*tranProb[,2], 
                                  ageInt= age, Cost = proxel$Prob*cost)
        
      }
      
    }else{
      tranProb <- calProb_s(state, proxel, delta)    #minimal repair
      age <- sapply(tranProb$States, calageInt<- function (x, proxel, delta){  ifelse(proxel$State != x, ageint <- 0, ageint <- proxel$ageInt + delta)
        ageint
      }, proxel, delta)
      cost<-c(0,possible_states$cost[1])
      next_proxel <- data.frame(State= tranProb[,1], Prob = proxel$Prob*tranProb[,2], 
                                ageInt= age, Cost = proxel$Prob*cost)
      
    }
  }
  
  if (state %in% wstate){
    tranProb <- calProb_s(state, proxel, delta)
    age <- sapply(tranProb$States, calageInt<- function (x, proxel, delta){  ifelse(proxel$State != x, ageint <- 0, ageint <- proxel$ageInt + delta)
      ageint
    }, proxel, delta)
    next_proxel <- data.frame(State= tranProb[,1], Prob = proxel$Prob*tranProb[,2], 
                              ageInt= age, Cost = proxel$Prob*0)
    
  }
  return(next_proxel) 
  
}
