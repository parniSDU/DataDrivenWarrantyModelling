#' Instantaneous Unavailability Vector
#'
#' This function calculates the isntantaneous unavailablity/reliabality values
#' of a basic event.
#'
#' @param BE a list containing states, transition matrix, distributions and their parameters for a basic event
#' @param state a string value for the state
#' @param totaltime an integer value for the total time
#' @param delta a numeric value as time step
#' @param tol a numeric value for the tolerance level
#'
#' @return a numeric vector of instantaneous unavailabilities when the state is F


Proxel<-function (state, W, delta, tol, alpha, K){
  tt<-delta
  steps <- W/delta
  meanCost <- numeric(steps)
  meanCost[1]<-0
  proxel <- list()
  proxel[[1]] <- data.frame(State = "WorkingState 1", ageInt = 0, 
                            Prob = 1, Cost=0)
  ###
  funsext <- function(s, delta, tt, alpha, W, K) {
    pL <- nextLevel(prox[s, ], delta, tt, alpha, W, K)
    ind <- which(pL$Prob < tol)
    if (length(ind) != 0) {
      pL <- pL[-ind, ]
    }
    return(pL)
  }
  
  ffunsext <- function(s) {
    return(funsext(s, delta, tt, alpha, W, K))
  }
  ###  
  i <- 2
  while (i <= (steps+1)) {
    Pro <- NULL
    prox <- proxel[[i - 1]]
    pL <- plyr::ldply(lapply(1:nrow(prox), ffunsext), data.frame)
    
    Pro<-pL
    meanCost[i]<-sum(Pro$Cost)
    
    proxel[[i]] <- Pro
    print(data.frame(i, tt, Pro))
    i <- i + 1
    tt<-tt+delta
  }
  out<-list(CumCost=cumsum(meanCost))
  return(out)
}
