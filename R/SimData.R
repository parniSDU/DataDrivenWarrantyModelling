SimData <- function(alpha, K, W, SimSize){
  path<-paste("C:/Users/parni/OneDrive - Syddansk Universitet/ftaproximExtension/RESS/Data/SimData_K",K,"_alpha",alpha,"_size",SimSize,".txt", sep = "")
  out<-NULL
  for (i in 1:SimSize){
    state='WorkingState 1'
    t<-0
    cost <- 0
    data<-data.frame(state = state, time = t, cost= cost)
    
    while (t < W){
      d<-next_transition(state, K, t, alpha, cost, W)
      next_state<-d$state
      t <- d$time
      cost<-d$cost
      state <- next_state
      data<- rbind(data, d)
    }
    data$Item<-i
    out<-rbind(out,data)
    #sheet<-paste("Sheet",i,sep = "")
    
  } 
  write.table(out,path, row.names = FALSE)
}
