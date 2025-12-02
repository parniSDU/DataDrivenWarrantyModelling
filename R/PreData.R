PreData<-function(data, withinWarranty=TRUE){
  #data<-d
  flist<-NULL
  cost<-numeric()
  t<-ifelse(withinWarranty , 2, Inf)
  SimSize<-length(unique(data$Item))
  
  for (i in 1:SimSize){
    
    x<-data[data$Item==i,]
    x<-x[x$time<=t,]
    cost[i]<-sum(x$cost)
    
    findex<-which(x$state %in% fstate)
    
    #obsw<-windex[which(windex>findex[1])]
    xf<-x[findex,]
    
    if(nrow(xf)>0){
      
      df <- x %>%
        mutate(
          state_type = case_when(
            state %in% wstate ~ "w",
            state %in% fstate ~ "f",
            TRUE ~ NA_character_
          )
        )
      
      tf<-xf[1,"time"]
      df <- df %>% filter(df$time>=tf)
      matching_rows <- list()
      for (j in 1:(nrow(df) - 1)) {
        window <- df[j:(j+1), ]
        type_sequence <- paste(window$state_type, collapse = "")
        
        # Check for the exact pattern
        if (type_sequence == "fw") {
          matching_rows[[length(matching_rows) + 1]] <- window
        }
      }
      df<-bind_rows(matching_rows)
      
      flist<-rbind(flist,df)
    }
    
  }
  return(flist)
}
