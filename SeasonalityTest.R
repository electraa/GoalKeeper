### Cross-Validation 
DecomposeC = function(data,frequency){
  
  frame<-matrix(data = NA, nrow = length(data), ncol = 1, byrow = FALSE,dimnames = NULL)
  frame<-data.frame(frame); colnames(frame)<-c("ID"); frame$Data<-data
  frame$kmo<-NA # moving average based on frequency
  
  seasonality<-frequency
  ID<-c(); IDref<-c(1:seasonality) # which month is this observation?
  for (i in 1:(length(data)%/%seasonality)){ ID<-c(ID,IDref) }
  ID<-c(ID,head(IDref,(length(data)%%seasonality))) ;frame$ID<-ID
  
  
  if (frequency==1){
    
    frame$Seasonality<-1
    
  }else if (frequency==2){
    
    for (i in 4:(length(data)-3)){
      n1<-(frame$Data[i+3]+frame$Data[i+2]+frame$Data[i+1]+frame$Data[i]+frame$Data[i-1]+frame$Data[i-2])/6
      n2<-(frame$Data[i+2]+frame$Data[i+1]+frame$Data[i]+frame$Data[i-1]+frame$Data[i-2]+frame$Data[i-3])/6
      frame$kmo[i]=(n1+n2)/2
    }
    
  }else if (frequency==3){
    
    for (i in 3:(length(data)-2)){
      n1<-(frame$Data[i+2]+frame$Data[i+1]+frame$Data[i]+frame$Data[i-1])/4
      n2<-(frame$Data[i+1]+frame$Data[i]+frame$Data[i-1]+frame$Data[i-2])/4
      frame$kmo[i]=(n1+n2)/2
    }
    
  }else if (frequency==4){
    
    for (i in 2:(length(data)-1)){
      frame$kmo[i]=(frame$Data[i+1]+frame$Data[i]+frame$Data[i-1])/3
    }
    
  }else if (frequency==12){
    
    for (i in 7:(length(data)-6)){
      n1<-(frame$Data[i+5]+frame$Data[i+4]+frame$Data[i+3]+frame$Data[i+2]+
             frame$Data[i+1]+frame$Data[i]+frame$Data[i-1]+frame$Data[i-2]+
             frame$Data[i-3]+frame$Data[i-4]+frame$Data[i-5]+frame$Data[i+6])/12
      n2<-(frame$Data[i+5]+frame$Data[i+4]+frame$Data[i+3]+frame$Data[i+2]+
             frame$Data[i+1]+frame$Data[i]+frame$Data[i-1]+frame$Data[i-2]+
             frame$Data[i-3]+frame$Data[i-4]+frame$Data[i-5]+frame$Data[i-6])/12
      frame$kmo[i]=(n1+n2)/2
    }
    
  }
  
  #Calculate SR and SI
  if (frequency>1){
    frame$LE<-frame$Data/frame$kmo
    LE<-matrix(data = NA, nrow = seasonality, ncol = 2, byrow = FALSE,dimnames = NULL)
    LE<-data.frame(LE); colnames(LE)<-c("ID","LE"); LE$ID<-c(1:seasonality)
    for (i in 1:seasonality){
      LE$LE[i]<-mean(frame$LE[ (frame$ID==i) & (is.na(frame$LE)==FALSE) & (frame$LE<max(frame$LE[(frame$ID==i)&(is.na(frame$LE)==FALSE)])) & (frame$LE>min(frame$LE[(frame$ID==i)&(is.na(frame$LE)==FALSE)])) ])
    }
    sndarize=mean(LE$LE)
    LE$LE<-LE$LE/sndarize 
    frame$kmo<-NA
    DE<-c(); DEref<-LE$LE
    for (i in 1:(length(data)%/%seasonality)){ DE<-c(DE,DEref) }
    DE<-c(DE,head(DEref,(length(data)%%seasonality))) 
    frame$Seasonality<-DE
  }
  
  frame$Deseasonalized<-frame$Data/frame$Seasonality
  
  #Calculate Randomness
  for (i in 2:(length(data)-1)){
    frame$kmo[i]=(frame$Deseasonalized[i+1]+frame$Deseasonalized[i]+frame$Deseasonalized[i-1])/3
  }
  frame$kmo[1]<-(2*frame$Deseasonalized[1]+frame$Deseasonalized[2])/3
  frame$kmo[length(data)]<-(2*frame$Deseasonalized[length(data)]+frame$Deseasonalized[length(data)-1])/3
  
  frame$kmo3<-NA
  for (i in 2:(length(data)-1)){
    frame$kmo3[i]=(frame$kmo[i+1]+frame$kmo[i]+frame$kmo[i-1])/3
  }
  frame$kmo3[1]<-(2*frame$kmo[1]+frame$kmo[2])/3
  frame$kmo3[length(data)]<-(2*frame$kmo[length(data)]+frame$kmo[length(data)-1])/3
  frame$Randomness<-frame$Deseasonalized/frame$kmo3
  frame$kmo3=frame$kmo=frame$ID<-NULL
  
  #Calculate Linear Trend and Cyrcle
  TC<-frame$Deseasonalized/frame$Randomness ; frame$Deseasonalized<-NULL
  xs<-c(1:length(data))
  frame$Trend<-as.numeric(predict(lm(TC~xs)))  
  frame$Cyrcle<-TC/frame$Trend
  # frame$LE<-LE$LE
  return(frame)	
}

