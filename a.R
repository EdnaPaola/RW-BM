library("ggplot2")
library("reshape2")
start<-0
end<-1
size<-.01
i=20
N<-10000
pn<-1/2

Yt<-function(x,i){1/sqrt(N)*df[trunc(N*x)+1,i]}


  columns<-c("Step",1:i)
  df = data.frame(matrix(nrow = N+1, ncol = length(columns))) 
  colnames(df) = columns
  df$Step<-c(0:N)
  for(j in 1:i){
    e<-rbinom(N,1,pn)*2-1 
    df[,j+1]<-c(append(cumsum(e),0,after=0))
  }
  df
  df_long <- melt(df, id= c("Step")) 
  ggplot(data = df_long, aes(x = Step, y = value, color = variable)) +
    geom_step()+theme(legend.position="none")+ ggtitle('Random Walks')
  
  dfb<-data.frame(matrix(nrow = length(seq(start,end,size)), ncol = length(columns))) 
  colnames(dfb) = columns
  t<-seq(start,end,size)
  dfb$Step<-t
  for(j in 1:i){
    
    dfb[,j+1]<-Yt(dfb$Step,j+1) 
  }
  dfb_long <- melt(dfb, id= c("Step")) 
  ggplot(data = dfb_long, aes(x = Step, y = value, color = variable)) +
    geom_line()+theme(legend.position="none")+ ggtitle('Brownian Approx')

##comparar con graficas
  f<-sqrt(t)
  f2<- -sqrt(t)
  dfc<-data.frame(dfb,f,f2)

  dfc_long <- melt(dfc, id= c("Step")) 
  ggplot(data = dfc_long, aes(x = Step, y = value, color = variable)) +
    geom_line()+theme(legend.position="none")+ 
    scale_color_manual(values = c(rep("gray", 20),"blue","blue")) + 
    ggtitle('Brownian Approx Comparison')
