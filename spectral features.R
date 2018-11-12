#load emuR library
library(emuR)
KH <- load_emuDB("C:\\rec\\KH_emuDB")
#create a query for vowels
q1_KH <- emuR::query(KH,query="[MAU==shortvowels]")

#creating DFT spectrum for the trackdata

q1_KH_DFT <- get_trackdata(KH,seglist = q1_KH,ssffTrackName= "dft")
#cut the spectrum 
q1_KH_DFT_cut <- dcut(q1_KH_DFT, .5,prop=TRUE)
View(q1_KH_DFT_cut)
q1_KH_labs <- label(q1_KH)

plot(q1_KH_DFT_cut[1,],q1_KH_labs[1])
fapply(q1_KH_DFT_cut[,0:8000],mean)
#to find the high frequency energy ratio


  s1000=fapply(q1_KH_DFT_cut[,0:1000],sum,power=T)
  stotal=fapply(q1_KH_DFT_cut,sum,power=T)
  

ratio<- function(tot,lower)
{
  s=tot-lower
  ratio1=s/tot
  return(ratio1)
} 
o <- ratio(stotal,s1000)
write.csv(o,file="C:\\rec\\KH\\power.csv",append = T,sep = ',',row.names= F,col.names = F)



#function to find out the peak values
 
    peaks <- function(series, span = 3, do.pad = TRUE) {
      if((span <- as.integer(span)) %% 2 != 1) stop("'span' must be odd")
        s1 <- 1:1 + (s <- span %/% 2)
        if(span == 1) return(rep.int(TRUE, length(series)))
          z <- embed(series, span)
        v <- apply(z[,s1] > z[, -s1, drop=FALSE], 1, all)
         if(do.pad) {
              pad <- rep.int(FALSE, s)
               c(pad, v, pad)
           } else v
    }
    
  #applying the peak function to all segments
    for(i in 1:828){
    df <- q1_KH_DFT_cut[i,]
    temp <- as.vector(df)
    temp1 <- peaks(temp,11,TRUE)
    which(temp1==TRUE)
    t <- which(temp1==TRUE)
    s <- t[1:3]
    f <- (s-1)*31.25
    p <- as.vector(df)
    p1 <- p[s]
    h1 <- slope1(p1[1],p1[2],f[1],f[2])
    h2 <- slope2(p1[2],p1[3],f[2],f[3])
    hh[i] <- as.array(h1)
    hhh2[i] <- as.array(h2)       
    }
    write.csv(hh,file="C:\\rec\\KH\\test.csv",append = T,sep = ',',row.names= F,col.names = F)
    write.csv(hhh2,file="C:\\rec\\KH\\test1.csv",append = T,sep = ',',row.names= F,col.names = F)
    #function to calculate slope1 value
    slope1<- function(p,q,r,s)
    {
      h1slope=((p-q)/(r-s))
      return(h1slope)
    }
    #function to calculate slope2 values
    slope2<-function(p1,q1,r1,s1)
    {
      h2slope=((p1-q1)/(r1-s1))
      return(h2slope)
    }
    utt(q1_KH[1,])
    speakers <- substring(utt(q1_KH),1,2)
    p_types <- substring(utt(q1_KH),7,8)
    table(substring(utt(q1_KH),1,2))
    (substring(utt(q1_KH),7,8))
    table(substring(utt(q1_KH),4,8))
    
    write.csv(speakers,file="C:\\rec\\KH\\speakers.csv",append = T,sep = ',',row.names= F,col.names = F)
    write.csv(p_types,file="C:\\rec\\KH\\p_types.csv",append = T,sep = ',',row.names= F,col.names = F)
    