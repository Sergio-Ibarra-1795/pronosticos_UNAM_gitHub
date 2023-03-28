#Código para descomponer series y ver cada 

par(mfrow=c(3,2))

mes=c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")

for (j in 1:12)
{
  
  bp=window(hsbcdep,start=c(2005,j), deltat=1)
  plot(bp, main=paste("mes",mes[j]),type="o",ylimit=c(70000,400000))
  
}
j=12
bp=window(hsbcdep,start=c(2005,j), deltat=1)
plot(bp, main=paste("mes",mes[j]),type="o",ylimit=c(70000,400000))

tiempo=c(1:18)

ajuste1=lm(bp~1+tiempo)       #y=beta0+beta1*x
summary(ajuste1)
plot(ajuste1)
serie=as.data.frame(tiempo,bp)

bp=as.vector(bp)
length(bp)
summary(bp)
is(bp)
plot(bp, type="o")
bp
summary(dep_hsbc$depositos.HSBC)
dim(dep_hsbc)  
electime=1:142
elecseason[1:24,]

elecseason=model.matrix(~-1+factor(cycle(elecUS)))[,-1]
dimnames(elecseason)[[2]]=c("feb","mar","apr","may","jun",
                            "jul","aug","sep","oct","nov","dec")

elecUS.lm=lm(elecUS~electime+elecseason)
summary(elecUS.lm)