#Transformaciones
#tiempo 
#concentracion

tmn=c(3,4,10,15,20,30,40,50,60,75,90)
con=c(25.5,23.4,18.2,14.2,11,6.7,4.1,2.5,1.5,0.7,0.4)

plot(con,tmn, type="l")
cor(con,tmn)

ester=cbind(tmn,con)
ester

ester=as.data.frame(ester)
cor(ester)
#sin transformacion

ajuste_lineal=lm(con~1+tmn, data=ester) 
ajuste_lineal#y=beta0+beta1*x
#ajuste2=lm(demoxi~ -1 + redsol, data=tabdat) #y=beta1*x

summary(ajuste_lineal)

plot(ajuste_lineal)



#transformada, es una relacion exponencial, va mejor un logaritmo

logcon=log(con)
logcon

ester=cbind(tmn,con,logcon)
ester


ester=as.data.frame(ester)
ester

ajuste2=lm(logcon~1+tmn, data=ester)
ajuste2


summary(ajuste2)
plot(ajuste2)

#resultados-regresar(destransformar)

#exp(logcon=con=exp(3.3652700-0.0486451 tmn))



