

power=0.8
c=1E-8 #rec per bp
rec<-function(power,gens,c){
 return(log(power)/(gens*log(1-c)))  
}

length1=sapply(1:2000*100,function(x) rec(power,x,c/10))/1000
length2=sapply(1:2000*100,function(x) rec(power,x,c))/1000
length3=sapply(1:2000*100,function(x) rec(power,x,c*10))/1000
g=1:2000*100/1000
plot(length1~g,log="y",type="line",lty=1,lwd=2,xlab="Time (thousands of years)",cex.lab=1.5,ylab="Length (kb)",ylim=c(0.1,2000),yaxt="n",cex.axis=1.25)
axis(side=2,at=c(.1,1,5,10,50,100,500),labels=c(.1,1,5,10,50,100,500),cex.axis=1.25)
legend("top",lty=c(1,2,3),legend=c(0.1,1,10),box.lwd=0,title="cM/Mb",lwd=2)
lines(length2~g,log="y",type="line",lty=2,lwd=2)
lines(length3~g,log="y",type="line",lty=3,lwd=2)
 
par(mar=c(5,5,3,3))
plot(length1~g,log="y",type="line",lty=1,lwd=3,ylab="Length (kb)",cex.lab=2,xlab="Time (ky)",ylim=c(0.1,2000),yaxt="n",cex.axis=1.25,xlim=c(0,5))
axis(side=2,at=c(.1,1,5,10,50,100,500),labels=c(.1,1,5,10,50,100,500),cex.axis=1.25)
lines(length2~g,log="y",type="line",lty=2,lwd=3)
lines(length3~g,log="y",type="line",lty=3,lwd=3)

