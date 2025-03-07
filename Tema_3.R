
# Chapter 3 Simple Population Models --------------------------------------
rm(list=ls())

## 3.1.1 The Discrete Logistic Model
#Code to produce Figure 3.1. Note the two one-line functions 
surprod<-function(Nt,r,K) return((r*Nt)*(1-(Nt/K)))
densdep<-function(Nt,K){
  return((1-(Nt/K)))
}
r<-1.2;Nt<-seq(10,1000,10);K<-1000
par(mfrow=c(2,1),mai=c(0.4,.4,.05,.05),oma=c(0,0,0,0))
par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
plot1(Nt,surprod(Nt,r,K),xlab="Population Nt",defpar=F,  
      ylab="Production") 
plot1(Nt,densdep(Nt,K),xlab="Population Nt",defpar=FALSE,  
      ylab="Density-Dependence") 
## 3.1.2 Dynamic Behaviour
#Code for Figure 3.2. Try varying the value of rv from 0.5-2.8  
yrs <- 100; rv=2.8;  Kv <- 1000.0; Nz=100; catch=0.0; p=1.0  
ans <- discretelogistic(r=rv,K=Kv,N0=Nz,Ct=catch,Yrs=yrs,p=p)  
avcatch <- mean(ans[(yrs-50):yrs,"nt"],na.rm=TRUE) #used in text  
label <- paste0("r=",rv," K=",Kv," Ct=",catch, " N0=",Nz," p=",p=p)  
plot(ans, main=label, cex=0.9, font=7) #Schaefer dynamics  
table(ans)
dinamic<-function (r = 0.5, K = 1000, N0 = 50, Ct = 0, Yrs = 50, p = 1)#Creamos esta funcion para visualizar la salida. Es igual que discretelogistic salvo en la salida que no es oculta
{
  yr1 <- Yrs + 1
  years <- seq(1, yr1, 1)
  pop <- numeric(yr1)
  pop[1] <- N0
  for (year in 2:yr1) {
    Bt <- pop[year - 1]
    pop[year] <- max((Bt + (r * Bt/p) * (1 - (Bt/K)^p) - 
                        Ct), 0)
  }
  pop2 <- pop[2:yr1]
  out <- cbind(year = years, nt = pop, nt1 = c(pop2, NA))
  rownames(out) <- years
  out <- out[-yr1, ]
  class(out) <- "dynpop"
  return(out)
}
dinamic(r=rv,K=Kv,N0=Nz,Ct=catch,Yrs=yrs,p=p)
#Lo hacemos para 600 años
yrs=600  
ans <- dinamic(r=2.2,K=1000.0,N0=100,Ct=0.0,Yrs=yrs) 
label <- paste0("r=",rv," K=",Kv," Ct=",catch, " N0=",Nz," p=",p=p)  
plot(ans, main=label, cex=0.9, font=7) #Schaefer dynamics  

##3.1.3 Finding Boundaries between Behaviours.
#searches for unique solutions given an r value  see Table 3.2
testseq <- seq(1.9,2.59,0.01)  
nseq <- length(testseq)  
result <- matrix(0,nrow=nseq,ncol=2,  
                 dimnames=list(testseq,c("r","Unique")))  
yrs <- 600  
for (i in 1:nseq) {  # i = 31  
  rval <- testseq[i]  
  ans <- discretelogistic(r=rval,K=1000.0,N0=100,Ct=0.0,Yrs=yrs)  
  ans <- ans[-yrs,] # remove last year, see str(ans) for why  
  ans[,"nt1"] <- round(ans[,"nt1"],3) #try hashing this out  
  result[i,] <- c(rval,length(unique(tail(ans[,"nt1"],100))))  
}  

##3.1.4 Classical Bifurcation Diagram of Chaos
#the R code for the bifurcation function  
bifurcation <- function(testseq,taill=100,yrs=1000,limy=0,incx=0.001){  
  nseq <- length(testseq)  
  result <- matrix(0,nrow=nseq,ncol=2,  
                   dimnames=list(testseq,c("r","Unique Values")))  
  result2 <- matrix(NA,nrow=nseq,ncol=taill)  
  for (i in 1:nseq) {    
    rval <- testseq[i]  
    ans <- discretelogistic(r=rval,K=1000.0,N0=100,Ct=0.0,Yrs=yrs)  
    ans[,"nt1"] <- round(ans[,"nt1"],4)  
    result[i,] <- c(rval,length(unique(tail(ans[,"nt1"],taill))))  
    result2[i,] <- tail(ans[,"nt1"],taill)  
  }    
  if (limy[1] == 0) limy <- c(0,getmax(result2,mult=1.02))  
  parset() # plot taill values against taill of each r value  
  plot(rep(testseq[1],taill),result2[1,],type="p",pch=16,cex=0.1,  
       ylim=limy,xlim=c(min(testseq)*(1-incx),max(testseq)*(1+incx)),  
       xlab="r value",yaxs="i",xaxs="i",ylab="Equilibrium Numbers",  
       panel.first=grid())  
  for (i in 2:nseq)  
    points(rep(testseq[i],taill),result2[i,],pch=16,cex=0.1)  
  return(invisible(list(result=result,result2=result2)))  
} # end of bifurcation  
#Alternative r value arrangements for you to try; Fig 3.3  
#testseq <- seq(2.847,2.855,0.00001) #hash/unhash as needed  
#bifurcation(testseq,limy=c(600,740),incx=0.0001) # t  
#testseq <- seq(2.6225,2.6375,0.00001) # then explore   
#bifurcation(testseq,limy=c(660,730),incx=0.0001)   
testseq <- seq(1.9,2.975,0.0005) # modify to explore  
bifurcation(testseq,limy=0) 

##3.1.5 The Effect of Fishing on Dynamics
#Effect of catches on stability properties of discretelogistic  
yrs=50; Kval=1000.0  
nocatch <- discretelogistic(r=2.56,K=Kval,N0=500,Ct=0,Yrs=yrs)  
catch50 <- discretelogistic(r=2.56,K=Kval,N0=500,Ct=50,Yrs=yrs)  
catch200 <- discretelogistic(r=2.56,K=Kval,N0=500,Ct=200,Yrs=yrs)  
catch300 <- discretelogistic(r=2.56,K=Kval,N0=500,Ct=300,Yrs=yrs)
#Effect of different catches on n-cyclic behaviour Fig3.4  
plottime <- function(x,ylab) {  
  yrs <- nrow(x)  
  plot1(x[,"year"],x[,"nt"],ylab=ylab,defpar=FALSE)  
  avB <- round(mean(x[(yrs-40):yrs,"nt"],na.rm=TRUE),3)  
  mtext(avB,side=1,outer=F,line=-1.1,font=7,cex=1.0)   
} # end of plottime  
#the oma argument is used to adjust the space around the graph  
par(mfrow=c(2,2),mai=c(0.25,0.4,0.05,0.05),oma=c(1.0,0,.2,0))   
par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)    
plottime(nocatch,"Catch = 0")  
plottime(catch50,"Catch = 50")  
plottime(catch200,"Catch = 200")  
plottime(catch300,"Catch = 300")  
mtext("years",side=1,outer=TRUE,line=-0.2,font=7,cex=1.0) 
#Phase plot for Schaefer model Fig 3.5  
plotphase <- function(x,label,ymax=0) { #x from discretelogistic  
  yrs <- nrow(x)  
  colnames(x) <- tolower(colnames(x))  
  if (ymax[1] == 0) ymax <- getmax(x[,c(2:3)])  
  plot(x[,"nt"],x[,"nt1"],type="p",pch=16,cex=1.0,ylim=c(0,ymax),  
       yaxs="i",xlim=c(0,ymax),xaxs="i",ylab="nt1",xlab="",  
       panel.first=grid(),col="darkgrey")  
  begin <- trunc(yrs * 0.6) #last 40% of yrs = 20, when yrs=50  
  points(x[begin:yrs,"nt"],x[begin:yrs,"nt1"],pch=18,col=1,cex=1.2)  
  mtext(label,side=1,outer=F,line=-1.1,font=7,cex=1.2)   
} # end of plotphase  
par(mfrow=c(2,2),mai=c(0.25,0.25,0.05,0.05),oma=c(1.0,1.0,0,0))   
par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)    
plotphase(nocatch,"Catch = 0",ymax=1300)  
plotphase(catch50,"Catch = 50",ymax=1300)  
plotphase(catch200,"Catch = 200",ymax=1300)  
plotphase(catch300,"Catch = 300",ymax=1300)  
mtext("nt",side=1,outer=T,line=0.0,font=7,cex=1.0)  
mtext("nt+1",side=2,outer=T,line=0.0,font=7,cex=1.0) 

##3.2.1 Survivorship in a Cohort
#Exponential population declines under different Z. Fig 3.6  
yrs <- 50;  yrs1 <- yrs + 1 # to leave room for B[0]  
years <- seq(0,yrs,1)  
B0 <- 1000        # now alternative total mortality rates  
Z <- c(0.05,0.1,0.2,0.4,0.55)   
nZ <- length(Z)  
Bt <- matrix(0,nrow=yrs1,ncol=nZ,dimnames=list(years,Z))  
Bt[1,] <- B0  
for (j in 1:nZ) for (i in 2:yrs1) Bt[i,j] <- Bt[(i-1),j]*exp(-Z[j])  
plot1(years,Bt[,1],xlab="Years",ylab="Population Size",lwd=2)  
if (nZ > 1) for (j in 2:nZ) lines(years,Bt[,j],lwd=2,col=j,lty=j)  
legend("topright",legend=paste0("Z = ",Z),col=1:nZ,lwd=3,  
       bty="n",cex=1,lty=1:5)
## 3.2.2 Instantaneous vs Annual Mortality Rates
#Prepare matrix of harvest rate vs time to appoximate F  
Z <- -log(0.5)  
timediv <- c(2,4,12,52,365,730,2920,8760,525600)  
yrfrac <- 1/timediv  
names(yrfrac) <- c("6mth","3mth","1mth","1wk","1d","12h",  
                   "3h","1h","1m")  
nfrac <- length(yrfrac)  
columns <- c("yrfrac","divisor","yrfracH","Remain")  
result <- matrix(0,nrow=nfrac,ncol=length(columns),  
                 dimnames=list(names(yrfrac),columns))  
for (i in 1:nfrac) {  
  timestepmort <- Z/timediv[i]   
  N <- 1000  
  #for (j in 1:timediv[i]) #N <- N * (1-timestepmort)
    N<-N*exp(-timestepmort)
  result[i,] <- c(yrfrac[i],timediv[i],timestepmort,N)  
}  
result
#Annual harvest rate against instantaneous F, Fig 3.7  
Fi <- seq(0.001,2,0.001)  
H <- 1 - exp(-Fi)  
parset()  # a wrapper for simplifying defining the par values  
plot(Fi,H,type="l",lwd=2,panel.first=grid(),  
     xlab="Instantaneous Fishing Mortality F",  
     ylab="Annual Proportion Mortality H")  
lines(c(0,1),c(0,1),lwd=2,lty=2,col=2)

## 3.3 Simple Yield per Recruit
# Simple Yield-per-Recruit see Russell (1942)  
age <- 1:11;  nage <- length(age); N0 <- 1000  # some definitions  
# weight-at-age values  
WaA <- c(NA,0.082,0.175,0.283,0.4,0.523,0.7,0.85,0.925,0.99,1.0)  
# now the harvest rates  
H <- c(0.01,0.06,0.11,0.16,0.21,0.26,0.31,0.36,0.55,0.8)  
nH <- length(H)  
NaA <- matrix(0,nrow=nage,ncol=nH,dimnames=list(age,H)) # storage  
CatchN <- NaA;  CatchW <- NaA      # define some storage matrices  
for (i in 1:nH) {                # loop through the harvest rates  
  NaA[1,i] <- N0  # start each harvest rate with initial numbers  
  for (age in 2:nage) {  # loop through over-simplified dynamics  
    NaA[age,i] <- NaA[(age-1),i] * (1 - H[i])  
    CatchN[age,i] <- NaA[(age-1),i] - NaA[age,i]  
  }  
  CatchW[,i] <- CatchN[,i] * WaA  
}                      # transpose the vector of total catches to  
totC <- t(colSums(CatchW,na.rm=TRUE))   # simplify later printing 
#Use MQMF::plot1 for a quick plot of the total catches. Figure 3.8  
plot1(H,totC,xlab="Harvest Rate",ylab="Total Yield",lwd=2) 

## 3.3.1 Selectivity in Yield-per-Recruit
#Logistic S shaped cureve for maturity  
ages <- seq(0,50,1)  
sel1 <- mature(-3.650425,0.146017,sizeage=ages) #-3.65/0.146=25  
sel2 <- mature(-6,0.2,ages)  
sel3 <- mature(-6,0.24,ages)  
plot1(ages,sel1,xlab="Age Yrs",ylab="Selectivity",cex=0.75,lwd=2)  
lines(ages,sel2,col=2,lwd=2,lty=2)  
lines(ages,sel3,col=3,lwd=2,lty=3)  
abline(v=25,col="grey",lty=2)   
abline(h=c(0.25,0.5,0.75),col="grey",lty=2)  
legend("topleft",c("25_15.04","30_10.986","25_9.155"),col=c(1,2,3),  
       lwd=3,cex=1.1,bty="n",lty=1:3)

## 3.3.2 The Baranov Catch Equation
# Baranov catch equation  
age <- 0:12;  nage <- length(age)   
sa <-mature(-4,2,age) #selectivity-at-age  
H <- 0.2;  M <- 0.35  
FF <- -log(1 - H)#Fully selected instantaneous fishing mortality  
Ft <- sa * FF     # instantaneous Fishing mortality-at-age  
N0 <- 1000  
out <- cbind(bce(M,Ft,N0,age),"Select"=sa)  # out becomes Table 3.7 

## 3.4 Full Yield-per-Recruit
# A more complete YPR analysis  
age <- 0:20;  nage <- length(age) #storage vectors and matrices  
laa <- vB(c(50.0,0.25,-1.5),age) # length-at-age  
WaA <- (0.015 * laa ^ 3.0)/1000  # weight-at-age as kg  
H <- seq(0.01,0.65,0.05);  nH <- length(H)     
FF <- round(-log(1 - H),5)  # Fully selected fishing mortality  
N0 <- 1000  
M <- 0.1  
numt <- matrix(0,nrow=nage,ncol=nH,dimnames=list(age,FF))  
catchN <- matrix(0,nrow=nage,ncol=nH,dimnames=list(age,FF))  
as50 <- c(1,2,3)    
yield <- matrix(0,nrow=nH,ncol=length(as50),dimnames=list(H,as50))  
for (sel in 1:length(as50)) {  
  sa <- logist(as50[sel],1.0,age)  # selectivity-at-age  
  for (harv in 1:nH) {  
    Ft <- sa * FF[harv]      # Fishing mortality-at-age  
    out <- bce(M,Ft,N0,age)  
    numt[,harv] <- out[,"Nt"]  
    catchN[,harv] <- out[,"Catch"]  
    yield[harv,sel] <- sum(out[,"Catch"] * WaA,na.rm=TRUE)  
  } # end of harv loop  
} # end of sel loop  

#A full YPR analysis  Figure 3.10  
plot1(H,yield[,3],xlab="Harvest Rate",ylab="Yield",cex=0.75,lwd=2)  
lines(H,yield[,2],lwd=2,col=2,lty=2)  
lines(H,yield[,1],lwd=2,col=3,lty=3)  
legend("bottomright",legend=as50,col=c(3,2,1),lwd=3,bty="n",  
       cex=1.0,lty=c(3,2,1)) 
