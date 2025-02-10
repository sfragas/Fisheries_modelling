# TEMA 2 ------------------------------------------------------------------
file.edit(".Rprofile")

## 2.2.4 Examining Code within Functions
getS3method("print","default")
getS3method("print","table")
library(MQMF)
ls("package:MQMF")
getNamespaceExports("MQMF")#Aunque no se haya cargado el paquete se pueden ver las funciones del paquete
##2.2.5 Using Functions
#make a function called countones2, don't overwrite original  
countones2 <- function(x) return(length(which(x == 1)))  # or  
countones3 <- function(x) return(length(x[x == 1]))  
vect <- c(1,2,3,1,2,3,1,2,3)  # there are three ones  
countones2(vect)  # should both give the answer: 3  
countones3(vect)  
set.seed(7100809) # if repeatability is desirable.   
matdat <- matrix(trunc(runif(40)*10),nrow=5,ncol=8)  
matdat #a five by eight matrix of random numbers between 0 - 9  
apply(matdat,2,countones3)  # apply countones3 to 8 columns  
apply(matdat,1,countones3)  # apply countones3 to 5 rows
#Openning window outside of Rstudio
plotprep2 <- function(plots=c(1,1),width=6, height=3.75,usefont=7,   
                      newdev=TRUE) {  
  if ((names(dev.cur()) %in% c("null device","RStudioGD")) &   
      (newdev)) {  
    dev.new(width=width,height=height,noRStudioGD = TRUE)  
  }  
  par(mfrow=plots,mai=c(0.45,0.45,0.1,0.05),oma=c(0,0,0,0))  
  par(cex=0.75,mgp=c(1.35,0.35,0),font.axis=usefont,font=usefont,   
      font.lab=usefont)  
}  #  see ?plotprep; see also parsyn() and parset() 
## 2.2.6 Random Number Generation
RNGkind()
getseed()#Genera numeros para comenzar una semilla
##2.2.8 Plotting in R
#library(MQMF)   # The development of a simple graph  see Fig. 2.1
data("LatA")  #LatA = length at age data; try properties(LatA)  
#The statements below open the RStudio graphics window, but opening  
#a separate graphics window using plotprep is sometimes clearer.  
#plotprep(width=6.0,height=5.0,newdev=FALSE)   
setpalette("R4") #a more balanced, default palette see its help  
par(mfrow=c(2,2),mai=c(0.45,0.45,0.1,0.05))  # see ?parsyn  
par(cex=0.75, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)   
hist(LatA$age) #examine effect of different input parameters  
hist(LatA$age,breaks=20,col=3,main="") # 3=green #try ?hist  
hist(LatA$age,breaks=30,main="",col=4) # 4=blue  
hist(LatA$age, breaks=30,col=2, main="", xlim=c(0,43), #2=red  
     xlab="Age (years)",ylab="Count")
##2.2.9 Dealing with Factors
DepCat <- as.factor(rep(seq(300,600,50),2)); DepCat 
try(5 * DepCat[3], silent=FALSE) #only returns NA and a warning!  
try(5 * facttonum(DepCat[3]), silent=FALSE)# transformamos factor en numerico 
as.numeric(DepCat) # returns the levels not the original values 
as.numeric(levels(DepCat)) #converts 7 levels not the replicates 
DepCat <- as.numeric(levels(DepCat))[DepCat] # try ?facttonum  
#converts replicates in DepCat to numbers, not just the levels   
5 * DepCat[3]   # now treat DepCat as numeric  

#2.3 Writing Functions

##2.3.1 Simple functions
# Implement the von Bertalanffy curve in multiple ways  
ages <- 1:20 
nages <- length(ages)  
Linf <- 50;  K <- 0.2;  t0 <- -0.75  
# first try a for loop to calculate length for each age  
loopLt <- numeric(nages)  
for (ag in ages) loopLt[ag] <- Linf * (1 - exp(-K * (ag - t0)))  
# the equations are automatically vectorized so more efficient  
vecLt <- Linf * (1 - exp(-K * (ages - t0))) # or we can convert   
# the equation into a function and use it again and again  
vB <- function(pars,inages) { # requires pars=c(Linf,K,t0)  
  return(pars[1] * (1 - exp(-pars[2] * (inages - pars[3]))))  
}  
funLt <- vB(c(Linf,K,t0),ages)  
ans <- cbind(ages,funLt,vecLt,loopLt) 
ans
# Vectorizacion es mas eficiente que los loops
#A vB function with some error checking  
vB <- function(pars,inages) { # requires pars=c(Linf,K,t0)  
  if (is.numeric(pars) & is.numeric(inages)) {  
    Lt <- pars[1] * (1 - exp(-pars[2] * (inages - pars[3])))  
  } else { stop(cat("Not all input values are numeric! \n")) }  
  return(Lt)  
}  
param <- c(50, 0.2,"-0.75")  
funLt <- vB(as.numeric(param),ages) #try without the as.numeric  
vB(param,ages) #Error por no ser numericos los valores de los parametros
halftable(cbind(ages,funLt)) 
## 2.3.4 Scoping of Objects
# demonstration that the global environment is 'visible' inside a  
# a function it calls, but the function's environment remains  
# invisible to the global or calling environment  
vBscope <- function(pars) { # requires pars=c(Linf,K,t0)  
  rhside <- (1 - exp(-pars[2] * (ages - pars[3])))  
  Lt <- pars[1] * rhside  
  return(Lt)  
}  
ages <- 1:10; param <- c(50,0.2,-0.75)  
vBscope(param)  
try(rhside)    # note the use of try() which can trap errors ?try

## 2.3.5 Function Inputs and Outputs

#Bring the data-set schaef into the working of global environment  
data(schaef)  
#examine the properties of the data-set schaef  
class(schaef) 
a<- schaef[1:5,2]
b<-schaef[1:5,"catch"]
c<-schaef$catch[1:5]
cbind(a,b,c)
mschaef<-as.matrix(schaef)
mschaef[1:5,"catch"]
class(mschaef)
mschaef[1:2,1:2]
d<-try(mschaef$catch[1:5])#invalid for matrices
colnames(mschaef)
rownames(mschaef)
#Convert column names of a data.frame or matrix to lowercase  
dolittle <- function(indat) {  
  indat1 <- as.data.frame(indat)  
  colnames(indat) <- tolower(colnames(indat))  
  return(list(dfdata=indat1,indat=as.matrix(indat)))  
} # return the original and the new version  
colnames(schaef) <- toupper(colnames(schaef))  
out <- dolittle(schaef)
str(out)
str(out, width=63, strict.width="cut")
#Could have used an S3 plot method had we defined a class   Fig.2.2 
plotspmdat(schaef) # examine the code as an eg of a custom plot  

##2.4 Appendix: Less-Traveled Functions
getNamespaceExports("MQMF")
ls("package:MQMF")
ls.str("package:MQMF")
methods("print")
packageDescription("MQMF")
RNGkind()
sessionInfo()
sink("filename",split=TRUE)
suppressWarnings(sink())
zz <- file("all.Rout", open = "wt")
class(zz)
