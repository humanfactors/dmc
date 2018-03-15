####################### LBA go-nogo ----
require(rtdists) 

  
  rlba.normgng <- function (n, A, b, t0, mean_v, sd_v, st0 = 0, posdrift = TRUE) 
    # Race among length(mean_v) accumulators, first of which is a no-go 
    # accumulator. For trials with winning first accumulator RT set to NA. 
  {
    out <- rlba.norm(n,A,b,t0,mean_v,sd_v,st0,posdrift)
    out[out$response==1,"rt"] <- NA
    out$response <- factor(as.numeric(out$response))
    data.frame(out)
  }
  
  
  
  n1PDFfixedt0.normgng=function(dt,A,b,mean_v,sd_v,st0=0,posdrift=TRUE) 
    # Same as n1PDFfixedt0 except dt=NA done by integration
  {
    
    stopfn <- function(t,A,b,mean_v,sd_v,st0=0,posdrift=TRUE) 
    {
      n1PDFfixedt0.norm(
        matrix(rep(t,each=length(mean_v)),nrow=length(mean_v)),
        A=A,b=b,mean_v=mean_v,sd_v=sd_v,posdrift=posdrift
      )
    }
    
    n.trials <- dim(dt)[2]
    out <- numeric(n.trials)
    is.stop <- is.na(dt[1,])
    if (any(!is.stop)) dt[1,!is.stop] <- n1PDFfixedt0.norm(dt[,!is.stop,drop=F],
                                                           A=A,b=b,mean_v=mean_v,sd_v=sd_v,posdrift=posdrift)
    # tmp <- try(integrate(f=stopfn,lower=0,upper=Inf,
    #     A=A,b=b,mean_v=mean_v,sd_v=sd_v,posdrift=posdrift)$value,silent=TRUE)
    # if (!is.numeric(tmp)) tmp <- 0
    tmp <- my.integrate(f=stopfn,lower=0,
                        A=A,b=b,mean_v=mean_v,sd_v=sd_v,posdrift=posdrift)
    dt[1,is.na(dt[1,])] <- tmp 
    dt[1,]
  }
  
  n1PDF.normgng <- function(dt,A,b,t0,mean_v,sd_v,st0=0,posdrift=TRUE)
    # Same as n1PDF except NAs dealt with by integration
  {
    
    if ( st0 < 1e-3 ) # smaller values can cause numerical integration problems
      return(n1PDFfixedt0.normgng(
        A=A,b=b,mean_v=mean_v,sd_v=sd_v,posdrift=posdrift,
        dt=matrix(pmax(rep(dt,each=length(mean_v))-t0,0),nrow=length(mean_v))
      )) else
      {    
        
        integrate.f <- function(A,b,t0,mean_v,sd_v,st0,posdrift) 
          n1PDFfixedt0.normgng(
            A=A,b=b,mean_v=mean_v,sd_v=sd_v,posdrift=posdrift,
            dt=matrix(pmax(rep(dt,each=length(mean_v))-t0,0),
                      nrow=length(mean_v)))/st0
        
        outs <- numeric(length(dt))
        for (i in 1:length(outs)) {
          tmp <- try(integrate(f=integrate.f,
                               lower=dt[i]-st0[1],upper=dt[i],A=A,b=b,t0=t0,mean_v=mean_v,sd_v=sd_v,
                               st0=st0[1],posdrift=posdrift)$value,silent=TRUE)
          if (is.numeric(tmp)) 
            outs[i] <- tmp else
              outs[i] <- 0
        }
        return(outs)
      }
  }
  
  
  # # Check
  # 
  # # 2 Choice case
  # n=1e5
  # A=c(1,1);b=c(2,2);t0=.2;mean_v=c(1,0);sd_v=c(1,1);st0=0;posdrift=TRUE
  # sim <- rlba.normgng(n,A,b,t0,mean_v,sd_v,st0,posdrift)
  # names(sim) <- c("RT","R")
  # dns <- plot.cell.density(sim,xlim=c(0,7),save.density=TRUE)
  # d <- n1PDF.normgng(dt=dns$'2'$x,A=A[2:1],b=b[2:1],mean_v=mean_v[2:1],
  #   sd_v=sd_v[2:1],t0=t0,st0=st0,posdrift=posdrift)
  # # d <- n1PDF.normgng(dt=dns$'2'$x,A=A,b=b,mean_v=mean_v,
  # #   sd_v=sd_v,t0=t0,st0=st0,posdrift=posdrift)
  # 
  # lines(dns$'2'$x,d,col="red")
  # 
  # # p(Stop check)
  # mean(is.na(sim$RT))
  # n1PDFfixedt0.normgng(dt=matrix(rep(NA,2),ncol=1),A=A,b=b,
  #                      mean_v=mean_v,sd_v=sd_v)
  # 
  # # 3 Choice case
  # n=1e5
  # A=c(1,1,1);b=c(2,2,2);t0=.2;mean_v=c(1,.5,0);sd_v=c(1,1,1);st0=0;posdrift=TRUE
  # sim <- rlba.normgng(n,A,b,t0,mean_v,sd_v,st0,posdrift)
  # names(sim) <- c("RT","R")
  # dns <- plot.cell.density(sim,xlim=c(0,3),save.density=TRUE)
  # d2 <- n1PDF.normgng(dt=dns$'2'$x,A=A[c(2,3,1)],b=b[c(2,3,1)],mean_v=mean_v[c(2,3,1)],
  #   sd_v=sd_v[c(2,3,1)],t0=t0,st0=st0,posdrift=posdrift)
  # lines(dns$'2'$x,d2,col="red")
  # d3 <- n1PDF.normgng(dt=dns$'3'$x,A=A[3:1],b=b[3:1],mean_v=mean_v[3:1],
  #   sd_v=sd_v[3:1],t0=t0,st0=st0,posdrift=posdrift)
  # lines(dns$'3'$x,d3,col="red",lty=2)
  # 
  # 
  # # p(Stop check)
  # mean(is.na(sim$RT))
  # n1PDFfixedt0.normgng(dt=matrix(rep(NA,3),ncol=1),A=A,b=b,
  #                      mean_v=mean_v,sd_v=sd_v)
  
