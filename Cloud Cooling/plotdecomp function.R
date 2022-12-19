source('plotts2 function.R')
source('decompts function.R')
# source("Willis Functions.R")
library(robustbase)
# tser=rutts
seaslm=function(tser){

  tspvals=tsp(tser)
  if (length(which(is.na(tser)))>0) {
    navalues=min(which(is.na(tser)))
    tser=ts(tser[1:(navalues-1)],
                  start=tspvals[1],frequency=tspvals[3])
  }
  timevalues=time(tser)
  seasonal=stl(tser,"periodic")$time.series[,1]

  newtime=tser-seasonal

  summary(lm(newtime~timevalues))$coeff
}




titletext="Dummy\nTwo Line"

# tser=surfnino
varname="aVar"
theunits="W/m2"
roundto=2
drawyears=TRUE
printloess=TRUE
as.percent=FALSE
# tser=meits
titletext="Dummy\nTwo Line"
varname="aVar"
theunits="W/m2"
roundto=2
drawyears=TRUE
printloess=TRUE
keyspan=.4
as.percent=FALSE
plotstart=c(2008,1);plotend=c(2012,1)
# tser=toaswmon
# tser=rutts
# titletext="Dummy\nTwo Line"
# plotstart=NA
# plotend=NA
# varname="aVar"
# theunits="W/m2"
# roundto=2
# drawyears=TRUE
# printloess=TRUE
# keyspan=.4
# as.percent=FALSE
# printtrend=T
# mylwd=1.5
# majorlines=NA
# lessmean=F
# doplot=T

# tser=co2full
plotdecomp=function(tser,
                    titletext="Dummy\nTwo Line",
                    plotstart=NA,
                    plotend=NA,
                    varname="aVar",
                    theunits="W/m2",
                    roundto=2,
                    drawyears=TRUE,
                    printloess=TRUE,
                    keyspan=.4,
                    as.percent=FALSE,
                    printtrend=T,
                    mylwd=1.5,
                    majorlines=NA,
                    lessmean=F,
                    doplot=T,...){
  old.cex.axis=par("cex.axis")
  old.mai=par("mai")
  old.mgp=par("mgp")
  par(cex.axis=1.3,mai=c( 1.55, 1.30, 1.00, 0.25),mgp=c(1.8,1,0))
  if (length(dim(tser))==3) tser=getmonths(tser)
  if (length(plotstart)==1) plotstart=start(tser)
  if (length(plotend)==1) plotend=end(tser)
  thestats=seaslm(tser)
  thedecomp=decompts(tser)
  plotstl=thedecomp[[1]]
  plotgauss=thedecomp[[2]]
  plotstl=window(plotstl,start=plotstart,end=plotend)
  tser=window(tser,start=plotstart,end=plotend)

  if (as.percent==FALSE){
    Data=cbind(tser,
               plotstl[,1]+mean(tser,na.rm=T),
               tser-plotstl[,1]-ifelse(lessmean,mean(tser,na.rm=T),0))
    colnames(Data)=c(paste0(varname," ( ",theunits," )"),
                     paste0("Seasonal Component ( ",theunits," )"),
                     paste("Data minus Seasonal (",theunits,")\n(gold lines ± 1 σ, blue line is gaussian)"))
  } else{
    Data=cbind(tser,
               plotstl[,1]+mean(tser,na.rm=T),
               (tser-plotstl[,1]-mean(tser,na.rm=T))/
                 (mean(tser,na.rm=TRUE)+plotstl[,1])*100)
    colnames(Data)=c(paste0(varname," ( ",theunits," )"),
                     paste0("Seasonal Component ( ",theunits," )"),
                     "Data minus Seasonal as %\n(gold lines ± 1 σ, blue line is gaussian)")

  }
  if (doplot==T){
    plot.ts2(Data,main="",yax.flip=TRUE,cex.lab=.9,lwd=3,
             xlab="",
             printloess=printloess,majorlines = majorlines,
             oma.multi = c(6,.7, 5, 0),drawyears=drawyears,
             keyspan=keyspan)
    thetitle=paste0(titletext,"\nSeasonal Decomposition")
    title(main=thetitle,
          line=2.3,cex.main=.85,adj=.3)
    par(cex.axis=1)
    sprinttext=paste0("%.",roundto,"f")
    sprinttext2=paste0("%.",roundto+1,"f")
    subtext=paste0("(Trend = ",sprintf(sprinttext,thestats[2,1]*10),
                   " ± ",
                   sprintf(sprinttext,thestats[2,2]*10*hurstfactor(Data[,3])),
                   " ",theunits,
                   " per decade, p-value = ",
                   sprintf(sprinttext2,
                           hurstp(as.vector(Data[,3]))$coeff[2,4]),
                   ")")
    if (printtrend) title(sub=subtext,line=5.2,cex.sub=.8,adj=.2)
  }

  myloess=ts(loess(tser~as.vector(time(tser)),span=keyspan)$fitted,
             start=start(tser),frequency=frequency(tser))
  myloess=plotgauss
  thereturn=cbind(time(tser),Data,myloess)
  colnames(thereturn)=c("Date","Data","Seasonal","Residual","Gaussian")

  par(cex.axis=old.cex.axis,mai=old.mai,mgp=old.mgp)

  invisible(thereturn)

}





seasonal=function(tser){
  plotdecomp(tser,doplot=F)[,"Seasonal"]
}

residual=function(tser,remove_mean=F){
 residint(residint(residint(tser)))-
    ifelse(remove_mean,mean(tser,na.rm=T),0)
}

residint=function(tser){
  plotdecomp(tser,doplot=F)[,"Residual"]
}
