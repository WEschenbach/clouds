# timeseries=thehad
# mystl=stl(thehad,"periodic")
# class(mystl$time.series)
# names(mystl)


gfilt = function(x){
  l=2*(x)+1#fwhm2g
  temp=c(0,pnorm(-3+(6/l)*(1:l)))
  diff(temp)/sum(diff(temp))
  #   temp1=temp
}

# gfilt(3)
realgauss=function(xline,myfilt=myfilt)  {sum(xline*myfilt,na.rm=TRUE)/sum(myfilt[which(!is.na(xline))])}

gaussian=function(x,gauss=0){
  if (gauss==0) {x} else{
    myfilt=gfilt(gauss)
    xlong=c(rep(NA,gauss),x,rep(NA,gauss))
    mywid=2*gauss+1
    gmatrix=matrix(NA,length(x),mywid)
    rowmatrix=row(gmatrix)+col(gmatrix)-1
    gmatrix[,]=(xlong[rowmatrix[,]])

    result=apply(gmatrix,1,realgauss,myfilt=myfilt)
    if (class(x) == "zoo") result=zoo(result,time(x))
    if (class(x) == "ts") result=ts(result,start=start(x),frequency=frequency(x))
    result[which(is.na(x))]=NA
    result
  }
}

gaussiants=function(timeseries, gauss=0) {
  ts(gaussian(timeseries, gauss),
     start=start(timeseries),
     frequency=frequency(timeseries))
}

# timeseries=sfotide
# decompts(sfotide,doplot=TRUE)
decompts=function(timeseries,doplot=FALSE,main="",cex.main=1,thegauss=NA){
  if (is.na(thegauss)) thegauss=frequency(timeseries)*4
  mygauss=gaussiants(timeseries,thegauss)
  #   plot(timeseries)
  #   lines(mygauss,col="red")
  timeseriesred=tapply(timeseries-mygauss,cycle(timeseries),mean,na.rm=TRUE)
  themeans=ts(timeseriesred[cycle(timeseries)],start=start(timeseries),frequency=frequency(timeseries))
  theresid=timeseries-themeans
  theresult=cbind(themeans,mygauss,timeseries-mygauss)
  colnames(theresult)=c("Seasonal","Gaussian Trend","Residual")
  if (doplot) {
    plot(theresult,yax.flip=TRUE,main="")
    title(main=paste0("Periodic Decomposition\n",main),line=1,cex.main=cex.main)
  }
  list(theresult,mygauss)
}

