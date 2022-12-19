
# require("dvmisc")
require("mapdata",warn.conflicts = T)
require("mapproj",warn.conflicts = T)
require("maps",warn.conflicts = T)
require("RNetCDF",warn.conflicts = T)
require("robustbase",warn.conflicts = T)
require("abind",warn.conflicts = T)
require("lubridate",warn.conflicts = T)
require("pbapply",warn.conflicts = T)


latlength=function(x) earthradm*dcos(x)

polemask=matrix(1,180,360)
polemask[c(1:24,157:180),]=NA

nhmask=matrix(1,180,360)
nhmask[c(1:90),]=NA
shmask=matrix(1,180,360)
shmask[c(91:180),]=NA


# new map -----------------------------------------------------------------
# http://stackoverflow.com/questions/5353184/fixing-maps-library-data-for-pacific-centred-0-360-longitude-display
#
plot.map<- function(database,central,...){
  Obj <- maps::map(database,...,plot=F)
  coord <- cbind(Obj[[1]],Obj[[2]])

  # split up the coordinates
  id <- rle(!is.na(coord[,1]))
  id <- matrix(c(1,cumsum(id$lengths)),ncol=2,byrow=T)
  polygons <- apply(id,1,function(i){coord[i[1]:i[2],]})

  # split up polygons that differ too much
  polygons <- lapply(polygons,function(x){
    x[,1] <- x[,1] + central
    x[,1] <- ifelse(x[,1]>180,x[,1]-360,x[,1])
    thediff=abs(diff(x[,1]));thediff
    if(length(which(is.finite(thediff)))>=1){
      # print(max(thediff))
      outcome=max(thediff,na.rm=T)>170
      if(outcome){
        id <- x[,1] < 0
        x <- rbind(x[id,],c(NA,NA),x[!id,])
      }
      x
    }
  })

  # reconstruct the object
  polygons <- do.call(rbind,polygons)
  Obj[[1]] <- polygons[,1]
  Obj[[2]] <- polygons[,2]

  maps::map(Obj,...)
}

redo.map<- function(database,central=0,...){
  Obj <- maps::map(database,plot=F)
  coord <- cbind(Obj[[1]],Obj[[2]])

  # split up the coordinates
  id <- rle(!is.na(coord[,1]))
  id <- matrix(c(1,cumsum(id$lengths)),ncol=2,byrow=T)
  polygons <- apply(id,1,function(i){coord[i[1]:i[2],]})

  # split up polygons that differ too much
  polygons <- lapply(polygons,function(x){
    x[,1] <- x[,1] + central
    x[,1] <- ifelse(x[,1]>180,x[,1]-360,x[,1])
    thediff=abs(diff(x[,1]));thediff
    if(length(which(is.finite(thediff)))>=1){
      # print(max(thediff))
      outcome=max(thediff,na.rm=T)>170
      if(outcome){
        id <- x[,1] < 0
        x <- rbind(x[id,],c(NA,NA),x[!id,])
      }
      x
    }
  })

  # reconstruct the object
  polygons <- do.call(rbind,polygons)
  Obj[[1]] <- polygons[,1]
  Obj[[2]] <- polygons[,2]

  Obj
}


# end new map -------------------------------------------------------------



# test area

# source('~/CERES Setup.R')

# save(seamaskarr,seamask,landmaskarr,landmask,file="LandSea Masks.tab")
thefiles=load("LandSea Masks.tab");thefiles
seamask0=seamask
seamask0[is.na(seamask)]=0
landmask0=landmask
landmask0[is.na(landmask)]=0
# seamaskarr=abind(seamaskarr,seamaskarr[,,1:72])
# landmaskarr=abind(landmaskarr,landmaskarr[,,1:72])

# seamaskarr=abind(seamaskarr,seamaskarr[,,1:12])
# landmaskarr=abind(landmaskarr,landmaskarr[,,1:12])
# save(seamaskarr,seamask,landmaskarr,landmask,file="LandSea Masks.tab")
# dim(seamaskarr)
# if (exists("toa_sw_all")){
#   if (dim(seamaskarr)[3]<dim(toa_sw_all)[3]){
#     seamaskarr=abind(seamaskarr[,,1:12],seamaskarr)
#     landmaskarr=abind(landmaskarr[,,1:12],landmaskarr)
#   }
# }else{
#   if (dim(seamaskarr)[3]<180){
#     seamaskarr=abind(seamaskarr[,,1:12],seamaskarr)
#     landmaskarr=abind(landmaskarr[,,1:12],landmaskarr)
#   }
# }

# FUNCTIONS

# ind=toa_net_adj;dep=allt2
arrayccf=function(ind,dep){
  bigvec=abind(dep,ind,along=3)
  x=bigvec[45,70,]
  thecc=ccf(firsthalf(x),lasthalf(x))
  thecc$n.used
}


maskstack=function(target,themask=landmask) {
  N=dim(target)[3]
  tmask=array(rep(themask,N),c(180,360,N))
  tmask*target
}



# plotscatter function ----------------------------------------------------
thelat=45.5;thelong=-179.5;#ind=toa_net_adj;dep=allt2;
col="black";thelag=0;
indname="Net TOA Radiation";
depname="Surf. Temperature";
showtrend=T;showtext=T;
themain=NA;thesub=NA;
alpha=.7;xoff=.00;yoff=.05
plotscatter=function(thelat=45.5,thelong=-179.5,ind=toa_net_adj,dep=allt2,
                     col="black",thelag=0,
                     indname="Net TOA Radiation",
                     depname="Surf. Temperature",
                     showtrend=T,showtext=T,
                     themain=NA,thesub=NA,
                     alpha=.7,xoff=.02,yoff=.02){
  therow=lattorow(thelat);thecol=longtocol(thelong)
  indts=ts(ind[therow,thecol,],start=c(2000,3),freq=12)
  depts=ts(dep[therow,thecol,],start=c(2000,3),freq=12)
  plot(depts~lag(indts,thelag),type="o",
       pch=c("M","A","M","J",
             "J","A","S","O","N","D","J","F"),
       ylab=depname,xlab=indname,
       cex=.4,col=addalpha(col,alpha))
  if (is.na(themain)){
    nstext=ifelse(thelat>0,"N","S")
    ewtext=ifelse(thelong>0,"E","W")
    main=paste0(indname," vs. ",depname,
                "\n",abs(thelat),"°",nstext," x ",abs(thelong),"°",ewtext,
                ", Temps lag by ",thelag,
                ifelse(thelag==1," month.", " months."))
  }
  scatlm=lm(depts~lag(indts,thelag))
  trendcol="red"
  if (showtrend==T) abline(scatlm,lwd=3,lty="dashed",col=trendcol)
  if (showtext==T) {
    thistrend=scatlm$coef[2]
    thissum=summary(scatlm)
    lastline=paste0("Trend = ",round(thistrend*3.7,2)," ± ",
                    round(thissum$coef[2,2]*3.7,2),
                    "°C",
                    "\nr^2 = ",round(thissum$r.squared,2),"\np < ",
                    round(thissum$coef[2,4]*3.7,3))

    theusr=par("usr");ul=theusr[1];ur=theusr[2];ub=theusr[3];ut=theusr[4]
    ycoord=ut-(ut-ub)*yoff
    xcoord=ul+(ur-ul)*xoff
    text(xcoord, ycoord, lastline,adj=c(0,1),cex=.8)
  }
  if (is.na(thesub)){
    thesub="Letters in the graph indicate months."
  }
  xlab="Temperature (°C)";ylab="Net TOA Radiation"
  title(main=main,cex.main=.9,line=.8)

  title(sub=thesub,cex.sub=.9,line=3)
}

# plotscatter(35,-10,thelag=4)

# arraymon1 function ------------------------------------------------------
arraymeans=function(testar,na.rm=T) {
	rowMeans(testar,dims=2,na.rm=na.rm)
}

arraymedians=function(testar,na.rm=T) {
	pbapply(testar,c(1,2),median,na.rm)
}


arraymeansp=function(testar,na.rm=T) {
	cl <- parallel::makeCluster(detectCores())
	rowMeans(testar,dims=2,na.rm=na.rm)
	parallel::stopCluster(cl)
}


arraymeans1=function(testar,na.rm=T) {
	arraymeans(testar,na.rm=T)
}
arraymaxs=function(testar,na.rm=T) {
	apply(testar,c(1,2),max,na.rm=na.rm)
}
# arraysums=function(testar,na.rm=T) {
# 	theans=apply(testar,c(1,2),sum,na.rm=na.rm)
# 	theans[theans==0]=NA
# 	theans
# }
arraysums=function(testar,na.rm=T) {
	apply(testar,c(1,2),function(x){
		ifelse(length(which(is.finite(x)))==0,NA,apply(testar,c(1,2),sum,na.rm=na.rm))
	})
}


arraywhichmaxs=function(testar,na.rm=T) {
	apply(testar,c(1,2),which.max)
}
arraysds=function(testar,na.rm=T) {
	apply(testar,c(1,2),sd,na.rm=na.rm)
}


arraytrends=function(testar,na.rm=T) {
	removemonmeans(testar)
	pbapply(testar,c(1,2),function(x) {
		coefficients(lm(x~seq_along(x)))[2]*12
	})
}

arrayrowmeans=function(testar,na.rm=T) {
  aperm(apply(testar,c(1,3),mean,na.rm=na.rm))
}
# dim(arrayrowmeans(mmapghe))
# themonth=;thearray=uahtemps;startmonth=1;blocksize=168
arraymon1=function(themonth,thearray,startmonth=3){
  blocksize=dim(thearray)[3]
  arraymeans1(thearray[,,seq(((themonth-startmonth) %% 12)+1,blocksize,12)])
}

monthmaps=function(thearray,startmonth=3) {
  thearray=array(sapply(1:12,FUN = arraymon1,thearray=thearray,
                        startmonth=startmonth),c(180,360,12))
}

# thearray=surfabs
yearmaps=function(thearray,startmonth=3) {
	(theseq=seq(1,dim(thearray)[3],12))
	nuarray=array(sapply(theseq,FUN = function(i){
		arraymeans(thearray[,,i:(i+11)])
	}),c(180,360,dim(thearray)[3]/12))
}

yearmapsums=function(thearray,startmonth=3) {
	(theseq=seq(1,dim(thearray)[3],12))
	nuarray=array(sapply(theseq,FUN = function(i){
		getarraysums(thearray[,,i:(i+11)])
	}),c(180,360,dim(thearray)[3]/12))
}

# thearray=solar
theyear=2002
arrayyear1=function(theyear,thearray){
  thestart=(theyear-2001)*12+11;theend=thestart+11
  arraymeans1(thearray[,,c(thestart:theend)])
}

# yearmaps=function(thearray) {
#   array(sapply(2001:2013,FUN = arrayyear1,thearray=thearray),c(180,360,12))
# }

# drawworld(yearmaps(toa_net_all)[,,2])

gettrendts=function(x) summary(lm(x~time(x)))$coefficients

# timestring=surfmon;dotrend=T;linecolor="gold2";linewidth=2

drawtrend <- function (timestring,dotrend=FALSE,linecolor="gold2",linewidth=2) {
  gettrend=function(x) summary(lm(x~time(x)))$coefficients

  momm=gettrend(timestring)
  alltime=time(timestring)
  starttime=alltime[1]
  endtime=alltime[length(alltime)]
  meantime=(endtime+starttime)/2
  yup=mean(timestring)+1.96*sd(timestring)/sqrt(length(timestring))
  ydown=mean(timestring)-1.96*sd(timestring)/sqrt(length(timestring))
  if (dotrend) abline(a=momm[1,1],b=momm[2,1],lty="dotted",lwd=2,col="blue")
  lines(x=c(meantime,endtime),y=c(yup, yup+(momm[2,1]+1.96*momm[2,2])*
                                    (endtime-meantime)),col=linecolor,lwd=linewidth)
  lines(x=c(meantime,endtime),y=c(ydown, ydown+(momm[2,1]-1.96*momm[2,2])*(endtime-meantime)),col=linecolor,lwd=linewidth)
  lines(x=c(meantime,starttime),y=c(ydown, ydown+(momm[2,1]+1.96*momm[2,2])*(starttime-meantime)),col=linecolor,lwd=linewidth)
  lines(x=c(meantime,starttime),y=c(yup, yup+(momm[2,1]-1.96*momm[2,2])*(starttime-meantime)),col=linecolor,lwd=linewidth)
}

# plot(avgzoo,type="p")
# drawtrendzoo(avgzoo)
drawtrendzoo <- function (timestring,dotrend=T,linecolor="gold2",linewidth=2) {
  gettrend=function(x) summary(lm(x~time(x)))$coefficients
  momm=gettrend(timestring)
  secsperyear=365.2425*24*3600
  momm[2,]=momm[2,]*secsperyear
  alltime=decimal_date(timestring)
  starttime=alltime[1]
  endtime=alltime[length(alltime)]
  meantime=(endtime+starttime)/2
  starttimepos=date_decimal(starttime,tz="GMT")
  endtimepos=date_decimal(endtime,tz="GMT")
  meantimepos=date_decimal(meantime,tz="GMT")
  yup=mean(timestring)+1.96*sd(timestring)/sqrt(length(timestring))
  ydown=mean(timestring)-1.96*sd(timestring)/sqrt(length(timestring))
  if (dotrend) abline(a=momm[1,1],b=momm[2,1]/secsperyear,lty="dotted",lwd=2,col="blue")
  lines(x=c(meantimepos,endtimepos),y=c(yup, yup+(momm[2,1]+1.96*momm[2,2])*
                                          (endtime-meantime)),col=linecolor,lwd=linewidth)
  lines(x=c(meantimepos,endtimepos),y=c(ydown, ydown+(momm[2,1]-1.96*momm[2,2])
                                        *(endtime-meantime)),col=linecolor,lwd=linewidth)
  lines(x=c(meantimepos,starttimepos),y=c(ydown, ydown+(momm[2,1]+1.96*momm[2,2])*
                                            (starttime-meantime)),col=linecolor,lwd=linewidth)
  lines(x=c(meantimepos,starttimepos),y=c(yup, yup+(momm[2,1]-1.96*momm[2,2])*
                                            (starttime-meantime)),col=linecolor,lwd=linewidth)
}

# y=arraymeans(ghepercent)[,,1]
# x=arraymeans(temprem)[,,1]
# length(which(is.na(y)))
# dotrend=TRUE

drawtrendraw <- function (x,y,dotrend=FALSE,linecolor="gold2",linewidth=2) {
  momm=summary(lm(as.vector(y)~as.vector(x)))$coefficients
  alltime=x
  meantime=mean(alltime)
  starttime=min(alltime)
  endtime=max(alltime)
  yup=mean(y)+1.96*sd(y)/sqrt(length(y))
  ydown=mean(y)-1.96*sd(y)/sqrt(length(y))
  if (dotrend) abline(a=momm[1,1],b=momm[2,1],lty="dotted",lwd=2,col="blue")
  lines(x=c(meantime,endtime),y=c(yup, yup+(momm[2,1]+1.96*momm[2,2])*
                                    (endtime-meantime)),col=linecolor,lwd=linewidth)
  lines(x=c(meantime,endtime),y=c(ydown, ydown+(momm[2,1]-1.96*momm[2,2])*(endtime-meantime)),col=linecolor,lwd=linewidth)
  lines(x=c(meantime,starttime),y=c(ydown, ydown+(momm[2,1]+1.96*momm[2,2])*(starttime-meantime)),col=linecolor,lwd=linewidth)
  lines(x=c(meantime,starttime),y=c(yup, yup+(momm[2,1]-1.96*momm[2,2])*(starttime-meantime)),col=linecolor,lwd=linewidth)
  invisible(momm)
}






cosd=function(x) cos(radians(x))
sind=function(x) sin(radians(x))
tand=function(x) tan(radians(x))

#lm for timeseries


# translating to/from watts kelvin celsius
wtok=function(watts,emissivity=1) (watts/emissivity/5.67e-8)^.25
ktow=function(kelvin,emissivity=1) emissivity*5.67e-8 * kelvin^4
ctok =function(celsius) celsius+273.15
ktoc =function(kelvin) kelvin-273.15
wtoc=function(watts,emissivity=1) ktoc(wtok(watts,emissivity))
ctow=function(celsius, emissivity=1) ktow(ctok(celsius),emissivity)

# reweight a map by latitude
weightmap=function(themap) {
  cellareas=latmean
  if (length(dim(themap))==2){
    themap*cellareas
  } else {
    themap*array(rep(cellareas,
                     dim(themap)[3]),c(180,360,dim(themap)[3]))
  }
}


# reweight a map by area
areamap=function(themap) {

  cellareas=latmean*5.11e14/360/1e12
  if (length(dim(themap))==2){
    themap*cellareas
  } else {
    themap*array(rep(cellareas,
                     dim(themap)[3]),c(180,360,dim(themap)[3]))
  }
}
# round(cellareas[1:10,1:6],3)

# x= toa_net_all
lat1=89.5;lat2=-89.5;long1=-179.5;long2=179.5;mask2=NA

getweightedmonths=function(x,lat1=89.5,lat2=-89.5,
                           long1=-179.5,long2=179.5,mask2=NA,start=c(2000,3)){
  thelat=dim(x)[1];thelong=dim(x)[2]
  x=x*array(rep(latmean,dim(x)[3]),c(thelat,thelong,dim(x)[3]))
  themask=makemask(lat1,lat2,long1,long2,thelat,thelong)
  if (length(mask2)>1) themask=themask*mask2

  maskarr=array(rep(themask,dim(x)[3]),c(thelat,thelong,dim(x)[3]))


  #   x=weightmap(x)
  ts(apply(x*maskarr,3,mean,na.rm=TRUE),start=start,frequency=12)
}

getactualarea=function(mask2=NA,lat1=89.5,lat2=-89.5,
                       long1=-179.5,long2=179.5){
  x=latmean
  themask=makemask(lat1,lat2,long1,long2)

  if (length(mask2)>1) themask=themask*mask2

  sum(themask*x,na.rm=TRUE)/sum(x)
}


detrend=function(x){
  xlm=lm(x~c(0:(length(x)-1)))

  xlm$fitted.values=c(xlm$fitted.values,rep(NA,length(x)-length(xlm$fitted.values)))
  (x-xlm$fitted.values)
}

# quenouille=function(x,y){
#   x=as.vector(x)
#   y=as.vector(y)
#
#   detrend=function(x){
#     xlm=lm(x~c(0:(length(x)-1)))
#
#     xlm$fitted.values=c(xlm$fitted.values,rep(NA,length(x)-length(xlm$fitted.values)))
#     (x-xlm$fitted.values)
#   }
#
#   n=length(x)
#   acfx=acf(detrend(as.vector(x)),lag.max=n-1,plot=FALSE)$acf[-1]
#   acfy=acf(detrend(as.vector(y)),lag.max=n-1,plot=FALSE)$acf[-1]
#   n/(1+sum(2*acfx*acfy))
# }


# qtest=function(x,y=NA){
#   if (is.na(y)) y=time(x)
#   seasonal=stl(x,"periodic")[[1]][,1]
#   x=as.vector(x)-seasonal
#   y=as.vector(y)
#   thelm=lm(y~x)
#   truen=quenouille(x,y)
#   ESS=sum((y-mean(y))^2)
#   RSS=sum((y-thelm$fitted.values)^2)
#   pf((ESS-RSS)/(RSS/truen),1,truen)*2
# }


plottrendline=function(timeseries,col="blue",lwd=2,lty="dashed"){
  thetime=decompose(timeseries)
  newtime=thetime$x-thetime$seasonal
  thelm=lmrob(newtime~time(newtime))
  abline(thelm$coefficients[1],thelm$coefficients[2],col=col,lwd=lwd,lty=lty)
}

seastrend=function(timeseries){
  if (class(timeseries)!="ts"){
    timeseries=ts(timeseries,start=c(1,1),frequency=12)
  }
  thetime=decompose(timeseries)
  newtime=thetime$x-thetime$seasonal
  lm(newtime~time(newtime))
}

seastrendonly=function(timeseries){
  if (class(timeseries)!="ts"){
    timeseries=ts(timeseries,start=c(1,1),frequency=12)
  }
  thetime=decompose(timeseries)
  newtime=thetime$x-thetime$seasonal
  as.double(lm(newtime~time(newtime))$coeff[2])
}
i=473
# timeseries=as.double(tideblock[,whichone]);thestart=start(tideblock)
# gausstrendonly(as.double(timeseries)),thestart=start(tideblock))
gausstrendonly=function(timeseries,thestart=c(1,1),threshold=.05){
  if (class(timeseries)!="ts"){
    timeseries=ts(timeseries,start=thestart,frequency=12)
  }
  time2=trimNA(timeseries)
  residraw=time2-gaussian(time2,8*frequency(timeseries))

  newtime=time2-rep(tapply(residraw, cycle(time2), FUN=mean,na.rm=T),
                    length.out = length(time2));newtime
  if (length(which(is.finite(newtime)))==0){
    returnval=NA
  } else {
    mylm=summary(lm(newtime~time(newtime)));mylm
    if (mylm$coefficients[2,4]> threshold) {
      NA
    } else {

      mylm$coefficients[2,1]
    }
  }
}
#timeseries=tf;threshold=100
gausstrendcoef=function(timeseries,thestart=c(1,1),threshold=.05){
  if (class(timeseries)!="ts"){
    timeseries=ts(timeseries,start=thestart,frequency=12)
  }
  time2=trimNA(timeseries)
  residraw=time2-gaussian(time2,8*frequency(timeseries))
  tsp(time2)
  seasonal=rep(tapply(residraw, cycle(time2), FUN=mean,na.rm=T),
               length.out = length(time2))
  seasts=ts(seasonal,
            start=c(start(time2)[1],1),freq=frequency(time2))
  newtime=time2-seasts;newtime
  if (length(which(is.finite(newtime)))==0){
    returnval=NA
  } else {
    mylm=summary(lm(newtime~time(newtime)));mylm
    if (mylm$coefficients[2,4]> threshold) {
      NA
    } else {

      mylm$coefficients
    }
  }
}

gausserronly=function(timeseries,thestart=c(1,1),threshold=.05){
  if (class(timeseries)!="ts"){
    timeseries=ts(timeseries,start=thestart,frequency=12)
  }
  time2=trimNA(timeseries)
  residraw=time2-gaussian(time2,8*frequency(timeseries))

  newtime=time2-rep(tapply(residraw, cycle(time2), FUN=mean,na.rm=T),
                    length.out = length(time2));newtime
  if (length(which(is.finite(newtime)))==0){
    returnval=NA
  } else {
    mylm=summary(lm(newtime~time(newtime)));mylm
    if (mylm$coefficients[2,4]> threshold) {
      NA
    } else {

      mylm$coefficients[2,2]
    }
  }
}


# seastrendonly(as.double(shmatshort[,1]))

# timeseries=shmatshort[,1]
# plot(timeseries)
# plot(newtime)
# abline(lmrob(newtime~time(newtime)))

# plot(modelmatsh[,1])



seaslm=function(timeseries){

  tspvals=tsp(timeseries)
  if (length(which(is.na(timeseries)))>0) {
    navalues=min(which(is.na(timeseries)))
    timeseries=ts(timeseries[1:(navalues-1)],
                  start=tspvals[1],frequency=tspvals[3])
  }
  timevalues=time(timeseries)

  newtime=timeseries-stl(timeseries,"periodic")[[1]][,1]

  summary(lmrob(newtime~timevalues))$coeff
}



# add lines to ggplot
addlinetoplot <- function(dataset, varx, vary)
{
  p <- geom_line(data=dataset, aes_string(x=varx, y=vary)) +
    geom_point(data=dataset, aes_string(x=varx, y=vary))

  p
}

shorten=function(x) x[,,1:144]

# thedata=allt2
# thedata=surf_lw_up_all
removeseasons=function(thedata,includemean=F){
  datalen=dim(thedata)[3]
  datamonths=max(c(144,dim(thedata)[3]))
  themonths=array(NA,c(180,360,12))
  ptr=0
  i=12
  for (i in c(1:12)){
    ptr=ptr+1
    layerlist=seq(i,max(c(144,datamonths)),12);layerlist
    themonths[,,ptr]=apply(thedata[,,layerlist],c(1,2),mean,na.rm=TRUE)
  }
  meandata=array(rep(arraymeans(thedata[,,1:datalen])[,,1],
                     datamonths),c(180,360,datalen))

  ifelse(includemean,meandata,0)+thedata-array(rep(themonths,datalen/12),
                                               c(180,360,datalen))
}
# drawworld(meandata[,,1]+thedata[,,1]-themonths[,,1])
# drawworld(removeseasons(allt2)[,,1])

plottrend=function(x, col="dark red"){
  trendline=as.vector(lm(as.vector(x)~time(x))$fitted.values)
  lines(trendline~as.vector(time(x)),col=col)
}

noseasonsts=function(x){
  y = decompose(x)
  y$x-y$seasonal
}

residuals=function(x){
  stlnet=stloess(x,doplot=FALSE)
  ts(x-stlnet[[1]][,1],start=start(x), frequency=12)

}
residualsmasked=function(x){
  stlnet=stloess(x,doplot=FALSE)
  swresid=ts(x-stlnet[[1]][,1],start=start(x), frequency=12)
}

# x=minusmap
# themask=makemask(lat1=-.5,lat2=-89.5,long1 = -80,long2 = 179.5)
# # themask[,1]
# whichmask=which(themask==1)
# themask[whichmask]=rnorm(length(whichmask))
# drawworld(themask,rotation = 180)
# x=reyn[,,100]
getmonthsbox=function(somearray,somebox,start=c(2000,3),frequency=12,resid=F){
	lat1=somebox$lats[3]-.5
	lat2=somebox$lats[1]+.5
	long1 = somebox$longs[2]+.5
	long2=somebox$longs[4]-.5
	getmonths(somearray,lat1=lat1,lat2=lat2,long1=long1,long2=long2,start=start,frequency=frequency,resid=resid)
}

getmonths=function(x,lat1=89.5,lat2=-89.5,long1=-179.5,long2=179.5,start=c(2000,3),frequency=12,resid=T,removemean=F){
  # therows=dim(x)[1];thecols=dim(x)[2]

  thelat=dim(x)[1];thelong=dim(x)[2]
  themask=makemask(lat1,lat2,long1,long2,thelat,thelong)


  maskarr=array(rep(themask,dim(x)[3]),c(thelat,thelong,dim(x)[3]))
  dim(maskarr)
  theans=ts(pbapply(x*maskarr,3,getweightedmean),start=start,frequency=frequency)
  if (resid){
  	residual(theans,remove_mean = removemean)
  } else{
  	theans
  }
}

getmonthareas=function(x,lat1=89.5,lat2=-89.5,long1=-179.5,long2=179.5,start=c(2000,3)){
	x[is.finite(x)]=1
	areaarr=array(rep(cellareas,dim(x)[3]),c(180,360,dim(x)[3]))
	ts(apply(x*areaarr,3,sum,na.rm=T),start=start,frequency=12)
}

# x=solarwatts
lat2=.5;lat1=89.5;lat2=-89.5;long1=-179.5;long2=179.5;
start=c(2000,3)
getmonthsums=function(x,lat1=89.5,lat2=-89.5,long1=-179.5,long2=179.5,start=c(2000,3)){
  therows=dim(x)[1];thecols=dim(x)[2]

  thelat=dim(x)[1];thelong=dim(x)[2]
  themask=makemask(lat1,lat2,long1,long2,thelat,thelong)

  maskarr=array(rep(themask,dim(x)[3]),c(thelat,thelong,dim(x)[3]))
  dim(maskarr)
  ts(pbapply(x*maskarr,3,sum,na.rm=T),start=start,frequency=12)
}

# getmonthsums()
# x=allt2
getmonthserrors=function(x,lat1=89.5,lat2=-89.5,long1=-179.5,long2=179.5,start=c(2000,3)){

  therows=dim(x)[1];thecols=dim(x)[2]
  themask=makemask(lat1,lat2,long1,long2)

  maskarr=array(rep(themask,dim(x)[3]),c(180,360,dim(x)[3]))
  dim(maskarr)
  ts(pbapply(x*maskarr,3,getweightedsem),
  	 start=start,frequency=12)
}



rangevalue=function(x) max(as.vector(x))-min(as.vector(x))

stloess=function(x,maintitle="Decomposition of Seasonal Time Data",thenames=NA,doplot=TRUE) {
  if (is.na(thenames)) thenames=c("Data","Seasonal","Trend","Residual")
  thestl=stl(x,"periodic")
  par(cex.axis=1.3)
  if (doplot) {
    plot(thestl, labels=thenames)
    par(cex.axis=1)
    maintitle=paste0(maintitle,"\nLoess trend. Gray bars at right show relative scale")
    title(main=maintitle,line=2,cex.main=1.1)
    abline(v=2000:2013,col="gray50",lty="dashed")
  }
  invisible(thestl)
}

# themap=toa_net_all
# themap=testmap
maxcolor=NA
mincolor=NA
titletext="Dummy Two-line\nTitle"
roundto=0
printavgs=TRUE
isweighted=FALSE
printunits=TRUE
theunits="W/m2"
thebox=NA
printlegend=TRUE
reversecolors=FALSE
rotation=180
oceanonly=FALSE
point.cex=3
thepch="."
latlines=NA

drawbox=function(thebox,therot=180){
  temp=mapproject(thebox$longs,
                  thebox$lats,"mollweide",

                  orientation=c(90,therot,0))
  lines(temp$y~temp$x,lwd=thebox$linewidth,col=thebox$boxcolor)
}

makebox=function(bot,left,top,right,lwd=2,col="blue"){
  list(longs=c(left,left,right,right,left),
       lats=c(bot,top,top,bot,bot),
       linewidth=lwd, boxcolor=col)
}


getboxarea=function(thebox){
  sum(makemask(lat1 = thebox$lats[2],lat2=thebox$lats[1],
            long1 = thebox$longs[2],long2=thebox$longs[3]) *
        areamatrix,na.rm = T)
}


getboxmonths=function(x,thebox,thestart=c(1854,1)){
  getmonths(x,lat1 = thebox$lats[2],lat2=thebox$lats[1],
            long1 = thebox$longs[2],long2=thebox$longs[3],
            start=thestart)
}


drawbox=function(thebox,therot=180){
  temp=mapproject(thebox$longs,
                  thebox$lats,"mollweide",

                  orientation=c(90,therot,0))
  lines(temp$y~temp$x,lwd=thebox$linewidth,col=thebox$boxcolor)
}



# drawworld(themap)
# themap=thetrends
# themap=fulltemps;linecol=NA;rotation=180
# themap=cormap
# drawworld ---------------------
drawworld=
	function(
		themap,
		maxcolor = NA,
		mincolor = NA,
		titletext = "Dummy Two-line\nTitle",
		roundto = 0,
		cex.title = .9,
		titlespacing = 2,
		titleline = -3,
		# cex.title = 1,
		# titleline = -1.7,
		# titlespacing = 1.6,
		printavgs = TRUE,
		isweighted = FALSE,
		printunits = TRUE,
		theunits = "W/m2",
		thebox = NA,
		printlegend = TRUE,
		reversecolors = FALSE,
		rotation = 0,
		linewidth=1,
		oceanonly = FALSE,
		point.cex = 3,
		whichglobe = "l",
		linecol = "black",
		thepch = ".",
		colorlist = c("blue", "cyan", "green3", "yellow", "orange", "red3"),
		uselist = F,
		thebias = 1,
		latlines = NA,
		headlist = NA,
		legend.cex = .85,
		subline=-.3,
		subsize=.9,
		subtext="DATA: CERES EBAF 4.1 https://ceres.larc.nasa.gov/data/",
		plotreset=F,
		drawlogo=T,
		logoleft=0.4,
		logobot=.9,
		drawqr=F
	) {
	# oldmar=c(0,0,0,0)
		par(font=2)
		par(lwd=linewidth)
	if (whichglobe != "l")
		latmatrix = latmatrixold
	if (length(dim(themap)) == 3)
		themap = arraymeansall(themap)[, , 1]
	rowcount = dim(themap)[1]
	columncount = dim(themap)[2]
	yint = 180 / rowcount
	xint = 360 / columncount
	legendlabel = paste0(" ", theunits)
	if (printunits == FALSE)
		legendlabel = " "
	par(mai = c(.05, .05, .05, .05), cex = 1)
	masklong = seq(-179.5, 179.5, 1)
	masklat = seq(89.5, -89.5, -1)

	globallongs = matrix(rep(masklong, 180), nrow = 180, byrow = TRUE)
	globallats = matrix(rep(masklat, 360), nrow = 180)
	if (is.na(maxcolor))
		maxcolor = max(themap, na.rm = TRUE) # ------------lowlow
	if (is.na(mincolor))
		mincolor = min(themap, na.rm = TRUE)
	colormin = mincolor

	colormax = maxcolor
	# choose which trends

	if (reversecolors)
		colorlist = rev(colorlist)
	#   colorlist=c("blue4","blue","cyan","yellow","red","red4")
	color.palette = colorRampPalette(colorlist, bias = thebias)
	mycolors = color.palette(100)
	#   mycolors=rainbow(27)
	cvalues = matrix(oneto100value(themap), nrow = 180)
	colormat = matrix(mycolors[oneto100value(themap, mymax = colormax,
																					 mymin = colormin)], nrow = 180)
	# themap=testmap;rotation=180
	avgdec = roundto + 1

	# calc averages--------
	if (!isweighted) {
		#   bylatmedians=apply(themap,1,median,na.rm=TRUE)
		#   gavg=round(weighted.mean(bylatmedians,latmatrix[,1],na.rm=TRUE),avgdec)
		#   nhavg=round(weighted.mean(bylatmedians[1:90],latmatrix[1:90,1],na.rm=TRUE),avgdec)
		#   shavg=round(weighted.mean(bylatmedians[91:180],latmatrix[91:180,1],na.rm=TRUE),avgdec)
		#   tropavg=round(weighted.mean(bylatmedians[67:114],latmatrix[67:114,1],na.rm=TRUE),avgdec)

		gavg = round(weighted.mean(themap, latmatrix, na.rm = TRUE), avgdec)
		gavg
		nhavg = round(weighted.mean(themap[1:90, ], latmatrix[1:90, ], na.rm =
																	TRUE), avgdec)
		shavg = round(weighted.mean(themap[91:180, ], latmatrix[91:180, ], na.rm =
																	TRUE), avgdec)
		tropavg = round(weighted.mean(themap[68:113, ], latmatrix[68:113, ], na.rm =
																		TRUE), avgdec)
		arcavg = round(weighted.mean(themap[1:24, ], latmatrix[1:24, ], na.rm =
																 	TRUE), avgdec)
		antavg = round(weighted.mean(themap[157:180, ], latmatrix[157:180, ], na.rm =
																 	TRUE), avgdec)

		#   bylatland=apply(themap*seamaskarr[,,1],1,median,na.rm=TRUE)
		#   bylatlandweights=apply(seamaskarr[,,1],1,sum,na.rm=TRUE)*latmatrix[,1]
		gavgland = round(weighted.mean(themap * seamask, latmatrix, na.rm =
																	 	TRUE),
										 avgdec)


		#   bylatsea=apply(themap*landmaskarr[,,1],1,median,na.rm=TRUE)
		#   bylatseaweights=apply(landmaskarr[,,1],1,sum,na.rm=TRUE)*latmatrix[,1]
		gavgsea = round(weighted.mean(themap * landmask, latmatrix, na.rm =
																		TRUE),
										avgdec)

	} else {
		gavg = round(mean(themap, na.rm = TRUE), avgdec)
		nhavg = round(mean(themap[1:90, ], na.rm = TRUE), avgdec)
		shavg = round(mean(themap[91:180, ], na.rm = TRUE), avgdec)
		gavgland = round(mean(themap * seamask, na.rm = TRUE), avgdec)
		gavgsea = round(mean(themap * landmask, na.rm = TRUE), avgdec)
		tropavg = round(mean(themap[67:114, ], na.rm = TRUE), avgdec)
		arcavg = round(mean(themap[1:23, ], na.rm = TRUE), avgdec)
		antavg = round(mean(themap[168:180, ], na.rm = TRUE), avgdec)
	}

	# rotateit ----------------------------------------------------
	# rotation=0
	therot = 180
	if (rotation != 0) {
		# if (rotation>0){
		if (rotation > 0) {
			themap = themap[, c((rotation+1):360, 1:rotation)]
		} else {
			themap = themap[, c((360 + rotation + 1):360,
													1:(360 + rotation))]
		}
		therot = rotation + 180
		if (therot > 360)
			therot = therot - 360
		# 	} else{
		#
		#   }
		if (therot == 360)
			therot = 0
	}
	therot=therot+.1
	newworld = redo.map("world", therot)
	maps::map(newworld,
						projection = 'mollweide',
						interior = F,
						col = linecol)
	#,orient=c(90,therot,0),interior=F,wrap=T)# draws the map outlines
	temp = mapproject(globallongs,
										globallats,
										"mollweide",
										orientation = c(90, therot, 0)) #translates to map coordinates
	lines(
		temp$y ~ temp$x,
		type = "p",
		pch = thepch,
		col = colormat,
		cex = point.cex
	)#10*latmatrix) #colors the map
	maps::map(
		newworld,
		projection = 'mollweide',
		interior = T,
		col = linecol,
		add = T
	) #redraws the lines

	#   map('world', projection='azequalarea',ylim=c(50,90))
	#   temp=mapproject(globallongs,globallats,"azequalarea") #translates to map coordinates
	#   lines(temp$y~temp$x,type="p",pch=".",col=colormat,cex=10)#10*latmatrix) #colors the map
	#   map('world', projection="",ylim=c(50,90),interior=F,add=T) #redraws the lines

	if (length(thebox) > 1) {
		temp = mapproject(thebox$longs,
											thebox$lats,
											"mollweide",

											orient = c(90, therot, 0))
		lines(temp$y ~ temp$x,
					lwd = thebox$linewidth,
					col = thebox$boxcolor)
	}

	mylats = seq(-90, 90, 5)
	mylongs = rep(90, length(mylats))

	temp = mapproject(mylongs, mylats, "mollweide",
										orientation = c(90, therot, 0))
	newlongs = temp$x * 2.005
	lines(temp$y * 1 ~ newlongs, lwd = 2.5)
	newlongs = newlongs * -0.9999
	lines(temp$y * 1 ~ newlongs, lwd = 2.5)
	temp = mapproject(c(-90, 90), c(0, 0), "mollweide", orientation = c(90, therot, 0))
	newlongs = temp$x * 2
	lines(temp$y ~ newlongs)

	temp = mapproject(c(-90, 90),
										c(66.55, 66.55),
										"mollweide",
										orientation = c(90, therot, 0))
	newlongs = temp$x * 2
	lines(temp$y ~ newlongs, lty = "dashed")
	lines(-temp$y ~ newlongs, lty = "dashed")
	temp = mapproject(c(-90, 90),
										c(23.45, 23.45),
										"mollweide",
										orientation = c(90, therot, 0))
	newlongs = temp$x * 2
	lines(temp$y ~ newlongs, lty = "dashed")
	lines(-temp$y ~ newlongs, lty = "dashed")
	if (is.finite(latlines)) {
		temp = mapproject(c(-90, 90),
											c(latlines, latlines),
											"mollweide",
											orientation = c(90, therot, 0))
		newlongs = temp$x * 2
		lines(temp$y ~ newlongs, lty = "dotted")
		lines(-temp$y ~ newlongs, lty = "dotted")
	}



	# make the legend =========
	par(lwd=1)

	multiplier = 1
	colorcount = 6

	mintext = round(colormin, roundto)
	if (is.na(colormin))
		mintext = round(min(themap, na.rm = TRUE) * multiplier, roundto)
	maxtext = round(colormax, roundto)
	if (is.na(colormax))
		maxtext = round(max(themap, na.rm = TRUE) * multiplier, roundto)
	steptext = (maxtext - mintext) / (colorcount - 1)
	midtext = round((mintext + maxtext) / 2, roundto)
	#midtext=round(median(themap),roundto)
	q1 = round((mintext + midtext) / 2, roundto)
	q3 = round((midtext + maxtext) / 2, roundto)
	p1 = round(mintext + steptext, roundto)
	p2 = round(mintext + steptext * 2, roundto)
	p3 = round(mintext + steptext * 3, roundto)
	p4 = round(mintext + steptext * 4, roundto)
	plusminus = ""

	legendtext = c(
		paste(plusminus, mintext, legendlabel, sep = ""),
		paste(plusminus, p1, legendlabel, sep = ""),
		paste(plusminus, p2, legendlabel, sep = ""),
		paste(plusminus, p3, legendlabel, sep = ""),
		paste(plusminus, p4, legendlabel, sep = ""),
		paste(plusminus, maxtext, legendlabel, sep = "")
	)
	newcolors = mycolors[seq(1, 100, length.out = 6)]
	if (printlegend) {
		legend(
			"bottom",
			inset = .1,
			legend = legendtext,
			col = newcolors,
			fill = newcolors,
			horiz = TRUE,
			cex = legend.cex
		)
	}

	# title(sub=paste("Global Average:",round(weighted.mean(themap,latmatrix,na.rm=TRUE),1),"W/m2 per °C"),line=-3.9)

	if (is.na(titleline))
		if (printavgs)
			titleline = -2.5
	else
		titleline = -4.4
	title(main = titletext,
				line = titleline,
				cex.main = cex.title)
	if (oceanonly == TRUE) {
		gavgland = NaN
		gavgsea = NaN
	}

	if (uselist == T) {
		gavg = headlist$gavg
		nhavg = headlist$nhavg
		shavg = headlist$shavg
		gavgland = headlist$gavgland
		gavgsea = headlist$gavgsea
		tropavg = headlist$tropavg
		arcavg = headlist$arcavg
		antavg = headlist$antavg
	}

	theaverages = paste(
		"Avg Globe:",
		gavg,
		" NH:",
		nhavg,
		" SH:",
		shavg,
		" Trop:",
		tropavg,
		"\nArc:",
		arcavg,
		" Ant:",
		antavg,
		" Land:",
		gavgland,
		"Ocean:",
		gavgsea,
		theunits
	)
	if (printavgs) {
		title(theaverages,
					line = titleline - titlespacing,
					cex.main = cex.title)
	} else {
		#     mtext(text=theaverages,line=-16, cex=.6,adj=0.5)

	}
	par(mai = c(1.3, .15, 1, .25))
	title(sub=subtext,cex.sub=subsize,line=subline,font=2)
	if (plotreset) resetplot()
	if (drawlogo){
		ima <- readPNG(paste0("~/Pictures/willis logo 3.png"))
		(usr=par("usr"))
		(theleft=usr[1]+logoleft)
		(thebot=usr[4]-logobot)
		(thetop=thebot+.30)
		(theright=theleft+.30)
		rasterImage(ima,theleft,thebot,theright,thetop)
	}
	if (drawqr){
		ima <- readPNG("~/Pictures/QR Willis 2021 Index.png")
		(usr=par("usr"))
		# (theleft=usr[1]+.38)
		(theleft=usr[2]-.38-.3)
		(thebot=usr[3]+.69)
		(thetop=thebot+.35)
		(theright=theleft+.35)
		rasterImage(ima,theleft,thebot,theright,thetop)
	}

	invisible(
		data.frame(
			gavg = gavg,
			nhavg = nhavg,
			shavg = shavg,
			gavgland = gavgland,
			gavgsea = gavgsea,
			tropavg = tropavg,
			arcavg = arcavg,
			antavg = antavg
		)
	)
}
# END OF DRAWWORLD =================================
# drawworld(reytrends*10, roundto=1,rotation=0,
# 					titletext = paste0("Decadal Sea Surface Temperature Trends\n",
# 														 "Reynolds OI SST, December 1981 - December 2021"),
# 					theunits = "°C/dec",
# 					mincolor=-.3,maxcolor = .7,
# 					subtext=subtextreyn)



# drawworld(climat)

# lats=metadatareal$LAT
# longs=metadatareal$LON
# resetplot()
# drawworld(tempmap)
# hist(longs)
# hist(temp$x)

drawmappoints = function(lats,
												 longs,
												 rotation = 0,
												 pch = 20,
												 col = "blue",
												 cex = .3) {
	therot = 180
	if (rotation != 0) {
		# themap=themap[,c((rotation+1):360,1:rotation)]
		therot = rotation + 180
		if (therot == 360)
			therot = 0
	}
	temp = mapproject(longs, lats, "mollweide", orientation = c(90, therot, 0)) #translates to map coordinates
	lines(
		temp$y ~ temp$x,
		type = "p",
		pch = pch,
		col = col,
		cex = cex
	)
}
# theint=c(13:23)
# which(temp$x< -1.5)[1:10]
# lines(temp$y[theint]~temp$x[theint],type="p",pch=pch,col=col,cex=cex)
#
# cbind(temp$y[theint],temp$x[theint])
# par("usr")
#
# points(-1.66,0,cex=2)
# abline(v=seq(-2,2,.1))



getrange = function(myvector) {
	therange = range(myvector, na.rm = TRUE)
	therange[2] - therange[1]
}

e_error = function(myvector) {
	tau = myvector[1]
	lambda = myvector[2]
	val2 = myvector[3]
	#   dforce=diff(fforce)
	emul = crossovery
	emul[2] = val2
	a = exp(-1 / tau)
	for (i in 3:length(emul))  {
		emul[i] = emul[i - 1] + lambda * (crossoverx[i] - crossoverx[i - 1]) * (1 -
																																							a) + a * (emul[i - 1] - emul[i - 2])
	}
	errorf(crossovery, emul)
}

e_result = function(myvector) {
	tau = myvector[1]
	lambda = myvector[2]
	val2 = myvector[3]
	emul = crossovery
	emul[2] = val2
	a = exp(-1 / tau)
	for (i in 3:length(emul))  {
		emul[i] = emul[i - 1] + lambda * (crossoverx[i] - crossoverx[i - 1]) * (1 -
																																							a) + a * (emul[i - 1] - emul[i - 2]) # one-line equation
	}
	emul[25:36] - mean(emul[25:36])
}

errorf = function(x, y) {
	x = x[-c(1:12)]
	y = y[-c(1, 12)]
	sum(((x - mean(x)) - (y - mean(y))) ^ 2)
}

isitocean = function(x)
	ifelse(is.na(seamaskflat[x]), TRUE, FALSE)
isitocean = function(x)
	ifelse(is.na(landmaskflat[x]), TRUE, FALSE)

whichtorc = function(x) {
	myrow = floor((x - 1) / 360) + 1
	mycol = x - 360 * (myrow - 1)
	cbind(myrow, mycol)
}

getphaseangle2 = function(maindata) {
	if (length(which(!is.finite(maindata))) > 0) {
		NA
	} else {
		xxes = maindata[1:13]
		yyes = maindata[14:26]
		answer = vector(length = 2)
		xxes = xxes - mean(xxes[1:12])
		yyes = yyes - mean(yyes[1:12])
		z = 1
		for (m in 1:12) {
			if (sign(xxes[m]) != sign(xxes[m + 1])) {
				lowx = xxes[m]
				highx = xxes[m + 1]
				lowy = yyes[m]
				highy = yyes[m + 1]
				answer[z] = as.double(approx(c(lowx, highx), c(lowy, highy), 0)$y)
				z = z + 1
			}
		}
		asin(sum(abs(answer[1:2])) / (range(yyes)[2] - range(yyes)[1])) * 180 /
			pi

	}
}

getphaseangle = function(xxes, yyes) {
	answer = vector(length = 2)
	xxes = xxes - mean(xxes)
	yyes = yyes - mean(yyes)
	z = 1
	for (m in 1:12) {
		if (sign(xxes[m]) != sign(xxes[m + 1])) {
			lowx = xxes[m]
			highx = xxes[m + 1]
			lowy = yyes[m]
			highy = yyes[m + 1]
			answer[z] = as.double(approx(c(lowx, highx), c(lowy, highy), 0)$y)
			z = z + 1
		}
	}
	asin(sum(abs(answer[1:2])) / (range(yyes)[2] - range(yyes)[1])) * 180 /
		pi
}

getarea = function(xes, yes)
	area.poly(as(cbind(xes, yes), "gpc.poly"))
getbb = function(xes, yes)
	get.bbox(as(cbind(xes, yes), "gpc.poly"))


# x=thelats;
# length(x)
lattorow = function (x, thelat = 180) {
	x = ifelse (x < 89.5, x, 89.5)
	x = ifelse(x > -89.5, x, -89.5)
	thestep = 180 / thelat
	intervals = seq(90, -90, -thestep)
	max(which(intervals >= x))
}
lattorowv = Vectorize(lattorow, "x")

# x=template[,1]-.5
longtocol = function(x, thelong = 360) {
	x[x > 179.5] = 179.5
	x[x < -179.5] = -79.5
	thestep = 360 / thelong
	intervals = seq(-180, 180, thestep)
	max(which(intervals <= x))
	#   (floor(x)+181)
}
longtocolv = Vectorize(longtocol, vectorize.args = "x")

coltolong = function(x, thelong = 360) {
	thestep = 360 / thelong
	intervals = seq(-180 + thestep / 2, 180 - thestep / 2, thestep)
	intervals[x]
}
coltolong(1, 72)

rowtolat = function(x, thelong = 180) {
	thestep = 180 / thelong
	intervals = seq(90 - thestep / 2, -90 + thestep / 2, -thestep)
	intervals[x]
}



ctor = function(tempc, emissivity = 1) {
	if ("ts" %in% class(tempc)) {
		ts(
			emissivity * 5.67e-8 * (tempc + 273.15) ^ 4,
			start = start(tempc),
			frequency = frequency(tempc)
		)
	} else {
		emissivity * 5.67e-8 * (tempc + 273.15) ^ 4
	}
}

rtoc = function(r, emissivity = 1) {
	(r / emissivity / 5.67e-8) ^ .25 - 273.15
}
rtoc(390, 1)
replaceit = function(x, old, new) {
	dimz = dim(x)
	badlist = which(x == old)
}

realtrend = function(y, x) {
	startlen = length(y) - 120
	tottrend = vector(length = 12)
	selected = seq(1, startlen, 10)
	tottrend[1] = lm(y[selected] ~ x[selected])$coefficients[1]
	for (i in seq(2, 12, 1))  {
		selected = c(selected[-1], max(selected) + 10)
		tottrend[i] = lm(as.vector(y[selected]) ~ as.vector(x[selected]))$coefficients[2]
	}
	mean(tottrend)
}



degrees = function(x)
	(360 * x / (2 * pi)) %% 360
radians = function(x)
	x / 360 * (2 * pi)


# realgauss=function(xline,myfilt=myfilt)  {
#   sum(xline*myfilt,na.rm=TRUE)/sum(myfilt[which(!is.na(xline))])
# }
# # x=3
#
# gfilt = function(x){
#   l=2*(x)+1#fwhm2g
#   temp=c(0,pnorm(-3+(6/l)*(1:l)))
#   diff(temp)/sum(diff(temp))
#   #   temp1=temp
# }
#
# gfilt(3)
# gaussian=function(x,gauss=0){
#   if (gauss==0) {x} else{
#     myfilt=gfilt(gauss)
#     xlong=c(rep(NA,gauss),x,rep(NA,gauss))
#     mywid=2*gauss+1
#     gmatrix=matrix(NA,length(x),mywid)
#     rowmatrix=row(gmatrix)+col(gmatrix)-1
#     gmatrix[,]=(xlong[rowmatrix[,]])
#
#     result=apply(gmatrix,1,realgauss,myfilt=myfilt)
#     if (class(x) == "zoo") result=zoo(result,time(x))
#     if (class(x) == "ts") result=ts(result,start=start(x),frequency=frequency(x))
#     result
#   }
# }

# dep=bigtlt[,,205:444]
# thismap=getarraytrends(dep)
# drawworld(thismap)
# dep=ghe;ind=shortwater
# tarray = allt2
onearraytrends = function(tarray) {
	require(pbapply)
	theseq = seq(1, by = 1 / 12, length.out = dim(tarray)[3])
	meany = mean(theseq)
	yfactor = theseq - meany
	ysum = sum(yfactor ^ 2, na.rm = T)
	pbapply(tarray, c(1, 2), function(x) {
		sum((x - mean(x, na.rm = T)) * yfactor, na.rm = T) / ysum
	})
}

# x= allt2[90,1,]
# lm(x~theseq)$coef[2]*12
# dep=ghmappct;ind=tempmap
getarraytrendsold = function(dep, ind = NA) {
	dim1 = dim(dep)[1]
	dim2 = dim(dep)[2]
	tempstack = array(NA, c(dim1, dim2, 12))
	if (is.na(ind[1])) {
		stackheight = dim(dep)[3]
		# print(paste("Stack = ",stackheight))
		ind = array(rep(x = (1:stackheight) / 12, each = dim1 * dim2),
								c(dim1, dim2, stackheight))
	}
	ind[which(is.na(dep))] = NA
	dep[which(is.na(ind))] = NA
	#   dep=round(dep,3)
	#   ind=round(ind,3)
	stackheight = dim(ind)[3] - 12
	# print(paste("Newstack = ",stackheight))
	for (i in 1:12) {
		print(paste0(i, " of 12"))
		x = ind[, , i:(i + stackheight)]
		y = dep[, , i:(i + stackheight)]
		ulrmeans = arraymeansall(y)
		nsrmeans = arraymeansall(x)
		tempstack[, , i] = rowSums(((y - ulrmeans) * (x - nsrmeans)),
															 dims = 2, na.rm = TRUE) /
			rowSums((x - nsrmeans) ^ 2, dims = 2, na.rm = TRUE)
	}
	# print(head(tempstack))
	rowMeans(tempstack, dims = 2, na.rm = T)
}
# dep=gre
# dep=tempshift;ind=surfabsmap
# dep=yearrain;removemeans=F;divideby=1;ind=NA
getarraytrends = function(dep, ind = NA,removemeans=T,divideby=12) {
	(dim1 = dim(dep)[1])
	(dim2 = dim(dep)[2])
	domeans=T
	if (length(ind)==1) {
		stackheight = dim(dep)[3]
		# print(paste("Stack = ",stackheight))
		ind = array(rep(x = (1:stackheight) / divideby, each = dim1 * dim2),
								c(dim1, dim2, stackheight))
		domeans=F
	}
	if (removemeans){
		if ((domeans) & (dim(dep)[3]!=12)) ind=removemonmeans(ind)
		if (dim(dep)[3]!=12) dep=removemonmeans(dep)
	}
	ind[which(is.na(dep))] = NA
	dep[which(is.na(ind))] = NA
	ulrmeans = arraymeansall(dep)
	nsrmeans = arraymeansall(ind)
	rowSums(((dep - ulrmeans) * (ind - nsrmeans)),
					dims = 2, na.rm = TRUE) /
		rowSums((ind - nsrmeans) ^ 2, dims = 2, na.rm = TRUE)
}

# head(dep[,,2])
# xx=rowMeans(tempstack,dims=2,na.rm=T)
# xx[therow,thecol]
# mean(tempstack[therow,thecol,])


# dep=allt2;ind=toa_net_all
# dep=removemonmeans(dep);ind=removemonmeans(ind)
# themap=getarraytrends(dep,ind)
# drawworld(themap)
# drawworld(y[,,1])
# drawworld(dep[,,1])
# dep=emisfull
# newarraytrends=function(dep,ind=NA){
#   # thedepth=dim(dep)[3]
#   if (is.na(ind[1])) {
#     stackheight=dim(dep)[3]
#     # print(paste("Stack = ",stackheight))
#     ind=array(rep(x = 1:stackheight,each=dim1*dim2),
#               c(dim1,dim2,stackheight))
#   }
#
#   ind[which(is.na(dep))]=NA
#   dep[which(is.na(ind))]=NA
#   y=(dep)
#   x=(ind)
#
#   ulrmeans=arraymeans(y)
#   nsrmeans=arraymeans(x)
#   tempstack=rowSums(((y-ulrmeans) * (x-nsrmeans)),dims=2,na.rm=TRUE)/
#     rowSums((x-nsrmeans)^2,dims=2,na.rm=TRUE)
#   tempstack
# }
# drawworld(tempstack*seamask,mincolor = -10,maxcolor = 10,rotation = 180)
# dep=allt2;addmean=T
# x=1;thedepth=24;dep=toa_lw_all
removemonmeans = function(dep, addmean = F) {
	(thedepth = dim(dep)[3])

	temp = array(pbsapply(
		c(1:12),
		FUN = function(x) {
			arraymeans(dep[, , seq(x, thedepth, 12)])
		}
	),c(dim(dep)[1],dim(dep)[2], 12))
	dim(temp)
	alltemp=array(NA,c(dim(dep)[1],dim(dep)[2], dim(dep)[3]))

	for (i in seq(1, thedepth, 12)) {
		(remains=min(c(i+11),thedepth)-i+1)
		alltemp[,,i:min(c(i+11),thedepth)]=temp[,,1:remains]
		}

	# temp = array(rep(temp,length.out=product(dim(dep))), c(dim(dep)[1],dim(dep)[2], dim(dep)[3]))
	if (addmean) {
		invisible(dep - alltemp + arraymeansall(dep))
	} else {
		invisible(dep - alltemp)
	}
}
# rep1=removemonmeans(toa_lw_all)
# rep2=removemonmeans(rep1)
# rep3=removemonmeans(rep2)
# rep4=removemonmeans(rep3)
# sum(rep2-rep1)
# sum(rep3-rep2)
# sum(rep3-rep2)
# sum(rep4-rep1)
# getweightedmean(rep1)
# drawworld(rep1,roundto=1)

# thedata=ts(rep1[lattorow(45),longtocol(-90),],start=c(2000,3),frequency = 12)
# plot(thedata)
# sd(thedata)
# lines(residual(thedata))

# myarray=array(rep(1:(2*3*12),2),c(2,3,24))
# junk=removemonmeans(myarray)

arrayranges = function(testarray) {
	apply(testarray, c(1, 2), function(x)
		diff(range(x, na.rm = T)))
}
arraysds = function(testarray)
	apply(testarray, c(1, 2), sd, na.rm = T)

calctrends = function(x) {
	shortlen = length(x) / 3
	dv = x[1:shortlen]
	iv = x[(shortlen + 1):(2 * shortlen)]
	sv = x[(2 * shortlen + 1):(3 * shortlen)]
	sv[which(sv == 0)] = NA
	if (length(is.finite(iv)) > 60) {
		lmrob(dv ~ iv + sv)$coefficients[2]
	} else {
		NA
	}
}

getarray2var = function(x)
	apply(x, c(1, 2), calctrends)

getarraycorsmon = function(themon, y, x) {
	thelayers = seq(themon, dim(y)[3], 12)
	getarraycors(y[, , thelayers], x[, , thelayers])
}

getarraycors = function(y, x,removemeans=F) {
	x[which(!is.finite(y))] = NA
	y[which(!is.finite(y))] = NA
	if (removemeans){
		y=removemonmeans(y)
		x=removemonmeans(x)
	}
	ymeans = arraymeansall(y)
	xmeans = arraymeansall(x)
	answer = rowSums(((y - ymeans) * (x - xmeans)), dims = 2, na.rm = TRUE) /
		sqrt(rowSums((x - xmeans) ^ 2, dims = 2, na.rm = TRUE) * rowSums((y - ymeans) ^
																																		 	2, dims = 2, na.rm = TRUE))
	#answer[which(rowSums(is.finite(x),dims=2)<24)]=NA
	answer
}
getarraycors2 = function(y, x) {
	x[which(!is.finite(y))] = NA
	y[which(!is.finite(y))] = NA
	ba = array(NA, c(dim(x)[1], dim(x)[2], dim(x)[3] * 2))
	ba[, , 1:120] = x
	ba[, , 121:240] = y
	apply(ba, c(1, 2), docor)
}

getarraymaxes = function(x) {
	apply(x, c(1, 2), max)
}

getarraymins = function(x) {
	apply(x, c(1, 2), min)
}
# anarray=allt2
diffarray = function(anarray) {
	theht=dim(anarray)[3]
	anarray[,,2:theht]-anarray[,,1:(theht-1)]
}

getarraysds = function(x) {
	apply(x, c(1, 2), sd, na.rm = T)
}

getarraysums = function(x) {
	apply(x, c(1, 2), sum, na.rm = T)
}

docor = function(h) {
	halflen = length(h) / 2
	v1 = h[1:halflen]
	v2 = h[(halflen + 1):(2 * halflen)]
	cor(v1, v2, use = "pairwise.complete.obs")
}

removearraymonths = function(x) {
	myrows = dim(x)[1]
	mycols = dim(x)[2]
	myheight = dim(x)[3]
	monthnum = 0:(myheight - 1) %% 12 + 1
	monthfactor = array(rep(monthnum, each = myrows * mycols),
											c(myrows, mycols, myheight))

}




oneto100value = function(x, mymin = NA, mymax = NA) {
	if (is.na(mymin))
		mymin = min(x, na.rm = TRUE)
	if (is.na(mymax))
		mymax = max(x, na.rm = TRUE)
	if (length(dim(x)) == 0) {
		theanswer = round((x - mymin) / (mymax - mymin) * 99 + 1)
	} else {
		if (length(dim(x)) == 2) {
			theanswer = matrix(round((x - mymin) / (mymax - mymin) * 99 + 1), nrow =
												 	nrow(x))
		} else {
			theanswer = array(round((x - mymin) / (mymax - mymin) * 99 + 1), dim(x))
		}
	}

	theanswer[which(theanswer < 1)] = 1
	theanswer[which(theanswer > 100)] = 100
	theanswer
}

arraymeansall = function(testar) {
	dimz = dim(testar)
	array(rep(arraymeans(testar), dimz[3]), dimz)
}

# arraymeans = function(testar) {
# 	dimz = dim(testar)
# 	array(rep(rowMeans(
# 		testar, dims = 2, na.rm = TRUE
# 	), dimz[3]), dimz)
# }


# dim(rowMeans(solar,dims=2,na.rm=TRUE))

piermean = function(testar)
	aperm(apply(testar, 1, getmeans, dim(testar)[2]))
piersum = function(testar)
	aperm(apply(testar, 1, getsums, dim(testar)[2]))
piersd = function(testar)
	aperm(apply(testar, 1, getsds, dim(testar)[2]))
piercount = function(testar)
	aperm(apply(testar, 1, getcounts, dim(testar)[2]))

getmeans = function(x, rowcount) {
	y = matrix(x, nrow = rowcount)
	apply(y, 1, mean, na.rm = TRUE)
}

getsums = function(x, rowcount) {
	y = matrix(x, nrow = rowcount)
	apply(y, 1, sum, na.rm = TRUE)
}

getsds = function(x, rowcount) {
	y = matrix(x, nrow = rowcount)
	apply(y, 1, sd, na.rm = TRUE)
}

getcounts = function(x, rowcount) {
	y = matrix(x, nrow = rowcount)
	apply(y, 1, function(x)
		length(which(is.finite(x))))
}



anomaly = function(x) {
	x = x[which(is.finite(x))]
	(x - mean(x, na.rm = T))
}

getmax = function(x) {
	if (length(which.max(x)) == 0)
		NA
	else
		which.max(x)
}

# themap=albcors
whichlines = 0
thecolor = "red"

doplot = FALSE
theunits = ""
cex = .5
colorname = NA
therot = 180
# draw contoursblack ---------------------
# themap=thetrends;cex=.8
drawcontoursblack=function(themap,
													 whichlines = 0,
													 thecolor = "white",
													 backcolor = "black",
													 colorname = NA,
													 thebox = NA,
													 doplot = FALSE,
													 theunits = "",
													 cex = .3,
													 cexback=1,
													 therot = 180,
													 titleline = -5.2,
													 cex.title = .85,
													 nlim = 89.55,
													 slim = -89.55){
	resetplot()
	drawcontours(themap,
							 whichlines = whichlines,
							 thecolor = backcolor,
							 colorname = colorname,
							 thebox = thebox,
							 doplot = FALSE,
							 theunits = theunits,
							 cex = cexback,
							 therot = therot,
							 titleline = titleline,
							 cex.title = cex.title,
							 nlim = nlim,
							 slim = slim)
	resetplot()
	drawcontours(themap,
							 whichlines = whichlines,
							 thecolor = thecolor,
							 colorname = colorname,
							 thebox = thebox,
							 doplot = doplot,
							 theunits = theunits,
							 cex = cex,
							 therot = therot,
							 titleline = titleline,
							 cex.title = cex.title,
							 nlim = nlim,
							 slim = slim)
}

# drawcontoursblack2 -----------------




# themap=thetrends
# drawworld(thetrends)
# whichlines=.01
drawcontours = function(themap,
												whichlines = 0,
												thecolor = "white",
												colorname = NA,
												thebox = NA,
												doplot = FALSE,
												theunits = "",
												doboundaries=F,
												cex = .3,
												therot = 180,
												titleline = -5.2,
												cex.title = .85,
												nlim = 89.55,
												slim = -89.55) {
	if (is.na(colorname)) colorname=thecolor
  if (length(dim(themap))==3) themap=arraymeansall(themap)[,,1]
  rowcount=dim(themap)[1]
  colcount=dim(themap)[2]
  # if (rowcount==180){
  #   themap(c(1:lattorow(66.5),lattorow(-66.5):180),)=NA
  # }
  theadjust=ifelse(rowcount==90,2,1)

  masklong=seq(-179,179,2)
  masklat=seq(89.,-89,-2)
  # masklat=seq(66.55,-66.55,-2)

  globallongs=matrix(rep(masklong,90),nrow=90,byrow=TRUE)
  globallats=matrix(rep(masklat,180),nrow=90)

  oldmai=par("mai")
  par(mai=c(.25,.25,.25,.25))
  # resetplot(left=.025)

  thelen=length(whichlines)

  # myout= grDevices::contourLines(x=seq(0,1,length.out=colcount),
  #                                y=seq(0,1,length.out=rowcount),
  #                                z=aperm(themap),
  #                                # nlevels=thelen+1,
  #                                levels=whichlines)
  myout= grDevices::contourLines(x=1:colcount,
                                 y=1:rowcount,
                                 z=aperm(themap),
                                 nlevels=thelen+1,
                                 levels=whichlines)
  #   myout=contourLines(x=1:360,y=1:180,z=aperm(themap),nlevels=2,levels=0)
  length(myout)
  # str(myout)
  i=104
  for (i in 1:length(myout)){
    (globallats=(90.5-myout[[i]]$y*theadjust))
    (globallongs=(-180.5+myout[[i]]$x*theadjust))
  	# (thesigns=sign(globallongs))
  	# longmean=mean(globallongs)
  	# (wrongway=which(thesigns != sign(longmean)))
  	# globallongs[wrongway]=-globallongs[wrongway]
  	#   (globallongs=globallongs+210)
  # 	(globallats=globallats+60)
  	if (length(globallats)> 10) {
      if( (min(globallats)>slim) &
          (max(globallats)<nlim) ){
        (temp=mapproject(globallongs,globallats,"mollweide",
                        orientation=c(90,therot,0))) #translates to map coords
      	# par(mai=c(.025,.025,.025,.025))
      	# plot(temp$y~temp$x,type="l")
        lines(temp$y~temp$x,type="p",pch=20,
              col=thecolor,cex=cex) #draws zeros
        # if (min(temp$x)< -1.75)
        # 	print(paste(i, min(temp$x)))
      }
    }
  }
  # par("usr")
  if (doboundaries){
  newworld=redo.map("world",therot)
  maps::map(newworld, projection='mollweide',orient=c(90,0,0),interior=F,add=T,wrap=T,lwd=1.5) #redraws the lines
}
  masklong=seq(-179.5,179.5,1)
  masklat=seq(89.5,-89.5,-1)
  globallongs=matrix(rep(masklong,180),nrow=180,byrow=TRUE)
  globallats=matrix(rep(masklat,360),nrow=180)
  if (doplot) title(main=paste0("The ",colorname, "/black contour lines show ",whichlines," ",theunits,"."),line=titleline,cex.main=cex.title)



  mylats=seq(-90,90,5)
  mylongs=rep(90,length(mylats))
  temp=mapproject(mylongs,mylats,"mollweide",
                  orientation=c(90,therot,0))
  newlongs=temp$x*2.005

  lines(temp$y*1~newlongs,lwd=2.5)
  newlongs=newlongs*-0.9999
  lines(temp$y*1~newlongs,lwd=2.5)

  # temp=mapproject(c(-90,90),c(0,0),"mollweide",
  #                 orientation=c(90,therot,0))
  if (length(thebox)>1){
  	temp=mapproject(thebox$longs,
  									thebox$lats,"mollweide",

  									orientation=c(90,therot,0))
  	lines(temp$y~temp$x,lwd=thebox$linewidth,col=thebox$boxcolor)
  }
  par(mai=c(oldmai))
}


# end drawcontours ------------------------

showmask=function(lat1,lat2,long1,long2,thelat=180,thelong=360,backmask=NA){

	themask=makemask(lat1,lat2,long1,long2)
	if (length(backmask)>1) themask=themask*backmask
	repmask=themask
	masklen=length(which(themask==1))
	themask[which(themask==1)]=rnorm(masklen)
	drawworld(themask)
	invisible(repmask)
}
makemask=function(lat1,lat2,long1,long2,thelat=180,thelong=360){ # masks all but the incuded lat/long box

  thematrix=matrix(NA,nrow=thelat,ncol=thelong)
  if (longtocol(long1,thelong) <= longtocol(long2,thelong)){
    thematrix[lattorow(lat1,thelat):
                lattorow(lat2,thelat),
              longtocol(long1,thelong):longtocol(long2,thelong)]=1
  } else{
    thematrix[lattorow(lat1,thelat):lattorow(lat2,thelat),
              longtocol(long1,thelong):360]=1
    thematrix[lattorow(lat1,thelat):lattorow(lat2,thelat),
              1:longtocol(long2,thelong)]=1
  }
  thematrix
}

# thematrix[1:10,c(350:360,1:10)]

getmaskfilter=function(lat1,lat2,long1,long2){
  which(is.na(makemask(lat1,lat2,long1,long2)))
}

themap=matrix(NA,36,72)
# themap=toa_net_all;lat1=89.5;lat2=-89.5;long1=-179.5;long2=179.5;na.rm=TRUE

getweightedmean=function(themap,lat1=89.5,lat2=-89.5,long1=-179.5,long2=179.5,na.rm=TRUE){
  if (length(dim(themap))==3) themap = arraymeans(themap)
  thelat=dim(themap)[1];thelong=dim(themap)[2]
  thestep=180/thelat
  intervals=cos(seq(90-thestep/2,-90+thestep/2,-thestep)*pi/180)
  # latmatrixtest=matrix(rep(intervals,thelong),thelat,thelong,byrow=F)
  themask=makemask(lat1,lat2,long1,long2,thelat=thelat,thelong=thelong)
  themap=themap*themask
  weighted.mean(themap,latmatrix,na.rm=na.rm)
}
getweightedmedian=function(themap,lat1=89.5,lat2=-89.5,long1=-179.5,long2=179.5,na.rm=TRUE){
  if (length(dim(themap))==3) themap = arraymeans(themap)
  thelat=dim(themap)[1];thelong=dim(themap)[2]
  thestep=180/thelat
  intervals=cos(seq(90-thestep/2,-90+thestep/2,-thestep)*pi/180)
  # latmatrixtest=matrix(rep(intervals,thelong),thelat,thelong,byrow=F)
  themask=makemask(lat1,lat2,long1,long2,thelat=thelat,thelong=thelong)
  themap=themap*themask
  weightedMedian(themap,latmatrix,na.rm=na.rm)
}

# plot(latmatrixtest[,1])
# lines(latmatrix[,1],col="red3")

getweightedsd=function(themap,lat1=89.5,lat2=-89.5,long1=-179.5,long2=179.5){
	require(SDMTools)
  themask=makemask(lat1,lat2,long1,long2)
  themap=themap*themask
  wt.sd(themap,latmatrix)
}


getweightedsem=function(themap,lat1=89.5,lat2=-89.5,long1=-179.5,long2=179.5,na.rm=TRUE){
  if (length(dim(themap))==3) themap = arraymeansall(themap)[,,1]
  themask=makemask(lat1,lat2,long1,long2)
  themap=themap*themask
  weighted.sd(themap,latmatrix)/
  	sqrt(length(which(is.finite(themap))))
}



geteximj=function(themap, direction="ex",themask=NA,na.rm=TRUE){
  if (length(themask)!=(180*360)) { themask=matrix(1,nrow=180,ncol=360) }

  if (direction == "ex") {
    thefilter=themap >= 0
  } else {
    thefilter = themap < 0
  }
  thefilter[which(thefilter==FALSE)]=NA
  themap=themap*themask*thefilter
  thearea=sum(themask*thefilter*latmatrix,na.rm=TRUE)/sum(latmatrix)
  weighted.mean(themap,latmatrix,na.rm=na.rm)*thearea*(365.25/12*24*3600)*5.11e14/1e22
}

anomaly=function(x,around=mean(x,na.rm=T)) x-around


getexportarea=function(themap, themask=NA,na.rm=TRUE){
  if (length(themask)!=(180*360)) { themask=matrix(1,nrow=180,ncol=360) }
  thefilter=(themap >= 0)
  thefilter[which(thefilter==FALSE)]=NA
  themap=themap*themask*thefilter
  weighted.mean(themap,latmatrix,na.rm=na.rm)
}

# END FUNCTIONS
# CONSTANTS
secspermonth=rep(c(2678400, 2592000, 2678400, 2592000, 2678400,2678400,
                   2592000, 2678400, 2592000, 2678400, 2678400, 2440800),13)



CERES90=c(0.013859917, 0.04157975, 0.069119585, 0.096839419, 0.124379254, 0.151919088, 0.179458923, 0.206818759, 0.234178595, 0.261538431, 0.288718268, 0.315898104, 0.342897942, 0.369717782, 0.396537621, 0.423177461, 0.449637302, 0.476097144, 0.502196986, 0.52829683, 0.554036676, 0.579776521, 0.605336368, 0.630716216, 0.655736065, 0.680575917, 0.705235769, 0.729715622, 0.754015476, 0.777775333, 0.801535191, 0.82511505, 0.84833491, 0.871194773, 0.893874637, 0.916194503, 0.938154371, 0.95993424, 0.981534111, 1.002593984, 1.023473859, 1.043993736, 1.064153615, 1.084133495, 1.103573379, 1.122653264, 1.141553151, 1.15991304, 1.178092931, 1.195732826, 1.213192721, 1.23011262, 1.246852519, 1.263052421, 1.278892327, 1.294192235, 1.309312144, 1.323892057, 1.338111972, 1.351971888, 1.365471807, 1.37843173, 1.391031654, 1.403271581, 1.41497151, 1.426131443, 1.437111378, 1.447551315, 1.457451255, 1.467171197, 1.476171143, 1.48499109, 1.493091041, 1.501010994, 1.50839095, 1.515230908, 1.52171087, 1.527650834, 1.5332308, 1.538270771, 1.542950742, 1.547090717, 1.550690696, 1.553930676, 1.556810659, 1.558970647, 1.560950634, 1.562210626, 1.56329062, 1.563650618)
CERESweights=c(CERES90,rev(CERES90))

czonalwts=c(0.000077, 0.000231, 0.000384, 0.000538, 0.000691, 0.000844, 0.000997, 0.001149, 0.001301, 0.001453, 0.001604, 0.001755, 0.001905, 0.002054, 0.002203, 0.002351, 0.002498, 0.002645, 0.00279, 0.002935, 0.003078, 0.003221, 0.003363, 0.003504, 0.003643, 0.003781, 0.003918, 0.004054, 0.004189, 0.004321, 0.004453, 0.004584, 0.004713, 0.00484, 0.004966, 0.00509, 0.005213, 0.005333, 0.005453, 0.00557, 0.005686, 0.0058, 0.005912, 0.006023, 0.006131, 0.006237, 0.006342, 0.006445, 0.006545, 0.006643, 0.00674, 0.006834, 0.006927, 0.007017, 0.007105, 0.007191, 0.007274, 0.007355, 0.007434, 0.007511, 0.007586, 0.007658, 0.007728, 0.007796, 0.007861, 0.007923, 0.007984, 0.008042, 0.008097, 0.008151, 0.008201, 0.00825, 0.008295, 0.008339, 0.00838, 0.008418, 0.008454, 0.008487, 0.008518, 0.008546, 0.008572, 0.008595, 0.008615, 0.008633, 0.008649, 0.008661, 0.008672, 0.008679, 0.008685, 0.008687, 0.008687, 0.008685, 0.008679, 0.008672, 0.008661, 0.008649, 0.008633, 0.008615, 0.008595, 0.008572, 0.008546, 0.008518, 0.008487, 0.008454, 0.008418, 0.00838, 0.008339, 0.008295, 0.00825, 0.008201, 0.008151, 0.008098, 0.008042, 0.007984, 0.007923, 0.007861, 0.007796, 0.007728, 0.007658, 0.007586, 0.007511, 0.007434, 0.007355, 0.007274, 0.00719, 0.007105, 0.007017, 0.006927, 0.006834, 0.00674, 0.006643, 0.006545, 0.006444, 0.006342, 0.006237, 0.006131, 0.006023, 0.005912, 0.0058, 0.005686, 0.00557, 0.005453, 0.005334, 0.005212, 0.00509, 0.004966, 0.00484, 0.004713, 0.004584, 0.004453, 0.004322, 0.004189, 0.004054, 0.003918, 0.003781, 0.003643, 0.003504, 0.003363, 0.003221, 0.003079, 0.002935, 0.00279, 0.002645, 0.002498, 0.002351, 0.002203, 0.002054, 0.001905, 0.001755, 0.001604, 0.001453, 0.001301, 0.001149, 0.000997, 0.000844, 0.000691, 0.000538, 0.000384, 0.000231, 0.000077)
#  Ceres weights http://ceres.larc.nasa.gov/data/zone_weights_lou.txt

# load(file="CERES Zonal Area.tab",verbose=T)
latmean=matrix(rep(CERESweights/mean(CERESweights),360),nrow=180)
latmatrix=matrix(rep(czonalwts,360),nrow=180);latmatrix
latmatrix=latmatrix*360/sum(latmatrix)
latmatadj=latmatrix/weightedMean(latmatrix,latmatrix)
latmatrixold=matrix(rep(cosd(-89.5:89.5),360),nrow=180)
# plot(CERESweights)
# lines(dcos(-89.5:89.5))
# lines(dcos(-89.5:89.5)/mean(dcos(-89.5:89.5)))
# lines(CERESweights/max(CERESweights),col="red3")
# 1/max(CERESweights)
# max(dcos(-89.5:89.5))/max(CERESweights)
# latmatrix=latmatrixold #
# latmatrix=latmean/max(CERESweights);latmatrix[1:95,1]
# plot(latmatrixold[,1]-latmatrix[,1])
latmeanold=latmatrix/mean(latmatrix)
colorlist=(c("blue", "green","yellow2","orange","red","red4"))
colorlist=c("blue4","blue","cyan","gold","red","red4")
color.palette = colorRampPalette(colorlist)
mycolors=color.palette(100)
themonths=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

justlats=matrix(rep(seq(89.5,-89.5),360),nrow=180)

# monthweight array
monthdays=c(31,28,31,30,31,30,31,31,30,31,30,31)/(365/12)
monthdaysleap=c(31,29,31,30,31,30,31,31,30,31,30,31)/(366/12)
theadjust=rep(c(monthdaysleap,rep(monthdays,3)),3)
theadjust=c(theadjust[c(3:144,1:2)],monthdays[c(3:12,1:2)])
# leaparray=array(rep(theadjust,each=180*360),c(180,360,156))
monthnames=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# LOAD DATA


areamatrix=matrix(rep(latmatrix[,1]*(surfaream)/360,360),180,360)
areamatrix=areamatrix*surfaream/sum(areamatrix)

oceanaream= sum(areamatrix*landmask,na.rm=T)
landaream= surfaream - oceanaream


# areamatrixarr=array(rep(areamatrix,180),c(180,360,180))
#
# require(maps)
# map(col = "grey80", border = "grey40", fill = TRUE,
#     xlim = c(10, 36), ylim = c(36, 62), mar = rep(0.1, 4))
# map('world', projection='mollweide',orient=c(90,0,0),
#     interior=F,wrap=T)
# map(map("world",add=T))
# map('world', projection='mollweide',orient=c(90,therot,0),interior=F,wrap=T)
cellareas=latmean*5.11e14/64800
# sum(cellareas)
#
# sum(latmean[,1])

temperatemask=makemask(23.5,-23.5,-179.5,179.5)
tropicmask=temperatemask
tropicmask[tropicmask==1]=2
tropicmask[is.na(tropicmask)]=1
tropicmask[tropicmask==2]=NA

load(file="countrymaps.tab")

loadcountrymat=function(x="U",thedepth=264,return=T) {
	if (!is.double(x)) {
		thenames = colnames(countrymaps)
		goodboys = which(tolower(substr(thenames, 1, 1)) == tolower(x))
		print(paste(goodboys, colnames(countrymaps)[goodboys]))
	} else {
		if (thedepth == 1) {
			if (return == F) {
				newname = paste0(gsub(" ", "_", colnames(countrymaps)[x]), "_mask")

				assign(newname, matrix(countrymaps[, x], 180, 360),
							 envir = .GlobalEnv)
				print(newname)
			} else{
				invisible(matrix(countrymaps[, x], 180, 360))
			}
		} else {
			if (return == F) {
				newname = paste0(gsub(" ", "_", colnames(countrymaps)[x]), "_arr")

				assign(newname,
							 array(rep(
							 	matrix(countrymaps[, x], 180, 360), thedepth
							 ), c(180, 360, thedepth)),
							 envir = .GlobalEnv)

				print(newname)
			} else {
				invisible(array(rep(
					matrix(countrymaps[, x], 180, 360), thedepth
				), c(180, 360, thedepth)))
			}
		}
	}
}

subtextceres= paste0("CERES EBAF 4.1 DATA: ", "https://ceres.larc.nasa.gov/data")
subtextcet="https://www.metoffice.gov.uk/hadobs/hadcet/cetml1659on.dat"
subtextco2="ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt"
subtexthad5="https://tinyurl.com/mr7ymbbz"
subtextmsu="https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt"
subtextbp="https://www.bp.com/en/global/corporate/energy-economics/\nstatistical-review-of-world-energy.html"
subtexthadts="https://crudata.uea.ac.uk/cru/data/temperature/HadCRUT.4.6.0.0.median.nc"
subtextjapan="https://ds.data.jma.go.jp/tcc/tcc/products/gwp/temp/list/csv/mon_wld.csv"
subtextpsmsl= "https://www.psmsl.org/data/"
subtextgiss="https://data.giss.nasa.gov/gistemp/"
subtextrss="http://data.remss.com/msu/data/netcdf/RSS_Tb_Maps_ch_TLT_V4_0.nc"
subtextberkall="http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_complete.txt"
subtextberkgridall="http://berkeleyearth.lbl.gov/auto/Global/Gridded/Land_and_Ocean_LatLong1.nc"

subtextberkland="http://berkeleyearth.lbl.gov/auto/Global/Complete_TAVG_complete.txt"
subtextberklandmax="http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_LatLong1.nc"
subtextberklandmin="http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_LatLong1.nc"
subtextreyn=paste0("ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc")
subtextberk="http://berkeleyearth.lbl.gov/auto/Global/Gridded/"
subtextmlo="ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt"
subtextowidco="https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
subtextmsu="https://www.nsstc.uah.edu/data/msu/v6.0/"


435/13
435.5/12.5
434.5/13.5
