# getwd()
# lns=loadedNamespaces()
# unloadNamespace(lns)
# setwd("~/R Files")
# source("plotdecomp function.R")
# require(conflicted)
require(conflicted,warn.conflicts = T)
require(pBrackets,warn.conflicts = T)
require(fpCompare,warn.conflicts = T)
require(deming,warn.conflicts = T)
require(anytime,warn.conflicts = T)
require(ExcelFunctionsR,warn.conflicts = T)
require(areaplot,warn.conflicts = T)
require(wCorr,warn.conflicts = T)
require(lattice,warn.conflicts = T)
require(zoo,warn.conflicts = T)
require(matrixStats,warn.conflicts = T)
# require(seewave,warn.conflicts = T)
require(TeachingDemos,warn.conflicts = T)
require(hht,warn.conflicts = T)
require(pracma,warn.conflicts = T)
require(plotrix,warn.conflicts = T)
require(insol,warn.conflicts = T)
require(pbapply,warn.conflicts = T)
require(lubridate,warn.conflicts = T)
require(xts,warn.conflicts = T)
require(wrapr,warn.conflicts = T)
require(abind,warn.conflicts = T)
require(vioplot,warn.conflicts = T)
require(clipr,warn.conflicts = T)
require(reshape,warn.conflicts = T)
require(devtools,warn.conflicts = T)
require(stringi,warn.conflicts = T)
# require(basicPlotteR,warn.conflicts = T)
require(magrittr,warn.conflicts = T)
require(ralger,warn.conflicts = T)
require(dplyr,warn.conflicts = T)
require(png,warn.conflicts = T)
require(stringr,warn.conflicts = T)
require(stats4,warn.conflicts = T)
require(Hmisc,warn.conflicts = T)
require(MASS,warn.conflicts = T)
require(errors,warn.conflicts=T)
conflict_prefer("last", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("summary", "stats4")
conflict_prefer("interp1", "pracma")
conflict_prefer("rowMedians", "matrixStats")
# require(reshape2)
# clrconsole()
# conflict_scout()
# require(helfRlein)

# source("plotdecomp function.R")
# source("plotdecomp function.R")
# Start Constants -------------------------------------

loglines=c(1:20,seq(20,100,5),seq(100,1000,50),
					seq(1000,10000,500),
					seq(10000,100000,5000),
					seq(100000,1000000,50000),
					seq(1000000,10000000,500000))
loglinesbold=c(5,10,15,seq(20,100,10),seq(100,1000,100),
					 seq(1000,10000,1000),
					 seq(10000,100000,10000),
					 seq(100000,1000000,100000),
					 seq(1000000,10000000,1000000))
atmosphericmass=5.1480e18 #kg
whitepct=76.5
blackpct=13.4
maunder=c(1645,1715)
dalton=c(1790,1830)
sporer=c(1460,1550)
wolf=c(1280,1350)
options(stringsAsFactors=F)
# options(useFancyQuotes)
# options("browser")
# options("install.packages.compile.from.source")
# options("mc.cores")

dayspertropicyear=365.24219876
co2kgperbbl=432.71
co2kgpertonne=432.71*7.15
dayspermonth=c(31,28,31,30,31,30,31,31,30,31,30,31)
speedoflight=299792458 #m/sec



dwkey="eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Ondlc2NoZW5iYWNoIiwiaXNzIjoiYWdlbnQ6d2VzY2hlbmJhY2g6OjU3YTgxYTI4LTlhNmMtNGNhYy05OWZkLWI2ZDhlZTFmNzJlYSIsImlhdCI6MTUwMjIzNDg3Miwicm9sZSI6WyJ1c2VyX2FwaV93cml0ZSIsInVzZXJfYXBpX3JlYWQiXSwiZ2VuZXJhbC1wdXJwb3NlIjp0cnVlfQ.ZFUCDE3ZZmuFKuwb3uMOvbsDSNpKwhLzonvc4LqDga73UNTRQN0EHrpOwM0mP7zcquJAfMs-pA3DeMqQYzGzVg"
hrsperyear=365.25*24
hoursperyear=365.25*24
hourspermonth=365.25*2
hrspermonth=365.25*2
secsperday=24*60*60;secsperyear=365.2425*secsperday
secsperdayadj=secsperday+35.11545

earthradm=6378136.047
surfaream=4*pi*earthradm^2

sbconst=5.67e-8


planckconst=6.6260693e-34 #  W s2
celer=2.99792458e8  #m s-1
boltconst=1.380658e-23
wl2freq=function(wl) celer/wl
freq2wl=function(freq) celer/freq
wl2wn=function(wln) 10000/wln #wavelength in nm

load("Colors By Name.tab",verbose=F)

# target=lwdownmon;ind=ghemon;ind2=abswdownmon
# start functions ----------------
plotadd=function(thets,col="firebrick",black=T,...){
	par(new=T)
	if (black==T){
		blackplot(thets,xlab=NA,ylab=NA,xaxt="n",yaxt="n",col=col,...)
	} else {
		plot(thets,xlab=NA,ylab=NA,xaxt="n",yaxt="n",col=col,...)
	}
}
bestfit=function(target,ind,ind2=NA,ind3=NA){
	if (length(ind3)>1){
		thelm=lm(target~ind+ind2+ind3)
	} else if (length(ind2)>1){
		thelm=lm(target~ind+ind2)
	} else {
		thelm=lm(target~ind)
	}
	print(summary(thelm))
	if ("ts" %in% class(ind)){
		return(ts(thelm$fitted.values,start=start(ind),
							frequency = frequency(ind)))
	} else {
		return(thelm$fittedvalues)
	}
}

# tser=creswmon
ccf0=function(a,b){
	ccf(a,b,main="")
	abline(v=0,col=addalpha("red",.5))
}

airspeed=function(temp=288,mass=32) {
	sqrt(3*boltconst*temp/
			 	(mass/6.022e23))
			 }
airspeed(300,24)


getmode <- function(v) {
	uniqv <- unique(v)
	uniqv[which.max(tabulate(match(v, uniqv)))]
}

tabunique=function(v) {
	data.frame(name=unique(v),count=tabulate(match(v,unique(v)))) %>%
						 	arrange(desc(count))
}

readghcnmax=function(theid){
	testdata=read.csv(paste0("https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/",theid,".csv"))
	zoo(testdata$TMAX/10,as.POSIXct(testdata$DATE,tz="GMT"))
}

readghcnmin=function(theid){
	testdata=read.csv(paste0("https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/",theid,".csv"))
	zoo(testdata$TMIN/10,as.POSIXct(testdata$DATE,tz="GMT"))
}

yearfrac=function(somedate){
	decimal_date(somedate)-floor(decimal_date(somedate))
}

# dtrendlin(somezoo)
# somezoo=therain

sumrain=function(shortrain,filtlen=3){
	zoo(stats::filter(shortrain,rep(1/filtlen,filtlen))*filtlen,
			time(shortrain)[(filtlen+1):(length(shortrain)-filtlen-1)])
}
# somezoo=maxshort
# plot(somezoo)
#somezoo=sfun
removedailymeans=function(somezoo,removemean=F){
	(thefracts=(yearfrac(time(somezoo))))
	themeans=tapply(dtrendlin(somezoo),thefracts,mean,na.rm=T)
	meanlen=length(themeans)
	longmeans=rep(themeans,3)
	# plot(longmeans,type="l")
	# longlow=lowess(1:length(longmeans),longmeans,.05)
	longgauss=mygauss(longmeans,40)
	# blackline(longgauss~c(1:(3*meanlen)))
	# blackline(longlow,col="red")
	#
	dayavgs=data.frame(fract=sort(unique(thefracts)),
										 val=longgauss[(meanlen+1):(2*meanlen)])
	# dayavgs=data.frame(fract=as.double(levels(as.factor(thefracts))),
	# 									 val=as.double(themeans))

	(thesubs=dayavgs$val[match(round(thefracts,5),round(dayavgs$fract,5))])
	addval=ifelse(removemean,0,mean(somezoo,na.rm=T))
	somezoo-thesubs #+addval
}
#
# meansval=(removedailymeans(dcozoo))
# gaussval=(removedailymeans(dcozoo))
#
# plot(meansval)
# lines(gaussval,col="red")
#
#
# round(thesubs,2)

onetime=function(thedata,thetime) as.double(window(thedata, thetime, thetime))
plottrenderror=function(tser,doplot=T,dotrend=T,...){
	thep=pvalueh(tser)
	tser=thep$newtser
	(thelm=thep$lm)

	(thelen=length(tser))
	(thestep=(thep$summary$coef[2,1]+thep$summary$coef[2,2])/frequency(tser))
	(midtime=mean(tsp(tser)[1:2]))
	(theoffset=sem(tser))
	if(ISEVEN(thelen)){
		(halflen=thelen/2)
		(halferror=seq(halflen*thestep,0,length.out=halflen))
		(fullerror=c(halferror,rev(halferror))+theoffset)
	} else {
		(halflen=floor(thelen/2))
		halferror=seq(halflen*thestep,thestep,length.out=halflen)
		fullerror=c(halferror,0,rev(halferror))+theoffset
	}
	fullts=ts(fullerror,start=start(tser),frequency = frequency(tser))
	# plot(tser)
	if (doplot) error.shade(as.double(time(tser)),thelm$fitted.values,fullerror, ...)
	if (dotrend) blackline(thelm)
	invisible(fullts)
}

cuft2gal=function(cuft) cuft*6.2288354590428
gal2cuft=function(gal) gal/cuft2gal(1)

atmosprop=function(altitude,prop="t"){
	thedata=read.csv("standard atmosphere.csv")
	whichcol=which(c("a","t","g","p","d")==prop)
	interp1(thedata[,1],thedata[,whichcol],altitude,method = "cubic")
}


beatfreq=function(f1,f2) max(c(f1,f2))-min(c(f1,f2))
beatperiod=function(p1,p2) 1/beatfreq(1/p1,1/p2)


curlybraces <- function(x, y, range, depth=1,pos = 1, direction = 1, ... ) {

	a=c(1,2,3,48,50)    # set flexion point for spline
	b=c(0,.2,.28,.7,.8) # set depth for spline flexion point
	b=b*depth
	curve = spline(a, b, n = 50, method = "natural")$y / 2

	curve = c(curve,rev(curve))

	a_sequence = rep(x,100)
	b_sequence = seq(y-range/2,y+range/2,length=100)

	# direction
	if(direction==1)
		a_sequence = a_sequence+curve
	if(direction==2)
		a_sequence = a_sequence-curve

	# pos
	if(pos==1)
		lines(a_sequence,b_sequence,...) # vertical
	if(pos==2)
		lines(b_sequence,a_sequence,...) # horizontal

}


mttnt2wh=function(tnt) tnt*1162222222222.2
wh2mttnt=function(wh) wh/1162222222222.2

# heatanom=350 #heat anomaly in zetajoules
zj2deg2000=function(heatanom){ #zetajoules to heat to 2000m
	swspec=3960 #specific heat, j/kg/°C
	vol2000=647679070 #km^3 to 2000 m
	mass2000=vol2000*1e9*1000*1.02 #kg to 2000 m
	jperdeg=mass2000*swspec #joules/deg, to warm to 2000m 1°C
	heatanom*1e21/jperdeg #temp. chg. °C
}

monlength=function(somezoo) apply.monthly(as.zoo(somezoo),function(x) length(which(is.finite(x))))
yearlength=function(somezoo) apply.yearly(as.zoo(somezoo),function(x) length(which(is.finite(x))))


apply.monmean=function(somezoo,minlen=25){
	thelengths=as.double(monlength(somezoo))
	lengthmask=ifelse(thelengths>minlen,1,NA)
	trimzoo=trimNA(apply.monthly(somezoo,mean,na.rm=T)*lengthmask)
	zoototsmon(trimzoo)
}

# somezoo=newtideblock[,1]
apply.annmean=function(somezoo,minlen=4){
	somezoo=as.zoo(somezoo)
	thelengths=as.double(yearlength(somezoo))
	lengthmask=ifelse(thelengths>minlen,1,NA)
	if (sum(lengthmask,na.rm=T)>0){
		trimzoo=trimNA(apply.yearly(somezoo,mean,na.rm=T)*lengthmask)
		zoototsann(trimzoo)
	}
}

flattendir=function(somefolder) {
	x=(paste0("find '",somefolder,"'/ -mindepth 2 -type f -exec mv -i '{}' '",somefolder,"'/ ';'"))
	print(x)
	write_clip(x)
}

# tser=cotrail
shadowline=function(tser,y=NA,thelwd=6,rightshift=.005,downshift=.01,
										thecol="black",thealpha=.4,thetype="l"){
	if (class(tser) %in% c("zoo","ts")){
		y=as.double(tser)
		x=as.double(time(tser))
	} else x=tser
	(theusr=par("usr"))
	xwid=theusr[2]-theusr[1]
	yhgt=theusr[4]-theusr[3]
	lines(y-yhgt*downshift~I(x+xwid*rightshift),lwd=thelwd,col=addalpha(thecol,thealpha),type=thetype,
				lend="butt",ljoin="bevel")
}
# plot(tser)
# shadowline(theavg)
colorbars=function(thedata,horiz=F,doplot=T,thecols=NA,
									 drawtops=F){
	themids=barplot(thedata,plot=doplot)
	thelen=length(themids)
	theedges=data.frame(leftedge=themids-.5,
											rightedge=themids+.5)

	if (is.na(thecols[1])) thecols=rainbow(thelen)
	bardens=100
	thebar=1
	for (thebar in 1:thelen){
		(theheight=thedata[thebar])
		(linelocs=seq(theedges$leftedge[thebar],
									theedges$rightedge[thebar],
									length.out=bardens))
		cf=colorRampPalette(c(thecols[thebar],
													"white",
													thecols[thebar],
													"gray30"))
		ablineclip(v=linelocs,col=cf(bardens),y1=0,y2=theheight)
	}
	for (thebar in 1:thelen){
		(theheight=thedata[thebar])
		rect(theedges$leftedge[thebar],0,
				 theedges$rightedge[thebar],
				 theheight,lwd=2)
	}
	if (drawtops){
		themax=max(thedata)
		themin=0
		for (thebar in 1:thelen){
			(theheight=thedata[thebar])
			draw.ellipse(
				themids[thebar],theheight,
				a=.5,
				b=(1-getfract(theheight,themin,themax))/3,
				col=thecols[thebar],
				lwd=2)
		}
	}
}
# colorbars(thedata,drawtops = F)
# thedata=rnorm(8,10,4)


getfract=function(data,bot=0,top=1) {
	(data-bot)/(top-bot)
}



readknmi=function(theurl,thefreq=12,...) {
	(theskip=max(which(substr(readLines(theurl),1,1)=="#")))
	thetable=read.table(theurl,skip=theskip,...)
	if (dim(thetable)[2]==13){
	tabletots(thetable[,2:13],start=thetable[1,1])
	} else{
	ts(thetable[2], decimaltotwodate(thetable[1,1]),frequency = thefreq)
	}
}

theurl="http://climexp.knmi.nl/data/xgdcnIN011351500.dat"
theurl="http://climexp.knmi.nl/data/xgdcnUSW00013958.dat"

theurl="http://climexp.knmi.nl/data/pgdcnUSC00047902__yr.txt"
# head(thetable)
readknmidaily=function(theurl) {
	(theskip=max(grep("#",readLines(theurl))))
	thetable=read.table(theurl,skip=theskip)

	if (nchar(thetable[1,1])==8) {
		datetext=paste0(substr(thetable$V1,1,4),
										"-",
										substr(thetable$V1,5,6),
										"-",
										substr(thetable$V1,7,8))
		zoo(thetable[,2], as.POSIXct(datetext,tz="GMT"))
	} else{

		(	datetext=paste0(thetable$V1,"-",twodigit(thetable$V2),"-",
											twodigit(thetable$V3)))
	zoo(thetable[,4], as.POSIXct(datetext,tz="GMT"))
	}
}


knmidown=function(theurl,thesearch){
	thetable=read.table(theurl,
											skip=grep(thesearch,
																readLines(theurl),fixed=T))
	ts(thetable[,2],thetable[1,1],frequency = round(1/(thetable[2,1]-thetable[1,1])))
}

zeroline=function(tser) ts(rep(0,length(tser)),start=start(tser),frequency =  frequency(tser))

overlay=function(whichfile,thesize=1,
								 gravity="northwest",
								 moveright=40,
								 lr=91,tb=92,
								 moveup=30,
								 final=F,oldfile="willis logo 3.png") {
	if (final==T) outfile=whichfile else outfile="output.png"
	if (length(which(dir("Screenshots")==whichfile))==0) stop("NO SUCH FILE")


	if (length(which(dir("Screenshots")==oldfile))==0) stop("NO SUCH OVERLAY")

	theline = paste0(
		"convert \"",whichfile, "\""," \"",oldfile,"\" -gravity ",gravity,
		" -geometry '",round(lr*thesize),"x",round(tb*thesize),"+",moveright,"+",moveup,
		"'  -composite -format png  \"", outfile,"\";open \"", outfile,"\"")

	print(theline)
	write_clip(theline)

}

xlog=T
logo=function(xdiv=10,ydiv=8,
							thesize=.65,
							squeezelr=1,
							squeezetb=1,
							xlog=F,
							yv=0, xh=-.3){
	require(png)
	(mai=par("mai")[1])
	if (mai==0.9) yv=yv/2
	if (mai==1.3) yv=yv*1.5
	(usr=par("usr"))
	if (xlog) {
		usr[1:2] = 10^usr[1:2]
		xdiv=xdiv*10
	}
	(xdiff=(usr[2]-usr[1]))
	# xdiff=xdiff-xdiff/1.04
	(ydiff=(usr[4]-usr[3]))
	# ydiff=ydiff-ydiff/1.04
	ima <- readPNG("~/Pictures/willis logo 3.png")
	(thefrac=ydiff/xdiff)
	# xdiv=10;ydiv=8
	(theleft=usr[1]-xdiff/xdiv-xdiff/xdiv*xh)
	(thebot=usr[4]+ydiff/ydiv/20+ydiff/ydiv*yv)
	thetop=thebot+ydiff/ydiv*thesize*squeezetb
	theright=theleft+xdiff/xdiv*thesize*squeezelr
	rasterImage(ima,theleft,thebot,theright,thetop,xpd=T)
}
qrplot=function(xdiv=10,ydiv=8,
							thesize=.65,
							squeezelr=1.5,
							squeezetb=1.5,
							xlog=F,
							yv=-.2, xh=-.05){
	require(png)
	(mai=par("mai")[1])
	if (mai==0.9) yv=yv/2
	if (mai==1.3) yv=yv*1.5
	(usr=par("usr"))
	if (xlog) {
		usr[1:2] = 10^usr[1:2]
		xdiv=xdiv*10
	}
	(xdiff=(usr[2]-usr[1]))
	# xdiff=xdiff-xdiff/1.04
	(ydiff=(usr[4]-usr[3]))
	# ydiff=ydiff-ydiff/1.04
	ima <- readPNG("~/Pictures/QR Willis 2021 Index.png")
	(thefrac=ydiff/xdiff)
	# xdiv=10;ydiv=8
	(theleft=usr[1]-xdiff/xdiv-xdiff/xdiv*xh)
	(thebot=usr[3]-ydiff/ydiv*1+ydiff/ydiv*yv)
	(thetop=thebot+ydiff/ydiv*1*thesize*squeezetb)
	theright=theleft+xdiff/xdiv*thesize*squeezelr
	rasterImage(ima,theleft,thebot,theright,thetop,xpd=T)
}


# logo()


halflife=function(tau) -tau*log(.5)

# watt-hours to joules
wh2j=function(wh) 3600*wh

# joules to watt-hours
j2wh=function(j) j/wh2j(1)


# gallons of gas to kilowatt-hours
galgas2kwh=function(gal) gal*j2wh(130927880)/1000

# gallons of diesel to kilowatt-hours
galdiesel2kwh=function(gal) gal*j2wh(146765930)/1000


oilbbl2ft3=function(oil) oil*5.6145835124493
gal2ft3=function(gal) gal*0.13368055555556


posixtime=function(posixsecs) as.POSIXct(posixsecs,origin=as.POSIXct("1970-01-01 00:00:00",tz="GMT"),tz="GMT")

zooslope=function(azoo,theyears=10,roundto=2){
	round(as.double(lm(azoo~time(azoo))$coef[2]*secsperyear)*theyears,roundto)
}

polyfromlines=function(upline,dnline,col="dodgerblue",border=NA,fade=.5){
	polygon(c(as.double(time(dnline)),as.double(rev(time(dnline)))),c(dnline,rev(upline)),col = addalpha(col,fade),border = NA)
}


hp2kw=function(hp) hp*0.74566272
kw2hp=function(kw) kw/0.74566272

monthpluserr=function(tarray,thestart=c(2000,3),removeseason=T,...){
	monval=getmonths(tarray,resid = removeseason,start=thestart,...)
	monerr=getmonthserrors(tarray,start=thestart,...)
	cycerr=cycleerror(monval)
	fullerror=ts(sqrt(as.double(monerr)^2+as.double(cycerr)^2),start=thestart,frequency = 12)
	data_frame(values=monval,montherr=monerr,
						 cycleerr=cycerr,errors=fullerror)
}

demingall=function(dep, ind){

	deperr=monthpluserr(dep)
	inderr=monthpluserr(ind)
	deming(deperr$values~inderr$values,xstd = inderr$errors,
				 ystd=deperr$errors)
}


demingpvalue=function(dlm) {
	2*pt(dlm$coef[2]/dlm$se[2],dlm$n-2,lower=F)
}

demlmline=function(thedem){
	(thetime=as.double(unlist(thedem$model[2])))

	list(x=thedem$model[,2],y=thetime*thedem$coefficients[2]+thedem$coefficients[1])
}
# blackline(demlmline(thedem))

weighted.sd=function(x,wt) sqrt(wtd.var(x, wt,na.rm = T))

toatotrop=function(tropdoubling,latitude=0) tropdoubling/(.384+.22*dcos(latitude))
troptotoa=function(toadoubling,latitude=0) toadoubling * 1/toatotrop(1,latitude)

trailingtrend=function(tser,tlength=120,gt=.8,mult=10) {
	thefreq=frequency(tser)
	thetrends=pbsapply(tlength:length(tser),function(x){
		(thestart=time(tser)[x-tlength+1])
		(theend=time(tser)[x])
		testts=window(tser,thestart,theend)
		if(length(which(is.finite(testts)))>tlength*gt){
			thets=lmts(testts)
			as.double(thets$coefficients[2])
		} else {
			NA
		}
	})
	ts(thetrends,time(tser)[tlength],frequency = thefreq)*mult
}

trailingacc=function(tser,tlength=120) {
	thefreq=frequency(tser)
	thetrends=pbsapply(tlength:length(tser),function(x){
		(thestart=time(tser)[x-tlength+1])
		(theend=time(tser)[x])
		(quadtest(window(tser,thestart,theend))$acc)

	})
	ts(thetrends,time(tser)[tlength],frequency = thefreq)
}

rewrap=function(thetext) {
	thetext=gsub("\n"," ",thetext)
	write_clip(gsub("\n"," ",thetext))
}
stackem=function(name1=NA,name2=NA,newname="test",vertical=T){
	if ((is.na(name1)) & (is.na(name2))) {
		name1=dir("Screenshots")[grep("OSXDaily",dir("Screenshots"))[1]]
		name1=gsub(".png","",name1)
		name2=dir("Screenshots")[grep("OSXDaily",dir("Screenshots"))[2]]
		name2=gsub(".png","",name2)
	}
	direction=ifelse(vertical==T,"-","+")
	theline=paste0("convert \"",name1,".png\""," \"",name2,".png\"", " -gravity center ",direction,"append ","\"",newname,".png\"",";open \"",newname,".png", "\"")
	write_clip(theline)
	clrconsole()
	cat(theline)
	invisible(theline)
}


# stackem(name1,name2)
#

stackshadow=function(name1=NA,name2=NA,newname="TEST",newsize=100,vertical=T){
	if ((is.na(name1)) & (is.na(name2))) {
		name1=dir("Screenshots")[grep("OSXDaily",dir("Screenshots"))[1]]
		name1=gsub(".png","",name1)
		name2=dir("Screenshots")[grep("OSXDaily",dir("Screenshots"))[2]]
		name2=gsub(".png","",name2)
	}
	oldline=stackem(name1,name2,"testimage",vertical=vertical)
	oldline=unlist(strsplit(oldline,";"))[1]

	(newline=dropshadow(oldname="testimage.png",newname=newname,newsize = newsize))
	finalline=paste0(oldline,";",newline)
	write_clip(finalline)
	clrconsole()
	cat(finalline)
}
# stackshadow("OSXDaily 2021-12-22 at 11.23.02 AM",
# 						"OSXDaily 2021-12-22 at 11.23.15 AM",
# 						"South Africa CD")
# stackem("OSXDaily 2021-12-22 at 11.23.02 AM",
# 				"OSXDaily 2021-12-22 at 11.23.15 AM")


thetest=dir("Screenshots")[grep("OSXDaily",dir("Screenshots"))]

test2=unlist(lapply(strsplit(thetest," "),function(x) x[4]))

which.max(as.double(substr(test2,1,nchar(test2)-3)))

dropshadow=function(newname="some picture",newsize=90,blursize=15, oldname=NA){
	if (is.na(oldname)){
		thetest=dir("Screenshots")[grep("OSXDaily",dir("Screenshots"))]

		test2=unlist(lapply(strsplit(thetest," "),function(x) x[4]))

		finalnum=which.max(as.double(substr(test2,1,nchar(test2)-3)))

		oldname=dir("Screenshots")[grep("OSXDaily",dir("Screenshots"))][finalnum]
	}
	firstline=roundcorners(oldname,extension="")


	shadowsize=round(20*100/newsize,3)
	shadowsize2=round(shadowsize/2,3)

	if (is.na(oldname)) {
		print("NO FILE AVAILABLE")
		theline=""
		write_clip("")
	} else {
		theline=paste0("convert \"",oldname,"\""," \\( +clone -background black -shadow 80x",blursize,"+",shadowsize2,"+",shadowsize," \\) +swap -background none -layers merge +resize ",newsize,"% +repage \"",oldname,"\"",";mv \"",oldname,"\""," ","\"",newname,".png\"",";open \"",newname,".png", "\"")
		theline=paste0(firstline,"\n",theline)
		write_clip(theline)
	clrconsole()
	cat(theline)
	invisible(theline)
	}
}

# oldname="OSXDaily"
roundcorners=function(oldname="NA",extension=".png"){
	if (is.na(oldname)){
		thetest=dir("Screenshots")[grep("OSXDaily",dir("Screenshots"))]

		test2=unlist(lapply(strsplit(thetest," "),function(x) x[4]))

		finalnum=which.max(as.double(substr(test2,1,nchar(test2)-3)))

		oldname=dir("Screenshots")[grep("OSXDaily",dir("Screenshots"))][finalnum]
	}

	theline=paste0("convert \"",oldname,extension,"\"", "\\
	\\( +clone  -alpha extract \\
		 -draw 'fill black polygon 0,0 0,25 25,0 fill white circle 25,25 25,0' \\
		 \\( +clone -flip \\) -compose Multiply -composite \\
		 \\( +clone -flop \\) -compose Multiply -composite \\
		 \\) -alpha off -compose CopyOpacity -composite ",
	"\"",oldname,extension,"\"")
	write_clip(theline)
	clrconsole()
	cat(theline)
	invisible(theline)

}



meanbylat=function(somemap){
	ts(rev(rowMeans(somemap,na.rm = T)),-89.5)
}
minbylat=function(somemap){
	ts(rev(rowMins(somemap,na.rm = T)),-89.5)
}
maxbylat=function(somemap){
	ts(rev(rowMaxs(somemap,na.rm = T)),-89.5)
}
sumbylat=function(somemap){
	ts(rev(rowSums(somemap,na.rm = T)),-89.5)
}
trendbylat=function(somedata){
	meanbylat(getarraytrends(somedata))
}
trendbylat2=function(depdata,inddata){
	meanbylat(getarraytrends(depdata,inddata))
}


lookup=function(thevalue,theindex,theresult){
	if (first(theindex)>last(theindex))	{
		theindex=rev(theindex)
		theresult=rev(theresult)
	}
	theresult[max(which(theindex<=thevalue))]
}

ej2twh=function(ej) 277.77 * ej



decimalpart=function(fullnumber) abs(fullnumber)-floor(abs(fullnumber))



twh2gw=function(twh,peakfactor=2,capfactor=1,transmission=.95) (twh/hrsperyear*1e3*peakfactor)/capfactor/transmission

mtoe2twh=function(mtoe) mtoe*11.63
twh2mtoe=function(twh) twh/mtoe2twh(1)

right=function(x,num) substr(x,nchar(x)-num+1,nchar(x))
left=function(x,num) substr(x,1,num)

ac2m2=function(acres=1) acres*4046.8564224

mps2mph=function(mps) mps*2.2369362920544
mph2mps=function(mph) mph/2.2369362920544


altrows=function(somemat,odd=T){
	thestart=ifelse(odd,1,2)
	somemat[seq(thestart,NROW(somemat),2),]
}


whpermile2mpge=function(whpermile) 33722/whpermile
mpge2whpermile=function(mpge) 33722/mpge

toe2wh=function(toe) toe*11.63e6
wh2toe=function(wh) wh/11.63e6
bar2psi=function(bar) bar* 14.503773773022
psi2bar=function(psi) psi/14.503773773022

xcminus=function(somexc,badboys=ncol(somexc)-2){
	badboys=c(1,badboys+1)
	(thetime=time(somexc))
	therows=rowSums(somexc[,-badboys])
	if (class(somexc)=="zoo"){
		zoo(therows,thetime)
	} else {
		therows
	}
}

# thelow=thelowly
# i=0
# whichone="y"
drawintersects=function(i,thelow,whichone="x",...){
	if (whichone=="x"){
		yval=lookuplow(i,thelow)
	} else {
		yval=i
		i=lookuplow(i,thelow,whichone="y")
	}

	ablineclip(v=i,y2=yval,...)
	ablineclip(h=yval,x2=i,...)
	invisible(yval)
}
trendfrom=function(tser,upto=12*frequency(tser)) {
	ttime=as.double(time(tser))
	ts(sapply(1:(length(tser)-upto), function(x){
		lmts(window(tser,ttime[x]))$coeff[2]
	}),start=start(tser),frequency = frequency(tser))
}


justdecimal=function(x) abs(x)-floor(abs(x))

acres2sqft=function(acres) acres *43560
sqft2acres=function(sqft) sqft/43560

acres2km2=function(acres) acres*0.0040468564224
km2acres2=function(acres) acres/0.0040468564224

zootsfillann=function(zooann){
	tsout=ts(NA,start=year(start(zooann)),end=year(end(zooann)))
	tsout[match(year(time(zooann)),time(tsout))]=as.double(zooann)
	tsout
}

ac2sqft=function(acres) acres*5280^2/640
sqft2ac=function(sqft)sqft/43560


clrconsole=function() cat("\014")
btu2whr=function(btu) btu*0.29329722222222

ftlb2wh=function(lbs,ft) lbs*ft*0.00037661609675872




daystodate=function(thedate) {
	if (is.double(thedate)) thedate = paste0(thedate,"-01-01")
	as.double(as.Date(thedate)-Sys.Date())
}

monthstodate=function(thedate) {
	if (is.double(thedate)) thedate = paste0(thedate,"-01-01")
	as.double(as.Date(thedate)-Sys.Date())/(365.25/12)
}

yearstodate=function(thedate) {
	if (is.double(thedate)) thedate = paste0(thedate,"-01-01")
	as.double(as.Date(thedate)-Sys.Date())/(365.25)
}
weekstodate=function(thedate) {
	if (is.double(thedate)) thedate = paste0(thedate,"-01-01")
	as.double(as.Date(thedate)-Sys.Date())/7
}

# carbontaxtokeroprice(94)

carbontaxtogasprice=function(pricepertonne,metric=T) pricepertonne/ifelse(metric, 2200,2000)*18.74
carbontaxtokeroprice=function(pricepertonne,metric=T) pricepertonne/ifelse(metric, 2200,2000)*21.78

carbontaxtogasprice(50)

usrpct=function(lr=10,ud=10){
	(theusr=par("usr"))
	xwid=diff(theusr[c(1,2)])
	ywid=diff(theusr[c(3,4)])
	list(x=theusr[1]+xwid*lr/100,y=theusr[3]+ywid*ud/100)
}
# text="funk"
textpct=function(lr=10,ud=10,thetext,...){
	(theusr=par("usr"))
	xwid=diff(theusr[c(1,2)])
	ywid=diff(theusr[c(3,4)])
	text(x=theusr[1]+xwid*lr/100,y=theusr[3]+ywid*ud/100,
			 thetext,...)
}

loadfolder=function(foldername=NA,number=NA){
	if(is.na(number[1])) dir(foldername) else{
		for (i in number){
			(fullfile=paste0(foldername,dir(foldername)[i]))
			load(fullfile,verbose = T,envir = globalenv())
		}
	}
}
makearray=function(amat,number=264) array(rep(amat,number),c(dim(amat),number))
km2ac=function(km2) km2*247.10538146717
ac2km2=function(ac) ac/km2ac(1)
natozero=function(x) {
	x[is.na(x)]=0
	x
}

# somelow=sealow
# plot(bigdf$lowy/bigdf$lowx~bigdf$x)
# blackline(lowess(bigdf$x,bigdf$lowy/bigdf$lowx,thef))
# plot(getslopelow(seaslope))
# head(bigdf)
# somelow=landl

# somelow=nulow
# mapto=nulow
# length(nulow$x)
# themask=nhmask*seamask
# nulowshland=sketch(depmap*themask,indmap*themask,doplot=T)
# somelow=nulowshland
# anylow = nulow
# anylow = crelow

getslopelow=function(anylow,thef=.001,mapto=NA){
	sx=anylow$x;sy=anylow$y
	uniq=unique(sx)
	thedata=pbsapply(uniq,function(i) {
		mean(sy[which(sx==i)])
	})
	somelow=list(x=as.double(uniq),y=thedata)
	bigdf=data.frame(lowx=diff(somelow$x),
									 lowy=diff(somelow$y),
									 x=somelow$x[-(length(somelow$x)-1)],
									 y=somelow$y[-(length(somelow$y)-1)]) %>%
		dplyr::filter(lowx!=0)

	# plot(somelow)
	# plot(bigdf$x,bigdf$lowy/bigdf$lowx)
	thelow=lowess(bigdf$x,bigdf$lowy/bigdf$lowx,thef)
	# lines(thelow)
	length(thelow$x)
	mainlow=anylow
	length(anylow$x)
	length(mainlow$y)
	mainlow$y=interp1(thelow$x,thelow$y,anylow$x)
	templow=mainlow
	if (!is.na(mapto[1])) {
		templow=mapto
		templow$y=rep(NA,length(mapto$y))
		i=1
		(i=length(mainlow$x))
		# pbsapply(1:length(mainlow$x),function(i)
		goodboys=0
		length(which(mainlow$x %in% mapto$x))
		length(which(mapto$x %in% mainlow$x))
		for (i in 1:length(mainlow$x)) {
			(xval=mainlow$x[i])
			(yval=mainlow$y[i])
			templow$y[which(templow$x==xval)]=yval
			goodboys=goodboys+length(which(templow$x==xval))
		}
	}
	templow
}
# 	head(templow$y)
# 	length(templow$x)
# 	blackplot(templow)
# blackplot(templow)
# length(which(is.na(templow)))


# anylow=nulowshland
# plot(nulow)
getslopelow2=function(anylow,thef=.001,mapto=NA){
	sx=anylow$x;sy=anylow$y
	uniq=unique(sx)
	thedata=pbsapply(uniq,function(i) {
		mean(sy[which(sx==i)])
	})
	somelow=list(x=as.double(uniq),y=thedata)

	plot(somelow)
	bigdf=data.frame(lowx=diff(somelow$x),
									 lowy=diff(somelow$y),
									 x=somelow$x[-(length(somelow$x)-1)],
									 y=somelow$y[-(length(somelow$y)-1)]) %>%
		mutate(slope=lowy/lowx)
	thelow=lowess(bigdf$x,bigdf$slope,thef)
	blackplot(thelow)
	length(thelow$x)
	nulow=list(x=sx,y=interp1(thelow$x,thelow$y,sx))

	if (!is.na(mapto[1])) {
		templow=mapto
		templow$y=rep(NA,length(mapto$y))
		i=1
		i=10
		(i=length(thelow$x))
		# pbsapply(1:length(mainlow$x),function(i)
		goodboys=0
		length(which(thelow$x %in% mapto$x))
		length(which(mapto$x %in% thelow$x))
		for (i in 1:length(thelow$x)) {
			(xval=thelow$x[i])
			(yval=thelow$y[i])
			which(templow$x==xval)
			templow$y[which(templow$x==xval)]=yval
			# goodboys=goodboys+length(which(templow$x==xval))
		}
		nulow=templow
	}
	nulow
}
# head(templow$y)
#
# blackplot(templow)
# length(which(is.na(templow$y)))
#
# blackplot(nulow)
# length(nulow$x)
findinterval=function(someval,somerange){
	if (is.na(someval)){
		theans=NA
	} else {
	theans=findInterval(someval,somerange)
	thelen=length(somerange)-1
	if (theans==0) theans=1
	if (theans>thelen) theans=thelen
	}
	theans
}

findintervalv=Vectorize(findinterval,"someval")

lookuplow=function(someval,somelow,whichone="x"){
	if (whichone=="x"){
		theint=findintervalv(someval,somelow$x)
		(pctpast=(someval-somelow$x[theint])/
				(somelow$x[theint+1]-somelow$x[theint]))
		somelow$y[theint]+pctpast*(somelow$y[theint+1]-somelow$y[theint])
	} else{
		theint=findintervalv(someval,somelow$y)
		(pctpast=(someval-somelow$y[theint])/
				(somelow$y[theint+1]-somelow$y[theint]))
		somelow$x[theint]+pctpast*(somelow$x[theint+1]-somelow$x[theint])	}
}


bar2m=function(bar) bar*10.197162129779
m2bar=function(m) m/bar2m(1)



oceanvolcukm=1330000000

pnl <- function(x, y, ...) {
	panel.number <- parent.frame()$panel.number
	# if (panel.number == 1) lines(x, c(2,2,2), col = "blue")
	lines(x, y)
	thelm=lm(y~x)
	blackabline(thelm)
	text(1942,1.25,paste0("Trend = ",round(thelm$coeff[2]*100,2)," index units/century."),cex=1.4,font=2)
}
#
# resetplot()
# par(font=2)
# plot.zoo(allpdsi, panel = pnl,ylim=c(-1.5,1.5),yax.flip = T,
# 				 ylab=c("Global", "N. Hem.","S. Hem"),main="",xlab="Year",
# 				 cex.axis=1.4,font.lab=2,font.axis=2)



ftlb2wh=function(ftlb) ftlb*0.00037661609675872
wh2ftlb=function(wh) wh/ftlb2wh(1)
zerolim=function(x) c(0,max(x,na.rm = T))
plotlow=function(thets,f=.1,colfront="yellow",colmain="black",
								 lwd=1,frontline=2,backline=6,...){
	plot(thets, col=colmain,...)
	blackline(lowts(thets),col=colfront,frontline = frontline,
						backline=backline)
}



diff.array=function(myarray) myarray[,,2:dim(myarray)[3]] - myarray[,,1:(dim(myarray)[3]-1)]

monlen=function(theyear) {
	if (((theyear) %%4 == 0) & (theyear != 2000)){
		366*monthdaysleap/12 } else{
			365*monthdays/12
		}
}

repeach=function(vec1,vec2){
	getreps=function(x,vec1,vec2){
		rep(vec1[x],vec2[x])
	}

	unlist(pbsapply(1:length(vec1),getreps,as.vector(vec1),
									as.vector(vec2))	)
}

xctots=function(thexc,tser, thenum=1,justone=F){
	colcount=ncol(thexc)
	if (justone==F){
		thelm=lm(tser~thexc[,(colcount+1-thenum):colcount])
	} else {
		thelm=lm(tser~thexc[,(colcount+1-thenum)])
	}
	ts(thelm$fitted.values,start=start(tser),frequency=frequency(tser))
}
lowts=function(tser,f=.1) {
	if (class(tser)=="ts"){
		ts(lowess(time(tser),tser,f)$y,start=start(tser),frequency = frequency(tser))
	}else{
		thelow=lowess(time(tser),tser,f)
		zoo(thelow$y,
				as.POSIXct(thelow$x,
									 origin="1970-01-01 00:00",
									 tz="GMT"))
	}
}



remean=function(x) x-mean(x)
ts12=function(var,tstart) ts(var,start=tstart,frequency = 12)
totsann=function(var,tstart) ts(var,start=tstart,frequency = 1)
j2whr=function(x) x*0.0002777777777
btu2wh=function(x) x*0.29329722222222
mm2ft=function(x) x/1000*39.37/12
mm2in=function(x) x/1000*39.37
in2mm=function(x) x/mm2in(1)
m2ft=function(x=1) x*39.37/12
m2in=function(x=1) x*39.37
ft2m=function(x=1) x/m2ft(1)
ft2mm=function(x=1) x/mm2ft(1)
hec2ac=function(x=1) x*2.471044
ac2hec=function(x=1) x/hec2ac(1)
mi2km=function(x=1) x*1.609344
km2mi=function(x=1) x/mi2km(1)
km2mi2=function(x=1) x*0.38610215854245
mi2km2=function(x=1) x/0.38610215854245

co2lbspergal=function(gals=1) 19.6*gals
co2galsperlb=function(co2=1) co2/19.6


tryc <- function(expr) {
	W <- NULL
	w.handler <- function(w) {
		# warning handler
		W <<- w
		invokeRestart("muffleWarning")
	}
	thelist=list(value = withCallingHandlers(tryCatch(
		expr,
		error = function(e)
			e
	),
	warning = w.handler),
	warning = W)
	iserror=("error" %in% class(thelist$value))
	if (iserror) NA else thelist$value
}


loadfiles=1
loadceres=function(loadfiles = 0) {
	loadfiles=sort(loadfiles)
	thefiles = dir("CERES Datasets")
	if(loadfiles[1]==0){
		print(paste0(twodigit(seq_along(thefiles)), "  ", thefiles))
	} else {
		if (is.na(loadfiles[1])) {
			print(paste0(twodigit(seq_along(thefiles)), "  ", thefiles))
			thefile = readline(prompt = "Enter numbers separated by commas.")
			if (thefile == "") {
				print("No Entry.")
				break
			}
			(loadfiles = as.double(unlist(
				strsplit(thefile, split = ",", fixed = T)
			)))
		}
		i=loadfiles[1]
		for (i in loadfiles) {
			(fullfile=paste0("CERES Datasets/", thefiles[i]))
			load(fullfile, verbose = T,envir = .GlobalEnv)

		}
	}
	thefiles=paste0("# ", paste0(twodigit(seq_along(thefiles)), " ", substr(thefiles,1,nchar(thefiles)-4),";")[loadfiles])
	cat(thefiles)
	write_clip(thefiles)
	# substr(thefiles,1,nchar(thefiles)-4)
}
loadmaps=function(loadfiles = 0) {
	loadfiles=sort(loadfiles)
	thefiles = dir("CERES Maps")
	if(loadfiles[1]==0){
		print(paste0(twodigit(seq_along(thefiles)), "  ", thefiles))
	} else {
		if (is.na(loadfiles[1])) {
			print(paste0(twodigit(seq_along(thefiles)), "  ", thefiles))
			thefile = readline(prompt = "Enter numbers separated by commas.")
			if (thefile == "") {
				print("No Entry.")
				break
			}
			(loadfiles = as.double(unlist(
				strsplit(thefile, split = ",", fixed = T)
			)))
		}
		i=loadfiles[1]
		for (i in loadfiles) {
			(fullfile=paste0("CERES Maps/", thefiles[i]))
			load(fullfile, verbose = T,envir = .GlobalEnv)

		}
	}
	thefiles=paste0("# ", paste0(twodigit(seq_along(thefiles)), " ", substr(thefiles,1,nchar(thefiles)-4),";")[loadfiles])
	cat(thefiles)
	write_clip(thefiles)
	# substr(thefiles,1,nchar(thefiles)-4)
}
loadmonths=function(loadfiles = 0) {
	loadfiles=sort(loadfiles)
	thefiles = dir("CERES Months")
	if(loadfiles[1]==0){
		print(paste0(twodigit(seq_along(thefiles)), "  ", thefiles))
	} else {
		if (is.na(loadfiles[1])) {
			print(paste0(twodigit(seq_along(thefiles)), "  ", thefiles))
			thefile = readline(prompt = "Enter numbers separated by commas.")
			if (thefile == "") {
				print("No Entry.")
				break
			}
			(loadfiles = as.double(unlist(
				strsplit(thefile, split = ",", fixed = T)
			)))
		}
		i=loadfiles[1]
		for (i in loadfiles) {
			(fullfile=paste0("CERES Months/", thefiles[i]))
			load(fullfile, verbose = T,envir = .GlobalEnv)

		}
	}
	thefiles=paste0("# ", paste0(twodigit(seq_along(thefiles)), " ", substr(thefiles,1,nchar(thefiles)-4),";")[loadfiles])
	cat(thefiles)
	write_clip(thefiles)
	# substr(thefiles,1,nchar(thefiles)-4)
}

catcount=function(thevar) tapply(thevar,thevar,length)
catpct=function(thevar) round(tapply(thevar,thevar,length)/length(thevar)*100,1)

lastrow=function(themat) themat[nrow(themat),]
lastcol=function(themat) themat[,ncol(themat)]

getxcfitted = function(x, testxc, orig) {
	ts(
		lm(orig ~ testxc[, x:ncol(testxc)])$fitted.values,
		start = start(orig),
		frequency = frequency(orig)
	)
}


loglines=function(thecex=1,dolabel=T){
	thetype="dashed";thecol="dark gray"
	abline(h=seq(.1,1,.1),lty=thetype,col=thecol)
	abline(h=seq(.01,.1,.01),lty=thetype,col=thecol)
	abline(h=seq(1,10,1),lty=thetype,col=thecol)
	abline(h=seq(10,100,10),lty=thetype,col=thecol)
	abline(h=seq(100,1000,100),lty=thetype,col=thecol)
	abline(h=seq(1000,10000,1000),lty=thetype,col=thecol)
	abline(h=seq(10000,100000,10000),lty=thetype,col=thecol)
	abline(h=c(.01,.1,1,10,100,1000,10000),lwd=2)
	abline(h=c(.05,.5,5,50,500,5000),lwd=1.2,lty="dashed")
	if (dolabel){
		axis(2,at=c(.01,.1,1,10,100,1000,10000),
				 labels=c(.01,.1,1,10,100,1000,10000),
				 cex.axis=thecex)
	}
}

getpopbox=function() load("Roser Populations.tab",verbose = T)

sealevelerror=function(ayear){
	# require(growthmodels)
	signif(exp(-ayear/65.63)*1.06 + exp(-ayear/11.08)*12.38,2)
	# gompertz(ayear,0.10178144,	-4.93888145	,0.019937481)
}


tstimetozootime=function(tser) {
	tidetimes=time(tser)
	as.POSIXlt(paste0(floor(tidetimes),"-",twodigit(round((tidetimes-floor(tidetimes))*12)+1),"-15"),
						 tz="GMT")
}

decimaltomonthyear=function(somedecimal){
	moyr=decimaltotwodate(somedecimal)
	paste0(month.abb[moyr[2]]," ",moyr[1])
}

# ts=ballts
findbreaks = function(ts,
											h = 0.2,
											ylab = NA,
											xlab = "Year",
											per = 10,
											roundto = 2,
											multiplier = 1,
											theunits = "W/m2",
											gausslen = 48,
											subtext = subtextceres,
											subline=3,
											titletext="thetitle",
											plotblack = F,
											plottrend = F,
											frontcol="red",
											font=2,
											dodates=T,
											plotbot=.9,
											...) {
	require(strucchange)
	resetplot(top=0.8,bot=plotbot)
	if (dodates) {
		thetsp=tsp(ts)
		titletext = paste0(titletext,", ",
											 decimaltomonthyear(thetsp[1]),
											 " to ",
											 decimaltomonthyear(thetsp[2]))
											 }
	if (per == 100) tn = "cen" else if (per == 10) tn = "dec" else if (per == 1) tn = "yr" else tn = per

	bp_ts <- breakpoints(ts ~ 1,h=h)

	ci_ts <- confint(bp_ts)

	## to plot the breakpoints with confidence intervals
	plot(ts,ylab=ylab,xlab=xlab,...)
	if (plotblack) blackline(mygauss(ts,gausslen),backline=4,frontlin=2)
	cint=ci_ts$confint
	theusr=par("usr")
	liney=theusr[3]+(theusr[4]-theusr[3])*.98
	topliney=theusr[3]+(theusr[4]-theusr[3])*.93
	midliney=theusr[3]+(theusr[4]-theusr[3])*.88
	botliney=theusr[3]+(theusr[4]-theusr[3])*.041
	breaktimes=time(ts)[bp_ts$breakpoints]
	topliney=rep(c(topliney,midliney),length.out=length(breaktimes))
	breakuncertminus=sapply(1:length(breaktimes),function(i) {
		round((time(ts)[cint[i,2]]-time(ts)[cint[i,1]])/2,1)
	})
	breakuncertplus=sapply(1:length(breaktimes),function(i) {
		round((time(ts)[cint[i,3]]-time(ts)[cint[i,2]])/2,1)
	})
	abline(v=breaktimes,col="red")

	boxed.labels(breaktimes,topliney,paste0(round(breaktimes,1)," -",breakuncertminus," +",breakuncertplus),cex=.8,ypad = 2,font=2)

i=1
	for (i in 1:nrow(cint)){
		arrows(x0=time(ts)[cint[i,1]],x1=time(ts)[cint[i,3]],
					 y0=liney,angle = 90,length = .05,code = 3,col="red")
	}
	firsttime=time(ts)[1]
	lasttime=last(time(ts))
	alltimes=c(firsttime,breaktimes,lasttime)
	x=6
	junk=sapply(1:(length(alltimes)-1),function(x){
		tests=window(ts,start=alltimes[x],end=alltimes[x+1])
		thelm=lmts(tests)
		blackabline(thelm,x1 = alltimes[x],x2=alltimes[x+1],
								col=frontcol)
		boxed.labels(
			mean(tsp(tests)[1:2]),
			botliney,
			paste0(
				round(thelm$coef[2] * per * multiplier, roundto),
				"\n",
				theunits,
				"/",
				tn
			),
			cex = 0.7,
			ypad = 1.3,
			font=2
		)
	})
	if (plottrend) blackline(lmts(ts))
	title(sub=subtext,cex.sub=.9,line=subline)
	title(main=paste0(titletext,"\nStructural Breakpoint Analysis per https://tinyurl.com/zr8mtf49\n",
										"Minimum section length = ",round(diff(tsp(ts)[1:2])*h,1)," years."),cex.main=1,line=.8)
	logo(squeezetb = 1.2,squeezelr = 1.1)
	resetplot()
}

residualxc = function(somexc,
											titletext = NA,
											subtext = NA,
											thecol = NA,
											ylab = "Empirical Mode Residual",
											thepicture="",
											thefade=0,
											theunits="mm",
											xlab = "Year",
											backwid = NA,
											subline=3,
											frontwid = NA,
											doplot = T,
											linetype="o",
											multiplier=1,
											realsd = NA,
											botmargin=.9,
											...
){
	resetplot(bot=botmargin)
	if (is.na(backwid)) {
		backwid=6
		frontwid=2
	}
	if (is.na(thecol)) thecol=ncol(somexc)
	theline=somexc[,thecol]

	nulm=lmts(theline)
	if (doplot){
		if (is.na(realsd[1])) {
			blackline(theline,add=F,ylim=c(-3,3),type="n",xlab=xlab,ylab=paste0(ylab," (standard deviations)"),backline = backwid,frontline = frontwid)
			grid(ny=NA,lwd=2)
			blackabline(nulm,col="red",
									backline = backwid,frontline = frontwid,
									x1=start(theline),x2=end(theline))
			blackline(theline,add=T,backline = backwid,frontline = frontwid)
			title(main=paste0("CEEMD Residual (yellow) and Linear Trend (red)","\n",titletext," ",round(start(theline),1)," - ",round(end(theline),1)),cex.main=.9,line=.8)
			if (!is.na(subtext)){
				title(sub=paste0("DATA: ",subtext),cex.sub=.9,line=3)
			}
#else =======================
		} else {
			# truedata=rowsumsts(somexc)
			truedata=somexc[,1]
			trueadj=lmts(truedata)$coef[2];trueadj
			actual=truedata*realsd/trueadj*multiplier
			blackline(actual,xlab="Year",ylab=paste0(ylab," (",theunits,")"),frontline = 1,backline = 3,col="white",add=F,type=linetype, ...)
			if (nchar(thepicture)>0){
				plotpicture(thepicture,fade = thefade,
										xlab="Year",
										ylab=paste0(ylab," (",theunits,")"))
				blackline(actual,
									frontline = 1,backline = 3,
									col="white",add=T,type=linetype, ...)
			}
			newlm=lm(actual~theline)
			thefreq=round(1/(time(actual)[2]-time(actual)[1]))
			theline2=ts(newlm$fitted.values,start=start(theline),
									frequency = thefreq)
			# theline=theline*realsd/trueadj*multiplier
			nulm=lmts(actual);nulm
			lmts(truedata*realsd/trueadj*multiplier)
			blackline(theline2,add=T,backline = backwid,frontline = frontwid)
			blackabline(nulm,col="red",
									backline = backwid,frontline = frontwid,
									x1=start(theline),x2=end(theline))
			blackline(theline2,add=T,backline = backwid,frontline = frontwid)
			title(main=paste0("CEEMD Residual (yellow) and Linear Trend (red)","\n",titletext," ",round(start(theline),1)," - ",round(end(theline),1)),cex.main=.9,line=.8)
			if (!is.na(subtext)){
				title(sub=subtext,cex.sub=.9,line=subline)
			}
			fourwalls=par("usr")
			thetrend=nulm$coefficients[2];thetrend
			yval=fourwalls[4]-.1*(fourwalls[4]-fourwalls[3])
			if (nulm$coefficients[2]>0){
				xval=fourwalls[1]+.25*(fourwalls[2]-fourwalls[1])
			} else {
				xval=fourwalls[1]+.75*(fourwalls[2]-fourwalls[1])
			}
			text(xval,yval,paste0("Trend = ",round(thetrend,2)," ", theunits," per year"))
		}
	}
	resetplot()
	invisible(sd(nulm$residuals))
}
# tser=newannblock[,1]
quadtesth=function(tser,multiplier=1,theunits=NULL,doplot=T){
	theheff=floor(heff(as.double(tser)))
	newdata=splitmean(theheff,tser)
	newpval=quadtest(newdata,
										multiplier=multiplier,
										theunits=theunits,doplot=F)$pval
	realacc=quadtest(tser,multiplier=multiplier,
									 theunits=theunits,doplot=F)
	realacc$pval=newpval
	realacc
}

quadtest=function(tser,multiplier=1,theunits=NULL,doplot=T){
	thetime=as.double(time(tser))
	thetimesq=thetime^2/2
	lm1=lm(tser~thetime)
	lm2=lm(tser~thetime+thetimesq)
	lm3=lm(tser~thetimesq)
	sum1=summary(lm1)
	if (doplot) print(sum1)
	sum2=summary(lm2)
	if (doplot) print(sum2)
	# print(summary(lm3))
	# print(paste0("Acceleration = ",
	# 						 round(lm3$coefficients[2]*multiplier,3),
	# 						 " ",theunits," per year"))
	myanova=(anova(lm1,lm2))
	if (doplot) print(myanova)
	# print(anova(lm1,lm3))
	if (doplot) cat("\n")
	if (doplot) {print(paste0("Acceleration = ",
														signif(sum2$coefficients[3,1]*multiplier,2),
														" ",theunits," per year^2"))
		print(paste0("Trend = ",signif(lm1$coef[2]*multiplier,2)," ",
								 theunits," per year"))}
	aback=matrix(c(round(sum2$coefficients[3,1]*multiplier,4),myanova$`Pr(>F)`[2]),1,2,byrow = T)
	colnames(aback)=c("Acc","pVal")
	if (doplot) print(paste0("p-Value = ",round(myanova$`Pr(>F)`[2],3)))
	# invisible(aback)
	invisible(list("acc"=sum2$coefficients[3,1]*multiplier,"pval"= myanova$`Pr(>F)`[2],"lm1" = lm1,"lm2"=lm2))
}

# tser=sfoxc[,11];trueh=.7

# tsp(sfoxc[,11])
hurstquad=function(tser,doplot=F,ylab=NA,xlab="Year",trueh=NA){
	if (is.na(trueh)){
		truelen=round(heff(tser))
	} else{
		truelen=round(length(tser)^(2-2*trueh))
	}
	thestart=time(tser)[1];theend=last(time(tser))
	thebreaks=floor(seq(0,length(tser),length.out = truelen+1))
	breakstimes=seq(thestart,theend,length.out = truelen+1)
	themids=butlast(breakstimes)+diff(breakstimes)/2
	avgs=period.apply(as.double(tser),thebreaks,mean,na.rm=T)
	x=as.double(themids)
	xsquared=x^2/2
	if(doplot) {plot(avgs~x,type="l")
		lines(avgts)
	}
	avgts=ts(avgs,start=themids[1],deltat = diff(themids)[1])
	if (doplot){
		plot(tser,xlab=xlab,ylab=ylab)
		points(themids,avgs,col="red",pch=20,cex=1.2)

	}
	if(truelen<5) data.frame(pval=NA,acc=NA) else	quadtest(avgts,doplot=doplot)
}

`%notin%` <- Negate(`%in%`)

toend=function(thestring,thestart) substr(thestring,thestart,nchar(thestring))

fromstart=function(thestring,thestart) substr(thestring,1,thestart[1])


insertrow <- function(existingDF, newrow, r) {
	existingDF <- rbind(existingDF,newrow)
	existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
	row.names(existingDF) <- 1:nrow(existingDF)
	return(existingDF)
}

rhtovaporpress=function(RH,T){
	esat=6.108*exp(17.27*T/(237.3*T))
	(RH/100)*esat
}

granger=function(x,y,namex="x",namey="y",thesig=2){
	require(lmtest)
	thelag=1
	theresult=signif(grangertest(x,y,thelag)$`Pr(>F)`[2],thesig)
	print(paste0("pValue, ",namey," Granger-causes ",namex,
							 ", lag(",thelag,") = ", theresult))
	theresult2=signif(grangertest(y,x,thelag)$`Pr(>F)`[2],thesig)
	print(paste0("pValue, ",namex," Granger-causes ",namey,
							 ", lag(",thelag,") = ", theresult2))
	cat("\n")
	thelag=2
	theresult=signif(grangertest(x,y,thelag)$`Pr(>F)`[2],thesig)
	print(paste0("pValue, ",namey," Granger-causes ",namex,
							 ", lag(",thelag,") = ", theresult))
	theresult2=signif(grangertest(y,x,thelag)$`Pr(>F)`[2],thesig)
	print(paste0("pValue, ",namex," Granger-causes ",namey,
							 ", lag(",thelag,") = ", theresult2))
}


corrplot2 <- function(
	data,
	titletext=NA,
	subtext=subtextceres,
	method = "pearson",
	corrmethod="color",
	sig.level = 0.05,
	insig = "blank",
	pch.col = addalpha("yellow", .7),
	pch.cex = 5,
	pch = 19,
	diag = FALSE,
	type = "full",
	tl.srt = 90,
	number.font = 1,
	number.cex = .01,
	number.digits=1,
	rect.col="black",order="hclust",addrect=2,
	mar = c(2, 0, 3, 0),
	...) {
	require(corrplot)
	data_incomplete <- data
	data <- data[complete.cases(data), ]
	mat <- cor(data, method = method)
	cor.mtest <- function(mat, method) {
		mat <- as.matrix(mat)
		n <- ncol(mat)
		p.mat <- matrix(NA, n, n)
		diag(p.mat) <- 0
		for (i in 1:(n - 1)) {
			for (j in (i + 1):n) {
				tmp <- cor.test(mat[, i], mat[, j], method = method)
				p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
			}
		}
		colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
		p.mat
	}
	p.mat <- cor.mtest(data, method = method)
	col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
	corrplot(
		mat,
		method = corrmethod,
		col = col(200),
		number.font = number.font,
		number.digits = number.digits,
		mar = mar,
		number.cex = number.cex,
		type = type,
		addCoef.col = "black",
		# add correlation coefficient
		tl.col = "black",
		tl.srt = tl.srt,
		# rotation of text labels
		# combine with significance level
		p.mat = p.mat,
		sig.level = sig.level,
		insig = insig,
		rect.col=rect.col,
		order=order,
		addrect=addrect,
		pch = pch,
		pch.col = pch.col,
		pch.cex = pch.cex,
		# hide correlation coefficiens on the diagonal
		diag = diag,
		...
	)
	title(main=paste0("Correlation, ",titletext),cex.main=1.2,line=3.5)
	title(main=paste0("(Yellow circled values are not statistically significant at p<",sig.level,")"),cex.main=.8,line=2.6)
	title(sub=subtext,cex.sub=.9,line=6)
	title(sub=subtext,cex.sub=.9,line=6,font=2)

}


zerocross=function(tser,doplot=F) {
	if (!is.null(ncol(tser))) tser=tser[,ncol(tser)]
	thelm=lmts(tser)
	# plot(tser,ylim=c(-3,3))
	# abline(thelm)
	newline=tser-thelm$fitted.values
	if (doplot) plot(newline)
	thel=length(newline)
	newfront=as.double(newline[1:(thel-1)])
	newback=as.double(newline[2:(thel)])
	length(which(sign(newfront) != sign(newback)))
}

zerocrosslen=function(tser,doplot=F) {
	if (!is.null(ncol(tser))) tser=tser[,ncol(tser)]
	thelm=lmts(tser)
	# plot(tser,ylim=c(-3,3))
	# abline(thelm)
	newline=tser-thelm$fitted.values
	if (doplot) plot(newline)
	thel=length(newline)
	newfront=as.double(newline[1:(thel-1)])
	newback=as.double(newline[2:(thel)])
	time(tser)[which(sign(newfront) != sign(newback))]
}

'%ni%' <- Negate('%in%')

gridy=function(lwd=1,col="dark gray", ...) grid(NA,NULL,lwd=lwd,col=col, ...)

gridx=function(lwd=1,col="dark gray", ...) grid(NULL,NA,lwd=lwd,col=col, ...)


is.badone=function(x) {((length(x)==0) |
													(length(is.finite(x))==0))}
is.bad=Vectorize(is.badone,vectorize.args = "x")

grepf=function(pattern,x) grep(pattern,x,fixed = T)

dirftp=function(theurl,theextension=".txt",
								uname="10173",upwd="10005"){
	require(RCurl)
	require(XML)
	myopts=curlOptions()
	myopts[["username"]]=uname
	myopts[["userpwd"]]=upwd
	getHTMLLinks(
		getURL(
			theurl,
			verbose = F,
			.opts = myopts,
			ftp.use.epsv = TRUE,
			dirlistonly = TRUE
		),
		xpQuery = paste0("//a/@href[\"",
										 theextension,
										 "\"=substring(., string-length(.) - 3)]")
	)
}


removeall=function(){
	rm(list=ls(all.names = T))
}

# x= as.vector(thetest);weights=as.vector(latmatrix);conf.level=0.95
weighted.ttest.ci <- function(x, weights, conf.level = 0.95) {
	require(Hmisc)
	nx <- length(x)
	df <- nx - 1
	vx <- wtd.var(x, weights, normwt = TRUE) ## From Hmisc
	mx <- weighted.mean(x, weights,na.rm = T)
	stderr <- sqrt(vx/nx)
	tstat <- mx/stderr ## not mx - mu
	alpha <- 1 - conf.level
	cint <- qt(1 - alpha/2, df)
	cint <- tstat + c(-cint, cint)
	cint * stderr
}

weightedcormat=function(dep,ind,weights=latmatrix){
	dep=allna(as.vector(dep))
	ind=allna(as.vector(ind))
	weights=as.vector(weights)
	dep[!is.finite(ind)]=NA
	weights[!is.finite(ind)]=NA
	ind[!is.finite(dep)]=NA
	weights[!is.finite(dep)]=NA
	dep=na.omit(dep)
	ind=na.omit(ind)
	weights=na.omit(weights)
	weightedCorr(dep,ind,weights=weights,method="Spearman")
}

lmmat=function(dep,ind,themask=1,c=NULL,weights=latmatrix){
	dep=as.vector(dep*themask)
	ind=as.vector(ind*themask)
	if (!is.null(c)) {
		c=as.vector(c)
		if (!is.null(weights)){
			weights=as.vector(weights)
			return(lm(dep~ind+c,weights = weights))
		} else {
			return(lm(dep~ind+c))
		}
	} else {
		if (!is.null(weights)){
			weights=as.vector(weights)
			return(lm(dep~ind,weights = weights))
		} else {
			return(lm(dep~ind))
		}
	}
}

maskflip=function(mask){
	mask[is.na(mask)] = 2
	mask[mask==1] = NA
	mask[mask==2] = 1
	mask
}



tsileif = function() {
	# groupurl="http://www.sidc.be/silso/DATA/GN_y_tot_V2.0.txt"
	groupurl="http://www.sidc.be/silso/DATA/GroupNumber/GNbb2_y.txt"
	group=read.table(groupurl,fill=T)
	# str(group)
	# group=group[1:406,]
	gts=ts(group[,2],start = 1610,frequency = 1)

	1360.43 + 0.24 * gts ^ 0.7
}


removedupes=function(alltime,alldata){
	badboys=which(duplicated(alltime))

	while (length(badboys)>0){
		i=last(badboys)
		others=which(alltime==alltime[i]);others
		alldata[others[1]]=mean(alldata[others])
		alltime=alltime[-butfirst(others)]
		alldata=alldata[-butfirst(others)]
		badboys=which(duplicated(alltime))
	}
	zoo(alldata,alltime)
}

corerror=function(r,n) sqrt((1-r^2)/(n-2))



# tarr=allt2
lagarray=function(tarr,thelag=1){
	if (thelag==0) {
		return(tarr)
		break()
	}
	trows=dim(tarr)[1]
	tcols=dim(tarr)[2]
	nmon=dim(tarr)[3]
	if (thelag>0) {
		blanklayer=array(NA,c(trows,tcols,thelag))

		return(abind(blanklayer,
								 tarr[,,1:(nmon-thelag)]))
		break()
	}
	if (thelag<0) {
		blanklayer=array(NA,c(trows,tcols,-thelag))

		return(abind(tarr[,,(-thelag+1):nmon],blanklayer))
		break()
	}
}

tseriestoarray=function(tser,trow=180,tcol=360){
	array(rep(tser,each=trow*tcol),c(trow,tcol,length(tser)))
}

propercase <- function(x) {
	(s <- strsplit(tolower(x), " ")[[1]])

	test=paste(toupper(substring(s, 1, 1)), substring(s, 2),
				sep = "", collapse = " ")
	test
}
# x=(sitename)
# propercase(sitename)
# propercase("ToM joNes")

numbers2words <- function(x){
	## Function by John Fox found here:
	## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
	## Tweaks by AJH to add commas and "and"
	if (x==0){
		return("zero")
		break()
	}
	helper <- function(x){

		digits <- rev(strsplit(as.character(x), "")[[1]])
		nDigits <- length(digits)
		if (nDigits == 1) as.vector(ones[digits])
		else if (nDigits == 2)
			if (x <= 19) as.vector(teens[digits[1]])
		else trim(paste(tens[digits[2]],
										Recall(as.numeric(digits[1]))))
		else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
																			Recall(makeNumber(digits[2:1]))))
		else {
			nSuffix <- ((nDigits + 2) %/% 3) - 1
			if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
			trim(paste(Recall(makeNumber(digits[
				nDigits:(3*nSuffix + 1)])),
				suffixes[nSuffix],"," ,
				Recall(makeNumber(digits[(3*nSuffix):1]))))
		}
	}
	trim <- function(text){
		#Tidy leading/trailing whitespace, space before comma
		text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
		#Clear any trailing " and"
		text=gsub(" and$","",text)
		#Clear any trailing comma
		gsub("\ *,$","",text)
	}
	makeNumber <- function(...) as.numeric(paste(..., collapse=""))
	#Disable scientific notation
	opts <- options(scipen=100)
	on.exit(options(opts))
	ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
						"eight", "nine")
	names(ones) <- 0:9
	teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
						 "sixteen", " seventeen", "eighteen", "nineteen")
	names(teens) <- 0:9
	tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
						"ninety")
	names(tens) <- 2:9
	x <- round(x)
	suffixes <- c("thousand", "million", "billion", "trillion")
	if (length(x) > 1) return(trim(sapply(x, helper)))
	helper(x)
}

lit2gal=function(litres) litres*0.26417205235815
gal2lit=function(gallons) 1/lit2gal(1)*gallons


pr2alt=function(p,efold=0.8660150466){
	ifelse(p==0, 100,log(p/1013)/log(efold))
}

# t=matrix(26:31,2);t
rh2ah=function(t=25,rh=50,p=1.011){
	thelength=length(t)
	thedims=dim(t)
	if (is.null(thedims)){
		if (length(p) != thelength) {
			rep(p,length.out=thelength)
		}
	} else {
		# if(dim(p) != dim(t)){
			p=matrix(rep(p,thedims[1]*thedims[2],nrow=thedims[1],
									 ncol=thedims[2]))
		# }
	}
	marelac::air_density(t,p)*marelac::air_spechum(t,rh,p)*1000
} # g per m3

# mround=function(x,nearest=10) as.double(round(x/m)*m)


mergezoo=function(somezoo, ...){
	themerge=merge.zoo(somezoo,...)
	thezoo=zoo(rowMeans(themerge,na.rm=T),time(themerge))
	class(time(thezoo))=class(time(somezoo))
	thezoo
}

mergezoov=Vectorize(mergezoo,"somezoo")

ablineclipv=Vectorize(ablineclip,c("x1","x2","y1","y2"))

period.return=function(x,ep){
	period.apply(x,ep,function(x) x)
}

apply.return.monthly=function(x){
	{
		ep <- endpoints(x, "months")
		period.return(x, ep)
	}
}

apply.return.daily=function(x){
	{
		ep <- endpoints(x, "days")
		period.return(x, ep)
	}
}

apply.return.hourly=function(x){
	{
		ep <- endpoints(x, "hours")
		period.return(x, ep)
	}
}

apply.return.yearly=function(x){
	{
		ep <- endpoints(x, "years")
		period.return(as.double(x), ep)
	}
}

removena=function(x) x[which(is.finite(x))]
badna=function(x) x[which(!is.finite(x))]


residzoo=function(monthzoo,removemean=F){
	themonths=month(time(monthzoo))
	themeans=tapply(monthzoo,themonths,mean,na.rm=T)
	monthzoo-as.double(themeans[themonths])+
		ifelse(removemean,0,mean(monthzoo,na.rm=T))
}


makesin=function(length,period,amp=1) {
	sin(c(0:(length-1))*2*pi/period)*amp
}


makesinplus=function(period,nsamp=1440,amp=1,doplot=F){
	answer=zoo(dsin((0:(2*nsamp-1))*period/2)*amp,(1:(nsamp*2))/nsamp)
	if (doplot) plot(answer)
	invisible(answer)
}


makecos=function(length,period,amp=1) cos(c(0:(length-1))*2*pi/period)*amp

insertna=function(source,target){
	for(i in which(is.na(source))) {
		target <- append(target, NA, after=(i-1))
	}
	target
}


yearstart=function(tser) floor(time(tser))[1]

apply.metquarterly=function(tser, passfunction, ...){
	require(xts)
	thestart=yearstart(tser)
	tfront=timeshift(tser,1);tsp(tfront)
	quartime=ts(butfirst(
		apply.quarterly(as.zoo(tfront),passfunction, ...)),
		start=thestart,frequency = 4)
	# ts(matrix(as.double(quartime),ncol=4,byrow = T),start=thestart,frequency = 1)
}

# tsp(timeshift(rutts,-6))
# thesums=ts(apply.yearly(as.zoo(timeshift(rutts,-6)),mean),start=1972,frequency = 1)
# thesums=butfirst(butlast(thesums))

# plot(thesums)

averagedupes=function(themat,dupecol,valcol){
	thedupes=which(duplicated(themat[,dupecol]));thedupes

	while(length(thedupes)>0){
		i=thedupes[1];i
		theval=themat[i,dupecol];theval
		alldupes=which(themat[,dupecol]==theval);alldupes
		alldupes=alldupes[-which(alldupes==i)];alldupes
		themat[i,valcol]
		themat[alldupes,valcol]
		newval=mean(c(themat[i,valcol],themat[alldupes,valcol]));newval
		themat[i,valcol]=newval
		themat=themat[-alldupes,]
		thedupes = which(duplicated(themat[,dupecol]));thedupes
	}
	themat
}

scalets=function(tser,themean=0,thesd=1){
	ts(scale(tser,themean,thesd),start=start(tser),
		 frequency = frequency(tser))
}
rowmeansts=function(tser,na.rm=T){
	ts(rowMeans(tser,na.rm = na.rm),start=start(tser),
		 frequency = frequency(tser))
}
rowsems=function(tser,na.rm=T){
	goodcounts=apply(tser,1,function(x) length(is.finite(x)))
	goodcounts[goodcounts==0]=NA
	goodcounts=sqrt(goodcounts)
	rowSds(tser,na.rm=na.rm)/goodcounts
}
rowsemsts=function(tser,na.rm=T){
	ts(rowsems(tser,na.rm = na.rm),start=start(tser),
		 frequency = frequency(tser))
}
rowsumsts=function(tser,na.rm=T){
	ts(rowSums(tser,na.rm = na.rm),start=start(tser),
		 frequency = frequency(tser))
}

# parta=scalebox[,1]
# partb=scalebox[,2]
splitep=function(x,somevec){
	round(seq(0,length(somevec),length.out = x+1))
}

splitmean=function(x,somevec,na.rm=F){
	thenums=period.apply(as.double(somevec),splitep(x,somevec),mean,na.rm=na.rm)
	# (thenums-mean(thenums,na.rm=na.rm))/sd(thenums)*sd(somevec)+mean(thenums)
}

splitfunction=function(x,somevec,FUN,...){
	period.apply(as.double(somevec),splitep(x,somevec),FUN,...)
}
# somevec=spotshort
# x=theheff+1;x
# splitep(theheff+1,parta)
# parta=spotshort
# splitmean(x,parta)
splitheff=function(parta){
	theheff=floor(heff(as.double(parta)))+1;theheff
	splitmean(theheff,parta)
}

# parta=msuts3[,"Globe"]
# partb=shortspots
# parta=as.double(downres)
# parta=msuceres;H=NA;partb=NA
# pvalueh(msuceres)
# pvalueh(tempmon)
pvalueh=function(parta,partb=NA,H=NA,doadj=F) {
	if (length(partb)==1){
		theheff=floor(heff(as.double(parta),H=H));theheff
		newdata=splitmean(theheff,parta);newdata
		if (doadj) newdata=(newdata-mean(newdata))*sd(parta)/sd(newdata)+mean(newdata)


		if (class(parta) %in% c("zoo","ts")){
			(thetrend=lmts(parta)$coef[2])
			newtime=splitmean(theheff,as.double(time(parta)));newtime
			deltanew=diff(as.vector(newtime))[1]

			return(list(lm=lmts(ts(as.vector(newdata),newtime[1],deltat=deltanew)), summary=summary(lmts(ts(as.vector(newdata),newtime[1],deltat=deltanew))),newtser=ts(as.vector(newdata),newtime[1],deltat=deltanew)))} else{
				return(summary(lm(newdata~seq_along(newdata))))
			}
	} else {
		theheff=floor(commonheff(parta,partb,H=H));theheff
		newdata=splitmean(theheff,parta);newdata
		newdatb=splitmean(theheff,partb);newdatb
		newdata=(newdata-mean(newdata))*sd(parta)/sd(newdata)+mean(newdata)
		newdatb=(newdatb-mean(newdatb))*sd(partb)/sd(newdatb)+mean(newdatb)
		return(list(lm=lm(newdata~newdatb),summary=summary(lm(newdata~newdatb)),newdata=newdata,newdatb=newdatb))
	}
}

# parta=tempmon;partb=NA;H=NA
psummaryh=function(parta,partb=NA,H=NA) {
	if (length(partb)==1){

		theheff=floor(heff(as.double(parta),H=H));theheff
		newdata=splitmean(theheff,parta);newdata
		return(summary(lm(newdata~seq_along(newdata))))

	} else {
		theheff=floor(commonheff(parta,partb,H=H));theheff
		newdata=splitmean(theheff,parta);newdata
		newdatb=splitmean(theheff,partb);newdatb
		return(summary(lm(newdata~newdatb)))
	}
}

seff=function(tser){
	theac=acf(tser,plot=F)$acf[2]
	length(tser)*(1-theac)/(1+theac)
}

pvalues=function(parta) {
		theheff=floor(seff(as.double(parta)));theheff
		newdata=splitmean(theheff,parta);newdata
		return(round(summary(lm(newdata~seq_along(newdata)))$coef[2,4],6))
}


# pvalueh=function(parta,partb=NA,H=NA) {
# 	if (length(partb)==1){
# 		mysum=summary(lmts(parta))
# 	} else {
# 		mysum=summary(lm(parta~partb))
# 	}
# 	# heff1=heff(parta)
# 	# heff2=heff(partb)
# 	as.double(1-pf(mysum$fstatistic[1],
# 								 mysum$fstatistic[2],
# 								 commonheff(parta,partb,H=H)))
# }

# pf(mysum$fstatistic[1],
# 	 mysum$fstatistic[2],
# 	 commonheff(parta,partb,H=H))

# pvalueh(testdata)
bonferroni=function(x,pvalue=0.05){
	1-(1-pvalue)^(1/x)
}

theform=mylag[,1]~mylag[,2]
as.name(substr(theform[3],1,nchar(theform[3])))
eval(as.name("pi"))
theform[2]


radiationperdeltat=function(t,dT){
	1 + 6 * (dT/t)^2
}



zonemeans=function(x){
	ts(rev(rowMeans(x,na.rm=T)),start=-89.5,end=89.5)
}

co2toph=function(co2) {8.49 -.00107*co2}
co2toph2=function(co2) {8.3591759-0.0008939 *co2}
# co2toph(400)-co2toph(600)
zoototsmon=function(maxzoo,thefunction=mean,usena=T){
	require(xts)
	if(usena){
	maxmon=apply.monthly(maxzoo,thefunction,na.rm=T)
	} else {
		maxmon=apply.monthly(maxzoo,thefunction)
	}
	startyear=year(start(maxmon));startyear
	startmon=month(start(maxmon));startmon
	endyear=year(end(maxmon));endyear
	endmon=month(end(maxmon));endmon
	dummytime=ts(NA, start=c(startyear,startmon),
							 end=c(endyear,endmon),frequency = 12)
	zooyears=year(maxmon);zooyears
	zoomonths=month(maxmon);zoomonths
	zoodecimal=apply(cbind(zooyears,zoomonths),1,twodatetodecimal)
	tsdecimal=round(time(dummytime),2)
	theoverlap=match(zoodecimal,tsdecimal)
	dummytime[theoverlap]=as.double(maxmon)
	dummytime
}

# maxzoo=liv2
# plot(liv2)
# maxzoo=chinacogdp
zooanntotsann=function(maxzoo){
	require(xts)
	maxmon=maxzoo
	startyear=as.double(time(maxmon)[1]);startyear
	endyear=as.double(last(time(maxmon)));endyear
	dummytime=ts(NA, start=startyear,
							 end=endyear,frequency = 1)
	zooyears=as.double(time(maxmon));zooyears
	zoodecimal=zooyears
	tsdecimal=round(time(dummytime),2);tsdecimal
	theoverlap=match(zoodecimal,tsdecimal)
	dummytime[theoverlap]=as.double(maxmon)
	dummytime
}

# plot(zooanntotsann(chinacogdp))

zoototsann=function(maxzoo,thefunction=mean){
	require(xts)
	maxmon=maxzoo
	startyear=year(start(maxmon));startyear
	endyear=year(end(maxmon));endyear
	dummytime=ts(NA, start=startyear,
							 end=endyear,frequency = 1)
	zooyears=year(maxmon);zooyears
	zoodecimal=zooyears
	tsdecimal=round(time(dummytime),2);tsdecimal
	theoverlap=match(zoodecimal,tsdecimal)
	dummytime[theoverlap]=as.double(maxmon)
	dummytime
}

# thematrix=fuelmatrix[,c(1:5,8,9,10,7)];ytoplim=3100

# areaplot=function(thematrix,allcolors=NA,ytoplim=NA,
# 									thealpha=.7,add=F,legendloc="topleft",
# 									legendcols=1,
# 									dolegend=T,usepercent=F,theborder=NA,
# 									thepicture="",thefade=0,...){
#
# 	stackdata=function(thetop,thebottom,thefill="blue"){
# 		if (length(thebottom)==1){
# 			thebottom=rep(0,length(thetop))
# 		}
# 		thetop[is.na(thetop)]=thebottom[is.na(thetop)]
# 		thelen=length(thetop)
# 		xall=time(thetop)
# 		thex=c(xall[1],xall,rev(xall))
# 		they=c(thebottom[1],thetop,rev(thebottom))
# 		# plot(thetop,ylim=c(0,max(thetop)))
# 		polygon(thex,they,col = thefill,border = theborder)
# 	}
# 	if (add==F) resetplot()
# 	if (length(allcolors)==1) allcolors=rev(rainbow(ncol(thematrix)))
# 	# allcolors=allcolors[c(1,5,2,6,3,7,8,4,9)]
# 	# allcolors[4]=allcolors[9]
# 	# allcolors[9]="gray25"
# 	# allcolors=allcolors[c(3,2,1,4,6,5,7:9)]
# 	# # allcolors=allcolors[c(3,2,1,4,6,5,7:9)]
# 	# allcolors[3]="salmon"
# 	if (is.na(ytoplim)) {ytoplim=max(rowSums(thematrix,
# 																					 na.rm = T))}
# 	if (add==F) plot(thematrix[,1],ylim=c(0,ytoplim),type="n",...)
# 	if (nchar(thepicture)>0){
# 		plotpicture(thepicture,fade=thefade,...)
# 	}
# 	i=1
# 	for (i in 1:ncol(thematrix)){
# 		# i=i+1
# 		if (i==1) {
# 			thetop=thematrix[,i]
# 			thebottom=NA
# 		} else {
# 			thetop=ts(rowSums(thematrix[,1:(i)]),start=start(thematrix),
# 								frequency = frequency(thematrix))
# 			if (i == 2) {
# 				thebottom=thematrix[,1]
# 			} else {
# 				thebottom=rowSums(thematrix[,1:(i-1)])
# 			}
# 		}
# 		# lines(thebottom,lwd=3)
# 		# lines(thetop,lwd=3)
#
# 		stackdata(thetop,thebottom,thefill = allcolors[i])
# 	}
#
# 	ablineclip(h=0,x1=start(thematrix),x2=end(thematrix))
# 	ablineclip(v=start(thematrix),y1=0,y2=sum(thematrix[1,],na.rm=T))
# 	ablineclip(v=end(thematrix),y1=0,y2=sum(thematrix[nrow(thematrix),],na.rm = T))
# 	if (dolegend==T){
# 		legendstring=paste0(rev(colnames(thematrix)),"  ")
# 		if (usepercent){
# 			finalrow=thematrix[nrow(thematrix),]
# 			thepct=finalrow/sum(finalrow,na.rm = T)*100
# 			thesmall=which(thepct<5)
# 			finalpct=round(thepct,0)
# 			if (length(thesmall)>0){
# 				finalpct[thesmall]=round(thepct[thesmall],1)
# 			}
# 			finalpct[which(is.na(finalpct))]=0
# 			legendstring=paste0(legendstring,":  ",
# 													rev(finalpct),"%   ")
# 		}
# 		par(font=2)
# 		legend(legendloc, box.col = NA,bg=addalpha("white",thealpha),
# 					 legend = legendstring,ncol=legendcols,
# 					 col = rev(allcolors),lwd=c(8), cex=.8)
# 		# fossilpct=sum(finalrow[1:3],na.rm = T)/sum(finalrow,na.rm = T)*100
# 		# renewpct=sum(finalrow[c(6:9)],na.rm = T)/sum(finalrow,na.rm = T)*100
# 		# legend("top",c(paste0("Fossil Fuels: ",
# 		# 											round(fossilpct,0),"%"),
# 		# 							 paste0("Renewables: ",
# 		# 							 			 round(renewpct,0),"%")),
# 		# 			 lwd=.01,box.col=NA,
# 		# 			 col=NA,cex=.85,bg=NA)
# 		# par(font=1)
# 	}
# }

# areaplot(fuelmatrix[,1:6],ylab="Million Tonnes of Oil Equivalent (Mtoe)")

shadowtext <- function(x, y=NULL, labels,
											 col='white', bg='black',
											 theta= seq(pi/4, 2*pi, length.out=8),
											 r=0.1, ... ) {
	xy <- xy.coords(x,y)
	xo <- r*strwidth('A')
	yo <- r*strheight('A')
	for (i in theta) {
		text( xy$x + cos(i)*xo, xy$y + sin(i)*yo,
					labels, col=bg, ... )
	}
	text(xy$x, xy$y, labels, col=col, ... )
}

seaoxy=function(tempc,depthm=0) {
	(149.25*(1/ctok(tempc)*200)^9.28+2.91)*(depthm/10+1)
} # mg/l


elevation.from.pressure=function(Tair=-30,pressure=300,surfpress=1013.171) -29.3*(Tair+273.15)*log(pressure/surfpress)/1000

# elevation.from.pressure(pressure=600)/3.549
# elevation.from.pressure(pressure=400)

# y=landline
# plot(landline)
# y=ts(rep(1,180),-89.5)
# linelen(landline)
# plot(y)
# y=ts(rnorm(100),1)
linelen=function(y,x=NULL,doscale=F){
	if (length(x)!=0){
		x=y[,2]
		y=y[,1]
	} else {
		x=as.double(time(y))
		y=as.double(y)
	}
	y=dtrendlin(y)
	if (doscale){
		x=scale(x)
		y=scale(y)
	}
	xdiff=diff(x)
	ydiff=diff(y)
	(badboys=which(is.na(ydiff)))

	if (length(badboys)>0) {
		xdiff=xdiff[-which(is.na(ydiff))]
		ydiff=ydiff[-which(is.na(ydiff))]
	}

	truex=sum(xdiff)
	# i=1
	(fullen=sum(sqrt(xdiff^2 +ydiff^2)))
	(fullen/truex-1)*100
}

tslen=function(tser,start=start(tser),end=end(tser),
							 frequency=frequecy(tser)){
	dval=diff(as.double(tser))
	dtime=diff(as.double(time(tser)))
	sum(sqrt(dval^2+dtime^2))
}


# left=dalton
drawrect=function(left,right,col="blue",alpha=.3,
									theborder=NA,bottom=NA,top=NA,...){
	if (length(left)==2) {
		right=left[2]
		left=left[1]
	}
	usr=par("usr");usr
	if (is.na(bottom)) bottom = usr[3]-(usr[4]-usr[3])
	if (is.na(top)) top = usr[4]+(usr[4]-usr[3])
	rect(xleft = left,ybottom=bottom,xright = right,ytop=top,
			 col=addalpha(col,alpha),border = theborder)
}


plotredblue = function(tser,add=F,alpha=1,...){
	plusvals=which(tser>0)
	minusvals=which(tser<=0)
	plustser=tser;plustser[minusvals]=NA
	minustser=tser;minustser[plusvals]=NA
	col1=addalpha("red",alpha)
	col2=addalpha("blue",alpha)

	if (add){
		lines(plustser,type="h",col=col1,ylim=c(min(tser),max(tser)),...)
		lines(minustser,type="h",col=col2)

	} else {
		plot(plustser,type="h",col=col1,ylim=c(min(tser),max(tser)),...)
		lines(minustser,type="h",col=col2)
	}
}



gtCO2toppmv=function(x) x/(7.76)

ppmv2gtC=function(x) x/gtCtoppmv(1)


gtCtoppmv=function(x) x/(2.13)

tabletots=function(thetable,start,frequency=12){
	ts(as.vector(aperm(data.matrix(thetable))),
		 start=start,frequency=frequency)
}


s.e.cor=function(correl,n){
	sqrt((1-correl^2)/(n-2))
}

# tser=mints
# tser=testts
# tser=olrts
# tser=temp91
removecycle=function(tser,removemean=F) {
	tserraw=dtrendlin(tser)
	# tsp(tserraw)
	# plot(tserraw)
	if(class(tser)=="ts") {
		thecycle=cycle(tserraw)
	} else if(class(tser)=="zoo"){
		thecycle=hour(tser)+minute(tser)/3600
		thetapp=tapply(tser,thecycle,mean,na.rm=T)
		dimnumbers=attributes(thetapp)$dimnames[[1]]
		thesub=thetapp[match(thecycle,dimnumbers)]
		return(tser-as.double(thesub)+
					 	ifelse(removemean==F,mean(tser,na.rm=T),0))

	}
	tserraw=allna(tserraw)
	cyclemeans=tapply(tserraw,thecycle,mean,na.rm=T)
	startmon=start(tser)[2];startmon
	if (startmon==1){
		repcycle=cyclemeans
	} else {
		repcycle=cyclemeans[c(startmon:12,1:(startmon-1))]
	}
	allna(tser-rep(repcycle,length.out=length(tser)))
}

removecycle=function(tser,removemean=F) {
	tserraw=dtrendlin(tser)
	# tsp(tserraw)
	# plot(tserraw)
	if(class(tser)=="ts") {
		thecycle=cycle(tserraw)
	} else if(class(tser)=="zoo"){
		thecycle=hour(tser)+minute(tser)/3600
		thetapp=tapply(tser,thecycle,mean,na.rm=T)
		dimnumbers=attributes(thetapp)$dimnames[[1]]
		thesub=thetapp[match(thecycle,dimnumbers)]
		return(tser-as.double(thesub)+
					 	ifelse(removemean==F,mean(tser,na.rm=T),0))

	}
	tserraw=allna(tserraw)
	cyclemeans=tapply(tserraw,thecycle,mean,na.rm=T)
	startmon=start(tser)[2];startmon
	if (startmon==1){
		repcycle=cyclemeans
	} else {
		repcycle=cyclemeans[c(startmon:12,1:(startmon-1))]
	}
	allna(tser-rep(repcycle,length.out=length(tser)))
}

# tser=tempmon
cycleerror=function(tser) {
	tserraw=dtrendlin(tser)
	# plot(tserraw)
	thecycle=cycle(tserraw)
	tserraw=allna(tserraw)
	cyclesems=tapply(tserraw,thecycle,sem)
	startmon=start(tser)[2];startmon
	if (startmon==1){
		repcycle=cyclesems
	} else {
		repcycle=cyclesems[c(startmon:12,1:(startmon-1))]
	}
	ts(allna(rep(repcycle,length.out=length(tser))),start=start(tser),frequency = frequency(tser))
}

to.vector=function(datablock,thecols=2:13) {
	lastline=as.vector(data.matrix(datablock[dim(datablock)[1],thecols]));lastline
	firstline=as.vector(data.matrix(datablock[1,thecols]));firstline
	missingdata=length(thecols)-max(which(is.finite(lastline)))
	missingfirst=length(thecols)-max(which(is.finite(firstline)))
	thetest=as.vector(aperm(data.matrix(datablock[,thecols])))
	testlen=length(thetest)
	thetest[(missingfirst+1):(testlen-missingdata)]
}


allna=function(thedata) {
	thedata[which(!is.finite(thedata))]=NA
	thedata
}

allna(c(3,NaN,5))

specdigits=function(n,digits=2){
	sprintf(paste0("%0",digits,"d"),n)
}

specdigits(5)

flip=function(x) -(x-mean(x))+mean(x)

# picture="bg horizon mlo corr.png"
# ccf(avgresid,meits,xlab="Lag (Years, Positive is MLO Lagging MEI",
#     ylab="Correlation",ci.col=NA,ylim=c(-1,1),main="")
# xlim=NA;ylim=NA;xlab="Year";ylab=NA;fade=0
# ima=theimage

# picture="bg spy vs spy.png"
# plotpicture(picture,fade=.5)


plotpicture=function(picture,
										 xlim = NA,
										 ylim = NA,
										 x1 = NA,
										 x2 = NA,
										 y1 = NA,
										 y2 = NA,
										 justfade = F,
										 addata = NA,
										 leftinset = NA,
										 rightinset = NA,
										 topinset = NA,
										 bottominset = NA,
										 xlab = "Year",
										 ylab = NA,
										 fade = 0,
										 ...
){
	require(png)

	usr=par("usr")
	xdiff=(usr[2]-usr[1])
	xdiff=xdiff-xdiff/1.04
	ydiff=(usr[4]-usr[3])
	ydiff=ydiff-ydiff/1.04
	if (class(picture)=="character"){
		ima <- readPNG(paste0("~/Pictures/",picture))
	} else{
		ima=picture
	}

	if (length(xlim)==1) xlim=c(usr[1]+xdiff,usr[2]-xdiff)
	if (length(ylim)==1) ylim=c(usr[3]+ydiff,usr[4]-ydiff)
	# thedata=c(1:2)
	# if (length(addata)>1) thedata=addata
	# if (justfade==F) plot(thedata, type='n', xlab=xlab, ylab=ylab,...)

	if (is.na(x1)) x1=usr[1]
	if (is.na(x2)) x2=usr[2]
	if (is.na(y1)) y1=usr[3]
	if (is.na(y2)) y2=usr[4]

	if (!is.na(leftinset)) x1=x1+leftinset
	if (!is.na(rightinset)) x2=x2-rightinset
	if (!is.na(topinset)) y2=y2-topinset
	if (!is.na(bottominset)) y1=y1+bottominset

	# rasterImage(ima, usr[1], usr[3], usr[2], usr[4])
	if (justfade==F) rasterImage(ima, x1,y1,x2,y2)
	rect(xleft = xlim[1]-diff(xlim),xright = xlim[2]+diff(xlim),
			 ybottom = ylim[1]-diff(ylim),ytop = ylim[2]+diff(ylim),
			 col = addalpha("white",fade))
}
# plotpicture("bg horizon mlo corr.png")

# ts1=msuglobe;ts2=meits
# plot(scale(ts1))
# lines(scale(stats::lag(ts2,-6)),col="red")
# cor(ts1,stats::lag(ts2,-6))
# stats::lag(ts2,-6)[1]
removets=function(ts1in,ts2in,thelag=0){
	inbox=ts.intersect(ts1in,ts2in)
	ts1=inbox[,1];ts2=inbox[,2]
	thetrend=lmts(ts1)$fitted.values
	flatts=ts1-thetrend
	thetrend2=lmts(ts2)$fitted.values
	flatts2=ts2-thetrend2
	theccf=(ccf(flatts,flatts2))
	thelag=which.max(theccf$acf)-which(theccf$lag==0)

	if (thelag!=0) {
		ts2lag=ts(stats::lag(as.double(ts2in),thelag))
		nubox=ts.intersect(ts1in,ts2lag)
	}

	flatts-lm(flatts~flatts2)$fitted.values+thetrend
}

# plot(ts1)
# lines(removets(ts1,ts2),col=red)
#
# plot(ts1)
# plot(ts2)
# theccf=(ccf(ts1,ts2))
# which.max(theccf$acf)-which(theccf$lag==0)

# head(ts2)
# head(flatts)
# length(ts2)
# length(ts1)
# length(flatts)
# cor(flatts,ts2)
# cor(ts1,lag(as.double(ts2),thelag),use="pairwise.complete.obs")
#
# plot(ts1)
# lines(flatts-lm(flatts~ts2)$fitted.values+butfirst(thetrend,thelag),col="red")
# head(ts2)
# summary(lm(ts1~stats::lag(ts2,0)))

# tser=tsmonthlyspots
resetplot=function(bot=.9,left=.7,top=.6,right=.25,mgp=c(2,.8,0),mfrow=c(1,1),cex.axis=1,font=2, axisfont=2,labelfont=2) {
	close.screen(all.screens = T)
	# clrconsole()
	par(mai=c(bot,left,top,right),
			cex.axis=1,cex.lab=1,
			mfrow=mfrow,
			mgp=mgp,
			cex.axis=cex.axis,
			font=font,
			font.axis=axisfont,
			font.lab=labelfont)
}
plotreset=resetplot
# tser=maxblock[,i]
tstoann=function(tser,cutout=12){
	require(xts)
	require(lubridate)
	thezoo=as.zoo(tser)
	anncount=as.double(apply.yearly(thezoo,function(x) length(which(is.finite(x)))))
	annzoo=apply.yearly(thezoo,mean,na.rm=T)
	# if(anncount[1]<cutout) annzoo=butfirst(annzoo)
	# if(anncount[length(anncount)]<cutout) annzoo=butlast(annzoo)
	badyears=which(anncount<cutout)
	thets=ts(as.double(annzoo),start=year(start(annzoo)),
					 frequency = 1)
	thets[badyears]=NA
	trimNA(thets)
}
tstoannsum=function(tser,cutout=12){
	require(xts)
	require(lubridate)
	thezoo=as.zoo(tser)
	anncount=as.double(apply.yearly(thezoo,function(x) length(which(is.finite(x)))))
	annzoo=apply.yearly(thezoo,sum,na.rm=T)
	# if(anncount[1]<cutout) annzoo=butfirst(annzoo)
	# if(anncount[length(anncount)]<cutout) annzoo=butlast(annzoo)
	badyears=which(anncount<cutout)
	thets=ts(as.double(annzoo),start=year(start(annzoo)),
					 frequency = 1)
	thets[badyears]=NA
	trimNA(thets)
}
# plot(tstoannsum(tsmonthlyspots))
detachAllPackages <- function() {

	basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")

	package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]

	package.list <- setdiff(package.list,basic.packages)

	if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)

}

# reflat=thecentroid[2];reflat
lattofeet=function(reflat){latlen=matrix(NA,ncol=2,nrow=7)
lonlen=matrix(NA,ncol=2,nrow=7)
latlen=matrix(NA,ncol=2,nrow=7)


lonlen[1,]=c(0,365220.5884178935)
latlen[1,]=c(0,362775.7596463919)

lonlen[2,]=c(15,352855.119688254)
latlen[2,]=c(15,363019.90764380957)

lonlen[3,]=c(30,316555.29716035206)
latlen[3,]=c(30,363688.33032771584)

lonlen[4,]=c(45,258683.22840902332)
latlen[4,]=c(45,364604.73335045605)

lonlen[5,]=c(60,183070.43108390004)
latlen[5,]=c(60,365524.9913523626)

lonlen[6,]=c(75,94822.62308833752)
latlen[6,]=c(75,366201.12399460137)

lonlen[7,]=c(90,0)
latlen[7,]=c(90,366449.1269711854)

equatorlen=lonlen[1,2]*360;equatorlen
equatorlen/5280
equatrad=equatorlen/pi;equatrad

thecos=dcos(seq(0,90,15))*lonlen[1,2];thecos

# plot(lonlen[,2]~lonlen[,1])
# lines(thecos~lonlen[,1])
#
#
# plot(lonlen[,2]-thecos~seq(0,90,15))
lonspline=spline(x=seq(0,90,15),y=lonlen[,2]-thecos,xout = seq(0,90,.01) )
# lines(lonspline$y~lonspline$x)

# plot(latlen[,2]~latlen[,1])
latspline=spline(x=seq(0,90,15),y=latlen[,2],xout = seq(0,90,.01) )
# lines(latspline$y~latspline$x)

# refround=42
# lonspline$x[4200:4300]
refround=round(reflat,2);refround
lonspline$x=round(lonspline$x,2)
lonspline$y=round(lonspline$y,2)
lonfeet=dcos(reflat)*lonlen[1,2] +lonspline$y[which(lonspline$x==refround)];lonlen;lonfeet
latfeet=latspline$y[which(lonspline$x==refround)];latlen;latfeet
data.frame(lonfeet,latfeet)}

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
										result=c("none", "html", "latex")){
	#Compute correlation matrix
	require(Hmisc)
	x <- as.matrix(x)
	correlation_matrix<-rcorr(x, type=method[1])
	R <- correlation_matrix$r # Matrix of correlation coeficients
	p <- correlation_matrix$P # Matrix of p-value

	## Define notions for significance levels; spacing is important.
	mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))

	## trunctuate the correlation matrix to two decimal
	R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

	## build a new matrix that includes the correlations with their apropriate stars
	Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
	diag(Rnew) <- paste(diag(R), " ", sep="")
	rownames(Rnew) <- colnames(x)
	colnames(Rnew) <- paste(colnames(x), "", sep="")

	## remove upper triangle of correlation matrix
	if(removeTriangle[1]=="upper"){
		Rnew <- as.matrix(Rnew)
		Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
		Rnew <- as.data.frame(Rnew)
	}

	## remove lower triangle of correlation matrix
	else if(removeTriangle[1]=="lower"){
		Rnew <- as.matrix(Rnew)
		Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
		Rnew <- as.data.frame(Rnew)
	}

	## remove last column and return the correlation matrix
	Rnew <- cbind(Rnew[1:length(Rnew)-1])
	if (result[1]=="none") return(Rnew)
	else{
		if(result[1]=="html") print(xtable(Rnew), type="html")
		else print(xtable(Rnew), type="latex")
	}
}



fisherz=function(x) atanh(x)
fisherzinv=function(x) (exp(2*x)-1)/(exp(2*x)+1)

diffw=function(w,epsilon=1) ((w/(5.67e-8 * epsilon))^(1/4))/(4*w)

infill=function(timeser){
	timeser=trimNA(timeser)
	badboys=which(!is.finite(timeser))
	seas=seasonal(timeser)
	newser=timeser
	newser[badboys]=seas[badboys]
	newser
}
# tseries=tidesavg
cycleresid=function(tseries){
	tser=as.ts(tseries)
	startmon=start(tser)[2]
	thecycles=cycle(tser);thecycles
	cycmeans=as.double(tapply(tser,INDEX = thecycles,FUN = mean,na.rm=T))
	if (startmon!=1) cycmeans=cycmeans[c(startmon:12,1:startmon-1)]
	tseries-rep(cycmeans,length.out=length(tseries))
}

cycletrend=function(tseries,thestart=NA,thefreq=NA){
	if (class(tseries) %in% c("zoo","ts")){
		tser=as.ts(tseries)
		thestart=start(tser)
		thefreq=frequency(tser)
	} else {

		if (is.na(thestart[1])) thestart=c(1979,1)
		if (is.na(thefreq)) thefreq=12
		tser=ts(tseries,start=thestart,frequency = thefreq)
	}
	testlm=lm(cycleresid(tser)~time(tser))
	return(as.double(testlm$coef[2]))
	invisible(summary(testlm))
}
# tseries=tempbox[,11]
# plot(cycleresid(tempbox[,11]))
# cycletrend(tempbox[,11])

#
fwhm=function (g,roundto=0){
	round((2 * g + 1) * 0.39212, roundto)
}

fwhm2g=function(f,roundto=0){
	if (f == 0){
		return(0)
	} else{
		return(round((f * 2.55 - 1) / 2,roundto))
	}
}
fwhm2g(fwhm(1024))
ctok=function(c) c+273.15
ftoc=function(f) (f-32)*5/9
ktoc=function(k) k-273.15
ftok=function(f) ctok(ftoc(f))






# propercase <- function(s, strict = FALSE) {
# 	cap <- function(s) paste(toupper(substring(s, 1, 1)),
# 													 {s <- substring(s, 2); if(strict) tolower(s) else s},
# 													 sep = "", collapse = " " )
# 	sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
# }

# alow=(low_CERES);xleft=280
# length(alow$x)
# length(alow$y)

trimlow=function(alow,xleft=NA,xright=NA){
	if (!is.na(xleft)){
		(themin=min(which(alow$x>=xleft)))
		alow$x=alow$x[themin:length(alow$x)]
		alow$y=alow$y[themin:length(alow$y)]
	}
	if (!is.na(xright)){
		(themax=max(which(alow$x<=xright)))
		alow$x=alow$x[1:themax]
		alow$y=alow$y[1:themax]
	}
	alow
}

# alow$x[max(which(alow$x<=xright))]
# length(alow$x)

# testmap=allt2[,,1];mainmap=gre[,,1]
# testmap=allt2[,,1];mainmap=gre[,,1]
# testmap=surflwmap ;mainmap=surfabsmap
squaretrends=function(depmap,indmap,newshift=9){
	getarraytrends(dep=makeshiftsquare(depmap,theshift=newshift),
								 ind=makeshiftsquare(indmap,theshift=newshift),
								 removemeans=F,divideby=1
	)
}


# (x= bigarray[40,120,])
# bigdeming=function(x,newshift){
# 	blocksize=newshift^2
# 	depvec=x[1:blocksize]
# 	indvec=x[(blocksize+1):(2*blocksize)]
# 	deperrvec=x[(2*blocksize+1):(3*blocksize)]
# 	inderrvec=x[(3*blocksize+1):(4*blocksize)]
# 	deming(depvec~indvec,xstd=inderrvec,ystd=deperrvec)$coef[2]
# }

# witherr=function(x){
# 	thelen=length(x)/3
# 	lm(x[1:thelen]~x[(thelen+1):(2*thelen)],
# 		 weights = x[(2*thelen+1):(3*thelen)])$coef[2]
# }
#
# squaretrendsnew=function(depmap,indmap,newshift=9){
# 	dep=makeshiftsquare(depmap,theshift=newshift)
#   ind=makeshiftsquare(indmap,theshift=newshift)
# 	allwts=makeshiftsquare(latmatrix,theshift=newshift)
# 	bigarray=abind(dep,ind,allwts)
# 	thetest=pbapply(bigarray,c(1,2),witherr)
# }
#
# drawworld(thetest)

# resetplot()
# drawworld(thetest,
# 					mincolor = -40,maxcolor = 30)
# hist(thetest,breaks=80)
# length(which(thetest< -100))
# length(which(thetest>100))




# thetest[40:44,90:94]

# deparray=makeshiftsquare(surf_cre_net_tot_map,theshift = 9) #[88:92,178:182,]
# indarray=makeshiftsquare(allt2_map,theshift = 9) #[88:92,178:182,]
# weightarray=makeshiftsquare(latmatrix,theshift = 9)[88:92,178:182,]
#
# bigarray=abind(deparray,indarray)

gettrenderr=function(longvec){
	summary(lm(firsthalf(longvec)~lasthalf(longvec)))$coeff[2,2]
}

# deparray[1,1,1]
# indarray[1,1,1]
# bigarray[1,1,]
#
# gettrenderr(bigarray[1,1,])

getarraytrenderrs=function(deparray,indarray){
	bigarray=abind(deparray,indarray)
	pbapply(bigarray,c(1,2),gettrenderr)
}
# sqrt(sum(allerrs^2))/sqrt(64800)



# drawworld(depmap)
# depmap=lwmap;indmap=tempmap
# deperrmap=lwerr;inderrmap=temperr
makeerrmap=function(anarray) getarraysds(anarray)/sqrt(dim(anarray)[3])
squaretrends2=function(depmap,indmap,
											 deperrmap,inderrmap,
											 newshift=9){
	dep=makeshiftsquare(depmap,theshift=newshift)
	ind=makeshiftsquare(indmap,theshift=newshift)
	deperr=makeshiftsquare(deperrmap,theshift=newshift)
	inderr=makeshiftsquare(inderrmap,theshift=newshift)
	x=181
	#product(dim(depmap)
	matrix(pbsapply(1:product(dim(depmap)),
					 function(x){
					 		therc=indextorc(x,dim(depmap)[1],
					 										dim(depmap[2]))

					 		as.double(deming(dep[therc[1],therc[2],]~
					 					 ind[therc[1],therc[2],],
					 					 xstd = inderr[therc[1],therc[2],],
					 					 ystd=deperr[therc[1],therc[2],])$coef[2])
					 }),dim(depmap)[1],dim(depmap)[2])

}
idx=3;rowcount=3;colcount=4
indextorc=function(idx,rowcount,colcount){
	(thecol=floor((idx-1)/rowcount)+1)
	c(idx-(thecol-1)*(rowcount),thecol)
}
matrix(1:12,3,4)
indextorc(4,3,4)

makeshiftsquare=function(somemap, theshift=9){
	halfshift=(theshift-1)/2

	bigmap=rbind(somemap[1:halfshift,c(181:360,1:180)],
							 somemap,
							 somemap[(180-halfshift+1):180,c(181:360,1:180)])

	bigmap=cbind(bigmap[,(360-halfshift+1):360],
							 bigmap,
							 bigmap[,1:halfshift])


	dim(bigmap)
	bigarray=array(NA, c(180,360,theshift^2))
	# therow=1;thecol=1
	# drawworld(bigmap[therow:(therow+179),thecol:(thecol+359)])
	# for (thelayer in 1:theshift^2){
	for(thecol in 1:theshift){
		for(therow in 1:theshift){
			bigarray[,,(thecol-1)*theshift+therow]=
				bigmap[therow:(therow+179),thecol:(thecol+359)]
		}
	}
	dim(bigarray)
	bigarray
}


sketch = function(testmap,
									mainmap,
									main="Test Sketch",
									themask=1,
									add = F,
									pch = 20,
									cex = .3,
									alpha = .1,
									col = "dodgerblue3",
									type = "p",
									log = "",
									doplot=T,
									doprint=F,
									dolow=T,
									dotrend=F,
									lowleft=NA,
									lowright=NA,
									lowcol="yellow",
									lowf=.2,
									lowadd=T,
									lowback=6,
									lowfront=2,
									lowtype="solid",
									...) {
	resetplot()
	if ((length(dim(testmap))>2) | (length(dim(mainmap))>2)){
		print("Can't sketch an array!")
		break
	}
	par(mgp=c(2,.8,0))
	testmap=testmap*themask
	mainmap=mainmap*themask
	weightmap=latmatrix*themask
	lowdf=data.frame(theind=as.vector(mainmap),
									 dep=as.vector(testmap),
									 thelat=as.vector(latmatrix)) %>%
		arrange(theind)
	lmmatrix=lmmat(testmap,mainmap)
	# lmmatrix=lmmat(lowdf$dep,lowdf$theind,weights = latmatrix)
	lowdf=na.omit(lowdf)
	if (doprint) print(summary(lmmatrix))



	if (doplot){
		if (!add){
			# resetplot()
			plot(testmap~mainmap,col=addalpha(col,alpha),main=main,
					 pch=pch,cex=cex,type=type,log=log,...)
		} else {
			lines(testmap~mainmap,col=addalpha(as.vector(col),alpha),
						pch=pch,cex=cex,type=type, ...)

		}
	}
	# lowdf=data.frame(theind=as.vector(mainmap),
	# 												 dep=as.vector(testmap),thelat=as.vector(latmatrix))
	# lowdf=na.omit(lowdf)
	# print(dim(lowdf))
	# lowdf=arrange(lowdf,theind)



	if (!is.na(lowleft)) {
		lowdf = lowdf %>%
			dplyr::filter(theind >=lowleft)
	}
	if (!is.na(lowright)) {
		lowdf = lowdf %>%
			dplyr::filter(theind <=lowright)
	}

	if ((dotrend) & (doplot)){
		blackabline(lmmatrix,x1 = min(lowdf$theind),
								x2=max(lowdf$theind),col="red",frontline=4)
	}

	thelow=lowess(lowdf$theind,lowdf$dep,f=lowf)

	if ((dolow) & (doplot)){

		blackline(thelow, add=lowadd,
							col=lowcol,frontline=lowfront,backline=lowback,
							lty = lowtype,...)
	}
	invisible(thelow)
}



ezvals=function (f, leftend, rightend, n = 100)
{
	fun <- match.fun(f)
	f <- function(x) fun(x)
	stopifnot(is.numeric(leftend), is.numeric(rightend), length(n) == 1,
						length(rightend) == 1, leftend < rightend)
	x <- seq(leftend, rightend, length.out = n)
	y <- f(x)
	data.frame(x,y)
}
#
# ezvals(antisigmoid,0,.99999)
#
# antisigmoid(.7,a=2)
# a=surfmean;b=solarmean;c=els

lmts=function(tser) {
	if(length(grep("POSIX",class(time(tser))))>0) {
		tser=tser*secsperday
	}
	lm(tser~time(tser))}
lmtsrob=function(tser) {lmrob(tser~time(tser))}

lmtsum=function(tser) {
	thelm=lm(tser~time(tser))
	print(summary(thelm))
	invisible(thelm)
}
trendchange=function(x){
	changelen=length(x)/frequency(x)-1
	lmts(x)$coefficients[2]*changelen
}

lm.ts=function(tser) {lm(tser~time(tser))}


sumlm=function(response,forcing=NA,forcing2=NA,doprint=T) {
	a=response;b=forcing;c=forcing2
	# if (!is.null(dim(a))) {
	#   thedim=dim(a);thedim
	#   if (thedim[2]==3){
	#     c=a[,3]
	#   }
	#   b=a[,2]
	#   a=a[,1]
	# }
	if (length(b)==1){
		b=seq_along(a)
		if (class(a) %in% c("ts","zoo")){
			b=as.double(time(a))
		}
	}
	if (length(c)==1) {
		thelm=lm(as.vector(a)~as.vector(b))
		if (doprint) print(summary(thelm))
	} else {
		thelm=lm(as.vector(a)~as.vector(b)+as.vector(c))
		if (doprint) print(summary(thelm))
	}
	invisible(summary(thelm))
}

wilsonscore=function(successes,trials,z=1){
	prob=successes/trials
	errorminus=1/(1+z^2/trials)*(prob + z^2/(2*trials)-z*sqrt(prob*(1-prob)/trials +z^2/(4*trials^2)))*trials
	errorplus=1/(1+z^2/trials)*(prob + z^2/(2*trials)+z*sqrt(prob*(1-prob)/trials +z^2/(4*trials^2)))*trials
	lo_err=successes-errorminus;hi_err=errorplus-successes
	data.frame(errorminus,successes,errorplus,lo_err,hi_err)
}

TAOsunzenith=function(mainfile){
	thejd=JD(x = time(mainfile));head(thejd)
	thelat=attr(mainfile,"lat")
	thelong=attr(mainfile,"long")
	thelong=-thelong
	if (thelong< -180) thelong=thelong+360
	timezone=thelong/15  #
	#    timezone=floor((abs(thelong)+7.5)/15)*sign(thelong)

	sunpos(sunvector(thejd,thelat,thelong,timezone))[,"zenith"]
}

# mainfile=ts(NA,start=c(2010,1),end=c(2011,12),freq=12)
#
#
# timeposix=as.POSIXct((1:(365*2*24*60)-1)*60,origin="2010-01-01 00:00:00",tz="GMT");tail(timeposix)
# mainfile=timeposix
# thelat=34.2547;thelong=-89.8729
# jd=JD(seq(ISOdate(2012,3,20,0,0),ISOdate(2012,3,20,23,59),by="min"));jd
# options(digits = 11)
# mainfile=zoo(NA,seq(ISOdate(2012,3,20,0,0),ISOdate(2012,3,20,23,59),by="min"))
sunzenith=function(mainfile,thelat=34.25,thelong=-89.87){
	thejd=JD(x = time(mainfile));head(thejd);head(jd)
	# thelat=attr(mainfile,"lat")
	# thelong=attr(mainfile,"long")
	# thelong=-thelong
	if (thelong< -180) thelong=thelong+360
	timezone=thelong/15;timezone  #
	#    timezone=floor((abs(thelong)+7.5)/15)*sign(thelong)

	sunpos(sunvector(thejd,thelat,thelong,timezone))[,"zenith"]
}

# plot(sunvector(thejd,thelat,thelong,timezone))

sunstrength=function(mainfile,thelat=34.25,thelong=-89.87){
	# print(attr(mainfile,"name"))
	thejd=JD(x = time(mainfile));head(thejd)
	zangles=sunzenith(mainfile,thelat,thelong)
	zangles[which(zangles > 90)]=90
	strzoo=zoo(cos(zangles*pi/180)*1365/(sunr(thejd)^2),time(mainfile))
	attr(strzoo,"lat")=thelat
	attr(strzoo,"long")=thelong
	strzoo
}
# plot(strzoo)
# plot(sunstrength(mainfile))

anydigit=function(x,n){
	if (is.numeric(x)) {

		sprintf(paste0("%0",n,"d"), x)

	} else {
		if (as.double(x) == 0) {
			"00"
		} else{
			format(x, digits = n)
		}
	}

}

twodigit = function(x) {
	if (is.numeric(x)) {

		sprintf("%02d", x)

	} else {
		if (as.double(x) == 0) {
			"00"
		} else{
			format(x, digits = 2)
		}
	}
}
threedigit = function(x) {
	if (is.numeric(x)) {

		sprintf("%03d", x)

	} else {
		if (as.double(x) == 0) {
			"000"
		} else{
			format(x, digits = 3)
		}
	}
}


# new residual function

# timeseries=part2
resid.ts=function(timeseries){
	newcycle=tapply(timeseries,cycle(timeseries),mean,na.rm=T)
	timeseries-newcycle[cycle(timeseries)]
}

# resid.ts(part2)

# from Amazon Flow --------------------------------------------------------


eemdf=function(flowcheck,num_imfs=floor(log(length(flowcheck),2))) {
	require(hht)
	imfs=eemd(flowcheck,num_imfs = num_imfs, num_siftings = 50,
						ensemble_size = 2000,
						threads = 1,noise_strength = 0.6*sd(flowcheck))
	lastcheck=flowcheck-mean(flowcheck)
	thefinal=ts.intersect(lastcheck,imfs)
	thefinalzoo=zoo(thefinal,time(thefinal))
	colnames(thefinalzoo)=c("Data",paste0("C",1:(dim(thefinal)[2]-2)),"Trend")

	plot.zoo(thefinalzoo,nc=1,type="l",pch=20,cex=.3,
					 # ylim=c(min(thefinal[,1]-50),max(thefinal[,1])),
					 ylim=c(min(thefinal[,1]),max(thefinal[,1])),
					 #ylim=c(-.4,.4),
					 # ylim=c(-15,15),
					 lwd=2,
					 yax.flip = F,main="",col=addalpha("blue3",.8))
	invisible(imfs)
}

scale.ts=function(ts,center=TRUE,scale=TRUE,na.rm=T) {
	tsdouble=as.double(ts)
	newval=(tsdouble-mean(tsdouble,na.rm=na.rm))/
		sd(tsdouble,na.rm=na.rm) *
		ifelse(is.logical(scale),1,scale)+
		ifelse(is.logical(center),0,center)

	ts(newval,start=start(ts),
		 frequency = frequency(ts))
}


# from lod mann -----------------------------------------------------------

sdout=function(ts1,ts2,useacf=F){
	thecor=cor(ts1,ts2,use="pairwise.complete.obs")
	zvalue=ztransform(thecor)
	effectiven=commonheff(ts1,ts2,useacf=useacf)
	zvalue*sqrt(effectiven-3)
}

# thetime=c(1870,11)
twodatetodecimal=function(thetime,time2=NA,thefreq=12,roundto=2){
	if (!is.na(time2)) {thetime=c(thetime,time2)}
	round(thetime[1]+(thetime[2]-1)/thefreq,roundto)
}
twodatetodecimalv=Vectorize(twodatetodecimal,"thetime","time2")

# decimaltotwodate(1988+1/3)
# timedec=1988+2+1/3

decimaltotwodate=function(timedec,thefreq=12) {
	testr=round((timedec-floor(timedec))*thefreq,0)+1
	c(floor(timedec),testr)
}

timedec=1866.042
decimaltotwodate24=function(timedec,thefreq=12) {
	thefrac=round(timedec-floor(timedec),3)
	themon=which(round(seq(1,24,2)/24,3)==thefrac)

	c(floor(timedec),themon)
}
dectotwo24=Vectorize(decimaltotwodate24,"timedec")

# tser=window(avgtemp,end=1980)

timeshift=function(tser,theshift=1){
	fq=frequency(tser)
	thestart=time(tser)[1]
	if (length(time(tser)[1])==2){
		thestart=twodatetodecimal(time(tser[1]))
	}
	ts(as.double(tser),
		 start=thestart +theshift/fq,
		 frequency = fq)
}

#
# ts1=tser;ts2=ts1
plotlag=function(somebox) {
	resetplot()
	plot(somebox[,1]~somebox[,2])
}
mylag=function(ts1,ts2,thelag=1,doplot=F){
	thebox=ts.intersect(ts1,timeshift(ts2,thelag))
	if (doplot) plotlag(thebox)
	invisible(thebox)
}

mylagdec=function(ts1,ts2,thelag=1,doplot=F){
	cbind(ts1[(thelag+1):length(ts1)],
				ts2[1:(length(ts2)-thelag)])
}



hurstp2=function(ts1,ts2=NA,H=NA){
	pvalueh(ts1,ts2,H)
}

# ts1=testdata;ts2=NA;H=.81
# commonheff(testdata,H=.81)
commonheff=function(ts1,ts2=NA,useacf=F,H=NA){
	if (length(ts2)==1){
		heff(ts1,H=H)
	} else {
		ifelse(useacf,
					 geomean(c(efoldeff(ts1),efoldeff(ts2))),
					 geomean(c(heff(ts1),heff(ts2))))
	}
}
ztransform=function(r) 1/2 * log((1+r)/(1-r))

geomean=function(x) prod(x,na.rm=T)^(1/length(which(is.finite(x))))

# x=surflm
# h=lmts(window(bwinannnh,start=thestart,end=theend))
# x1=NA;x2=NA;y1=NA;y2=NA

# devtools::install_github("jennybc/jadd",force = T)
# h=min(bgauss)
# h = dlm


blackdeming=function(h,col="yellow",bg="black",
										 lty="solid",
										 backline=6,frontline=2,...){
	(mod1=as.double(unlist(h$model[1])))
	(mod2=as.double(unlist(h$model[2])))
	(leftx=min(mod2,na.rm=T))
	(rightx=max(mod2,na.rm=T))
	(leftid=as.double(which(mod2==leftx)))
	(rightid=as.double(which(mod2==rightx)))
	(lefty=mod1[leftid]-as.double(unlist(h$residuals[leftid])))
	(righty=mod1[rightid]-as.double(unlist(h$residuals[rightid])))
		blacklines(c(lefty,righty)~c(leftx,rightx),
							backline=backline,frontline=frontline,
							col=col,bg=bg,lty=lty,...)
}
# h=lmts(tempmonfull)
# plot(tempmonfull)
# blackabline(h)
blackabline=function(h=NA,v=NA, col="yellow",bg="black",
										 x1=NA,x2=NA,y1=NA,y2=NA,lty="solid",
										 backline=6,frontline=2,...){
	thewindow=par("usr")
	if (is.na(x1[1])) x1=thewindow[1]
	if (is.na(x2[1])) x2=thewindow[2]
	if (is.na(y1[1])) y1=thewindow[3]
	if (is.na(y2[1])) y2=thewindow[4]
	if (class(h)=="lm"){
		mod1=as.double(unlist(h$model[1]))
		mod2=as.double(unlist(h$model[2]))
		(leftx=min(mod2,na.rm=T))
		(rightx=max(mod2,na.rm=T))
		(lefty=h$fitted.values[1])
		(righty=last(h$fitted.values))
		blacklines(c(lefty,righty)~c(leftx,rightx),
							 backline=backline,frontline=frontline,
							 col=col,bg=bg,lty=lty,...)

		# 	ablineclip(h,lwd=backline,col=bg,
		# 						 x1=min(h$model[2],na.rm=T),
		# 						 x2=max(h$model[2],na.rm=T),
		# 						 y1=min(h$model[1],na.rm=T),
		# 						 y2=max(h$model[1],na.rm=T),
		# 						 lty=lty)
		# 	ablineclip(h,lwd=frontline,col=col,
		# 						 x1=min(h$model[2],na.rm=T),
		# 						 x2=max(h$model[2],na.rm=T),
		# 						 y1=min(h$model[1],na.rm=T),
		# 						 y2=max(h$model[1],na.rm=T),
		# 						 lty=lty)
	} else {

		if (length(h)>0){
			ablineclip(h=h,lwd=backline,col=bg,
								 x1=x1,x2=x2)
			ablineclip(h=h,lwd=frontline,col=col,
								 x1=x1,x2=x2,lty=lty)
		}
		if (length(v)>0){
			ablineclip(v=v,lwd=backline,col=bg,
								 x1=x1,x2=x2,y1=y1,y2=y2)
			ablineclip(v=v,lwd=frontline,col=col,
								 x1=x1,x2=x2,y1=y1,y2=y2,lty=lty)
		}
	}
}
# thelm=lmts(pdsi)
# blackline(pdsi,add = F,
# 					ylim=c(8.5,-8.5))
# blackline(palmgauss,col="red")
# abline(h=0, lty="dashed")
# blackablm(thelm)
blackablm=function(thelm=NA,col="yellow",bg="black",
										 lty="solid",
										 backline=6,frontline=2){

	abline(thelm,lwd=backline,col=bg)
	abline(thelm,lwd=frontline,col=col,lty=lty)
}
# blackabline(v=1960,y1=5)

blacklegend = function(wherein = "topleft",
											 thex=NA,they=NA,
											 thefont = 2,
											 legend = c("first", "second"),
											 col = c("black", "red"),
											 mainbg = "white",
											 backline = 6,
											 backcol = "black",
											 frontline = 4,
											 cex = .85,
											 lty = "solid",
											 ...
){
	par(font=thefont)
	if (!is.na(wherein)){
		legend(wherein, legend = legend,lty="solid",bg=mainbg,
					 col = backcol,lwd=backline, cex=cex, ...)
		legend(wherein, legend = legend,bg = NA, lty=lty,
					 col = col,lwd=frontline, cex=cex, ...)
	} else {
		legend(x=thex,y=they, legend = legend,lty="solid",bg=mainbg,
					 col = backcol,lwd=backline, cex=cex, ...)
		legend(x=thex,y=they, legend = legend,bg = NA, lty=lty,
					 col = col,lwd=frontline, cex=cex, ...)
	}
	par(font=1)
}

blackplot=function(tser,backline = 6,
									 frontline = 3,
									 pch = 20,
									 bg = "black",
									 col = "yellow",
									 lty = "solid",
									 type = "l",
									 add = F,...) {
	blacklines(tser,backline = backline,
						 frontline = frontline,
						 pch = pch,
						 bg = bg,
						 col = col,
						 lty = lty,
						 type = type,
						 add = add,...)
}
# x=templatemy ~ colatemy
# x=thelm3$fitted.values~as.double(time(led))
# add=F
# resetplot()
# x=tpwmon$LED
# blackline(x,add=T)
# plot(led)
# x=onixc[,6]
# class(x)
# add=F
# blackline----------------
blacklines = function(x,
											backline = 6,
											frontline = 3,
											pch = 20,
											bg = "black",
											col = "yellow",
											lty = "solid",
											type = "l",
											add = T,
											doshadow=F,
											...
){
	if (class(x)=="zoo") x=as.ts(x)
	if (add) {
		if (class(x)=="lm"){
			ablineclip(x,lwd=backline,col=bg,lty="solid",...)
		} else {
			if (doshadow) shadowline(x,thelwd=backline)
			lines(x,lwd=backline,col=bg,type=type,pch=pch,
						lty="solid")
			# par(new=F)
		}
	} else {
		plot.default(x,lwd=backline,col=bg,type=type,pch=pch,lty="solid",...)
	}
	if (class(x)=="lm"){
		ablineclip(x, lwd=frontline,col=col,...)
	} else {
		lines(x,lwd=frontline,col=col,type=type,pch=pch,lty=lty)
		# par(new=F)
	}
}

# warnings()
# x=theformula;add=F
# blackplot(x,type="l")
# x=invlm
# end blackline ----------
blackline= function(x,y=NA,
										 backline = 6,
										 frontline = 3,
										 pch = 20,
										 bg = "black",
										 col = "yellow",
										 lty = "solid",
										 type = "l",
										 add = T,
										 ...){
	if (class(x)=="zoo") x=as.ts(x)
	if (add) {
		if ((class(x)=="lm")|(class(x)=="deming")) {
			lines(x$fitted.values~unlist(x$model[2]),
						type=type,
						lwd=backline,
								 ...)
			lines(x$fitted.values~unlist(x$model[2]),
						type=type,col=col,
						lwd=frontline,lty=lty,
						...)

		} else {
			lines(x,lwd=backline,col=bg,type=type)
			# par(new=F)
		}
	} else {
		plot.default(x,lwd=backline,col=bg,type=type,pch=pch,...)
	}
	if (class(x)=="lm"){
		ablineclip(x, lwd=frontline,col=col,
							 x1=min(x$model[2]),
							 x2=max(x$model[2]),
							 ...)
	} else {
		lines(x,lwd=frontline,col=col,type=type,pch=pch,lty=lty)
		# par(new=F)
	}
}
# plot.default(templatemy ~ colatemy,lwd=6,type="l",col=1)

# x=as.double(GDP)~as.double(energytot)
blackpoint=function(x,backline=1.6,frontline=.75,
										bg="black",col="red",
										add=F,pch=20, ...){

	if (add) {
		points(x,cex=backline,col=bg,pch=pch)
	} else {
		plot(x,cex=backline,col=bg,pch=pch,...)
	}

	points(x,cex=frontline,col=col,pch=pch)
	# points(x,cex=backline*.67 ,col=bg,pch=1)

}
# blackpoint(x,frontcol = "red")
# x0=2023;y0=min(theline);y1=max(theline)
warnings()
blackarrow=function(x0, y0, x1=x0, y1=y0,
										length = 0.125, angle = 30,
										code = 2,
										lty = par("lty"),
										backline=6,frontline=2,col="yellow",
										...){
	arrows(x0, y0, x1, y1,
				 length = length, angle = angle,
				 code = code,
				 lty = lty,lwd=backline,
				 col="black")
	arrows(x0, y0, x1, y1,
				 length = length, angle = angle,
				 code = code,
				 lty = lty,lwd=frontline,
				 col=col)
}


boxcar=function(ts,filtlen=5) {
	trimNA(stats::filter(ts,filter = rep(1/filtlen,filtlen)))
}



# tser=precipmonfull
# hurstexp(tser)
# heff(tser)
# hurstfactor(tser)
# hurstp(tser)
#
# thearray=tlt[,,1];newy=100
# thearray=mld
# dim(thearray)
x=NULL
# thearray=vonp[,,i];newy=153
resampleit=function(thearray,newx=360,newy=180){
	require(raster)
	thedims=dim(thearray);thedims
	thedepth=ifelse(length(thedims)==2,1,thedims[3]);thedepth
	biglight=array(NA,c(180,360,thedepth))
	i=1
	startrow=(180-newy)/2+1;startrow
	endrow=startrow-1+newy;endrow
	for (i in 1:thedepth){
		if (i %% 100==0) print(i)
		x <- raster(nrow=thedims[1],ncol=thedims[2],crs=NA)
		y <- raster(nrow=newy,ncol=newx,crs=NA)
		if (thedepth==1) values(x) = thearray else values(x) =thearray[,,i]
		finaly=raster::resample(x,y)
		biglight[startrow:endrow,,i]=as.matrix(finaly)
	}
	# detach(raster,unload = T)

	return(biglight)
}


enth_vapor=40700;R=8.3145
claus=function(tt,enth_vapor=40700,R=8.3145) exp(-(enth_vapor/R)*(1/tt - 1/373))
# plot(claus(273:373)~I(0:100))

deltavap=function(tt) (2.45*10^9*exp(-4900./tt)) / tt^2 / .01
deltavap(273+30)
claus(273+31) - claus(273+30)

ctof=function(cc) cc*9/5+32

hefftoh=function(heff,len) 1-(log(heff)/log(len)/2)
# tser=x
heff=function(tser,H=NA) {
	theexp=ifelse(is.na(H),hurstexp(tser),H);theexp
	if (is.na(theexp)) theexp=as.double(pracma::hurstexp(dtrendlin(tser),display=F)[1])
	min(c(length(tser),length(tser)^(2*(1-theexp))))
}
# hurstexp(c(1:4))
# tser=cotsnoann
# tser=tsyearlyspots
# print(hurst(tser))
hurstexp=function(tser,detrend=T,datapoints=20) {
	ans=try(hurst(tser,doplot=F,
								datapoints = datapoints,
								details=T,detrend =detrend)$exp,silent = T)
	if((inherits(ans, "try-error")) | (is.na(ans))){
		as.double(pracma::hurstexp(dtrendlin(tser),display=F)[1])
	} else {
		ifelse(ans<0,NA,ifelse(ans>1,NA,ans))
	}
}

hurstp=function(tser,partb=NA,H=NA){
	pvalueh(tser,partb=partb, H=H)
}

hurstpindiv=function(tser,tvalue){
	dt(abs(tvalue/hurstfactor(tser)),(heff(tser)-1))
}

# sd(mylmsum$residuals)/sqrt(length(tser))

hurstfactor=function(tser) length(tser)^.5/heff(dtrendlin(tser))^.5

hurstsem=function(tser) sem(tser)*hurstfactor(tser)

fillNA=function(tseries) {
	library(mice)
	if (length(which(is.na(tseries)))==0) tseries else {
		theclass=class(tseries)
		if (theclass %in% c("ts","zoo")){
			timematrix=data.frame(cbind(as.double(time(tseries)),as.double(tseries)))
			ans=complete(mice(timematrix,print=F))
			if(theclass=="ts"){
				ts(ans[,2],ans[,1])
			} else {
				zoo(ans[,2],ans[,1])
			}
		} else {
			timematrix=data.frame(cbind(seq_along(tseries),as.double(tseries)))
			ans=complete(mice(timematrix,print=F))
			ans[,2]
		}
	}
}

numberpad=function(n,thelen) {
	thecode=paste0("%0",thelen,"d")
	sprintf(thecode,n)
}

# yy=armraw$Temp;xx=armraw$yearfrac
midgauss=function(xx,yy){
	require(dplyr)
	dataf=data.frame(xx=xx,yy=yy)
	dataf=dplyr::arrange(dataf,xx)
	thecurve=tapply(dataf$yy,dataf$xx,FUN=mean,na.rm=T)
	lines(levels(as.factor(dataf$xx)),as.double(thecurve),col="red3")
}
# length(levels(as.factor(dataf$xx)))
#
# head()


par(mgp=c(2,.8,0))


# source("HadCRUT Setup.R")
# timeseries=hadamon;zvals=stats::fft(timeseries);doscale=F;detrend=T;ampvar=0
#
makepseudo=function(x,timeseries) pseudo(timeseries)
# timeseries=ERSST
# zvals=stats::fft(timeseries)
# randwidth=2*pi;
# doscale=T;detrend=T;ampvar=0;trendvar=0
# ,zvals=stats::fft(timeseries)
# pseudo(ERSST)
# timeseries=bosts
# start(timeseries)
# pseudo(timeseries)
pseudo=function(timeseries,
								randwidth=2*pi,
								doscale=T,detrend=T,ampvar=0,trendvar=0){
	zvals=stats::fft(timeseries)
	if (class(timeseries) != "ts") timeseries=as.ts(timeseries)
	if (detrend) {
		trendresults=lmts(timeseries)$coefficients[2]
		timeseries=dtrendlin(timeseries)
	}
	N=length(timeseries)

	phasenew=runif(N,-randwidth/2,randwidth/2)
	amplitude=Mod(zvals)
	if (ampvar>0){
		amplitude=abs(amplitude+rnorm(length(amplitude),sd=amplitude*ampvar))
	}

	newzvals=amplitude*exp(complex(N,imaginary=phasenew))
	randone=Re(stats::fft(newzvals, inverse = TRUE))/length(newzvals)
	if (doscale){
		randone=as.double(scale(randone)*sd(timeseries)+mean(timeseries))
	}

	if (detrend) {
		randone=randone+trendresults*runif(1,min=1-trendvar,max=1+trendvar)
	}
	ts(round(randone,3),start=start(timeseries),frequency=frequency(timeseries))
}

pseudobox=function(timeseries,
									 doscale=T,detrend=T,ampvar=0,trendvar=0){
	zvals=stats::fft(timeseries)
	timeseries=as.ts(timeseries)
	if (detrend) {
		trendresults=dtrendlin(timeseries)
		timeseries=trendresults$out
	}
	N=length(timeseries)
	phasenew=runif(N,0,2*pi)
	amplitude=Mod(zvals)
	if (ampvar>0){
		amplitude=abs(amplitude+rnorm(length(amplitude),sd=amplitude*ampvar))
	}

	newzvals=amplitude*exp(complex(N,imaginary=phasenew))
	randone=Re(stats::fft(newzvals, inverse = TRUE))/length(newzvals)
	if (doscale){
		randone=as.double(scale(randone)*sd(timeseries)+mean(timeseries))
	}

	if (detrend) {
		randone=randone+trendresults$trend*runif(1,min=1-trendvar,max=1+trendvar)
	}
	ts(round(randone,3),start=start(timeseries),freq=frequency(timeseries))
}


# x=standardspots;tau=4;lambda=12;gausslen=5
taulag=function(x,tau=1,lambda=1,gausslen=5) {

	errorfun=function(mypar){
		start1=mypar[1];start2=mypar[2]
		sqrt(sum((maketrial(x,tau,lambda,start1,start2)-x)^2,na.rm=T))
	}

	maketrial=function(x,tau,lambda,start1,start2) {
		answer=x
		#     answer[1]=mean(x[1:2])*lambda
		#     answer[2]=answer[1] #+ lambda*(x[2]-x[1])*(1-alpha)
		answer[1]=start1  #mean(x[1:2])*lambda
		answer[2]=start2 #+ lambda*(x[2]-x[1])*(1-alpha)
		for (i in 3:length(x)){
			answer[i]=answer[i-1]+ lambda*(x[i]-x[i-1])*(1-alpha) +
				(answer[i-1]-answer[i-2])*alpha
		}
		answer
	}
	# end of internal taulag function -----------------------------------------

	if (tau==0) x*lambda else {
		thestart=start(x)
		thefrequency=frequency(x)
		thetime=time(x)
		x=as.vector(x)
		alpha=exp(-1/tau);alpha

		start1=mygauss(x,gausslen)[1]
		start2=start1
		mypar=c(start1,start2)
		newpar=optim(mypar,fn=errorfun)$par

		ts(maketrial(x,tau,lambda,newpar[1],newpar[2]),start=thestart,
			 frequency = thefrequency)
	}
}


standardize=function(x,newsd=1,newmean=0) {
	#   x=x[which(is.finite(x))]
	if ("ts" %in% class(x)){
		ts((x-mean(x,na.rm=T))/sd(x,na.rm=T)*newsd+newmean,start=start(x),
			 frequency=frequency(x))
	} else {
		if ("zoo" %in% class(x)) {
			zoo((x-mean(x,na.rm=T))/sd(x,na.rm=T)*newsd+newmean,time(x))
		} else {
			(x-mean(x,na.rm=T))/sd(x,na.rm=T)*newsd+newmean
		}
	}
}

# plot(scale(nilometer))
#
hursterror=function(timeseries,H=.5) {
	N=length(timeseries)
	sd(timeseries)*N^(H-1)
}
# timeseries setup----------
# timevector=filtbox[,"sst"] # data to analyze
# timevector=tideblock[,2];plot(timevector)
doplot=T # draw analysis plot?
dataplot=F # draw plot of data
expandtime=1
boxwidth=.3 # decimal 0-1, amount of data to analyze
details=F #calculate details?
downset=.02
midset=.7 # annotation adjustment
main=NA # title text
add=F # add to existing (T) or new plot (F)
pch=20 # point character
col="black" # line and text color
drawtrend=F # draw  annotate with exponent?
drawline=F # draw trendline and annotate with exponent?
detrend=T # detrend data?
graphdepth=1 # adjust depth/width ratio
freq=1 # not used
type="o" # plot type
cex.main=.95 # title text size
cex=1 # data point size
dolegend=F # add legend to plot?
leg.x="bottomleft"
leg.y=NULL # legend location
leg.text=c("Raw Data","Detrended Data") # legend text,
leg.col=c("blue3","red3") #legend colors
cex.leg=.9 #legend expansion
datapoints=20

# hurst(nilometer,datapoints=10,boxwidth=.4)
# hurst function---------------------
hurst=function(timevector, # data to analyze
							 doplot=T, # draw analysis plot?
							 dataplot=F, # draw plot of data
							 expandtime=1,
							 boxwidth=.1, # decimal 0-1, amount of data to analyze
							 details=F, #calculate details?
							 downset=.02,midset=.7, # annotation adjustment
							 main=NA, # title text
							 add=F, # add to existing (T) or new plot (F)
							 pch=20, # point character
							 col="black", # line and text color
							 drawtrend=T, # draw  annotate with exponent?
							 drawline=T, # draw trendline and annotate with exponent?
							 detrend=T, # detrend data?
							 graphdepth=1, # adjust depth/width ratio
							 freq=1, # not used
							 type="o", # plot type
							 cex.main=.95, # title text size
							 cex=1, # data point size
							 dolegend=F, # add legend to plot?
							 leg.x="bottomleft",leg.y=NULL, # legend location
							 leg.text=c("Raw Data","Detrended Data"),# legend text,
							 leg.col=c("blue3","red3"),#legend colors
							 cex.leg=.9, #legend expansion
							 datapoints=20){ # number of points calculated in graph
	require(plotrix)
	par(mgp=c(2,.8,0))


	# the onehurst function calculates the standard deviation of the means of
	#  all possible contiguous subsets of length "interval" of
	#  the timevariable "tv"
	onehurst=function(interval,tv) {
		thefilt=stats::filter(as.double(tv),filter=rep(1,interval)/interval)
		testline=seq(interval/2+1,length(thefilt),interval)
		#     if(length(which(is.finite(thefilt[testline])))<10) {
		#       NA
		#     } else{
		sd(thefilt,na.rm=T)
		# }
		# print(interval)
	}

	# the addalpha function allows for transparent colors, with "alpha"
	# controlling the transparency
	addalpha=function(colors, alpha=1.0) {
		r <- col2rgb(colors, alpha=T)
		# Apply alpha
		r[4,] <- alpha*255
		r <- r/255.0
		return(rgb(r[1,], r[2,], r[3,], r[4,]))
	}

	# the standardize function transforms a dataset to have a mean of zero and a standard deviation of one.
	standardize=function(x) {
		if (class(x)=="ts"){
			ts((x-mean(x,na.rm=T))/sd(x,na.rm=T),start=start(x),
				 frequency=frequency(x))
		} else {
			(x-mean(x,na.rm=T))/sd(x,na.rm=T)
		}
	}

	# end functions -----------------------------------------------------------

	if (detrend) timevector=dtrendlin(timevector)
	timevector=standardize(timevector)
	boxwidth=floor(length(which(is.finite(timevector)))*boxwidth);boxwidth
	# if (boxwidth<3) boxwidth=3
	pointseq=round(c(1,10^(seq(log(2,10),log(boxwidth/freq,10),length.out = datapoints-1))),0);pointseq # calculates equal spaced points
	while (length(which(duplicated(pointseq)))>0){
		datapoints=datapoints-1
		pointseq=round(c(1,10^(seq(.30103,log(boxwidth/freq,10),length.out = datapoints-1))),0);pointseq # calculates equal spaced points
	};pointseq


	thedata=(sapply(pointseq,onehurst, tv=timevector)) # this is the heart
	# of the program. It applies the values in "pointseq" to the function
	# "onehurst" to give the values of the dots shown in the graph.
	N=length(thedata)
	runstart=2

	if (doplot){
		plotwidth=log(boxwidth/freq,10);plotwidth
		if (add ==F){
			plot(log(thedata,10)~log(c(1,pointseq[2:datapoints]*expandtime),10),
					 pch=pch,
					 xlab="Log(subsample size, base 10)",
					 ylab="Log(Standard Deviation, base 10)",
					 ylim=c(-plotwidth/2*graphdepth,0),
					 type=type,col=col,cex=cex)
			abline(h=0,lty="dotted")
			if (dolegend) {
				legend("bottomleft", legend=leg.text, box.lwd = 1.5,
							 fill = leg.col,
							 cex=cex.leg)
			}
		} else {
			lines(log(thedata,10)~log(c(1,pointseq[2:datapoints]*expandtime),10),
						pch=pch,
						type=type,col=col,cex=cex)
		}
		if (add==F) {lines(c(0,-5)~c(0,10))
			text(plotwidth*.15,-plotwidth*.15,paste0("Random",
																							 "\n(Hurst Exp. = 0.5)"),
					 cex=.9,adj=.5)
		}
		mylm=lm(log(thedata[runstart:N],10)~0+I(log(pointseq[runstart:N]*expandtime,10)))
		if (drawtrend){
			text(plotwidth*midset,-plotwidth*downset, adj=.5,col=col,cex=.9,font=2,
					 labels = paste0("Hurst Exponent = ",round(mylm$coef[1]+1,2)))
			if (drawline) ablineclip(mylm,col="black",lwd=2,lty="dotted",x1=0,x2=max(log(pointseq,10)))
		}
		if (add==F) title(main=paste0("Hurst Exponent Analysis\n",main,", N = ",length(which(is.finite(timevector)))),cex.main=cex.main,line=.8)
		effn=floor(heff(timevector,as.double(round(mylm$coef[1]+1,2))))
		theusr=par("usr")
		(heffx=theusr[1]+diff(theusr[1:2])/4)
		(heffy=theusr[3]+diff(theusr[3:4])/4)

		text(heffx,heffy,labels =
				 	paste0("Effective Number of",
				 				 "\nData Points",
				 				 "\n(Effective N)",
				 				 "\n= ",effn))
	}

	# details ---------------


	if (details){
		hexp=1+as.double(lm(log(thedata[runstart:N],10)~0+I(log(pointseq[runstart:N]*expandtime,10)))$coef[1]);hexp
		neff=max(c(3,length(timevector)^(2-2*hexp)))
		neffhi=max(c(3,length(timevector)^(2-2*((hexp-.5)*.90+.5))))
		nefflo=max(c(3,length(timevector)^(2-2*(hexp+(1-hexp)*.10))))
		thesd=((neff+4)^-.521)*3.93
		thesdhi=((neffhi+4)^-.521)*3.93
		thesdlo=((nefflo+4)^-.521)*3.93

		thelm=lm(standardize(as.double(timevector))~
						 	seq(0,1,length.out = length(timevector)))
		thetrend=as.double(thelm$coef[2])
		pvalue=round(2*(1-pnorm(thetrend/thesd)),3)
		if (dataplot) {
			plot(standardize(as.double(timevector))~
					 	seq(0,1,length.out = length(timevector)),type="l",
					 ylab="Standard Deviations",xlab="Time")
			thelm=lm(standardize(as.double(timevector))~
							 	seq(0,1,length.out = length(timevector)))
			if (is.na(main)){
				main="Hurst Exponent Trend Analysis"
			} else {
				main=paste0("Hurst Exponent Trend Analysis\n",main)}
			title(main=main,cex.main=cex.main,line=.8)
			title(sub=paste0("Trend = ",round(thelm$coefficients[2],2),
											 " (dashed blue line); n_eff = ",round(nefflo,0)," to ",
											 round(neffhi,0)," independent points",
											 "\nHurst Exponent = ",round(hexp,2),
											 "; Trend p-value = ",pvalue),
						cex.sub=.9,line=3.8)
			polygon(x = c(0,1,1,0,0),
							y=c(-thesdhi*.98,thesdhi*.98,thesdlo*.98,-thesdlo*.98,-thesdlo*.98),
							col=addalpha("red",.5),border=NA)
			polygon(x = c(0,1,1,0,0),
							y=c(thesdhi*.98,-thesdhi*.98,-thesdlo*.98,thesdlo*.98,thesdlo*.98),
							col=addalpha("red",.5),border=NA)
			abline(thelm,col=addalpha("skyblue3",1),lwd=4,lty="dashed")
		}
		theanswer=data.frame(exp=hexp,
												 nefflo=max(c(3,round(nefflo,1))),
												 neff=max(c(3,round(neff,1))),
												 neffhi=max(c(3,round(neffhi,1))),
												 sdlo=round(thesdlo,2),
												 sd=round(thesd,2),
												 sdhi=round(thesdhi,2),
												 cilo=round(1.96*thesdlo,2),
												 ci=round(1.96*thesd,2),
												 cihi=round(1.96*thesdhi,2),
												 trend=round(thetrend,2),

												 pvaluelo=round(2*(1-pnorm(thetrend/thesdlo)),3),
												 pvalue=round(pvalue,3),
												 pvaluehi=round(2*(1-pnorm(thetrend/thesdhi)),3))
	} else {
		theanswer=thedata
	}



	invisible(theanswer)
}
# hurst(timevector,details = T)
# end hurst --------------------------------
hurstpnew=function(tser){
	avals = arima(tser, c(1, 0, 1))
	thetrends=pbsapply(1:5000, function(x) {
		# thetest=simFGN0(240,H=y)
		thets=ts(arima.sim(list(
			ar = avals$coef[1], ma = avals$coef[2]),
			n = 240),
			start=start(tser),frequency = frequency(tser))
		# sd(dtrendlin(thets))
		# lines(adjsd(tser,thets))
		lmts(thets)$coef[2]

	}
	)
	thep=pnorm(lmts(tser)$coeff[2],mean = mean(thetrends),sd=sd(thetrends))
	as.double(round(ifelse(thep>.5,1-thep,thep),3))*2
}

# Round to function -------------------------------------------------------


roundto = function(x,nearest=.5){ round(as.double(x)/nearest,0)*nearest}

round5=function(x) (floor(abs(x))+.5) * sign(x)

# Random sine distribution ------------------------------------------------



rsine=function(howmany=1,themean=0,halfwidth=1,noise=0){
	answer=sin(runif(howmany,0,2*pi))*halfwidth+themean
	if (noise!=0){
		therange=diff(range(answer))*noise
		answer=answer+rnorm(howmany,mean=answer,sd=therange)
	}
	answer
}






mround=function(x,nearest) round(x/nearest,0)*nearest

ctor=function(temp,epsilon=1) {
	theresult=5.67e-8*epsilon*(as.double(temp)+273.15)^4
	if ("ts" %in% class(temp)){
		theresult=ts(theresult,start=start(temp),frequency = frequency(temp))
	}
	if ("zoo" %in% class(temp)){
		theresult=zoo(theresult,time(temp))
	}
	theresult
}
ktor=function(temp,epsilon=1) {
	if ("ts" %in% class(temp)) {
		ts(5.67e-8*epsilon*as.double(temp)^4,start=start(temp),
			 frequency = frequency(temp))
	} else {
		5.67e-8*epsilon*as.double(temp)^4
	}
}
rtoc=function(rad,epsilon=1) {
	theresult=(as.double(rad)/epsilon/5.67e-8)^(1/4)-273.15
	if ("ts" %in% class(rad)){
		theresult=ts(theresult,start=start(rad),frequency = frequency(rad))
	}
	if ("zoo" %in% class(rad)){
		theresult=zoo(theresult,time(rad))
	}
	theresult
}

rtok=function(rad,epsilon=1) {
	if ("ts" %in% class(rad)) {
		ts((as.double(rad)/epsilon/5.67e-8)^(1/4),start=start(rad),
			 frequency = frequency(rad))
	} else {
		(as.double(rad)/epsilon/5.67e-8)^(1/4)
	}
}

rtoc(ctor(26),.999)

reltoabshum <- function (temp, RH,press=1013) {
	((((1.0016+0.00000315*press-0.074/press)*
		 	(6.112*exp((17.62*temp)/(243.12+temp))))*RH/100)/
	 	(461.5*(temp+273.15))*100)
} # kg/m3

abstorelhum <- function (temp, AH,press=1013) {
	(75.51*AH*(273.15 + temp))/
		(exp((17.62*temp)/(243.12 + temp))*(1.0016 - 0.074/press +
																					3.15e-6*press))
}

reltoabshum(30,50)
abstorelhum(30,.01520223)

histerrors=function(thist,dhist){
	thist$counts[is.na(thist$counts)]=0
	countstop=min(c(length(dhist$counts),length(thist$counts)))
	for (whichbin in 1:countstop){
		#     if (is.na(thist$counts[whichbin])) {} else {
		breaklist=dhist$mids
		mytest=binom.test(x = thist$counts[whichbin],sum(thist$counts),
											alternative = "t",
											conf.level = .95^(1/(length(dhist$breaks)-1)),
											p = dhist$counts[whichbin]/sum(dhist$counts))
		#   abline(h=mytest$conf.int*sum(dhist$density))
		errlims=mytest$conf.int*sum(dhist$density);errlims
		#     if(thist$density[whichbin]==0) {} else{
		histline= thist$density[whichbin]
		uperr=errlims[2]-histline
		downerr=histline-errlims[1]
		error.bar(x = breaklist[whichbin],y = histline,
							#                 length = diff(breaklist)[1]*.8,
							upper = uperr,
							lower = downerr)
		#     }
		#     }
	}
	invisible(errlims)
}

# histdata=surfshortmap;thebreaks=80;doplot=T
histadj=function(histdata,breaks=NA,doplot=F,...){
	if (is.na(thebreaks)){
		myhist=hist(histdata,plot = F)
	} else{
		myhist=hist(histdata,breaks = breaks,plot = F)
	}
	myhist$density=myhist$counts/sum(myhist$counts)*100
	if (doplot) plot(myhist,freq=F, ylab="Percent",...)
	invisible(myhist)
}
# histadj(surfshortmap,thebreaks=80,doplot=T)


histplus=function(theerupts,monthssn,splitwid=40,theylim=1.6,
									thecol="skyblue",colorname="Blue",
									thevar="Distance",
									occurrence="quake",
									histmain="Test Histogram",
									datatext="solar distances",
									thebreaks=seq(min(theerupts,na.rm=T),
																max(theerupts,na.rm=T),splitwid),
									colortext=paste0(": ",thevar," at time of ",occurrence,"; ",
																	 "Red hatched: all ",datatext ),
									labels=TRUE){
	theerupts=as.double(theerupts);monthssn=as.double(monthssn)
	testhist=histadj(theerupts,thebreaks=thebreaks)
	plot(testhist,freq = F,col=thecol,
			 main="",
			 ylim=c(0,max(testhist$density)*theylim),
			 xlab=NA, #paste0(colorname,colortext),
			 ylab="Percentage of Data in Each Bin",
			 cex.lab=.9)

	par(lwd=2)
	datahist=histadj(monthssn,thebreaks=thebreaks)
	plot(datahist,freq=F,add=T,density = 10,col="tomato")
	par(lwd=1)
	#   histerror(theerupts,monthss)
	title(main=histmain,
				cex.main=.9)
	title(sub=paste0("Error bars show 95% confidence interval, ",
									 "adjusted for bin count."),cex.sub=.8)
	histerrors(testhist,datahist)
	if (labels) text((butlast(testhist$breaks)+testhist$mids)/2,
									 testhist$density,testhist$counts,cex=.9)
	theks=ks.test(theerupts,monthssn)
	ptext=ifelse(theks$p.value<.05,"is","is not")
	text(x = max(thebreaks)/2,y=max(testhist$density)*theylim*.9,
			 labels=paste0("KS test of results, p-value = ", round(theks$p.value,2),
			 							"\nwhich ",ptext," statistically significant"),
			 cex=.8)
	# end eruptions by dist
	invisible(list(test=testhist,data=datahist))
}


reducedlm=function(timeseries,thestart=c(2005,1),thefreq=12){
	if (is.na(timeseries[1])) {
		NA
	} else if (length(which(!is.finite(timeseries))==0)){
		NA
	} else {
		seasonal=stl(ts(timeseries,thestart,freq=thefreq),"periodic")[[1]][,1]
		newtime=timeseries-seasonal
		lm(newtime~time(newtime))$coeff[2]
	}
}

reducedlmall=function(timeseries,thestart=c(2005,1),thefreq=12){
	if (is.na(timeseries[1])) {
		NA
	} else if (length(which(!is.finite(timeseries))==0)){
		NA
	} else {
		seasonal=stl(ts(timeseries,thestart,freq=thefreq),"periodic")[[1]][,1]
		newtime=timeseries-seasonal+mean(seasonal)
		lm(newtime~time(newtime))
	}
}



cyclelen=24;detrend=T;doplot=T


# sidc=ts(read.csv("SIDC Sunspots.csv")[,2],start=1700,frequency=1)
# myssn=read.fwf("http://sidc.oma.be/DATA/monthssn.dat",widths=rep(8,4))
# monthssn=ts(myssn[,3],start=c(1749,1),frequency=12)
# lastyear=1946
# window(monthssn,end=c(lastyear,12))=window(monthssn,end=c(lastyear,12))*1.2

# timeseries=conc;plot(timeseries)

# x=themap
histbreaks=function(x,breakwidth=diff(range(x,na.rm=T))/20,roundto=0){
	therange=diff(range(x,na.rm=T))
	seq(round(min(x,na.rm=T)-2*breakwidth,roundto),
			round(max(x,na.rm=T)+2*breakwidth,roundto),breakwidth)

}

makecycles=function(ts) ts(rep(tapply(ts,cycle(ts),
																				mean,na.rm=T),2),
													 start=1,frequency = 1)


climatologyerrors=function(timeseries) {
	tapply(timeseries,
				 cycle(timeseries),
				 function(x) sd(x,na.rm=T)/sqrt(heff(x)))
}

makeproxies=function(timeseries,nproxies=10,arimadepth=4,doplot=F){
	myarima=arima(timeseries,order=c(arimadepth,0,arimadepth))
	myar=as.double(myarima$coef[1:arimadepth])
	myma=as.double(myarima$coef[(arimadepth+1):(arimadepth*2)])
	thelength=length(timeseries)
	themat=matrix(arima.sim(model=list(ar=myar,ma=myma),
													thelength*nproxies),ncol=nproxies)
	themeans=matrix(rep(colMeans(themat),thelength),ncol=nproxies,byrow=T)
	thesds=matrix(rep(apply(themat,2,sd),thelength),ncol=nproxies,byrow=T)
	mat2=(themat-themeans)/thesds*sd(timeseries,na.rm=T)+mean(timeseries,na.rm=T)
	answer=ts(mat2,start=start(timeseries),
						freq=frequency(timeseries))
	if (doplot==TRUE) {
		proxy=answer[,1:min(c(10,ncol(answer)))]
		proxy[,ncol(proxy)-3]=timeseries
		colnames(proxy)=c(1:ncol(proxy))
		plot(proxy,yax.flip=T,main="")
	}
	invisible(list(ans=answer,ar=myar,ma=myma))
}
# makeproxies(conc,nproxies=20,doplot=T)

# trims a ts object
# timeseries= (coyears[1])
trimNA=function(timeseries,timestring=NA,thefreq=NA){
	if (class(timeseries)=="ts"){
		if (length(timestring)==1) timestring=time(timeseries)
		if (is.na(thefreq)) thefreq =frequency(timeseries)
		themin=min(which(is.finite(timeseries)))
		themax=max(which(is.finite(timeseries)))
		ts(as.double(timeseries[themin:themax]),start=timestring[themin],
			 end=timestring[themax], frequency=thefreq)
	} else{
		themin=min(which(is.finite(timeseries)))
		themax=max(which(is.finite(timeseries)))
		timeseries[themin:themax]
	}

}
# tideblock[,276]



dcos=function(x) cos(x*pi/180)
dsin=function(x) sin(x*pi/180)
dtan=function(x) tan(x*pi/180)
dasin=function(x) asin(x)*180/pi
dacos=function(x) acos(x)*180/pi
datan=function(x) atan(x)*180/pi


cumsumts=function(timeseries,newzero=mean(timeseries,na.rm = T)){
	timeseries=timeseries-newzero
	timeseries[!is.finite(timeseries)]=0
	ts(c(0,cumsum(timeseries)),start=start(timeseries),
		 frequency=frequency(timeseries))
}
cummeants=function(timeseries,domean=F){
	if (domean) timeseries=timeseries-mean(timeseries)
	ts(cumsum(timeseries)/seq_along(timeseries),start=start(timeseries),frequency=frequency(timeseries))
}

# timeseries=(preresid)
dtrendlin=function(timeseries,doplot=FALSE){
	outseries=timeseries
	badboys=which(!is.finite(timeseries));badboys
	if (length(badboys)>0){
		thelm=lm(timeseries[-badboys]~time(timeseries)[-badboys])
	} else {
		thelm=lm(timeseries~time(timeseries))
	}
	thegood=which(is.finite(timeseries))
	outseries[thegood]=outseries[thegood]-thelm$fit
	if (doplot){
		plot(outseries)
	}
	outseries
}



# x=as.double(spothist$mids[i]);y=as.double(btest$estimate*100);upper=errone

error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
	if(length(x) != length(y) | length(y) !=length(lower) | length(lower) !=
		 length(upper))
		stop("vectors must be same length")
	arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

# abline(h=20)

# x=as.double(time(tser));y=thelm$fitted.values;upper=fullerror

error.shade=function(x,y,upper,lower=upper, col="blue3",trans=.5,border=NA){
	yvec=as.vector(y)
	xvec=as.vector(x)
	ylist=c(plusna(yvec,upper),
					rev(minusna(yvec,upper)),yvec[1])
	xlist=c(xvec,rev(xvec),xvec[1])
	polygon(x=xlist,y=ylist,col=addalpha(col,trans),border=border)
}

plusna=function(v1,v2) colSums(rbind(v1,v2),na.rm=T)
plusnacol=function(somemat)  rowSums(somemat,na.rm=T)
minusna=function(v1,v2) colSums(rbind(v1,-v2),na.rm=T)

round(runif(10,.5,1),1)



butlast=function(x, thelen=NA) {
	if (is.na(thelen)) thelen=1;thelen
	if ("ts" %in% class(x))
		answer=window(x,end=time(x)[length(x)-thelen])
	else{
		answer=x[1:(length(x)-thelen)]
	}
	answer
}

butlastrows=function(x, thelen=NA) {
	x[1:(nrow(x)-thelen),]
}

# x=stratdata
# tail(butlast(stratdata))
# tail(stratdata)

butfirst=function(x,thelen=NA) {
	if (is.na(thelen)) thelen=1
	if (thelen!=0){
		if ("ts" %in% class(x))
			answer=window(x,start=time(x)[thelen+1])
		else{
			answer=x[-c(1:thelen)]
		}
		answer
	} else{
		x
	}
}
butends=function(x,thelen=NA) {
	butfirst(butlast(x))
}

lastdata=function(x,n) x[(length(x)-n+1):length(x)]
firstdata=function(x,n) x[1:n]



firsthalf=function(x,fraction=2,len) {
	if (missing(len)){
		butlast(x,length(x)/fraction)
	} else {
		x[1:len]
	}
}
lasthalf=function(x,fraction=2, len) {
	if (missing(len)){
		butfirst(x,length(x)/fraction)
	} else {
		thelen=length(x)
		x[(thelen-len+1):thelen]
	}
}

drawshade=function(x,origin,col="yellow",lwd=1){
	widther=3
	thedays=as.POSIXct(x=seq(1:(x*widther))*secsperday/widther, tz="GMT",
										 origin=as.POSIXct(origin,tz="GMT"),)
	abline(v=thedays,col=col,lwd=lwd)
}

cumsumzoo=function(x){

}

tsp.zoo=function(x){
	print("HW")
	c(start(x),end(x))
}


gfilt = function(x){
	l=2*(x)+1#fwhm2g
	temp=c(0,pnorm(-3+(6/l)*(1:l)))
	diff(temp)/sum(diff(temp))
	#   temp1=temp
}

realgauss=function(xline,myfilt=myfilt)  {
	xline[which(is.na(myfilt))]=NA
	sum(xline*myfilt,na.rm=TRUE)/sum(myfilt[which(!is.na(xline))],na.rm=T)
}

# realgauss(gmatrix[1,],myfilt)
# sum(myfilt,na.rm=T)
# xline=gmatrix[1,];xline
# xline*myfilt

# gauss=6;x=phsort$ph;nullends=F
# x=bets
backfilt=function(gauss){
	myfilt=gfilt(gauss*2)[1:(2*gauss+1)]
	myfilt[1]=2*myfilt[2]-myfilt[3]
	# plot(myfilt)
	myfilt
}



# thefilt="trail"
# rm(nullends)
# nullends=F
# x=testcases
mygauss=function(x,gauss=0,nullends=F,thefilt=NA){
	if (gauss==0) {x} else{
		if (is.na(thefilt[1])) {
			myfilt=gfilt(gauss)
		}else{
			myfilt=gfilt(gauss)
			myfilt[(gauss+2):(2*gauss+1)]=NA
		}
		# plot(myfilt)
		xlong=c(rep(NA,gauss),x,rep(NA,gauss))
		mywid=2*gauss+1
		gmatrix=matrix(NA,length(x),mywid)
		rowmatrix=row(gmatrix)+col(gmatrix)-1
		gmatrix[,]=(xlong[rowmatrix[,]])

		result=apply(gmatrix,1,realgauss,myfilt=myfilt)
		if (class(x) == "zoo") result=zoo(result,time(x))
		if (class(x) == "ts") result=ts(result,start=start(x),
																		frequency=frequency(x))
		if (nullends>0){
			if (nullends==T){
				nullends=gauss
			}
			N=length(result)
			result[1:nullends]=NA
			result[(N-nullends+1):N]=NA
		}
		result
	}
}
# tail(result)

mygaussmat=function(x,gauss=0,nullends=gauss,thefilt=NA){
	newmat=apply(x,2,mygauss,gauss=gauss,nullends=nullends,thefilt=thefilt)
	if ("ts" %in% class(x)){
		ts(newmat,start=start(x),frequency = frequency(x))
	} else {
		newmat
	}
}

5==F

gaussfactor=Vectorize(mygauss,"gauss")
# x=monthssn;gauss=120;mygausst(x,gauss)
# x=flux;gauss=36
# x=hadshort;gauss=72
par(mgp=c(2,.8,0))

# x=shortbets;gauss=3*12
mygausst=function(x,gauss=0){
	if (gauss==0) {x} else{
		# gauss=gauss*2
		myfilt=backfilt(gauss)
		# myfilt[(gauss+2):(2*gauss+1)]=NA
		xlong=c(rep(NA,gauss),x,rep(NA,gauss))
		mywid=2*gauss+1
		gmatrix=matrix(NA,length(x),mywid)
		rowmatrix=row(gmatrix)+col(gmatrix)-1
		gmatrix[,]=(xlong[rowmatrix[,]])
		result=apply(gmatrix,1,realgauss,myfilt=myfilt);result
		if (class(x) == "zoo") result=zoo(result,time(x))
		if (class(x) == "ts") result=ts(result,start=start(x),frequency=frequency(x))
		result
	}
}

sem=function(x,effn=NA) {
	thelen=ifelse(is.na(effn),length(which(is.finite(x))),effn)
	sd(x[is.finite(x)])/sqrt(thelen)
}



# x=atmabs
# semh(atmabs)
semh = function(tser) {
	avals = arima(tser, c(1, 0, 1))
	round(sd(sapply(1:1000, function(x) {
		# thetest=simFGN0(240,H=y)
		mean(arima.sim(list(
			ar = avals$coef[1], ma = avals$coef[2]
		), n = 240))

	})),2)
}

# x=c(12,7,NA,6)
# na.omit(x)
semedian=function(x) 1.253*sd(x[is.finite(x)],na.rm=T)/sqrt(length(which(is.finite(x))))




truen = function(x) {
	x=as.vector(x)
	auto=acf(dtrendlin(x), plot=FALSE,na.action=na.pass)[[1]][2]
	min(length(x)*(1-auto-0.68/sqrt(length(x)))/(1+auto+0.68/sqrt(length(x))),
			length(x))
}
# quenouille(thesin,tred)
# x=thesin;y=tred
quenouille=function(x,y){
	x=as.vector(x)
	y=as.vector(y)

	detrend=function(x){
		xlm=lm(x~c(0:(length(x)-1)))

		xlm$fitted.values=c(xlm$fitted.values,rep(NA,length(x)-
																								length(xlm$fitted.values)))
		(x-xlm$fitted.values)
	}

	n=length(which(is.finite(x)));n
	acfx=acf(detrend(as.vector(x)),lag.max=n-1,plot=F,na.action=na.pass)$acf[-1]
	acfy=acf(detrend(as.vector(y)),lag.max=n-1,plot=F,na.action=na.pass)$acf[-1]
	max(c((min(c(n/(1+sum(2*acfx*acfy,na.rm=T)),n),na.rm=T)),3),na.rm=T)
}
#   x=thesin;y=tdata

# x=plotdata;y=-huydiff
qtest=function(x,y,q=TRUE){
	if ((class(x)=="ts") & (class(y)=="ts")){
		tsint=ts.intersect(x,y)
		x=tsint[,1]
		y=tsint[,2]
	}
	x=as.vector(x)
	y=as.vector(y)


	bady=which(!is.finite(y))
	if (length(bady)>0) {
		x=x[c(-1*bady)]
		y=y[c(-1*bady)]
	}
	badx=which(!is.finite(x))
	if (length(badx)>0) {x=x[c(-1*badx)];y=y[c(-1*badx)]}


	thelm=lm(y~x)
	which(is.na(y))
	length(thelm$fit)

	if (q) thetruen=quenouille(x,y) else thetruen=truen(y)
	ESS=sum((y-mean(y))^2)
	RSS=sum((y-thelm$fitted.values)^2)
	min(c(1,df((ESS-RSS)/(RSS/thetruen),1,thetruen)))
}

# y=testdata[45,1,]
# plot(y~seq(1,by=1/12,length.out = 120),type="o")
# lines(newtime,type="o",col="red3")
shortq=function(y){
	if (is.na(y[1])) NA else {
		y=ts(y,start=c(1,1),freq=12)
		timevalues=time(y)

		y=y-stl(y,"periodic")[[1]][,1]
		thelm=lm(y~seq_len(length(y)))
		thetruen=truen(y) #quenouille(x,y)
		ESS=sum((y-mean(y))^2)
		RSS=sum((y-thelm$fitted.values)^2)
		min(c(1,df((ESS-RSS)/(RSS/thetruen),1,thetruen)))
	}
}

# adds an alpha channel to a color
addalpha = function(colors, alpha=1.0) {
	r <- col2rgb(colors, alpha=T)
	# Apply alpha
	r[4,] <- alpha*255
	r <- r/255.0
	return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


# cumulative sum of time series
cumsumzoo=function(timeseries,doscale=T){
	timeseries=timeseries-mean(timeseries,na.rm=TRUE)
	thecum=cumsum(as.vector(timeseries))
	thetimes=time(timeseries)
	if (doscale) thecum=scale(thecum)
	zoo(thecum,thetimes)
}
# trims NAs from both ends of a time series
trim=function(timestring){
	window(timestring,
				 start=time(timestring)[min(which(is.finite(timestring)))],
				 end=time(timestring)[max(which(is.finite(timestring)))])
}

daysperyear=365.2425
hrsperyear=daysperyear*24
secsperyear=hrsperyear*3600-17075

# plot(tideblock[,1])


trimall=function(timeblock){
	if (!is.finite(dim(timeblock))){
		trim(timeblock)
	} else {
		apply(timeblock,2,trim)
	}
}

trimlength=function(timestring) length(trim(timestring))

# hurstonly function ----------------------------

hurstonly=function(timevector,
									 boxwidth=NA,
									 datapoints=20){
	onehurst=function(interval,tv) {
		sd(stats::filter(as.double(tv),filter=rep(1,interval)/interval),na.rm=T)
	}
	timevector=as.double(timevector)
	timevector=(timevector-mean(timevector,na.rm=T))/sd(timevector,na.rm=T)
	if (is.na(boxwidth)) boxwidth=floor(length(timevector)/10) else boxwidth=floor(length(timevector)*boxwidth);boxwidth

	pointseq=round(c(1,10^(seq(.2,log(boxwidth,10),length.out = datapoints-1))),0);pointseq
	thedata=(sapply(pointseq,onehurst, tv=timevector))


	1+as.double(lm(log(thedata,10)~0+log(pointseq,10))$coef[1])

}

mod=function(a,b) a %% b

tau2lag=function(tau=3.3,period=12) atan(tau*2*pi/period)/(2*pi/period)
tau2lag()
# [1] 1.998003
lag2tau=function(lag=2,period=12) period*tan((2*lag*pi)/period)/(2*pi)
lag2tau(1)
# [1] 3.307973

tau2atten=function(tau=3.3,period=12) 1/sqrt(1+(tau*2*pi/period)^2)
tau2atten()
# [1] 0.5009052
lag2atten=function(lag=2,period=12) tau2atten(lag2tau(lag,period),period)
lag2atten()
# [1] 0.5

tauconvert=function(tau=3.3,period=12,newperiod=24) tau/period*newperiod
tauconvert()
# [1] 6.6

# timeser=nao;halflen=6

x=9

runmean=function(timeser,halflen){
	filtlen=2*halflen+1
	thefilt=rep(1,filtlen)/filtlen;thefilt
	timeplus=c(rep(NA,halflen),timeser,rep(NA,halflen))
	ts(sapply((1+halflen):
							(length(timeser)+halflen),
						function(x){
							thetest=timeplus[(x-halflen):(x+halflen)]
							thetest
							sum(thetest*thefilt,na.rm = T)/
								sum(thefilt[which(is.finite(thetest))],
										na.rm = T)
						}),
		 start=start(timeser),
		 frequency = frequency(timeser))
}


dTdW=function(w,epsilon=1)  {16.2/(epsilon*(w*epsilon)^(3/4))}
dWdT=function(k,epsilon=1)  {2.27*10^-7*epsilon* k^3}
dWdTC=function(k,epsilon=1)  {2.27*10^-7 * epsilon * (k+273.15)^3}



1/(4^4)
1/5.67e-8
getstatename=function(abbrev=qc(AK, AL, AR, AS, AZ, CA, CO, CT, DC, DE, FL, GA, GU, HI, IA, ID, IL, IN, KS, KY, LA, MA, MD, ME, MI, MN, MO, MP, MS, MT, NC, ND, NE, NH, NJ, NM, NV, NY, OH, OK, OR, PA, PR, RI, SC, SD, TN, TX, UT, VA, VI, VT, WA, WI, WV, WY, US)){
	rawdata=read.csv("State Populations.csv",as.is = T)
	data.frame(Abbr=abbrev,Name=rawdata$State[match(abbrev,rawdata$Abbr)])
}
getstatepop=function(abbrev=qc(AK, AL, AR, AS, AZ, CA, CO, CT, DC, DE, FL, GA, GU, HI, IA, ID, IL, IN, KS, KY, LA, MA, MD, ME, MI, MN, MO, MP, MS, MT, NC, ND, NE, NH, NJ, NM, NV, NY, OH, OK, OR, PA, PR, RI, SC, SD, TN, TX, UT, VA, VI, VT, WA, WI, WV, WY, US)){
	rawdata=read.csv("State Populations.csv")
	data.frame(Name=getstatename(abbrev)[,2],Abbr=abbrev,Pop=rawdata$Pop[match(abbrev,rawdata$Abbr)])
}

# stname=nytcovcounty$state;stname
# getstateabb(stname)
getstateabb=function(stname=NA){
	rawdata=read.csv("State Populations.csv")
	if (is.na(stname[1])) stname=rawdata$State
	theframe=data.frame(Name=stname,Abbr=rawdata$Abbr[match(stname,rawdata$State)]);theframe

	thedistrict=grep("District",theframe$Name,fixed = T)
	if (length(thedistrict)>0) theframe$Abbr[thedistrict]="DC"
	theus=grep("US",theframe$Name,fixed = T);theus
	# if (length(thedistrict)>0) theframe$Abbr="DC"
	theframe
}

getstatepopbyname=function(stname=NA){
	rawdata=read.csv("State Populations.csv")
	if (is.na(stname[1])) stname=rawdata$State
	data.frame(Name=stname,Abbr=getstateabb(stname)[,2],Pop=rawdata$Pop[match(stname,rawdata$State)])
}

logtextbox <- function(mline,
											 theadj,
											 themid,
											 thewid,
											 backcol = "white",
											 thetext,
											 thecex = .8,
											 shiftlr=.05,
											 newmline=.97,
											 shadow=.5,
											 ...) {
	mtop = mline*newmline * (1 + theadj)
	mbottom = mline*newmline / (1 + theadj)
	rect(themid+shiftlr - thewid, mbottom, themid+shiftlr + thewid,
			 mtop, col = addalpha("black",shadow),
			 border=addalpha("black",shadow))

	mtop = mline * (1 + theadj)
	mbottom = mline / (1 + theadj)
	rect(themid - thewid, mbottom, themid + thewid,
			 mtop, col = backcol)
	text(themid, mline, thetext, cex = thecex, ...)
}

source("plotdecomp function.R")

# Derived constants ----------------------------------
pinatubo=twodatetodecimal(c(1991,6))
agung=twodatetodecimal(c(1963,2))
elchichon=twodatetodecimal(c(1982,3))
santamaria=twodatetodecimal(c(1902,10))
krakatoa=twodatetodecimal(c(1883,5))


cases=function (..., check.xor = c("warn", "stop", "ignore"))
{
	subst <- match.call(expand.dots = FALSE)$...
	if (!missing(check.xor))
		if (is.logical(check.xor))
			check.xor <- ifelse(check.xor, "stop", "ignore")
		else check.xor <- as.character(check.xor)
		check.xor <- match.arg(check.xor)
		deflabels <- sapply(subst, deparse)
		if (length(subst) < 2)
			stop("need at least two conditions")
		have.arrows <- sapply(subst, length) > 1
		have.arrows[have.arrows] <- have.arrows[have.arrows] & sapply(sapply(subst[have.arrows],
																																				 "[[", 1), paste) == "<-"
		parent <- parent.frame()
		if (all(have.arrows)) {
			cond.names <- names(subst)
			conditions <- lapply(subst, "[[", 3)
			values <- lapply(subst, "[[", 2)
			conditions <- do.call(cbind, lapply(conditions, eval,
																					envir = parent))
			if (ncol(conditions) != length(subst))
				stop("at least one condition results in NULL")
			if (!is.logical(conditions))
				stop("all conditions have to be logical")
			na.cond <- rowSums(is.na(conditions)) > 0
			done <- rowSums(conditions, na.rm = TRUE)
			if (any(done != 1) && check.xor != "ignore") {
				msg <- switch(check.xor, warn = warning, stop = stop)
				if (any(done == 0) && any(done > 1))
					msg("conditions are neither exhaustive nor mutually exclusive")
				else if (any(done == 0))
					msg("conditions are not exhaustive")
				else if (any(done > 0))
					msg("conditions are not mutually exclusive")
			}
			never <- colSums(conditions[!na.cond, , drop = FALSE]) ==
				0
			if (any(never) && check.xor != "ignore") {
				neverlab <- deflabels[never]
				if (length(neverlab) == 1)
					warning("condition ", neverlab, " is never satisfied")
				else warning("conditions ", paste(neverlab, collapse = ", "),
										 " are never satisfied")
			}
			values <- lapply(values, eval, envir = parent.frame(),
											 enclos = parent.frame())
			nrow <- unique(sapply(values, length))
			if (length(nrow) > 1 || nrow != nrow(conditions)) {
				nrow <- nrow(conditions)
				values <- lapply(values, function(x) {
					tmp <- x
					length(tmp) <- nrow
					tmp[] <- x
					tmp
				})
			}
			values <- do.call(cbind, values)
			res <- vector(nrow(conditions), mode = storage.mode(values))
			conditions[na.cond, ] <- FALSE
			for (i in rev(1:ncol(conditions))) {
				res[conditions[, i]] <- values[conditions[, i], i]
			}
			res[na.cond] <- as.vector(NA, mode = storage.mode(values))
			res[done == 0] <- as.vector(NA, mode = storage.mode(values))
			if (length(cond.names) && all(nzchar(cond.names))) {
				uq.values <- drop(unique(values))
				if (length(uq.values) == length(cond.names))
					labels(res) <- structure(unique(uq.values), names = cond.names)
			}
			res
		}
		else if (!any(have.arrows)) {
			conditions <- cbind(...)
			if (ncol(conditions) != length(subst))
				stop("at least one condition results in NULL")
			if (!is.logical(conditions))
				stop("all conditions have to be logical")
			na.cond <- rowSums(is.na(conditions)) > 0
			codes <- 1:ncol(conditions)
			labels <- colnames(conditions)
			if (length(labels))
				labels <- ifelse(nzchar(labels), labels, deflabels)
			else labels <- deflabels
			done <- rowSums(conditions, na.rm = TRUE)
			if (any(done != 1) && check.xor != "ignore") {
				msg <- switch(check.xor, warn = warning, stop = stop)
				if (any(done == 0) && any(done > 1))
					msg("conditions are neither exhaustive nor mutually exclusive")
				else if (any(done == 0))
					msg("conditions are not exhaustive")
				else if (any(done > 0))
					msg("conditions are not mutually exclusive")
			}
			never <- colSums(conditions[!na.cond, , drop = FALSE]) ==
				0
			if (any(never)) {
				neverlab <- deflabels[never]
				if (length(neverlab) == 1)
					warning("condition ", neverlab, " is never satisfied")
				else warning("conditions ", paste(neverlab, collapse = ", "),
										 " are never satisfied")
			}
			res <- integer(nrow(conditions))
			conditions[na.cond, ] <- FALSE
			for (i in rev(1:ncol(conditions))) {
				res[conditions[, i]] <- i
			}
			res[na.cond] <- NA_integer_
			res[done == 0] <- NA_integer_
			factor(res, levels = codes, labels = labels)
		}
		else stop("inconsistent arguments to 'cases'")
}

detachall=function(){
	invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)),   # Unload add-on packages
									 detach,
									 character.only = TRUE, unload = TRUE))
	(.packages())
}

source("CERES Functions.R")
par(font.lab=2,font.axis=2)
landfrac=sum(areamatrix*seamask,na.rm=T)/surfaream
seafrac=1-landfrac
#
# resetplot()
#
# source("Berkeley TS Setup.R")
# lmts(window(ballts,1900,c(1999,12)))
#
# temprange=getarrayrange(allt2)
# drawworld(temprange)
# 8/2.88
# .4/2.88



