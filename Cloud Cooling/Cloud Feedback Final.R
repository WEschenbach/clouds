# Put the "Cloud Cooling" folder in your usual workspace, and enter the name of that workspace below
usual_workspace="R Files"
setwd(paste0("~/",usual_workspace,"/Cloud Cooling"))
source("Willis Functions.R")


# Load Data -----------------------------------
load("CERES Maps.tab",verbose = T)
load("allt2 Part 1.tab",verbose = T)
load("allt2 Part 2.tab",verbose = T)
load("allt2 Part 3.tab",verbose = T)
allt2=abind(allt2part1,allt2part2,allt2part3,along=3)


# Functions ---------------------------------------------
intarea = function(x, thestep = 10, thevar = allt2_map) {
	temparea = thevar
	temparea[temparea < x] = NA
	temparea[temparea >= (x + thestep)] = NA
	temparea[!is.na(temparea)] = 1
	if (x < -20) {
		theans = round(sum(temparea * areamatrix, na.rm = T) / sum(areamatrix) * 100, 1)
	} else{
		theans = round(sum(temparea * areamatrix, na.rm = T) / sum(areamatrix) * 100, 0)
	}
	paste0(theans, "%")
}

getvals=function(depmap,indmap,theshift=9){
	cretrendlongsea=squaretrends(depmap*landmask,indmap*landmask,newshift=theshift)*landmask
	cretrendlongland=squaretrends(depmap*seamask,indmap*seamask,newshift=theshift)*seamask
	cretrendall=arraymeans(abind(cretrendlongland,cretrendlongsea,along = 3))
	drawworld(cretrendall,roundto=1,doplot=F)
}


# Setup sketch CERES ------------------------------------
theseq = seq(-65, 25, 10)
(inttext = sapply(theseq, intarea, thevar = allt2_map))
whichvar = "CERES"

# sketch temp cre CERES ----------------------------
resetplot()

crelow = sketch(
	surf_cre_net_tot_map,
	allt2_map,
	themask = 1,
	dolow = F,
	lowright = 30,
	lowf = .1,
	col = "black",
	alpha = .06,
	ylab = "Net Surface CRE (W/m2)",
	main = "",
	xlab = "Surface Temperature (°C)",
	cex = .3,
	xaxp = c(-60, 30, 9)
)

abline(h = 0)
ablineclip(v = 0, y2 = 50)
abline(v = c(theseq, 35), lty = "dotted")
blackline(crelow,
					bg = "gray50",
					col = "white",
					frontline = 2)

lr = -30
text(lr, 0, "Warming\n\nCooling", font = 2)
boxed.labels(0, -105, "Freezing", ypad = 2, font = 2)
title(
	main = paste0(
		"Scatterplot, ",
		whichvar,
		" Surface Temperature versus",
		"\nSurface Net CRE, 22 Year Averages"
	),
	cex.main = 1,
	line = .8
)
text(-33, -115, "White/black line is LOWESS smooth.", font = 2)

text(c(-58.5, seq(-50, 30, 10)), 77, inttext)
boxed.labels(-10, 65,
						 "Percent of Earth's Surface By Temperature", ypad = 2)
title(sub = paste0(subtextceres),
			cex.sub = .9,
			line = 3.1)
# __ ------------------------------------------------------


# Setup sketch Berkeley ------------------------------------
theseq = seq(-65, 25, 10)
(inttext = sapply(theseq, intarea, thevar = berk_map))
whichvar = "Berkeley Earth"

# sketch temp cre Berkeley ----------------------------
resetplot()
crelow = sketch(
	surf_cre_net_tot_map,
	berk_map,
	themask = 1,
	dolow = F,
	lowright = 29,
	lowf = .1,
	ylab = "Net Surface CRE (W/m2)",
	main = "",
	xlab = "Surface Temperature (°C)",
	cex = .3,
	col = "black",
	alpha = .06,
	xlim = c(min(allt2_map), max(allt2_map)),
	xaxp = c(-60, 30, 9)
)
abline(h = 0)
ablineclip(v = 0, y2 = 50)
abline(v = c(theseq, 35), lty = "dotted")
blackline(crelow,
					bg = "gray50",
					col = "white",
					frontline = 2)

lr = -30
text(lr, 0, "Warming\n\nCooling", font = 2)
boxed.labels(0, -105, "Freezing", ypad = 2, font = 2)
title(
	main = paste0(
		"Scatterplot, ",
		whichvar,
		" Surface Temperature versus",
		"\nSurface Net CRE, 22 Year Averages"
	),
	cex.main = 1,
	line = .8
)
text(-33, -115, "White/black line is LOWESS smooth.", font = 2)

text(c(-58.5, seq(-50, 30, 10)), 77, inttext)
boxed.labels(-10, 65, "Percent of Earth's Surface By Temperature", ypad = 2)
title(sub = paste0(subtextceres),
			cex.sub = .9,
			line = 3.1)
# __ ------------------------------------------------------



# Setup square trend -------------------------------------
theshift = 9 # this is the size of the box used for the comparison
cretrendlongsea = squaretrends(surf_cre_net_tot_map * landmask,
															 allt2_map * landmask,
															 newshift = theshift) * landmask
cretrendlongland = squaretrends(surf_cre_net_tot_map * seamask,
																allt2_map *	seamask,
																newshift = theshift) * seamask
cretrendall = arraymeans(abind(cretrendlongland,
															 cretrendlongsea, along = 3))
mainrot = 0
contourrot = 180

# # draw square trend all -------------------------------

drawworld(
	cretrendall,
	rotation = mainrot,
	linewidth = 2,
	theunits = "Wm-2/°C",
	titletext = "Long-Term Changes in Surface CRE per 1°C Surface Warming\n(Negative values show increased cooling as the surface warms.)",
	drawlogo = F,
	mincolor = -40,
	maxcolor = 30,
	thebox = thebox,
	legend.cex = .8,
	roundto = 0
)
drawcontoursblack(
	cretrendall,
	0,
	doplot = "T",
	thecolor = addalpha("white", 1),
	backcolor = addalpha("black", .5),
	colorname = "white",
	theunits = "W/m2 per 1°C",
	therot = contourrot,
	cex = .3,
	cexback = .6
)

# __ -----------------------------


# setup rotation ------------------------------------
mainrot = 180
contourrot = 0

# # draw square trend rotated -------------------------------

drawworld(
	cretrendall,
	rotation = mainrot,
	linewidth = 2,
	theunits = "Wm-2/°C",
	titletext = "Long-Term Changes in Surface CRE per 1°C Surface Warming\n(Negative values show increased cooling as the surface warms.)",
	drawlogo = F,
	mincolor = -40,
	maxcolor = 30,
	thebox = thebox,
	legend.cex = .8,
	roundto = 0
)
drawcontoursblack(
	cretrendall,
	0,
	doplot = "T",
	thecolor = addalpha("white", 1),
	backcolor = addalpha("black", .5),
	colorname = "white",
	theunits = "W/m2 per 1°C",
	therot = contourrot,
	cex = .3,
	cexback = .6
)

# end -----------------------------

# draw net cre ------------------------------------------
drawworld(
	surf_cre_net_tot_map,
	titletext = "Surface Net Cloud Radiative Effect (CRE), 22 Year Averages\n(Negative values show cooling.)",
	mincolor = -70,
	maxcolor = 50,
	drawlogo = F,
	roundto = 0,
	rotation = 0
)
drawcontoursblack(
	surf_cre_net_tot_map,
	doplot = "T",
	theunits = "W/m2",
	therot = 180
)

# boxplot setup --------------------------------------


allvals=sapply(1:22,function(i){
	print(i)
	depmap=surf_cre_net_tot_years[,,i]
	indmap=allt2_years[,,i]
	getvals(depmap,indmap)
})
allmat=(matrix(unlist(aperm(data.matrix(allvals))),c(22,8)))

# boxplot ------------------------------------
resetplot()
boxplot(allmat[,1:8],names=c("Global","NHem","SHem","Land","Sea","Tropics","Arctic","Antarc"))
abline(h=0)
gridy()
boxplot(allmat[,1:8],names=c("Global","NHem","SHem","Land","Sea","Tropics","Arctic","Antarc"),add=T)
title(sub=paste0(subtextceres),cex.sub=.9,line=2)
title(main=paste0("Annual Changes in Surface CRE per 1°C Surface Warming, 22 Individual Years\n(Negative values show increased cooling as the surface warms.)"),cex.main=1,line=.8)

# __ ----------------------------------------

# setup ranges ------------------------------------------------------------

theranges = pbapply(allt2, c(1, 2), function(x)
	diff(range(x)))
# draw ranges ---------------------------------------------
drawworld(
	theranges / ctok(allt2_map) * 100,
	drawlogo = F,
	titletext = "Gridcell Monthly Minimum to Maximum Temperature Range\n(percent of mean gridcell temperature, Mar 2000 - Feb 2022)",
	maxcolor = 26,
	mincolor = 1,
	theunits = "%"
)

# __ ----------------------------------------------------
