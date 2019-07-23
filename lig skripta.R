#instalacija svih živih paketa
library(devtools)
install_github("SWotherspoon/SGAT")
install_github("SWotherspoon/BAStag")
install_github("eldarrak/FLightR@0.3.6") # install this version if you want completely the same results as in the paper
install_github("eldarrak/FLightR")  # this will install latest version
library(BAStag)

d.lig <- readLig("MMk006.lig", skip = 1) #učitava i preskače prvi red koji ima neke gluposti
d.lig<-d.lig[,c(-1,-3)] #ostavlja samo Date i Light stupce
d.lig<- subset(d.lig,select=c("Date","Light")) #ne znam što radi

## Find proper offset and have a first look at the data
offset=10
lightImage(d.lig, offset = offset, zlim = c(0, 64), dt = 120) # dt specifies the recording interval


start <- as.POSIXct("2014-06-08", tz = "GMT")
end   <- as.POSIXct("2015-02-27", tz = "GMT")
abline(v = c(start, end), lty = c(1,2), col = "orange", lwd = 2)

d.lig <- subset(d.lig, Date>=start & Date<=end)

threshold=1.5 # better use 1.5 for Intigeo tags if no strong reason for other value

twl <- preprocessLight(d.lig, threshold, offset = offset, lmax=64) # only needs to be done once

library(FLightR)
TAGS.twilights.raw<-BAStag2TAGS(d.lig, twl, threshold=threshold)
TAGS.twilights.raw$datetime<-format(TAGS.twilights.raw$datetime, format="%Y-%m-%dT%T.000Z")
write.csv(TAGS.twilights.raw, file="tmp.csv", quote=FALSE, row.names=FALSE)
```
Now you can read these data by [GeoLight](https://github.com/eldarrak/FLightR/blob/master/examples/Black-Tailed_Godwit_JAB_example/A5_GeoLight_analysis.Rmd) or [FLightR](https://github.com/eldarrak/FLightR/blob/master/examples/Black-Tailed_Godwit_JAB_example/A6_FLightR_analysis.Rmd).

#Idemo dalje
Proc.data<-get.tags.data("tmp.csv")

#pa sad kalibracija
Calibration.periods<-data.frame(
        calibration.start=as.POSIXct("2014-07-25"),
        calibration.stop=as.POSIXct("2014-08-17"),
        lon=18.173, lat=47.185) 
#izbriso sam drugi red (NA vrijednosti) jer imamo samo jedan period kalibracije
#jer nemamo podataka o povratku
print(Calibration.periods)

Calibration<-make.calibration(Proc.data, Calibration.periods)
plot_slopes_by_location(Proc.data=Proc.data, location=c(18.173, 47.185))

#sad ide namještanje kalibracijskog perioda prema dbivenom grafu ali
#to još nisam isprobao pa samo stavljam ovdje za isprobat kasnije
abline(v=as.POSIXct("2013-08-20")) # end of first calibration period
abline(v=as.POSIXct("2014-05-05")) # start of the second calibration period

#ocrtavanje mreže pojavnosti životinje
Grid<-make.grid(left=-40, bottom=-40, right=70, top=50,
                distance.from.land.allowed.to.use=c(-Inf, Inf),
                distance.from.land.allowed.to.stay=c(-Inf, Inf))

#priprema
# ~ 30 min run time
all.in<-make.prerun.object(Proc.data, Grid, start=c(18.173, 47.185), Calibration=Calibration)

#analiza
nParticles=1e6
# ~ 120 min run time
Result<-run.particle.filter(all.in, threads=-1,
                            nParticles=nParticles, known.last=FALSE,
                            precision.sd=25, check.outliers=TRUE)
save(Result, file="ResultMMk006.RData")
Result548<-get(load("probaZ548.RData")) #učitavanje rezultata ostalih geolokatora
Qvantili<-Result$Results$Quantiles #izvlačenje tablice s kvantilima iz Result liste
write.csv(Qvantili, file="QvantiliMMk006.csv") #sejvanje gorenavedene tablice u fajl
#izračun kad je otišla iz mađarske
Index<-which(Result$Spatial$Grid[,1]>(20)) #20. meridijan uzet kako "granica mađarske". može se uzet latitude sa [,2] umjesto [,1].
Departures.HU<-find.times.distribution(Result,Index)
Departures.HU

#stacionarni periodi
stationary.migration.summary(Result, prob.cutoff = 0.1, min.stay = 3)

#karta s najvjerojatnijim lokacijama, ali treba imati API key
map.FLightR.ggmap(Result)

#procijenjeni sumraci tj kretanje po lon/lat
plot_lon_lat(Result)

#korištenje prostora na zimovalištu
plot_util_distr(Result, 
        dates=data.frame(as.POSIXct('2013-12-01'), as.POSIXct('2014-01-31')),
        add.scale.bar=TRUE, percentiles=0.5)
#ali nemamo zimovanja za Mk001 tak da jbg. a i opet fali API