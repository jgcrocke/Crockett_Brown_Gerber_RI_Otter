library(sf)
library(lubridate)
rm(list=ls())
library(this.path)
setwd(here())
setwd('..')
km=st_read("Site Shapefiles/km_IDM.shp")
OtterTTD=read.csv("OtterTTD_Kmlink.csv")
latrines=st_read("Other Shapefiles/latrines_in_km_clean.shp")
latrines=latrines[!is.na(latrines$kmu),]
latrines$year=year(latrines$first)
roadkill=st_read("Other Shapefiles/roadkill_clean.shp")
roadkill=roadkill[!is.na(roadkill$kmu),]
roadkill$year=year(roadkill$date_1)
resurveys=read.csv("Latrine.Resurveys.csv")
resurveys=resurveys[!is.na(resurveys$prsnt_l)&
                      !is.na(resurveys$kmu),-1]
OtterTTD=OtterTTD[,-1]
OtterTTD=OtterTTD[!is.na(OtterTTD$km),]
beavereffort=read.csv("BeaverEffort.csv")
beavereffort=beavereffort[,-1]

COVS=st_drop_geometry(km)
for(i in 7:38){
  COVS[,i]=(COVS[,i]-mean(COVS[,i]))/sqrt(var(COVS[,i]))
}
forest=COVS[,which(regexpr("Frs",colnames(COVS))>0)]
water=COVS[,which(regexpr("Wtr",colnames(COVS))>0)]
wetland=COVS[,which(regexpr("Wet",colnames(COVS))>0)]
Urban=COVS[,which(regexpr("Urb",colnames(COVS))>0)]


COVS$BStrm=ifelse(COVS$StrmOr_>1,1,0)


COVS$Broads=COVS$logroads-mean(COVS$logroads,na.rm=T)
COVS$roadpres=ifelse(is.na(COVS$roads),0,
                     ifelse(COVS$roads>0,1,0))
COVS$boatscale=(COVS$boatd-mean(COVS$boatd))/(sqrt(var(COVS$boatd)))

COVS$Broads[!is.finite(COVS$logroads)]=-99


ocovs=OtterTTD[,10:18]
for (i in 1:9){
  ocovs[,i]=(OtterTTD[,i+9]-mean(OtterTTD[,i+9]))/
    sqrt(var(OtterTTD[,i+9]))
}



#####
#Making the Effect Coding for Watershed and Observer
#####
wshed=as.factor(COVS$Watrshd)
levels(wshed)
WSHED=model.matrix(~wshed,contrasts = list(wshed = contr.sum))
colnames(WSHED)=c("Int","BIS","Black","Narr","Pawc","Pawt","Quin")
unique(OtterTTD$Observer)
OtterTTD$Observer[OtterTTD$Observer=="Emerson Paton"|
                    OtterTTD$Observer=="Emma Paton"|
                    OtterTTD$Observer=="Charlie Brown"|
                    OtterTTD$Observer=="Morgan Lucot"|
                    OtterTTD$Observer=="Richard Mercer"]="Other Observer"
obs=as.factor(OtterTTD$Observer)
levels(obs)
OBS=model.matrix(~obs,contrasts = list(obs = contr.sum))
colnames(OBS)=c("Int",levels(obs)[1:6])
ocovs$kayak=0
ocovs$kayak[OtterTTD$Mode=="kayak"]=1
#####
#Bits and bobs to give the model
#####
Years=seq(1999:2023)-1
UNQ=unique(OtterTTD[,1:2])
#d <- as.numeric(is.na(OtterTTD$TTD)) # Censoring indicator
M=length(unique(OtterTTD$km))
nobs=length(OtterTTD$TTD)
nrsurv=length(resurveys$WATERSH)
ursurv=match(resurveys$kmu,km$id)
rsrvyr=year(as.Date(resurveys$last))-1999
TTDF=vector(length = nobs)
for (i in 1:nobs){
  TTDF[i]=which(COVS$id==OtterTTD$km[i])
}
max(TTDF)
ncells=length(km$id)
npoints=length(latrines$ID)+length(roadkill$ID)
isrk=c(rep(0,length(latrines$first)),rep(1,length(roadkill$ID)))
u=c(match(latrines$kmu,km$id),match(roadkill$kmu,km$id))
pointyr=c(latrines$year-1998,roadkill$year-1998)
y=ifelse(is.na(OtterTTD$TTD),0,1)
occyr=year(as.Date(OtterTTD$Date))-1998
LCDates=ymd(c("2001-06-01","2004-06-01","2006-06-01",
              "2008-06-01","2011-06-01","2013-06-01",
              "2016-06-01","2019-06-01"))
yrstoNLCD=vector(length = 25)
for(i in 1:25){
  yrstoNLCD[i]=which.min(abs(year(LCDates)-(i+1998)))
}

library(rjags)
library(runjags)
library(mcmcplots)
library(MCMCvis)
model_string <-"model
{
  
# Likelihood
for (w in 1:25){
for (m in 1:ncells){ # Model for occurrence at site level
log(lambda[m,w])=lambda.0+lambdaWat*Wat[m,yrstoNLCD[w]]+
                        lambdaStrm*Strm[m]+lambdaSalt*Salt[m]+
                        lambdaWet*Wet[m,yrstoNLCD[w]]+
                        lambdaUrb*Urb[m,yrstoNLCD[w]]+
                        lambda.Time*(w-1)
z[m,w] ~ dbern(1-exp(-lambda[m,w]))
logit(b.L[m,w])=alpha.int.L+alphalaunch*launch[m]+alphaStrm*Strm[m]+
                          alphabeav*beaver[m,w]+
                          alphaSalt*Salt[m]+alphatime.L[w]
logit(b.R[m,w])=alpha.int.R+alpharoadp*roadpres[m]+alphatime.R[w]+
                          alpharoads*roads[m]*roadpres[m]
b[m,w]<-1-((1-b.L[m,w])*(1-b.R[m,w]))
}
denom[w]=inprod(lambda[1:ncells,w],b[1:ncells,w])
alphatime.L[w]~dnorm(0,1/tau.L)
alphatime.R[w]~dnorm(0,1/tau.R)
}
for(p in 1:npoints){
ones[p]~dbern(
                exp(
                    log(
                    lambda[landid[p],pointyr[p]]*(
                    isrk[p]*b.L[landid[p],pointyr[p]]+
                    (1-isrk[p])*b.R[landid[p],pointyr[p]]
                    )
                    )-
                    log(denom[pointyr[p]]/npoints)
                )/CONSTANT
              )
}


for (i in 1:nobs){ # Observation model at observation level
y[i] ~ dbern(p[i]*z[TTDF[i],occyr[i]])
logit(p[i]) <- p.0+alpha1*OBS[i,2]+alpha2*OBS[i,3]+
                  alpha3*OBS[i,4]+alpha4*OBS[i,5]+alpha5*OBS[i,6]+
                  alpha6*OBS[i,7]+alpha7*water[i]+
                  alpha8*built[i]+alpha9*wetland[i]+
                  alpha10*temp[i]+alpha11*cloud[i]+
                  alpha12*precip[i]+alpha13*kayak[i]
}
for (i in 1:nrsurv){#observation model for re-surveyed latrines
  yrsrv[i]~dbern(p.rsrv[i]*z[ursurv[i],rsrvyr[i]])
  logit(p.rsrv[i])=p.rsrv.0
}

# Priors
lambda.0    ~dnorm(log(-log(0.5)),4) 
lambdaStrm  ~dnorm(0,1)
lambdaWet  ~dnorm(0,1)
lambdaSalt  ~dnorm(0,1)
lambdaUrb  ~dnorm(0,1)
lambdaWat  ~dnorm(0,1)
lambda.Time~dnorm(0,25)
alpha.int.L    ~dlogis(0,1)
tau.L~dgamma(1,1)
alpha.int.R    ~dlogis(0,1)
tau.R~dgamma(1,1)
alphalaunch ~dlogis(0,1)
alphaStrm   ~dlogis(0,1)
alphabeav   ~dlogis(0,1)
alphaSalt   ~dlogis(0,1)
alpharoadp    ~dlogis(0,1)
alpharoads ~dlogis(0,1)
p.0~ dlogis(0,1)
alpha1 ~dlogis(0,1)
alpha2 ~dlogis(0,1)
alpha3 ~dlogis(0,1)
alpha4 ~dlogis(0,1)
alpha5 ~dlogis(0,1)
alpha6 ~dlogis(0,1)
alpha7 ~dlogis(0,1)
alpha8 ~dlogis(0,1)
alpha9 ~dlogis(0,1)
alpha10~dlogis(0,1)
alpha11~dlogis(0,1)
alpha12~dlogis(0,1)
alpha13~dlogis(0,1)
p.rsrv.0~dlogis(0,1)
}
"
data <- list(ncells=ncells,
             nobs=nobs,
             npoints=length(pointyr),
             yrstoNLCD=yrstoNLCD,
             nrsurv=nrsurv,
             Wat=as.matrix(water),
             Wet=as.matrix(wetland),
             Urb=as.matrix(Urban),
             Salt=COVS$Salt,
             Strm=COVS$BStrm,
             launch=COVS$boatscale,
             beaver=as.matrix(beavereffort),
             occyr=occyr,
             landid=c(match(latrines$kmu,km$id),
                      match(roadkill$kmu,km$id)),
             ursurv=ursurv,
             pointyr=pointyr,
             rsrvyr=rsrvyr,
             CONSTANT=10000,
             OBS=OBS,
             built=ocovs$Built,
             wetland=ocovs$Wetland,
             water=ocovs$Water,
             temp=ocovs$Temp,
             cloud=ocovs$cloudcover,
             precip=ocovs$Precip,
             roads=COVS$Broads,
             roadpres=COVS$roadpres,
             y=y,yrsrv=resurveys$prsnt_l,
             TTDF=TTDF,
             kayak=ocovs$kayak,
             ones=rep(1,length(latrines$WATERSH)+
                        length(roadkill$ID)),
             isrk=isrk
)
zst=matrix(nrow=ncells,ncol = 25)
for(i in 1:25){
  zst[,i] <- rep(1, ncells)
}

inits=function(){list(z =zst,
                      lambda.0   =rnorm(1),
                      lambdaStrm =rnorm(1),
                      lambdaWet  =rnorm(1),
                      lambdaSalt =rnorm(1),
                      lambdaUrb  =rnorm(1),
                      lambdaWat  =rnorm(1),
                      lambda.Time=rnorm(1),
                      alpha.int.L =rnorm(1),
                      alpha.int.R =rnorm(1),
                      tau.L=runif(1),
                      tau.R=runif(1),
                      alphalaunch=rnorm(1),
                      alphaSalt=rnorm(1),
                      alphaStrm  =rnorm(1),
                      alphabeav=rnorm(1),
                      alpharoads=rnorm(1),
                      alpharoadp=rnorm(1),
                      p.0        =rnorm(1),
                      alpha1     =rnorm(1),
                      alpha2     =rnorm(1),
                      alpha3     =rnorm(1),
                      alpha4     =rnorm(1),
                      alpha5     =rnorm(1),
                      alpha6     =rnorm(1),
                      alpha7     =rnorm(1),
                      alpha8     =rnorm(1),
                      alpha9     =rnorm(1),
                      alpha10    =rnorm(1),
                      alpha11    =rnorm(1),
                      alpha12    =rnorm(1),
                      alpha13    =rnorm(1),
                      p.rsrv.0=rnorm(1)
)}

#Otter.IDM_with_roadkill_TREND=run.jags(model=model_string,adapt = 25,
#                                     monitor= names(inits()[2:33]),
#                                     burnin=10000, sample=20000,thin = 4,
#                                     data=data, n.chains=4, method="rjags", inits=inits)

#save(Otter.IDM_with_roadkill_TREND,file="OtterIDM_with_roadkill_TREND.Rdata")

load("OtterIDM_with_roadkill_TREND.Rdata")


jpeg(filename = "Figures/Otter/Fig4BetaCoeff.jpg",width =6.5,height = 4,
     units = "in",res = 100)
MCMCplot(Otter.IDM_with_roadkill_TREND$mcmc,
         params =Otter.IDM_with_roadkill_TREND$monitor[c(1:7)],
        main = expression(paste("Otter intensity of use coefficients (",beta,")")),
        labels = c("Intercept","Stream","Wetland",
                   "Salt","Urban","Water","Time trend")
                   )
dev.off()
jpeg(filename = "Figures/Otter/Fig2AlphaDCoeff.jpg",width =6.5,height = 5.5,
     units = "in",res = 100)

MCMCplot(Otter.IDM_with_roadkill_TREND$mcmc,
         params =Otter.IDM_with_roadkill_TREND$monitor[c(18:31)],
         main = expression(paste("Otter detection coefficients in designed surveys (",alpha^D,")")),
         labels = c("Intercept","Observer 1","Observer 2",
         "Observer 3","Observer 4","Observer 5","Observer 6","Water",
         "Urban","Wetland","Temperature","Cloud cover",
         "Precipitation","Kayak")
)
dev.off()
png(filename = "Figures/Otter/Fig3AlphaRLCoeff.jpg",width =6,height = 8,
     units = "in",res = 100)
par(mfrow=c(2,1))
MCMCplot(Otter.IDM_with_roadkill_TREND$mcmc,
         params =Otter.IDM_with_roadkill_TREND$monitor[c(8,11:15)],
         main = expression(paste("Otter latrine detection coefficients (",alpha^L,")")),
         xlim = c(-5,15),
         labels = c("Intercept","Time trend","Distance to launch","Salt",
                    "Stream","Beaver surveys")
)
legend(x = "topleft",legend = "a",bty = "n", inset=c(-0.5,-.4),xpd=NA,cex=2)
MCMCplot(Otter.IDM_with_roadkill_TREND$mcmc,
         params =Otter.IDM_with_roadkill_TREND$monitor[c(9,10,16,17)],
         main = expression(paste("Otter roadkill detection coefficients (",alpha^R,")")),
         xlim = c(-5,15),
         labels = c("Intercept","Time trend","Presence of roads","Road density")
)
legend(x = "topleft",legend = "b",bty = "n", inset=c(-0.5,-.4),xpd=NA,cex=2)
dev.off()
MCMCplot(Otter.IDM_with_roadkill_TREND$mcmc,
         params =Otter.IDM_with_roadkill_TREND$monitor[c(7,10,11)]#,
         #main = "Covariates on otter roadkill detection",
         #labels = c("Intercept","Presence of roads","Road density")
)
#mcmcplot(Otter.IDM_with_roadkill_TREND)
#plot(Otter.IDM_with_roadkill_dyn,plot.type = c("trace","autocorr","histogram","ecdf"),
#     vars=names(inits()[c(2,8,11,12,16)]))
#latrines$beavereffort=NA
#for(i in 1:length(latrines$beavereffort)){
#  latrines$beavereffort[i]=beavereffort[match(latrines$kmu[i],km$id),
#                                        latrines$LCD[i]+1]
#}
#Otter.IDM_with_roadkill_dyn$timetaken/60
mcmc=data.frame(combine.mcmc(Otter.IDM_with_roadkill_TREND))
#mcmc01=data.frame(combine.mcmc(Otter.SDM))
#mcmc20=data.frame(combine.mcmc(Otter.occ.only))
#PredLam01=data.frame(matrix(NA, nrow=length(km$id),ncol=length(mcmc$lambda.0)))
#for(j in 1:length(km$id)){
#    PredLam01[j,]=mcmc$lambda.0+
#                   mcmc$lambdaStrm*COVS$BStrm[j]+
#                   mcmc$lambdaWet*wetland$N01_Wet[j]+
#                   mcmc$lambdaSalt*km$Salt[j]+
#                   mcmc$lambdaUrb*Urban$N01_Urb[j]+
#                   mcmc$lambdaWat*water$N01_Wtr[j]
#    print(j/2301)
#}
#km$medlam01=apply(X = PredLam01,MARGIN = 1,FUN = median)
#km$lcilam01=apply(X = PredLam01,MARGIN = 1,FUN = quantile,probs=0.025)
#km$ucilam01=apply(X = PredLam01,MARGIN = 1,FUN = quantile,probs=0.975)
#
#PredLam23=data.frame(matrix(NA, nrow=length(km$id),ncol=length(mcmc$lambda.0)))
#for(j in 1:length(km$id)){
#  PredLam23[j,]=mcmc$lambda.0+
#    mcmc$lambdaStrm*COVS$BStrm[j]+
#    mcmc$lambdaWet*wetland$N19_Wet[j]+
#    mcmc$lambdaSalt*km$Salt[j]+
#    mcmc$lambdaUrb*Urban$N19_Urb[j]+
#    mcmc$lambdaWat*water$N19_Wtr[j]+
#    mcmc$lambda.Time*24
#  print(j/2301)
#}
#km$medlam23=apply(X = PredLam23,MARGIN = 1,FUN = median)
#km$lcilam23=apply(X = PredLam23,MARGIN = 1,FUN = quantile,probs=0.025)
#km$ucilam23=apply(X = PredLam23,MARGIN = 1,FUN = quantile,probs=0.975)
#
#PredPsi99=1-exp(-exp(PredLam01))
#PredPsi23=1-exp(-exp(PredLam23))
#ChangeLam=PredLam23-PredLam01
#for(i in 1:2301){
#  ChangeLam[,i]=PredLam23[,i]-PredLam01[,i]
#  print(i/2301)
#}
#
#km$medPsi99=apply(X = PredPsi99,MARGIN = 1,FUN=median)
#km$medPsi23=apply(X = PredPsi23,MARGIN = 1,FUN=median)
#km$varPsi99=apply(X = PredPsi99,MARGIN = 1,FUN=var)
#km$varPsi23=apply(X = PredPsi23,MARGIN = 1,FUN=var)
#km$sd99=sqrt(km$varPsi99)
#km$sd23=sqrt(km$varPsi23)
#km$lamchange=apply(X = ChangeLam,MARGIN = 1,FUN=median)
#km$varchangelam=apply(X = ChangeLam,MARGIN = 1,FUN=var)
#km$sdchangelam=sqrt(km$varchangelam)
#
#save(ChangeLam,PredLam23,PredPsi23,PredLam01,PredPsi99,km,file="OtterPredictions.Rdata")
load("OtterPredictions.Rdata")
km$change=km$medPsi23-km$medPsi99
plot(km["medPsi99"],main = "Predicted probability of river otter occupancy in 1999",
     breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),
     at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
plot(km["medPsi23"],main = "Predicted probability of river otter occupancy in 2023",
     breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),
     at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
plot(km["sd99"],main = "Standard deviation of predicted probability of river otter occupancy in 1999",
     breaks=c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.2),
     at=c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.2))
plot(km["sd23"],main = "Standard deviation of predicted probability of river otter occupancy in 2023",
     breaks=c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.2),
     at=c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.2))

plot(km["lamchange"],main="Change in otter intensity of use 1999-2023")
plot(km["sdchangelam"],main="S.d. of change in otter intensity of use 1999-2023")
km$varlam99=apply(X = PredLam01,MARGIN = 1,FUN=var)
km$sdlam99=sqrt(km$varlam99)
km$varlam23=apply(X = PredLam23,MARGIN = 1,FUN=var)
km$sdlam23=sqrt(km$varlam23)

png(filename = "Figures/Otter/Fig6c.png",width =4,height = 6,
    units = "in",res = 150)
plot(km["lamchange"],main=NULL)
dev.off()
png(filename = "Figures/Otter/Fig6d.png",width =4,height = 6,
    units = "in",res = 150)
plot(km["sdchangelam"],
     main=NULL)
dev.off()
png(filename = "Figures/Otter/Fig6a.png",width =4,height = 6,
    units = "in",res = 150)
plot(km["medPsi23"],main=NULL,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),
     at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1) )
dev.off()
png(filename = "Figures/Otter/Fig6b.png",width =4,height = 6,
    units = "in",res = 150)
plot(km["sdlam23"],main=NULL)
dev.off()

plot(km["Psi01"])

expit=function(x){(exp(x))/(1+exp(x))}
expit(median(mcmc$p.rsrv.0))

#####
#Look at medians and HPDI
#####
HPD=cbind(apply(mcmc,2,median),
          HPDinterval(combine.mcmc(Otter.IDM_with_roadkill_TREND)))
HPD=as.data.frame(t(HPD))
rownames(HPD)=c("median","lower","upper")
#####
#Graphing by site type
#####
x=seq(0:24)
LMedNsaNst=HPD$lambda.0[1]+HPD$lambda.Time[1]*x
LLCINsaNst=HPD$lambda.0[2]+HPD$lambda.Time[2]*x
LUCINsaNst=HPD$lambda.0[3]+HPD$lambda.Time[3]*x
xyear=1999:2023

xLC=seq(0,1,100.1/100000)
xurb=(xLC-mean(km$N01_Urb))/sqrt(var(km$N01_Urb))
yUrbM=1-exp(-exp(HPD$lambda.0[1]+HPD$lambdaUrb[1]*xurb))
yUrbL=1-exp(-exp(HPD$lambda.0[2]+HPD$lambdaUrb[2]*xurb))
yUrbU=1-exp(-exp(HPD$lambda.0[3]+HPD$lambdaUrb[3]*xurb))

xWtr=(xLC-mean(km$N01_Wtr))/sqrt(var(km$N01_Wtr))
yWtrM=1-exp(-exp(HPD$lambda.0[1]+HPD$lambdaWat[1]*xWtr))
yWtrL=1-exp(-exp(HPD$lambda.0[2]+HPD$lambdaWat[2]*xWtr))
yWtrU=1-exp(-exp(HPD$lambda.0[3]+HPD$lambdaWat[3]*xWtr))

xWet=(xLC-mean(km$N01_Wet))/sqrt(var(km$N01_Wet))
yWetM=1-exp(-exp(HPD$lambda.0[1]+HPD$lambdaWet[1]*xWet))
yWetL=1-exp(-exp(HPD$lambda.0[2]+HPD$lambdaWet[2]*xWet))
yWetU=1-exp(-exp(HPD$lambda.0[3]+HPD$lambdaWet[3]*xWet))

png(filename = "Figures/Otter/Fig5PsiCovs.png",width =6.5,height = 8,
     units = "in",res = 150)
par(mfrow=c(2,2))
plot(xyear,1-exp(-exp(LMedNsaNst)),type = "l",ylim=c(0,1),col="black",
     xlab = "Year",ylab = "Predicted probability of occupancy")
polygon(c(xyear,rev(xyear)),
        c(1-exp(-exp(LLCINsaNst)),rev(1-exp(-exp(LUCINsaNst)))),
        col=adjustcolor("black",0.2))
legend(x = "topleft",legend = "a",cex=2,bty = "n",inset = c(-0.475,-0.2),xpd = NA)
plot(xLC,yUrbM,type = "l",ylim=c(0,1),
     xlab="Proportion of site covered by urban areas",
     ylab="Predicted probability of otter occupancy")
polygon(c(xLC,rev(xLC)),c(yUrbL,rev(yUrbU)),col=adjustcolor("black",0.2))
legend(x = "topleft",legend = "b",cex=2,bty = "n",inset = c(-0.475,-0.2),xpd = NA)
plot(xLC,yWtrM,type = "l",ylim=c(0,1),
     xlab="Proportion of site covered by water",
     ylab="Predicted probability of otter occupancy")
polygon(c(xLC,rev(xLC)),c(yWtrL,rev(yWtrU)),col=adjustcolor("black",0.2))
legend(x = "topleft",legend = "c",cex=2,bty = "n",inset = c(-0.475,-0.2),xpd = NA)
plot(xLC,yWetM,type = "l",ylim=c(0,1),
     xlab="Proportion of site covered by wetland",
     ylab="Predicted probability of otter occupancy")
polygon(c(xLC,rev(xLC)),c(yWetL,rev(yWetU)),col=adjustcolor("black",0.2))
legend(x = "topleft",legend = "d",cex=2,bty = "n",inset = c(-0.475,-0.2),xpd = NA)
dev.off()
#Calculating probabilities of support
for(i in 1:32){
  print(colnames(mcmc)[i])
  print(length(which(mcmc[,i]>0))/90000)
}
#Some Graphs
png(filename = "Figures/Otter/Fig1Histos.png",width =6.5,height = 9,
    units = "in",res = 150)
par(mfrow=c(2,1))
hist(roadkill$year,xlab = "Year",main=NULL)
legend(x = "topleft",legend = "a",bty = "n",cex=2,inset = c(-0.25,-0.25),xpd = NA)
hist(latrines$year,xlab = "Year",main=NULL)
legend(x = "topleft",legend = "b",bty = "n",cex=2,inset = c(-0.25,-0.25),xpd = NA)
dev.off()
