setwd("~/Desktop/NBAdata")
load("nba1.rda")
load("nba.rda")

nba$DATE<-as.Date(nba$DATE, "%m/%d/%y" )

gamepoints <- aggregate(nba$ PTS, list(DATE=nba$DATE, TEAM=nba$OWN.TEAM), sum)
gamepoints1 <- aggregate(nba$ PTS, list(DATE=nba$DATE, TEAM=nba$OPP.TEAM), sum)
names(gamepoints) <- c("DATE", "OWN.TEAM", "TOTAL.POINTS")
names(gamepoints1) <- c("DATE", "OWN.TEAM", "TOTAL.POINTS.OPP")
teamposs <- aggregate(nba$POSS, list(DATE=nba$DATE, TEAM=nba$OWN.TEAM), sum)
teamposs1 <- aggregate(nba$POSS, list(DATE=nba$DATE, TEAM=nba$OPP.TEAM), sum)
names(teamposs) <- c("DATE", "OWN.TEAM", "TOTAL.POSS")
names(teamposs1) <- c("DATE", "OWN.TEAM", "TOTAL.POSS.OPP") 
teamMIN <- aggregate(nba$MIN, list(DATE=nba$DATE, TEAM=nba$OWN.TEAM), sum)
names(teamMIN) <- c("DATE", "OWN.TEAM", "TOTAL.MIN")
teamassists <- aggregate(nba$A, list(DATE=nba$DATE, OWN.TEAM=nba$OWN.TEAM), sum)
names(teamassists) <- c("DATE", "OWN.TEAM", "TOTAL.ASSISTS")
teamFGs <- aggregate(nba$FG, list(DATE=nba$DATE, OWN.TEAM=nba$OWN.TEAM), sum)
names(teamFGs) <- c("DATE", "OWN.TEAM", "TOTAL.FG")
nba <- merge(teamMIN, nba, by= c("DATE", "OWN.TEAM"))
nba <- merge(teamposs, nba, by= c("DATE", "OWN.TEAM"))
nba <- merge(teamposs1, nba, by= c("DATE", "OWN.TEAM"))
nba <- merge(gamepoints, nba, by = c("DATE", "OWN.TEAM"))
nba <- merge(gamepoints1, nba, by = c("DATE", "OWN.TEAM"))
nba <- merge(teamassists, nba, by = c("DATE", "OWN.TEAM"))
nba <- merge(teamFGs, nba, by = c("DATE", "OWN.TEAM"))

na.omit(nba)
nba<-nba[nba$MIN!= 0, ]
nba$pace <- (48 * (nba$TOTAL.POSS  + nba$TOTAL.POSS.OPP)) / (2 * (nba$TOTAL.MIN / 5))


WorL<-nba$TOTAL.POINTS > nba$TOTAL.POINTS.OPP
WorL[WorL==TRUE] <- "W"
WorL[WorL==FALSE] <- "L"
nba$WinLoss <- WorL
names(WorL) <- c("Win/Loss")


nbaS1 <- subset(nba, nba$DATA.SET=="2008-2009 Regular Season")
nbaS2 <- subset(nba, nba$DATA.SET=="2009-2010 Regular Season")
nbaS3 <- subset(nba, nba$DATA.SET=="2010-2011 Regular Season")
nbaS4 <- subset(nba, nba$DATA.SET=="2011-2012 Regular Season")
nbaS5 <- subset(nba, nba$DATA.SET=="2012-2013 Regular Season")

listnba <- list(nbaS1, nbaS2, nbaS3, nbaS4, nbaS5)

uniquepace <- function(i){
  x <- unique(i$pace)
  y <- mean(x)
}

seasonpace <- sapply(listnba, uniquepace) 

Pace1 <- rep(seasonpace[1],nrow(nbaS1))
nbaS1$LPace <- Pace1
Pace2 <- rep(seasonpace[2],nrow(nbaS2))
nbaS2$LPace <- Pace2 
Pace3 <- rep(seasonpace[3],nrow(nbaS3))
nbaS3$LPace <- Pace3 
Pace4 <- rep(seasonpace[4],nrow(nbaS4))
nbaS4$LPace <- Pace4 
Pace5 <- rep(seasonpace[5],nrow(nbaS5))
nbaS5$LPace <- Pace5

listnba <- list(nbaS1, nbaS2, nbaS3, nbaS4, nbaS5) 


AST <- function(x) {
  y=sum(x$A)
  return(y)
}
FG <- function(x) {
  y = sum(x$FG)
  return(y)
}

FT <- function(x) {
  y= sum(x$FT)
  return(y)
}

PTS <- function(x) {
  y= sum(x$PTS)
}

FGA <- function(x) {
  y= sum(x$FGA)
  return(y)
}

ORB <- function(x) {
  y= sum(x$OR)
  return(y)
}

TOV <- function(x) {
  y= sum(x$TO)
  return(y)
}

FTA <- function(x) {
  y= sum(x$FTA)
  return(y)
}

TORB <- function(x) {
  y= sum(x$TOT)
  return(y)
}

PF <- function(x) {
  y= sum(x$PF)
  return(y)
}

lg_AST <- lapply(listnba, AST)
lg_FG <- lapply(listnba, FG)
lg_FT <- lapply(listnba, FT)
lg_PTS <- lapply(listnba, PTS)
lg_TOV <- lapply(listnba, TOV)
lg_FGA <- lapply(listnba, FGA)
lg_FTA <- lapply(listnba, FTA)
lg_OR <- lapply(listnba, ORB)
lg_TOT <- lapply(listnba, TORB)
lg_PF <- lapply(listnba, PF)


#Season 1


 #uPER1 = (1 / nbaS1$MIN) * (nbaS1$X3P
  # + (2/3) * nbaS1$AST
  # + (2 - factor * (nbaS1$TOTAL.ASSISTS / nbaS1$TOTAL.FG)) * nbaS1$FG
  # + (nbaS1$FT *0.5 * (1 + (1 - (nbaS1$TOTAL.ASSISTS / nbaS1$TOTAL.FG)) + (2/3) * (nbaS1$TOTAL.ASSISTS / nbaS1$TOTAL.FG)))
  # - VOP * nbaS1$TO
  # - VOP * DRB * (nbaS1$FGA - nbaS1$FG)
  # - VOP * 0.44 * (0.44 + (0.56 * DRB)) * (nbaS1$FTA - nbaS1$FT)
  # + VOP * (1 - DRB) * (nbaS1$TOT - nbaS1$OR)
  # + VOP * DRB * nbaS1$OR
  # + VOP * nbaS1$ST
  # + VOP * DRB * nbaS1$BL
  # - nbaS1$PF * ((lg_FT[[1]] / lg_PF[[1]]) - 0.44 * (lg_FTA[[1]] / lg_PF[[1]]) * VOP))

uPER <- function(i){ 

                          factor = (2 / 3) - (0.5 * (lg_AST[[i]] / lg_FG[[i]])) / (2 * (lg_FG[[i]] / lg_FT[[i]]))
                          VOP    = lg_PTS[[i]] / (lg_FGA[[i]] - lg_OR[[i]] + lg_TOV[[i]] + 0.44 * lg_FTA[[i]])
                          DRB   = (lg_TOT[[i]] - lg_OR[[i]]) / lg_TOT[[i]]
                             
                           y = ((1 / listnba[[i]]$MIN) * (listnba[[i]]$X3P
                           + ((2/3) * listnba[[i]]$A)
                           + (((2 - factor * (listnba[[i]]$TOTAL.ASSISTS / listnba[[i]]$TOTAL.FG))) * listnba[[i]]$FG)
                           + (listnba[[i]]$FT *0.5 * (2 - (listnba[[i]]$TOTAL.ASSISTS / listnba[[i]]$TOTAL.FG) + (2/3) * (listnba[[i]]$TOTAL.ASSISTS / listnba[[i]]$TOTAL.FG)))
                           - (VOP * listnba[[i]]$TO)
                           - (VOP * DRB * (listnba[[i]]$FGA - listnba[[i]]$FG))
                           - (VOP * 0.44 * (0.44 + (0.56 * DRB)) * (listnba[[i]]$FTA - listnba[[i]]$FT))
                           + (VOP * (1 - DRB) * (listnba[[i]]$TOT - listnba[[i]]$OR))
                           + (VOP * DRB * listnba[[i]]$OR)
                           + (VOP * listnba[[i]]$ST)
                           + (VOP * DRB * listnba[[i]]$BL)
                           - (listnba[[i]]$PF * ((lg_FT[[i]] / lg_PF[[i]]) - 0.44 * (lg_FTA[[i]] / lg_PF[[i]]) * VOP))))
                      return(y)
}

listnbaU <-lapply(1:5, uPER)

listnba[[1]]$uPER <- listnbaU[[1]]
listnba[[2]]$uPER <- listnbaU[[2]]
listnba[[3]]$uPER <- listnbaU[[3]]
listnba[[4]]$uPER <- listnbaU[[4]]
listnba[[5]]$uPER <- listnbaU[[5]]

nba2 <- rbind(listnba[[1]],listnba[[2]],listnba[[3]],listnba[[4]],listnba[[5]])

nba2$PaceAdj <- nba2$LPace/nba2$pace
nba2$aPER <- nba2$PaceAdj*nba2$uPER

nbaS1 <- subset(nba2, nba2$DATA.SET=="2008-2009 Regular Season")
nbaS2 <- subset(nba2, nba2$DATA.SET=="2009-2010 Regular Season")
nbaS3 <- subset(nba2, nba2$DATA.SET=="2010-2011 Regular Season")
nbaS4 <- subset(nba2, nba2$DATA.SET=="2011-2012 Regular Season")
nbaS5 <- subset(nba2, nba2$DATA.SET=="2012-2013 Regular Season")

listnba2 <- list(nbaS1, nbaS2, nbaS3, nbaS4, nbaS5) 

uniquepace1 <- function(i){
  x <- unique(i$aPER)
  y <- mean(x)
}

LaPER <- sapply(listnba2, function(x){mean(x$aPER)}) #this is not correct. almost because you have values appearing multiple times for each player. 

Pace1 <- rep(LaPER[1],nrow(nbaS1))
nbaS1$LaPER <- Pace1
Pace2 <- rep(LaPER[2],nrow(nbaS2))
nbaS2$LaPER <- Pace2 
Pace3 <- rep(LaPER[3],nrow(nbaS3))
nbaS3$LaPER <- Pace3 
Pace4 <- rep(LaPER[4],nrow(nbaS4))
nbaS4$LaPER <- Pace4 
Pace5 <- rep(LaPER[5],nrow(nbaS5))
nbaS5$LaPER <- Pace5

nba3 <- rbind(nbaS1,nbaS2,nbaS3,nbaS4,nbaS5)
nba3$PER <- nba3$aPER*(15/nba3$LaPER)

nbaS1 <- subset(nba3, nba3$DATA.SET=="2008-2009 Regular Season")
nbaS2 <- subset(nba3, nba3$DATA.SET=="2009-2010 Regular Season")
nbaS3 <- subset(nba3, nba3$DATA.SET=="2010-2011 Regular Season")
nbaS4 <- subset(nba3, nba3$DATA.SET=="2011-2012 Regular Season")
nbaS5 <- subset(nba3, nba3$DATA.SET=="2012-2013 Regular Season")

test <- subset(nbaS1,nbaS1$PLAYER == "L.James")
mean(test$PER)

library(MASS)

lebron <- subset(nba3,nba3$PLAYER == "L.James")
head(lebron)

plot(PER~DATE,data=lebron, main="PER Per Game, Every Season, Lebron James")

kobe <- subset(nba3,nba3$PLAYER == "K.Bryant")

plot(PER~DATE,data=kobe, main="PER Per Game, Every Season, Kobe Bryant")


Manu <- subset(nba3,nba3$PLAYER == "M.Ginobili")

plot(PER~DATE,data=Manu, main="PER Per Game, Every Season, Manu Ginobili")

# Chop off players who didn't play for more than 20 min as there season average:

avgMin <- aggregate(nba3$MIN, list(PLAYER=nba3$PLAYER, DATA.SET=nba3$DATA.SET), mean)
names(avgMin) <- c("PLAYER", "DATA.SET", "AVGMIN")
nbaPERanalysis <- merge(avgMin, nba3, by= c("PLAYER", "DATA.SET"))
nba.chop20 <- subset(nbaPERanalysis, nbaPERanalysis$AVGMIN > 20)

#differences of PERS by position. 
#consistency of PERS for players.  
#General understanding of PER per game means. 
#Estimating or predicting something....

#top 5 players with best avg PERs playing over 20 minutes...season by season
#way i did it if a player got injured haflway through and played over 20 min a game and then couldnt cuz of injury they wouldn't be included...set a minimum number of games perhaps played twenty min. 

topPER <- aggregate(nba.chop20$PER, list(Player=nba.chop20$PLAYER, SEASON=nba.chop20$DATA.SET), mean)

topPERS1 <- subset(topPER, topPER$SEASON == "2008-2009 Regular Season")

topOrder <- order(topPERS1$x, decreasing=TRUE)[1:5]
topPERS1 <- topPERS1[topOrder,]

a <- topPERS1

topPERS2 <- subset(topPER, topPER$SEASON == "2009-2010 Regular Season")

topOrder <- order(topPERS2$x, decreasing=TRUE)[1:5]
topPERS2 <- topPERS2[topOrder,]

b <- topPERS2

topPERS3 <- subset(topPER, topPER$SEASON == "2010-2011 Regular Season")

topOrder <- order(topPERS3$x, decreasing=TRUE)[1:5]
topPERS3 <- topPERS3[topOrder,]

c <- topPERS3

topPERS4 <- subset(topPER, topPER$SEASON == "2011-2012 Regular Season")

topOrder <- order(topPERS4$x, decreasing=TRUE)[1:5]
topPERS4 <- topPERS4[topOrder,]

d <- topPERS4

topPERS5 <- subset(topPER, topPER$SEASON == "2012-2013 Regular Season")

topOrder <- order(topPERS5$x, decreasing=TRUE)[1:5]
topPERS5 <- topPERS5[topOrder,]

e <- topPERS5

best <- rbind(a,b,c,d,e)
rownames(best) <- 1:nrow(best)

Player <- c("James","Paul","Wade","Howard","Duncan","James","Wade","Durant","Bosh","Duncan","James","Howard","Wade","Paul","Rose","James","Paul","Wade","Durant","Love","James","Durant","Paul","Anthony","Wade")
Season <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5)
topP <- best[,3]

test <- data.frame(Player,Season,topP)

library(lattice)

xyplot(topP ~ Season, test, groups = Player,auto.key=list(columns=4),main="Top PER by Season by Player",xlab="Season",ylab="PER", type='l')
xyplot(topP ~ Season, test, groups = Player,auto.key=list(columns=4),main="Top PER by Season by Player",xlab="Season",ylab="PER")

#Team with the highest average PER per game...did they win everytime? Measure of how good our statistic is. 

teamPER <- aggregate(nba3$PER, list(DATE=nba3$DATE, TEAM=nba3$OWN.TEAM), mean)
teamPER1 <- aggregate(nba3$PER, list(DATE=nba3$DATE, TEAM=nba3$OPP.TEAM), mean)
names(teamPER) <- c("DATE", "OWN.TEAM", "gamePER")
names(teamPER1) <- c("DATE", "OWN.TEAM", "gamePER.OPP")
nba3avgPER <- merge(teamPER, nba3, by = c("DATE", "OWN.TEAM"))
nba3avgPER <- merge(teamPER1, nba3avgPER, by = c("DATE", "OWN.TEAM"))

PerTest<-nba3avgPER$gamePER > nba3avgPER$gamePER.OPP
PerTest[PerTest==TRUE] <- "W"
PerTest[PerTest==FALSE] <- "L"

WintoPer <- PerTest == nba3avgPER$WinLoss

#76% of games that are Won..the team has an average PER that is higher than the other team.

#players playing over twenty minutes with the most variation in PER top 5...per season? Over five season? 

#doing SD change the name and change all the functions as well from VAR to SD 
varPER <- aggregate(nba.chop20$PER, list(Player=nba.chop20$PLAYER, SEASON=nba.chop20$DATA.SET), sd)

topVARS1 <- subset(varPER, varPER$SEASON == "2008-2009 Regular Season")

varOrder <- order(topVARS1$x, decreasing=TRUE)[1:5]
topVARS1 <- topVARS1[varOrder,]

a <- topVARS1

topVARS2 <- subset(varPER, varPER$SEASON == "2009-2010 Regular Season")

varOrder <- order(topVARS2$x, decreasing=TRUE)[1:5]
topVARS2 <- topVARS2[topOrder,]

b <- topVARS2

topVARS3 <- subset(varPER, varPER$SEASON == "2010-2011 Regular Season")

varOrder <- order(topVARS3$x, decreasing=TRUE)[1:5]
topVARS3 <- topVARS3[varOrder,]

c <- topVARS3

varPERS4 <- subset(varPER, varPER$SEASON == "2011-2012 Regular Season")

varOrder <- order(varPERS4$x, decreasing=TRUE)[1:5]
varPERS4 <- varPERS4[varOrder,]

d <-  varPERS4

varPERS5 <- subset(varPER, varPER$SEASON == "2012-2013 Regular Season")

varOrder <- order(varPERS5$x, decreasing=TRUE)[1:5]
varPERS5 <- varPERS5[varOrder,]

e <- varPERS5

goofs <- c("Williams","Dunleavy","Young","Ginobili","Barbosa","Scola","Smith","Delfino","Udrih","Derozan", "Anderson", "Gee","Bledsoe","Farmar","Thornton","Haywood","Johnson","Hudson","Thomas","Uzoh","Clark","Miles","Bayless","Meeks","Ginobili")
vars <- standard[,3]

standard <- rbind(a, b, c, d, e)

turrible <- data.frame(goofs,Season,vars)
xyplot(vars ~ Season, turrible, groups = goofs,auto.key=list(columns=4),main="TOP VAR by Season by Player",xlab="Season",ylab="SD")

#for players playing over 20 minutes, plot PER versus number of points scored...is there some sort of correlation? Do this per season? 

X11()
par(mfrow=c(3,2))

PointVSper1 <- subset(nba.chop20, nba.chop20$DATA.SET =="2008-2009 Regular Season")
plot(PointVSper1$PER~PointVSper1$PTS, ylim=c(-20, 110), xlab="Points", ylab="PER", main="PointsVSPER 08-09 Season")

PointVSper2 <- subset(nba.chop20, nba.chop20$DATA.SET =="2009-2010 Regular Season")
plot(PointVSper2$PER~PointVSper2$PTS, ylim=c(-20, 110), xlab="Points", ylab="PER", main="PointsVSPER 09-10 Season")

PointVSper3 <- subset(nba.chop20, nba.chop20$DATA.SET =="2010-2011 Regular Season")
plot(PointVSper3$PER~PointVSper3$PTS, ylim=c(-20, 110), xlab="Points", ylab="PER", main="PointsVSPER 10-11 Season")

PointVSper4 <- subset(nba.chop20, nba.chop20$DATA.SET =="2011-2012 Regular Season")
plot(PointVSper4$PER~PointVSper4$PTS, ylim=c(-20, 110), xlab="Points", ylab="PER", main="PointsVSPER 11-12 Season")

PointVSper5 <- subset(nba.chop20, nba.chop20$DATA.SET =="2012-2013 Regular Season")
plot(PointVSper5$PER~PointVSper5$PTS, ylim=c(-20, 110), xlab="Points", ylab="PER", main="PointsVSPER 12-13 Season")

 

GetInfo <- function(i, s){
y <- subset(nba3, nba3$DATA.SET == s)
x <- subset(y, y$PLAYER==i)


return(list("info"=x,"stats"=y,z))
}

par(mfrow=c(1,2))
Lebron1 <- subset(nba3, nba3$DATA.SET=="2008-2009 Regular Season" & nba3$PLAYER=="L.James")
plot(Lebron1$PER, type="b", main="LEBRON PER per Game 2008-2009 Season")
x <- c(1:81)
pred <- loess(Lebron1$PER~x)
lines(predict(pred), col='red', lwd=2)

Lebron2 <- subset(nba3, nba3$DATA.SET=="2012-2013 Regular Season" & nba3$PLAYER=="L.James")
plot(Lebron2$PER, type="b", main="LEBRON PER per Game 2012-2013 Season")
z <- c(1:76)
pred <- loess(Lebron2$PER~z)
lines(predict(pred), col='red', lwd=2)


par(mfrow=c(1,2))
Duncan1 <- subset(nba3, nba3$DATA.SET=="2008-2009 Regular Season" & nba3$PLAYER=="T.Duncan")
plot(Duncan1$PER, type="b", main="Duncan PER per Game 2008-2009 Season")
x <- c(1:75)
pred <- loess(Duncan1$PER~x)
lines(predict(pred), col='red', lwd=2)

Duncan2 <- subset(nba3, nba3$DATA.SET=="2012-2013 Regular Season" & nba3$PLAYER=="T.Duncan")
plot(Duncan2$PER, type="b", main="Duncan PER per Game 2012-2013 Season")
z <- c(1:69)
pred <- loess(Duncan2$PER~z)
lines(predict(pred), col='red', lwd=2)

par(mfrow=c(1,2))
Gin1 <- subset(nba3, nba3$DATA.SET=="2008-2009 Regular Season" & nba3$PLAYER=="M.Ginobili")
plot(Gin1$PER, type="b", main="Ginobili PER per Game 2008-2009 Season")
x <- c(1:44)
pred <- loess(Gin1$PER~x)
lines(predict(pred), col='red', lwd=2)

Gin2 <- subset(nba3, nba3$DATA.SET=="2012-2013 Regular Season" & nba3$PLAYER=="M.Ginobili")
plot(Gin2$PER, type="b", main="Ginobili PER per Game 2012-2013 Season")
z <- c(1:60)
pred <- loess(Gin2$PER~z)
lines(predict(pred), col='red', lwd=2)

#rookie of the year 

Lillard <- subset(nba3, nba3$DATA.SET=="2012-2013 Regular Season" & nba3$PLAYER=="D.Lillard")
plot(Lillard$PER, type="b", main="Lillard PER per Game 2012-2013 Season")
z <- c(1:82)
pred <- loess(Lillard$PER~z)
lines(predict(pred), col='red', lwd=2)

Davis <- subset(nba3, nba3$DATA.SET=="2012-2013 Regular Season" & nba3$PLAYER=="A.Davis")
plot(Davis$PER, type="b", main="Davis PER per Game 2012-2013 Season")
z <- c(1:64)
pred <- loess(Davis$PER~z)
lines(predict(pred), col='red', lwd=2)

boxplot(PER~factor(PLAYER),data=nbaS5[nbaS5$OWN.TEAM=="Mia",], las=2, main="Box and Whiskers for each player in 2012-2013 Season Miami Heat")

#Stretch but autocorrelations in the PER for Lebron...when he gets hot does he get really hot? 

plot(Lebron2$PER, type="b", main="LEBRON PER per Game 2012-2013 Season")
diffleb <- diff(Lebron2$PER, lag=1)
plot(diffleb, type="l", main="Difference Data")
par(mfrow=c(1,2))
acf(diffleb)
pacf(diffleb)
ARIMA1 =  auto.arima(diffleb, ic = "aicc")

#best series is an MA1 

arima <- arima.sim(list(order=c(0,0,1), ma=-.7629), n=75)
par(mfrow=c(2,1))
plot(arima, main="Arima Model MA(1)")
plot(diffleb, type="l", main="Difference Data")