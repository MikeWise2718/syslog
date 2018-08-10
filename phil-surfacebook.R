library(lubridate)
library(ggplot2)
library(scales)

# ldf <- read.csv("Failure Log August 1.csv",row.names=NULL,encoding = "UTF-8-BOM",stringsAsFactors=F)
ldf <- read.csv("Failure Log from 10.8..csv",row.names=NULL,encoding = "UTF-8-BOM",stringsAsFactors=F)
names(ldf) <-c( "Level","DateTime","Source","EventId","TaskCat","Desc")
ldf$dt <- dmy_hms(ldf$DateTime)

ldf$vval <- 1
ldf$vsize <- 1
ldf$EventType = "normal"
ldf$EventType[grepl("rebooted without cleanly shutting down first",ldf$Desc)] <- "unexpected reboot"
ldf$vsize[grepl("rebooted without cleanly shutting down first",ldf$Desc)] <- 2

edate <- ldf$dt[1]
sdate <- ldf$dt[nrow(ldf)]
if (edate<sdate)
{
  tmp = edate
  edate = sdate
  sdate = tmp
}
ldf$week <- week(ldf$dt)
ldf$wday  <- wday(ldf$dt)
ldf$hourtime = as.numeric(ldf$dt) %% (24*60*60) / 3600


dow <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
ldf$wday <- factor(dow[ldf$wday],levels=dow)

lrdf <- ldf[ldf$vlinecolor=="red",]

mtit <- "Event log"
xlab <- "Hour"
ylab <- ""


# Add date in upper right
ndays <- difftime(edate,sdate,"days")
days <-  0:ndays
dt <- sdate + days*3600*24
txtdate <- as.Date(dt, format = "%Y-%m-%d")
ddf <- data.frame(dt=dt,txtdate=txtdate)
ddf$week <- week(ddf$dt)
ddf$wday  <- wday(ddf$dt)
ddf$hourtime <- 20
ddf$vval <- 1.8
ddf$wday <- factor(dow[ddf$wday],levels=dow)

gp <- ggplot(ldf,aes(x=hourtime,y=vval))  +
      xlab(xlab) + ylab(ylab) + ggtitle(mtit) +
      theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
      geom_point(aes(color=EventType,size=EventType))  +
      geom_text(data=ddf,aes(x=hourtime,y=vval,label=txtdate)) +
      scale_color_manual(values=c( "normal"="darkgreen","unexpected reboot"="red") ) +
      scale_size_manual(values=c( "normal"=1,"unexpected reboot"=4) ) +
      guides( size = FALSE) +
      ylim(0,2) +
      facet_grid ( week ~ wday  )
      
print(gp)


# print(doplot(ldf,"hourtime","vval",vlines=lrdf$dt,EventType=ldf$EventType,ylab="rep-points",backg="lightsteelblue1",sdate=sdate,edate=edate))
