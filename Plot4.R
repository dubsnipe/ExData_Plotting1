testopen4<-function(afile){
  library(lubridate)
  library(dplyr)
  atable<-read.table(file=afile,header=TRUE,sep=";")
  thetable<-tbl_df(atable)
  thetable$Date<-dmy(thetable$Date)
  usabledata<-filter(thetable, Date>="2007-02-01" & Date<="2007-02-02")
  for(j in 3:9){
    usabledata[j][usabledata[j]=="?"]<-NA
  }
  
  usabledata<-mutate(usabledata,Global_active_power=as.numeric(as.character(usabledata$Global_active_power)),dateandtime=as.POSIXct(ymd_hms(paste(Date," ",Time))))

  png(filename = "plot4.png",
      width = 480, height = 480, units = "px")
  
  par(mfcol=c(2,2))
  with(usabledata,plot(dateandtime,Global_active_power,xlab=NA,ylab="Global Active Power (kilowatts)",type="n"))
  with(usabledata,lines(dateandtime,Global_active_power,xlab=NA,ylab="Global Active Power (kilowatts)"))
  with(usabledata,plot(dateandtime,usabledata$Sub_metering_1,ylim=c(0,38),xlab=NA,ylab="Energy sub metering",type="n"))
  with(usabledata,lines(dateandtime,as.character(Sub_metering_1),xlab=NA))
  with(usabledata,lines(dateandtime,as.character(Sub_metering_2),xlab=NA,col="red"))
  with(usabledata,lines(dateandtime,as.character(Sub_metering_3),xlab=NA,col="blue"))
  legend("topright",bty = "n",lty=c(1,1),col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  with(usabledata,plot(dateandtime,as.character(Voltage),xlab="datetime",ylab="Voltage",type="n"))
  with(usabledata,lines(dateandtime,as.character(Voltage)))
  with(usabledata,plot(dateandtime,as.character(Global_reactive_power),ylab="Global_reactive_power",xlab="datetime",type="n"))
  with(usabledata,lines(dateandtime,as.character(Global_reactive_power)))
  
  dev.off()
}