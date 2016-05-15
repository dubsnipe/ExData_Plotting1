testopen2<-function(afile){
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
  
  png(filename = "plot2.png",
      width = 480, height = 480, units = "px")
  
  with(usabledata,plot(dateandtime,Global_active_power,xlab=NA,ylab="Global Active Power (kilowatts)",type="n"))
  with(usabledata,lines(dateandtime,Global_active_power,xlab=NA,ylab="Global Active Power (kilowatts)"))
  
  dev.off()
}