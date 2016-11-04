library(intraDay)
library(httr)
options(stringsAsFactors=F)

#' loads in master file
#'
#' @param
#' @keywords load masterfile
#' @export
#' @examples
#' readCSV()
readCSV<-function(){
  allEnv <- Sys.getenv("ENV")
  
  if(allEnv=='DEV') data<-read.csv(file=file.choose())
  else data<-read.csv('/home/opencpu/R/HK.IntraDayProcessed.csv')
  
  if(grepl("/",data$DATE[1])) data$DATE<-as.Date(data$DATE,"%d/%m/%Y")
  return(data)
}


#options(stringsAsFactors=F)
#loads in the raw data
#set working directory for the lookup data file


#' calculates expected finish position given matrix position and price band
#'
#' @param dto distance course mtx odds parameters used for expected finish order
#' @keywords expected finish order
#' @export
#' @examples
#' expFin()
expFin<-function(data,dto,distance,course,mtx,odds,track){
  filter<-data$DATE<as.Date(dto) & data$DISTANCE==distance & data$COURSE==course & data$win_price_band==odds & data$mtx==mtx & !is.na(data$mtx) & is.finite(data$DISTANCE) & is.finite(data$win_price_band) & data$TRACK==track
  a<-data[filter,]
  if(is.numeric(mtx) | is.numeric(odds)) avg.fin<-mean(a$finish_order,na.rm=T)
  else avg.fin<-NA
  return(avg.fin)
}

#' price bandings based upon SPSS bandings
#' #'
#' @param odds allocates price bands
#' @keywords pricebands
#' @export
#' @examples
#' priceBandAlloc()
priceBandAlloc<-function(odds){
  if(is.na(odds)) return(NA)
  else if(odds<=4.4) return(1)
  else if(odds>4.5 & odds<=6.5) return(2)
  else if(odds>6.6 & odds<=8.9) return(3)
  else if(odds>9.0 & odds<=11.0) return(4)
  else if(odds>11.1 & odds<=15.0) return(5)
  else if(odds>15.1 & odds<=21.0) return(6)
  else if(odds>21.1 & odds<=31.0) return(7)
  else if(odds>31.1 & odds<=49.0) return(8)
  else if(odds>49.1 & odds<=96.0) return(9)
  else return(10)
}

#' allocates track type (AWT/POLY/Turf)
#' #'
#' @param track track type
#' @keywords tracktype
#' @export
#' @examples
#' trackType('POLYTRACK') 
trackType<-function(track){
  if(grepl('AWT',track)) return('AWT')
  else if(grepl('POLYTRACK',track)) return('POL')
  else return('TURF')
}

#' calculates error between actual vs expected
#'
#' @param actual exp calcs difference between actual and expected
#' @keywords error
#' @export
#' @examples
#' errorCalc()
errorCalc<-function(actual,exp){
  if(is.finite(actual) & is.finite(exp)) return(actual-exp)
  return(NA)
}

#' calculates the expected position for raceday to build the master csv
#'
#' @param raceday race master function to cycle through races
#' @keywords masterfunc
#' @export
#' @examples
#' processraceday()
processraceday<-function(data,raceday,race,ind){
  if(grepl("/",raceday$Date[1])) raceday$Date<-as.Date(raceday$Date,"%d/%m/%Y")
  raceday$exp_fin_order<-NA
  raceday$Matrix<-toupper(raceday$Matrix)
  raceday$FP<-as.numeric(raceday$FP)
  if(ind==1) total<-nrow(raceday[raceday$Race<=race,])
  else total<-nrow(raceday[raceday$Race==race,])
  
  if(ind==0) raceday<-raceday[raceday$Race==race,]
  else raceday<-raceday
  startTime<-Sys.time()
  for (i in 1:total){
    if(is.na(raceday$Matrix[i]) | is.na(raceday$Odds[i])) next
    exp.fin.order<-expFin(data,raceday$Date[i],raceday$Distance[i],raceday$Course[i],raceday$Matrix[i],raceday$Odds_Band[i],raceday$Track[i])
    raceday$exp_fin_order[i]<-exp.fin.order
    tt<-round(difftime(Sys.time(),startTime,units="mins"),digits=2)
    message(paste("Processing Intraday: ",i," of ",total," (",tt," minutes elapsed) Est. Remaining: ",round(tt/i*total-tt,digits=2)," mins", sep=''))
    flush.console()
  }
  return(raceday)
}

#' master process all
#'
#' @param raceday race race master to process all
#' @keywords masterproc
#' @export
#' @examples
#' masterProcess()
masterProcess<-function(data,raceday,race,ind){
  if(grepl("/",raceday$Date[1])) raceday$date<-as.Date(raceday$Date,"%d/%m/%Y")
  raceday$Odds_Band<-NA
  raceday$Odds_Band<-mapply(priceBandAlloc,raceday$Odds)
  raceday<-processraceday(data,raceday,race,ind)
  raceday$Error<-mapply(errorCalc,raceday$FP,raceday$exp_fin_order)
  return(raceday)
}



#' calculates +/- against expected position for course of a meeting (cumulative)
#'
#' @param today race generates matrix output for datalab
#' @keywords matrix
#' @export
#' @examples
#' meetingMatrix()
meetingMatrix<-function(today,race){
  races<-today[!duplicated(today[c("Date","Race")]),c("Date","Race")]
  r<-seq(10,1,-1)
  c<-c('E','D','C','B','A')
  diag<-data.frame(matrix(NA,length(c),length(r)))
  colnames(diag)<-r
  row.names(diag)<-c
  c.tot<-length(r)
  r.tot<-length(c)
  for (k in 1:race){
    for (i in 10:1){       #column names
      for(j in 1:r.tot){   #row names
        mtx<-paste(i,rownames(diag)[j],sep="")
        filtera<-toupper(today$Matrix)==mtx #today$Include==1
        b<-today[filtera,'Error']
        if(length(b)<1) diag[j,which(colnames(diag)==i)]<-0
        else diag[j,which(colnames(diag)==i)]<-sum(b,na.rm=T)
        flush.console()
      }
    }
    #write.csv(diag,paste(races$COURSE[k],"_",races$RACE[k],".csv",sep=""))
  }
  mtx<-data.matrix(diag)
  mtx<-round(mtx,2)
  #plotmtx<-plot_ly(z = mtx,
  #x = c("A","B","C","D","E"), y = c(1,2,3,4,5,6,7,8,9,10),
  #type = "heatmap",colorscale="Hot")
  return(mtx)
}

#' runs matrix for a given race + posts to DATALAB
#'
#' @param date venueName animal race
#' @keywords rerun
#' @export
#' @examples
#' masterIntra()
masterIntra<-function(date,venueName,animal,race){
  data<-readCSV()
  today<-intraDay::main(date,animal,venueName)
  today$Date<-date
  today$Track<-mapply(trackType,today$Race_Name)
  meetid<-today$MeetingID[1]
  today<-masterProcess(data,today,race,1)
  x<-meetingMatrix(today,race)/race
  x<-t(x)
  z<-list(payload=list(position=x))
  url<-paste("http://dw-staging-elb-1068016683.ap-southeast-2.elb.amazonaws.com/api/markets/analysis?event_number=",race,"&market_name=MTX_POSITIONS&meeting_id=",meetid,"&provider_name=dw",sep="")
  r<-httr::POST(url,body = z,encode="json",)
  today<-masterProcess(data,today,race,0)
  x<-meetingMatrix(today,race)
  x<-t(x)
  z<-list(payload=list(position=x))
  url<-paste("http://dw-staging-elb-1068016683.ap-southeast-2.elb.amazonaws.com/api/markets/analysis?event_number=",race,"&market_name=MTX_RACE_POSITIONS&meeting_id=",meetid,"&provider_name=dw",sep="")
  r<-httr::POST(url,body = z,encode="json",)
  return(x)
}
