library("oce")
library("zoo")
library("wmm")
library("ggplot2")
library("plotly")
library("zoo")

####
# Load all data
##
env <- "C:/Users/SStho/Documents/loki backup/Stefan/"  #"/home/loki/Documents/Stefan/"#"C:/Users/Schoombie_Asus/"
scols <- c("id","year","fls","gps_lon","gps_lat","Mag_x","Mag_y","Mag_z","roll","cam_angle_interp")

load("C:/Users/SStho/Documents/loki backup/Stefan/Dropbox/Analysis/R/DD_mag/ALL_OUT/waal_2020_01_f_ALL_OUT.Rdata")
dd_out$year <- substr(as.character(dd_out$datetime[1]),1,4)
dd_out$roll_diff <- abs(dd_out$roll - dd_out$cam_angle_interp)
wa1 <- dd_out
rm(dd_out)
wa1_rows <- which(!is.na(wa1$cam_angle_interp) )
wa1 <- wa1[wa1_rows[1]:wa1_rows[length(wa1_rows)]]
wa1 <- subset(wa1,select = scols)
plot(wa1$roll[500000:504000],type = "l");lines(wa1$cam_angle_interp[500000:504000],col = 2)
wa1$id <- "wa1"
unique(wa1$fls[!is.na(wa1$cam_angle_interp)])
wa1$fls[wa1$fls == 0] <- 1
wa1$fls[wa1$fls == 7] <- 2
wa1$fls[wa1$fls == 8] <- 3

wa1_1 = wa1[wa1$fls == 1,]
mean(wa1_1$roll_diff,na.rm = T)
wa1_2 = wa1[wa1$fls == 2,]
mean(abs(wa1_2$roll_diff),na.rm = T)
mean(wa1_2$roll - wa1_2$cam_angle_interp,na.rm = T)
wa1_3 = wa1[wa1$fls == 3,]

load("C:/Users/SStho/Documents/loki backup/Stefan/Dropbox/Analysis/R/DD_mag/ALL_OUT/waal_2020_02_m_ALL_OUT.Rdata")
dd_out$year <- substr(as.character(dd_out$datetime[1]),1,4)
wa2 <- dd_out
rm(dd_out)
wa2_rows <- which(!is.na(wa2$cam_angle_interp) )
wa2 <- wa2[wa2_rows[1]:wa2_rows[length(wa2_rows)],]
wa2 <- subset(wa2,select = scols)
plot(wa2$roll[500000:504000],type = "l");lines(wa2$cam_angle_interp[500000:504000],col = 2)
wa2$id <- "wa2"
unique(wa2$fls)



load("C:/Users/SStho/Documents/loki backup/Stefan/Dropbox/Analysis/R/DD_mag/ALL_OUT/dmsa_2019_04_f_ALL_OUT.Rdata")
dd_out$year <- substr(as.character(dd_out$datetime[1]),1,4)
sa4 <- dd_out
sa4$cam_angle_interp[1:(nrow(sa4)-299)] <- dd_out$cam_angle_interp[300:nrow(dd_out)]
rm(dd_out)
sa4_rows <- which(!is.na(sa4$cam_angle_interp) )
sa4 <- sa4[sa4_rows[1]:sa4_rows[length(sa4_rows)]]
sa4 <- subset(sa4,select = scols)
plot(sa4$roll[200000:204000],type = "l");lines(sa4$cam_angle_interp[200000:204000],col = 2)
sa4$id <- "sa1"


load("C:/Users/SStho/Documents/loki backup/Stefan/Dropbox/Analysis/R/DD_mag/ALL_OUT/dmsa_2019_05_m_ALL_OUT.Rdata")
dd_out$year <- substr(as.character(dd_out$datetime[1]),1,4)
sa5 <- dd_out
rm(dd_out)
sa5_rows <- which(!is.na(sa5$cam_angle_interp) )
sa5 <- sa5[sa5_rows[1]:sa5_rows[length(sa5_rows)]]
sa5 <- subset(sa5,select = scols)
plot(sa5$roll[300000:304000],type = "l");lines(sa5$cam_angle_interp[300000:304000],col = 2)
sa5$id <- "sa1"

load("C:/Users/SStho/Documents/loki backup/Stefan/Dropbox/Analysis/R/DD_mag/ALL_OUT/ghal_2019_01_u_ALL_OUT.Rdata")
dd_out$year <- substr(as.character(dd_out$datetime[1]),1,4)
gh1 <- dd_out
rm(dd_out)
gh1_rows <- which(!is.na(gh1$cam_angle_interp) )
gh1 <- gh1[gh1_rows[1]:gh1_rows[length(gh1_rows)]]
gh1 <- subset(gh1,select = scols)
plot(gh1$roll[100000:104000],type = "l");lines(gh1$cam_angle_interp[100000:104000],col = 2)
gh1$id <-"gh1"

dd_all <- rbind(wa1,wa2,sa4,sa5,gh1)
write.csv(dd_all, file = "C:/Users/SStho/Documents/Work/Publications/2022 Schoombie et al. Magnetometer bank angle method - SUBMIT TO RSoc Open/R/Schoombie_et_al_DATA.csv",row.names = F)
###############
#  WA example #
###############
rm(list = ls()) 

# env <- "C:/Users/Schoombie_Asus/"
options(digits.secs=20)

#Load function to calculate roll and yaw from Mag data
env <- "C:/Users/SStho/Documents/loki backup/Stefan/"  #"/home/loki/Documents/Stefan/"#"C:/Users/Schoombie_Asus/"
source(paste0(env,"Dropbox/Analysis/R/Scripts/June 2020/load_packages.R"))##########################################

#Load DD data and convert date
dd_raw <- read.delim(paste0(env,"Dropbox/Analysis/DD/2020_mar_waal_ds10_nest77/waal_2020_01_flast_selection_split#6_0.txt"))
dd_raw$datetime <- as.POSIXct(paste0(dd_raw$Date," ",dd_raw$Time_dd.hh.mm.ss.sss,".",substr(dd_raw$Decimal.secs,3,5)), format = "%d/%m/%Y %H:%M:%OS",tz = "GMT") #+ c_off
head(dd_raw$datetime)

#load GPS data and convert date and speed
# gps <- read.csv("D:/PhD/Raw data/M76/WA/DS10 - nest 77/Deployment/waal_20200316_j517_bro_c01_2126_d10_mar_f_1.csv")
gps <- read.csv("waal_20200316_j517_bro_c01_2126_d10_mar_f_1.csv")
gps$datetime <- as.POSIXct(paste0(gps$Date,gps$Time),format = "%Y/%m/%d %H:%M:%S",tz = "GMT" )
gps$datetime <- gps$datetime + (3600*3-8)
gps$mspeed <- gps$Speed/1000  

#Choose start time
ts <- as.POSIXct("2020/03/14 11:04:23" , format = "%Y/%m/%d %H:%M:%S", tz = "GMT")
te <- as.POSIXct("2020/03/14 11:09:09" , format = "%Y/%m/%d %H:%M:%S", tz = "GMT")

#Get reference angle dataset
ref_angles <- read.csv(paste0(env,"Dropbox/Analysis/R/DD_mag/waal_2020_01_flast_validation/waal_2020_01_flast.txt"), header=T)
names(ref_angles) <- c("frame","datetime","angle")
ref_angles$datetime <- as.POSIXct(ref_angles$datetime,tz = "GMT")
ref_angles$rm_angle <- rollmean(-ref_angles$angle,3,na.pad = T)

#Subset to match chosen times
gps_hold <- gps
gps <- subset(gps_hold,datetime > ts & datetime < te)
ggplotly(ggplot(gps,aes(Longitude,Latitude)) + geom_line()+ geom_line(data = gps_hold,aes(Longitude,Latitude),col = "grey",alpha = 0.5))
dd_raw <- subset(dd_raw,datetime > ts & datetime < te)

dd <- dd_raw
#Rotate the axes to match NED convention
# change axes to avionics standard. Measured data has x forward, y to the left and z up

dd$Mag_x <- -dd_raw$Mag_y
dd$Mag_y <- -dd_raw$Mag_x
dd$Mag_z <- -dd_raw$Mag_z

dd$Acc_x <- dd_raw$Acc_y
dd$Acc_y <- dd_raw$Acc_x
dd$Acc_z <- dd_raw$Acc_z



dd <- dd#[1:50000,] #Just try with small df first to make sure 

# Get reference Mag data using nearest recorded GPS coords
magF <- GetMagneticFieldWMM(gps$Longitude[1],gps$Latitude[1],5,2019)
MagrefNED <- matrix(c(magF$x,magF$y,magF$z)) *10000 * 1E-9 # nTesla, NED,
# Magrefnorm <- norm(matrix(MagrefNED),type ="F")
normNE <- norm(matrix(MagrefNED[1:2]),type = "F")   # get absolate length of horizontal magnetic field from the earth model
#Calculate mag declination for reference data
Beta <- atan2(MagrefNED[2],MagrefNED[1])
Magscale <- 1

#Calculate roll and yaw angles (2 solutions - pos and neg)
angles <- ry_calc(dd,Magscale,gps)

#Assign newly calculated values to dd dataframe
#Negative is the right solution for this flight
dd$roll_neg <- angles[,2]*180/pi#rollmean(round(angles[,2]*180/pi,0),4,na.pad = T)
dd$yaw_neg <- round(angles[,1]*180/pi,0)
dd$roll_pos <- rollmean(round(angles[,4]*180/pi,0),1,na.pad = T)
dd$yaw_pos <- round(angles[,3]*180/pi,0)

if(abs(mean(dd$roll_neg,na.rm = T)) < abs(mean(dd$roll_pos,na.rm = T))){
  dd$roll <- dd$roll_neg
  dd$yaw <- dd$yaw_neg
}else{
  dd$roll <- dd$roll_pos
  dd$yaw <- dd$yaw_pos
}

dd$roll[!is.na(dd$yaw) & dd$yaw > -70 & dd$yaw < -10] <- NA
dd$roll[dd$roll > 180 & !is.na(dd$roll)] <- dd$roll[dd$roll > 180& !is.na(dd$roll)] -360
dd$roll[dd$roll <= -180 & !is.na(dd$roll)] <- dd$roll[dd$roll <= -180 & !is.na(dd$roll)] +360

dd$mag_roll <- (atan2(dd$Mag_z,-dd$Mag_y))*180/pi
dd$alpha <- dd$roll_neg - dd$mag_roll

dd$roll[dd$roll > 180 & !is.na(dd$roll)] <- dd$roll[dd$roll > 180& !is.na(dd$roll)] -360
dd$roll_pos[dd$roll_pos > 180 & !is.na(dd$roll_pos)] <- dd$roll_pos[dd$roll_pos > 180& !is.na(dd$roll_pos)] -360
dd$roll_pos[dd$roll_pos <= -180 & !is.na(dd$roll_pos)] <- dd$roll_pos[dd$roll_pos <= -180 & !is.na(dd$roll_pos)] +360
dd$roll_neg[dd$roll_neg > 180 & !is.na(dd$roll_neg)] <- dd$roll_neg[dd$roll_neg > 180& !is.na(dd$roll_neg)] -360
dd$roll_neg[dd$roll_neg <= -180 & !is.na(dd$roll_neg)] <- dd$roll_neg[dd$roll_neg <= -180 & !is.na(dd$roll_neg)] +360

ggplotly(ggplot(data = dd) +
           geom_line(data = ref_angles,aes((datetime+1),rm_angle))+
           geom_line(aes(datetime+3.8,roll_neg),col = 2)+
           geom_line(aes(datetime+3.8,roll_pos),col = 3)+
           geom_line(aes(datetime+3.8,roll),col = 4)+
           geom_hline(yintercept = 0)
)

#Calculate the magnetic roll without pitch/yaw correction
#This will give the right shape to interpolate the missing values

#Transform the data to fit on a 0-360 degree frame
dd$mag_roll <- (dd$mag_roll +360)%%360
#Get the difference between successive sample
dd$mag_roll_diff <- NA
dd$mag_roll_diff[2:nrow(dd)] <- diff(dd$mag_roll)

#Interpolate the missing roll angles from the above slopes
dd$roll2 <- dd$roll
for(i in 1:(nrow(dd)-1)){
  if(is.na(dd$roll2[i]))next
  if(is.na(dd$roll2[i+1])){
    dd$roll2[i+1] <- dd$roll2[i] + dd$mag_roll_diff[i]
  }
}

dd$rm_roll<- rollmean(dd$roll2,4, na.pad = T)

##########
#Now calculate heading from new roll values
##########
#Normalize mag data
fm <- sqrt(dd$Mag_x^2+dd$Mag_y^2+dd$Mag_z^2)
dd$Mag_y_R <- cos(dd$rm_roll/180*pi)*(dd$Mag_y/fm) + sin(dd$rm_roll/180*pi)*(dd$Mag_z/fm)
dd$Mag.heading <- -atan2(dd$Mag_y_R,(dd$Mag_x/fm))*180/pi + Beta*180/pi

#Get pitch from Mag values?
dd$Mag.pitch <- atan2(dd$Mag_x,dd$Mag_z)*180/pi


dd$diff <- NA

#ACC roll angles
metric_calc <- function(ax,ay,az,window,metric){
  ax_rm <- rollapply(ax,window,mean,align = "center",fill = NA)
  ay_rm <- rollapply(ay,window,mean,align = "center",fill = NA)
  az_rm <- rollapply(az,window,mean,align = "center",fill = NA)
  vesba <- sqrt(ax_rm^2+ay_rm^2+az_rm^2)
  vedba <- sqrt((ax-ax_rm)^2+(ay-ay_rm)^2+(az-az_rm)^2) 
  odba <- sqrt((ax)^2+(ay)^2+(az)^2) 
  pitch <- atan2(ay_rm, sqrt(ax_rm^2 + az_rm^2))*180/pi
  # pitch <- atan2(ay_rm, az_rm)*180/pi
  roll <- atan2(ax_rm, sqrt(ay_rm^2 + az_rm^2))*180/pi
  
  
  # roll <- atan2(ax_rm, ay_rm)*180/pi
  
  if(metric == "vedba"){
    return(vedba)
  }else if(metric == "vesba"){
    return(vesba)
  }else if(metric == "roll"){
    return(roll)
  }else if(metric == "pitch"){
    return(pitch)
  }else if(metric == "odba"){
    return(odba)
  }
  
}
dd$acc_roll <- metric_calc(dd$Acc_x,dd$Acc_y,dd$Acc_z,80,"roll")

dd$acc_pitch <- metric_calc(dd$Acc_x,dd$Acc_y,dd$Acc_z,80,"pitch")

ggplotly(ggplot(dd)+geom_line(aes(Total.Event.no.,Acc_z),col = 4)+
           geom_line(aes(Total.Event.no.,Acc_x),col = 2)+
           geom_line(aes(Total.Event.no.,Acc_y),col = 3)+
           geom_line(aes(Total.Event.no.,Mag_x),col = 5)+
           geom_line(aes(Total.Event.no.,Mag_y),col = 6)+
           geom_line(aes(Total.Event.no.,Mag_z),col = 7)+
           # geom_line(aes(Total.Event.no.,acc_roll/180*pi),col = 1)#+
         geom_line(aes(Total.Event.no.,atan2(Acc_x,Acc_z)),col = 1)#+
           # geom_line(aes(Total.Event.no.,acc_pitch),col = 1)
         )

#Plot the reference angles with the newly calculated angles
(g <- ggplot() +
    geom_line(data = ref_angles[25:295,],aes((datetime+1),rm_angle))+
    # geom_point(data = dd, aes(datetime,roll_neg_spline),col = 4)+
    # geom_line(data = dd, aes(datetime+3.8,roll_neg2),col = 4)+
    geom_line(data = dd[1:3750,], aes(datetime+3.8,rm_roll),col = 2)+
    geom_line(data = dd[1:3750,], aes(datetime+3.8,roll2),col = 3,alpha = 0.5)+
    # geom_line(data = dd[1:3750,], aes(datetime+3.8,atan2(Acc_y.sm,Acc_z.sm)*180/pi),col = 3,alpha = 0.5)+
    
    geom_line(data = dd[1:3750,], aes(datetime+3.8,-acc_roll),col = 3)+
    # geom_line(data = dd[1:3750,], aes(datetime+3.8,acc_pitch),col = 3)+
    
    # geom_line(data = dd, aes(datetime+3.8,alpha),col = 6)+
    # geom_line(data = dd, aes(datetime+3.8,yaw),col = 3)+
    # geom_line(data = dd, aes(datetime+3.8,rollmean(Mag.heading,40,na.pad = T)),col = 4)+
    # geom_line(data = gps,aes(datetime,mspeed/180*pi))+
    # geom_line(data = dd, aes(datetime+3.8,rollmean(Mag.pitch/180*pi,40,na.pad = T)),col = 5)+
    # geom_line(data = dd,aes(datetime + 3.8,Acc_z),col = 3)+
    # geom_line(data = dd,aes(datetime + 3.8 ,(Pressure.hbar)/1000),col = 4)+
    # geom_line(data = gps,aes(datetime,Course))+
    # geom_line(data = dd, aes(datetime + 3.8, mag_roll),col = 3)+
    # geom_line(data = dd, aes(datetime + 3.8, g_roll_p),col = 5)+
    geom_hline(yintercept = 0)#+
    # geom_hline(yintercept = mean(gps$mspeed,na.rm = T))#+
    # xlim(c(min(ref_angles$datetime),max(ref_angles$datetime)))
    # xlim(c(min(ref_angles$datetime)+20,min(ref_angles$datetime)+60)
  
)
ggplotly(g)


g_flight_theme <- theme(panel.grid = element_blank(),
                        axis.line = element_line(),
                        axis.text.y = element_text(size = 12, colour = 1),
                        axis.text.x = element_text(size = 12, colour = 1),
                        text = element_text(size=12, colour = 1),
                        panel.background = element_blank()
)

(g_flight_angle <- ggplot() +
    geom_line(data = ref_angles, aes(datetime, rm_angle),size =1)+
    # geom_point(data = ref_angles, aes(datetime, rm_angle),size =1)+
    geom_line(data = dd, aes(datetime+2.7, rm_roll), size = 1,col = 2)+
    geom_line(data = dd, aes(datetime+2.7, acc_roll), size = 1,col = 3)+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7),limits = c(-50,90)) +
    xlim(c(min(ref_angles$datetime)+110,min(ref_angles$datetime)+201)) + 
    ylim(c(min(dd$rm_roll,na.rm=T)-5,max(dd$rm_roll,na.rm=T)+5)) + 
    xlab("Time (s)")+
    ylab("Roll angle (degrees)")+
    annotation_custom(grobTree(textGrob("a)",x = 0.05,y = 0.97,gp = gpar(fill=1))))+
    geom_hline(yintercept = 0)+
    g_flight_theme
)

#Make density plot of angles from reference vs Mag
(g_flight_dense <- 
    ggplot() +
    geom_density(data = ref_angles[],aes(rm_angle),size =1)+
    geom_density(data = dd[dd$datetime < max(ref_angles$datetime) ,],
                 aes(rm_roll),col = 2,size = 1)+
    geom_vline(xintercept = 0,linetype = "dashed")+
    ylab("Density")+
    xlab("Roll angle (degrees)")+
    annotation_custom(grobTree(textGrob("b)",x = 0.05,y = 0.97,gp = gpar(fill=1))))+
    g_flight_theme
)

(g_flight  <- ggplot()+
    coord_equal(xlim = c(0, 3.3), ylim = c(0, 1), expand = FALSE) +
    annotation_custom(ggplotGrob(g_flight_angle), xmin = 0.1, xmax = 1.6, ymin = 0, 
                      ymax = 1) +
    annotation_custom(ggplotGrob(g_flight_dense), xmin = 1.7, xmax = 3, ymin = 0, 
                      ymax = 1) +
    theme_void())


  ################
 # DMSA example #
################
rm(list = ls()) 

env <- "C:/Users/Schoombie_Asus/"
options(digits.secs=20)

#Load function to calculate roll and yaw from Mag data

source(paste0(env,"/Dropbox/Analysis/R/Scripts/calc_roll2.R"))
source(paste0(env,"/Dropbox/Analysis/R/Scripts/ry_calc.R"))

#Load DD data and convert date
dd_raw <- read.delim("C:/Users/SStho/Documents/loki backup/Stefan/Dropbox/Analysis/DD/2019_mar_phofus_ghr5/dmsa_flight9_Mar_2020_selection_split#1_0.txt")
dd_raw$datetime <- as.POSIXct(paste0(dd_raw$Date," ",dd_raw$Time_dd.hh.mm.ss.sss,".",substr(dd_raw$Decimal.secs,3,5)), format = "%d/%m/%Y %H:%M:%OS",tz = "GMT") #+ c_off
head(dd_raw$datetime)

#load GPS data and convert date and speed
# gps <- read.csv("G:/Raw data/M76/WA/DS10 - nest 77/Deployment/waal_20200316_j517_bro_c01_2126_d10_mar_f_1.csv")
# gps$datetime <- as.POSIXct(paste0(gps$Date,gps$Time),format = "%Y/%m/%d %H:%M:%S",tz = "GMT" )
# gps$datetime <- gps$datetime + (3600*3-8)
# gps$mspeed <- gps$Speed/1000  

#Choose start time
ts <- as.POSIXct("2020/03/14 11:04:23" , format = "%Y/%m/%d %H:%M:%S", tz = "GMT")
te <- as.POSIXct("2020/03/14 11:09:09" , format = "%Y/%m/%d %H:%M:%S", tz = "GMT")

#Get reference angle dataset
c_off <- 60160924 #camera offset 60160924
cam <- read.csv("D:/PhD/Raw data/Bank Angles/m76/dmsa_5_flight_9_angles.csv")
cam_start <-"2018.01.20 03:05:17.8"  #flight 1"2018.01.20 01:54:18.574" # flight 9 "2018.01.20 03:05:17.8"
cam$datetime <- as.POSIXct(cam_start,format = "%Y.%m.%d %H:%M:%OS", tz = "GMT") + cam$time/1000 + c_off
head(cam$datetime)
ref_angles <- subset(cam, select = c("datetime","spline"))
names(ref_angles) <- c("datetime","angle")
ref_angles$rm_angle <- rollmean(ref_angles$angle,3,na.pad = T)

#Subset to match chosen times
# gps_hold <- gps
# gps <- subset(gps_hold,datetime > ts & datetime < te)
# ggplotly(ggplot(gps,aes(Longitude,Latitude)) + geom_line()+ geom_line(data = gps_hold,aes(Longitude,Latitude),col = "grey",alpha = 0.5))
# dd_raw <- subset(dd_raw,datetime > ts & datetime < te)

dd <- dd_raw#subset(dd_raw, datetime > cam$datetime[1] & datetime < cam$datetime[nrow(cam)])
#Rotate the axes to match NED convention
# change axes to avionics standard. Measured data has x forward, y to the left and z up

dd$Mag_x <- -dd_raw$Mag_y
dd$Mag_y <- dd_raw$Mag_x
dd$Mag_z <- -dd_raw$Mag_z

dd$Acc_x <- dd_raw$Acc_y
dd$Acc_y <- dd_raw$Acc_x
dd$Acc_z <- dd_raw$Acc_z

dd <- dd[5000:10000,] #Just try with small df first to make sure 

# Get reference Mag data using nearest recorded GPS coords
magF <- GetMagneticFieldWMM(37.48932,-47.00082,5,2019)
MagrefNED <- matrix(c(magF$x,magF$y,magF$z)) *10000 * 1E-9 # nTesla, NED,
# Magrefnorm <- norm(matrix(MagrefNED),type ="F")
normNE <- norm(matrix(MagrefNED[1:2]),type = "F")   # get absolate length of horizontal magnetic field from the earth model
#Calculate mag declination for reference data
Beta <- atan2(MagrefNED[2],MagrefNED[1])
Magscale <- 1

#Calculate roll and yaw angles (2 solutions - pos and neg)
angles <- ry_calc(dd,Magscale,gps)

#Assign newly calculated values to dd dataframe
#Negative is the right solution for this flight
dd$roll_neg <- angles[,2]*180/pi#rollmean(round(angles[,2]*180/pi,0),4,na.pad = T)
dd$yaw_neg <- round(angles[,1]*180/pi,0)
dd$roll_pos <- angles[,4]*180/pi#rollmean(round(angles[,2]*180/pi,0),4,na.pad = T)
dd$yaw_pos <- round(angles[,3]*180/pi,0)

dd$roll_neg[dd$roll_neg > 180 & !is.na(dd$roll_neg)] <- dd$roll_neg[dd$roll_neg > 180& !is.na(dd$roll_neg)] -360
dd$roll_neg[dd$roll_neg < -180 & !is.na(dd$roll_neg)] <- 360 + dd$roll_neg[dd$roll_neg < -180& !is.na(dd$roll_neg)] 
dd$roll_pos[dd$roll_pos > 180 & !is.na(dd$roll_pos)] <- dd$roll_pos[dd$roll_pos > 180& !is.na(dd$roll_pos)] -360
dd$roll_pos[dd$roll_pos < -180 & !is.na(dd$roll_pos)] <- 360 + dd$roll_pos[dd$roll_pos < -180& !is.na(dd$roll_pos)] 

dd$roll_pos[!is.na(dd$yaw_pos) & ((dd$yaw_pos > -70 & dd$yaw_pos < -10)|(dd$yaw_pos > 110 & dd$yaw_pos < 170))] <- NA
dd$roll_neg[!is.na(dd$yaw_neg) & ((dd$yaw_neg > -70 & dd$yaw_neg < -10)|(dd$yaw_neg > 110 & dd$yaw_neg < 170))] <- NA


#Calculate the magnetic roll without pitch/yaw correction
#This will give the right shape to interpolate the missing values

#Transform the data to fit on a 0-360 degree frame
dd$mag_roll <- (atan2(dd$Mag_y,dd$Mag_z))*180/pi
dd$alpha <- dd$roll_pos - dd$mag_roll

dd$mag_roll <- (dd$mag_roll +360)%%360
#Get the difference between successive sample
dd$mag_roll_diff <- NA
dd$mag_roll_diff[2:nrow(dd)] <- diff(dd$mag_roll)

#Interpolate the missing roll angles from the above slopes
for(i in 1:(nrow(dd)-1)){
  if(is.na(dd$roll_pos[i]))next
  if(is.na(dd$roll_pos[i+1])){
    dd$roll_pos[i+1] <- dd$roll_pos[i] + dd$mag_roll_diff[i]
  }
}


for(i in 1:(nrow(dd)-1)){
  if(is.na(dd$roll_neg[i]))next
  if(is.na(dd$roll_neg[i+1])){
    dd$roll_neg[i+1] <- dd$roll_neg[i] + dd$mag_roll_diff[i]
  }
}

dd$roll <- NA
for(i in 401:(nrow(dd)-401)){
  if(is.na(dd$roll_neg[i])){
    dd$roll[i] <- NA
  }
  else if(abs(mean(dd$roll_neg[(i-200):(i+200)],na.rm = T)) < abs(mean(dd$roll_pos[(i-200):(i+200)],na.rm = T))){
    dd$roll[i] <- dd$roll_neg[i]
  }
  else{
    dd$roll[i] <- dd$roll_pos[i]
  }
}

dd$rm_roll_pos <- rollmean(dd$roll_pos2,4, na.pad = T)


dd$diff <- NA

# #Calculate roll from g-force ()
dd$g <- (sqrt(dd$Mag_x^2 + dd$Mag_y^2 + dd$Mag_z^2))
dd$g_roll_p <- acos(1/(1+dd$g))*180/pi
dd$g_roll_n <- -acos(1/(1+dd$g))*180/pi

#Acc roll and pitch
dd$acc_roll <- metric_calc(dd$Acc_x,dd$Acc_y,dd$Acc_z,80,"roll")
dd$acc_pitch <- metric_calc(dd$Acc_x,dd$Acc_y,dd$Acc_z,80,"pitch")


#Plot the reference angles with the newly calculated angles
(g <- ggplot() +
    geom_line(data = ref_angles,aes(datetime,rm_angle),size =1)+
    # geom_point(data = dd, aes(datetime + 6.1,roll_pos),col = 4)+
    # geom_point(data = dd, aes(datetime + 6.1,roll_neg),col = 5)+
    geom_line(data = dd, aes(datetime + 6.1,roll),col = 3,size =1)+
    geom_line(data = dd, aes(datetime + 6.1,acc_roll),col = 4,size =1)+
    geom_line(data = dd, aes(datetime + 6.1,acc_pitch),col = 5,size =1)+
    # geom_line(data = dd, aes(datetime+2.8,roll_neg2),col = 4)+
    # geom_line(data = dd, aes(datetime + 6.1,rm_roll_pos),col = 2)+
    # geom_line(data = dd, aes(datetime+2.8,alpha),col = 6)+
    # geom_line(data = dd, aes(datetime+2.8,yaw_neg),col =2)+
    # geom_line(data = dd, aes(datetime+2.8,yaw_pos),col =4)+
    # geom_line(data = dd, aes(datetime+2.8,yaw_neg),col =5)+
    # geom_line(data = gps,aes(datetime,Course))+
    # geom_line(data = dd, aes(datetime + 6.1 , mag_roll),col = 3,size =1)+
    # geom_line(data = dd, aes(datetime + 2.8, g_roll_p),col = 5)+
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = -20,linetype = "dashed")+
    geom_hline(yintercept = -70,linetype = "dashed")+
    xlim(c(min(dd$datetime),max(dd$datetime)))
    # xlim(c(min(ref_angles$datetime)+20,min(ref_angles$datetime)+60))
  
)


ggplotly(g)

g_flight_theme <- theme(panel.grid = element_blank(),
                        axis.line = element_line(),
                        axis.text.y = element_text(size = 12, colour = 1),
                        axis.text.x = element_text(size = 12, colour = 1),
                        text = element_text(size=12, colour = 1),
                        panel.background = element_blank()
)
t <- 470
(g_flight_angle <- ggplot() +
    geom_line(data = ref_angles, aes(datetime, rm_angle),size =1)+
    # geom_point(data = ref_angles, aes(datetime, rm_angle),size =1)+
    geom_line(data = dd, aes(datetime+6, roll), size = 1,col = 2)+
    geom_line(data = dd, aes(datetime+6, acc_roll), size = 1,col = 3)+
    # geom_line(data = dd, aes(datetime+6, acc_pitch), size = 1,col = 4)+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7),limits = c(-50,90)) +
    xlim(c(min(ref_angles$datetime)+t,min(ref_angles$datetime)+t+90)) + 
    xlab("Time (s)")+
    ylab("Roll angle (degrees)")+
    geom_hline(yintercept = 0)+
    annotation_custom(grobTree(textGrob("c)",x = 0.05,y = 0.97,gp = gpar(fill=1))))+
    g_flight_theme
)

#Make density plot of angles from reference vs Mag
(g_flight_dense <- 
    ggplot() +
    geom_density(data = ref_angles[ref_angles$datetime > min(ref_angles$datetime)+20 &
                                     ref_angles$datetime < min(ref_angles$datetime)+60 
                                   ,],aes(rm_angle),size =1)+
    geom_density(data = dd[dd$datetime+6 > min(ref_angles$datetime)+20 &
                             dd$datetime+6 < min(ref_angles$datetime)+60  
                           ,],
                 aes(roll),col = 2,size = 1)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8),limits = c(-70,80)) +
    geom_vline(xintercept = 0,linetype = "dashed")+
    ylab("Density")+
    xlab("Roll angle (degrees)")+
    annotation_custom(grobTree(textGrob("d)",x = 0.05,y = 0.97,gp = gpar(fill=1))))+
    g_flight_theme
)

(g_flight  <- ggplot()+
    coord_equal(xlim = c(0, 3.3), ylim = c(0, 1), expand = FALSE) +
    annotation_custom(ggplotGrob(g_flight_angle), xmin = 0.1, xmax = 1.6, ymin = 0, 
                      ymax = 1) +
    annotation_custom(ggplotGrob(g_flight_dense), xmin = 1.7, xmax = 3, ymin = 0, 
                      ymax = 1) +
    theme_void())

######################################################################



######################################################################
rm(list = ls()) 

env <- "C:/Users/SStho/Documents/loki backup/Stefan/"  #"/home/loki/Documents/Stefan/"#"C:/Users/Schoombie_Asus/"#"~/Documents/Stefan/"#C:/Users/Schoombie_Asus/" #G:/

options(digits.secs=20)

#Load function to calculate roll and yaw from Mag data

source(paste0(env,"/Dropbox/Analysis/R/Scripts/calc_roll2.R"))
source(paste0(env,"/Dropbox/Analysis/R/Scripts/ry_calc.R"))
source(paste0(env,"Dropbox/Analysis/R/Scripts/June 2020/load_packages.R"))##########################################

#Load DD data and convert date
# dd_raw <- read.delim("C:/Users/Schoombie_Asus/Dropbox/Analysis/DD/2020_DD_Mag_cal_Library/zero_bank_all_angles_selection_0.txt")
dd_raw <- read.delim(paste0(env,"/Dropbox/Analysis/DD/2020_DD_Mag_cal_Library/Lib_cal_ALL_selection_split#1_0.txt"))
# dd_raw$Date <- "1/2/2017"
dd_raw$datetime <- as.POSIXct(paste0(dd_raw$Date," ",dd_raw$Time_dd.hh.mm.ss.sss,".",substr(dd_raw$Decimal.secs,3,5)), format = "%d/%m/%Y %H:%M:%OS",tz = "GMT") #+ c_off
head(dd_raw$datetime)

dd <- dd_raw
#Rotate the axes to match NED convention
# change axes to avionics standard. Measured data has x forward, y to the left and z up
dd$Mag_x <- dd_raw$Mag_x
dd$Mag_y <- -dd_raw$Mag_y
dd$Mag_z <- -dd_raw$Mag_z

dd <- dd#[7000:7500,]#Just try with small df first to make sure 

# Get reference Mag data using nearest recorded GPS coords
magF <- GetMagneticFieldWMM( 37.859251,-46.875590,5,2019)
MagrefNED <- matrix(c(magF$x,magF$y,magF$z)) *10000 * 1E-9 # nTesla, NED,
# Magrefnorm <- norm(matrix(MagrefNED),type ="F")
normNE <- norm(matrix(MagrefNED[1:2]),type = "F")   # get absolate length of horizontal magnetic field from the earth model
#Calculate mag declination for reference data
Beta <- atan2(MagrefNED[2],MagrefNED[1])
decl <- Beta*180/pi
decl_window <- 1
mag_N <- c(decl -decl_window,decl+decl_window)
mag_S <- c((decl+180) -decl_window,(decl+180)+decl_window)

#Calculate roll and yaw angles (2 solutions - pos and neg)
angles <- ry_calc(dd,1,gps)
dd$cam_angle <- NA
dd <- mag_analysis(dd,100)


#Assign newly calculated values to dd dataframe
#Negative is the right solution for this flight
dd$roll_neg <- angles[,2]*180/pi#rollmean(round(angles[,2]*180/pi,0),4,na.pad = T)
dd$yaw_neg <- round(angles[,1]*180/pi,0)
dd$roll_pos <- angles[,4]*180/pi#rollmean(round(angles[,2]*180/pi,0),4,na.pad = T)
dd$yaw_pos<- round(angles[,3]*180/pi,0)

#Calculate roll from Mag alone
dd$mag_roll <- (atan2(dd$Mag_z,-dd$Mag_y))*180/pi

#Get alpha values for each calculated roll
dd$alpha <- dd$roll_neg - dd$mag_roll

#Make sure roll is on a -180 to 180 axis
dd$roll_neg[dd$roll_neg > 180 & !is.na(dd$roll_neg)] <- dd$roll_neg[dd$roll_neg > 180& !is.na(dd$roll_neg)] -360
dd$roll_neg[dd$roll_neg < -180 & !is.na(dd$roll_neg)] <- dd$roll_neg[dd$roll_neg < -180& !is.na(dd$roll_neg)] +360
dd$roll_pos[dd$roll_pos > 180 & !is.na(dd$roll_pos)] <- dd$roll_pos[dd$roll_pos > 180& !is.na(dd$roll_pos)] -360
dd$roll_pos[dd$roll_pos < -180 & !is.na(dd$roll_pos)] <- dd$roll_pos[dd$roll_pos < -180& !is.na(dd$roll_pos)] +360

#Make sure yaw is on a -180 to 180 axis
dd$yaw_neg[dd$yaw_neg > 180 & !is.na(dd$yaw_neg)] <- dd$yaw_neg[dd$yaw_neg > 180& !is.na(dd$yaw_neg)] -360
dd$yaw_neg[dd$yaw_neg < -180 & !is.na(dd$yaw_neg)] <- dd$yaw_neg[dd$yaw_neg < -180& !is.na(dd$yaw_neg)] +360
dd$yaw_pos[dd$yaw_pos > 180 & !is.na(dd$yaw_pos)] <- dd$yaw_pos[dd$yaw_pos > 180& !is.na(dd$yaw_pos)] -360
dd$yaw_pos[dd$yaw_pos < -180 & !is.na(dd$yaw_pos)] <- dd$yaw_pos[dd$yaw_pos < -180& !is.na(dd$yaw_pos)] +360

#Transform the data to fit on a 0-360 degree frame
dd$mag_roll <- (dd$mag_roll +360)%%360
#Get the difference between successive sample
dd$mag_roll_diff <- NA
dd$mag_roll_diff[2:nrow(dd)] <- diff(dd$mag_roll)

#Interpolate the missing roll angles from the above slopes
dd$roll_neg2 <- dd$roll_neg
dd$roll_pos2 <- dd$roll_pos
for(i in 1:(nrow(dd)-1)){
  if(is.na(dd$roll_neg2[i]))next
  if(is.na(dd$roll_neg2[i+1])){
    dd$roll_neg2[i+1] <- dd$roll_neg2[i] + dd$mag_roll_diff[i]
    # if(dd$roll_neg2[i+1] < -180){dd$roll_neg2[i+1] <- dd$roll_neg2[i+1] + 180}
  }
}
dd$roll_neg2[dd$roll_neg2 < -180 & !is.na(dd$roll_neg2)] <- 360 + dd$roll_neg2[dd$roll_neg2 < -180 & !is.na(dd$roll_neg2)]

for(i in 1:(nrow(dd)-1)){
  if(is.na(dd$roll_pos2[i]))next
  if(is.na(dd$roll_pos2[i+1])){
    dd$roll_pos2[i+1] <- dd$roll_pos2[i] + dd$mag_roll_diff[i]
    # if(dd$roll_pos2[i+1] < -180){dd$roll_pos2[i+1] <-  dd$roll_pos2[i+1] + 180}
  }
}
dd$roll_pos2[dd$roll_pos2 < -180 & !is.na(dd$roll_pos2)] <- 360 + dd$roll_pos2[dd$roll_pos2 < -180 & !is.na(dd$roll_pos2)]

ts <- which(dd$datetime == as.POSIXct("2017-02-01 00:01:26.0",format = "%Y-%m-%d %H:%M:%OS", tz = "GMT"))
te <- which(dd$datetime == as.POSIXct("2017-02-01 00:01:31.410",format = "%Y-%m-%d %H:%M:%OS", tz = "GMT"))
tmid <- which(dd$datetime == as.POSIXct("2017-02-01 00:04:10.410",format = "%Y-%m-%d %H:%M:%OS", tz = "GMT"))


dd$int_roll <- NA
dd$int_roll[1:tmid] <- dd$roll_neg2[1:tmid]
dd$int_roll[(tmid+1):nrow(dd)] <- dd$roll_pos2[(tmid+1):nrow(dd)]
dd$int_roll[dd$int_roll > 200] <- NA

dd$int_yaw <- NA
dd$int_yaw[1:tmid] <- dd$yaw_neg[1:tmid]
dd$int_yaw[(tmid+1):nrow(dd)] <- dd$yaw_pos[(tmid+1):nrow(dd)]
dd$int_yaw[dd$int_yaw < 0 & !is.na(dd$int_yaw)] <- NA#360 - (abs(dd$int_yaw[dd$int_yaw < 0& !is.na(dd$int_yaw)]))

dd$acc_roll <- atan2(dd$Acc_y,dd$Acc_z)*180/pi

dd$neg_error <- dd$pos_error <- dd$int_error <- NA
dd$neg_error <- abs(abs(dd$roll_neg)-abs(dd$acc_roll))
dd$pos_error <- abs(abs(dd$roll_pos)-abs(dd$acc_roll))
dd$int_error <- abs(abs(dd$int_roll)-abs(dd$acc_roll))

# dd$pos_error[dd$pos_error > 180 & !is.na(dd$pos_error)] <- 360 - dd$neg_error[dd$pos_error > 180& !is.na(dd$pos_error)]
# dd$pos_error[dd$pos_error > 180 & !is.na(dd$pos_error)] <- 360 - dd$neg_error[dd$pos_error > 180& !is.na(dd$pos_error)]


dd$rot <- 0
hd <- c(681,1406,1924,2456,2923,3399,3958,4756,5218,5673,6088,6525,6947,7420,7845,8260,8649,9075,9604,10046,10392,10766,11215,11667,12018, 12406, 13061, 13422, 13845 ,14148, 14421 ,14749,15218 ,15614, 15981, 16344, 16567)
hd0 <- 1
hd_n <- 0
for(i in 1:length(hd)){
  # plot(dd$int_roll[hd0:hd[i]],main = hd_n)
  dd$rot[hd0:hd[i]] <- hd_n
  hd0 <- hd[i]+1
  hd_n <- hd_n + 10
}

zoom <- 
  # c(ts:te)
  c(510:nrow(dd))
#Plot the reference angles with the newly calculated angles
(g <- ggplot() +
    # geom_point(data = dd, aes(datetime,roll_neg_spline),col = 4)+
    # geom_line(data = dd[zoom,], aes(datetime,rollmean(acc_roll,1,na.pad = T)),col = 1)+
    
     # geom_path(data = dd[zoom,], aes(datetime,rollmean(int_roll,1,na.pad = T)),col = 2)+
     # geom_path(data = dd[510:tmid,], aes(datetime,rollmean(roll_neg,1,na.pad = T)),col = 3)+
    # geom_path(data = dd[tmid:nrow(dd),], aes(datetime,rollmean(roll_pos,1,na.pad = T)),col = 3)+
    
        geom_line(data = dd[1:tmid,], aes(datetime,int_yaw),col = "grey")+
    geom_line(data = dd[tmid:nrow(dd),], aes(datetime,yaw_pos),col = "grey")+
    
    # geom_line(data = dd[zoom,], aes(datetime,alpha),col = 7)+
    # geom_line(data = gps,aes(datetime,Course))+
    # geom_line(data = dd, aes(datetime , mag_roll - mean(alpha,na.rm = T)),col = 6)+
    # geom_line(data = dd, aes(datetime + 2.8, g_roll_p),col = 5)+
    geom_hline(yintercept = 0)
  # xlim(c(min(ref_angles$datetime),min(ref_angles$datetime)+20))
  
)
ggplotly(g)

summary(dd$neg_error[1:tmid])
mean(dd$neg_error[1:tmid],na.rm=T)
sd(dd$neg_error[1:tmid],na.rm=T)

summary(dd$pos_error[tmid:nrow(dd)])
mean(dd$pos_error[tmid:nrow(dd)],na.rm = T)
sd(dd$pos_error[tmid:nrow(dd)],na.rm = T)

summary(dd$int_error[tmid:nrow(dd)])
mean(dd$int_error[tmid:nrow(dd)],na.rm = T)
sd(dd$int_error[tmid:nrow(dd)],na.rm = T)

#Set theme for plots
g_rot_theme <- theme(panel.grid = element_blank(),
                    axis.line = element_line(),
                    axis.text.y = element_text(size = 12, colour = 1),
                    axis.text.x = element_text(size = 12, colour = 1),
                    text = element_text(size=12, colour = 1),
                    panel.background = element_blank()
                    )

#Magnetic North
(gN <- ggplot()+
    geom_point(data =dd[dd$rot == 360,],aes(datetime,acc_roll))+
    geom_point(data =dd[dd$rot == 360,],aes(datetime,int_roll), col = 2)+
    geom_point(data =dd[dd$rot == 360,],aes(datetime,roll_pos), col = 3)+
    xlab("") +
    ylab("") +
    geom_hline(yintercept = 0) + 
    annotation_custom(grobTree(textGrob("North",x = 0.1,y = 0.97,gp = gpar(fill=1))))+
    g_rot_theme
)

#Magnetic East
(gE <- ggplot()+
    geom_point(data =dd[dd$rot == 90,],aes(datetime,acc_roll))+
    geom_point(data =dd[dd$rot == 90,],aes(datetime,int_roll), col = 2)+
    geom_point(data =dd[dd$rot == 90,],aes(datetime,roll_neg), col = 3)+
    xlab("") +
    ylab("") +
    geom_hline(yintercept = 0) + 
    annotation_custom(grobTree(textGrob("East",x = 0.1,y = 0.97,gp = gpar(fill=1))))+
    g_rot_theme
)

#Magnetic South
(gS <- ggplot()+
    geom_point(data =dd[dd$rot == 180,],aes(datetime,acc_roll))+
    geom_point(data =dd[dd$rot == 180,],aes(datetime,int_roll), col = 2)+
    geom_point(data =dd[dd$rot == 180,],aes(datetime,roll_pos), col = 3)+
    xlab("") +
    ylab("") +
    geom_hline(yintercept = 0) + 
    annotation_custom(grobTree(textGrob("South",x = 0.1,y = 0.97,gp = gpar(fill=1))))+
    g_rot_theme
)

#Magnetic West
(gW <- ggplot()+
    geom_point(data =dd[dd$rot == 270,],aes(datetime,acc_roll))+
    geom_point(data =dd[dd$rot == 270,],aes(datetime,int_roll), col = 2)+
    geom_point(data =dd[dd$rot == 270,],aes(datetime,roll_pos), col = 3)+
    xlab("") +
    ylab("") +
    geom_hline(yintercept = 0) + 
    annotation_custom(grobTree(textGrob("West",x = 0.1,y = 0.97,gp = gpar(fill=1))))+
    g_rot_theme
)

(g_rot <- ggplot()+
    coord_equal(xlim = c(0, 4), ylim = c(0, 4), expand = FALSE) +
    annotation_custom(ggplotGrob(gN), xmin = 0, xmax = 2, ymin = 2, 
                      ymax = 4) +
    annotation_custom(ggplotGrob(gE), xmin = 2 , xmax = 4, ymin = 2, 
                      ymax = 4) +
    annotation_custom(ggplotGrob(gS), xmin = 0, xmax = 2, ymin = 0, 
                      ymax = 2) +
    annotation_custom(ggplotGrob(gW), xmin = 2 , xmax = 4, ymin = 0, 
                      ymax = 2) +
    xlab("Time (s)") +
    ylab("Roll angle (degrees)") +
    theme(panel.grid = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          text = element_text(size=12),
          panel.background = element_blank()
    )
   )

################################
################################
## Yaw figure
################################
################################

rm(list = ls()) 

env <- "~/Documents/Stefan/"#C:/Users/Schoombie_Asus/" #G:/

options(digits.secs=20)

#Load function to calculate roll and yaw from Mag data

source(paste0(env,"Dropbox/Analysis/R/Scripts/June 2020/load_packages.R"))
source(paste0(env,"/Dropbox/Analysis/R/Scripts/mag_analysis_CAL.R"))

#Load DD data and convert date
# dd_raw <- read.delim("C:/Users/Schoombie_Asus/Dropbox/Analysis/DD/2020_DD_Mag_cal_Library/zero_bank_all_angles_selection_0.txt")
# dd_raw <- read.delim(paste0(env,"/Dropbox/Analysis/DD/2020_DD_Mag_cal_Library/Lib_cal_ALL_selection_split#1_0.txt"))
dd_raw <- read.delim(paste0(env,"/Dropbox/Analysis/DD/2020_DD_Mag_cal_Library/zero_bank_all_angles_selection_0.txt"))
dd_raw$Date <- "1/2/2017"
dd_raw$datetime <- as.POSIXct(paste0(dd_raw$Date," ",dd_raw$Time_dd.hh.mm.ss.sss,".",substr(dd_raw$Decimal.secs,3,5)), format = "%d/%m/%Y %H:%M:%OS",tz = "GMT") #+ c_off
head(dd_raw$datetime)

dd <- dd_raw

#Rotate the axes to match NED convention
# change axes to avionics standard. Measured data has x forward, y to the left and z up
dd$Mag_x <- dd_raw$Mag_x
dd$Mag_y <- -dd_raw$Mag_y
dd$Mag_z <- -dd_raw$Mag_z

#Thin data to allow easier plotting
dd$Total.secs <- dd$Total.secs - dd$Total.secs[1]
# Get reference Mag data using nearest recorded GPS coords
magF <- GetMagneticFieldWMM( 37.859251,-46.875590,5,2019)
MagrefNED <- matrix(c(magF$x,magF$y,magF$z)) *10000 * 1E-9 # nTesla, NED,
# Magrefnorm <- norm(matrix(MagrefNED),type ="F")
normNE <- norm(matrix(MagrefNED[1:2]),type = "F")   # get absolate length of horizontal magnetic field from the earth model
#Calculate mag declination for reference data
Beta <- atan2(MagrefNED[2],MagrefNED[1])
decl <- Beta*180/pi
decl_window <- 0
mag_N <- c(decl -decl_window,decl+decl_window)
mag_S <- c((decl+180) -decl_window,(decl+180)+decl_window)

#Calculate roll and yaw angles (2 solutions - pos and neg)

dd$cam_angle <- atan2(dd$Acc_y,dd$Acc_z)*180/pi
dd <- mag_analysis(dd,100)
dd$yaw_neg <- dd$yaw_neg - decl
dd$yaw_pos <- dd$yaw_pos - decl
dd$yaw_neg[dd$yaw_neg < 0 & !is.na(dd$yaw_neg)] <- 360+(dd$yaw_neg[dd$yaw_neg < 0& !is.na(dd$yaw_neg)] )
dd$yaw_pos[dd$yaw_pos < 0 & !is.na(dd$yaw_pos)] <- 360+(dd$yaw_pos[dd$yaw_pos < 0& !is.na(dd$yaw_pos)] )

dd$mag_heading <- dd$mag_heading - decl
dd$mag_heading[dd$mag_heading < 0 & !is.na(dd$mag_heading)] <- 360+(dd$mag_heading[dd$mag_heading < 0& !is.na(dd$mag_heading)] )

dd$Total.secs <- dd$Total.secs + dd$Decimal.secs

g_rot_theme <- theme(panel.grid = element_blank(),
                     axis.line = element_line(),
                     axis.text.y = element_text(size = 12, colour = 1),
                     axis.text.x = element_text(size = 12, colour = 1),
                     text = element_text(size=12, colour = 1),
                     # scale_x_continuous(breaks = (seq(min(dd$datetime), max(dd$datetime), by = 30))),
                     panel.background = element_blank()
)

dd$select_p <- dd$select_n <- 1
dd$select_p[7000:14000] <- -1
dd$select_p[1:7000] <- 1
dd$select_n[7000:14000] <- 1
dd$select_n[1:7000] <- -1

dd$mag_heading[c(13452:13464)] <- 357
dd$mag_heading[c(13464:13474)] <- NA

dd$Mag.heading[c(13415:13419,13450:13464)] <- 357
dd$Mag.heading[c(13499,13485,13966,13968,13639,13518)] <- 2
dd$Mag.heading[c(13450:13490)] <- NA

ggplot(dd[seq(1,nrow(dd),by = 1),])+
  geom_path(aes(Total.secs,Mag.heading),col = "black",size =1)+
  geom_path(aes(Total.secs,mag_heading),col = "blue",size =1)+
  geom_path(aes(Total.secs,yaw_pos,col = as.factor(select_p)),size =1)+
  geom_path(aes(Total.secs,yaw_neg,col = as.factor(select_n)),size =1)+
  scale_colour_manual(name = '', 
                      values =c('grey','red',"blue"), labels = c('','',''))+
  geom_hline(yintercept = c(0,180,360),lty ="dashed")+
  ylab("Magnetic heading (degrees)")+
  xlab("Time (s)") +
  g_rot_theme

View(dd$Mag.heading)  

dd$h_diff <- abs((dd$Mag.heading-dd$mag_heading))
mean(dd$h_diff,na.rm = T)
sd(dd$h_diff,na.rm = T)
################################
################################
## Pitch figure 
################################
################################
######################################################################
rm(list = ls()) 

env <- "C:/Users/SStho/Documents/loki backup/Stefan/"#"C:/Users/Schoombie_Asus/" #G:/

options(digits.secs=20)

#Load function to calculate roll and yaw from Mag data

source(paste0(env,"/Dropbox/Analysis/R/Scripts/calc_roll2.R"))
source(paste0(env,"/Dropbox/Analysis/R/Scripts/ry_calc.R"))

#Load DD data and convert date
dd_raw <- read.delim(paste0(env,"/Dropbox/Analysis/DD/2020_DD_Mag_cal_Pretoria_2020/Mag_cal_PTA_2020_4pitch_selection_split#1_1.txt"))
dd_raw$Date <- "1/2/2017"
dd_raw$datetime <- as.POSIXct(paste0(dd_raw$Date," ",dd_raw$Time_dd.hh.mm.ss.sss,".",substr(dd_raw$Decimal.secs,3,5)), format = "%d/%m/%Y %H:%M:%OS",tz = "GMT") #+ c_off
head(dd_raw$datetime)

dd <- dd_raw
#Rotate the axes to match NED convention
# change axes to avionics standard. Measured data has x forward, y to the left and z up
dd$Mag_x <- dd_raw$Mag_x
dd$Mag_y <- -dd_raw$Mag_y
dd$Mag_z <- -dd_raw$Mag_z

dd <- dd#[7000:7500,]#Just try with small df first to make sure 

# Get reference Mag data using nearest recorded GPS coords
magF <- GetMagneticFieldWMM(28.249022,-25.829066,5,2019)
MagrefNED <- matrix(c(magF$x,magF$y,magF$z)) *10000 * 1E-9 # nTesla, NED,
# Magrefnorm <- norm(matrix(MagrefNED),type ="F")
normNE <- norm(matrix(MagrefNED[1:2]),type = "F")   # get absolate length of horizontal magnetic field from the earth model
#Calculate mag declination for reference data
Beta <- atan2(MagrefNED[2],MagrefNED[1])
Magscale <- 1

#Calculate roll and yaw angles (2 solutions - pos and neg)
angles <- ry_calc(dd,Magscale,gps)

#Assign newly calculated values to dd dataframe
#Negative is the right solution for this flight
dd$roll_neg <- angles[,2]*180/pi#rollmean(round(angles[,2]*180/pi,0),4,na.pad = T)
dd$yaw_neg <- round(angles[,1]*180/pi,0)
dd$roll_pos <- angles[,4]*180/pi#rollmean(round(angles[,2]*180/pi,0),4,na.pad = T)
dd$yaw_pos<- round(angles[,3]*180/pi,0)

#Assign newly calculated values to dd dataframe
#Negative is the right solution for this flight
dd$roll_neg <- angles[,2]*180/pi#rollmean(round(angles[,2]*180/pi,0),4,na.pad = T)
dd$yaw_neg <- round(angles[,1]*180/pi,0)
dd$roll_pos <- angles[,4]*180/pi#rollmean(round(angles[,2]*180/pi,0),4,na.pad = T)
dd$yaw_pos<- round(angles[,3]*180/pi,0)

#Calculate roll from Mag alone
dd$mag_roll <- (atan2(dd$Mag_y,dd$Mag_z))*180/pi

#Get alpha values for each calculated roll
dd$alpha <- dd$roll_neg - dd$mag_roll

#Make sure roll is on a -180 to 180 axis
dd$roll_neg[dd$roll_neg > 180 & !is.na(dd$roll_neg)] <- dd$roll_neg[dd$roll_neg > 180& !is.na(dd$roll_neg)] -360
dd$roll_neg[dd$roll_neg < -180 & !is.na(dd$roll_neg)] <- dd$roll_neg[dd$roll_neg < -180& !is.na(dd$roll_neg)] +360
dd$roll_pos[dd$roll_pos > 180 & !is.na(dd$roll_pos)] <- dd$roll_pos[dd$roll_pos > 180& !is.na(dd$roll_pos)] -360
dd$roll_pos[dd$roll_pos < -180 & !is.na(dd$roll_pos)] <- dd$roll_pos[dd$roll_pos < -180& !is.na(dd$roll_pos)] +360

#Make sure yaw is on a -180 to 180 axis
dd$yaw_neg[dd$yaw_neg > 180 & !is.na(dd$yaw_neg)] <- dd$yaw_neg[dd$yaw_neg > 180& !is.na(dd$yaw_neg)] -360
dd$yaw_neg[dd$yaw_neg < -180 & !is.na(dd$yaw_neg)] <- dd$yaw_neg[dd$yaw_neg < -180& !is.na(dd$yaw_neg)] +360
dd$yaw_pos[dd$yaw_pos > 180 & !is.na(dd$yaw_pos)] <- dd$yaw_pos[dd$yaw_pos > 180& !is.na(dd$yaw_pos)] -360
dd$yaw_pos[dd$yaw_pos < -180 & !is.na(dd$yaw_pos)] <- dd$yaw_pos[dd$yaw_pos < -180& !is.na(dd$yaw_pos)] +360

#Transform the data to fit on a 0-360 degree frame
dd$mag_roll <- (dd$mag_roll +360)%%360
#Get the difference between successive sample
dd$mag_roll_diff <- NA
dd$mag_roll_diff[2:nrow(dd)] <- diff(dd$mag_roll)

#Interpolate the missing roll angles from the above slopes
dd$roll_neg2 <- dd$roll_neg
dd$roll_pos2 <- dd$roll_pos
for(i in 1:(nrow(dd)-1)){
  if(is.na(dd$roll_neg2[i]))next
  if(is.na(dd$roll_neg2[i+1])){
    dd$roll_neg2[i+1] <- dd$roll_neg2[i] + dd$mag_roll_diff[i]
    # if(dd$roll_neg2[i+1] < -180){dd$roll_neg2[i+1] <- dd$roll_neg2[i+1] + 180}
  }
}
dd$roll_neg2[dd$roll_neg2 < -180 & !is.na(dd$roll_neg2)] <- 360 + dd$roll_neg2[dd$roll_neg2 < -180 & !is.na(dd$roll_neg2)]

dd$mag.pitch <- atan2(dd$Mag_x,dd$Mag_z)*180/pi

(g <- ggplot() + 
    geom_line(data = dd, aes(datetime,roll_neg2))+
    geom_line(data = dd, aes(datetime,roll_pos2),col = 2)+
    geom_line(data = dd, aes(datetime,Pitch.angle),col = 5)+
    geom_line(data = dd, aes(datetime,mag.pitch-170),col = 3)+
    geom_hline(yintercept = 0)
    
  )

for(i in 1:(nrow(dd)-1)){
  if(is.na(dd$roll_pos2[i]))next
  if(is.na(dd$roll_pos2[i+1])){
    dd$roll_pos2[i+1] <- dd$roll_pos2[i] + dd$mag_roll_diff[i]
    # if(dd$roll_pos2[i+1] < -180){dd$roll_pos2[i+1] <-  dd$roll_pos2[i+1] + 180}
  }
}
dd$roll_pos2[dd$roll_pos2 < -180 & !is.na(dd$roll_pos2)] <- 360 + dd$roll_pos2[dd$roll_pos2 < -180 & !is.na(dd$roll_pos2)]

dd$int_roll <- NA
dd$int_roll[1:tmid] <- dd$roll_neg2[1:tmid]
dd$int_roll[(tmid+1):nrow(dd)] <- dd$roll_pos2[(tmid+1):nrow(dd)]
dd$int_roll[dd$int_roll > 200] <- NA

dd$int_yaw <- NA
dd$int_yaw[1:tmid] <- dd$yaw_neg[1:tmid]
dd$int_yaw[(tmid+1):nrow(dd)] <- dd$yaw_pos[(tmid+1):nrow(dd)]
dd$int_yaw[dd$int_yaw < 0 & !is.na(dd$int_yaw)] <- NA#360 - (abs(dd$int_yaw[dd$int_yaw < 0& !is.na(dd$int_yaw)]))
