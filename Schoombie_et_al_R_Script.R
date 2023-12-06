# Cite this article: Schoombie S, Wilson RP, Ryan PG. 2023 
# A novel approach to seabird posture estimation: finding roll and yaw angles of dynamic soaring albatrosses using tri-axial
# magnetometers. R. Soc. Open Sci. 10: 231363. https://doi.org/10.1098/rsos.231363

# 2023 The Authors. Published by the Royal Society under the terms of the Creative
# Commons Attribution License http://creativecommons.org/licenses/by/4.0/, which permits
# unrestricted use, provided the original author and source are credited

#This script can be used to reproduce the data used in analysis of the above mentioned publication.

# 1. First download the data from the following link: https://figshare.com/s/fe73f21171845d069b99 and the folder named:
# "Schoombie_et_al_DATA" to your local drive

# 2. Open the downloaded data
#specify the file location
  file_path <- "~/Schoombie_et_al_DATA.csv" #insert the file path where you saved the folder from 1. above
  data_all <- read.csv(file_path)

# 3. Load the required functions from
  function_path <- "~/Schoombie_et_al_FUNCTIONS.R" #insert the file path where you saved the folder from 1. above
  source(function_path)
  
# 4. Choose an individual to work with
  birdID <- unique(data_all$id)[1]
  birdFLIGTH <- 2
  data <- droplevels(subset(data_all, id == birdID & fls == birdFLIGTH ))
  
  #only use data where validation roll from video data is available
  data <- data[!is.na(data$cam_angle_interp),]
  
# 5. Get the reference magnetic field from the World Magnetic Model (WMM)
  library(wmm) #load the wmm library

  #get the first long and lat values, or specify the deployment location (37.859251,-46.875590)
  wmm_long <- data$gps_lon[which(!is.na(data$gps_lon))[1]]# or 37.859251
  wmm_lat <- data$gps_lat[which(!is.na(data$gps_lat))[1]]# or -46.875590
  
  #Get the year of deployment
  wmm_year <- data$year[1]
  
  #Now extract the data from the WMM
  magF <- GetMagneticFieldWMM(wmm_long,wmm_lat,5,wmm_year)
  
  #Calculate the components of the reference magnetic field in nTesla units (note that this is in the North,East,Down (NED) convention)
  MagrefNED <- matrix(c(magF$x,magF$y,magF$z)) *10000 * 1E-9 
  
  # get absolate length of horizontal magnetic field from the earth model
  normNE <- norm(matrix(MagrefNED[1:2]),type = "F")   
  
  #Calculate mag declination for reference data (in radians)
  Beta <- atan2(MagrefNED[2],MagrefNED[1])
  
  #Insert the correction scale for the magnetometer data (1 - no correction)
  Magscale <- 1
  
  
  #Calculate mag declination for reference data
  decl <- Beta*180/pi
  #Choose the window (in degrees) around magnetic north and south to interpolate
  decl_window <- 1
  mag_N <- c(decl -decl_window,decl+decl_window)
  mag_S <- c((decl+180) -decl_window,(decl+180)+decl_window)
  
  #Choose a window size to look for the correct value between the positive and negative solution
  correction_window <- 400 # 400 samples at 40 Hz is 10 s, which is the approximate duration of a dynamic soaring cycle in wandering albatrosses
  library(zoo)
  library("BBmisc")
  data <- mag_analysis(data,correction_window)

 