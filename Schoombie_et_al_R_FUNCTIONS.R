

#yaw_calc
yaw_calc <- function(dd){
  # Extract Mag values
  Magxyz <- as.matrix(droplevels(subset(dd,select = c("Mag_x","Mag_y","Mag_z"))))
  Magxyz[,1] <- (Magxyz[,1])
  
  #Normalise x-axis to NE plane
  hx <-  Magxyz[,1]/normNE
  
  #Define yaw and roll vectors (empty)
  angles <- matrix(nrow = length(hx),ncol = 2)
  
  #run through all lines and calculate yaw and roll
  valid_count = 0
  for (i in 1:length( Magxyz[,1])){
    # i <- i+1
    hxi <- hx[i]
    
    #clear values 20 samples on either side of NA values
    if ((hxi < -1) || (hxi > 1)){
      if(i > 20){
        angles[(i-20):i,1:2] = NA
        valid_count = 0
      }
      else(
        angles[i,1:2] = NA
      )
      next
    }
    valid_count <- valid_count + 1
    if(valid_count < 20){
      angles[i,1:2] = NA
      next
    }
    
    #Get yaw from hx - both pos and neg solutions
    #Positive
    yaw_pos <- acos(hxi) + Beta
    if(yaw_pos > pi){
      yaw_pos <- yaw_pos - 2*pi
    }else if(yaw_pos < -pi){
      yaw_pos <- 2*pi + yaw_pos
    }
    yaw_pos <- yaw_pos #+ Beta
    
    #Negative
    yaw_neg <- -acos(hxi) + Beta
    if(yaw_neg > pi){
      yaw_neg <- yaw_neg - 2*pi
    }else if(yaw_neg < -pi){
      yaw_neg <- 2*pi + yaw_neg
    }
    yaw_neg <- yaw_neg #+ Beta
    
    #Save values to matrix 
    angles[i,1] <- yaw_pos
    angles[i,2] <- yaw_neg
  }
  return(angles)
}

#ry_calc
ry_calc <- function(dd,Magscale,col_names ){
  # Extract Mag values and adjust with scale factor
  Magxyz <- as.matrix(droplevels(subset(dd,select =col_names)))
  Magxyz[,1] <- (Magxyz[,1])*Magscale
  Magxyz[,2] <- (Magxyz[,2])*Magscale
  Magxyz[,3] <- (Magxyz[,3])*Magscale
  
  #Normalise x-axis to NE plane
  hx <-  Magxyz[,1]/normNE
  
  #Define yaw and roll vectors (empty)
  angles <- matrix(nrow = length(hx),ncol = 6)
  
  #run through all lines and calculate yaw and roll
  na_start = F
  valid_count = 0
  for (i in 1:length( Magxyz[,1])){
    # i <- i+1
    hxi <- hx[i]
    Hxi <- Magxyz[,1][i]
    Hyi <- Magxyz[,2][i]
    Hzi <- Magxyz[,3][i]
    
    #Find points where hxi is not valid for the following acos function
    #Here we also remove values leading up to the invalid hxi values
    if ((hxi < -1) || (hxi > 1)){
      if(i > 20){
        angles[(i-20):i,1:6] = NA
        valid_count = 0
      }
      else(
        angles[i,1:6] = NA
      )
      
      next
    }
    valid_count <- valid_count + 1
    
    #If we don't have 20 successive valid hxi point we remove the estimations
    if(valid_count < 20){
      angles[i,1:6] = NA
      next
    }
    
    #Calculate the first solution for yaw and roll (positive)
    yaw_pos <- acos(hxi) 
    if(yaw_pos > pi){
      yaw_pos <- yaw_pos - 2*pi
    }else if(yaw_pos < -pi){
      yaw_pos <- 2*pi + yaw_pos
    }
    yaw_pos <- yaw_pos + Beta
    roll_pos <- calc_roll(yaw_pos,Hyi,Hzi)
    
    #Calculate the second solution for yaw and roll (negative)
    yaw_neg <- -acos(hxi) 
    if(yaw_neg > pi){
      yaw_neg <- yaw_neg - 2*pi
    }else if(yaw_neg < -pi){
      yaw_neg <- 2*pi + yaw_neg
    }
    yaw_neg <- yaw_neg + Beta
    roll_neg <- calc_roll(yaw_neg,Hyi,Hzi)
    
    alpha_pos <- roll_pos - atan2(Hyi , Hzi )  
    alpha_neg <- roll_neg - atan2(Hyi , Hzi ) 
    
    angles[i,1] <- yaw_pos
    angles[i,2] <- roll_pos
    angles[i,3] <- yaw_neg
    angles[i,4] <- roll_neg
    angles[i,5] <- alpha_pos
    angles[i,6] <- alpha_neg
    
  }
  return(angles)
}

#calc_roll
calc_roll <- function(yaw,Hy,Hz){
  #First calculate the alpha values from the reference magnetic field data and the calculated yaw
  alpha <- atan2(MagrefNED[3],normNE * sin(yaw - Beta))
  normNED <- sqrt(normNE^2 * (sin(yaw-Beta))^2 + (MagrefNED[3])^2)
  #Next calculate the roll
  rollx <- atan2(Hz , -Hy ) - alpha
  return(rollx)
}

mag_analysis <- function(dd,window){
  #Get yaw angles
  yaw_angles <- yaw_calc(dd)
  
  dd$yaw_pos <- yaw_angles[,1]*180/pi
  dd$yaw_neg <- yaw_angles[,2]*180/pi
  
  #Exclude points where heading is close to magnetic north/south
  dd$yaw_pos[(dd$yaw_pos<mag_N[2] & dd$yaw_pos > mag_N[1]) | (dd$yaw_pos<mag_S[2] & dd$yaw_pos > mag_S[1])] <- NA
  dd$yaw_neg[(dd$yaw_neg<mag_N[2] & dd$yaw_neg > mag_N[1]) | (dd$yaw_neg<mag_S[2] & dd$yaw_neg > mag_S[1])] <- NA
  dd$roll_neg <- dd$roll_pos <- NA
  
  #Calculate roll angle (positive and negative respectively)
  dd$alpha_pos <- atan2(normNE * sin(dd$yaw_pos/180*pi - Beta),MagrefNED[3])
  
  #Mark points where alpha values are valid
  #Use the previous valid alpha value
  dd$qual <- 2
  dd$qual[!is.na(dd$alpha_pos)] <- 1 
  dd$alpha_pos[which(!is.na(dd$alpha_pos))[1]:nrow(dd)] <- na.locf(dd$alpha_pos)
  dd$alpha_neg <- atan2(normNE * sin(dd$yaw_neg/180*pi - Beta),MagrefNED[3])
  dd$alpha_neg[which(!is.na(dd$alpha_neg))[1]:nrow(dd)] <- na.locf(dd$alpha_neg)
  
  #Interpolate the missing roll angles by using the above alpha values
  #Positive solution
  dd$roll_pos_raw <- atan2(dd$Mag_y , dd$Mag_z ) #+ dd$alpha_pos
  dd$roll_pos <- dd$roll_pos_raw + dd$alpha_pos
  dd$roll_pos[dd$roll_pos < -pi & !is.na(dd$roll_pos)] <-  2*pi + dd$roll_pos[dd$roll_pos < -pi & !is.na(dd$roll_pos)]
  dd$roll_pos[dd$roll_pos > pi & !is.na(dd$roll_pos)] <-   dd$roll_pos[dd$roll_pos > pi & !is.na(dd$roll_pos)] - 2*pi 
  dd$roll_pos <- dd$roll_pos*180/pi
  #Convert to degrees
  dd$roll_pos_raw <- dd$roll_pos_raw*180/pi
  dd$alpha_pos <- dd$alpha_pos*180/pi
  
  #Negative solution
  dd$roll_neg_raw <- atan2(dd$Mag_y , dd$Mag_z ) #+ dd$alpha_neg
  dd$roll_neg <- dd$roll_neg_raw + dd$alpha_neg
  dd$roll_neg[dd$roll_neg < -pi & !is.na(dd$roll_neg)] <-  2*pi + dd$roll_neg[dd$roll_neg < -pi & !is.na(dd$roll_neg)]
  dd$roll_neg[dd$roll_neg > pi & !is.na(dd$roll_neg)] <-   dd$roll_neg[dd$roll_neg > pi & !is.na(dd$roll_neg)] - 2*pi 
  dd$roll_neg <- dd$roll_neg*180/pi
  #Convert to degrees
  dd$roll_neg_raw <- dd$roll_neg_raw*180/pi
  dd$alpha_neg <- dd$alpha_neg*180/pi
  
  #Decide which solution is correct (pos vs neg)
  dd$roll <- NA
  dd$yaw <- NA
  dd$alpha <- NA
  dd$roll_raw <- NA
  
  rm_seq <- seq(1,nrow(dd)-window,window)#1:(nrow(dd)-160)
  for(i in rm_seq){
    if(is.na(mean(dd$roll_neg[i:(i+window)],na.rm = T)))next
    if(
      mean(abs(dd$roll_neg[i:(i+window)]),na.rm = T) < mean(abs(dd$roll_pos[i:(i+window)]),na.rm = T)
    ){
      dd$roll[i:(i+window)] <- dd$roll_neg[i:(i+window)]
      dd$yaw[i:(i+window)] <- dd$yaw_neg[i:(i+window)]
      dd$alpha[i:(i+window)] <- dd$alpha_neg[i:(i+window)]
      dd$roll_raw[i:(i+window)] <- dd$roll_neg_raw[i:(i+window)]
    }else if(
      mean(abs(dd$roll_pos[i:(i+window)]),na.rm = T) < mean(abs(dd$roll_neg[i:(i+window)]),na.rm = T)
    ){
      dd$roll[i:(i+window)] <- dd$roll_pos[i:(i+window)]
      dd$yaw[i:(i+window)] <- dd$yaw_pos[i:(i+window)]
      dd$alpha[i:(i+window)] <- dd$alpha_pos[i:(i+window)]
      dd$roll_raw[i:(i+window)] <- dd$roll_pos_raw[i:(i+window)]
    }
  }
  
  dd$roll_int <- round(norm_int_roll(dd$roll,dd$Mag_y,dd$Mag_z),0)
  dd$alpha2 <- dd$roll_int - dd$roll_raw
  dd <- droplevels(dd[which(!is.na(dd$roll_int))[1]:nrow(dd),])
  
  ##########
  #Now calculate heading from new roll values
  ##########
  #Normalize mag data
  fm <- 1
  dd$Mag_y_R <- cos(dd$roll_int/180*pi)*(dd$Mag_y/fm) + sin(dd$roll_int/180*pi)*(dd$Mag_z/fm)
  dd$Mag_y_R2 <- cos(dd$cam_angle/180*pi)*(dd$Mag_y/fm) + sin(dd$cam_angle/180*pi)*(dd$Mag_z/fm)
  dd$Mag_x_R <- dd$Mag_x/fm
  dd$mag_heading2 <- dd$mag_heading <-NA# dd$yaw
  dd$mag_heading[is.na(dd$mag_heading)] <- atan2(-dd$Mag_y_R[is.na(dd$mag_heading)],dd$Mag_x_R[is.na(dd$mag_heading)])*180/pi + Beta*180/pi
  dd$mag_heading2[is.na(dd$mag_heading2)] <- atan2(-dd$Mag_y_R2[is.na(dd$mag_heading2)],dd$Mag_x_R[is.na(dd$mag_heading2)])*180/pi + Beta*180/pi
  # dd$mag_heading <- atan2(-dd$Mag_y_R,dd$Mag_x_R)*180/pi + Beta*180/pi
  dd$mag_heading[!is.na(dd$mag_heading) & dd$mag_heading < -180] <- dd$mag_heading[!is.na(dd$mag_heading) & dd$mag_heading < -180]%%180
  dd$mag_heading[!is.na(dd$mag_heading) & dd$mag_heading > 180] <- dd$mag_heading[!is.na(dd$mag_heading) & dd$mag_heading > 180]%%180
  dd$mag_heading2[!is.na(dd$mag_heading2) & dd$mag_heading2 < -180] <- dd$mag_heading2[!is.na(dd$mag_heading2) & dd$mag_heading2 < -180]%%180
  dd$mag_heading2[!is.na(dd$mag_heading2) & dd$mag_heading2 > 180] <- dd$mag_heading2[!is.na(dd$mag_heading2) & dd$mag_heading2 > 180]%%180
  
  dd_out <- subset(dd, select = c("id","year","fls","gps_lon","gps_lat","Mag_x","Mag_y","Mag_z","roll","cam_angle_interp","mag_heading"))
  
  return(dd_out)
}

norm_int_roll <- function(i_roll, iy, iz){
  d1 <- normalize(i_roll,method = "range")
  d2 <- normalize((atan2(iy,iz)*180/pi + 360)%%360,method = "range")
  ddiff <- diff(d2)
  # ddiff[ddiff > 0.1] <- 0
  # ddiff[ddiff < -0.1] <- 0
  for(i in 1:(length(i_roll)-1)){
    if(is.na(d1[i]))next
    if(is.na(d1[i+1])){
      d1[i+1] <- d1[i] + ddiff[i]
    }
  }
  roll<- (d1*(summary(i_roll)[6] - summary(i_roll)[1]) + summary(i_roll)[1])#*180/pi
  return(roll)
}
