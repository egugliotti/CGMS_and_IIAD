#Functions to create the color schemes in the flyout tables depending on the sensor
colormatrix<- function(dat){
  #If data in table columns (except for first) is equal to the thing Below, assign the color given ifelse, then do the things after
  ifelse(dat[, -c(1)] == "SS Early AM", "#9ECAE1", #Light Blue
         ifelse(dat[, -c(1)] == "SS Mid AM", "#4292C6", #Medium Blue
                ifelse(dat[, -c(1)] == "SS PM", "#084594", #Dark Blue
                       ifelse(dat[, -c(1)] == "L1", "#FDAE61", #Orange
                              ifelse(dat[, -c(1)] == "SS Drifting", "#08306B",
                                     ifelse(dat[, -c(1)]< 0 | dat[, -c(1)]> 0, "#980043", #Pink-Red
                                            ifelse(dat[, -c(1)] == 0 , "#49006A",  #Purple
                                                ifelse(dat[, -c(1)] == "TBD","#E8E419FF", "white"))))))))}

colormatrix_energetic_particle_LEO<- function(dat){
  ifelse(dat[, -c(1)] == "SS Early AM", "#9ECAE1", #Light Blue
         ifelse(dat[, -c(1)] == "SS Mid AM", "#4292C6", #Medium Blue
                ifelse(dat[, -c(1)] == "SS PM", "#084594", #Dark Blue
                       ifelse(dat[, -c(1)] == "L1", "#FDAE61", #Orange
                              ifelse(dat[, -c(1)] == "SS Drifting", "#08306B",
                                                      ifelse(dat[, -c(1)] == "TBD","#E8E419FF", "white"))))))}

colormatrix_energetic_particle_GEO_high<- function(dat){
  ifelse(dat[, -c(1)] == "TBD","#D6604D",
        ifelse(dat[, -c(1)]< 0 & dat[, -c(1)] < 128.2, "#980043", #Pink-Red
              ifelse(dat[, -c(1)] == 128.2, "#49006A",  #Purple
                    ifelse(dat[, -c(1)]> 0, "#C994C7", #Pale Purple
                          ifelse(dat[, -c(1)] == 0 , "#49006A", "white")))))} #Purple


colormatrix_energetic_particle_GEO_low<- function(dat){
  ifelse(dat[, -c(1)]< 0, "#980043", #Pink-Red
         ifelse(dat[, -c(1)]> 0, "#C994C7", #Pale Purple
                ifelse(dat[, -c(1)] == "TBD","#E8E419FF", "white")))}



colormatrix2 <- function(dat){
  ifelse(dat[, -c(1)] == "SS Early AM", "#9ECAE1", #Light Blue
         ifelse(dat[, -c(1)] == "SS Mid AM", "#4292C6", #Medium Blue
                ifelse(dat[, -c(1)] == "SS PM", "#084594", #Dark Blue
                       ifelse(dat[, -c(1)] == "L1", "#FDAE61", #Orange
                              ifelse(dat[, -c(1)] == "SS Drifting", "#08306B", #dark dark blue
                                     ifelse(dat[, -c(1)] == "SS Drift Low Incl", "mediumblue",
                                        ifelse(dat[, -c(1)]< 86.5 & dat[, -c(1)]> 0, "#D6604D", #Pink-Red
                                            ifelse(dat[, -c(1)]< 0, "#980043", #Pink
                                                   ifelse(dat[, -c(1)] == 0 , "#49006A",  #Purple
                                                          ifelse(dat[, -c(1)] >= 86.5 , "#C994C7", #Pale Purple
                                                                 ifelse(dat[, -c(1)] == "TBD","#E8E419FF", "white")))))))))))}

colormatrix3 <- function(dat){
  ifelse(dat[, -c(1)] == "Early AM", "#9ECAE1", #Light Blue
         ifelse(dat[, -c(1)] == "Mid AM", "#4292C6", #Medium Blue
                ifelse(dat[, -c(1)] == "PM", "#084594", #Dark Blue
                       ifelse(dat[, -c(1)] == "L1", "#FDAE61", #Orange
                              ifelse(dat[, -c(1)] == "Drifting", "#08306B",
                                     ifelse(dat[, -c(1)]<= -100, "#980043", #Pink
                                        ifelse(dat[, -c(1)]== -137 | dat[, -c(1)]== -137.2, "#980043", #Pink
                                            ifelse(dat[, -c(1)]> -100 & dat[, -c(1)]< -20, "lightpink", #Light Pink
                                                   ifelse(dat[, -c(1)]>= -20 & dat[, -c(1)]< 0, "#D6604D", #Pink-Red (For Elektro)    
                                                          ifelse(dat[, -c(1)]< 86.5 & dat[, -c(1)]> 0, "#D6604D", #Pink-Red
                                                                 ifelse(dat[, -c(1)] >= 86.5 , "#C994C7", #Pale Purple, 
                                                                             ifelse(dat[, -c(1)] == 0 , "#49006A",  #Purple
                                                                                ifelse(dat[, -c(1)] == "TBD","#E8E419FF", "white")))))))))))))}

#For Multi-purpsoe Meterological Imager GEO
colormatrix4 <- function(dat){
       ifelse(dat[, -c(1)] == "Early AM", "#9ECAE1", #Light Blue
              ifelse(dat[, -c(1)] == "Mid AM", "#4292C6", #Medium Blue
                    ifelse(dat[, -c(1)] == "PM", "#084594", #Dark Blue
                       ifelse(dat[, -c(1)] == "L1", "#FDAE61", #Orange
                              ifelse(dat[, -c(1)] == "Drifting", "#08306B",
                                     ifelse(dat[, -c(1)]< -100, "#980043", #Pink
                                            ifelse(dat[, -c(1)] == 105.0, "#C994C7", #Pale Purple,
                                                   ifelse(dat[, -c(1)] == 123.5, "#C994C7", #Pale Purple,
                                                          ifelse(dat[, -c(1)] == 128.2, "#C994C7", #Pale Purple,
                                                                 ifelse(dat[, -c(1)] == 140.0, "#C994C7", #Pale Purple,
                                                                        ifelse(dat[, -c(1)] == 86.5, "#C994C7", #Pale Purple,
                                                                              ifelse(dat[, -c(1)]> -100 & dat[, -c(1)]< -20, "lightpink", #Light Pink
                                                                                    ifelse(dat[, -c(1)]> -20 & dat[, -c(1)]< 0, "#D6604D", #For Elektro    
                                                                                          ifelse(dat[, -c(1)]== 82 | dat[, -c(1)] == 74.0, "#ff83fa", # For Insat
                                                                                                ifelse( dat[, -c(1)] == 76.0,"#D6604D", #For Elektro
                                                                                                      ifelse(dat[, -c(1)] > 160, "#D6604D", #For Elektro
                                                                                                            ifelse(dat[, -c(1)] == 0.0 , "#49006A",  #Purple
                                                                                                                   ifelse(dat[, -c(1)] == 3.5|9.5 , "#49006A",  #Purple
                                                                                                                      ifelse(dat[, -c(1)] == "TBD","#E8E419FF", "white")))))))))))))))))))}
#For X-Ray Spectrograph
colormatrix_xray <- function(dat){
                       ifelse(dat[, -c(1)] == "L1", "#FDAE61", #Orange
                              ifelse(dat[, -c(1)]== -137 | dat[, -c(1)]== -137.2 , "#980043", #Pink
                                     ifelse(dat[, -c(1)]== -75 | dat[, -c(1)]== -75.2 , "#980043", #Pink
                                            ifelse(dat[, -c(1)]== -14.5 | dat[, -c(1)] == 76, "#D6604D", #Pink-Red
                                                   ifelse(dat[, -c(1)] == 86.5 , "#980043", #Pink
                                                                 ifelse(dat[, -c(1)] == "TBD","#E8E419FF", "white"))))))}
# For Magnetometer
colormatrix_mag <- function(dat){
  ifelse(dat[, -c(1)] == "Early AM", "#9ECAE1", #Light Blue
         ifelse(dat[, -c(1)] == "Mid AM", "#4292C6", #Medium Blue
                ifelse(dat[, -c(1)] == "PM", "#084594", #Dark Blue
                       ifelse(dat[, -c(1)] == "L1", "#FDAE61", #Orange
                              ifelse(dat[, -c(1)] == "Drifting", "#08306B",
                                     ifelse(dat[, -c(1)]<= -100, "#980043", #Pink
                                            ifelse(dat[, -c(1)]== -137 | dat[, -c(1)]== -137.2, "#980043", #Pink
                                                          ifelse(dat[, -c(1)]== -75 | dat[, -c(1)]== -75.2, "lightpink", ##Pale Purple
                                                                 ifelse(dat[, -c(1)]< 86.5 & dat[, -c(1)]> 0, "#C994C7", #Pale Purple
                                                                        ifelse(dat[, -c(1)] >= 86.5 , "#C994C7", #Pale Purple, 
                                                                               ifelse(dat[, -c(1)] == 0 , "#49006A",  #Purple
                                                                                      ifelse(dat[, -c(1)] == "TBD","#E8E419FF", "white"))))))))))))}

# For radio occultation
colormatrix_ro <- function(dat){
  ifelse(dat[, -c(1)] == "Sun-synchronous", "#9ECAE1", #Light Blue
                              ifelse(dat[, -c(1)] == "SS Drifting", "#08306B",
                                     ifelse(dat[, -c(1)] == "SS Drift Low Incl", "mediumblue",
                                             ifelse(dat[, -c(1)] == "TBD","#E8E419FF", "white"))))}