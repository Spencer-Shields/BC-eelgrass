library(tidyverse)

#calculate swath width given field of view and altitude

swidth = function(altitude, fov){
  2*altitude*tan((fov*pi/180)/2)
}
