# SIMULATION OF THE WINGS FOR LIFE GOAL CALCULATOR
# https://www.wingsforlifeworldrun.com/en/goal-calculator

# load packages
{
  library(ggplot2) # for plotting output
  library(dplyr) # for manipulation
}

# GENERAL INFORMATION ----
# all times measured in minutes 

kmh_to_minskm <- function(kmh){ return(60/kmh) } # km/h to mins/km
minskm_to_kmh <- function(minskm){ return(60/minskm) }  # mins/km to km/h
minskm_to_minsmile <- function(minskm){ return(minskm*1.60934) } # km to mile
mins_to_hrsmins <- function(mins){ # number of minutes to hrs:mins string
  hrsmins <- paste(floor(mins/60),"h ",ceiling(mins%%60),"min",sep="")
  return(hrsmins)
}
mins_to_minsecs <- function(mins){ # minutes dec to mins:secs, for speed
  minssecs <- paste(floor(mins),ceiling((mins%%1)*60),sep=":");
  minssecs <- paste(minssecs,"min/km",sep="")
  return(minssecs)
}

# CAR INFORMATION ----
# the car does not start moving for 30 minutes
# it then proceeds at 14 km/h for 30 mins, then 15 km/h usw.

time_lag <-  30 # half an hour before starting
initial_speed <- 14 # km/h
speed_increment <- c(
  0, # initially just 14 km/h
  1,1,1,1,1, # first 2.5h increment by 1 km/h
  4,4,4,4 # next 2h increment by 4 km/h
)
all_speeds <- c(
  0,
  14,15,16,17,18,
  22,26,30,34
)
time_increment <- 30 # speed increases every half hour
max_time <- 5*60

# ....car functions ----
calc_current_car_speed <- function(current_time){
  # current_time in minutes since start
  # output speed in km/h
  if(current_time < time_lag){ # doesn't start moving for half an hour
    current_car_speed <- 0
    return(current_car_speed)
  }
  if(current_time>=max_time){
    current_car_speed <- max(all_speeds)
    return(current_car_speed)
  }
  current_speed_increment <- 
    sum(speed_increment[1:round(current_time/time_increment,1)]) # sum of prev increments
  current_car_speed <- initial_speed+current_speed_increment
  return(current_car_speed)
}

get_current_car_speed <- function(current_time){ 
  # current_time in minutes since start
  # output current speed in prettier format
  current_car_speed <- calc_current_car_speed(current_time)
  print(paste(
    "Current Speed:",current_car_speed,"km/h"
  ))
}

calc_current_car_distance <- function(current_time){
  # current_time in minutes since start
  # output distance travelled in km
  if(current_time < time_lag){ # doesn't start moving for half an hour
    current_car_distance <- 0
    return(current_car_distance)
  }
  current_speed <- calc_current_car_speed(current_time)
  
  # get completed speed increments
  completed_time <- (round(current_time/time_increment,0)) # half hours
  if(current_time>=max_time){
    completed_distance <- sum((time_increment/60)*all_speeds)+
      max(all_speeds*(current_time-max_time)/60) # car has reached top speed
  }else{
    completed_distance <- sum((time_increment/60)*all_speeds[1:completed_time])
  }
  
  #get partial speed increments
  additional_time <- current_time-(completed_time*time_increment)
  additional_distance <- (additional_time/60)*current_speed
  current_car_distance <- completed_distance + additional_distance
  return(current_car_distance)
}

get_current_car_distance <- function(current_time){ 
  # output current speed in prettier format
  current_car_distance <- calc_current_car_distance(current_time)
  print(paste(
    "Current Distance Travelled:",round(current_car_distance,1),"km"
  ))
}

calc_needed_car_time <- function(current_distance){
  # time needed for car to reach current_distance in km
  # output in minutes
  
  all_distances <- cumsum((time_increment/60)*all_speeds)
  completed_time <- length(all_distances[all_distances<current_distance]) # completed half hours
  additional_distance <- current_distance-all_distances[completed_time] # kms
  additional_time <- (additional_distance/all_speeds[completed_time+1])*60
  
  needed_time <- (completed_time*time_increment)+additional_time
  return(needed_time)
}

get_needed_car_time <- function(current_distance){
  out_time <- calc_needed_car_time(current_distance)
  print(paste(
    "Time for car to reach ",current_distance,"km: ",
    mins_to_hrsmins(out_time),sep=""
  ))
}

# example calculations
# for(t in seq(0,120,15)){
#   get_current_car_distance(t)
# }


# RUNNER INFORMATION ----
# runners get a half hour head start on the car
# when the car reaches the same distance as the runner, the runner is finished

# ....runner functions ----

time_to_speed <- function(total_time){
  # convert total race time to speed needed to beat the car
  # output speed in mins/km
  
  # get distance car has travelled
  car_distance <- calc_current_car_distance(total_time)
  # needed speed to reach that distance
  needed_speed <- total_time/car_distance # in mins/km
  return(needed_speed)
}

distance_to_speed <- function(total_distance){
  # convert total race distance to speed needed to beat the car
  # output speed in mins/km
  car_time <- calc_needed_car_time(total_distance)
  needed_speed <- 1/(total_distance/(car_time))
  return(needed_speed)
}

get_distance_to_speed <- function(total_distance){
  out_speed <- distance_to_speed(total_distance)
  print(paste(
    "Minimum running speed required to reach ",total_distance,"km: ",
    mins_to_minsecs(out_speed),sep=""
  ))
}

speed_to_distance <- function(average_speed){
  # convert average speed to distance at finish
  # output distance in km
  
  # approximate using minute by minute, because non-linear maths is hard
  all_times <- seq(32,600,1)
  all_times_speeds <- c()
  for(at in all_times){
    all_times_speeds <- c(all_times_speeds,time_to_speed(at)) # mins/km
  }
  top_speed <- length(all_times_speeds[all_times_speeds>=average_speed])
  top_time <- all_times[top_speed]
  top_distance <- calc_current_car_distance(top_time)
  return(top_distance)
}

get_speed_to_distance <- function(mins,secs){
  average_speed <- mins+(secs/60)
  out_distance <- speed_to_distance(average_speed)
  print(paste(
    "Estimated distance run at finish: ",
    round(out_distance,1),"km",sep=""
  ))
}


# DRAW GRAPHS ----

# ....speed of the car using time and distance
p_time <- seq(0,5*60,15) # 0 to 4 hrs in 15 minute increments
p_distance <- c()
for(pt in p_time){
  p_distance <- c(p_distance,
                  calc_current_car_distance(pt))
}
plot(p_time,p_distance);lines(p_time,p_distance)
df1 <- data.frame(time=p_time,distance=p_distance)
ggplot(df1,aes(x/60,y))+
  geom_area(fill="grey")+geom_line()+
  theme_bw()+labs(x="Hours",y="Distance, km",
  title="Car Distance over Time")


# ....required speed to reach a given time
p_speed <- c()
for(pt in p_time){
  p_speed <- c(p_speed,
               time_to_speed(pt))
}
plot(p_time,p_speed);lines(p_time,p_speed)
df2 <- data.frame(time=p_time,speed=p_speed)
ggplot(df2,aes(x/60,y))+
  geom_line()+ylim(0,15)+
  theme_bw()+labs(x="Hours",y="Speed, mins/km",
                  title="Speed Needed per Race Time")

# ....distance vs speed needed
p_distance <- seq(1,30,1)
p_speed <- c()
for(pd in p_distance){
  p_speed <- c(p_speed,distance_to_speed(pd))
}
df3 <- data.frame(distance=p_distance,speed=p_speed)
ggplot(df3,aes(x,y))+
  geom_line()+ylim(0,35)+
  theme_bw()+labs(x="Distance, km",y="Speed, mins/km",
                  title="Speed Needed per Race Distance")


# car distance vs speed needed
df4 <- merge(df1,df2,by="time")

ggplot(df4,aes(x=time/60,y=speed))+theme_bw()+
  geom_area(aes(y=distance),fill="grey",alpha=0.5)+
  geom_line(aes(y=speed*3))+
  labs(x="Time, hours",title="Speed required vs Time of finishing")+xlim(0,3)+
  scale_y_continuous(name="Distance, km",
                     sec.axis=sec_axis(~./3,name="Speed, mins/km"))


# CALCULATOR ----

# ....where will the car be at a given time?
get_current_car_distance(current_time=60) # enter number of minutes

# ....how long will it take the car to reach a given distance?
get_needed_car_time(current_distance=12) # enter number of kilometres

# ....how fast do you need to run to reach a given distance?
get_distance_to_speed(total_distance=12) # enter number of kilometres

# ....how far will you reach if you run at a given average speed?
get_speed_to_distance(mins=5,secs=30)


