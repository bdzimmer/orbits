# Copyright (c) 2016 Ben Zimmer. All rights reserved.

# Rough estimations of acceleration required for interplanetary flights.
# 2016-07

flightTime <- 4.0 # days
distance <- 7.0   # AU

halfFlightTime <- flightTime / 2

# for constant acceleration, starting and ending at 0
accel <- (4 * distance) / (flightTime * flightTime)
halfDistance <- distance / 2.0

cat("acceleration:", accel, "AU / day^2")

res = 0.01

times <- seq(0, flightTime, res)

velocities <- sapply(times, function(t) {
  if (t < halfFlightTime) {
    accel * t
  } else {
    - accel * t + accel * flightTime
  }
})

positions <- sapply(times, function(t) {
  if (t < halfFlightTime) {
    0.5 * accel * t * t
  } else {
    - 0.5 * accel * t * t + accel * flightTime * t - 0.25 * accel * flightTime * flightTime
  }
})

# graphs of position and velocity

par(mfrow=c(2, 1))
plot(times, velocities, xlab = "d", ylab = "AU/d", type = "l", lwd = 3)
title(main = "Velocity")
plot(times, positions,  xlab = "d", ylab = "AU",   type = "l", lwd = 3)
title(main = "Position")