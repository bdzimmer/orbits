# Copyright (c) 2016 Ben Zimmer. All rights reserved.

# Test visualizations of planetary orbits.

SHOW_VELOCITY  <- FALSE
SCALE_VELOCITY <- 10
WINDOW_WIDTH   <- 640
WINDOW_HEIGHT  <- 480

library(rgl)


cargs <- commandArgs(trailingOnly = TRUE)
inputFile <- cargs[1]
inputFile <- "../../../../test.csv"

states <- read.csv(inputFile, header = FALSE)
colnames(states) <- c("planet", "date", "x", "y", "z", "dx", "dy", "dz")

uniquePlanets <- unique(states$planet)
planets <- lapply(uniquePlanets, function(x) {
  list(planet = x, states = states[states$planet == x, 2:8])
})


# perspective transformation calculations
# https://en.wikipedia.org/wiki/3D_projection#Perspective_projection

cameraTransform <- function(camOrient, camPosition) {
   
  rotX <- matrix(data = c(
    1, 0, 0,
    0, cos(-camOrient[1]), -sin(-camOrient[1]),
    0, sin(-camOrient[1]),  cos(-camOrient[1])
  ), nrow = 3, ncol = 3, byrow = TRUE)
  
  rotY <- matrix(data = c(
     cos(-camOrient[2]), 0, sin(-camOrient[2]),
    0, 1, 0,
    -sin(-camOrient[2]), 0, cos(-camOrient[2])
  ), nrow = 3, ncol = 3, byrow = TRUE)
  
  rotZ <- matrix(data = c(
    cos(-camOrient[3]), -sin(-camOrient[3]), 0,
    sin(-camOrient[3]),  cos(-camOrient[3]), 0,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  
  rotXYZ <- rotZ %*% rotY %*% rotX
  
  rotExpand <- identityMatrix()
  rotExpand[1:3, 1:3] <- rotXYZ
  
  # translationMatrix is an rgl function - easy to understand how it works though
  transMat <- t(translationMatrix(-camPosition[1], -camPosition[2], -camPosition[3]))
  
  print(transMat)
  
  camTransform <- rotExpand %*% transMat
  
  print(camTransform)
  
  camTransform
  
}


perspective <- function (coords, camTrans, viewPos) {
  
  coordsMat <- cbind(as.matrix(coords), 1)
  coordsCamHomo <- camTrans %*% t(coordsMat)
  coordsCam <- coordsCamHomo[1:3, ] / coordsCamHomo[4, ]
  
  data.frame(
    x = viewPos[3] * coordsCam[1, ] / coordsCam[3, ] - viewPos[1],
    y = viewPos[3] * coordsCam[2, ] / coordsCam[3, ] - viewPos[2]
  )
  
}


if (True) {

  # find the maximum planet coordinate
  planetMax <- max(abs(states[, 3:5]))
  
  camOrient <- c(pi * 0.25 , 0, pi)
  camPos <- c(0, - planetMax * 2.5, planetMax * 2.5)
  camTrans <- cameraTransform(camOrient, camPos)
  
  viewerPos <- c(0, 0, 800) # don't understand this too well - z seems to work like zoom
  
  cat("camera position: ", camPos, "\n")
  
  # plot(0, type="n", asp=1, xlim=c(-6, 6), ylim=c(-6, 6), xlab = "", ylab = "")
  plot(0, type="n", asp=1, xlim=range(-400, 400), ylim=c(-300, 300), xlab = "", ylab = "")
  
  for (x in planets) {
    coords2d <- perspective(x$states[, 2:4], camTrans, viewerPos)
    lines(coords2d)
  }

} else {
  
  par3d(windowRect = c(100, 100, WINDOW_WIDTH, WINDOW_HEIGHT))
  
  for (x in planets) {
    lines3d(x$states[, 2:4])
    
    if (SHOW_VELOCITY) {
      velSegments <- do.call(rbind, lapply(1:nrow(x$states), function(i) {
        pos <- x$states[i, 2:4]
        vel <- x$states[i, 5:7]
        rbind(pos, pos + vel * SCALE_VELOCITY)
      }))
      segments3d(velSegments)
    }
    
    text3d(x$states[1, 2:4], texts=x$planet)
  }
  
  axes3d(c('x', 'y', 'z'))
  
  library(tcltk)
  mywait <- function() {
    tt <- tktoplevel()
    tkpack(
      tkbutton(
        tt, text="Close", command = function() tkdestroy(tt)),
        side = "bottom")
    tkbind(tt,"<Key>", function() tkdestroy(tt))
    tkwait.window(tt)
  }
  mywait()
}
