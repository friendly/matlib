pkgname <- "ETRep"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('ETRep')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("check_Tube_Legality")
### * check_Tube_Legality

flush(stderr()); flush(stdout())

### Name: check_Tube_Legality
### Title: Check the Legality of an Elliptical Tube (ETRep)
### Aliases: check_Tube_Legality

### ** Examples

# Load tube
data("colon3D")
check_Tube_Legality(tube = colon3D)



cleanEx()
nameEx("create_Elliptical_Tube")
### * create_Elliptical_Tube

flush(stderr()); flush(stdout())

### Name: create_Elliptical_Tube
### Title: Create a Discrete Elliptical Tube (ETRep)
### Aliases: create_Elliptical_Tube

### ** Examples

numberOfFrames<-15
EulerAngles_alpha<-c(rep(0,numberOfFrames))
EulerAngles_beta<-c(rep(-pi/20,numberOfFrames))
EulerAngles_gamma<-c(rep(0,numberOfFrames))
EulerAngles_Matrix<-cbind(EulerAngles_alpha,
                          EulerAngles_beta,
                          EulerAngles_gamma)
tube <- create_Elliptical_Tube(numberOfFrames = numberOfFrames,
                               method = "basedOnEulerAngles",
                               EulerAngles_Matrix = EulerAngles_Matrix,
                               ellipseResolution = 10,
                               ellipseRadii_a = rep(3, numberOfFrames),
                               ellipseRadii_b = rep(2, numberOfFrames),
                               connectionsLengths = rep(4, numberOfFrames),
                               plotting = FALSE)
# Plotting
## Not run: 
##D  plot_Elliptical_Tube(tube = tube,plot_frames = FALSE,
##D                       plot_skeletal_sheet = TRUE,
##D                       plot_r_project = FALSE,
##D                       plot_r_max = FALSE,add = FALSE)
##D  
## End(Not run)



cleanEx()
nameEx("elliptical_Tube_Euclideanization")
### * elliptical_Tube_Euclideanization

flush(stderr()); flush(stdout())

### Name: elliptical_Tube_Euclideanization
### Title: Convert an ETRep to a Matrix in the Convex Transformed Space.
### Aliases: elliptical_Tube_Euclideanization

### ** Examples

#Example
# Load tube
data("tube_A")
Euclideanized_Tube<- elliptical_Tube_Euclideanization(tube = tube_A)




cleanEx()
nameEx("intrinsic_Distance_Between2tubes")
### * intrinsic_Distance_Between2tubes

flush(stderr()); flush(stdout())

### Name: intrinsic_Distance_Between2tubes
### Title: Calculating the intrinsic distance between two ETReps
### Aliases: intrinsic_Distance_Between2tubes

### ** Examples

# Load tubes
data("tube_A")
data("tube_B")
intrinsic_Distance_Between2tubes(tube1 = tube_A,tube2 = tube_B)



cleanEx()
nameEx("intrinsic_Transformation_Elliptical_Tubes")
### * intrinsic_Transformation_Elliptical_Tubes

flush(stderr()); flush(stdout())

### Name: intrinsic_Transformation_Elliptical_Tubes
### Title: Intrinsic Transformation Between Two ETReps
### Aliases: intrinsic_Transformation_Elliptical_Tubes

### ** Examples




cleanEx()
nameEx("intrinsic_mean_tube")
### * intrinsic_mean_tube

flush(stderr()); flush(stdout())

### Name: intrinsic_mean_tube
### Title: Calculate Intrinsic Mean of ETReps
### Aliases: intrinsic_mean_tube

### ** Examples

#Example 1
# Load tubes
data("tube_A")
data("tube_B")
intrinsic_mean<-
  intrinsic_mean_tube(tubes = list(tube_A,tube_B),
                      plotting = FALSE)
# Plotting
## Not run: 
##D plot_Elliptical_Tube(tube = intrinsic_mean,
##D                      plot_frames = FALSE,
##D                      plot_skeletal_sheet = FALSE,
##D                      plot_r_project = FALSE,
##D                      plot_r_max = FALSE,
##D                      add = FALSE)
##D  
## End(Not run)

#Example 2
data("simulatedColons")
intrinsic_mean<-
  intrinsic_mean_tube(tubes = simulatedColons,
                      plotting = FALSE)
# Plotting
## Not run: 
##D plot_Elliptical_Tube(tube = intrinsic_mean,
##D                      plot_frames = FALSE,
##D                      plot_skeletal_sheet = FALSE,
##D                      plot_r_project = FALSE,
##D                      plot_r_max = FALSE,
##D                      add = FALSE)
##D  
## End(Not run)



cleanEx()
nameEx("nonIntrinsic_Distance_Between2tubes")
### * nonIntrinsic_Distance_Between2tubes

flush(stderr()); flush(stdout())

### Name: nonIntrinsic_Distance_Between2tubes
### Title: Calculating the non-intrinsic distance between two ETReps
### Aliases: nonIntrinsic_Distance_Between2tubes

### ** Examples

# Load tubes
data("tube_A")
data("tube_B")
intrinsic_Distance_Between2tubes(tube1 = tube_A,tube2 = tube_B)



cleanEx()
nameEx("nonIntrinsic_Transformation_Elliptical_Tubes")
### * nonIntrinsic_Transformation_Elliptical_Tubes

flush(stderr()); flush(stdout())

### Name: nonIntrinsic_Transformation_Elliptical_Tubes
### Title: Non-Intrinsic Transformation Between Two ETReps
### Aliases: nonIntrinsic_Transformation_Elliptical_Tubes

### ** Examples




cleanEx()
nameEx("nonIntrinsic_mean_tube")
### * nonIntrinsic_mean_tube

flush(stderr()); flush(stdout())

### Name: nonIntrinsic_mean_tube
### Title: Compute Non-Intrinsic Mean of ETReps
### Aliases: nonIntrinsic_mean_tube

### ** Examples

#Example 1
# Load tubes
data("tube_A")
data("tube_B")
nonIntrinsic_mean<-
  nonIntrinsic_mean_tube(tubes = list(tube_A,tube_B),
                         plotting = FALSE)
# Plotting
## Not run: 
##D plot_Elliptical_Tube(tube = nonIntrinsic_mean,
##D                      plot_frames = FALSE,
##D                      plot_skeletal_sheet = FALSE,
##D                      plot_r_project = FALSE,
##D                      plot_r_max = FALSE,
##D                      add = FALSE)
##D  
## End(Not run)

#Example 2
data("simulatedColons")
nonIntrinsic_mean<-
  nonIntrinsic_mean_tube(tubes = simulatedColons,
                         plotting = FALSE)
# Plotting
## Not run: 
##D plot_Elliptical_Tube(tube = nonIntrinsic_mean,
##D                      plot_frames = FALSE,
##D                      plot_skeletal_sheet = FALSE,
##D                      plot_r_project = FALSE,
##D                      plot_r_max = FALSE,
##D                      add = FALSE)
##D  
## End(Not run)



cleanEx()
nameEx("plot_Elliptical_Tube")
### * plot_Elliptical_Tube

flush(stderr()); flush(stdout())

### Name: plot_Elliptical_Tube
### Title: Plot an Elliptical Tube (ETRep)
### Aliases: plot_Elliptical_Tube

### ** Examples

# Load tube
data("colon3D")
## Not run: 
##D plot_Elliptical_Tube(tube = colon3D,
##D                      plot_frames = FALSE)
##D  
## End(Not run)



cleanEx()
nameEx("simulate_etube")
### * simulate_etube

flush(stderr()); flush(stdout())

### Name: simulate_etube
### Title: Simulate Random Elliptical Tubes (ETReps)
### Aliases: simulate_etube

### ** Examples




cleanEx()
nameEx("tube_Surface_Mesh")
### * tube_Surface_Mesh

flush(stderr()); flush(stdout())

### Name: tube_Surface_Mesh
### Title: Create surface mesh of a tube
### Aliases: tube_Surface_Mesh

### ** Examples

## Not run: 
##D quad_mesh<-tube_Surface_Mesh(tube = ETRep::tube_B, 
##D                              meshType = "quadrilateral", 
##D                              plotMesh = TRUE, 
##D                              decorate = TRUE, 
##D                              color = "orange")
##D # draw wireframe of the mesh
##D rgl::wire3d(quad_mesh, color = "black", lwd = 1)   # add wireframe
##D # Display in browser
##D ETRep:::.etrep_show3d(width = 800, height = 600)
##D 
##D tri_mesh<-tube_Surface_Mesh(tube = ETRep::tube_B, 
##D                             meshType = "triangular", 
##D                             plotMesh = TRUE, 
##D                             decorate = TRUE, 
##D                             color = "green")
##D # draw wireframe of the mesh
##D rgl::wire3d(tri_mesh, color = "black", lwd = 1)   # add wireframe
##D # Display in browser
##D ETRep:::.etrep_show3d(width = 800, height = 600)
##D  
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
