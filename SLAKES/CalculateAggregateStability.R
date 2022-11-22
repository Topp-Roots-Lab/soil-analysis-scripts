# This script was built off the sample code in Appendix A for the Soil Health Institute's Standard Operating Procedure for Wet Aggregate Stability by Image Quantification. 
# Email koestreich@danforthcenter.org with questions. 

# The code uses two images of a petri dish containing three soil aggregates. One image is taken at the start and one is taken at the end of a 10 minute 
# submersion in water (images taken prior to running this script). The code calculates the mean aggregate stability for the three aggregates in the petri dish by 
# 1) cropping the images, 2) converting them to grayscale, 3) converting to binary by using a threshold 
# obtained using Otsu's method (this value represents the cutoff value between water and aggregate pixels) and then 4) dividing the total initial area of 
# aggregates (not individual aggregate areas) by the total final area of aggregates. 

# R requires functions to be defined before they are called, so here is a function we will use later. 
getCircularImg <- function(sideLength, img) {
  # This functions takes in a square image and outputs an image
  # containing the circular center, with everything outside the circle zeroed out. 
  
  mask <- EBImage::Image(matrix(0, dim(img)[1], dim(img)[2]))
  maskedImg <- drawCircle(mask, floor(dim(img)[1]/2), floor(dim(img)[2]/2), floor(sideLength/2)-1, col = 1, fill = TRUE)
  newImgMatrix <- imageData(img)
  maskMatrix <- imageData(maskedImg)
  
  # Only return the image contents within the circular mask 
  for (x in 1:(dim(img)[1]-1)) {
    for (y in 1:(dim(img)[2]-1)) {
      if (maskMatrix[x,y] == 0) {
        newImgMatrix[x,y] <- 1
      }
    }
  }
  
  return(as.Image(newImgMatrix))
  
}


# If user doesn't have BiocManager installed, install it
if (!require("BiocManager", quietly = TRUE)) { 
  install.packages("BiocManager")
}

# Do the same for all other necessary packages
if (!require("dplyr", quietly = TRUE)) { 
  install.packages("dplyr")
}

if (!require("EBImage", quietly = TRUE)) { 
  BiocManager::install("EBImage")
}

if (!require("stringr", quietly = TRUE)) { 
  install.packages("stringr")
}

library(stringr)
library(EBImage)
library(dplyr)

# Path to folder with a subfolder for each pair of images 
# POSSIBLE SOURCE OF ERROR: This script assumes a very specific folder structure
# Namely, one parent folder containing 1 folder FOR EACH pair of images (initial and final)
# In this step, you must select the PARENT FOLDER
images_path <- choose.dir()

# Prepare .csv file to store results  
# This will name results with the year, month, day, hour, and minute
processing_datetime <- format(Sys.time(), "%Y-%m-%d-%H%M")
results_filename <- paste0(images_path, "/results_", processing_datetime, ".csv")

# Create .csv file if it does not exist or abort if it does exist to avoid overwriting
if (!file.exists(results_filename)){
  file.create(results_filename)
} else {
  cont <- ''
  while (!(str_trim(cont) %in% c('yes', 'no'))){
    cont <- readline(prompt = "Results .csv already exists. Continuing will overwrite existing file. Do you still want to continue? (type 'yes' or 'no') ")
  }
  if (cont == 'no'){
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    print('Aborting process')
    stop()
  } else if (cont == 'yes'){
    file.remove(results_filename)
    file.create(results_filename)
  }
}

# Initialize dataframe that will eventually comprise the output .csv
df <- data.frame(matrix(ncol = 2, nrow = 0))
x <- c("Stability Index", "Initial Image Filename")
colnames(df) <- x

# Suppress warnings, because it doesn't like that we're "overwriting" column names
suppressWarnings(write.table(df, file=results_filename, append = TRUE, row.names = FALSE))

# Get list of folders in image folder to iterate through
folder_list <- list.dirs(path = images_path, recursive = FALSE)

for (folder in folder_list) {
  
  # Get file paths of images
  image_list <- list.files(path = folder, full.names = TRUE, recursive = FALSE)
  img_FinalPath <- NA
  img_InitialPath <- NA
  
  if (length(image_list)!=2){
    print(paste0("Warning: More than two files in subfolder ",folder,'. Continuing...'))
  }
  
  # Identify both images in directory
  for (image in image_list) {
    if (str_detect(image,"0(?=_c\\d+_p\\d+)")){
      img_InitialPath <- image
    }
    if (str_detect(image,"601(?=_c\\d+_p\\d+)")){
      img_FinalPath <- image
    }
  }
  
  if (is.na(img_FinalPath) | is.na(img_InitialPath)){
    print(c("ERROR: Found fewer than two images (filename should be '{anythinghere}_{0 or 601}_c{somenumber}_p{somenumber}.{filextension}'). Skipping folder: ",folder))
    next
  }
  
  # Read in the initial and final images and plot to visualize
  # You don't have to plot, but it can be helpful to confirm you have the correct image
  img_Initial <- readImage(img_InitialPath)
  
  # IMPORTANT!: If you need to check the uncropped image, uncomment the following line
  # plot(img_Initial); title(main = img_InitialPath)
  
  img_Final <-  readImage(img_FinalPath)
  
  # IMPORTANT!: If you need to check the uncropped image, uncomment the following line
  # plot(img_Final); title(main = img_FinalPath)
  
  # 1) Crop images so they only contain the three aggregates you intend to analyze.
  # It is good practice to leave the sample ID and replicate number in the image
  
  # Plot to see if you got a good trim, if not, change numbers and try again. Note that images have three bands and the first is plotted by default. We will aveage over bands at the end.

  
  # IMPORTANT!: These boundaries are specific to our lab's setup of slakes. Currently, this script cannot detect the petri dish and center in on it before using cropcircles, which
  # expects the circle to be cropped to be centralized in the image. Experiment with these numbers for your own setup.
  
  sideLength <- 2400
  xmin <- 800
  ymin <- 600
  
  imgCropError <- tryCatch(
    expr = {
       
      print(dim(img_Initial))
      img_InitialCrop <- img_Initial[xmin:(xmin+sideLength),ymin:(ymin+sideLength)]
      
      # IMPORTANT!: If you need to check the cropped image, uncomment the following line
      # plot(img_InitialCrop); title(main = img_InitialPath)
      
      print(dim(img_Final))
      img_FinalCrop <- img_Final[xmin:(xmin+sideLength),ymin:(ymin+sideLength)] 
      
      # IMPORTANT!: If you need to check the cropped image, uncomment the following line
      # plot(img_FinalCrop); title(main = img_FinalPath)
    },
    error = function(e) e
  )
  
  # If the previous section of code (a.k.a. tryCatch block) resulted in an error, print it out and skip this set of images
  if(inherits(imgCropError, "error")){
    print(imgCropError)
    next
  }

  # make the circle smaller than the petri dish
  circle_diameter <- sideLength - 150
  
  # Finally, use magick and plotrix packages to get just the circle in the center of the image. 
  img_InitialCircle <- getCircularImg(sideLength = circle_diameter, img_InitialCrop)
  
  # IMPORTANT!: If you need to check the circular-cropped image, uncomment the following line
  # plot(img_InitialCircle); title(main = img_InitialPath)
  
  img_FinalCircle <- getCircularImg(sideLength = circle_diameter, img_FinalCrop)
  
  # IMPORTANT!: If you need to check the circular-cropped image, uncomment the following line
  # plot(img_FinalCircle); title(main = img_FinalPath)
  
  # 2) Convert images to grayscale
  colorMode(img_InitialCircle) <- Grayscale
  colorMode(img_FinalCircle) <- Grayscale
  
  # 3) Get the threshold values for the images so we can make them binary, these are essentially the cutoffs for what values will be classified as soil    
  threshold_Initial <- otsu(img_InitialCircle)
  threshold_Final <- otsu(img_FinalCircle)
  
  # Make binary versions of the images by assigning 0 to values less than or equal to the threshold (found in lines 48 and 49) 
  # and assigning 1 to values greater than the threshold
  img_binary_Initial <- EBImage::combine( mapply(function(frame, th) frame <= th, getFrames(img_InitialCircle), threshold_Initial, SIMPLIFY=FALSE) )
  
  # IMPORTANT!: If you need to check the thresholded image, uncomment the following line
  # plot(img_binary_Initial); title(main=img_InitialPath)
  
  img_binary_Final <- EBImage::combine( mapply(function(frame, th) frame <= th, getFrames(img_FinalCircle), threshold_Final, SIMPLIFY=FALSE) )
  
  # IMPORTANT!: If you need to check the thresholded image, uncomment the following line
  # plot(img_binary_Final); title(main=img_FinalPath)
  
  # Sum all the pixels with finite values to get total area of aggregate for both images
  area_Initial <- sum(img_binary_Initial[is.finite(img_binary_Initial)]>threshold_Initial)
  area_Final <- sum(img_binary_Final[is.finite(img_binary_Final)]>threshold_Final)
  
  # 4) The stability index for this petri dish is initial area divided by the final area.      
  StabilityIndex<-mean(area_Initial/area_Final)
  
  df <- data.frame("Stability Index"=StabilityIndex, InitialImageFileName=img_InitialPath)
  write.table(df, file=results_filename, append=TRUE, col.names = FALSE, row.names = FALSE)
}

print("Done Processing. See results in the generated csv file in the parent folder")


