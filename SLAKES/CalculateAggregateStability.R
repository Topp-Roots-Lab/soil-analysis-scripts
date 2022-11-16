# This script was built off the sample code in Appendix A for the Soil Health Institute's Standard Operating Procedure for Wet Aggregate Stability by Image Quantification. 
# Email koestreich@danforthcenter.org with questions. 

# The code uses two images of a petri dish containing three soil aggregates. One image is taken at the start and one is taken at the end of a 10 minute 
# submersion in water (prior to this script). The code calculates the mean aggregate stability for the three aggregates in the petri dish by 1)cropping the images 
# (using hard-coded boundaries - POSSIBLE SOURCE OF ERROR), 2)converting them to grayscale, 3) converting to binary by using a threshold obtained using Otsu's method
# (this value represents the cutoff value between water and aggregate pixels) and then 4) dividing the total initial area of aggregates (not individual aggregate 
# areas) by the total final area of aggregates. 



# If user doesn't have BiocManager installed, install it
if (!require("BiocManager", quietly = TRUE)) { 
  install.packages("BiocManager")
}

# Do the same for EBImage, dplyr, and stringr
if (!require("dplyr", quietly = TRUE)) { 
  install.packages("dplyr")
}

if (!require("EBImage", quietly = TRUE)) { 
  BiocManager::install("EBImage")
}

if (!require("stringr", quietly = TRUE)) { 
  install.packages("stringr")
}

# Load in required packages
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

#create .csv file if it does not exist or abort if it does exist to avoid overwriting
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

#get list of folders in image folder to iterate through
folder_list <- list.dirs(path = images_path, recursive = FALSE)

for (folder in folder_list) {
  
  # Get file paths of images
  image_list <- list.files(path = folder, full.names = TRUE, recursive = FALSE)
  img_FinalPath <- NA
  img_InitialPath <- NA
  
  if (length(image_list)!=2){
    print(paste0("Warning: More than two files in subfolder ",folder,'. Continuing...'))
  }
  
  #identify both images
  for (image in image_list) {
    if (str_detect(image,"0(?=_c\\d+_p\\d+)")){
      img_InitialPath <- image
    }
    if (str_detect(image,"601(?=_c\\d+_p\\d+)")){
      img_FinalPath <- image
    }
  }
  
  if (is.na(img_FinalPath) | is.na(img_InitialPath)){
    print(c("ERROR: Found fewer than two images (filename should be {anythinghere}_{0 or 601}_c{somenumber}_p{somenumber}.{filextension}). Skipping folder: ",folder))
    next
  }
  
  #read in the initial and final images and plot to visualize
  #you don't have to plot, but it can be helpful to confirm you have the correct image
  img_Initial <- readImage(img_InitialPath)
  # plot(img_Initial)
  img_Final <-  readImage(img_FinalPath)
  # plot(img_Final)
  
  
  #get the image dimensions to inform where to crop
  dim(img_Initial)
  dim(img_Final)
  
  #1 crop images so they only contain the three aggregates you intend to analyze.
  #It is good practice to leave the sample ID and replicate number in the image
  #If you already cropped your images before reading in, you need to rename them. 

  #No other aggregates should be in the image and no petri dish edges should be present.
  
  #you may have to guess and check to get the right crop. 
  #plot to see if you got a good trim, if not, change numbers and try again. Note that images have three bands and the first is plotted by default. We will aveage over bands at the end.

  imgCropError <- tryCatch(
    expr = {
      img_Initial_crop <- img_Initial[1336:2736, 1124:2524]
      plot(img_Initial_crop)
      title(main = img_InitialPath)
      img_Final_crop <- img_Final[1336:2736, 1124:2524] 
      plot(img_Final_crop)
      title(main = img_FinalPath)
    },
    error = function(e) e
  )
  
  if(inherits(imgCropError, "error")){
    next
  }
  
  #2 make images gray scale
  colorMode(img_Initial_crop) <- Grayscale
  colorMode(img_Final_crop) <- Grayscale
  
  #3 get the threshold values for the images so we can make them binary, these are essentially the cutoffs for what values will be classified as soil    
  threshold_Initial <- otsu(img_Initial_crop)
  threshold_Final <- otsu(img_Final_crop)
  #make binary versions of the images by selecting values less than the threshold (found in lines 48 and 49) and then summing the pixels that are 
  #greater than the threshold (lines 48 and 49). These are done for of the three each image bands. 
  img_binary_Initial <- EBImage::combine( mapply(function(frame, th) frame <= th, getFrames(img_Initial_crop), threshold_Initial, SIMPLIFY=FALSE) )
  plot(img_binary_Initial)
  img_binary_Final <- EBImage::combine( mapply(function(frame, th) frame <= th, getFrames(img_Final_crop), threshold_Final, SIMPLIFY=FALSE) )
  plot(img_binary_Final)
  
  #area_Initial <- apply(img_binary_Initial, MARGIN = 3, sum, na.rm = T)
  
  #area_Final <- apply(img_binary_Final, MARGIN = 3, sum, na.rm = T)
  
  area_Initial <- sum(img_binary_Initial[is.finite(img_binary_Initial)]>threshold_Initial)
  area_Final <- sum(img_binary_Final[is.finite(img_binary_Final)]>threshold_Final)
  
  
  #4 The stability index for this petri dish is initial area dived by the final area. The mean is taken across the three image bands.     
  StabilityIndex<-mean(area_Initial/area_Final)
  
  df <- data.frame("Stability Index"=StabilityIndex, InitialImageFileName=img_InitialPath)
  write.table(df, file=results_filename, append=TRUE, col.names = FALSE, row.names = FALSE)
}

print("Done Processing. See results in the generated csv file in the parent folder")
