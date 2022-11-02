#This sample code is Appendix A for the Soil Health Institute's Standard Operating Procedure for Wet Aggregate Stability by Image Quantification. 
#The code was last updated on May 16th, 2022. Email eriekie@soilhealthinstitue.org with questions. 

#The code uses two images of a petri dish containing three soil aggregates. One image is taken at the start and one is taken at the end of a 10 minute 
#submersion in water.The code calculates the mean aggregate stability for the three aggregates in the petri dish by 1)cropping the images, 2)converting them to 
#grayscale, 3) converting to binary by using a threshold obtained using Otsu's method (this value represents the cutoff value between water and aggregate pixels) 
#and then 4) dividing the total initial area of aggregates (not individual aggregate areas) by the total final are of aggregates. In step 5), an example of how 
#to save data are is given. 

#You will need to update file paths to locate the images on your computer lines and to save output.

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

library(stringr)
library(EBImage)
library(dplyr)

#path to folder with a subfolder for each pair of images 
images_path <- readline(prompt = "Enter Absolute File Path for Folder Containing Image-Pair Subfolders: (Example: C:/Users/exampleUser/Data/AgStabData): ")

#get list of folders in image folder to iterate through
folder_list <- list.dirs(path = images_path, recursive = FALSE)

for (folder in folder_list) {
  
  results_path <- folder
  
  image_list <- list.files(path = folder, full.names = TRUE, recursive = FALSE)
  img_FinalPath <- NA
  img_InitialPath <- NA
  
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
  #To rename your image, you need to comment out lines 39 and 41 and uncomment lines 34 and 35 and run them:
  #img_Initial_crop <- img_Initial
  #img_Final_crop <- img_Final 
  #No other aggregates should be in the image and no petri dish edges should be present.
  
  #you may have to guess and check to get the right crop. 
  #img_Initial_crop <- img_Initial[1336:2736, 1124:2524]
  #plot(img_Initial_crop)
  #plot to see if you got a good trim, if not, change numbers and try again. Note that images have three bands and the first is plotted by default. We will aveage over bands at the end.
  #img_Final_crop <- img_Final[1336:2736, 1124:2524] 
  #plot(img_Final_crop)
  
  incorrectDims <- tryCatch(
    expr = {
      img_Initial_crop <- img_Initial[1336:2736, 1124:2524]
      # plot(img_Initial_crop)
      img_Final_crop <- img_Final[1336:2736, 1124:2524] 
      # plot(img_Final_crop)
    },
    error = function(e){
      message(paste0('Error; Skipping this folder; Ensure Images are 8-bit JPG and Check Cropping Boundaries for Folder: ', folder))
      print(e)
    }
  )
  
  if(inherits(incorrectDims, "error")){
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
  # plot(img_binary_Initial)
  img_binary_Final <- EBImage::combine( mapply(function(frame, th) frame <= th, getFrames(img_Final_crop), threshold_Final, SIMPLIFY=FALSE) )
  # plot(img_binary_Final)
  
  #area_Initial <- apply(img_binary_Initial, MARGIN = 3, sum, na.rm = T)
  
  #area_Final <- apply(img_binary_Final, MARGIN = 3, sum, na.rm = T)
  
  area_Initial <- sum(img_binary_Initial[is.finite(img_binary_Initial)]>threshold_Initial)
  area_Final <- sum(img_binary_Final[is.finite(img_binary_Final)]>threshold_Final)
  
  
  #4 The stability index for this petri dish is initial area dived by the final area. The mean is taken across the three image bands.     
  StabilityIndex<-mean(area_Initial/area_Final)
  
  #5 Example of how to save results    
  #This will name results with the year, month, day, hour, and minute
  processing_datetime <- format(Sys.time(), "%Y-%m-%d-%H%M")
  #this will save the value to a CSV
  write.csv(StabilityIndex, paste0(results_path, '/slaking_image_results_', processing_datetime, '.csv'), row.names = F)
}
