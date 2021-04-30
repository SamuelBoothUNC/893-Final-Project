library(tiff)



segment = function(img, g)
{
  #Inputs: NxMx3 array, threshold value in (0, 1)
  #Value: matrix with all (green) pixels above threshold turned to 1, 0 otherwise
  test = img
  for(i in 1:dim(test)[1])
    {
    for(j in 1:dim(test)[2])
    {
      if(test[i, j, 2] > g) #if green value is above threshold, set all colors values to 1
      {
        test[i, j, 1] = 1
        test[i, j, 2] = 1
        test[i, j, 3] = 1
      } else {
        test[i, j, 1] = 0
        test[i, j, 2] = 0
        test[i, j, 3] = 0
      }
    }
  }
  test = test[,,2] #collapse down to one dimension (black and white format)
  return(test)
}



clean_dat = function(img)
{
  #Input: binary matrix
  #Value: matrix with small "islands" removed
  for(i in 1:dim(img)[1]) #iterate over rows
  {
    print(i)
    for(j in 1:dim(img)[2]) #iterate over columns
    {
      if(test[i, j] == 1)
      {
        for(k in 1:30) #iterate over possible radii of squares
        {
          test_vec = vector(mode = 'logical', length = 8*k)
          for(l in 0:k)
          {
            test_vec[l+1] = ifelse(img[(i+k)%%dim(img)[1] + 1, (j-l)%%dim(img)[2] + 1] == 0,FALSE,TRUE)
            test_vec[l+1+k] = ifelse(img[(i+k)%%dim(img)[1] + 1, (j+l)%%dim(img)[2] + 1] == 0,FALSE,TRUE)
            test_vec[l+1+2*k] = ifelse(img[(i-k)%%dim(img)[1] + 1, (j-l)%%dim(img)[2] + 1] == 0,FALSE,TRUE)
            test_vec[l+1+3*k] = ifelse(img[(i-k)%%dim(img)[1] + 1, (j+l)%%dim(img)[2] + 1] == 0,FALSE,TRUE)
            test_vec[l+1+4*k] = ifelse(img[(i+l)%%dim(img)[1] + 1, (j+k)%%dim(img)[2] + 1] == 0,FALSE,TRUE)
            test_vec[l+1+5*k] = ifelse(img[(i-l)%%dim(img)[1] + 1, (j+k)%%dim(img)[2] + 1] == 0,FALSE,TRUE)
            test_vec[l+1+6*k] = ifelse(img[(i+l)%%dim(img)[1] + 1, (j-k)%%dim(img)[2] + 1] == 0,FALSE,TRUE)
            test_vec[l+1+7*k] = ifelse(img[(i-l)%%dim(img)[1] + 1, (j-k)%%dim(img)[2] + 1] == 0,FALSE,TRUE)
          }
          if(sum(test_vec) == 0) #if one of the square perimeters has no white pixels, turn whole square black
          {
            v1 = (i-k):(i+k)
            v2 = (j-k):(j+k)
            v1 = v1[v1 > 0]
            v1 = v1[v1 <= dim(img)[1]]
            v2 = v2[v2 > 0]
            v2 = v2[v2 <= dim(img)[2]]
            img[v1, v2] = 0
            break
          }
        }
      }
    }
  }
  return(img)
}




getDist = function(fittedImg, trueImg)
{
  #Inputs: two binary matrices of equal size
  #Value: frequency of differing pixels
  return(mean(abs(fittedImg - trueImg))) 
}




#==================================================================================================================
#Section 1: optimizing the threshold value

# rawImage = readTIFF("N 134 copy.tif")
# truth = readTIFF("N 134 groundtruth.tif", convert = T)
# 
# 
# 
# 
# 
# dmin = Inf
# for(gpct in seq(.35, .5, .01))
# {
#   test = segment(rawImage, gpct)
#   d = getDist(test, truth)
#   if(d < dmin)
#   {
#     best = gpct
#     dmin = d
#   }
#   message(d)
#   message(gpct)
# }


#Optimal threshold value is found to be .43



#==================================================================================================================
#Section 2: measuring performance on the validation data



# rawImage = readTIFF("N129 crop.tif")
# truth = readTIFF("N 129 groundtruth.tif", convert = T)
# 
# test = segment(rawImage, .43)
# 
# test = clean_dat(test)
# 
# accuracy = 1 - getDist(test, truth)
# accuracy

#On test data, 93% of pixels were reproduced accurately


#==================================================================================================================
#Section 3: 



rawImage = readTIFF(file.choose())

#segment
test = segment(rawImage, .43)

#remove islands, time how long it took
ptm = proc.time()
test = clean_dat(test)
proc.time() - ptm

#output file
writeTIFF(test, where = "output.tif")







