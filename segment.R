library(tiff)
setwd("C:/Users/samue/Downloads")

rawImage = readTIFF("N 134 copy.tif")
truth = readTIFF("N 134 groundtruth.tif", convert = T)

segment = function(img, gpct)
{
  test = img
  g = quantile(test[,,2], gpct)
  for(i in 1:dim(test)[1])
    {
    for(j in 1:dim(test)[2])
    {
      if(test[i, j, 2] > g)
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
  return(test)
}



getDist = function(fittedImg, trueImg)
{
  return(mean(abs(fittedImg[,,2] - trueImg))) 
}



dmin = Inf
for(gpct in seq(.9, .95, .005))
{
  test = segment(rawImage, gpct)
  d = getDist(test, truth)
  if(d < dmin)
  {
    best = gpct
    dmin = d
  }
  message(d)
  message(gpct)
}


test = segment(rawImage, best)[,,2]

writeTIFF(test, where = "test.tif")


clean_dat = function(img)
{
  for(i in 1:dim(img)[1])
  {
    print(i)
    for(j in 1:dim(img)[2])
    {
      if(test[i, j] == 1)
      {
      for(k in 1:25){
        test_vec <- vector(mode = 'logical', length = 8*k)
        for(l in 0:k){
          test_vec[l+1] = ifelse(img[i+k,j-l] == 0,FALSE,TRUE)
          test_vec[l+1+k] = ifelse(img[i+k,j+l] == 0,FALSE,TRUE)
          test_vec[l+1+2*k] = ifelse(img[i-k,j-l] == 0,FALSE,TRUE)
          test_vec[l+1+3*k] = ifelse(img[i-k,j+l] == 0,FALSE,TRUE)
          test_vec[l+1+4*k] = ifelse(img[i+l,j+k] == 0,FALSE,TRUE)
          test_vec[l+1+5*k] = ifelse(img[i-l,j+k] == 0,FALSE,TRUE)
          test_vec[l+1+6*k] = ifelse(img[i+l,j-k] == 0,FALSE,TRUE)
          test_vec[l+1+7*k] = ifelse(img[i-l,j-k] == 0,FALSE,TRUE)
        }
        if(sum(test_vec) == 0){
          img[(i-k):(i+k),(j-k):(j+k)] = 0
        }
        
      }
      }
    }
  }
  return(img)
}


test1 <- clean_dat(test)

writeTIFF(test1, where = "test1.tif")





