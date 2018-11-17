library(rgdal)
library(raster)
library(h5)
library(reshape2)

# Create save directory
ifelse(!dir.exists("tiff"), dir.create("tiff"), FALSE)

# List all HDF5 files available in Folder HDF5
dataFiles <- dir(path="HDF5", pattern=".h5$", full.names=T)

# Function to convert HDF5 to TIFF
HDF5_to_TIFF <- function(input){
  
  require(rgdal)
  require(raster)
  require(h5)
  require(reshape2)

  test <- h5file(input, mode="r")
  soilMoisture <- test["/Soil_Moisture_Retrieval_Data_1km/soil_moisture_1km"]  #retrieve Soil Moisture 1 km

  dfm <- as.data.frame.matrix(soilMoisture)
  
  # Get Attributes
  units = h5attr(soilMoisture, 'units')
  longname = h5attr(soilMoisture, 'long_name')
  FillValue = h5attr(soilMoisture, '_FillValue')
  valid_max = h5attr(soilMoisture, 'valid_max')
  valid_min = h5attr(soilMoisture, 'valid_min')
  invalid = xor(dfm > valid_max, dfm < valid_min)
  dfm[invalid] <- -9999
  
  # Get coordinates
  longitude <- test["/Soil_Moisture_Retrieval_Data_1km/longitude_1km"]
  dfmLon <- as.data.frame.matrix(longitude)
  
  latitude <- test["/Soil_Moisture_Retrieval_Data_1km/latitude_1km"]
  dfmLat <- as.data.frame.matrix(latitude)
  
  # Get number of rows and columns for raster creation
  rows <- NROW(dfm)
  cols <- NCOL(dfm)
  
  coordsDF = data.frame("X" = double(), "Y"=double(), "Value" = double())
  # Combine the data as one dataframe
  for (i in 1:cols){
    coordsDF2 <- data.frame("X" = dfmLon[,i], "Y"=dfmLat[,i], "Value" = dfm[,i])
    coordsDF <- merge(coordsDF2, coordsDF, all = T)
  }
  
  MinX <- min(coordsDF["X"])
  MaxX <- max(coordsDF["X"])
  MinY <- min(coordsDF["Y"])
  MaxY <- max(coordsDF["Y"])
  
  # Create the raster
  testRaster <- raster(nrows=rows, ncols=cols, xmn = MinX , xmx = MaxX, ymn = MinY, ymx = MaxY,
                       crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  
  # Set coordinates
  coordinates(coordsDF) <- ~X+Y
  testRaster <- rasterize(coordsDF, testRaster, "Value", fun='first')
  testRaster[testRaster<0] <- NA
  
  # Save as GeoTiff at EPSG:4326
  FileName <- as.character(basename(input))
  DirName <- "tiff/"
  writeRaster(testRaster, filename=paste(DirName, FileName, sep=""), format='GTiff', overwrite=T)
  
}

lapply(dataFiles, HDF5_to_TIFF)
