#Function that allows to download the data directly from Kaggle leaving no file in your computer
data_downloader = function(){
  
  library(kaggler)
  
  temp = tempfile()
  downLink = kgl_datasets_download('goldenoakresearch/us-household-income-stats-geo-locations', fileName = '')
  download.file(downLink$url,temp)
  data = read.csv(unz(temp, 'kaggle_income.csv'))
  unlink(temp)
  
  
  return(data)
}

