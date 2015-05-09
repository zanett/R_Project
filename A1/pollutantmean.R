####################################################
# pollutantmean
# 14.02.2015
####################################################

# calculate mean value for selected pollutant ids

pollutantmean<- function(directory,pollutant,id=1:332) 
  {
  files<- list.files(directory)
  df_filedata<- data.frame(Date=as.Date(character()),
                         sulfate=numeric(),
                         nitrate=numeric(),
                         ID=integer())
  for (i in id) 
    {
    df_filedata_temp<- read.csv(paste(directory,files[i],sep="/"))
    df_filedata=rbind(df_filedata,df_filedata_temp)
    }

  y<- df_filedata[[pollutant]]
  mean(y,na.rm=TRUE)
  }