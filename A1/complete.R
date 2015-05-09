
complete<- function(directory,ID=1:332)
{
  files<- list.files(directory)
  df_nobs<- data.frame(id=integer(),
                         nobs=integer()
                      )
  for (i in ID) 
  {
    df_filedata_temp<- read.csv(paste(directory,files[i],sep="/"))
    nobs_temp<- sum(complete.cases(df_filedata_temp))
    
    df_nobs_temp=data.frame(id=i,nobs=nobs_temp)
    df_nobs=rbind(df_nobs,df_nobs_temp)
  } 
  df_nobs
}
