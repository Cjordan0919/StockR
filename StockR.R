#' @title Predicts daily Stock Market Fluctuations for given stock symbols
#'
#' @description This package helps predict whether a certain stock will increase or decrease tomorrow based off today's market value
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples  stock_predict('NKE')
#'
#' @export stock_predict
stock_predict<-function(symbol)
{

  #To ignore warnings
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)
  #Importing price data for stock symbols
  data<-data.frame(xts::as.xts(get(quantmod::getSymbols(symbol))))

  #Assigning column names
  colnames(data) <- c("data.Open","data.High","data.Low","data.Close","data.Volume","data.Adjusted")

  #Creating lag and lead features for stock price
  data <- xts::xts(data,order.by=as.Date(rownames(data)))
  data <- as.data.frame(merge(data, lm1=stats::lag(data[,'data.Adjusted'],c(-1,1,3,5,10))))

  #Extracting Date
  data$Date<-as.Date(rownames(data))
  data$Day_of_month<-as.integer(format(as.Date(data$Date),"%d"))
  data$Month_of_year<-as.integer(format(as.Date(data$Date),"%m"))
  data$Year<-as.integer(format(as.Date(data$Date),"%y"))
  data$Day_of_week<-as.factor(weekdays(data$Date))

  #Declaring variables
  today <- 'data.Adjusted'
  tommorow <- 'data.Adjusted.5'

  #Creating data
  data$up_down <- as.factor(ifelse(data[,tommorow] > data[,today], 1, 0))

  #Creating tests
  train<-data[stats::complete.cases(data),]
  test<-data[nrow(data),]

  #Training model
  model<-stats::glm(up_down~data.Open+data.High+data.Low+data.Close+
                      data.Volume+data.Adjusted+data.Adjusted.1+
                      data.Adjusted.2+data.Adjusted.3+data.Adjusted.4+
                      Day_of_month+Month_of_year+Year+Day_of_week,
                    family=binomial(link='logit'),data=train)

  #Predictions based off data
  pred<-as.numeric(stats::predict(model,test[,c('data.Open','data.High','data.Low','data.Close','data.Volume','data.Adjusted','data.Adjusted.1','data.Adjusted.2','data.Adjusted.3','data.Adjusted.4','Day_of_month','Month_of_year','Year','Day_of_week')],type = 'response'))

  #Display results
  print("Probability of Stock price increasing tommorow based off current value:")
  print(pred)
}
