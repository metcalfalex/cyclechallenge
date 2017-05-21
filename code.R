
# RRzf-4534-BcsF


## Load train data
# input: NA
# output: NA
# e.g. data_train = fn_load_train_data()

fn_load_train_data = function() {
  
  data_train_loc = '~/gitnot/cycle/data_train.csv'
  
  data_train = read.table(
    file = data_train_loc,
    sep = ';',
    header = TRUE
  )
  
  return(data_train)
  
}


## Clean data
# input: data to be cleaned (data frame)
# input: proportion of data to sample (float)
# output: clean data (data frame)
# e.g. data_train_clean = fn_clean_data(data_train, sample = 0.25)
# e.g. data_test_clean = fn_clean_data(data_test, sample = 1)

fn_clean_data = function(df_raw, sample_perc) {
  
  
  ## Remove difficult columns
  
  df_raw$Date = NULL
  df_raw$Time = NULL
  
  ## Sample data (if sample_perc < 1)

  if(sample_perc < 1) {
    set.seed(1)
    samp <- sample(nrow(df_raw), sample_perc * nrow(df_raw))
    df_raw = df_raw[samp, ]
  }

  ## Fill NA with 0
  
  df_raw$Speed[is.na(df_raw$Speed)] = 0
  df_raw$Power[is.na(df_raw$Power)] = 0
  df_raw$Cadence[is.na(df_raw$Cadence)] = 0
  
  ## Return clean data
  
  df_clean = df_raw
  return(df_clean)
  
}


## Build a model
# input: attribute to predict (string)
# input: rider number, 0 for all (int) 
# input: clean data (data frame)
# output: model
# e.g. model_speed = fn_build_model('Speed',0,data_train_clean)

fn_build_model = function(attribute,rider,df_clean) {

  # Load library
  
  if (!require("randomForest")) {
    install.packages("randomForest", dependencies = TRUE)
    library(randomForest)
  }
  
  # Prepare selected attribute for prediction
  
  df_clean$to_predict = df_clean[,match(attribute, names(df_clean))]
  df_clean[,match(attribute, names(df_clean))] = NULL

  # Subset data based on rider
  
  if (rider != 0) {
    
    df_clean = df_clean[df_clean$RiderID == rider,]
    
  }
  
  # Train model
  
  model = randomForest(
    formula = to_predict ~ . , 
    data = df_clean,
    type = 'regression'
  )
  
  # Return model
  return(model)  
  
}



## Load test data
# input: NA
# output: NA
# e.g. data_test = fn_load_test_data()

fn_load_test_data = function() {
  
  data_test_loc = '~/gitnot/cycle/data_test.csv'
  
  data_test = read.table(
    file = data_test_loc,
    sep = ';',
    header = TRUE
  )
  
}



## Make predictions on test data
# input: clean data (data frame)
# (output: predictions csv file)
# output: predictions (data frame)
# e.g. predictions = fn_predict_data(data_test,data_test_clean)

fn_predict_data = function(df_raw,df_clean) {

  # Prepare predictions data frame
  
  df_pred = as.data.frame(cbind(df_clean$Id,rep(NA,nrow(df_clean))))
  colnames(df_pred) = c('Id','Prediction')
  
  # For each rider, predict three attributes and merge into predictions data frame
  
  for (rider in 1:15) {
  
    for (attr_num in 1:3) {
      
      if (attr_num == 1) {attr = 'Speed'}
      if (attr_num == 2) {attr = 'Power'}
      if (attr_num == 3) {attr = 'Cadence'}
      
      # Predict

      eval(parse(text=paste(
        'pred = predict(model_',attr,'_',rider,', newdata = df_clean)',
        sep='')))
      
      # Merge relevant predictions

      eval(parse(text=paste(
        'df_pred$Prediction[is.na(df_raw$',attr,') & df_raw$RiderID == ',rider,'] = pred[is.na(df_raw$',attr,') & df_raw$RiderID == ',rider,']',
        sep='')))

    }  
    
    print(paste('Rider: ',rider,' done.',sep=''))
    
  }
  
  # Output prediction
  
  prediction_loc = paste('~/gitnot/cycle/prediction_',format(Sys.time(), '%Y%m%d_%H%M%S'),'.csv',sep='')
  
  write.table(
    df_pred,
    file = prediction_loc,
    sep = ';',
    row.names = FALSE
    )
  
  return(df_pred)
  
}




## Sense check predictions
# input: predictions (data frame)
# (output: text)
# e.g. fn_qc_predictions(predictions)

fn_qc_predictions = function(predictions_df) {
  
  pred_speed_mean = mean(predictions$Prediction[is.na(data_test$Speed)])
  pred_power_mean = mean(predictions$Prediction[is.na(data_test$Power)])
  pred_cadence_mean = mean(predictions$Prediction[is.na(data_test$Cadence)])
  
  pred_speed_pass = if(pred_speed_mean > 5 & pred_speed_mean < 15) {TRUE} else {FALSE}
  pred_power_pass = if(pred_power_mean > 150 & pred_power_mean < 250) {TRUE} else {FALSE}
  pred_cadence_pass = if(pred_cadence_mean > 60 & pred_cadence_mean < 100) {TRUE} else {FALSE}

  print(paste('Speed passes QC: ',pred_speed_pass,'; mean: ',pred_speed_mean,sep = ''))
  print(paste('Speed passes QC: ',pred_power_pass,'; mean: ',pred_power_mean,sep = ''))
  print(paste('Speed passes QC: ',pred_cadence_pass,'; mean: ',pred_cadence_mean,sep = ''))

}




data_train = fn_load_train_data()
data_train_clean = fn_clean_data(data_train,sample = 1)

for(rider in 1:15) {
  
  for (attr_num in 1:3) {
    
    if (attr_num == 1) {attr = 'Speed'}
    if (attr_num == 2) {attr = 'Power'}
    if (attr_num == 3) {attr = 'Cadence'}

    eval(parse(text=paste('model_',attr,'_',rider,' = fn_build_model("',attr,'",',rider,',data_train_clean)',sep='')))

  }
  
  print(paste('Rider: ',rider,' done.',sep=''))

}

data_test = fn_load_test_data()
data_test_clean = fn_clean_data(data_test,sample = 1)

df_clean = data_test_clean
predictions = fn_predict_data(data_test,data_test_clean)

fn_qc_predictions(predictions)

