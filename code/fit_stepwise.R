fit_stepwise <- function(feat_single){
  
  train_list <- list()
  
  for(i in 1:length(train_feat)){
    
    train_list[[i]] <- list(train_feat[[i]], train_label[[i]], feat_single, validate_data[[i]], folds_index[[i]])
    names(train_list[[i]]) <- c("train_feat", "train_label", "feat_single", "validate_data", "folds_index")
  }
  
  
  fit_feat <- function(train_list){
    
    # initialize features
    num.trees.rf <- ifelse(feat_initial < 5, 50, 100)
    
    ## Create tune-grid
    tgrid <- expand.grid(
      mtry = c(2),
      splitrule = "variance", 
      min.node.size = c(50)
    )
    
    ## Create trainControl object
    myControl <- trainControl(
      method = "none",
      classProbs = FALSE,
      allowParallel = TRUE
    )
    
    ## train rf on folds 
    rf_model <- train(
      x = train_list$train_feat[,c(feat_single, feat_best)],
      y = train_list$train_label,
      num.trees = num.trees.rf,
      method = 'ranger',
      trControl = myControl,
      tuneGrid = tgrid,
      metric = "MAE"
    )
    
    
    # get all hold out predictions
    rf.pred <- train_list$validate_data %>%
      mutate(FCH4P = predict(rf_model, .),
             index = 1:n())
    
  }
  
  rf.pred.inner <- lapply(train_list, fit_feat)
  rf.pred.all <<- bind_rows(rf.pred.inner) %>% dplyr::select(Cluster, feat_single, FCH4, FCH4P)
  
}