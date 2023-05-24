fit_all_pairs <- function(feat_pairs){
  
  train_list <- list()
  
  for(i in 1:length(train_feat)){
    
    train_list[[i]] <- list(train_feat[[i]], train_label[[i]], feat_pairs, validate_data[[i]], folds_index[[i]])
    names(train_list[[i]]) <- c("train_feat", "train_label", "feat_pairs", "validate_data", "folds_index")
  }
  
  
  fit_one_pair <- function(train_list){
    
    # initialize feature pairs
    feat_initial <- 2
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
      x = train_list$train_feat[,train_list$feat_pairs],
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
  
  rf.pred.inner <- lapply(train_list, fit_one_pair)
  rf.pred.all <<- bind_rows(rf.pred.inner) %>% dplyr::select(Cluster, feat_pairs[1], feat_pairs[2], FCH4, FCH4P)
  
}