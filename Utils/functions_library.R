#===============================================================================
## Function0: load_packages
load_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!require(pkg, character.only = TRUE)) {
      stop(paste0("Package '", pkg, "' is not installed. Please run renv::restore() first."))
    }
  }
}
#===============================================================================
## Function1: extract_win_or_pay_mean
extract_win_or_pay_mean <- function(df_feature,df_y,feature,win_or_pay,show_pay_trialN){
  df_feature$Agood = rep(NaN, nrow(df_feature))
  df_feature$Abad = rep(NaN, nrow(df_feature))
  df_feature_Trial = df_feature[,grepl('R', colnames(df_feature))]
  df_y_win_Trial = df_y[,grepl('_wl', colnames(df_y))]
  df_y_pay_Trial = df_y[,grepl('_pay', colnames(df_y))]

  for (n in c(1:nrow(df_y))){
    IndexWin = df_y_win_Trial[n,] == 1
    IndexLose = df_y_win_Trial[n,] == 0
    
    if (win_or_pay == "win"){
      IndexGood = IndexWin
      IndexBad = IndexLose
    }else if (win_or_pay == "pay-control"){
      IndexGood_Win = IndexWin & (df_y_pay_Trial[n,] > median(as.numeric(df_y_pay_Trial[n,IndexWin])))
      IndexGood_Lose = IndexLose & (df_y_pay_Trial[n,] > median(as.numeric(df_y_pay_Trial[n,IndexLose])))
      IndexGood = IndexGood_Win | IndexGood_Lose
      IndexBad_Win = IndexWin & (df_y_pay_Trial[n,] < median(as.numeric(df_y_pay_Trial[n,IndexWin])))
      IndexBad_Lose = IndexLose & (df_y_pay_Trial[n,] < median(as.numeric(df_y_pay_Trial[n,IndexLose])))
      IndexBad = IndexBad_Win | IndexBad_Lose
      if (show_pay_trialN == TRUE){
        cat(sprintf('n = %d, High = %d, Low = %d, win = %d.\n', df_y$n[n], sum(IndexGood), sum(IndexBad), sum(IndexWin)))
      }
    }else if (win_or_pay == "pay-notcontrol"){
      IndexGood = df_y_pay_Trial[n,] > median(as.numeric(df_y_pay_Trial[n,]))
      IndexBad = df_y_pay_Trial[n,] < median(as.numeric(df_y_pay_Trial[n,]))
      if (show_pay_trialN == TRUE){
        cat(sprintf('n = %d, High = %d, Low = %d, win = %d.\n', df_y$n[n], sum(IndexGood), sum(IndexBad), sum(IndexWin)))
      }
    }
    
    TrialGood = data.frame()
    TrialBad = data.frame()
    TrialGood = df_feature_Trial[df_y$n[n] == df_feature$n, IndexGood]
    TrialBad = df_feature_Trial[df_y$n[n] == df_feature$n, IndexBad]
    
    if (is.null(ncol(TrialGood)) | is.null(ncol(TrialBad))){
      next
    }else if (ncol(TrialGood)>2 & ncol(TrialBad)>2){
      df_feature$Agood[df_y$n[n] == df_feature$n] = rowMeans(TrialGood)
      df_feature$Abad[df_y$n[n] == df_feature$n] = rowMeans(TrialBad)
    }
  }
  df_need_dcast = reorg_df(df_feature,feature)
  return(df_need_dcast)
}

#===============================================================================
## Function2: svm_cv_accuracy
svm_cv_accuracy <- function(df,nfold,n_iter,auc_or_acc,koi){
  df[,1] = factor(df[,1], levels = c(0, 1))
  df = na.omit(df)
  
  if (auc_or_acc == "acc_single"){
    accuracy_all = data.frame()
    for (i in 1:n_iter){
      folds = svm_createFolds(df,nfold)
      
      cv = lapply(folds, function(x) { 
        training_fold = df[-x, ] 
        test_fold = df[x, ] 
        test_fold[-1] = ind_scale(training_fold[-1],test_fold[-1])
        training_fold[-1] = scale(training_fold[-1])
        classifier = svm(formula = good_1 ~ .,
                         data = training_fold,
                         type = 'C-classification',
                         kernel = koi)
        y_pred = predict(classifier, newdata = test_fold[-1])
        cm = table(test_fold[, 1], y_pred)
        accuracy_single = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
        
        return(accuracy_single)
      })
      accuracy_all = rbind(accuracy_all,mean(as.numeric(cv)))
    }
    realMean = mean(accuracy_all[,1])
    return(realMean)
  }
  else if (auc_or_acc == "acc_force"){
    accuracy_all = data.frame()
    for (i in 1:n_iter){
      folds = svm_createFolds(df,nfold)
      
      cv = lapply(folds, function(x) { 
        training_fold = df[-x, ] 
        test_fold = df[x, ] 
        test_fold[-1] = ind_scale(training_fold[-1],test_fold[-1])
        training_fold[-1] = scale(training_fold[-1])
        classifier = svm(formula = good_1 ~ .,
                         data = training_fold,
                         type = 'C-classification',
                         kernel = koi,
                         probability = TRUE)
        correct = 0
        total_pairs = nrow(test_fold)/2
        for(p in 1:total_pairs){
          prob1 = predict(classifier, newdata = test_fold[p, -1], probability = TRUE)
          prob2 = predict(classifier, newdata = test_fold[p+total_pairs, -1], probability = TRUE)
          prob1_matrix = attr(prob1,"probabilities")
          prob2_matrix = attr(prob2,"probabilities")
          
          predicted_pair = ifelse(prob1_matrix[1] > prob2_matrix[1],1,0)
          true_label_pair = test_fold[p,1]
          if(predicted_pair == true_label_pair){
            correct = correct + 1
          }
        }
        
        accuracy_force = correct/total_pairs
        return(accuracy_force)
        
      })
      accuracy_all = rbind(accuracy_all,mean(as.numeric(cv)))
    }
    realMean = mean(accuracy_all[,1])
    return(realMean)
  }
  else if (auc_or_acc == "auc"){
    auc_data = data.frame(matrix(nrow = nrow(df), ncol = 0));
    folds = svm_createFolds(df,nfold)
    cv = lapply(folds, function(x) { 
      training_fold = df[-x, ] 
      test_fold = df[x, ] 
      test_fold[-1] = ind_scale(training_fold[-1],test_fold[-1])
      training_fold[-1] = scale(training_fold[-1])
      classifier = svm(formula = good_1 ~ .,
                       data = training_fold,
                       type = 'C-classification',
                       kernel = koi)
      y_pred = predict(classifier, newdata = test_fold[-1])
      y_pred = factor(y_pred, ordered=TRUE)
      return(list(test_fold[, 1], y_pred))
    })
    auc_data$y_real = c(cv$Fold1[[1]],cv$Fold2[[1]],cv$Fold3[[1]],cv$Fold4[[1]],cv$Fold5[[1]])
    auc_data$y_fit = c(cv$Fold1[[2]],cv$Fold2[[2]],cv$Fold3[[2]],cv$Fold4[[2]],cv$Fold5[[2]])
    return(auc_data)
  }
}

#===============================================================================
## Function3: svm_createFolds
svm_createFolds <- function(df,nfold){
  df[,1] = as.factor(df[,1])
  df_y<-df[df[,1] == 1,]
  n = nrow(df_y)
  folds = createFolds(df_y[,1], k = nfold)
  folds_2 = lapply(folds, function(x) {
    x_2 = c(x,x+n)
    return(x_2)
  })
  return(folds_2)
}

#===============================================================================
## Function4: ind_scale
ind_scale <- function(df1,df2){
  df1_mean = sapply(df1, mean)
  df1_sd = sapply(df1, sd)
  df2_scale = df2
  for (c in 1:length(df1_mean)){
    df2_scale[,c] = (df2[,c] - df1_mean[c])/df1_sd[c]
  }
  return(df2_scale)
}

#===============================================================================
## Function5: svm_perm
svm_perm <- function(df,nfold,n_iter,koi){
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = n_iter, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar

  permMean = data.frame();
  for (p in 1:n_iter){
    setTxtProgressBar(pb, p)
    folds = svm_createFolds(df,nfold)
    cv = lapply(folds, function(x) { 
      training_fold = df[-x, ] 
      test_fold = df[x, ] 
      test_fold[-1] = ind_scale(training_fold[-1],test_fold[-1])
      training_fold[-1] = scale(training_fold[-1])

      training_fold$good_1 = sample(factor(training_fold$good_1, levels = c(0, 1)))
      classifier = svm(formula = good_1 ~ .,
                       data = training_fold,
                       type = 'C-classification',
                       kernel = koi)
      y_pred = predict(classifier, newdata = test_fold[-1])
      cm = table(test_fold[, 1], y_pred)
      accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
      return(accuracy)
    })
    permMean = rbind(permMean,mean(as.numeric(cv)))
  }
  return(permMean)
}

#===============================================================================
## Function6: svm_feat_impor
svm_feat_impor <- function(df,feat,nfold,nCV,nPerm){
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = nPerm, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  df_perm = df
  acc_true = svm_cv_accuracy(df,nfold,nCV,"acc","radial")
  acc_perm = data.frame();
  for (i in 1:nPerm){
    setTxtProgressBar(pb, i)
    df_perm[,grepl(feat, colnames(df_perm))] = df_perm[sample(1:nrow(df_perm)),grepl(feat, colnames(df_perm))]
    df_perm$good_1 = factor(df_perm$good_1, levels = c(0, 1))
    folds = svm_createFolds(df_perm,nfold)
    cv = lapply(folds, function(x) { 
      training_fold = df_perm[-x, ] 
      test_fold = df_perm[x, ] 
      test_fold[-1] = ind_scale(training_fold[-1],test_fold[-1])
      training_fold[-1] = scale(training_fold[-1])
      classifier = svm(formula = good_1 ~ .,
                       data = training_fold,
                       type = 'C-classification',
                       kernel = 'radial')
      y_pred = predict(classifier, newdata = test_fold[-1])
      cm = table(test_fold[, 1], y_pred)
      accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
      return(accuracy)
    })
    acc_perm = rbind(acc_perm,mean(as.numeric(cv)))
    colnames(acc_perm) = c("acc_perm")
  }
  acc_diff = acc_true - colMeans(acc_perm)
  p = mean(acc_true < acc_perm)
  return(list(acc_true,acc_perm,acc_diff,p))
}

#===============================================================================
## Function7: svm_general_accuracy
svm_general_accuracy <- function(df1,df2,acc_or_auc){
  df1$good_1 = factor(df1$good_1, levels = c(0, 1))
  df2$good_1 = factor(df2$good_1, levels = c(0, 1))
  training_fold = df1 
  test_fold = df2 
  test_fold[-1] = ind_scale(training_fold[-1],test_fold[-1])
  training_fold[-1] = scale(training_fold[-1])
  classifier = svm(formula = good_1 ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred = predict(classifier, newdata = test_fold[-1])
  #browser()
  if (acc_or_auc == "acc"){
    cm = table(test_fold[, 1], y_pred)
    accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
    return(accuracy)
  }else{
    auc_data = data.frame(matrix(nrow = length(y_pred), ncol = 0));
    y_pred = factor(y_pred, ordered=TRUE)
    auc_data$y_real = test_fold[, 1]
    auc_data$y_fit = y_pred
    return(auc_data)
  }
}

#===============================================================================
## Function8: svm_general_accuracy_perm
svm_general_accuracy_perm <- function(df1,df2,n_iter){
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = n_iter, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  permMean = data.frame();
  for (p in 1:n_iter){
    setTxtProgressBar(pb, p)
    df_rand = df2
    df_rand$good_1 = sample(factor(df2$good_1, levels = c(0, 1)))
    df_rand = na.omit(df_rand)
    df1$good_1 = factor(df1$good_1, levels = c(0, 1))
    training_fold = df1 
    test_fold = df_rand
    test_fold[-1] = ind_scale(training_fold[-1],test_fold[-1])
    training_fold[-1] = scale(training_fold[-1])
    classifier = svm(formula = good_1 ~ .,
                     data = training_fold,
                     type = 'C-classification',
                     kernel = 'radial')
    y_pred = predict(classifier, newdata = test_fold[-1])
    cm = table(test_fold[, 1], y_pred)
    accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
    permMean = rbind(permMean,accuracy)
  }
  return(permMean)
}

#===============================================================================
## Function9: extract_match_win_lose_mean
extract_match_win_lose_mean <- function(df_feature,df_win,feature,match_style){
  
  df_feature$Awin = rep(NaN, nrow(df_feature))
  df_feature$Alose = rep(NaN, nrow(df_feature))
  df_feature_Trial = df_feature[,grepl('R', colnames(df_feature))]
  df_win_Trial = df_win[,grepl('_wl', colnames(df_win))]
  
  for (n in c(1:nrow(df_win))){
    IndexWin = df_win_Trial[n,] == 1
    IndexLose = df_win_Trial[n,] == 0
    Nwin = sum(IndexWin)
    Nlose = sum(IndexLose)
    TrialWin = data.frame()
    TrialLose = data.frame()
    TrialWin = df_feature_Trial[df_win$n[n] == df_feature$n, IndexWin]
    TrialLose = df_feature_Trial[df_win$n[n] == df_feature$n, IndexLose]
    
    if (Nwin > Nlose){
      
      if (match_style == "decrease"){
        Rwin = sample(1:Nwin)
        df_feature$Awin[df_win$n[n] == df_feature$n] = rowMeans(TrialWin[,Rwin[1:Nlose]])
        df_feature$Alose[df_win$n[n] == df_feature$n] = rowMeans(TrialLose)
      }else if (match_style == "increase"){
        Rlose = sample(1:Nlose, Nwin-Nlose, replace = TRUE)
        df_feature$Alose[df_win$n[n] == df_feature$n] = rowMeans(cbind(TrialLose,TrialLose[,Rlose]))
        df_feature$Awin[df_win$n[n] == df_feature$n] = rowMeans(TrialWin)
      }else{
        Rlose = sample(1:Nlose, match_style, replace = TRUE)
        Rwin = sample(1:Nwin, match_style, replace = TRUE)
        #browser()
        #cat(sprintf('n = %d, Rlose = %d, Rwin = %d.\n', df_win$n[n], Rlose, Rwin))
        if (match_style == 1){
          df_feature$Awin[df_win$n[n] == df_feature$n] = TrialWin[,Rwin]
          df_feature$Alose[df_win$n[n] == df_feature$n] = TrialLose[,Rlose]
        }else{
          df_feature$Awin[df_win$n[n] == df_feature$n] = rowMeans(TrialWin[,Rwin])
          df_feature$Alose[df_win$n[n] == df_feature$n] = rowMeans(TrialLose[,Rlose])
        }
      }
      
    }else{
      if (match_style == "decrease"){
        Rlose = sample(1:Nlose)
        df_feature$Alose[df_win$n[n] == df_feature$n] = rowMeans(TrialLose[,Rlose[1:Nwin]])
        df_feature$Awin[df_win$n[n] == df_feature$n] = rowMeans(TrialWin)
      }else if (match_style == "increase"){
        Rwin = sample(1:Nwin, Nlose-Nwin, replace = TRUE)
        df_feature$Awin[df_win$n[n] == df_feature$n] = rowMeans(cbind(TrialWin,TrialWin[,Rwin]))
        df_feature$Alose[df_win$n[n] == df_feature$n] = rowMeans(TrialLose)
      }else{
        Rlose = sample(1:Nlose, match_style, replace = TRUE)
        Rwin = sample(1:Nwin, match_style, replace = TRUE)
        #browser()
        #cat(sprintf('n = %d, Rlose = %d, Rwin = %d.\n', df_win$n[n], Rlose, Rwin))
        if (match_style == 1){
          df_feature$Awin[df_win$n[n] == df_feature$n] = TrialWin[,Rwin]
          df_feature$Alose[df_win$n[n] == df_feature$n] = TrialLose[,Rlose]
        }else{
          df_feature$Awin[df_win$n[n] == df_feature$n] = rowMeans(TrialWin[,Rwin])
          df_feature$Alose[df_win$n[n] == df_feature$n] = rowMeans(TrialLose[,Rlose])
        }
      }
    }
  }
  df_need_dcast = reorg_df(df_feature,feature)
  return(df_need_dcast)
}


#===============================================================================
## Function10: reorg_df
reorg_df <- function(df_feature,feature){
  df_feature = na.omit(df_feature)
  df_feature_var = df_feature[,!grepl('R|Agood|Abad', colnames(df_feature))]
  for (c in 1:ncol(df_feature_var)){
    if ((colnames(df_feature_var)[c] == "Gender(M=1)")|(colnames(df_feature_var)[c] == "Gender.M.1.")){
      colnames(df_feature_var)[c] = "GenderM1"
    }else if((colnames(df_feature_var)[c] == "role(A=1)")|(colnames(df_feature_var)[c] == "role.A.1.")){
      colnames(df_feature_var)[c] = "RoleA1"
      df_feature_var$RoleA1 = gsub('1','Attack',df_feature_var$RoleA1)
      df_feature_var$RoleA1 = gsub('2','Defend',df_feature_var$RoleA1)
      df_feature_var$RoleA1 = factor(df_feature_var$RoleA1, levels = c("Attack","Defend"))
      
    }else if((colnames(df_feature_var)[c] == "lead(L/LF=1)")|(colnames(df_feature_var)[c] == "lead.L.LF.1.")){
      colnames(df_feature_var)[c] = "LeadL1"
      df_feature_var$LeadL1 = gsub('1','L',df_feature_var$LeadL1)
      df_feature_var$LeadL1 = gsub('0','F',df_feature_var$LeadL1)
      df_feature_var$LeadL1 = factor(df_feature_var$LeadL1, levels = c("L","F"))
      
    }else if((colnames(df_feature_var)[c] == "CH/CH_pair")|(colnames(df_feature_var)[c] == "CH.CH_pair")){
      colnames(df_feature_var)[c] = "CH"
      for (c in 1:length(df_feature_var$CH)){
        if (df_feature_var$CH[c] == 1|df_feature_var$CH[c] == 3|df_feature_var$CH[c] == 4|df_feature_var$CH[c] == 7|df_feature_var$CH[c] == 9){
          df_feature_var$CH[c] <- paste0("rTPJ_CH0",df_feature_var$CH[c])
        }else if (df_feature_var$CH[c] == 10|df_feature_var$CH[c] == 13){
          df_feature_var$CH[c] <- paste0("rTPJ_CH",df_feature_var$CH[c])
        }else if (df_feature_var$CH[c] == 2|df_feature_var$CH[c] == 5|df_feature_var$CH[c] == 6|df_feature_var$CH[c] == 8){
          df_feature_var$CH[c] <- paste0("rDLPFC_CH0",df_feature_var$CH[c])
        }else if (df_feature_var$CH[c] == 11|df_feature_var$CH[c] == 12|df_feature_var$CH[c] == 14){
          df_feature_var$CH[c] <- paste0("rDLPFC_CH",df_feature_var$CH[c])
        }else{
          df_feature_var$CH[c] <- paste0("conn_CH",df_feature_var$CH[c])
        }
      }
    }
  }
  df_need = cbind(df_feature_var$n,df_feature_var$RoleA1,df_feature_var$LeadL1,df_feature_var$CH,df_feature[,(ncol(df_feature)-1):ncol(df_feature)])
  colnames(df_need) = c("n","RoleA1","LeadL1","CH","Agood","Abad")
  
  if (feature == "FC_"){
    df_need$CH = factor(df_need$CH, levels = c("conn_CH102","conn_CH105","conn_CH106","conn_CH108","conn_CH111","conn_CH112","conn_CH114","conn_CH302","conn_CH305","conn_CH306","conn_CH308","conn_CH311","conn_CH312","conn_CH314","conn_CH402","conn_CH405","conn_CH406","conn_CH408","conn_CH411","conn_CH412","conn_CH414","conn_CH702","conn_CH705","conn_CH706","conn_CH708","conn_CH711","conn_CH712","conn_CH714","conn_CH902","conn_CH905","conn_CH906","conn_CH908","conn_CH911","conn_CH912","conn_CH914","conn_CH1002","conn_CH1005","conn_CH1006","conn_CH1008","conn_CH1011","conn_CH1012","conn_CH1014","conn_CH1302","conn_CH1305","conn_CH1306","conn_CH1308","conn_CH1311","conn_CH1312","conn_CH1314"))
  }else{
    df_need$CH = factor(df_need$CH, levels = c("rTPJ_CH01","rDLPFC_CH02","rTPJ_CH03","rTPJ_CH04","rDLPFC_CH05","rDLPFC_CH06","rTPJ_CH07","rDLPFC_CH08","rTPJ_CH09","rTPJ_CH10","rDLPFC_CH11","rDLPFC_CH12","rTPJ_CH13","rDLPFC_CH14"))
  }
  
  df_need_good = df_need[,1:(ncol(df_need)-1)]
  df_need_bad = df_need[,c(1:(ncol(df_need)-2),ncol(df_need))]
  
  df_need_good_dcast<-dcast(df_need_good, n ~ RoleA1 + LeadL1 + CH)
  df_need_bad_dcast<-dcast(df_need_bad, n ~ RoleA1 + LeadL1 + CH)
 
  df_need_good_dcast$good_1 = rep(1,nrow(df_need_good_dcast))
  df_need_bad_dcast$good_1 = rep(0,nrow(df_need_bad_dcast))
  
  df_need_dcast = rbind(df_need_good_dcast,df_need_bad_dcast)
  df_need_dcast <- df_need_dcast %>% relocate(good_1,.before=n)
  colnames(df_need_dcast)[grepl('CH',colnames(df_need_dcast))] = paste0(feature,colnames(df_need_dcast[,grepl('CH',colnames(df_need_dcast))]))
  
  return(df_need_dcast)
}

#===============================================================================
## Function11: extract_pay_beh_mean
extract_pay_beh_mean <- function(df_y,subID, type){
  if ((subID == "all")&(type == "control")){
    df_y$pay_high = rep(NaN, nrow(df_y))
    df_y$pay_low = rep(NaN, nrow(df_y))
    df_y$high = rep(NaN, nrow(df_y))
    df_y$low = rep(NaN, nrow(df_y))
    for (s in 1:nrow(df_y)){
      pay = df_y[s, grepl('_pay', colnames(df_y))]
      win = df_y[s, grepl('_wl', colnames(df_y))]
      index_win = win==1
      index_lose = win==0
      median_win = median(as.numeric(pay[,index_win]))
      median_lose = median(as.numeric(pay[,index_lose]))
      index_high_win = index_win & (pay>median_win)
      index_high_lose = index_lose & (pay>median_lose)
      index_low_win = index_win & (pay<median_win)
      index_low_lose = index_lose & (pay<median_lose)
      index_high = index_high_win | index_high_lose
      index_low = index_low_win | index_low_lose
      trial_high = pay[,index_high]
      trial_low = pay[,index_low]
      if (is.null(ncol(trial_high)) | is.null(ncol(trial_low))){
        next
      }else if (ncol(trial_high)>2 & ncol(trial_low)>2){
        df_y$pay_high[s] = rowMeans(trial_high)
        df_y$pay_low[s] = rowMeans(trial_low)
        df_y$high[s] = 1
        df_y$low[s] = 0
        df_y$n_high[s] = df_y$n[s]
        df_y$n_low[s] = df_y$n[s]
      }
    }
    df_pay_high = cbind(df_y$n_high, df_y$high, df_y$pay_high)
    df_pay_low = cbind(df_y$n_low, df_y$low,df_y$pay_low)
    df_pay = rbind(df_pay_high, df_pay_low)
    df_pay = na.omit(df_pay)
    colnames(df_pay) = c("subID", "pay_level", "beh_mean")
    return(as.data.frame(df_pay))
    
  }else if((subID == "all")&(type == "non-control")){
    df_y$pay_high = rep(NaN, nrow(df_y))
    df_y$pay_low = rep(NaN, nrow(df_y))
    df_y$high = rep(NaN, nrow(df_y))
    df_y$low = rep(NaN, nrow(df_y))
    for (s in 1:nrow(df_y)){
      pay = df_y[s, grepl('_pay', colnames(df_y))]
      median_all = median(as.numeric(pay))
      index_high = pay > median_all
      index_low = pay < median_all
      trial_high = pay[,index_high]
      trial_low = pay[,index_low]
      if (is.null(ncol(trial_high)) | is.null(ncol(trial_low))){
        next
      }else if (ncol(trial_high)>2 & ncol(trial_low)>2){
        df_y$pay_high[s] = rowMeans(trial_high)
        df_y$pay_low[s] = rowMeans(trial_low)
        df_y$high[s] = 1
        df_y$low[s] = 0
        df_y$n_high[s] = df_y$n[s]
        df_y$n_low[s] = df_y$n[s]
      }
    }
    df_pay_high = cbind(df_y$n_high, df_y$high, df_y$pay_high)
    df_pay_low = cbind(df_y$n_low, df_y$low,df_y$pay_low)
    df_pay = rbind(df_pay_high, df_pay_low)
    df_pay = na.omit(df_pay)
    colnames(df_pay) = c("subID", "pay_level", "beh_mean")
    return(as.data.frame(df_pay))
    
  }else{
    if (type == "control"){
      pay = df_y[df_y$n == subID, grepl('_pay', colnames(df_y))]
      win = df_y[df_y$n == subID, grepl('_wl', colnames(df_y))]
      index_win = win==1
      index_lose = win==0
      median_win = median(as.numeric(pay[,index_win]))
      median_lose = median(as.numeric(pay[,index_lose]))
      median_all = median(as.numeric(pay))
      index_high_win = index_win & (pay>median_win)
      index_high_lose = index_lose & (pay>median_lose)
      index_low_win = index_win & (pay<median_win)
      index_low_lose = index_lose & (pay<median_lose)
      index_high = index_high_win | index_high_lose
      trial = 1:ncol(pay)
      df_pay = data.frame(
        pay = as.numeric(pay[1,]),
        win = as.numeric(win[1,]),
        high = as.numeric(index_high),
        trial = trial
      )
      return(list(df_pay, median_win, median_lose, median_all))
    }else if (type == "non-control"){
      pay = df_y[df_y$n == subID, grepl('_pay', colnames(df_y))]
      win = df_y[df_y$n == subID, grepl('_wl', colnames(df_y))]
      median_all = median(as.numeric(pay))
      index_high = pay > median_all
      index_low = pay < median_all
      trial = 1:ncol(pay)
      df_pay = data.frame(
        pay = as.numeric(pay[1,]),
        win = as.numeric(win[1,]),
        high = as.numeric(index_high),
        trial = trial
      )
      return(list(df_pay, median_all))
    }
    
  }
}

#===============================================================================
## Function12: perm_t_independ
perm_t_independ <- function(list_fi,fi_1,fi_2){
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = 5000, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  dif <- vector(length = 5000)
  list_fi_1 <- list_fi[[fi_1]][[2]]
  list_fi_2 <- list_fi[[fi_2]][[2]]
  fi_combine <- rbind(list_fi_1, list_fi_2)
  fi_combine$feature <- rep(c(1,2), each = 5000)
  
  obs_dif <- diff(tapply(X = fi_combine$acc_perm,
                         INDEX = fi_combine$feature,
                         FUN = mean))
  
  for (i in 1:length(dif)){
    setTxtProgressBar(pb, i)
    fi_combine$feature <- sample(fi_combine$feature)
    dif[i] <- diff(tapply(X = fi_combine$acc_perm,
                          INDEX = fi_combine$feature,
                          FUN = mean))
  }
  
  p_two = mean(abs(dif) >= abs(obs_dif))
  
  return(list(obs_dif, dif, p_two))
}

#===============================================================================
## Function13: track_regress
track_regress <- function(df_invest){
  df_invest_Trial = df_invest[,grepl('_inv', colnames(df_invest))]
  df_invest_Manip = df_invest[,!grepl('_inv', colnames(df_invest))]
  delta_invest = df_invest_Trial[,2:24] - df_invest_Trial[,1:23]
  ingroup_mean = data.frame(matrix(nrow = nrow(df_invest), ncol = 23));
  ingroup_dif = data.frame(matrix(nrow = nrow(df_invest), ncol = 23));
  ingroup_sum = data.frame(matrix(nrow = nrow(df_invest), ncol = 23));
  outgroup_sum = data.frame(matrix(nrow = nrow(df_invest), ncol = 23));
  alpha_dif = vector(length = nrow(df_invest))
  alpha_sum = vector(length = nrow(df_invest))
  beta_sum = vector(length = nrow(df_invest))
  
  for (r in 1:nrow(df_invest)){
    ingroup_data = df_invest_Trial[(df_invest$n == df_invest$n[r])&(df_invest$role.A.1. == df_invest$role.A.1.[r]),]
    outgroup_data = df_invest_Trial[(df_invest$n == df_invest$n[r])&(df_invest$role.A.1. != df_invest$role.A.1.[r]),]
    ingroup_mean[r,] = colMeans(ingroup_data[,1:23])
    ingroup_dif[r,] = df_invest_Trial[r,1:23] - ingroup_mean[r,]
    ingroup_sum[r,] = colSums(ingroup_data[,1:23])
    outgroup_sum[r,] = colSums(outgroup_data[,1:23])
   
    z_delta_invest = scale(as.numeric(delta_invest[r,]))
    z_ingroup_dif = scale(as.numeric(ingroup_dif[r,]))
    z_ingroup_sum = scale(as.numeric(ingroup_sum[r,]))
    z_outgroup_sum = scale(as.numeric(outgroup_sum[r,]))
    
    lm <- lm(z_delta_invest ~ z_ingroup_dif + z_ingroup_sum + z_outgroup_sum)
    alpha_dif[r] = lm$coefficients[2]
    alpha_sum[r] = lm$coefficients[3]
    beta_sum[r] = lm$coefficients[4]
  }
  reg_coef = cbind(df_invest_Manip, data.frame(alpha1 = alpha_dif,alpha2 = alpha_sum,beta1 = beta_sum))
  reg_coef_lead = reg_coef[reg_coef$lead.L.1. == 1,]
  reg_coef_follow_attack = reg_coef[(reg_coef$lead.L.1. == 0)&(reg_coef$role.A.1. == 1),]
  reg_coef_follow_defend = reg_coef[(reg_coef$lead.L.1. == 0)&(reg_coef$role.A.1. == 2),]
  uniqN = unique(reg_coef_follow_attack$n)
  reg_coef_follow_attack_mean = data.frame(matrix(nrow = length(uniqN), ncol = ncol(reg_coef_follow_attack)));
  reg_coef_follow_defend_mean = data.frame(matrix(nrow = length(uniqN), ncol = ncol(reg_coef_follow_defend)));
  
  for (f in 1:length(uniqN)){
    reg_coef_follow_attack_mean[f,] = colMeans(reg_coef_follow_attack[reg_coef_follow_attack$n == uniqN[f],])
    reg_coef_follow_defend_mean[f,] = colMeans(reg_coef_follow_defend[reg_coef_follow_defend$n == uniqN[f],])
  }
  reg_coef_follow = rbind(reg_coef_follow_attack_mean,reg_coef_follow_defend_mean)
  colnames(reg_coef_follow) = colnames(reg_coef_lead)
  reg_coef_mean = rbind(reg_coef_lead, reg_coef_follow)
  reg_coef_mean = reg_coef_mean[order(reg_coef_mean$n, reg_coef_mean$role.A.1., reg_coef_mean$lead.L.1.),]
  return(reg_coef_mean)
}

#===============================================================================
## Function14: extract_feature_all_trial_mean
extract_feature_all_trial_mean <- function(df_brain, noi, AD, LF, CH){
  df_feature = df_brain[(df_brain$role.A.1. == AD)&(df_brain$lead.L.LF.1. == LF)&(df_brain$CH.CH_pair == CH), ]
  df_fit = df_feature %>% filter(n %in% noi)
  df_fit_Trial = df_fit[,grepl('R', colnames(df_fit))]
  df_fit_feature = df_fit[,!grepl('R', colnames(df_fit))]
  df_fit_feature$Brain = rowMeans(df_fit_Trial)
  df_fit_feature = df_fit_feature[,c(1, ncol(df_fit_feature))]
  df_fit_feature_new = df_fit_feature[order(df_fit_feature$n),]
  return(df_fit_feature_new)
}

#===============================================================================
## Function15: compare_win_lose
compare_win_lose <- function(df_winlose, AD, LF, CH_key){
  df_feature = df_winlose[(df_winlose$role == AD)&(df_winlose$Lead == LF), grepl(CH_key, colnames(df_winlose))]
  #browser()
  df_feature_win = df_feature[, grepl('_win', colnames(df_feature))]
  df_feature_lose = df_feature[, grepl('_lose', colnames(df_feature))]
  df_feature_average = data.frame(cbind(rowMeans(df_feature_win),rowMeans(df_feature_lose)))
  colnames(df_feature_average) = c("win","lose")
  df_feature_stack = stack(df_feature_average)
  t_param = t.test(values~ind, data = df_feature_stack, paired = TRUE, alternative = "two.sided")
  d_param = cohens_d(values~ind, data = df_feature_stack, paired = TRUE)
  m_param = df_feature_stack %>%
    group_by(ind) %>%
    get_summary_stats(values, type = "mean_sd")
  #browser()
  out_param = data.frame(mean_win = m_param$mean[1],
                         sd_win = m_param$sd[1],
                         mean_lose = m_param$mean[2],
                         sd_lose = m_param$sd[2],
                         t_win_lose = t_param$statistic[1],
                         df = t_param$parameter[1],
                         p_value = t_param$p.value,
                         cohenD = d_param$effsize)
  print(out_param)
  return(out_param)
}

#===============================================================================
## Function16: svm_lime
svm_lime <- function(df1,df2,n_b,target_label,n_feat,select_method){
  df1$good_1[df1$good_1 == 1] <- 'yes'
  df1$good_1[df1$good_1 == 0] <- 'no'
  df2$good_1[df2$good_1 == 1] <- 'yes'
  df2$good_1[df2$good_1 == 0] <- 'no'
  df1$good_1 = factor(df1$good_1, levels = c("no", "yes"))
  df2$good_1 = factor(df2$good_1, levels = c("no", "yes"))
  training_fold = df1 
  test_fold = df2 
  test_fold[-1] = ind_scale(training_fold[-1],test_fold[-1])
  training_fold[-1] = scale(training_fold[-1])
  classifier <- train(good_1~., data=training_fold, method = 'svmRadial', tuneGrid = data.frame(C=1, sigma = 1/ncol(training_fold[-1])), trControl=trainControl(classProbs = TRUE, method = "none"))
  explainer <- lime(x = training_fold[-1], model = classifier, n_bins = n_b)
  explanation <- lime::explain(test_fold[-1], explainer, labels = target_label, n_features = n_feat, feature_select = select_method)
  
  return(explanation)
}

#===============================================================================
## Function17: modified_lime_plot_explanations
modified_lime_plot_explanations <- function (explanation, ...) 
{
  num_cases <- unique(suppressWarnings(as.numeric(explanation$case)))
  if (!anyNA(num_cases)) {
    explanation$case <- factor(explanation$case, levels = as.character(sort(num_cases)))
  }
  
  p <- ggplot(explanation, aes_(~case, ~feature_desc)) + geom_tile(aes_(fill = ~feature_weight)) + 
    scale_x_discrete("Case", expand = c(0, 0)) + scale_y_discrete("Feature", expand = c(0, 0)) + 
    scale_fill_gradient2("Feature\nweight", low = "firebrick", mid = "#f7f7f7", high = "steelblue") + 
    modified_theme_lime() + 
    theme(panel.border = element_rect(fill = NA, colour = "grey60", size = 1), 
          panel.grid = element_blank(), 
          legend.position = "right", 
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  if (is.null(explanation$label)) {
    p
  }
  else {
    p + facet_wrap(~label, ...)
  }
}

#===============================================================================
## Function18: modified_theme_lime
modified_theme_lime <- function(...) {
  theme_minimal() +
    theme(
      strip.text = element_text(face = 'bold', size = 9),
      plot.margin = margin(15, 15, 15, 15),
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = 'bottom',
      panel.spacing.y = unit(15, 'pt'),
      strip.text.x = element_text(margin = margin(t = 2, b = 2), hjust = 0),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      ...
    )
}

#===============================================================================
## Function19: svm_ale
svm_ale <- function(df,foi){
  #df$good_1[df$good_1 == 1] <- 'yes'
  #df$good_1[df$good_1 == 0] <- 'no'
  #df$good_1 = factor(df$good_1, levels = c("no", "yes"))
  training_fold = df
  training_fold[-1] = scale(training_fold[-1])
  classifier <- caret::train(good_1~., data=training_fold, method = 'svmRadial', tuneGrid = data.frame(C=1, sigma = 1/ncol(training_fold[-1])), trControl=trainControl(classProbs = TRUE, method = "none"))
  #predictor <- Predictor$new(classifier, data = training_fold[-1], y = training_fold$good_1, type = "prob")
  predictor <- Predictor$new(classifier, data = training_fold[-1], y = training_fold$good_1)
  if (foi == "all"){
    ale <- FeatureEffects$new(predictor)
  }else{
    ale <- FeatureEffect$new(predictor,feature = foi)
  }
  
  return(ale)
}

#===============================================================================
## Function20: ilm_feat_impor
ilm_feat_impor <- function(df, n_perm){
  training_fold = df
  training_fold[-1] = scale(training_fold[-1])
  classifier <- caret::train(good_1~., data=training_fold, method = 'svmRadial', tuneGrid = data.frame(C=1, sigma = 1/ncol(training_fold[-1])), trControl=trainControl(classProbs = TRUE, method = "none"))
  predictor <- Predictor$new(classifier, data = training_fold[-1], y = training_fold$good_1)
  imp <- FeatureImp$new(predictor, loss = "ce", compare = "difference", n.repetitions = n_perm)
  return(imp)
}

#===============================================================================
## Function21: svm_lime_imporfeat
svm_lime_imporfeat <- function(df1,df2,n_b=2,target_label,n_feat,select_method){
  df1$good_1[df1$good_1 == 1] <- 'yes'
  df1$good_1[df1$good_1 == 0] <- 'no'
  df1$good_1 = factor(df1$good_1, levels = c("no", "yes"))
  training_fold = df1 
  test_fold = df2 
  #browser()
  classifier <- train(good_1~., data=training_fold, method = 'svmRadial', tuneGrid = data.frame(C=1, sigma = 1/ncol(training_fold[-1])), trControl=trainControl(classProbs = TRUE, method = "none"))
  explainer <- lime(x = training_fold[-1], model = classifier, n_bins = n_b)
  explanation <- lime::explain(test_fold, explainer, labels = target_label, n_features = n_feat, feature_select = select_method)
  
  return(explanation)
}

#===============================================================================
## Function22: svm_perm_trial
svm_perm_trial <- function(df_BOLD,df_FC,df_WNS,df_BNS,df_y,foi,nfold,n_iter,koi){
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = n_iter, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  permACC = data.frame();
  for (p in 1:n_iter){
    setTxtProgressBar(pb, p)
    BOLD_mean = extract_win_or_pay_mean(df_BOLD,df_y,"BOLD_","win","separate","current",FALSE,TRUE)
    FC_mean = extract_win_or_pay_mean(df_FC,df_y,"FC_","win","separate","current",FALSE,TRUE)
    WNS_mean = extract_win_or_pay_mean(df_WNS,df_y,"WNS_","win","separate","current",FALSE,TRUE)
    BNS_mean = extract_win_or_pay_mean(df_BNS,df_y,"BNS_","win","separate","current",FALSE,TRUE)
    if (foi == "all"){
      data_invest = cbind(WNS_mean[,c(1,4:ncol(WNS_mean))],BNS_mean[,4:ncol(BNS_mean)],BOLD_mean[,4:ncol(BOLD_mean)],FC_mean[,4:ncol(FC_mean)])
    }else if (foi == "intra"){
      data_invest = cbind(BOLD_mean[,c(1,4:ncol(BOLD_mean))],FC_mean[,4:ncol(FC_mean)])
    }else if (foi == "inter"){
      data_invest = cbind(WNS_mean[,c(1,4:ncol(WNS_mean))],BNS_mean[,4:ncol(BNS_mean)])
    }
    acc = svm_cv_accuracy(data_invest,nfold,1,"acc","radial")
    permACC = rbind(permACC,acc)
  }
  
  return(permACC)
}

#===============================================================================
## Function23: calc_win_trialN
calc_win_trialN <- function(df_y){
  df_y_win_Trial = df_y[,grepl('_wl', colnames(df_y))]
  winTrialN = data.frame();
  loseTrialN = data.frame();
  for (n in c(1:nrow(df_y))){
      IndexWin = df_y_win_Trial[n,] == 1
      IndexLose = df_y_win_Trial[n,] == 0
      winTrialN = rbind(winTrialN,sum(IndexWin))
      loseTrialN = rbind(loseTrialN,sum(IndexLose))
  }
  colnames(winTrialN) = "winN"
  colnames(loseTrialN) = "loseN"
  trialN = data.frame(winTrialN, loseTrialN)
  #browser()
  cat(sprintf('win_mean = %f, win_sd = %f, lose_mean = %f, lose_sd = %f.\n', mean(winTrialN[,1]), sd(winTrialN[,1]), mean(loseTrialN[,1]), sd(loseTrialN[,1])))
  return(trialN)
}

#===============================================================================
## Function24: svm_cv_accuracy_distribution
svm_cv_accuracy_distribution <- function(df,nfold,n_iter,koi){
  df[,1] = factor(df[,1], levels = c(0, 1))
  df = na.omit(df)
  accuracy_all = data.frame()
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = n_iter, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  for (i in 1:n_iter){
    setTxtProgressBar(pb, i)
    folds = svm_createFolds(df,nfold)
    
    cv = lapply(folds, function(x) { 
      training_fold = df[-x, ] 
      test_fold = df[x, ] 
      test_fold[-1] = ind_scale(training_fold[-1],test_fold[-1])
      training_fold[-1] = scale(training_fold[-1])
      classifier = svm(formula = good_1 ~ .,
                       data = training_fold,
                       type = 'C-classification',
                       kernel = koi)
      y_pred = predict(classifier, newdata = test_fold[-1])
      cm = table(test_fold[, 1], y_pred)
      accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
      return(accuracy)
    })
    accuracy_all = rbind(accuracy_all,mean(as.numeric(cv)))
  }
  return(accuracy_all[,1])
}

#===============================================================================
## Function25: lm_cv_correlation
lm_cv_correlation <- function(df,nfold,n_iter,out_mat){
  df = na.omit(df)
  colnames(df)[1] = "y_value"
  out_all = data.frame()
  for (i in 1:n_iter){
      folds = createFolds(df[,1], k = nfold)
      
      cv = lapply(folds, function(x) { 
      training_fold = df[-x, ] 
      test_fold = df[x, ] 
      test_fold[-1] = ind_scale(training_fold[-1],test_fold[-1])
      training_fold[-1] = scale(training_fold[-1])
      model = lm(formula = y_value ~ .,data = training_fold)
      #browser()
      y_pred = predict(model, newdata = test_fold[-1])
      
      if(out_mat == 'r_value'){
        r_value = cor(test_fold$y_value, y_pred)
        return(r_value)
      }else if(out_mat == 'mse_value'){
        mse_value = mean((test_fold$y_value - y_pred)^2)
        return(mse_value)
      }
      })
      out_all = rbind(out_all,mean(as.numeric(cv)))
    }
    realMean = mean(out_all[,1])
    return(realMean)
}

#===============================================================================
## Function26: lm_perm
lm_perm <- function(df,nfold,n_iter,out_mat){
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = n_iter, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  df = na.omit(df)
  colnames(df)[1] = "y_value"
  permMean = data.frame();
  for (p in 1:n_iter){
    setTxtProgressBar(pb, p)
    folds = createFolds(df[,1], k = nfold)
    cv = lapply(folds, function(x) { 
      training_fold = df[-x, ] 
      test_fold = df[x, ] 
      test_fold[-1] = ind_scale(training_fold[-1],test_fold[-1])
      training_fold[-1] = scale(training_fold[-1])
      
      training_fold$y_value = sample(training_fold$y_value)
      
      model = lm(formula = y_value ~ .,data = training_fold)
      #browser()
      y_pred = predict(model, newdata = test_fold[-1])
      if(out_mat == 'r_value'){
        r_value = cor(test_fold$y_value, y_pred)
        return(r_value)
      }else if(out_mat == 'mse_value'){
        mse_value = mean((test_fold$y_value - y_pred)^2)
        return(mse_value)
      }
    })
    permMean = rbind(permMean,mean(as.numeric(cv)))
  }
  return(permMean)
}

#===============================================================================
## Function27: nested_cv_classifier
nested_cv_classifier <- function(df, nfold, n_iter, classifier_type, permute_or_not) {
  
  pb <- txtProgressBar(min = 0, max = n_iter, style = 3, width = 50, char = "=")
  
  # 数据预处理
  df[,1] = factor(df[,1], levels = c(0, 1))
  df = na.omit(df)
  
  # 根据分类器类型设置参数范围
  if (classifier_type == "svm-rbf") {
    params <- list(
      C_range = 2**seq(-5, 15, length.out = 10),
      gamma_range = 2**seq(-15, 3, length.out = 10)
    )
    cat("使用SVM分类器（带RBF核）\n")
  } else {
    params <- list(C_range = 2**seq(-5, 15, length.out = 10))
    
    # 根据分类器类型输出不同的消息
    switch(classifier_type,
           "logistic_l2" = {
             cat("使用L2正则化逻辑回归\n")
             cat("正则化参数: L2\n")
           },
           "logistic_l1" = {  # 新增L1正则化逻辑回归
             cat("使用L1正则化逻辑回归\n")
             cat("正则化参数: L1\n")
           },
           "linear_svm_l2" = {
             cat("使用L2正则化线性SVM\n")
             cat("正则化参数: L2\n")
           },
           "linear_svm_l1" = {
             cat("使用L1正则化线性SVM\n")
             cat("正则化参数: L1\n")
           },
           cat("使用", classifier_type, "分类器\n")
    )
  }
  
  cat("C参数范围:", params$C_range, "\n")
  if (classifier_type == "svm-rbf") {
    cat("γ参数范围:", params$gamma_range, "\n")
  }
  
  accuracy_all = data.frame()
  all_y_true <- numeric(0)
  all_y_pred <- numeric(0)
  
  for (i in 1:n_iter) {
    setTxtProgressBar(pb, i)
    folds = svm_createFolds(df, nfold)
    
    cv = lapply(folds, function(x) { 
      training_fold = df[-x, ]
      test_fold = df[x, ]
      
      # 数据标准化
      test_fold[-1] = ind_scale(training_fold[-1], test_fold[-1])
      training_fold[-1] = scale(training_fold[-1])

      if (permute_or_not){
        training_fold$good_1 = sample(factor(training_fold$good_1, levels = c(0, 1)))
      }
      
      # 准备矩阵格式数据
      X_train = as.matrix(training_fold[, -1])
      y_train = as.numeric(as.character(training_fold[, 1]))
      
      X_test = as.matrix(test_fold[, -1])
      y_test = as.numeric(as.character(test_fold[, 1]))
      
      if (classifier_type == "svm-rbf") {
        # 原始SVM（带核函数）
        best_params = tune_svm(training_fold, params, nfold)
        
        classifier = svm(
          formula = good_1 ~ .,
          data = training_fold,
          type = "C-classification",
          kernel = "radial",
          cost = best_params$C,
          gamma = best_params$gamma
        )
        
        y_pred = predict(classifier, newdata = test_fold[-1])
        y_pred_num = as.numeric(as.character(y_pred))
        y_test_num = as.numeric(as.character(test_fold[,1]))

        cm = table(test_fold[, 1], y_pred)
        
        return(list(accuracy = sum(diag(cm))/sum(cm), 
                    best_C = best_params$C, 
                    best_gamma = best_params$gamma),
                    y_true = y_test_num,
                    y_pred = y_pred_num)
        
      } else if (classifier_type == "logistic_l2") {
        # L2正则化逻辑回归
        best_C = tune_glmnet(X_train, y_train, params$C_range, nfold, alpha = 0)
        
        # 训练最终模型
        model = glmnet(
          x = X_train,
          y = y_train,
          family = "binomial",
          alpha = 0,  # L2正则化
          lambda = 1/best_C,
          standardize = FALSE
        )
        
        # 预测
        pred_prob = predict(model, newx = X_test, type = "response")
        y_pred = ifelse(pred_prob > 0.5, 1, 0)
        cm = table(y_test, y_pred)
        
        return(list(accuracy = sum(diag(cm))/sum(cm), best_C = best_C))
        
      } else if (classifier_type == "logistic_l1") {  # 新增L1正则化逻辑回归
        # L1正则化逻辑回归
        best_C = tune_glmnet(X_train, y_train, params$C_range, nfold, alpha = 1)
        
        # 训练最终模型
        model = glmnet(
          x = X_train,
          y = y_train,
          family = "binomial",
          alpha = 1,  # L1正则化
          lambda = 1/best_C,
          standardize = FALSE
        )
        
        # 预测
        pred_prob = predict(model, newx = X_test, type = "response")
        y_pred = ifelse(pred_prob > 0.5, 1, 0)
        cm = table(y_test, y_pred)
        
        return(list(accuracy = sum(diag(cm))/sum(cm), 
                    best_C = best_C,
                    y_true = y_test,
                    y_pred = y_pred))
        
      } else if (classifier_type == "linear_svm_l2") {
        # L2正则化线性SVM
        best_C = tune_liblinear(X_train, y_train, params$C_range, nfold, 
                                type = 1)  # type=1: L2正则化L2-loss SVM
        
        model = LiblineaR(
          data = X_train,
          target = factor(y_train, levels = c(0, 1)),
          type = 1,
          cost = best_C,
          bias = 1,
          verbose = FALSE
        )
        
        y_pred = predict(model, newx = X_test, decisionValues = FALSE)$predictions
        y_pred = as.numeric(as.character(y_pred))
        cm = table(y_test, y_pred)
        
        return(list(accuracy = sum(diag(cm))/sum(cm), 
                    best_C = best_C,
                    y_true = y_test,
                    y_pred = y_pred))
        
      } else if (classifier_type == "linear_svm_l1") {
        # L1正则化线性SVM
        best_C = tune_liblinear(X_train, y_train, params$C_range, nfold, 
                                type = 5)  # type=5: L1正则化L2-loss SVM
        
        model = LiblineaR(
          data = X_train,
          target = factor(y_train, levels = c(0, 1)),
          type = 5,
          cost = best_C,
          bias = 1,
          verbose = FALSE
        )
        
        y_pred = predict(model, newx = X_test, decisionValues = FALSE)$predictions
        y_pred = as.numeric(as.character(y_pred))
        cm = table(y_test, y_pred)
        
        return(list(accuracy = sum(diag(cm))/sum(cm), 
                    best_C = best_C,
                    y_true = y_test,
                    y_pred = y_pred))
        
      } else {
        stop("未知的分类器类型。支持的类型: 'svm', 'logistic_l2', 'logistic_l1', 'linear_svm_l2', 'linear_svm_l1'")
      }
    })
    
    # 收集结果
    accuracies = sapply(cv, function(x) x$accuracy)
    best_Cs = sapply(cv, function(x) x$best_C)

    for (fold_result in cv) {
      all_y_true <- c(all_y_true, fold_result$y_true)
      all_y_pred <- c(all_y_pred, fold_result$y_pred)
    }
    
    if (classifier_type == "svm-rbf") {
      best_gammas = sapply(cv, function(x) x$best_gamma)
      accuracy_all = rbind(accuracy_all, 
                           data.frame(
                             iteration = i,
                             accuracy = mean(accuracies),
                             mean_C = mean(best_Cs),
                             mean_gamma = mean(best_gammas)
                           ))
    } else {
      accuracy_all = rbind(accuracy_all, 
                           data.frame(
                             iteration = i,
                             accuracy = mean(accuracies),
                             mean_C = mean(best_Cs)
                           ))
    }
  }
  
  # 输出结果
  realMean = mean(accuracy_all$accuracy)
  cat("\n嵌套交叉验证完成！\n")
  cat("平均准确率:", round(realMean, 4), "\n")
  cat("平均最佳C参数:", round(mean(accuracy_all$mean_C), 4), "\n")
  
  if (classifier_type == "svm") {
    cat("平均最佳γ参数:", round(mean(accuracy_all$mean_gamma), 6), "\n")
  }

  if (length(all_y_true) > 0 && length(all_y_pred) > 0) {
    # 使用您的函数格式计算准确率置信区间
    bootstrap_result <- acc_95CI(
      y_true_all = all_y_true,
      y_pred_all = all_y_pred,
      nBoot = n_bootstrap
    )
    
    ci_lower <- bootstrap_result$ci[1]
    ci_upper <- bootstrap_result$ci[2]
    bootstrap_mean <- bootstrap_result$mean
    
    ci_method_used <- "Bootstrap (acc_95CI)"
  } 
  
  return(list(
    mean_accuracy = realMean, 
    accuracy_details = accuracy_all,
    parameter_ranges = params,
    classifier_type = classifier_type,
    confidence_interval = list(
      method = ci_method_used,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      ci_level = ci_level,
      bootstrap_mean = bootstrap_mean
    )
  ))
}

#===============================================================================
## Function28: tune_svm
# 辅助函数：调优SVM参数
tune_svm <- function(training_data, params, nfold) {
  tune_grid = expand.grid(C = params$C_range, gamma = params$gamma_range)
  best_accuracy = 0
  best_params = list(C = 1, gamma = 1/ncol(training_data[-1]))
  
  inner_folds = svm_createFolds(training_data, nfold)
  
  for(j in 1:nrow(tune_grid)) {
    inner_accuracies = numeric(length(inner_folds))
    
    for(k in 1:length(inner_folds)) {
      inner_training = training_data[-inner_folds[[k]], ]
      inner_test = training_data[inner_folds[[k]], ]
      
      inner_test[-1] = ind_scale(inner_training[-1], inner_test[-1])
      inner_training[-1] = scale(inner_training[-1])
      
      inner_classifier = svm(
        formula = good_1 ~ .,
        data = inner_training,
        type = "C-classification",
        kernel = "radial",
        cost = tune_grid$C[j],
        gamma = tune_grid$gamma[j]
      )
      
      inner_pred = predict(inner_classifier, newdata = inner_test[-1])
      inner_cm = table(inner_test[, 1], inner_pred)
      inner_accuracies[k] = sum(diag(inner_cm)) / sum(inner_cm)
    }
    
    mean_inner_accuracy = mean(inner_accuracies)
    
    if(mean_inner_accuracy > best_accuracy) {
      best_accuracy = mean_inner_accuracy
      best_params = list(C = tune_grid$C[j], gamma = tune_grid$gamma[j])
    }
  }
  
  return(best_params)
}

#===============================================================================
## Function29: tune_glmnet
# 通用化的glmnet调优函数（支持L1和L2正则化）
tune_glmnet <- function(X_train, y_train, C_range, nfold, alpha) {
  best_accuracy = 0
  best_C = 1
  
  # 创建内层交叉验证的折
  fold_indices = cut(seq(1, nrow(X_train)), breaks = nfold, labels = FALSE)
  fold_indices = sample(fold_indices)
  
  for(C_val in C_range) {
    inner_accuracies = numeric(nfold)
    
    for(k in 1:nfold) {
      test_indices = which(fold_indices == k, arr.ind = TRUE)
      train_indices = which(fold_indices != k, arr.ind = TRUE)
      
      # 使用glmnet训练固定lambda的模型
      model = glmnet(
        x = X_train[train_indices, ],
        y = y_train[train_indices],
        family = "binomial",
        alpha = alpha,  # L1或L2正则化
        lambda = 1/C_val,  # lambda = 1/C
        standardize = FALSE
      )
      
      # 预测概率
      pred_prob = predict(model, newx = X_train[test_indices, ], type = "response")
      # 转换为类别预测
      pred = ifelse(pred_prob > 0.5, 1, 0)
      cm = table(y_train[test_indices], pred)
      inner_accuracies[k] = sum(diag(cm)) / sum(cm)
    }
    
    mean_inner_accuracy = mean(inner_accuracies)
    
    if(mean_inner_accuracy > best_accuracy) {
      best_accuracy = mean_inner_accuracy
      best_C = C_val
    }
  }
  
  return(best_C)
}

#===============================================================================
## Function30: tune_liblinear
# 辅助函数：调优LiblineaR参数
tune_liblinear <- function(X_train, y_train, C_range, nfold, type) {
  best_accuracy = 0
  best_C = 1
  
  fold_indices = cut(seq(1, nrow(X_train)), breaks = nfold, labels = FALSE)
  fold_indices = sample(fold_indices)
  
  for(C_val in C_range) {
    inner_accuracies = numeric(nfold)
    
    for(k in 1:nfold) {
      test_indices = which(fold_indices == k, arr.ind = TRUE)
      train_indices = which(fold_indices != k, arr.ind = TRUE)
      
      model = LiblineaR(
        data = X_train[train_indices, ],
        target = factor(y_train[train_indices], levels = c(0, 1)),
        type = type,
        cost = C_val,
        bias = 1,
        verbose = FALSE
      )
      
      pred = predict(model, newx = X_train[test_indices, ], 
                     decisionValues = FALSE)$predictions
      pred = as.numeric(as.character(pred))
      cm = table(y_train[test_indices], pred)
      inner_accuracies[k] = sum(diag(cm)) / sum(cm)
    }
    
    mean_inner_accuracy = mean(inner_accuracies)
    
    if(mean_inner_accuracy > best_accuracy) {
      best_accuracy = mean_inner_accuracy
      best_C = C_val
    }
  }
  
  return(best_C)
}

#===============================================================================
## Function32: acc_95CI
acc_95CI <- function(y_true_all, y_score_all, nBoot) {
  acc_boot <- numeric(nBoot)
  N <- length(y_true_all)
  
  for (b in 1:nBoot) {
    idx <- sample(seq_len(N), size = N, replace = TRUE)
    y_b <- y_true_all[idx]
    s_b <- y_score_all[idx]
    
    # 计算准确率（而不是AUC）
    # 这里假设y_score_all是预测的类别（0/1）而不是概率
    # 所以准确率就是预测正确的比例
    acc_boot[b] <- mean(y_b == s_b)
  }
  
  acc_boot <- acc_boot[!is.na(acc_boot)]
  CI95 <- quantile(acc_boot, c(0.025, 0.975))
  
  return(CI95)
}
