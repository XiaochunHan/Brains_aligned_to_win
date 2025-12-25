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
## Function2: reorg_df
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
  
  df_need_good_dcast<-dcast(df_need_good, n ~ RoleA1 + LeadL1 + CH, value.var = "Agood")
  df_need_bad_dcast<-dcast(df_need_bad, n ~ RoleA1 + LeadL1 + CH, value.var = "Abad")
  
  df_need_good_dcast$good_1 = rep(1,nrow(df_need_good_dcast))
  df_need_bad_dcast$good_1 = rep(0,nrow(df_need_bad_dcast))
  
  df_need_dcast = rbind(df_need_good_dcast,df_need_bad_dcast)
  df_need_dcast <- df_need_dcast %>% relocate(good_1,.before=n)
  colnames(df_need_dcast)[grepl('CH',colnames(df_need_dcast))] = paste0(feature,colnames(df_need_dcast[,grepl('CH',colnames(df_need_dcast))]))
  
  return(df_need_dcast)
}
#===============================================================================
## Function3: svm_cv_accuracy
svm_cv_accuracy <- function(df,nfold,n_iter,koi){
  df[,1] = factor(df[,1], levels = c(0, 1))
  df = na.omit(df)
  
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

#===============================================================================
## Function4: svm_createFolds
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
## Function5: ind_scale
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
## Function6: svm_perm
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
## Function7: svm_feat_impor
svm_feat_impor <- function(df,feat,nfold,nCV,nPerm){
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = nPerm, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  df_perm = df
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
  return(acc_perm)
}

#===============================================================================
## Function8: svm_general_accuracy
svm_general_accuracy <- function(df1,df2){
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
  cm = table(test_fold[, 1], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  ci_level = acc_95CI(test_fold[, 1], y_pred,1000)
  
  ci_lower <- ci_level[1]
  ci_upper <- ci_level[2]
  ci_method_used <- "Bootstrap"

  cat("下限:", round(ci_lower, 4), "\n")
  cat("均值:", round(accuracy, 4), "\n")
  cat("上限:", round(ci_upper, 4), "\n")
  cat("方法:", ci_method_used, "\n")
  return(list(accuracy,
              ci_level))
}

#===============================================================================
## Function9: acc_95CI
acc_95CI <- function(y_true_all, y_score_all, nBoot) {
  acc_boot <- numeric(nBoot)
  N <- length(y_true_all)
  
  for (b in 1:nBoot) {
    idx <- sample(seq_len(N), size = N, replace = TRUE)
    y_b <- y_true_all[idx]
    s_b <- y_score_all[idx]
    
    acc_boot[b] <- mean(y_b == s_b)
  }
  
  acc_boot <- acc_boot[!is.na(acc_boot)]
  CI95 <- quantile(acc_boot, c(0.025, 0.975))
  
  return(CI95)
}
#===============================================================================
## Function10: svm_general_accuracy_perm
svm_general_accuracy_perm <- function(df1,df2,n_iter){
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = n_iter, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  permAcc = data.frame();
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
    permAcc = rbind(permAcc,accuracy)
  }
  colnames(permAcc) = "acc_perm"
  return(permAcc)
}

#===============================================================================
## Function11: track_regress
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
## Function12: svm_lime
svm_lime <- function(df,n_b,target_label,n_feat,select_method){
  df$good_1[df$good_1 == 1] <- 'yes'
  df$good_1[df$good_1 == 0] <- 'no'
  df$good_1 = factor(df$good_1, levels = c("no", "yes"))
  df[-1] = scale(df[-1])
  classifier <- train(good_1~., data=df, method = 'svmRadial', tuneGrid = data.frame(C=1, sigma = 1/ncol(training_fold[-1])), trControl=trainControl(classProbs = TRUE, method = "none"))
  explainer <- lime(x = df[-1], model = classifier, n_bins = n_b)
  explanation <- lime::explain(df[-1], explainer, labels = target_label, n_features = n_feat, feature_select = select_method)
  
  return(explanation)
}

#===============================================================================
## Function13: lm_cv_correlation
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
## Function14: lm_perm
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
  colnames(permMean) = 'lm_perm'
  return(permMean)
}

#===============================================================================
## Function15: nested_cv_classifier
nested_cv_classifier <- function(df, nfold, n_iter, classifier_type, permute_or_not, n_bootstrap = 1000, ci_level = 0.95) {
  
  pb <- txtProgressBar(min = 0, max = n_iter, style = 3, width = 50, char = "=")
  
  df[,1] = factor(df[,1], levels = c(0, 1))
  df = na.omit(df)
  
  if (classifier_type == "svm-rbf") {
    params <- list(
      C_range = 2**seq(-5, 15, length.out = 10),
      gamma_range = 2**seq(-15, 3, length.out = 10)
    )
    cat("使用SVM分类器（带RBF核）\n")
  } else {
    params <- list(C_range = 2**seq(-5, 15, length.out = 10))
    
    switch(classifier_type,
           "logistic_l2" = {
             cat("使用L2正则化逻辑回归\n")
             cat("正则化参数: L2\n")
           },
           "logistic_l1" = {
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
      
      test_fold[-1] = ind_scale(training_fold[-1], test_fold[-1])
      training_fold[-1] = scale(training_fold[-1])
      
      if (permute_or_not){
        training_fold$good_1 = sample(factor(training_fold$good_1, levels = c(0, 1)))
      }
      
      X_train = as.matrix(training_fold[, -1])
      y_train = as.numeric(as.character(training_fold[, 1]))
      
      X_test = as.matrix(test_fold[, -1])
      y_test = as.numeric(as.character(test_fold[, 1]))
      
      if (classifier_type == "svm-rbf") {
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
                    best_gamma = best_params$gamma,
                    y_true = y_test_num,
                    y_pred = y_pred_num))
        
      } else if (classifier_type == "logistic_l2") {
        best_C = tune_glmnet(X_train, y_train, params$C_range, nfold, alpha = 0)
        
        model = glmnet(
          x = X_train,
          y = y_train,
          family = "binomial",
          alpha = 0,  # L2正则化
          lambda = 1/best_C,
          standardize = FALSE
        )
        
        pred_prob = predict(model, newx = X_test, type = "response")
        y_pred = ifelse(pred_prob > 0.5, 1, 0)
        cm = table(y_test, y_pred)
        
        return(list(accuracy = sum(diag(cm))/sum(cm), 
                    best_C = best_C,
                    y_true = y_test,
                    y_pred = y_pred))
        
      } else if (classifier_type == "logistic_l1") {
        best_C = tune_glmnet(X_train, y_train, params$C_range, nfold, alpha = 1)
        
        model = glmnet(
          x = X_train,
          y = y_train,
          family = "binomial",
          alpha = 1,  # L1正则化
          lambda = 1/best_C,
          standardize = FALSE
        )
        
        pred_prob = predict(model, newx = X_test, type = "response")
        y_pred = ifelse(pred_prob > 0.5, 1, 0)
        cm = table(y_test, y_pred)
        
        return(list(accuracy = sum(diag(cm))/sum(cm), 
                    best_C = best_C,
                    y_true = y_test,
                    y_pred = y_pred))
        
      } else if (classifier_type == "linear_svm_l2") {
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
        stop("未知的分类器类型。支持的类型: 'svm-rbf', 'logistic_l2', 'logistic_l1', 'linear_svm_l2', 'linear_svm_l1'")
      }
    })
    
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
                             std_C = sd(best_Cs),
                             mean_gamma = mean(best_gammas),
                             std_gamma = sd(best_gammas)
                           ))
    } else {
      accuracy_all = rbind(accuracy_all, 
                           data.frame(
                             iteration = i,
                             accuracy = mean(accuracies),
                             mean_C = mean(best_Cs),
                             std_C = sd(best_Cs)
                           ))
    }
  }
  
  realMean = mean(accuracy_all$accuracy)
  cat("\n嵌套交叉验证完成！\n")
  cat("平均准确率:", round(realMean, 4), "\n")
  cat("平均最佳C参数:", round(mean(accuracy_all$mean_C), 4), "\n")
  cat("标准差最佳C参数:", round(mean(accuracy_all$std_C), 4), "\n")
  
  if (classifier_type == "svm-rbf") {
    cat("平均最佳γ参数:", round(mean(accuracy_all$mean_gamma), 6), "\n")
    cat("标准差最佳γ参数:", round(mean(accuracy_all$std_gamma), 6), "\n")
  }
  
  ci_method_used <- "None"
  ci_lower <- NA
  ci_upper <- NA
  bootstrap_mean <- NA
  
  if (length(all_y_true) > 0 && length(all_y_pred) > 0) {
    
    bootstrap_result <- acc_95CI(
      y_true_all = all_y_true,
      y_score_all = all_y_pred,
      nBoot = n_bootstrap
    )
    
    ci_lower <- bootstrap_result[1]
    ci_upper <- bootstrap_result[2]
    bootstrap_mean <- mean(all_y_true == all_y_pred)
    ci_method_used <- "Bootstrap"
  } 
  
  cat("\n置信区间 (", ci_level*100, "%):\n", sep="")
  cat("方法:", ci_method_used, "\n")
  cat("下限:", round(ci_lower, 4), "\n")
  cat("均值:", round(realMean, 4), "\n")
  cat("上限:", round(ci_upper, 4), "\n")
  
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
    ),
    prediction_data = list(
      y_true = all_y_true,
      y_pred = all_y_pred
    )
  ))
}

#===============================================================================
## Function16: tune_svm
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
## Function17: tune_glmnet
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
## Function18: tune_liblinear
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
## Function19: tune_sensitive
tune_sensitive <- function(df, params, nfold) {
  tune_grid = expand.grid(C = params$C_range, gamma = params$gamma_range)

  accuracy_matrix <- matrix(0, nrow = length(params$C_range), 
                            ncol = length(params$gamma_range))
  rownames(accuracy_matrix) <- params$C_range
  colnames(accuracy_matrix) <- params$gamma_range

  fold_accuracy_list <- vector("list", nrow(tune_grid))
  
  best_accuracy = 0
  best_params = list(C = 1, gamma = 1/ncol(df[-1]))
  
  folds = svm_createFolds(df, nfold)

  for(j in 1:nrow(tune_grid)) {
    accuracies = numeric(length(folds))
    
    for(k in 1:length(folds)) {
      training = df[-folds[[k]], ]
      test = df[folds[[k]], ]
      
      test[-1] = ind_scale(training[-1], test[-1])
      training[-1] = scale(training[-1])
      
      classifier = svm(
        formula = good_1 ~ .,
        data = training,
        type = 'C-classification',
        kernel = "radial",
        cost = 2**tune_grid$C[j],
        gamma = 2**tune_grid$gamma[j]
      )
      
      pred = predict(classifier, newdata = test[-1])
      cm = table(test[, 1], pred)
      accuracies[k] = sum(diag(cm)) / sum(cm)
    }

    fold_accuracy_list[[j]] <- list(
      C = tune_grid$C[j],
      gamma = tune_grid$gamma[j],
      fold_accuracies = accuracies,
      mean_accuracy = mean(accuracies)
    )
    
    mean_accuracy = mean(accuracies)

    c_index <- which(params$C_range == tune_grid$C[j])
    gamma_index <- which(params$gamma_range == tune_grid$gamma[j])
    accuracy_matrix[c_index, gamma_index] <- mean_accuracy
    
    if(mean_accuracy > best_accuracy) {
      best_accuracy = mean_accuracy
      best_params = list(C = tune_grid$C[j], gamma = tune_grid$gamma[j])
    }
  }

  cat("每种C-γ组合的交叉验证准确率:\n")
  print(round(accuracy_matrix, 4))
  
  return(list(
    best_params = best_params, 
    best_accuracy = best_accuracy,
    accuracy_matrix = accuracy_matrix,
    fold_accuracy_details = fold_accuracy_list
  ))
}

#===============================================================================
## Function20: draw_accuracy_heatmap
draw_accuracy_heatmap <- function(accuracy_matrix, c_range, gamma_range, figname) {

  accuracy_df <- as.data.frame(accuracy_matrix)
  colnames(accuracy_df) <- gamma_range
  accuracy_df$C <- c_range

  accuracy_melted <- melt(accuracy_df, id.vars = "C", 
                          variable.name = "gamma", value.name = "accuracy")
  accuracy_melted$gamma <- as.numeric(as.character(accuracy_melted$gamma))
  
  p <- ggplot(accuracy_melted, aes(x = factor(gamma), y = factor(C), fill = accuracy)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "#4575B4", mid = "#FFFFBF",high = "#D73027", 
                         midpoint = 0.65,
                         limits = c(0.3,1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  print(p)

  ggsave(figname, p, width = 10, height = 8, dpi = 300)
  cat("热图已保存为", figname, "\n")
  
  return(p)
}

#===============================================================================
## Function21: auc_boot_cv
auc_boot_cv <- function(df,nfold,koi,nBoot,plotROC,cond_col,filename){
  
  df[,1] = factor(df[,1], levels = c(0, 1)) 
  df = na.omit(df) 
  auc_data = data.frame(matrix(nrow = nrow(df), ncol = 0));
  folds = svm_createFolds(df,nfold) 
  
  y_true_all <- c()
  y_score_all <- c()
  
  cv <- lapply(folds, function(x) { 
    training_fold <- df[-x, ] 
    test_fold     <- df[x, ] 
    
    test_fold[-1]     <- ind_scale(training_fold[-1], test_fold[-1]) 
    training_fold[-1] <- scale(training_fold[-1]) 
    
    classifier <- svm(
      good_1 ~ .,
      data = training_fold,
      type = "C-classification",
      kernel = koi,
      probability = TRUE
    )
    
    y_pred <- predict(classifier, test_fold[-1], probability = TRUE)
    
    prob_mat <- attr(y_pred, "probabilities")
    score_1  <- prob_mat[, "1"]
    
    return(list(
      y_true  = test_fold[, 1],
      y_score = score_1
    ))
  })
  
  auc_data$y_real <- unlist(lapply(cv, `[[`, "y_true"))
  auc_data$y_fit  <- unlist(lapply(cv, `[[`, "y_score"))
  
  roc_obj <- roc(auc_data$y_real, auc_data$y_fit, quiet = TRUE)
  mean_auc <- as.numeric(auc(roc_obj))
  
  auc_boot <- numeric(nBoot)
  N <- length(auc_data$y_real)
  
  for (b in 1:nBoot) {
    idx <- sample(seq_len(N), size = N, replace = TRUE)
    y_b <- auc_data$y_real[idx]
    s_b <- auc_data$y_fit[idx]
    
    if (length(unique(y_b)) < 2) {
      auc_boot[b] <- NA
    } else {
      auc_boot[b] <- as.numeric(auc(roc(y_b, s_b, quiet = TRUE)))
    }
  }
  
  auc_boot <- auc_boot[!is.na(auc_boot)]
  CI95 <- quantile(auc_boot, c(0.025, 0.975))
  
  if (plotROC) {
    roc_smooth <- smooth(roc_obj, method = "binormal")
    roc_df <- data.frame(
      FPR = 1 - roc_smooth$specificities,
      TPR = roc_smooth$sensitivities
    )
    
    p <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
      geom_line(color = cond_col, linewidth = 1.2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
      coord_equal() +
      theme_classic(base_size = 14) +
      theme(axis.ticks.length=unit(-0.1, "cm"))+
      labs(x = "1 - Specificity",
        y = "Sensitivity")
    print(p)
    
    png(filename, width = 5, height = 4, units = "in", res = 300)  # 300DPI高清图片
    print(p)
    dev.off()
  }
  
  message(sprintf("AUC: %.3f\n95%% CI: [%.3f, %.3f]\n",
                  mean_auc, CI95[1], CI95[2]))
  
  return(list(
    mean_auc = mean_auc,
    CI95 = CI95,
    auc_boot = auc_boot,
    y_true_all = auc_data$y_real,
    y_score_all = auc_data$y_fit
  ))
}

#===============================================================================
## Function22: compare_models_delong
compare_models_delong <- function(y_true, score_list) {
  
  y_true <- factor(y_true, levels = c(0, 1))
  
  model_names <- names(score_list)
  n_models <- length(score_list)
  
  results <- data.frame(
    Model1 = character(),
    Model2 = character(),
    AUC1   = numeric(),
    AUC2   = numeric(),
    Statistic = numeric(),
    P_value   = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(n_models - 1)) {
    for (j in (i + 1):n_models) {
      
      roc1 <- pROC::roc(y_true, score_list[[i]], quiet = TRUE)
      roc2 <- pROC::roc(y_true, score_list[[j]], quiet = TRUE)
      
      test_res <- pROC::roc.test(
        roc1, roc2,
        method = "delong",
        paired = TRUE
      )
      
      results <- rbind(
        results,
        data.frame(
          Model1 = model_names[i],
          Model2 = model_names[j],
          AUC1   = as.numeric(pROC::auc(roc1)),
          AUC2   = as.numeric(pROC::auc(roc2)),
          Statistic = unname(test_res$statistic),
          P_value   = test_res$p.value
        )
      )
    }
  }
  
  return(results)
}

#===============================================================================
## Function23: compare_models_mcnemar
compare_models_mcnemar <- function(df1, df2, df3, nfold = 5) {

  run_cv <- function(df, folds) {
    df[,1] <- factor(df[,1], levels = c(0,1))
    df <- na.omit(df)
    
    cv_res <- lapply(folds, function(x) {
      train_df <- df[-x,]
      test_df  <- df[x,]
      
      # scaling
      test_df[-1]  <- ind_scale(train_df[-1], test_df[-1])
      train_df[-1] <- scale(train_df[-1])
      
      # SVM classification
      clf <- svm(good_1 ~ ., data=train_df, type="C-classification", kernel="radial")
      y_pred <- predict(clf, newdata=test_df[-1])
      
      return(list(y_true = test_df[,1], y_pred = y_pred))
    })
    
    y_true_all <- unlist(lapply(cv_res, function(x) x$y_true))
    y_pred_all <- unlist(lapply(cv_res, function(x) x$y_pred))
    
    return(list(y_true = y_true_all, y_pred = y_pred_all))
  }
  
  df1[,1] <- factor(df1[,1], levels = c(0,1))
  df1 <- na.omit(df1)
  folds <- svm_createFolds(df1, nfold)
  
  cv1 <- run_cv(df1, folds)
  cv2 <- run_cv(df2, folds)
  cv3 <- run_cv(df3, folds)
  
  y_real <- factor(cv1$y_true, levels = c(0,1))  # consistent true labels
  
  preds_list <- list(
    SVM1 = cv1$y_pred,
    SVM2 = cv2$y_pred,
    SVM3 = cv3$y_pred
  )
  
  model_names <- names(preds_list)
  n_models <- length(preds_list)
  
  results <- data.frame(
    Model1 = character(),
    Model2 = character(),
    Statistic = numeric(),
    P_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(n_models - 1)) {
    for (j in (i + 1):n_models) {
      pred1 <- factor(preds_list[[i]], levels=c(0,1))
      pred2 <- factor(preds_list[[j]], levels=c(0,1))
      
      correct1 <- pred1 == y_real
      correct2 <- pred2 == y_real
      
      tbl <- table(correct1, correct2)
      
      test_res <- mcnemar.test(tbl, correct=TRUE)
      
      results <- rbind(
        results,
        data.frame(
          Model1 = model_names[i],
          Model2 = model_names[j],
          Statistic = unname(test_res$statistic),
          P_value = test_res$p.value
        )
      )
    }
  }
  
  return(results)
}

#===============================================================================
## Function24: prepare_anova_data
prepare_anova_data <- function(result) {
  fold_details <- result$fold_accuracy_details
  
  anova_data <- data.frame(
    C = numeric(0),
    gamma = numeric(0),
    accuracy = numeric(0),
    fold = numeric(0)
  )
  
  for (i in 1:length(fold_details)) {
    combo <- fold_details[[i]]
    fold_accuracies <- combo$fold_accuracies
    
    for (j in 1:length(fold_accuracies)) {
      new_row <- data.frame(
        C = combo$C,
        gamma = combo$gamma,
        accuracy = fold_accuracies[j],
        fold = j
      )
      anova_data <- rbind(anova_data, new_row)
    }
  }
  
  anova_data$C_factor <- as.factor(anova_data$C)
  anova_data$gamma_factor <- as.factor(anova_data$gamma)
  
  return(anova_data)
}

#===============================================================================
## Function25: perform_tuning_anova
perform_tuning_anova <- function(anova_data) {
  anova_model <- aov(accuracy ~ C_factor * gamma_factor, data = anova_data)
  
  cat("=== SVM参数对准确率影响的ANOVA分析结果 ===\n\n")
  
  cat("1. 方差分析表:\n")
  anova_summary <- summary(anova_model)
  print(anova_summary)
  
  cat("\n2. 方差齐性检验:\n")
  levene_test <- car::leveneTest(accuracy ~ interaction(C_factor, gamma_factor), 
                                 data = anova_data)
  print(levene_test)
  
  cat("\n3. 残差正态性检验:\n")
  residuals <- residuals(anova_model)
  shapiro_test <- shapiro.test(residuals)
  print(shapiro_test)
  
  cat("\n4. 效应大小分析:\n")
  ss <- summary(anova_model)[[1]]["Sum Sq"]
  total_ss <- sum(ss)
  eta_sq <- ss / total_ss
  
  eta_results <- data.frame(
    Effect = rownames(ss),
    Eta_Squared = round(eta_sq, 4)
  )
  print(eta_results)
  
  return(list(
    anova_model = anova_model,
    anova_summary = anova_summary,
    levene_test = levene_test,
    shapiro_test = shapiro_test,
    eta_squared = eta_results
  ))
}

#===============================================================================
## Function26: svm_general_auc
svm_general_auc <- function(train_df, test_df, nBoot = 1000, plotROC = TRUE, cond_col = "blue",filename = 'general_roc_plot.png') {
  
  train_df[,1] <- factor(train_df[,1], levels = c(0, 1))
  test_df[,1] <- factor(test_df[,1], levels = c(0, 1))
  
  train_df <- na.omit(train_df)
  test_df <- na.omit(test_df)
  
  train_scaled <- train_df
  test_scaled <- test_df
  
  train_scaled[-1] <- scale(train_df[-1])
  
  if(ncol(train_df) > 1 && nrow(train_df) > 0) {
    train_means <- apply(train_df[-1], 2, mean, na.rm = TRUE)
    train_sds <- apply(train_df[-1], 2, sd, na.rm = TRUE)
    
     zero_sd <- which(train_sds == 0)
    if(length(zero_sd) > 0) {
      train_sds[zero_sd] <- 1  # 对于标准差为0的变量，设置为1避免除零
    }
    
    test_scaled[-1] <- scale(test_df[-1], center = train_means, scale = train_sds)
  }
  
  classifier <- svm(
    good_1 ~ .,
    data = train_scaled,
    type = "C-classification",
    kernel = "radial",
    probability = TRUE
  )
  
  y_pred <- predict(classifier, test_scaled[-1], probability = TRUE)
  prob_mat <- attr(y_pred, "probabilities")
  y_score <- prob_mat[, "1"]
  y_true <- test_scaled[, 1]
 
  roc_obj <- roc(y_true, y_score, quiet = TRUE)
  mean_auc <- as.numeric(auc(roc_obj))
  
  auc_boot <- numeric(nBoot)
  N <- length(y_true)
  
  for (b in 1:nBoot) {
    idx <- sample(seq_len(N), size = N, replace = TRUE)
    y_b <- y_true[idx]
    s_b <- y_score[idx]
    
    if (length(unique(y_b)) < 2) {
      auc_boot[b] <- NA
    } else {
      auc_boot[b] <- as.numeric(auc(roc(y_b, s_b, quiet = TRUE)))
    }
  }
  
  auc_boot <- auc_boot[!is.na(auc_boot)]
  CI95 <- quantile(auc_boot, c(0.025, 0.975))
 
  if (plotROC) {
    roc_smooth <- smooth(roc_obj, method = "binormal")
    roc_df <- data.frame(
      FPR = 1 - roc_smooth$specificities,
      TPR = roc_smooth$sensitivities
    )
    
    p <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
      geom_line(color = cond_col, linewidth = 1.2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
      coord_equal() +
      theme_classic(base_size = 14) +
      theme(axis.ticks.length=unit(-0.1, "cm"))+
      labs(x = "1 - Specificity",
           y = "Sensitivity")
    print(p)
    
    png(filename, width = 5, height = 4, units = "in", res = 300)  # 300DPI高清图片
    print(p)
    dev.off()
  }
  
  message(sprintf("AUC: %.3f\n95%% CI: [%.3f, %.3f]\n",
                  mean_auc, CI95[1], CI95[2]))
  
  return(list(
    mean_auc = mean_auc,
    CI95 = CI95,
    auc_boot = auc_boot,
    y_true = y_true,
    y_score = y_score,
    roc_object = roc_obj
  ))
}