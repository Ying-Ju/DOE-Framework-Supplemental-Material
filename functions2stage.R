### In this file, we included all functions we wrote and used in the
### article: A Two-Stage Machine Learning Approach to Predict Heart 
### Transplantation Survival Probabilities over Time with a Monotonic 
### Probability Constraint


## cat_changer() function re-groups values in a categorical variable
## data_set: a data object that contains the varaible that we want to re-group
## var: the variable that we want to re-group
## val_old: some old levels in the variable
## val_new: some new levels in the variable
## val_old and val_new should be ONE-TO-ONE corresponds to each other
## Other levels that are not specified in val_old are put into "OTHER"
## Return value: a data object after re-groupping levels in the given variable
cat_changer <- function(data_set,var,val_old,val_new){
  temp_var <- dplyr::pull(data_set,var)
  cum_index <-c()
  for (i in 1:length(val_old)){
    index <- which(data_set[,var]==val_old[i])
    temp_var[index] <- val_new[i]
    cum_index <- c(cum_index,index)
  }
  na.index <- which(is.na(data_set[,var]))
  temp_var[-sort(c(cum_index, na.index))] <- "OTHER"
  data_set[,var] <- temp_var
  return(data_set)
}


## detect_terms() detects if the given term is in a vector
## x: a vector where we want to detect a given term
## term: a given term we want to detect
## Return Value: "YES" or "NO", "YES" means the given term appears at least once in the vector; "NO" means it doesn't appear in the vector
detect_terms <- function(x,term){
  result <- sapply(x, function(y) any(gsub(" ", "", strsplit(y, ",")[[1]])==term))
  if (all(is.na(result))) return(NA)
  result <- ifelse(is.na(result), FALSE, result)
  if (any(result==TRUE)) return("Y")
  return("N")
}


## col_missing_function() counts number of nas in each column in a given data object
## input_data: a array, including a matrix, dataframe
## Return Values: a data frame object that reconds percentage of missing values in each column in the input data
col_missing_function <- function(input_data){
  # first, we count nas in each column
  na_count_col <- apply(input_data, 2, function(y) length(which(is.na(y)))/nrow(input_data)) 
  # we saved the object into a data_frame
  na_count_col <- data.frame(na_count_col) 
  return(na_count_col)
}


## col_missing_function() counts number of nas in each row in a given data object
## input_data: a array, including a matrix, dataframe
## Return Values: a data frame object that reconds percentage of missing values in each row in the input data

row_missing_function <- function(input_data){
  # first, we count nas in each row
  na_count_row <- apply(input_data, 1, function(y) length(which(is.na(y)))/ncol(input_data)) # counting nas in each row
  # we saved the object into a data_frame
  na_count_row <- data.frame(na_count_row) #transforming this into a data_frame
  return(na_count_row)
}


## dummy_maker() performs One-hot encoding (creates dummy variables) for categorical variables in a given data object
## input_data: a data object
## char_var: a vector of all independent categorical variables in the data
## Return Values: a data object after One-hot encoding is applied
dummy_maker <- function(input_data,char_var){
  for (i in 1:ncol(input_data)){
    if(names(input_data[i]) %in% char_var){
      # Use createDummyvars() function to create dummy variables for each categorical variable
      # The definition of createDummyvars() can be found in this file
      temp <- createDummyvars(input_data[i])
      names(temp) <- paste(names(input_data[i]),levels(as.factor(input_data[,i])),sep="_")
      input_data <- cbind(input_data,temp)
      input_data[,ncol(input_data)]<-NULL}
  }
  # We removed the dependent dummy variable in each categorical variable
  input_data <- input_data[-which(names(input_data) %in% char_var)]
  return(input_data)
}


## createDummyvars() function creates dummy variables for a given categorical variable
## data0: a column (vector, categoric) in the data
## Return Values: a data object after one-hot encoding is applied to the given variable
## we used it in dummy_maker() and dummy_maker_app()
createDummyvars <- function(data0){
  dd<-as.data.frame(table(data0))
  dum_data<-as.data.frame(matrix(0, ncol = nrow(dd), nrow = nrow(data0)))
  names(dum_data) <- dd[,1]
  for(i in 1:ncol(dum_data)){
    dum_data[which(data0==names(dum_data)[i]),i]<-1
    dum_data[i]<-as.factor(dum_data[,i])
  }
  return(dum_data)
}


## class_generator_bino() function creates binary response variable
## gstatus: GRAFT FAILED (1=YES)
## gtime: GRAFT LIFESPAN-Days From Transplant to Failure/Death/Last Follow-Up
## p_unit: time period, the unit is year
## predict_length: the length of days, 365 here if the unit is 1 year 
## Return Value: a vector (our response/dependent variable), 0: death, 1:survival, NA: unknow status
class_generator_bino <- function(gstatus,gtime,p_unit,predict_length){
  p_unit <- as.numeric(p_unit)
  predict_length <- as.numeric(predict_length)
  if(gtime < p_unit*predict_length){
    if(is.na(gstatus)){return(NA)}else{
      if(gstatus==0){return(NA)}
      if(gstatus==1){return(0)}  # death
    }
  }else{
    return(1)  # survival
  }
}


# check_range: check if numerical values are in the corresponding range (this is for shinny app)
check_range <- function(df, vars, Num_Range){
  out_range <- rep(NA, 13)
  for (i in 1:length(vars)){
    m <- min(df[,vars[i]])
    M <- max(df[,vars[i]])
    out_range[i] <- ifelse((m<Num_Range[i,1]|M>Num_Range[i,2]), 1, 0)
  }
  return(out_range)
}
#________________________________________________________________________

# check_levels: check if categorical values are in our data (this is for shinny app)
check_levels <- function(df, vars, Cat_Levels){
  CAT_match <- rep(NA, length(vars))
  for (i in 1:length(vars)){
    temp_level <- levels(as.factor(df[,vars[i]]))
    CAT_match[i] <- ifelse(all(temp_level%in%Cat_Levels[[i]]), 0, 1)
  }
  return(CAT_match)
}


#________________________________________________________________________

# creating dummy variables for input data (this is for shinny app)
dummy_maker_app<-function(input_data, char_var){
  for (i in 1:ncol(input_data)){
    if(names(input_data[i]) %in% char_var){
      temp<-createDummyvars(input_data[i])
      names(temp)<-paste(names(input_data[i]),levels(as.factor(input_data[,i])),sep="_")
      
      input_data<-cbind(input_data,temp)
      if (length(levels(as.factor(input_data[,i])))!=1){
        input_data[,ncol(input_data)]<-NULL
      }else{
        input_data[,ncol(input_data)]<-factor(input_data[,ncol(input_data)], levels = c(0,1))
      }
    }
  }
  input_data<-input_data[-which(names(input_data) %in% char_var)]
  return(input_data)
}

#________________________________________________________________________

# change all "U" to Unknown (this is for shinny app)
find_U <- function(x){
  col_index <- which(x=="U")
  if (length(col_index)!=0) x[col_index] <- "UNKOWN"
  return(x)
}

# finding mode for categorical variables

cahrmode <- function(x) {
  x<-x[complete.cases(x)]
  uniq_val <- unique(x)
  return(uniq_val[which.max(tabulate(match(x, uniq_val)))])
}

#==================================================================


# function for cleaning tables
# Try_data<-heart.df.cleaned
# kept_opt<-c("FUNC_STAT_TRR","FUNC_STAT_TCR")
# ID<-"ID"
# kept_col<-c("year1")
# col2row_emp<-1
table_cleaner<-function(Try_data,col2row_emp,ID,kept_col,kept_opt){
  
  data0<-Try_data
  Try_data<-data0[complete.cases(data0[kept_col]),]
  
  max_col<-0
  max_row<-0
  Try_data_temp <<- Try_data
  
  for(i in 1:100000){
    Try_data<-Try_data_temp
    gc()
    count_col <- col_missing_function(Try_data)
    gc()
    count_row <- row_missing_function(Try_data)
    max_col<-max(count_col)
    max_row<-max(count_row)
    max_emp<- max(max_col,max_row)
    
    if(max_col==0){break()}
    if(max_row==0){break()}
    
    if (max_emp==max_row)
    {
      if((nrow(Try_data))>3){
        a<-which(count_row$na_count_row==max_row)
        
        Try_data_temp <<- Try_data[-a,]
        print(c("nrow",nrow(Try_data_temp)))
      }
    }
    if(max_col>=max_row*col2row_emp){
      b<-names(Try_data[which(count_col$na_count_col ==max_col)])
      
      if (max_emp==max_col){
        if((ncol(Try_data))>2){
          #in the next lines, I try to find which columns are the emptiest and then take out the one that I don't want to be deleted
          
          f<-which(count_col$na_count_col==max_col)
          print(paste("iteration: ",i," // dropped column name: ",names(Try_data[f]),sep=""))
          Try_data_temp<<- Try_data[, -f]
        }
        
      }
      
      print(c("ncol",ncol(Try_data_temp)))
    }else{
      if((nrow(Try_data))>3){
        a<-which(count_row$na_count_row==max_row)
        
        Try_data_temp <<- Try_data[-a,]
        
      }
      print(c("nrow",nrow(Try_data_temp)))
    }
    
  }
  
  
  input_object<-list()
  
  print("Data Cleaning is Done! Exit the tool. Run the tool again, load the cleaned file for Data Analysis")
  out_object<-list()
  out_object$col_names<-names(Try_data_temp)
  out_object$row_ID<-Try_data_temp[ID]
  
  if(sum(names(data0) %in% kept_opt)>0){
    temp<-data0[out_object$row_ID[,],which(names(data0) %in% kept_opt)]
    if(sum(names(data0) %in% kept_opt)==1){
      temp<-as.data.frame(temp)
      names(temp)<-names(data0[kept_opt])}
    
    data_inc<-cbind(Try_data_temp,temp)
    data_inc<-data_inc[complete.cases(data_inc[kept_opt]),]
    out_object$col_names_inc<-names(data_inc)
    out_object$row_ID_inc<-data_inc[ID]
  }else{
    out_object$col_names_inc<-out_object$col_names
    out_object$row_ID_inc<-out_object$row_ID
  }
  out_object$data<-Try_data_temp
  return(out_object)
}
#==================================================================

## encode_cat() retuns the data after the assigned encoding method for
## categorical variables
## df: data; method = c("numeric", "factor")
encode_cat <- function(df, cat_vars, method){
  cat_index <- which(colnames(df)%in%cat_vars)
  if (method=="numeric"){
    df[,cat_index] <- apply(df[,cat_index], 2, function(x) as.numeric(as.factor(x)))
  }else if (method=="factor"){
    variables <- colnames(df)[cat_index]
    df <- dummy_maker(df, variables)
  }
  return(df)
}


## testdata_index() creates IDs for test data
## n: number of rows from the entire dataset
## seed: random seed used in the function, the default is 2019
holdout_index <- function(IDs, seed=2019){
  n<-length(IDs)
  set.seed <- seed
  index <- list()
  index0 <- c()
  
  for (i in 1:5){
    index[[i]] <- sample(setdiff(IDs, index0), floor(n/5))
    index0 <- c(index0, index[[i]])
  }
  return(index)
} 

## select_vars() does the variable selection for the data in one timestamp and the for scenarios of DoE
## three variable selection methods are available: FFS, LASSO, Random Forest
## ii: the index to find the iith training data (we have 5 disjoint holdout datasets)
## df: the whole dataset
## y: name of the response variable
## method: FFS or LASSO or RF
## ID: the ID for the test data set 
## seed: random seed used in the function, the default is 201905

# the following block of code is a versatile function that does feature selection, 
# or running over Ohio supercomputer, there were some limitations for installing some packages
# then instead some source codes of the packages are after select_vars()
# beginning of the code block
{
select_vars <- function(ii, df, y, method, ID, seed=201905){
  set.seed <- seed
  df <- df[!df$ID%in%ID[[ii]],]
  df <- df[,colnames(df)!="ID"]
  X <- df[,colnames(df)!=y]
  if (method=="FFS"){
    X$y <- df[,y]
    disc <- "MDL"
    threshold <- 0.001
    attrs.nominal <- numeric()
    result_temp <- Biocomb::select.fast.filter(X, disc.method=disc, threshold=threshold, attrs.nominal=attrs.nominal)
    Imp_vars <- as.character(result_temp$Biomarker)
  }else if (method=="LASSO"){
    '%ni%' <- Negate('%in%')
    X <- data.matrix(apply(X, 2, as.numeric))
    glmnet1 <- glmnet::cv.glmnet(x=X,y=as.factor(df[,y]),type.measure='auc', family="binomial")
    co <- coef(glmnet1,s = "lambda.1se")
    co <- as.matrix(co[-1,])
    Imp_vars <- row.names(co)[which((co[,1]!=0))]
    Imp_vars <- Imp_vars[Imp_vars %ni% '(Intercept)']
  }else if (method=="RF"){
    library(party)
    library(varImp)
    # formul<- as.formula(paste0(y,"~."))
    # cf1 <- Boruta::Boruta(formul, data = df, doTrace = 2)
    # find_vars <- cf1$finalDecision[which(cf1$finalDecision!="Rejected")]
    # Imp_vars <- names(find_vars)
    # for parallel processing I have to load the functions
    source("https://raw.githubusercontent.com/transplantation/DoE/Hamid/DoE_paper_functions.R")
    y<-as.factor(df[[y]])
    Imp_vars <- var.sel.r2vim(X, y, no.runs = 1,ntree = 500, random_seed = seed)
    Imp_vars<-Imp_vars$var
  }
  return(list(Imp_vars))
}


#' Variable selection using recurrent relative variable importance (r2VIM).
#'
#' Generates several random forests using all variables and different random
#' number seeds. For each run, the importance score is divided by the (absolute)
#' minimal importance score (relative importance scores). Variables are selected
#' if the minimal relative importance score is >= factor.
#'
#' Note: This function is a reimplementation of the R package \code{RFVarSelGWAS}.
#'
#' @inheritParams wrapper.rf
#' @param no.runs number of random forests to be generated
#' @param factor minimal relative importance score for a variable to be selected
#'
#' @return List with the following components:
#'   \itemize{
#'   \item \code{info} data.frame
#'   with information for each variable
#'   \itemize{
#'   \item vim.run.x = original variable importance (VIM) in run x
#'   \item rel.vim.run.x = relative VIM in run x
#'   \item rel.vim.min = minimal relative VIM over all runs
#'   \item rel.vim.med = median relative VIM over all runs
#'   \item selected = variable has been selected
#'   }
#'   \item \code{var} vector of selected variables
#'   }
#'
#'  @examples
#' # simulate toy data set
#' data = simulation.data.cor(no.samples = 100, group.size = rep(10, 6), no.var.total = 200)
#'
#' # select variables
#' res = var.sel.r2vim(x = data[, -1], y = data[, 1], no.runs = 5, factor = 1)
#' res$var
#'
#' @export

# we used it inside of select_vars()
var.sel.r2vim <- function(x, y, no.runs = 1, factor = 1, ntree = 500, 
                          mtry.prop = 0.2, nodesize.prop = 0.1,
                          no.threads = 1, method = "ranger", 
                          type = "classification",random_seed =seed) {
  set.seed(random_seed)
  
  ## importance for each run
  imp.all = NULL
  for (r in 1:no.runs) {
    print(paste("run", r))
    rf = wrapper.rf(x = x, y = y,
                    ntree = ntree, mtry.prop = mtry.prop, nodesize.prop = nodesize.prop, no.threads = no.threads,
                    method = method, type = type)
    imp.all = cbind(imp.all, get.vim(rf))
  }
  
  ## factors
  min.global = min(imp.all)
  if (min.global >= 0) {
    stop("Global minimal importance score is not negative!")
  }
  no.neg.min = 0
  fac = matrix(nrow = nrow(imp.all), ncol = ncol(imp.all),
               dimnames = dimnames(imp.all))
  for (i in 1:ncol(imp.all)) {
    x = imp.all[,i]
    min = min(x)
    if (min >= 0) {
      no.neg.min = no.neg.min + 1
      fac[,i] = x / abs(min.global)
    } else {
      fac[, i] = x / abs(min)
    }
  }
  if (no.neg.min > 0) {
    print(paste(no.neg.min, "runs with no negative importance score!"))
  }
  fac.min = apply(fac, 1, min)
  fac.med = apply(fac, 1, median)
  
  ## select variables
  ind.sel = as.numeric(fac.min >= factor)
  
  ## info about variables
  info = data.frame(imp.all, fac, fac.min, fac.med, ind.sel)
  colnames(info) = c(paste("vim.run.", 1:no.runs, sep = ""),
                     paste("rel.vim.run.", 1:no.runs, sep = ""),
                     "rel.vim.min", "rel.vim.median", "selected")
  return(list(info = info, var = sort(rownames(info)[info$selected == 1])))
}

#' Wrapper function to call random forests function.
#'
#' Provides an interface to different parallel implementations of the random
#' forest algorithm. Currently, only the \code{ranger} package is
#' supported.
#'
#' @param x matrix or data.frame of predictor variables with variables in
#'   columns and samples in rows (Note: missing values are not allowed).
#' @param y vector with values of phenotype variable (Note: will be converted to factor if
#'   classification mode is used).
#' @param ntree number of trees.
#' @param mtry.prop proportion of variables that should be used at each split.
#' @param nodesize.prop proportion of minimal number of samples in terminal
#'   nodes.
#' @param no.threads number of threads used for parallel execution.
#' @param method implementation to be used ("ranger").
#' @param type mode of prediction ("regression", "classification" or "probability").
#' @param ... further arguments needed for \code{\link[relVarId]{holdout.rf}} function only.
#'
#' @return An object of class \code{\link[ranger]{ranger}}.
#'
#' @import methods stats
#'
#' @export
#'
#' @examples
#' # simulate toy data set
#' data = simulation.data.cor(no.samples = 100, group.size = rep(10, 6), no.var.total = 200)
#'
#' # regression
#' wrapper.rf(x = data[, -1], y = data[, 1],
#'            type = "regression", method = "ranger")

# we used it inside of var.sel.r2vim()
wrapper.rf <- function(x, y, ntree = 100, mtry.prop = 0.2, nodesize.prop = 0.1, no.threads = 1,
                       method = "ranger", type = "regression", ...) {
  
  ## check data
  if (length(y) != nrow(x)) {
    stop("length of y and number of rows in x are different")
  }
  
  if (any(is.na(x))) {
    stop("missing values are not allowed")
  }
  
  if (type %in% c("probability", "regression") & (is.character(y) | is.factor(y))) {
    stop("only numeric y allowed for probability or regression mode")
  }
  
  ## set global parameters
  nodesize = floor(nodesize.prop * nrow(x))
  mtry = floor(mtry.prop * ncol(x))
  if (mtry == 0) mtry = 1
  
  if (type == "classification") {
    #    print("in classification")
    y = as.factor(y)
  }
  
  ## run RF
  if (method == "ranger") {
    if (type == "probability") {
      y = as.factor(y)
      prob = TRUE
    } else {
      prob = FALSE
    }
    
    rf = ranger::ranger(data = data.frame(y, x),
                        dependent.variable.name = "y",
                        probability = prob,
                        importance = "permutation", scale.permutation.importance = FALSE,
                        num.trees = ntree,
                        mtry = mtry,
                        min.node.size = nodesize,
                        num.threads = no.threads,
                        write.forest = TRUE,
                        ...)
  } else {
    stop(paste("method", method, "undefined. Use 'ranger'."))
  }
  
  return(rf)
}


#' Error calculation.
#'
#' Calculates errors by comparing predictions with the true values. For
#' regression and probability mode, it will give root mean squared error (rmse) and
#' pseudo R-squared (rsq). For classification mode, overall accuracy (acc), overall
#' error (err), Matthews correlation coefficient (mcc), sensitivity (sens) and
#' specificity (spec) are returned.
#'
#' @param rf Object of class \code{\link[ranger]{ranger}}
#' @param true vector with true value for each sample
#' @param test.set matrix or data.frame of predictor variables for test set with variables in
#'   columns and samples in rows (Note: missing values are not allowed)
#' @inheritParams wrapper.rf
#'
#' @return numeric vector with two elements for regression and probability estimation (rmse, rsq) and
#' five elements for classification (acc, err, mcc, sens, spec)
#'
#' @export
#'
#' @examples
#' # simulate toy data set
#' data = simulation.data.cor(no.samples = 100, group.size = rep(10, 6), no.var.total = 200)
#'
#' # random forest
#' rf = wrapper.rf(x = data[, -1], y = data[, 1],
#'                 type = "regression")
#'
#' # error
#' calculate.error(rf = rf, true = data[, 1])

# a source code file for feature selection
calculate.error <- function(rf, true, test.set = NULL) {
  
  if (is(rf, "ranger")) {
    if (!is.null(test.set)) {
      pred = predict(rf, data = test.set)$predictions
    } else {
      pred = rf$predictions
    }
    if (rf$treetype == "Probability estimation") {
      pred = pred[, 2]
    }
  } else {
    stop(paste("rf needs to be of class ranger"))
  }
  
  if ((is(rf, "randomForest") && rf$type == "classification") |
      (is(rf, "ranger") && rf$treetype == "Classification")) {
    conf.matrix = table(pred = pred, true = true)
    tp = conf.matrix[2, 2]
    tn = conf.matrix[1, 1]
    fn = conf.matrix[2, 1]
    fp = conf.matrix[1, 2]
    
    ## accuracy
    acc = (tp + tn) / sum(conf.matrix)
    
    ## Matthews correlation coefficient
    mcc = (tp * tn - fp * fn) /
      sqrt( (tp + fn) * (tn + fp) * (tp + fp) * (tn + fn))
    
    ## sensitivity
    sens = tp / (tp + fn)
    
    ## specificity
    spec = tn / (fp + tn)
    
    error = c(err = 1 - acc, acc = acc, mcc = mcc, sens = sens, spec = spec)
  } else {
    mse = sum((pred - true)^2, na.rm = TRUE) / sum(!is.na(pred))
    
    ## pseudo R-squared uses sum of squared differences divided by n instead of variance!
    v = sum((true - mean(true))^2) / length(true)
    rsq = 1 - mse/v
    error = c(rmse = sqrt(mse), rsq = rsq)
  }
  
  return(error)
}


#' Get variable importance.
#'
#' Extracts variable importance depending on class of random forest object.
#'
#' @param rf Object of class \code{\link[ranger]{ranger}}
#'
#' @return numeric vector with importance value for each variable (in original order)
#'
#' @export

# we used it inside of var.sel.r2vim()
get.vim <- function(rf) {
  if (is(rf, "ranger")) {
    vim = ranger::importance(rf)
  } else {
    stop(paste("rf needs to be of class ranger"))
  }
  return(vim)
}

}
# end of the code block


# parralel processing for experiments in DoE
# design_table0 the information of factors in factorial design
# All_data0: data of transplantations
# idenx_holdout0: index of the holdout set
# features0: features that attained after feature selection
# seed0: random seed number

train_para<-function(iii, design_table0 ,All_data0,TARGET0="year1", 
                     Index_test0=idenx_holdout, features0,seed0=2019){
  
  gc()
  if(!"pacman" %in% rownames(installed.packages())){
    install.packages(pkgs = "pacman",repos = "http://cran.us.r-project.org")
  }
  # p_load is equivalent to combining both install.packages() and library()
  pacman::p_load(caret,AUC,MASS,ROSE,DMwR,snow,ranger,parallel,xgboost,gbm,naivebayes,e1071,kernlab,parallel)
  
  set.seed(seed0)
  fold_no<-design_table0[iii,"fold"]
  alg_fact<-design_table0[iii,"algorithm"]
  resample_fact<-design_table0[iii,"resampling"]
  feature_fact<-design_table0[iii,"features"]
  impute_num_fact<-design_table0[iii,"numerical_imputation"]
  impute_cat_fact<-design_table0[iii,"categorical_imputation"]
  encoding_fact<-design_table0[iii,"encoding"]
  data_fact<-as.numeric(design_table0[iii,"ID"])
  
  # putting summary of the experiment in here
  experiment.summary<-as.data.frame(matrix(0, ncol = 9, nrow = 1))
  names(experiment.summary)<-c("data_scenario","algorithm","resampling","feature_selection",
                               "imputation_num","imputation_cat","encoding","fold","TARGET")
  
  experiment.summary[1,]<-c(data_fact,alg_fact,resample_fact,feature_fact,
                            impute_num_fact,impute_cat_fact,encoding_fact,fold_no,TARGET0)
  
  
  df <- All_data0[[data_fact]]
  index_t<- which(df$ID%in%Index_test0[[fold_no]])
  vars<-features0[feature_fact][[1]][[data_fact]][[fold_no]]
  df <- df[,which(names(df)%in% c(vars,as.character(TARGET0[1])) )]
  hold_out <- df[index_t,]
  traindata <- df[-index_t,]
  traindata$ID <- NULL
  
  set.seed(seed0+fold_no)
  
  # 1: survival; 0: death
  traindata[,as.character(TARGET0)] <- as.factor(ifelse(traindata[,as.character(TARGET0)]=="0", "Death", "Survival"))
  hold_out[,as.character(TARGET0)] <- as.factor(ifelse(hold_out[,as.character(TARGET0)]=="0", "Death", "Survival"))
  
  
  formul<-as.formula(paste0(as.character(TARGET0[1]),"~."))
  
  run.code<-"YES"
  sampling.method<-resample_fact
  if(resample_fact=="none"){sampling.method<-NULL}
  # Reference for geometric mean
  # Kim, Myoung-Jong, Dae-Ki Kang, and Hong Bae Kim. "Geometric mean based boosting 
  # algorithm with over-sampling to resolve data imbalance problem for bankruptcy prediction." 
  # Expert Systems with Applications 42.3 (2015): 1074-1082.
  
  gmeanfunction <- function(data, lev = NULL, model = NULL) {
    sub_sens<-caret::sensitivity(data$pred,data$obs)
    sub_spec<-caret::specificity(data$pred,data$obs)
    c(gmean = sqrt(sub_sens*sub_spec))
  }
  
  
  
  # I used 5 fold cross validation 
  control_setting <- caret::trainControl(method = "cv", number=5, sampling=sampling.method , verboseIter= TRUE,
                                         #summaryFunction = twoClassSummary, 
                                         search="random", classProbs = TRUE, selectionFunction="best"
                                         ,summaryFunction = gmeanfunction)
  #models:
  # KPLS: kernelpls
  # CART: rpart
  # XGB: xgbDART
  # random forest (RF): ranger
  # logistic regression (LR): glm
  
  
  if (alg_fact%in% c("glm", "nnet", "svmRadial","kernelpls")){
    if((class(try( result_model <- train(formul, data=traindata, method=alg_fact, family="binomial",
                                         trControl = control_setting, metric="gmean"),silent = TRUE))=="try-error")[1]){run.code<-"NO"}
  }else if (alg_fact%in%c("rf", "gbm", "earth", "rpart", "xgbTree", "naive_bayes","xgbDART" ,"ranger","glmnet")){
    #if(alg_fact=="earth"){alg_fact<-"glmnet"}
    if((class(try( result_model <- train(formul,  data=traindata, method=alg_fact,
                                         trControl = control_setting, tuneLength=10, metric="gmean"),silent = TRUE))=="try-error")[1]){run.code<-"NO"}
  }else if(alg_fact=="lda"){
    if((class(try(  result_model <- train(formul, data=traindata, method=alg_fact, preProcess="pca", preProcOptions = list(method="BoxCox"),
                                          trControl = control_setting, metric="gmean"),silent = TRUE))=="try-error")[1]){run.code<-"NO"}
  }else if(alg_fact=="treebag"){
    if((class(try(  result_model <- train(formul, data=traindata, method=alg_fact, family="binomial",
                                          trControl = control_setting, tuneLength=10, metric="gmean"),silent = TRUE))=="try-error")[1]){run.code<-"NO"}
  }
  
  if(run.code=="YES"){
    resul_raw <- as.data.frame(matrix(NA, ncol = 3, nrow = nrow(hold_out)))
    colnames(resul_raw) <- c("TARGET", alg_fact, "Probability")
    resul_raw$TARGET <- hold_out[as.character(TARGET0)]
    
    train_raw <- as.data.frame(matrix(NA, ncol = 3, nrow = nrow(traindata)))
    colnames(train_raw) <- c("TARGET", alg_fact, "Probability")
    train_raw$TARGET <- traindata[as.character(TARGET0)]
    
    resul_pred_perf<-as.data.frame(matrix(NA, ncol = 1, nrow = 4))
    colnames(resul_pred_perf)<-c(alg_fact)
    rownames(resul_pred_perf)<-c("auc","sen","spec","accu")
    
    #if(class(try(varImp(result_model),silent = TRUE))!="try-error"){
    train_raw$Probability <- predict(result_model, newdata=traindata, type="prob")[,2]
    train_raw[alg_fact] <- predict(result_model, newdata=traindata, type="raw")
    #train_auc <- AUC::auc(roc(train_raw$Probability, traindata[,TARGET]))
    resul_raw[alg_fact] <- predict(result_model, newdata=hold_out, type="raw")
    resul_raw$Probability <- predict(result_model, newdata=hold_out, type="prob")[,2]
    resul_pred_perf[1,1] <- AUC::auc(roc(resul_raw$Probability,hold_out[,as.character(TARGET0)]))
    resul_pred_perf[2,1] <- caret::sensitivity(resul_raw[,alg_fact],hold_out[,as.character(TARGET0)])
    resul_pred_perf[3,1] <- caret::specificity(resul_raw[,alg_fact],hold_out[,as.character(TARGET0)])
    resul_pred_perf[4,1] <- (as.data.frame(confusionMatrix(resul_raw[,alg_fact], hold_out[,as.character(TARGET0[1])])$overall))[1,]
    #}
    gmean_overal<-as.data.frame(result_model$results)
    gmean_folds<-as.data.frame(result_model$resample)
    equat<-as.data.frame(result_model$finalModel$coefficients)
    names(equat)<-"coef"
    equat$vars<-rownames(equat)
    equat$vars[which(equat$vars==c("(Intercept)"))]<-c("INTERCEPT")
    
    dir.create(paste0(getwd(),"/data_pile"))
    rm(list=setdiff(ls(), c("experiment.summary","resul_pred_perf","resul_raw","alg_fact","iii","gmean_overal","gmean_folds","equat")))
    save.image(paste0(getwd(),"/data_pile/",alg_fact,"-",iii,".RData"))
    saveRDS(list(experiment.summary=experiment.summary, Performance=resul_pred_perf, Predicted=resul_raw,gmean_overal=gmean_overal,
                 gmean_folds=gmean_folds,equat=equat),
            paste0(getwd(),"/data_pile/",alg_fact,"-",iii,".rds"))
    
    return(list(experiment.summary=experiment.summary, Performance=resul_pred_perf, Predicted=resul_raw))
  }else{
    dir.create(paste0(getwd(),"/data_pile"))
    rm(list=setdiff(ls(), c("experiment.summary","alg_fact","iii")))
    save.image(paste0(getwd(),"/data_pile/NOT-",alg_fact,"-",iii,".RData"))
    saveRDS(list(experiment.summary=experiment.summary, Performance="NOT", Predicted="NOT",gmean_overal="NOT",
                 gmean_folds="NOT",equat="NOT"),
            paste0(getwd(),"/data_pile/NOT-",alg_fact,"-",iii,".rds"))
    return(list(experiment.summary=experiment.summary, Performance="NOT", Predicted="NOT",gmean_overal="NOT",
                gmean_folds="NOT",equat="NOT"))
  }
}

#==================================================================

# parralel processing for survival prediction
# t_data: training data
# h_data: holdout data
# folds: number of folds
# resampl_meth: the resampling method
# alg_used: the algorithm used for training
# features0: features that attained after feature selection
# seed0: random seed number

train_para2<-function(iii, t_data,h_data,features,folds=5,resampl_meth="up",alg_used,seed0=2019){
  
  gc()
  if(!"pacman" %in% rownames(installed.packages())){
    install.packages(pkgs = "pacman",repos = "http://cran.us.r-project.org")
  }
  # p_load is equivalent to combining both install.packages() and library()
  pacman::p_load(caret,AUC,MASS,ROSE,DMwR,snow,ranger,parallel,xgboost,gbm,naivebayes,e1071,kernlab,pls,parallel)
  
  set.seed(seed0)
  fold_no<-folds
  alg_fact<-alg_used
  resample_fact<-resampl_meth
  coef<-"NO"
  var_imp<-"NO"
  
  
  df <- t_data[[iii]]
  hold_out_ <- h_data[[iii]]
  traindata <- df[c(as.character(features[[paste0("year",iii-1)]][,]),paste0("year",iii-1) )]
  hold_out<-hold_out_[c(as.character(features[[paste0("year",iii-1)]][,]),paste0("year",iii-1),"ID" )]
  TARGET0<-paste0("year",iii-1)
  
  # 1: survival; 0: death
  traindata[,as.character(TARGET0)] <- as.factor(ifelse(traindata[,as.character(TARGET0)]=="0", "Death", "Survival"))
  hold_out[,as.character(TARGET0)] <- as.factor(ifelse(hold_out[,as.character(TARGET0)]=="0", "Death", "Survival"))
  
  
  formul<-as.formula(paste0(as.character(TARGET0),"~."))
  
  run.code<-"YES"
  sampling.method<-resample_fact
  if(resample_fact=="none"){sampling.method<-NULL}
  # Reference for geometric mean
  # Kim, Myoung-Jong, Dae-Ki Kang, and Hong Bae Kim. "Geometric mean based boosting 
  # algorithm with over-sampling to resolve data imbalance problem for bankruptcy prediction." 
  # Expert Systems with Applications 42.3 (2015): 1074-1082.
  
  gmeanfunction <- function(data, lev = NULL, model = NULL) {
    sub_sens<-caret::sensitivity(data$pred,data$obs)
    sub_spec<-caret::specificity(data$pred,data$obs)
    c(gmean = sqrt(sub_sens*sub_spec))
  }
  
  
  
  # I used 5 fold cross validation 
  control_setting <- caret::trainControl(method = "cv", number=5, sampling=sampling.method , 
                                         #summaryFunction = twoClassSummary, 
                                         # the next one is for saving the predictions
                                         savePredictions = TRUE,
                                         search="random", classProbs = TRUE, selectionFunction="best"
                                         ,summaryFunction = gmeanfunction)
  
  
  if (alg_fact%in% c("glm", "nnet", "svmRadial")){
    
    if((class(try( result_model <- train(formul, data=traindata, method=alg_fact, family="binomial",
                                         trControl = control_setting, metric="gmean"),silent = TRUE))=="try-error")[1]){run.code<-"NO"}
    
    
    
  }else if (alg_fact%in%c("rf", "gbm", "earth", "rpart", "xgbTree", "naive_bayes","xgbDART" ,"ranger","glmnet")){
    
    if((class(try( result_model <- train(formul,  data=traindata, method=alg_fact,
                                         trControl = control_setting, tuneLength=10, metric="gmean"),silent = TRUE))=="try-error")[1]){ run.code<-"NO"}
    
    
  }else if(alg_fact=="lda"){
    if((class(try(  result_model <- train(formul, data=traindata, method=alg_fact, preProcess="pca", preProcOptions = list(method="BoxCox"),
                                          trControl = control_setting, metric="gmean"),silent = TRUE))=="try-error")[1]){run.code<-"NO"}
    
    
  }else if(alg_fact=="treebag"){
    if((class(try(  result_model <- train(formul, data=traindata, method=alg_fact, family="binomial",
                                          trControl = control_setting, tuneLength=10, metric="gmean"),silent = TRUE))=="try-error")[1]){run.code<-"NO"}  
    
  }
  
  if(run.code=="YES"){
    
    coef<-as.data.frame(summary(result_model)$coefficients)
    coef$vars<-rownames(coef)
    coef$vars<-as.character(sapply(coef$vars,function(x) gsub("1","",x)))
    coef$vars[which(coef$vars=="(Intercept)")]<-"INTERCEPT"
    
    coef$vars[match("DAYS_STAT1",rownames(coef))]<-"DAYS_STAT1"
    coef$vars[match("DAYS_STAT1A",rownames(coef))]<-"DAYS_STAT1A"
    coef$vars[match("DAYS_STAT1B",rownames(coef))]<-"DAYS_STAT1B"
    
    if(alg_fact=="glm"){
      var_imp<-as.data.frame(caret::varImp(result_model)$importance)
      var_imp$vars<-rownames(var_imp)
      var_imp$vars<-as.character(sapply(var_imp$vars,function(x) gsub("1","",x)))  
      var_imp$vars[match("DAYS_STAT1",rownames(var_imp))]<-"DAYS_STAT1"
      var_imp$vars[match("DAYS_STAT1A",rownames(var_imp))]<-"DAYS_STAT1A"
      var_imp$vars[match("DAYS_STAT1B",rownames(var_imp))]<-"DAYS_STAT1B"
    }
    
    
    resul_raw <- as.data.frame(matrix(NA, ncol = 4, nrow = nrow(hold_out)))
    colnames(resul_raw) <- c("TARGET_raw", alg_fact, "Probability","ID")
    resul_raw$TARGET_raw <- hold_out[as.character(TARGET0)]
    resul_raw$ID<-hold_out$ID
    
    train_raw <- as.data.frame(matrix(NA, ncol = 3, nrow = nrow(traindata)))
    colnames(train_raw) <- c("TARGET", alg_fact, "Probability")
    train_raw$TARGET <- traindata[as.character(TARGET0)]
    
    resul_pred_perf<-as.data.frame(matrix(NA, ncol = 1, nrow = 5))
    colnames(resul_pred_perf)<-c(alg_fact)
    rownames(resul_pred_perf)<-c("auc","sen","spec","accu","GMEAN")
    
    #if(class(try(varImp(result_model),silent = TRUE))!="try-error"){
    train_raw$Probability <- predict(result_model, newdata=traindata, type="prob")[,2]
    train_raw[alg_fact] <- predict(result_model, newdata=traindata, type="raw")
    #train_auc <- AUC::auc(roc(train_raw$Probability, traindata[,TARGET]))
    resul_raw[alg_fact] <- predict(result_model, newdata=hold_out, type="raw")
    resul_raw$Probability <- predict(result_model, newdata=hold_out, type="prob")[,2]
    resul_pred_perf[1,1] <- AUC::auc(roc(resul_raw$Probability,hold_out[,as.character(TARGET0)]))
    resul_pred_perf[2,1] <- caret::sensitivity(resul_raw[,alg_fact],hold_out[,as.character(TARGET0)])
    resul_pred_perf[3,1] <- caret::specificity(resul_raw[,alg_fact],hold_out[,as.character(TARGET0)])
    resul_pred_perf[4,1] <- (as.data.frame(confusionMatrix(resul_raw[,alg_fact], hold_out[,as.character(TARGET0[1])])$overall))[1,]
    resul_pred_perf[5,1] <- sqrt(resul_pred_perf[2,1]*resul_pred_perf[3,1])
    #}
    gmean_overal<-as.data.frame(result_model$results)
    gmean_folds<-as.data.frame(result_model$resample)
    equat<-as.data.frame(result_model$finalModel$coefficients)
    names(equat)<-"coef"
    equat$vars<-rownames(equat)
    equat$vars[which(equat$vars==c("(Intercept)"))]<-c("INTERCEPT")
    
    max_gmean<-max(result_model$resample$gmean)
    fold_pick<-result_model$resample[which(result_model$resample$gmean==max_gmean),c("Resample")]
    foldbest<-result_model$pred[which(result_model$pred$Resample==fold_pick),c("obs","pred","Survival")]
    names(foldbest)<-c(paste0("year",iii-1),"log","Probability")
    
    
    dir.create(paste0(getwd(),"/data_pile"))
    rm(list=setdiff(ls(), c("resul_pred_perf","resul_raw","alg_fact","iii","coef","var_imp",
                            "gmean_overal","gmean_folds","equat","foldbest")))
    
    saveRDS(list(Performance=resul_pred_perf, Predicted=resul_raw,coef=coef,var_imp=var_imp,gmean_overal=gmean_overal,gmean_folds=gmean_folds,
                 equat=equat,foldbest=foldbest ),
            paste0(getwd(),"/data_pile/",alg_fact,"-year-",iii-1,".rds"))
    
    return(list(Performance=resul_pred_perf, Predicted=resul_raw,coef=coef,var_imp=var_imp,gmean_overal=gmean_overal,gmean_folds=gmean_folds,
                equat=equat,foldbest=foldbest))
  }else{
    dir.create(paste0(getwd(),"/data_pile"))
    rm(list=setdiff(ls(), c("experiment.summary","alg_fact","iii","coef","var_imp")))
    save.image(paste0(getwd(),"/data_pile/NOT-",alg_fact,"-",iii,".RData"))
    saveRDS(list(Performance="NOT", Predicted="NOT",coef=coef,var_imp=var_imp,gmean_overal="NOT",gmean_folds="NOT",
                 equat="NOT",foldbest="NOT"),
            paste0(getwd(),"/data_pile/NOT-",alg_fact,"-year-",iii-1,".rds"))
    return(list(Performance="NOT", Predicted="NOT",coef=coef,var_imp=var_imp,gmean_overal="NOT",gmean_folds="NOT",
                equat="NOT",foldbest="NOT"))
  }
}
#==================================================================


# defining the function that is used for making pivot figures
# functio_pvt: The type of statistics that is used for pivot function
pvt_maker<-function(data_res,functio_pvt=c("mean"),
                    rows_names=c("encoding","feature_selection","imputation_num","imputation_cat","resampling"),
                    columns_names=c("auc","sen","spec","accu","gmean")){
  
  if(!"pacman" %in% rownames(installed.packages())){
    install.packages(pkgs = "pacman",repos = "http://cran.us.r-project.org")
  }
  # p_load is equivalent to combining both install.packages() and library()
  pacman::p_load(pivottabler,openxlsx)
  
  pt <- PivotTable$new()
  pt$addData(data_res)
  #pt$addColumnDataGroups("auc")
  for(i in rows_names){
    pt$addRowDataGroups(i, totalCaption="Total")
  }
  for(j in columns_names){
    pt$defineCalculation(calculationName=paste0(j,"_",functio_pvt), 
                         summariseExpression=paste0(functio_pvt,"(",j, ", na.rm=TRUE)"))
  }
  
  pt$renderPivot()
  pt<-pt$asDataFrame()
  return(pt)}

#==================================================================

# LASSO Feature Selection Function for all the timestamps
# data: data imported for feature selection
# exclud: variables that are excluded for feature selection such as ID variables
LASSO_features <- function(ii,data,exclud,folds=5,trace=F,alpha=1,seed=110){
  set.seed(seed)
  dataset<-data[[ii]]
  dff<-dataset[!names(dataset)%in%paste0("year",ii-1)]
  
  for(i in 1:ncol(dff)){
    dff[i] <- as.numeric(dff[,i])
  }
  x <- data.matrix(dff)
  glmnet1 <- glmnet::cv.glmnet(x=x,y=as.factor(dataset[,paste0("year",ii-1)]),type.measure='auc',nfolds=folds,alpha=alpha, family="binomial")
  co <- coef(glmnet1,s = "lambda.1se")
  inds <- which(co[,1]!=0)
  variables <- row.names(co)[inds]
  variables <- as.data.frame(variables[!(variables %in% '(Intercept)')])
  colnames(variables) <- c("variables")
  newlist <- list(variables)
  names(newlist) <- paste("year", ii-1, sep="")
  return(newlist)
}
#==================================================================



