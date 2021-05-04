######################################
#                                    #
#  ML IN ECONOMICS - FINAL PROJECT   #
#       GILLIAN, PABLO, YANN         #
#             MAY 2021               #
#                                    #
######################################





# 00. PACKAGES AND DATA LOADING -------------------------------------------
## 00.1. LOADING PACKAGES
# Installs packages if not already installed, then loads packages 
list.of.packages <- c("glmnet", "rpart", "rpart.plot", "randomForest", "devtools", "tidyverse",
  "knitr", "SuperLearner", "caret", "xgboost","stargazer","xtable",'haven','labelled','writexl',
  'readxl', 'kableExtra')


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))



# install causalTree package from Susan Athey's github
#install_github('susanathey/causalTree')
library(causalTree)

## 00.2. DATA LOADING
# Setting working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Checking correct path loaded:
getwd()

# Loading cleaned dataset
data = read_dta('ELA.dta')

# Checking column names
colnames(data)



# 00.3. DATA PREPARATION --------------------------------------------------
# Finding controls variables to add:

variables_labels = as.data.frame(do.call(rbind,lapply(1:length(data), function(k){
  c(names(data)[k], as.character(var_label(data[,k])),as.numeric(sum(is.na(data[,k]))))
})))
# Keeping variables with smaller than 6 missing observations
View(variables_labels %>% mutate(V3=as.numeric(V3)))
variables_labels = variables_labels %>% mutate(V3=as.numeric(V3)) %>% filter(V3<6)


write_xlsx(x= as.data.frame(variables_labels),
           path = file.path('variables.xlsx'))

# Creating lists:
responses = c("Entrep_total", "any_iga", "selfempl", "empl", "Expenditure_totDF")
treatment = c('treatment')
lines = c('R','Q')
# Removes all identifiers 
remove = c('id','idno','pid')

controls = setdiff(variables_labels$V1, c(treatment,
                                                    paste0(rep(lines,1,
                                                               each = length(responses)),
                                                           responses),
                                                    remove))


length(controls)# 58 variables selected



# 01. CAUSAL TREES --------------------------------------------------------
cleanTitle <- function(name,add=""){
  line = str_extract(name,'^.{1}')
  end = ifelse(line=='R','midline','endline')
  title = names_clean[[str_remove(name,'^.{1}')]][1]
  return(paste0(add,title,' at ', end))
}

causal_tree <- function(data,line,response,treatment,controls){
  # Data preparation / Fold splitting
  data = drop_na(data[,c(paste0(line,response),treatment,controls)])
  nrow(data)
  folds = createFolds(1:nrow(data), k=2)
  
  Y1 = pull(data[folds[[1]],], paste0(line,response))
  Y2 = pull(data[folds[[2]],], paste0(line,response))
  
  
  X1 = pull(data[folds[[1]],], treatment)
  X2 = pull(data[folds[[2]],], treatment)
  
  
  W1 = data[folds[[1]], controls]
  W2 = data[folds[[2]], controls]
  
  
  # Formula
  tree_fml <- as.formula(paste("Y",paste0('`',paste(names(W1), collapse = '` + `'),'`'), sep = " ~ "))
  
  # Honest tree
  honest_tree <- honest.causalTree(formula = tree_fml,
                                   data = cbind(W1,data.frame(Y=Y1)),
                                   treatment = X1,
                                   est_data = cbind(W2,data.frame(Y=Y2)),
                                   est_treatment = X2,
                                   split.alpha = 0.5,
                                   split.Rule = "CT",
                                   split.Honest = T,
                                   cv.alpha = 0.5,
                                   cv.option = "CT",
                                   cv.Honest = T,
                                   split.Bucket = T,
                                   bucketNum = 10,
                                   bucketMax = 100, # maximum number of buckets
                                   minsize = 100) # number of observations in treatment and control on leaf


  rpart.plot(honest_tree, roundint = F)


  opcpid <- which.min(honest_tree$cp[, 4])
  opcp <- honest_tree$cp[opcpid, 1]
  honest_tree_prune <- prune(honest_tree, cp = opcp)

  png(filename = paste0(line,response,'.png'), units = 'px',
  width = 1500, height = 800)
  rpart.plot(honest_tree_prune, roundint = F)
  title = cleanTitle(paste0(line,response),add = 'Sources of treatment heterogeneity on ')
  title(title)
  
  dev.off()



  leaf2 <- as.factor(round(predict(honest_tree_prune,
                                   newdata = cbind(W2,data.frame(Y=Y2)),
                                   type = "vector"), 4))
  
  if (length(unique(leaf2))==1){
    return("one leaf")
  }else{
    honest_ols_2 <- lm( Y ~ leaf + X * leaf - X -1,
                        data = cbind(W2,data.frame(Y=Y2, X=X2, leaf=leaf2)))
    
    return(honest_ols_2)
  }

  
}

resultCausalTree = list()
for(line in lines){
  for(response in responses){
    print(response)
    resultCausalTree[[paste0(line,response)]] <- causal_tree(data,line,response,treatment,controls)
  }
}
  



# 02. BLP -----------------------------------------------------------------
zeros <- function(n) {
  return(integer(n))
}
ones <- function(n) {
  return(integer(n)+1)
}

blp <- function(data,line,response,treatment,controls, prop_scores=F) {
  ## STEP 0: CREATE DATA VECTORS
  data = drop_na(data[,c(paste0(line,response),treatment,controls)])
  nrow(data)
  Y = pull(data, paste0(line,response))
  X = pull(data, treatment)
  W = data[, controls]
  
  ### STEP 1: split the dataset into two sets, 1 and 2 (50/50)
  split <- createFolds(1:length(Y), k=2)[[1]]
  
  Ya = Y[split]
  Yb = Y[-split]
  
  Xa = X[split]
  Xb = X[-split]
  
  Wa = W[split, ]
  Wb = W[-split, ]
  
  ### STEP 2a: (Propensity score) On set A, train a model to predict X using W. Predict on set B.
  if (prop_scores==T) {
    sl_w1 = SuperLearner(Y = Xa, 
                         X = Wa, 
                         newX = Wb, 
                         family = binomial(), 
                         SL.library = "SL.xgboost", 
                         cvControl = list(V=0))
    
    p <- sl_w1$SL.predict
  } else {
    p <- rep(mean(Xa), length(Xb))
  }
  
  ### STEP 2b let D = W(set B) - propensity score.
  D <- Xb-p
  
  ### STEP 3a: Get CATE (for example using xgboost) on set A. Predict on set B.
  sl_y = SuperLearner(Y = Ya, 
                      X = cbind(Wa,data.frame(X=Xa)), 
                      family = gaussian(), 
                      SL.library = "SL.xgboost", 
                      cvControl = list(V=0))
  
  pred_y1 = predict(sl_y, newdata=cbind(Wb,data.frame(X=ones(nrow(Wb)))))
  
  pred_0s <- predict(sl_y, cbind(Wb,data.frame(X=zeros(nrow(Wb)))), onlySL = T)
  pred_1s <- predict(sl_y, cbind(Wb,data.frame(X=ones(nrow(Wb)))), onlySL = T)
  
  cate <- pred_1s$pred - pred_0s$pred
  
  ### STEP 3b: Subtract the expected CATE from the CATE
  C = cate-mean(cate)
  
  ### STEP 4: Create a dataframe with Y, W (set B), D, C and p. Regress Y on W, D and D*C. 
  df <- cbind(Wb,data.frame(Y=Yb, D, C, p))
  
  Wnames <- paste0('`',paste(colnames(Wb), collapse="`+`"),'`')
  fml <- paste("Y ~",Wnames,"+ D + D:C")
  model <- lm(fml, df, weights = 1/(p*(1-p))) 
  
  return(model) 
}

table_from_blp <-function(model) {
  thetahat <- model%>% 
    .$coefficients %>%
    .[c("D","D:C")]
  
  # Confidence intervals
  cihat <- confint(model)[c("D","D:C"),]
  
  res <- tibble(Coefficient = c("beta1","beta2"),
                Estimates = thetahat,
                'Lower Bound 90%' = cihat[,1],
                'Upper Bound 90%'= cihat[,2])
  
  return(res)
}

rerunParam = 100
resultBLP = list()
for(line in lines){
  for(response in responses){
    print(response)
    resultBLP[[paste0(line,response)]] <- rerun(rerunParam, table_from_blp(blp(data,line,response,treatment,controls))) %>% # Increase reruns in practice!
      bind_rows %>%
      group_by(Coefficient) %>%
      summarize_all(median)
    }
}
saveRDS(resultBLP, file="resultBLP.RDS")
resultBLP = readRDS("resultBLP.RDS")

names(resultBLP)
names_clean = list('Entrep_total'='Entrepreneurial ability index',
                   "any_iga" = 'Any income-generating activity',
                   "selfempl" = "Self-employed",
                   "empl" = 'Wage employed',
                   "Expenditure_totDF" = "Expenditure on goods in the last month")



for(name in names(resultBLP)){
 title =  cleanTitle(name,add="Best Linear Predictor for ")
 print(title)
 table = kable(resultBLP[['REntrep_total']], "latex",caption=title,label=name,booktabs = T)
 writeLines(table,con=paste0(name,'.tex'))
}



# 03. GATES ---------------------------------------------------------------


gates <- function(data,line,response,treatment,controls, Q=4, prop_scores=F) {
  ## STEP 0: CREATE DATA VECTORS
  data = drop_na(data[,c(paste0(line,response),treatment,controls)])
  nrow(data)
  Y = pull(data, paste0(line,response))
  X = pull(data, treatment)
  W = data[, controls]
  
  ### STEP 1: split the dataset into two sets, 1 and 2 (50/50)
  split <- createFolds(1:length(Y), k=2)[[1]]
  
  Ya = Y[split]
  Yb = Y[-split]
  
  Xa = X[split]
  Xb = X[-split]
  
  Wa = W[split, ]
  Wb = W[-split, ]
  
  ### STEP 2a: (Propensity score) On set A, train a model to predict X using W. Predict on set B.
  if (prop_scores==T) {
    sl_w1 = SuperLearner(Y = Xa, 
                         X = Wa, 
                         newX = Wb, 
                         family = binomial(), 
                         SL.library = "SL.xgboost", 
                         cvControl = list(V=0))
    
    p <- sl_w1$SL.predict
  } else {
    p <- rep(mean(Xa), length(Xb))
  }
  
  ### STEP 2b let D = W(set B) - propensity score.
  D <- Xb-p
  
  ### STEP 3a: Get CATE (for example using xgboost) on set A. Predict on set B.
  sl_y = SuperLearner(Y = Ya, 
                      X = data.frame(X=Xa, Wa), 
                      family = gaussian(), 
                      SL.library = "SL.xgboost", 
                      cvControl = list(V=0))
  
  pred_y1 = predict(sl_y, newdata=cbind(Wb,data.frame(X=ones(nrow(Wb)))))
  
  pred_0s <- predict(sl_y, cbind(Wb,data.frame(X=zeros(nrow(Wb)))), onlySL = T)
  pred_1s <- predict(sl_y, cbind(Wb,data.frame(X=ones(nrow(Wb)))), onlySL = T)
  
  cate <- pred_1s$pred - pred_0s$pred
  
  ### STEP 3b: divide the cate estimates into Q tiles, and call this object G. 
  # Divide observations into n tiles
  G <- data.frame(cate) %>% # replace cate with the name of your predictions object
    ntile(Q) %>%  # Divide observations into Q-tiles
    factor()
  
  ### STEP 4: Create a dataframe with Y, W (set B), D, G and p. Regress Y on group membership variables and covariates. 
  df <- data.frame(Y=Yb, Wb, D, G, p)
  
  Wnames <- paste0('`',paste(colnames(Wb), collapse="+"),'`')
  fml <- paste("Y ~",Wnames,"+ D:G")
  model <- lm(fml, df, weights = 1/(p*(1-p))) 
  
  group_mean <- df %>% 
    select(-Y, -p,-D) %>% 
    filter(G==1 | G==Q) %>% 
    group_by(G) %>% 
    summarise_all(mean)
  
  
  return(list("model" = model,
              "clan"= group_mean)) 
}

table_from_gates <-function(model) {
  thetahat <- model%>% 
    .$coefficients %>%
    .[c("D:G1","D:G2","D:G3","D:G4")]
  
  # Confidence intervals
  cihat <- confint(model)[c("D:G1","D:G2","D:G3","D:G4"),]
  
  res <- tibble(coefficient = c("gamma1","gamma2","gamma3","gamma4"),
                estimates = thetahat,
                ci_lower_90 = cihat[,1],
                ci_upper_90 = cihat[,2])
  
  return(res)
}

output <- rerun(10, gates(data,line,response,treatment,controls)) 


table = lapply(1:length(output),
               function(k) table_from_gates(output[[k]][["model"]]))%>%
  bind_rows %>%
  group_by(coefficient) %>%
  summarize_all(median)
table
kable(table, format = 'latex')


resultGATES = list()
for(line in lines){
  for(response in responses){
    print(response)
    resultGATES[[paste0(line,response)]][["raw"]] <- rerun(10, gates(data,line,response,treatment,controls))
    resultGATES[[paste0(line,response)]][['tableGATES']] = lapply(1:length(resultGATES[[paste0(line,response)]][["raw"]]),
                                               function(k) table_from_gates(resultGATES[[paste0(line,response)]][["raw"]][[k]][["model"]]))%>%
      bind_rows %>%
      group_by(coefficient) %>%
      summarize_all(median)
    resultGATES[[paste0(line,response)]][['clan']] = lapply(1:length(resultGATES[[paste0(line,response)]][["raw"]]),
                                                            function(k) resultGATES[[paste0(line,response)]][["raw"]][[k]][["clan"]])%>% 
      bind_rows %>%
      group_by(G) %>% 
      summarize_all(median)
    }
}


