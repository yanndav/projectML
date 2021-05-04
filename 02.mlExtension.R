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
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org", dependencies = T)

invisible(lapply(list.of.packages, library, character.only = TRUE))


# install causalTree package from Susan Athey's github
if(!('causalTree' %in% installed.packages())){
  install_github('susanathey/causalTree')
}
library(causalTree)

## 00.2. DATA LOADING
# Setting working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Checking correct path loaded:
getwd()

# Loading cleaned dataset
data = read_dta('ELA.dta') %>% filter(panel==1) %>% select(-panel)

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
           path = file.path('output/variables.xlsx'))

# Creating lists:
responses = c("Entrep_total", "any_iga", "selfempl", "empl", "Expenditure_totDF")
treatment = c('treatment')
lines = c('R','Q')
# Removes all identifiers 
remove = c('id','idno','pid','branch_name',grep('z.*',colnames(data),value = T),
           grep('ALL',colnames(data),value = T), 'baseline','follow_up')
controls = as.vector(variables_labels$V1[!(variables_labels$V1 %in% c(treatment,
                                                    paste0(rep(lines,1,
                                                               each = length(responses)),
                                                           responses),
                                                    remove))])
matCor = cor(select_if(data[,controls], is.numeric), use='pairwise.complete.obs')
length(controls)# 58 variables selected

names_clean = list('Entrep_total'='Entrepreneurial ability index',
                   "any_iga" = 'Any income-generating activity',
                   "selfempl" = "Self-employed",
                   "empl" = 'Wage employed',
                   "Expenditure_totDF" = "Expenditure on goods in the last month")

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

  png(filename = paste0('output/',line,response,'.png'), units = 'px',
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
saveRDS(resultCausalTree, file="output/resultCausalTree.RDS")
# resultCausalTree = readRDS("output/resultCausalTreeRDS")

#HERE, NEED TO EXPORT RESULTS
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
  summary(model)
  return(model) 
}

table_from_blp <-function(model) {
  thetahat <- model%>% 
    .$coefficients %>%
    .[c("D","D:C")]
  
  # Confidence intervals
  cihat <- confint(model)[c("D","D:C"),]
  
  res <- tibble(coef = c("beta1","beta2"),
                esti = thetahat,
                lb = cihat[,1],
                ub= cihat[,2])
  
  return(res)
}

rerunParam = 2
resultBLP = list()
for(line in lines){
  for(response in responses){
    print(line)
    print(response)
    resultBLP[[paste0(line,response)]] <- rerun(rerunParam, table_from_blp(blp(data,line,response,treatment,controls))) %>% # Increase reruns in practice!
      bind_rows %>%
      group_by(coef) %>%
      summarize_all(median, na.rm=T)
    }
}
saveRDS(resultBLP, file="output/resultBLP.RDS")
resultBLP = readRDS("output/resultBLP.RDS")

BLPmid = data.frame(cbind(t(resultBLP[[names(resultBLP)[1]]][,2:4]),
      t(resultBLP[[names(resultBLP)[2]]][,2:4]),
      t(resultBLP[[names(resultBLP)[3]]][,2:4]),
      t(resultBLP[[names(resultBLP)[4]]][,2:4]),
      t(resultBLP[[names(resultBLP)[5]]][,2:4]))) %>% 
  mutate_all(round,digits=2)

write.table(BLPmid, file = "output/blp_midline.tex",eol='\\\\', sep='&',quote = F,row.names = F,
            col.names = F)


BLPend = data.frame(cbind(t(resultBLP[[names(resultBLP)[6]]][,2:4]),
                          t(resultBLP[[names(resultBLP)[7]]][,2:4]),
                          t(resultBLP[[names(resultBLP)[8]]][,2:4]),
                          t(resultBLP[[names(resultBLP)[9]]][,2:4]),
                          t(resultBLP[[names(resultBLP)[10]]][,2:4]))) %>% 
  mutate_all(round,digits=2) 
rownames(BLPend) <- c('Estimate','LB 90\%','UB 90\%')

write.table(BLPend, file = "output/blp_endline.tex",eol='\\\\', sep='&',quote = F,row.names = F,
            col.names = F)

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
                      X = cbind(Wa,data.frame(X=Xa)), 
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
  df <- cbind(Wb,data.frame(Y=Yb, D, G, p))
  
  Wnames <- paste0('`',paste(colnames(Wb), collapse="`+`"),'`')
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
  
  res <- tibble(Coefficient = c("Gamma1","Gamma2","Gamma3","Gamma4"),
                Estimates = thetahat,
                'Lower Bound 90%' = cihat[,1],
                'Upper Bound 90%' = cihat[,2])
  
  return(res)
}


rereunParam = 100
resultGATES = list()
for(line in lines){
  for(response in responses){
    print(line)
    print(response)
    resultGATES[[paste0(line,response)]][["raw"]] <- rerun(rereunParam, gates(data,line,response,treatment,controls))
    resultGATES[[paste0(line,response)]][['tableGATES']] = lapply(1:length(resultGATES[[paste0(line,response)]][["raw"]]),
                                               function(k) table_from_gates(resultGATES[[paste0(line,response)]][["raw"]][[k]][["model"]]))%>%
      bind_rows %>%
      group_by(Coefficient) %>%
      summarize_all(median)
    resultGATES[[paste0(line,response)]][['clan']] = lapply(1:length(resultGATES[[paste0(line,response)]][["raw"]]),
                                                            function(k) resultGATES[[paste0(line,response)]][["raw"]][[k]][["clan"]])%>% 
      bind_rows %>%
      group_by(G) %>% 
      summarize_all(median)
    }
}
saveRDS(resultGATES, file="output/resultGATES.RDS")
# resultGATES = readRDS("output/resultGATES.RDS")

for(name in names(resultGATES)){
  # GATES
  title =  cleanTitle(name,add="Sorted Group Average Treatment Effects for ")
  print(title)
  table = kable(resultGATES[[name]][['tableGATES']], "latex",caption=title,label=paste0("gates",name),booktabs = T)
  writeLines(table,con=paste0('output/GATES_',name,'.tex'))
  
  # CLAN
  CLAN_df = as.data.frame(t(resultGATES[[name]][['clan']])[2:ncol(resultGATES[[name]][['clan']]),]) %>% 
    mutate(V1=as.numeric(V1),
           V2=as.numeric(V2),
           "Abs. Perc. Difference" = abs((V1-V2)/V1),
           rank = n()-rank(`Abs. Perc. Difference`)) %>% 
    rename("Group 1"=V1,
           "Group 4"=V2) %>% 
    filter(rank<=10) %>% 
    select(-rank)
  
  titleCLAN =  cleanTitle(name,add="Classification Analysis for ")
  print(titleCLAN)
  tableCLAN = kable(CLAN_df, "latex",caption=titleCLAN,label=paste0("clan",name),booktabs = T)
  writeLines(tableCLAN,con=paste0('output/CLAN_',name,'.tex'))
  
}



  
  

