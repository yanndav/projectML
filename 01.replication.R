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
list.of.packages <- c('haven',
                      'labelled','lmtest','sandwich')

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

# Checking labels of some variables
var_label(data$incind_empl) # "Wage employment income (trimmed) (last year)"
var_label(data$Rincind_empl) # "Self-employment income (trimmed) (last year)"
var_label(data$Entrep_total) # "Total Entreprenuership score [0,100] rescaled"


# 01. RESULTS REPLICATION -------------------------------------------------

var = c("Entrep_total", "any_iga", "selfempl", "empl", "Expenditure_totDF")
branchs = grep('_B.*',colnames(data),value = TRUE)
controls = c("age")

results = data.frame('var'=character(),
                     'esti' = character(),
                     'vals'=numeric(),
                     'line'=character())

for(i in seq(1,length(var))){
  for(line in c('R','Q')){
    
    formula = as.formula(paste0(line,var[i],
                  ' ~ ',
                  var[i],
                  ' + treatment + ',
                  paste(controls,collapse = " + "),' + `',
                  paste(branchs, collapse = '` + `'),"`"))

    reg = lm(formula = formula, data = data[data$panel==1,])
    reg_clu = coeftest(reg,  vcov = vcovCL(reg, type="HC1", cluster = ~villid))
    
    te = round(reg_clu['treatment',"Estimate"],3)
    se = round(reg_clu['treatment','Std. Error'],3)
    
    results = results %>% 
      rbind(data.frame('var'=rep(var[i],2),
                       'esti'=c('te','se'),
                       'vals'= c(te,se),
                       'line'=rep(line,2)))
    
  }
  
  
  
}
view(pivot_wider(results,id_cols=c('var','esti'), names_from = 'line', values_from = 'vals'))
# HERE, NEED TO EXPORT TABLES
names_clean = list('Entrep_total'='Entrepreneurial ability index',
                   "any_iga" = 'Any income-generating activity',
                   "selfempl" = "Self-employed",
                   "empl" = 'Wage employed',
                   "Expenditure_totDF" = "Expenditure on goods in the last month")
TABLE = pivot_wider(results,id_cols=c('var','esti'), names_from = 'line', values_from = 'vals') %>% 
  mutate(R = ifelse(esti=="se",paste0('(',R,')'),R),
         Q = ifelse(esti=="se",paste0('(',Q,')'),Q)) %>% 
  select(-esti) 

TABLE[,"var"] <- c("Entrep. ability index",'',"Income-gen. activ.","",
                   "Self-employed",'','Wage employed',"","Expend. goods. month","")

write.table(TABLE, file = "output/replication.tex",eol='\\\\', sep='&',quote = F,row.names = F,
            col.names = F,na="")

