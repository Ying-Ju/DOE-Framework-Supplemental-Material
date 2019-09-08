# DoE for ML simulation
# Write-out files after running script?
WriteOut = F

# install and load necessary R packages via pacman library
if(any(installed.packages()[,'Package']!='pacman')){
  install.packages('pacman')
}
library(pacman)
p_load('tidyverse', 'rstudioapi', 'readxl', 'DoE.base', 'AlgDesign')

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

# Read in original data file, factors of interest and response retained
dat_all <- read_excel('Data/all_scenarios_rdl.xlsx') %>%
  select(c(impute_num, encoding, var_selection,	impute_cat,	resampling,	algorithm, gmean, auc))

# Read in D-opt design created with JMP's DoE Custom Design Tool
Dopt_JMP <- read_excel('Data/D-opt_design_223459.xlsx')
# Find rows in original data file (including replicates) that match rows of JMP design
dat_Dopt_JMP <- inner_join(dat_all, Dopt_JMP)
# If set as TRUE, write result to file
if(WriteOut) write_csv(dat_Dopt_JMP, 'Data/Dopt223459_JMP_w_ys.csv')


# Create a near OA using DoE.base R package
library(DoE.base)
cand <- oa.design(nlevels = c(2, 2, 3, 4, 5, 9), nruns = 180,
                  randomize = F, seed = 2718, columns = 'min3') # will auto create FF n=2160
library(AlgDesign)
# can we use AlgDesign::gen.factorial() in place of DoE.base::oa.design() above?
Dopt_R_MEs <- optFederov(~., cand, nRepeats = 500, nTrials = 180) # D criterion default
dat_Dopt_R_MEs <- Dopt_R_MEs$design %>%
  rename('impute_num'='A', 'encoding'='B', 'var_selection'='C',
         'impute_cat'='D',	'resampling'='E',	'algorithm'='F') #%>% 
  # recode() %>% 
  # inner_join(dat_all)
if(WriteOut) write_csv(dat_Dopt_R_MEs, 'Data/Dopt223459_MEs_R.csv')

Dopt_R_MEs_2FIs <- optFederov(~.^2, cand, nRepeats = 100, nTrials = 180)    # criterion = 'D')
dat_Dopt_R_MEs_2FIs <- Dopt_R_MEs_2FIs$design %>%
  rename('impute_num'='A', 'encoding'='B', 'var_selection'='C',
         'impute_cat'='D',	'resampling'='E',	'algorithm'='F') #%>% 
# recode() %>% 
# inner_join(dat_all)
if(WriteOut) write_csv(dat_Dopt_R_MEs_2FIs, 'Data/Dopt223459_MEs_2FIs_R.csv')


