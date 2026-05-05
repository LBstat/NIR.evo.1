
options(scipen = 999)

# General setting
library(R6)
library(readxl)
library(tidyverse)

# Data manipulation and handling
library(mice)
library(signal)
library(naniar)
library(data.table)
library(checkmate)

# Data visualization
library(igraph)
library(cowplot)
library(patchwork)

# Statistical analysis
library(MASS)
library(mlr3)
library(kknn)
library(klaR)
library(pROC)
library(lme4)
library(e1071)
library(ppcor)
library(MuMIn)
library(ranger)
library(nortest)
library(xgboost)
library(lmerTest)
library(mlr3learners)

# Report
library(kableExtra)

# Project settings
N_IMP <- 10
N_PERM <- 999
NFEAT_MAX_FS <- 5L
NFEAT_MAX_QDA <- 2L
KVALUE_THRESHOLD <- 0.5
POS_LABEL <- "very good/good"
fs_methods <- c("auc", "anova", "mrmr")
NEG_LABEL <- "impending spoilage/spoiled"
CLASS_LEVELS <- c("very good/good", "impending spoilage/spoiled")

# Tuning grid kNN:
KNN_K_GRID <- seq_len(10L)  
KNN_KERNEL_GRID <- c("optimal", "rectangular", "epanechnikov", "gaussian")

# Tuning grid SVM
SVM_C_GRID <- c(0.01, 0.1, 1, 10, 100)

# Tuning grid RF
RF_MTRY_GRID <- c(2L, 5L, 10L)
RF_NODESIZE_GRID <- c(1L, 3L, 5L)

# Tuning grid XGBoost
XGB_ETA_GRID <- c(0.05, 0.1, 0.3)
XGB_DEPTH_GRID <- c(2L, 3L, 4L)
XGB_SUB_GRID <- c(0.7, 0.8, 1.0)

