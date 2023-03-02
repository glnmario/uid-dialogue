library(lme4)
library(lmerTest)
library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)
library(reshape2)
library(effects)
library(jtools)

mt <- read.csv('/Users/mario/code/uid-dialogue/re-analysis/results/mt_gpt2-ft_all.csv')

# Keep only information-transmission acts
acts <- list('instruct', 'explain', 'check', 'query_yn', 'reply_w', 'clarify', 'query_w', 'reply_yn')
mt <- mt[mt$move_type %in% acts,]

# Log transform variables
mt$logh <- log(mt$normalised_h)
mt$loghdial <- log(mt$normalised_h_dialogue)
mt$loghtran <- log(mt$normalised_h_transaction_number)
mt$logpdial <- log(mt$position_in_dialogue)
mt$logptran <- log(mt$position_in_transaction)
mt$loglen <- log(mt$length)



# ================ Decontextualised information content ================

# -------------- Dialogue, all --------------
m <- lmer(logh ~ 1 + logpdial + loglen + speaker + (1 + logpdial + loglen | dialogue_id), mt)
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr       
# dialogue_id (Intercept) 1.143e-02 0.106903            
# logpdial    3.372e-05 0.005807 -1.00      
# loglen      8.837e-04 0.029728 -0.11  0.11
# Residual                1.048e-01 0.323781            
# Number of obs: 8001, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.883e+00  2.479e-02  4.735e+01  75.965   <2e-16 ***
#   logpdial    -5.348e-03  3.848e-03  3.634e+02  -1.390    0.165    
#   loglen      -1.847e-01  6.158e-03  3.632e+01 -29.988   <2e-16 ***
#   speakerg     6.358e-02  7.505e-03  7.967e+03   8.472   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) logpdl loglen
# logpdial -0.803              
# loglen   -0.168  0.014       
# speakerg -0.152  0.029 -0.122

# -------------- Dialogue, giver --------------
m <- lmer(logh ~ 1 + logpdial + loglen + (1 + logpdial + loglen | dialogue_id), mt[mt$speaker == 'g',])
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr       
# dialogue_id (Intercept) 1.230e-02 0.110920            
# logpdial    9.237e-05 0.009611 -0.68      
# loglen      9.447e-04 0.030735  0.01 -0.75
# Residual                9.124e-02 0.302055            
# Number of obs: 4559, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.911e+00  2.744e-02  5.062e+01  69.639    <2e-16 ***
#   logpdial   2.785e-04  4.716e-03  1.490e+02   0.059     0.953    
#   loglen      -1.734e-01  6.685e-03  3.457e+01 -25.944  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) logpdl
# logpdial -0.794       
# loglen   -0.148 -0.211

# -------------- Dialogue, follower --------------
m <- lmer(logh ~ 1 + logpdial + loglen + (1 + logpdial + loglen | dialogue_id), mt[mt$speaker == 'f',])
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr       
# dialogue_id (Intercept) 1.452e-02 0.120489            
# logpdial    3.152e-05 0.005614 -0.83      
# loglen      2.129e-03 0.046143 -0.05  0.60
# Residual                1.168e-01 0.341806            
# Number of obs: 3442, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   1.942329   0.035067  56.785531  55.389   <2e-16 ***
#   logpdial   -0.013947   0.006381 509.575045  -2.186   0.0293 *  
#   loglen     -0.215737   0.010299  26.680959 -20.947   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) logpdl
# logpdial -0.838       
# loglen   -0.170  0.066


# -------------- Transaction, all --------------
m <- lmer(logh ~ 1 + logptran + loglen + speaker + (1 + logptran + loglen | dialogue_id), mt)
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr       
# dialogue_id (Intercept) 7.203e-03 0.084868            
# logptran    4.642e-05 0.006813 -0.35      
# loglen      9.045e-04 0.030075 -0.10 -0.18
# Residual                1.049e-01 0.323830            
# Number of obs: 8001, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.858e+00  1.756e-02  4.038e+01 105.794   <2e-16 ***
#   logptran     8.670e-04  4.143e-03  2.754e+01   0.209    0.836    
# loglen      -1.848e-01  6.204e-03  3.597e+01 -29.789   <2e-16 ***
#   speakerg     6.409e-02  7.574e-03  7.967e+03   8.463   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgptrn loglen
# logptran -0.516              
# loglen   -0.221 -0.032       
# speakerg -0.247  0.130 -0.119

# -------------- Transaction, giver --------------
m <- lmer(logh ~ 1 + logptran + loglen + (1 + logptran + loglen | dialogue_id), mt[mt$speaker == 'g',])
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr       
# dialogue_id (Intercept) 0.0000000 0.00000             
# logptran    0.0009746 0.03122    NaN      
# loglen      0.0011769 0.03431    NaN -0.07
# Residual                0.0918397 0.30305             
# Number of obs: 4559, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.905e+00  1.163e-02  4.346e+03 163.712   <2e-16 ***
#   logptran   7.837e-03  7.038e-03  5.049e+01   1.114    0.271    
# loglen      -1.760e-01  7.080e-03  4.194e+01 -24.856   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgptrn
# logptran -0.476       
# loglen   -0.359 -0.042

# -------------- Transaction, follower --------------
m <- lmer(logh ~ 1 + logptran + loglen + (1 + logptran + loglen | dialogue_id), mt[mt$speaker == 'f',])
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr       
# dialogue_id (Intercept) 0.0178882 0.13375             
# logptran    0.0006127 0.02475  -0.76      
# loglen      0.0021866 0.04676  -0.15  0.47
# Residual                0.1166417 0.34153             
# Number of obs: 3442, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)  1.895132   0.028316 31.062027  66.927   <2e-16 ***
#   logptran    -0.006881   0.008410 27.048090  -0.818     0.42    
# loglen      -0.215439   0.010371 26.796835 -20.774   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgptrn
# logptran -0.753       
# loglen   -0.281  0.197




# ================ Contextualised information content ================

# -------------- Dialogue, all, context = dialogue --------------
m <- lmer(loghdial ~ 1 + logpdial + loglen + speaker + (1 + logpdial + loglen | dialogue_id), mt)
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr       
# dialogue_id (Intercept) 8.443e-03 0.091886            
# logpdial    2.091e-05 0.004573 -1.00      
# loglen      9.210e-04 0.030348  0.19 -0.18
# Residual                1.249e-01 0.353446            
# Number of obs: 8001, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.864e+00  2.441e-02  5.048e+01  76.379  < 2e-16 ***
#   logpdial    -3.176e-02  4.140e-03  6.388e+02  -7.671  6.4e-14 ***
#   loglen      -1.608e-01  6.429e-03  3.715e+01 -25.011  < 2e-16 ***
#   speakerg     6.936e-02  8.186e-03  7.962e+03   8.473  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) logpdl loglen
# logpdial -0.821              
# loglen   -0.033 -0.029       
# speakerg -0.168  0.029 -0.126


# -------------- Dialogue, giver, context = dialogue --------------
m <- lmer(loghdial ~ 1 + logpdial + loglen + (1 + logpdial + loglen | dialogue_id), mt[mt$speaker == 'g',])
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr       
# dialogue_id (Intercept) 1.103e-02 0.105025            
# logpdial    7.229e-05 0.008502 -1.00      
# loglen      6.403e-04 0.025305  0.20 -0.20
# Residual                1.118e-01 0.334418            
# Number of obs: 4559, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   1.897659   0.028649  51.524314  66.239  < 2e-16 ***
#   logpdial     -0.025099   0.005094 254.083348  -4.928  1.5e-06 ***
#   loglen       -0.152105   0.006329  32.888103 -24.032  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) logpdl
# logpdial -0.871       
# loglen   -0.121 -0.037


# -------------- Dialogue, follower, context = dialogue --------------
m <- lmer(loghdial ~ 1 + logpdial + loglen + (1 + logpdial + loglen | dialogue_id), mt[mt$speaker == 'f',])
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr     
# dialogue_id (Intercept) 6.341e-03 0.079632          
# logpdial    8.947e-05 0.009459 0.02     
# loglen      2.505e-03 0.050046 0.06 1.00
# Residual                1.367e-01 0.369699          
# Number of obs: 3442, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)     1.934715   0.034256  67.427542  56.478  < 2e-16 ***
#   logpdial     -0.044644   0.007065 161.067964  -6.319 2.47e-09 ***
#   loglen       -0.188105   0.011045  27.704396 -17.030 3.30e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) logpdl
# logpdial -0.852       
# loglen   -0.155  0.180


# -------------- Transaction, all, context = transaction --------------
m <- lmer(loghtran ~ 1 + logptran + loglen + speaker + (1 + logptran + loglen | dialogue_id), mt)
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance  Std.Dev.  Corr       
# dialogue_id (Intercept) 5.234e-03 0.0723471            
# logptran    1.341e-07 0.0003662 -1.00      
# loglen      1.149e-03 0.0338898 -0.16  0.16
# Residual                1.072e-01 0.3274576            
# Number of obs: 8001, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)    1.945e+00  1.608e-02  4.215e+01 120.948  < 2e-16 ***
#   logptran    -2.646e-02  3.997e-03  5.842e+03  -6.621 3.89e-11 ***
#   loglen      -2.130e-01  6.733e-03  3.702e+01 -31.626  < 2e-16 ***
#   speakerg     5.870e-02  7.658e-03  7.969e+03   7.666 1.99e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgptrn loglen
# logptran -0.516              
# loglen   -0.262  0.011       
# speakerg -0.272  0.135 -0.111

# -------------- Transaction, giver, context = transaction --------------
m <- lmer(loghtran ~ 1 + logptran + loglen + (1 + logptran + loglen | dialogue_id), mt[mt$speaker == 'g',])
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr     
# dialogue_id (Intercept) 0.0000000 0.00000           
# logptran    0.0006516 0.02553   NaN     
# loglen      0.0010771 0.03282   NaN 0.06
# Residual                0.0960985 0.31000           
# Number of obs: 4559, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.968e+00  1.189e-02  4.336e+03  165.49   <2e-16 ***
#   logptran    -1.572e-02  6.442e-03  4.770e+01   -2.44   0.0184 *  
#   loglen      -1.969e-01  6.945e-03  4.093e+01  -28.35   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgptrn
# logptran -0.531       
# loglen   -0.375  0.034

# -------------- Transaction, follower, context = transaction --------------
m <- lmer(loghtran ~ 1 + logptran + loglen + (1 + logptran + loglen | dialogue_id), mt[mt$speaker == 'f',])
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr       
# dialogue_id (Intercept) 0.0141001 0.11874             
# logptran    0.0006154 0.02481  -0.78      
# loglen      0.0030573 0.05529  -0.20  0.62
# Residual                0.1159993 0.34059             
# Number of obs: 3442, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)    2.01204    0.02650 31.28371  75.925  < 2e-16 ***
#   logptran    -0.04174    0.00837 30.35150  -4.986 2.35e-05 ***
#   loglen      -0.25725    0.01145 29.52896 -22.459  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgptrn
# logptran -0.780       
# loglen   -0.311  0.273



# ================ Mutual information ================

# -------------- Dialogue, all, context = dialogue --------------
m <- lmer(mi_dialogue_id ~ 1 + logpdial + loglen + speaker + (1 + logpdial + loglen | dialogue_id), mt)
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.073316 0.27077             
# logpdial    0.002656 0.05153  -0.69      
# loglen      0.007667 0.08756  -0.73  0.02
# Residual                0.733418 0.85640             
# Number of obs: 8001, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  1.435e-01  6.472e-02  4.310e+01   2.218   0.0319 *  
#   logpdial     1.554e-01  1.323e-02  4.195e+01  11.743 7.63e-15 ***
#   loglen      -2.357e-01  1.742e-02  3.753e+01 -13.533 5.15e-16 ***
#   speakerg     1.759e-03  1.981e-02  7.953e+03   0.089   0.9293    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) logpdl loglen
# logpdial -0.814              
# loglen   -0.491 -0.017       
# speakerg -0.157  0.028 -0.113

# -------------- Dialogue, giver, context = dialogue --------------
m <- lmer(mi_dialogue_id ~ 1 + logpdial + loglen + (1 + logpdial + loglen | dialogue_id), mt[mt$speaker == 'g',])
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.052536 0.22921             
# logpdial    0.001905 0.04364   0.37      
# loglen      0.019754 0.14055  -0.92 -0.70
# Residual                0.636376 0.79773             
# Number of obs: 4559, groups:  dialogue_id, 38
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)  0.15119    0.06655 40.84549   2.272   0.0284 *  
#   logpdial     0.14037    0.01393 51.80363  10.080 8.23e-14 ***
#   loglen      -0.20209    0.02575 34.57920  -7.847 3.47e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) logpdl
# logpdial -0.539       
# loglen   -0.572 -0.341


# -------------- Dialogue, follower, context = dialogue --------------
m <- lmer(mi_dialogue_id ~ 1 + logpdial + loglen + (1 + logpdial + loglen | dialogue_id), mt[mt$speaker == 'f',])
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)


# -------------- Transaction, all, context = transaction --------------
m <- lmer(mi_transaction_number ~ 1 + logptran + loglen + speaker + (1 + logptran + loglen | dialogue_id), mt)
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)


# -------------- Transaction, giver, context = transaction --------------
m <- lmer(mi_transaction_number ~ 1 + logptran + loglen + (1 + logptran + loglen | dialogue_id), mt[mt$speaker == 'g',])
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)


# -------------- Transaction, follower, context = transaction --------------
m <- lmer(mi_transaction_number ~ 1 + logptran + loglen + (1 + logptran + loglen | dialogue_id), mt[mt$speaker == 'f',])
summ(m, scale = TRUE, n.sd = 2,  confint = TRUE, digits = 3)


