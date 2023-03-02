library(lme4)
library(lmerTest)

pb <- read.csv('/Users/mario/code/uid-dialogue/re-analysis/results/pb_gpt2-ft_all.csv')
chains <- read.csv('/Users/mario/code/uid-dialogue/re-analysis/results/pbchains_gpt2-ft_both_contexts.csv')

# Log transform variables
pb$logh <- log(pb$normalised_h)
pb$loghdial <- log(pb$normalised_h_dialogue)
pb$loghrnd <- log(pb$normalised_h_round_number)
pb$logpdial <- log(pb$position_in_dialogue)
pb$logprnd <- log(pb$position_in_round)
pb$loglen <- log(pb$length)
# pb$logmidial <- log(pb$mi_dialogue_id)
# pb$logmirnd <- log(pb$mi_round_number)

chains$logh <- log(chains$normalised_h)
chains$loghchain <- log(chains$normalised_h_chains_id)
chains$loghdial <- log(chains$hsc_dial)
chains$logpchain <- log(chains$position_in_chain)
chains$loglen <- log(chains$length)
# chains$logmidial <- log(chains$mi_dial)
# chains$logmichain <- log(chains$mi_chains_id)


# ================ Decontextualised information content ================

# -------------- Dialogue --------------
m <- lmer(logh ~ 1 + logpdial + loglen + (1 + logpdial + loglen | dialogue_id), pb)
summary(m)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.031969 0.17880             
# logpdial    0.001442 0.03797  -0.57      
# loglen      0.003381 0.05814  -0.29 -0.06
# Residual                0.112029 0.33471             
# Number of obs: 49012, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   1.788005   0.009363 662.527845  190.96   <2e-16 ***
#   logpdial      0.040983   0.002213 712.291813   18.52   <2e-16 ***
#   loglen       -0.181847   0.002787 679.188684  -65.25   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) logpdl
# logpdial -0.739       
# loglen   -0.403  0.047


# -------------- Round --------------
m <- lmer(logh ~ 1 + logprnd + loglen + (1 + logprnd + loglen | dialogue_id), pb)
summary(m)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.023209 0.15234             
# logprnd     0.001006 0.03171  -0.26      
# loglen      0.003382 0.05816  -0.37 -0.03
# Residual                0.113916 0.33751             
# Number of obs: 49012, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   1.931732   0.007547 584.790343 255.961   <2e-16 ***
#   logprnd      -0.001482   0.002344 730.506870  -0.632    0.527    
# loglen       -0.188843   0.002796 676.836178 -67.544   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgprnd
# logprnd -0.550       
# loglen  -0.497  0.077


# -------------- Chain --------------
m <- lmer(logh ~ 1 + logpchain + loglen + (1 + logpchain + loglen | dialogue_id), chains)
summary(m)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.125263 0.35392             
# logpchain   0.003736 0.06112  -0.63      
# loglen      0.018095 0.13452  -0.92  0.65
# Residual                0.077140 0.27774             
# Number of obs: 14431, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   2.062132   0.019093 601.460557  108.00   <2e-16 ***
#   logpchain     0.061040   0.005543 681.758059   11.01   <2e-16 ***
#   loglen       -0.244561   0.007427 642.552585  -32.93   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgpchn
# logpchain -0.545       
# loglen    -0.945  0.437


# ================ Contextualised information content ================

# -------------- Dialogue, context = dialogue --------------
m <- lmer(loghdial ~ 1 + logpdial + loglen + (1 + logpdial + loglen | dialogue_id), pb)
summary(m)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.034945 0.18694             
# logpdial    0.001273 0.03568  -0.50      
# loglen      0.004464 0.06681  -0.49  0.09
# Residual                0.137325 0.37057             
# Number of obs: 49012, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   1.983570   0.010093 679.229591 196.526  < 2e-16 ***
#   logpdial     -0.014241   0.002309 736.647652  -6.168 1.14e-09 ***
#   loglen       -0.251812   0.003155 684.500521 -79.809  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) logpdl
# logpdial -0.725       
# loglen   -0.509  0.118
 
# -------------- Round, context = round --------------
m <- lmer(loghrnd ~ 1 + logprnd + loglen + (1 + logprnd + loglen | dialogue_id), pb)
summary(m)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr       
# dialogue_id (Intercept) 0.0241656 0.15545             
# logprnd     0.0008905 0.02984  -0.07      
# loglen      0.0041151 0.06415  -0.52 -0.07
# Residual                0.1188569 0.34476             
# Number of obs: 49012, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   2.053854   0.007712 606.697689  266.33   <2e-16 ***
#   logprnd      -0.024582   0.002349 727.088149  -10.46   <2e-16 ***
#   loglen       -0.249814   0.002993 676.828055  -83.46   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgprnd
# logprnd -0.488       
# loglen  -0.574  0.059

# -------------- Round, context = dialogue --------------
m <- lmer(loghdial ~ 1 + logprnd + loglen + (1 + logprnd + loglen | dialogue_id), pb)
summary(m)
# Random effects:
#   Groups      Name        Variance  Std.Dev. Corr       
# dialogue_id (Intercept) 0.0274227 0.16560             
# logprnd     0.0009717 0.03117  -0.18      
# loglen      0.0043449 0.06592  -0.48 -0.07
# Residual                0.1377465 0.37114             
# Number of obs: 49012, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   1.900867   0.008248 591.712933 230.453  < 2e-16 ***
#   logprnd       0.017189   0.002510 710.192673   6.849 1.61e-11 ***
#   loglen       -0.247183   0.003131 676.053686 -78.958  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgprnd
# logprnd -0.532       
# loglen  -0.557  0.064

# -------------- Chain, context = chain --------------
m <- lmer(loghchain ~ 1 + logpchain + loglen + (1 + logpchain + loglen | dialogue_id), chains)
summary(m)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.10708  0.3272              
# logpchain   0.01092  0.1045   -0.60      
# loglen      0.01790  0.1338   -0.93  0.64
# Residual                0.10140  0.3184              
# Number of obs: 14431, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   2.184723   0.019786 572.955679  110.42   <2e-16 ***
#   logpchain    -0.135605   0.006950 716.058581  -19.51   <2e-16 ***
#   loglen       -0.295868   0.007947 618.967756  -37.23   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgpchn
# logpchain -0.570       
# loglen    -0.950  0.470

# -------------- Chain, context = dialogue --------------
m <- lmer(loghdial ~ 1 + logpchain + loglen + (1 + logpchain + loglen | dialogue_id), chains)
summary(m)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.119056 0.34505             
# logpchain   0.007557 0.08693  -0.60      
# loglen      0.017471 0.13218  -0.92  0.62
# Residual                0.104419 0.32314             
# Number of obs: 14431, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   2.008682   0.020415 582.845902   98.39  < 2e-16 ***
#   logpchain    -0.047542   0.006687 710.103623   -7.11 2.83e-12 ***
#   loglen       -0.250182   0.007995 627.285801  -31.29  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgpchn
# logpchain -0.562       
# loglen    -0.946  0.447


# ==================== Mutual information ====================

# -------------- Dialogue --------------
m <- lmer(mi_dialogue_id ~ 1 + logpdial + loglen + (1 + logpdial + loglen | dialogue_id), pb)
summary(m)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.27801  0.5273              
# logpdial    0.01616  0.1271   -0.59      
# loglen      0.03764  0.1940   -0.96  0.33
# Residual                0.76071  0.8722              
# Number of obs: 49012, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  -1.075671   0.026109 775.455745  -41.20   <2e-16 ***
#   logpdial      0.275803   0.006478 811.835351   42.57   <2e-16 ***
#   loglen        0.352345   0.008505 727.855161   41.43   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) logpdl
# logpdial -0.728       
# loglen   -0.790  0.258

# -------------- Round, context = round --------------
m <- lmer(mi_round_number ~ 1 + logprnd + loglen + (1 + logprnd + loglen | dialogue_id), pb)
summary(m)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.229251 0.47880             
# logprnd     0.002602 0.05101  -0.36      
# loglen      0.043118 0.20765  -0.98  0.33
# Residual                0.546110 0.73899             
# Number of obs: 49012, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  -0.816199   0.020786 743.665713  -39.27   <2e-16 ***
#   logprnd       0.113558   0.004799 714.609138   23.66   <2e-16 ***
#   loglen        0.391400   0.008563 740.863544   45.71   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgprnd
# logprnd -0.503       
# loglen  -0.899  0.178

# -------------- Round, context = dialogue --------------
m <- lmer(mi_dialogue_id ~ 1 + logprnd + loglen + (1 + logprnd + loglen | dialogue_id), pb)
summary(m)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.231588 0.48124             
# logprnd     0.004378 0.06617  -0.35      
# loglen      0.037380 0.19334  -0.84 -0.17
# Residual                0.824702 0.90813             
# Number of obs: 49012, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   0.051852   0.022238 748.637400   2.332     0.02 *  
#   logprnd      -0.093519   0.005866 745.544142 -15.944   <2e-16 ***
#   loglen        0.289325   0.008574 728.957734  33.743   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgprnd
# logprnd -0.541       
# loglen  -0.777  0.012

# -------------- Chain, context = chain --------------
m <- lmer(mi_chains_id ~ 1 + logpchain + loglen + (1 + logpchain + loglen | dialogue_id), chains)
summary(m)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.16222  0.4028              
# logpchain   0.10430  0.3230    0.52      
# loglen      0.02838  0.1685   -1.00 -0.54
# Residual                0.48632  0.6974              
# Number of obs: 14431, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)  -0.08611    0.03579 686.37733  -2.406   0.0164 *  
#   logpchain     0.80325    0.01717 898.99201  46.788   <2e-16 ***
#   loglen        0.03650    0.01428 667.14084   2.556   0.0108 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgpchn
# logpchain -0.211       
# loglen    -0.966  0.071

# -------------- Chain, context = dialogue --------------
m <- lmer(mi_dial ~ 1 + logpchain + loglen + (1 + logpchain + loglen | dialogue_id), chains)
summary(m)
# Random effects:
#   Groups      Name        Variance Std.Dev. Corr       
# dialogue_id (Intercept) 0.65799  0.8112              
# logpchain   0.07891  0.2809    0.00      
# loglen      0.09893  0.3145   -1.00 -0.01
# Residual                0.50201  0.7085              
# Number of obs: 14431, groups:  dialogue_id, 750
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   0.63504    0.04535 706.64347  14.004   <2e-16 ***
#   logpchain     0.45938    0.01642 837.23660  27.977   <2e-16 ***
#   loglen       -0.15490    0.01772 731.41689  -8.743   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) lgpchn
# logpchain -0.320       
# loglen    -0.977  0.210
