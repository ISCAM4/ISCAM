# iscaminvt handles above and between directions

    Code
      res_above$output
    Output
      [1] "The observation with 0.1 probability above is 1.356 "

---

    Code
      res_between$output
    Output
      [1] "There is 0.9 probability between -1.86 and 1.86 "

# iscamtprob matches t tail probabilities

    Code
      res_below$output
    Output
      [1] "probability: 0.03375 "

---

    Code
      res_between$output
    Output
      [1] "probability: 0.9313 "

# iscamtprob handles all branches and validates parameters

    Code
      res_above$output
    Output
      [1] "[1] 1.65"             "[1] 5"                "probability: 0.0606 "

---

    Code
      res_outside$output
    Output
      [1] "probability: 0.1554 "

# iscamonesamplet returns Welch statistics

    Code
      res$output
    Output
      [1] ""                                                          
      [2] "One Sample t test"                                         
      [3] ""                                                          
      [4] "mean = 2.5, sd = 1.2,  sample size = 30"                   
      [5] "Null hypothesis       : mu = 2 "                           
      [6] "Alternative hypothesis: mu > 2 "                           
      [7] "t-statistic: 2.282 "                                       
      [8] "95 % Confidence interval for mu: ( 2.051913 ,  2.948087 ) "
      [9] "p-value: 0.01499991 "                                      

# iscamonesamplet handles one-sided and two-sided hypotheses

    Code
      res_less$output
    Output
      [1] ""                                       
      [2] "One Sample t test"                      
      [3] ""                                       
      [4] "mean = 4.8, sd = 1.1,  sample size = 22"
      [5] "Null hypothesis       : mu = 5.2 "      
      [6] "Alternative hypothesis: mu < 5.2 "      
      [7] "t-statistic: -1.706 "                   
      [8] "p-value: 0.05141529 "                   

---

    Code
      res_two$output
    Output
      [1] ""                                       
      [2] "One Sample t test"                      
      [3] ""                                       
      [4] "mean = 7.1, sd = 1.5,  sample size = 18"
      [5] "Null hypothesis       : mu = 7 "        
      [6] "Alternative hypothesis: mu <> 7 "       
      [7] "t-statistic: 0.2828 "                   
      [8] "p-value: 0.7807125 "                    

# iscamonesamplet requires numeric summaries

    Code
      iscamonesamplet(xbar = "?", sd = 1, n = 10)
    Condition
      Error in `iscamonesamplet()`:
      ! iscamonesamplet(xbar, sd, n,  hypothesized=0, alternative = NULL, conf.level =0
       This function calculates a one sample t-test and/or interval from summary statistics. 
        Input the observed mean, standard deviation, and sample size 
       Input  hypothesized population mean (default is zero)  
       Optional: Input the form of alternative ("less", "greater", or "two.sided") 
       Optional: Input confidence level(s) for a two-sided confidence interval.
         

# iscamonesamplet returns confidence intervals without hypothesis

    Code
      res_conf$output
    Output
      [1] ""                                                            
      [2] "One Sample t test"                                           
      [3] ""                                                            
      [4] "mean = 0.62, sd = 0.2,  sample size = 15"                    
      [5] "95 % Confidence interval for mu: ( 0.5092437 ,  0.7307563 ) "
      [6] "99 % Confidence interval for mu: ( 0.4662765 ,  0.7737235 ) "

# iscamonesamplet reports single confidence interval without hypothesis

    Code
      res$output
    Output
      [1] ""                                                          
      [2] "One Sample t test"                                         
      [3] ""                                                          
      [4] "mean = 1.8, sd = 0.4,  sample size = 20"                   
      [5] "95 % Confidence interval for mu: ( 1.612794 ,  1.987206 ) "

# iscamtwosamplet returns Welch two-sample results

    Code
      res$output
    Output
       [1] ""                                                                 
       [2] "Two Sample t test"                                                
       [3] ""                                                                 
       [4] "Group1: mean = 5, sd = 2,  sample size = 30"                      
       [5] "Group2: mean = 4, sd = 1.5,  sample size = 28"                    
       [6] "diff:1"                                                           
       [7] ""                                                                 
       [8] "Null hypothesis       : mu1-mu2 = 0 "                             
       [9] "Alternative hypothesis: mu1-mu2 <> 0 "                            
      [10] "t-statistic: 2.163 "                                              
      [11] "df: 53.58 "                                                       
      [12] "95 % Confidence interval for mu1-mu2: ( 0.07304471 ,  1.926955 ) "
      [13] "p-value: 0.035 "                                                  

# iscamtwosamplet handles less alternative

    Code
      res$output
    Output
       [1] ""                                               
       [2] "Two Sample t test"                              
       [3] ""                                               
       [4] "Group1: mean = 4.5, sd = 1.2,  sample size = 18"
       [5] "Group2: mean = 5.1, sd = 1.4,  sample size = 20"
       [6] "diff:-0.6"                                      
       [7] ""                                               
       [8] "Null hypothesis       : mu1-mu2 = 0 "           
       [9] "Alternative hypothesis: mu1-mu2 < 0 "           
      [10] "t-statistic: -1.422 "                           
      [11] "df: 35.93 "                                     
      [12] "p-value: 0.08181 "                              

# iscamtwosamplet provides intervals without hypothesis test

    Code
      res$output
    Output
      [1] ""                                                                 
      [2] "Two Sample t test"                                                
      [3] ""                                                                 
      [4] "Group1: mean = 6.2, sd = 1.1,  sample size = 25"                  
      [5] "Group2: mean = 5.8, sd = 1.3,  sample size = 24"                  
      [6] "diff:0.4"                                                         
      [7] ""                                                                 
      [8] "95 % Confidence interval for mu1-mu2: ( -0.2942233 ,  1.094223 ) "

