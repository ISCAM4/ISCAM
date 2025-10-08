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

---

    Code
      res_above$output
    Output
      [1] "[1] 2.5"               "[1] 5"                 "probability: 0.01572 "

---

    Code
      res_outside$output
    Output
      [1] "probability: 0.06866 "

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

---

    Code
      res_less$output
    Output
      [1] ""                                                          
      [2] "One Sample t test"                                         
      [3] ""                                                          
      [4] "mean = 1.5, sd = 1.2,  sample size = 30"                   
      [5] "Null hypothesis       : mu = 2 "                           
      [6] "Alternative hypothesis: mu < 2 "                           
      [7] "t-statistic: -2.282 "                                      
      [8] "95 % Confidence interval for mu: ( 1.051913 ,  1.948087 ) "
      [9] "p-value: 0.01499991 "                                      

---

    Code
      res_two$output
    Output
      [1] ""                                                          
      [2] "One Sample t test"                                         
      [3] ""                                                          
      [4] "mean = 2.5, sd = 1.2,  sample size = 30"                   
      [5] "Null hypothesis       : mu = 2 "                           
      [6] "Alternative hypothesis: mu <> 2 "                          
      [7] "t-statistic: 2.282 "                                       
      [8] "95 % Confidence interval for mu: ( 2.051913 ,  2.948087 ) "
      [9] "p-value: 0.02999982 "                                      

---

    Code
      res_conf_only$output
    Output
      [1] ""                                                          
      [2] "One Sample t test"                                         
      [3] ""                                                          
      [4] "mean = 2.5, sd = 1.2,  sample size = 30"                   
      [5] "95 % Confidence interval for mu: ( 2.051913 ,  2.948087 ) "

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

---

    Code
      res_less$output
    Output
       [1] ""                                                                 
       [2] "Two Sample t test"                                                
       [3] ""                                                                 
       [4] "Group1: mean = 5, sd = 2,  sample size = 30"                      
       [5] "Group2: mean = 4, sd = 1.5,  sample size = 28"                    
       [6] "diff:1"                                                           
       [7] ""                                                                 
       [8] "Null hypothesis       : mu1-mu2 = 0 "                             
       [9] "Alternative hypothesis: mu1-mu2 < 0 "                             
      [10] "t-statistic: 2.163 "                                              
      [11] "df: 53.58 "                                                       
      [12] "95 % Confidence interval for mu1-mu2: ( 0.07304471 ,  1.926955 ) "
      [13] "p-value: 0.9825 "                                                 

---

    Code
      res_greater$output
    Output
       [1] ""                                                                 
       [2] "Two Sample t test"                                                
       [3] ""                                                                 
       [4] "Group1: mean = 5, sd = 2,  sample size = 30"                      
       [5] "Group2: mean = 4, sd = 1.5,  sample size = 28"                    
       [6] "diff:1"                                                           
       [7] ""                                                                 
       [8] "Null hypothesis       : mu1-mu2 = 0 "                             
       [9] "Alternative hypothesis: mu1-mu2 > 0 "                             
      [10] "t-statistic: 2.163 "                                              
      [11] "df: 53.58 "                                                       
      [12] "95 % Confidence interval for mu1-mu2: ( 0.07304471 ,  1.926955 ) "
      [13] "p-value: 0.0175 "                                                 

---

    Code
      res_conf_only$output
    Output
      [1] ""                                                                 
      [2] "Two Sample t test"                                                
      [3] ""                                                                 
      [4] "Group1: mean = 5, sd = 2,  sample size = 30"                      
      [5] "Group2: mean = 4, sd = 1.5,  sample size = 28"                    
      [6] "diff:1"                                                           
      [7] ""                                                                 
      [8] "95 % Confidence interval for mu1-mu2: ( 0.07304471 ,  1.926955 ) "

