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
      [1] "probability: 0.0606 "

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

# iscamonesamplet prints help for question mark

    Code
      collapse_output(help_lines)
    Output
      [1] "Arguments:\n     xbar: Observed mean.\n     sd: Observed standard deviation.\n     n: Sample size.\n     hypothesized: Hypothesized population mean.\n     alternative: \"less\", \"greater\", or \"two.sided\"\n     conf.level: Confidence level.\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages"

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

# iscaminvt prints help for question mark

    Code
      collapse_output(help_lines)
    Output
      [1] "Arguments:\n     prob: Desired probability.\n     df: Degrees of freedom\n     direction: direction for probability calculation: \"above\", \"below\",\n     \"outside\", \"between\".\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages"

# iscamtwosamplet prints help for question mark

    Code
      collapse_output(help_lines)
    Output
      [1] "Arguments:\n     x1: Observed mean for group 1.\n     sd1: Observed standard deviation for group 1.\n     n1: Sample size for group 1.\n     x2: Observed mean for group 2.\n     sd2: Observed standard deviation for group 2.\n     n2: Sample size for group 2.\n     hypothesized: Hypothesized difference in population means.\n     alternative: \"less\", \"greater\", or \"two.sided\"\n     conf.level: Confidence level.\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages iscamtwosamplet( x1 = 25, sd1 = 5, n1 = 40, x2 = 22,\n     sd2 = 6, n2 = 45, alternative = \"greater\" ) iscamtwosamplet(\n     x1 = 10, sd1 = 2, n1 = 50, x2 = 12, sd2 = 2.5, n2 = 50,\n     alternative = \"two.sided\" ) iscamtwosamplet( x1 = 8, sd1 =\n     1.5, n1 = 30, x2 = 5, sd2 = 1.8, n2 = 33, alternative =\n     \"greater\", hypothesized = 2 ) iscamtwosamplet( x1 = 15, sd1 =\n     3, n1 = 25, x2 = 12, sd2 = 3.5, n2 = 28, conf.level = 0.95 )"

# iscamtprob prints help for question mark

    Code
      collapse_output(help_lines)
    Output
      [1] "Arguments:\n     xval: observed value.\n     df: degrees of freedom.\n     direction: direction for probability calculation, \"above\" or \"below\";\n     if \"outside\" or \"between\" are used, a second larger\n     observation, 'xval2' must be specified\n     xval2: second observation value.\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages"

