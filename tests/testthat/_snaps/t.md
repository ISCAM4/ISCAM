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

# iscamonesamplet prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "One Sample T-Test\n\nDescription:\n\n     'onesamplet' calculates a one sample t-test and/or interval from\n     summary statistics. It defaults to a hypothesized population mean\n     of 0. You can optionally set an alternative hypothesis and\n     confidence level for a two-sided confidence interval.\n\nUsage:\n\n     iscamonesamplet("

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     xbar: Observed mean.\n\n     sd: Observed standard deviation.\n\n     n: Sample size.\n\n     hypothesized: Hypothesized population mean.\n\n     alternative: \"less\", \"greater\", or \"two.sided\"\n\n     conf.level: Confidence level.\n\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages\n"

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
      collapse_output(head(help_lines, 12))
    Output
      [1] "Inverse T Calculation\n\nDescription:\n\n     'invt' calculates the t quantile of a specified probability.\n\nUsage:\n\n     iscaminvt(prob, df, direction, verbose = TRUE)\n\nArguments:\n"

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     prob: Desired probability.\n\n     df: Degrees of freedom\n\n     direction: direction for probability calculation: \"above\", \"below\",\n     \"outside\", \"between\".\n\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages\n"

# iscamtwosamplet prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "Two Sample T-Test\n\nDescription:\n\n     'twosamplet' calculates a two sample t-test and/or interval from\n     summary data. It defaults to a hypothesized population mean\n     difference of 0. You can optionally set an alternative hypothesis\n     and confidence level for a two-sided confidence interval.\n\nUsage:\n\n     iscamtwosamplet("

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     x1: Observed mean for group 1.\n\n     sd1: Observed standard deviation for group 1.\n\n     n1: Sample size for group 1.\n\n     x2: Observed mean for group 2.\n\n     sd2: Observed standard deviation for group 2.\n\n     n2: Sample size for group 2.\n\n     hypothesized: Hypothesized difference in population means.\n\n     alternative: \"less\", \"greater\", or \"two.sided\"\n\n     conf.level: Confidence level.\n\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages iscamtwosamplet( x1 = 25, sd1 = 5, n1 = 40, x2 = 22,\n     sd2 = 6, n2 = 45, alternative = \"greater\" ) iscamtwosamplet(\n     x1 = 10, sd1 = 2, n1 = 50, x2 = 12, sd2 = 2.5, n2 = 50,\n     alternative = \"two.sided\" ) iscamtwosamplet( x1 = 8, sd1 =\n     1.5, n1 = 30, x2 = 5, sd2 = 1.8, n2 = 33, alternative =\n     \"greater\", hypothesized = 2 ) iscamtwosamplet( x1 = 15, sd1 =\n     3, n1 = 25, x2 = 12, sd2 = 3.5, n2 = 28, conf.level = 0.95 )\n"

# iscamtprob prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "Tail Probability for t-distribution\n\nDescription:\n\n     Tail Probability for t-distribution\n\nUsage:\n\n     iscamtprob(xval, df, direction, xval2 = NULL, verbose = TRUE)\n\nArguments:\n"

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     xval: observed value.\n\n     df: degrees of freedom.\n\n     direction: direction for probability calculation, \"above\" or \"below\";\n     if \"outside\" or \"between\" are used, a second larger\n     observation, 'xval2' must be specified\n\n     xval2: second observation value.\n\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages\n"

