# iscamonepropztest agrees with prop.test

    Code
      res$output
    Output
       [1] ""                                                                        
       [2] "One Proportion z test"                                                   
       [3] ""                                                                        
       [4] "Data: observed successes = 35, sample size = 50, sample proportion = 0.7"
       [5] ""                                                                        
       [6] "Null hypothesis       : pi = 0.5 "                                       
       [7] "Alternative hypothesis: pi > 0.5 "                                       
       [8] "z-statistic: 2.828 "                                                     
       [9] "p-value: 0.002339 "                                                      
      [10] "95 % Confidence interval for pi: ( 0.5729798 ,  0.8270202 ) "            

# iscamonepropztest handles less alternatives with proportion input

    Code
      res$output
    Output
       [1] ""                                                                         
       [2] "One Proportion z test"                                                    
       [3] ""                                                                         
       [4] "Data: observed successes = 16, sample size = 50, sample proportion = 0.32"
       [5] ""                                                                         
       [6] "Null hypothesis       : pi = 0.35 "                                       
       [7] "Alternative hypothesis: pi < 0.35 "                                       
       [8] "z-statistic: -0.4447 "                                                    
       [9] "p-value: 0.3283 "                                                         
      [10] "90 % Confidence interval for pi: ( 0.2114895 ,  0.4285105 ) "             

# iscamonepropztest covers two-sided comparisons when sample is below null

    Code
      res$output
    Output
      [1] ""                                                                        
      [2] "One Proportion z test"                                                   
      [3] ""                                                                        
      [4] "Data: observed successes = 18, sample size = 60, sample proportion = 0.3"
      [5] ""                                                                        
      [6] "Null hypothesis       : pi = 0.4 "                                       
      [7] "Alternative hypothesis: pi <> 0.4 "                                      
      [8] "z-statistic: -1.581 "                                                    
      [9] "p-value: 0.1138 "                                                        

# iscamonepropztest reports intervals when hypothesis is omitted

    Code
      res$output
    Output
      [1] ""                                                                          
      [2] "One Proportion z test"                                                     
      [3] ""                                                                          
      [4] "Data: observed successes = 28, sample size = 100, sample proportion = 0.28"
      [5] ""                                                                          
      [6] "90 % Confidence interval for pi: ( 0.2061463 ,  0.3538537 ) "              

# iscamtwopropztest matches two-sample z test calculations

    Code
      res$output
    Output
       [1] ""                                                                             
       [2] "Two Proportion z test"                                                        
       [3] ""                                                                             
       [4] "Group1: observed successes = 35, sample size = 50, sample proportion = 0.7"   
       [5] ""                                                                             
       [6] "Group2: observed successes = 28, sample size = 45, sample proportion = 0.6222"
       [7] ""                                                                             
       [8] "Null hypothesis       : pi1-pi2 = 0 "                                         
       [9] "Alternative hypothesis: pi1-pi2 > 0 "                                         
      [10] "z-statistic: 0.8009 "                                                         
      [11] "95 % Confidence interval for pi1-pi2: ( -0.1124861 ,  0.2680417 ) "           
      [12] "p-value: 0.2116 "                                                             

# iscamtwopropztest handles less alternative with proportion input

    Code
      res$output
    Output
       [1] ""                                                                             
       [2] "Two Proportion z test"                                                        
       [3] ""                                                                             
       [4] "Group1: observed successes = 25, sample size = 60, sample proportion = 0.4167"
       [5] ""                                                                             
       [6] "Group2: observed successes = 28, sample size = 55, sample proportion = 0.5091"
       [7] ""                                                                             
       [8] "Null hypothesis       : pi1-pi2 = 0 "                                         
       [9] "Alternative hypothesis: pi1-pi2 < 0 "                                         
      [10] "z-statistic: -0.9932 "                                                        
      [11] "90 % Confidence interval for pi1-pi2: ( -0.2449163 ,  0.06006778 ) "          
      [12] "p-value: 0.1603 "                                                             

# iscamtwopropztest covers two-sided alternatives below the null

    Code
      res$output
    Output
       [1] ""                                                                             
       [2] "Two Proportion z test"                                                        
       [3] ""                                                                             
       [4] "Group1: observed successes = 20, sample size = 60, sample proportion = 0.3333"
       [5] ""                                                                             
       [6] "Group2: observed successes = 28, sample size = 60, sample proportion = 0.4667"
       [7] ""                                                                             
       [8] "Null hypothesis       : pi1-pi2 = 0 "                                         
       [9] "Alternative hypothesis: pi1-pi2 <> 0 "                                        
      [10] "z-statistic: -1.491 "                                                         
      [11] "p-value: 0.136 "                                                              

# iscamtwopropztest accepts tabular input and confidence-only intervals

    Code
      res_alt$output
    Output
       [1] ""                                                                          
       [2] "Two Proportion z test"                                                     
       [3] ""                                                                          
       [4] "Group1: observed successes = 30, sample size = 50, sample proportion = 0.6"
       [5] ""                                                                          
       [6] "Group2: observed successes = 25, sample size = 50, sample proportion = 0.5"
       [7] ""                                                                          
       [8] "Null hypothesis       : pi1-pi2 = 0 "                                      
       [9] "Alternative hypothesis: pi1-pi2 > 0 "                                      
      [10] "z-statistic: 1.005 "                                                       
      [11] "p-value: 0.1574 "                                                          

---

    Code
      res_conf$output
    Output
      [1] ""                                                                             
      [2] "Two Proportion z test"                                                        
      [3] ""                                                                             
      [4] "Group1: observed successes = 62, sample size = 100, sample proportion = 0.62" 
      [5] ""                                                                             
      [6] "Group2: observed successes = 50, sample size = 90, sample proportion = 0.5556"
      [7] ""                                                                             
      [8] "90 % Confidence interval for pi1-pi2: ( -0.05301566 ,  0.1819045 ) "          

