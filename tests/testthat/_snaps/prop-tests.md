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

---

    Code
      res_less$output
    Output
       [1] ""                                                                        
       [2] "One Proportion z test"                                                   
       [3] ""                                                                        
       [4] "Data: observed successes = 20, sample size = 50, sample proportion = 0.4"
       [5] ""                                                                        
       [6] "Null hypothesis       : pi = 0.5 "                                       
       [7] "Alternative hypothesis: pi < 0.5 "                                       
       [8] "z-statistic: -1.414 "                                                    
       [9] "p-value: 0.07865 "                                                       
      [10] "95 % Confidence interval for pi: ( 0.2642097 ,  0.5357903 ) "            

---

    Code
      res_two$output
    Output
       [1] ""                                                                        
       [2] "One Proportion z test"                                                   
       [3] ""                                                                        
       [4] "Data: observed successes = 35, sample size = 50, sample proportion = 0.7"
       [5] ""                                                                        
       [6] "Null hypothesis       : pi = 0.5 "                                       
       [7] "Alternative hypothesis: pi <> 0.5 "                                      
       [8] "z-statistic: 2.828 "                                                     
       [9] "p-value: 0.004678 "                                                      
      [10] "95 % Confidence interval for pi: ( 0.5729798 ,  0.8270202 ) "            

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

---

    Code
      res_less$output
    Output
       [1] ""                                                                             
       [2] "Two Proportion z test"                                                        
       [3] ""                                                                             
       [4] "Group1: observed successes = 20, sample size = 50, sample proportion = 0.4"   
       [5] ""                                                                             
       [6] "Group2: observed successes = 28, sample size = 45, sample proportion = 0.6222"
       [7] ""                                                                             
       [8] "Null hypothesis       : pi1-pi2 = 0 "                                         
       [9] "Alternative hypothesis: pi1-pi2 < 0 "                                         
      [10] "z-statistic: -2.163 "                                                         
      [11] "95 % Confidence interval for pi1-pi2: ( -0.4184497 ,  -0.02599474 ) "         
      [12] "p-value: 0.01527 "                                                            

---

    Code
      res_two$output
    Output
       [1] ""                                                                             
       [2] "Two Proportion z test"                                                        
       [3] ""                                                                             
       [4] "Group1: observed successes = 35, sample size = 50, sample proportion = 0.7"   
       [5] ""                                                                             
       [6] "Group2: observed successes = 28, sample size = 45, sample proportion = 0.6222"
       [7] ""                                                                             
       [8] "Null hypothesis       : pi1-pi2 = 0 "                                         
       [9] "Alternative hypothesis: pi1-pi2 <> 0 "                                        
      [10] "z-statistic: 0.8009 "                                                         
      [11] "95 % Confidence interval for pi1-pi2: ( -0.1124861 ,  0.2680417 ) "           
      [12] "p-value: 0.4232 "                                                             

