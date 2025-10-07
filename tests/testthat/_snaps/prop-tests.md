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

