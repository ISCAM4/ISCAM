# iscambinomprob returns expected tail probabilities

    Code
      res_lower$output
    Output
      [1] "Probability 3 and below = 0.3822806 "

---

    Code
      res_upper$output
    Output
      [1] "Probability 7 and above = 0.05476188 "

# iscaminvbinom solves the correct quantile

    Code
      res_lower$output
    Output
      [1] "The observation with at most 0.1 probability at or below is 4 "

---

    Code
      res_upper$output
    Output
      [1] "The observation with at most 0.1 probability at or above is 12 "

# iscambinomnorm executes for each direction

    Code
      capture_plot_result(iscambinomnorm(10, 20, 0.5, "below"))$output
    Output
      [1] " binomial: 0.5881 "                     
      [2] " normal approx: 0.5 "                   
      [3] " normal approx with continuity: 0.5885 "

---

    Code
      capture_plot_result(iscambinomnorm(10, 20, 0.5, "above"))$output
    Output
      [1] " binomial: 0.5881 "                     
      [2] " normal approx: 0.5 "                   
      [3] " normal approx with continuity: 0.5885 "

---

    Code
      capture_plot_result(iscambinomnorm(4, 20, 0.5, "two.sided"))$output
    Output
      [1] " binomial: 0.01182 "                     
      [2] " normal approx: 0.00729 "                
      [3] " normal approx with continuity: 0.01391 "

---

    Code
      capture_plot_result(iscambinomnorm(16, 20, 0.5, "two.sided"))$output
    Output
      [1] " binomial: 0.01182 "                     
      [2] " normal approx: 0.00729 "                
      [3] " normal approx with continuity: 0.01391 "

---

    Code
      capture_plot_result(iscambinomnorm(10, 20, 0.5, "two.sided"))$output
    Output
      [1] " binomial: 1.176 "                     
      [2] " normal approx: 1 "                    
      [3] " normal approx with continuity: 1.177 "

# iscambinompower reports rejection probabilities

    Code
      res$output
    Output
      [1] "Null: Probability 15 and above = 0.02069473 "     
      [2] "Alternative: Probability 15 and above = 0.125599 "

---

    Code
      res_less$output
    Output
      [1] "Null: Probability 5 and below = 0.02069473 "      
      [2] "Alternative: Probability 5 and below = 0.4163708 "

---

    Code
      res_two$output
    Output
      [1] "Null: Probability in rejection region 0.04139 "         
      [2] "Alternative: Probability in rejection region 0.1272105 "

# iscambinomtest matches binom.test results

    Code
      res$output
    Output
      [1] ""                                                                        
      [2] "Exact Binomial Test"                                                     
      [3] ""                                                                        
      [4] "Data: observed successes = 18, sample size = 30, sample proportion = 0.6"
      [5] ""                                                                        
      [6] "Null hypothesis       : pi = 0.5 "                                       
      [7] "Alternative hypothesis: pi <> 0.5 "                                      
      [8] "p-value: 0.36159 "                                                       
      [9] "95 % Confidence interval for pi: ( 0.40603 , 0.77344 ) "                 

---

    Code
      res_less$output
    Output
      [1] ""                                                                          
      [2] "Exact Binomial Test"                                                       
      [3] ""                                                                          
      [4] "Data: observed successes = 8, sample size = 30, sample proportion = 0.2667"
      [5] ""                                                                          
      [6] "Null hypothesis       : pi = 0.5 "                                         
      [7] "Alternative hypothesis: pi < 0.5 "                                         
      [8] "p-value: 0.0080624 "                                                       
      [9] "95 % Confidence interval for pi: ( 0.12279 , 0.45889 ) "                   

---

    Code
      res_greater$output
    Output
      [1] ""                                                                           
      [2] "Exact Binomial Test"                                                        
      [3] ""                                                                           
      [4] "Data: observed successes = 22, sample size = 30, sample proportion = 0.7333"
      [5] ""                                                                           
      [6] "Null hypothesis       : pi = 0.5 "                                          
      [7] "Alternative hypothesis: pi > 0.5 "                                          
      [8] "p-value: 0.0080624 "                                                        
      [9] "95 % Confidence interval for pi: ( 0.54111 , 0.87721 ) "                    

---

    Code
      res_conf_only$output
    Output
      [1] ""                                                                        
      [2] "Exact Binomial Test"                                                     
      [3] ""                                                                        
      [4] "Data: observed successes = 18, sample size = 30, sample proportion = 0.6"
      [5] ""                                                                        
      [6] "95 % Confidence interval for pi: ( 0.40603 , 0.77344 ) "                 

---

    Code
      res_prop$output
    Output
      [1] ""                                                                        
      [2] "Exact Binomial Test"                                                     
      [3] ""                                                                        
      [4] "Data: observed successes = 18, sample size = 30, sample proportion = 0.6"
      [5] ""                                                                        
      [6] "Null hypothesis       : pi = 0.5 "                                       
      [7] "Alternative hypothesis: pi > 0.5 "                                       
      [8] "p-value: 0.1808 "                                                        

---

    Code
      res_multi_conf$output
    Output
      [1] ""                                                                        
      [2] "Exact Binomial Test"                                                     
      [3] ""                                                                        
      [4] "Data: observed successes = 18, sample size = 30, sample proportion = 0.6"
      [5] ""                                                                        
      [6] "90 % Confidence interval for pi: ( 0.43395 , 0.75047 ) "                 
      [7] "95 % Confidence interval for pi: ( 0.40603 , 0.77344 ) "                 
      [8] "99 % Confidence interval for pi: ( 0.35302 , 0.81497 ) "                 

