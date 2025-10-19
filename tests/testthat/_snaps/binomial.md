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

# iscambinomnorm handles two-sided alternatives

    Code
      res_lower$output
    Output
      [1] " binomial: 0.1153 "                     
      [2] " normal approx: 0.07364 "               
      [3] " normal approx with continuity: 0.1175 "

---

    Code
      res_upper$output
    Output
      [1] " binomial: 0.1153 "                     
      [2] " normal approx: 0.07364 "               
      [3] " normal approx with continuity: 0.1175 "

# iscambinompower reports rejection probabilities

    Code
      res$output
    Output
      [1] "Null: Probability 15 and above = 0.02069473 "     
      [2] "Alternative: Probability 15 and above = 0.125599 "

# iscambinompower handles less and two-sided cases

    Code
      res_less$output
    Output
      [1] "Null: Probability 5 and below = 0.0826247 "       
      [2] "Alternative: Probability 5 and below = 0.3782785 "

---

    Code
      res_two$output
    Output
      [1] "Null: Probability in rejection region 0.03027 "         
      [2] "Alternative: Probability in rejection region 0.1648196 "

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

# iscambinomtest handles one-sided alternatives

    Code
      res_less$output
    Output
      [1] ""                                                                       
      [2] "Exact Binomial Test"                                                    
      [3] ""                                                                       
      [4] "Data: observed successes = 6, sample size = 15, sample proportion = 0.4"
      [5] ""                                                                       
      [6] "Null hypothesis       : pi = 0.4 "                                      
      [7] "Alternative hypothesis: pi < 0.4 "                                      
      [8] "p-value: 0.60981 "                                                      

---

    Code
      res_greater$output
    Output
      [1] ""                                                                           
      [2] "Exact Binomial Test"                                                        
      [3] ""                                                                           
      [4] "Data: observed successes = 11, sample size = 18, sample proportion = 0.6111"
      [5] ""                                                                           
      [6] "Null hypothesis       : pi = 0.45 "                                         
      [7] "Alternative hypothesis: pi > 0.45 "                                         
      [8] "p-value: 0.12796 "                                                          

# iscambinomtest converts proportion inputs and multiple confidence levels

    Code
      res$output
    Output
      [1] ""                                                                          
      [2] "Exact Binomial Test"                                                       
      [3] ""                                                                          
      [4] "Data: observed successes = 14, sample size = 100, sample proportion = 0.14"
      [5] ""                                                                          
      [6] "90 % Confidence interval for pi: ( 0.086668 , 0.21017 ) "                  
      [7] "95 % Confidence interval for pi: ( 0.078705 , 0.22373 ) "                  

# iscambinomtest treats not.equal as two-sided

    Code
      res$output
    Output
      [1] ""                                                                           
      [2] "Exact Binomial Test"                                                        
      [3] ""                                                                           
      [4] "Data: observed successes = 13, sample size = 24, sample proportion = 0.5417"
      [5] ""                                                                           
      [6] "Null hypothesis       : pi = 0.5 "                                          
      [7] "Alternative hypothesis: pi  0.5 "                                           
      [8] "p-value: 0.83882 "                                                          

# iscambinomtest returns single confidence intervals without hypothesis

    Code
      res$output
    Output
      [1] ""                                                                         
      [2] "Exact Binomial Test"                                                      
      [3] ""                                                                         
      [4] "Data: observed successes = 18, sample size = 40, sample proportion = 0.45"
      [5] ""                                                                         
      [6] "90 % Confidence interval for pi: ( 0.31461 , 0.59119 ) "                  

