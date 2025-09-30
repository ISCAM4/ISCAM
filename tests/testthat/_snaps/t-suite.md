# iscaminvt reports requested t quantiles

    Code
      cat("below_answer:\n")
    Output
      below_answer:
    Code
      print(res_below$value)
    Output
      $answer
      [1] -1.753
      
    Code
      cat("expected_below:", qb, "\n")
    Output
      expected_below: -1.75305 
    Code
      cat("below_output:\n")
    Output
      below_output:
    Code
      cat(collapse_output(res_below$output), "\n")
    Output
      The observation with 0.05 probability below is -1.753  
    Code
      cat("---\n")
    Output
      ---
    Code
      cat("outside_answer:\n")
    Output
      outside_answer:
    Code
      print(res_outside$value)
    Output
      $answer1
      [1] -1.812
      
      $answer2
      [1] 1.812
      
    Code
      cat("expected_outside:", ql, qu, "\n")
    Output
      expected_outside: -1.812461 1.812461 
    Code
      cat("outside_output:\n")
    Output
      outside_output:
    Code
      cat(collapse_output(res_outside$output), "\n")
    Output
      There is 0.1 probability outside -1.812 and 1.812  

# iscamtprob matches t tail probabilities

    Code
      cat("below_value:\n")
    Output
      below_value:
    Code
      print(res_below$value)
    Output
      NULL
    Code
      cat("below_expected:", pt(-2.05, 10), "\n")
    Output
      below_expected: 0.03375415 
    Code
      cat("below_output:\n")
    Output
      below_output:
    Code
      cat(collapse_output(res_below$output), "\n")
    Output
      probability: 0.03375  
    Code
      cat("---\n")
    Output
      ---
    Code
      cat("between_value:\n")
    Output
      between_value:
    Code
      print(res_between$value)
    Output
      NULL
    Code
      cat("between_expected:", pt(2, 12) - pt(-2, 12), "\n")
    Output
      between_expected: 0.931345 
    Code
      cat("between_output:\n")
    Output
      between_output:
    Code
      cat(collapse_output(res_between$output), "\n")
    Output
      probability: 0.9313  

# iscamonesamplet returns Welch statistics

    Code
      cat("value:\n")
    Output
      value:
    Code
      print(res$value)
    Output
      NULL
    Code
      cat("expected_t:", t_expected, "\n")
    Output
      expected_t: 2.282177 
    Code
      cat("expected_p:", p_expected, "\n")
    Output
      expected_p: 0.01499991 
    Code
      cat("expected_ci:", lower, upper, "\n")
    Output
      expected_ci: 2.051913 2.948087 
    Code
      cat("output:\n")
    Output
      output:
    Code
      cat(collapse_output(res$output), "\n")
    Output
      
      One Sample t test
      
      mean = 2.5, sd = 1.2,  sample size = 30
      Null hypothesis       : mu = 2 
      Alternative hypothesis: mu > 2 
      t-statistic: 2.282 
      95 % Confidence interval for mu: ( 2.051913 ,  2.948087 ) 
      p-value: 0.01499991  

# iscamtwosamplet returns Welch two-sample results

    Code
      cat("value:\n")
    Output
      value:
    Code
      print(res$value)
    Output
      NULL
    Code
      cat("expected_df:", df_expected, "\n")
    Output
      expected_df: 53.58 
    Code
      cat("expected_t:", t_expected, "\n")
    Output
      expected_t: 2.163254 
    Code
      cat("expected_p:", p_expected, "\n")
    Output
      expected_p: 0.03500158 
    Code
      cat("expected_ci:", lower, upper, "\n")
    Output
      expected_ci: 0.07304471 1.926955 
    Code
      cat("output:\n")
    Output
      output:
    Code
      cat(collapse_output(res$output), "\n")
    Output
      
      Two Sample t test
      
      Group1: mean = 5, sd = 2,  sample size = 30
      Group2: mean = 4, sd = 1.5,  sample size = 28
      diff:1
      
      Null hypothesis       : mu1-mu2 = 0 
      Alternative hypothesis: mu1-mu2 <> 0 
      t-statistic: 2.163 
      df: 53.58 
      95 % Confidence interval for mu1-mu2: ( 0.07304471 ,  1.926955 ) 
      p-value: 0.035  

