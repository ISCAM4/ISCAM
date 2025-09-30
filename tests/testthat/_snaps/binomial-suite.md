# iscambinomprob returns expected tail probabilities

    Code
      cat("lower_value:\n")
    Output
      lower_value:
    Code
      print(res_lower$value)
    Output
      [1] 0.3822806
    Code
      cat("lower_expected:", pbinom(3, 10, 0.4), "\n")
    Output
      lower_expected: 0.3822806 
    Code
      cat("lower_output:\n")
    Output
      lower_output:
    Code
      cat(collapse_output(res_lower$output), "\n")
    Output
      Probability 3 and below = 0.3822806  
    Code
      cat("---\n")
    Output
      ---
    Code
      cat("upper_value:\n")
    Output
      upper_value:
    Code
      print(res_upper$value)
    Output
      [1] 0.05476188
    Code
      cat("upper_expected:", 1 - pbinom(6, 10, 0.4), "\n")
    Output
      upper_expected: 0.05476188 
    Code
      cat("upper_output:\n")
    Output
      upper_output:
    Code
      cat(collapse_output(res_upper$output), "\n")
    Output
      Probability 7 and above = 0.05476188  

# iscaminvbinom solves the correct quantile

    Code
      cat("lower_value:\n")
    Output
      lower_value:
    Code
      print(res_lower$value)
    Output
      [1] 4
    Code
      cat("lower_expected:", qbinom(0.1, 20, 0.4, lower.tail = TRUE) - 1, "\n")
    Output
      lower_expected: 4 
    Code
      cat("lower_output:\n")
    Output
      lower_output:
    Code
      cat(collapse_output(res_lower$output), "\n")
    Output
      The observation with at most 0.1 probability at or below is 4  
    Code
      cat("---\n")
    Output
      ---
    Code
      cat("upper_value:\n")
    Output
      upper_value:
    Code
      print(res_upper$value)
    Output
      [1] 12
    Code
      cat("upper_expected:", qbinom(0.1, 20, 0.4, lower.tail = FALSE) + 1, "\n")
    Output
      upper_expected: 12 
    Code
      cat("upper_output:\n")
    Output
      upper_output:
    Code
      cat(collapse_output(res_upper$output), "\n")
    Output
      The observation with at most 0.1 probability at or above is 12  

# iscambinomnorm executes for each direction

    Code
      cat("below:\n")
    Output
      below:
    Code
      cat(collapse_output(capture_plot_result(iscambinomnorm(10, 20, 0.5, "below"))$
        output), "\n")
    Output
       binomial: 0.5881 
       normal approx: 0.5 
       normal approx with continuity: 0.5885  
    Code
      cat("---\n")
    Output
      ---
    Code
      cat("above:\n")
    Output
      above:
    Code
      cat(collapse_output(capture_plot_result(iscambinomnorm(10, 20, 0.5, "above"))$
        output), "\n")
    Output
       binomial: 0.5881 
       normal approx: 0.5 
       normal approx with continuity: 0.5885  
    Code
      cat("---\n")
    Output
      ---
    Code
      cat("two-sided:\n")
    Output
      two-sided:
    Code
      cat(collapse_output(capture_plot_result(iscambinomnorm(10, 20, 0.5, "two.sided"))$
        output), "\n")
    Output
       binomial: 1.176 
       normal approx: 1 
       normal approx with continuity: 1.177  

# iscambinompower reports rejection probabilities

    Code
      cat("critical_rr:", rr, "\n")
    Output
      critical_rr: 15 
    Code
      cat("null_prob:", null_prob, "\n")
    Output
      null_prob: 0.02069473 
    Code
      cat("alt_prob:", alt_prob, "\n")
    Output
      alt_prob: 0.125599 
    Code
      cat("value:\n")
    Output
      value:
    Code
      print(res$value)
    Output
      NULL
    Code
      cat("output:\n")
    Output
      output:
    Code
      cat(collapse_output(res$output), "\n")
    Output
      Null: Probability 15 and above = 0.02069473 
      Alternative: Probability 15 and above = 0.125599  

# iscambinomtest matches binom.test results

    Code
      cat("value:\n")
    Output
      value:
    Code
      print(res$value)
    Output
      $pvalue
      [1] 0.36159
      
      $lower
      [1] 0.40603
      
      $upper
      [1] 0.77344
      
    Code
      cat("expected_p: ", signif(bt$p.value, 5), "\n", sep = "")
    Output
      expected_p: 0.36159
    Code
      cat("expected_ci:", bt$conf.int[1], bt$conf.int[2], "\n")
    Output
      expected_ci: 0.4060349 0.7734424 
    Code
      cat("output:\n")
    Output
      output:
    Code
      cat(collapse_output(res$output), "\n")
    Output
      
      Exact Binomial Test
      
      Data: observed successes = 18, sample size = 30, sample proportion = 0.6
      
      Null hypothesis       : pi = 0.5 
      Alternative hypothesis: pi <> 0.5 
      p-value: 0.36159 
      95 % Confidence interval for pi: ( 0.40603 , 0.77344 )  

