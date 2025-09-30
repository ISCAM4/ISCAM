# iscamonepropztest agrees with prop.test

    Code
      cat("value:\n")
    Output
      value:
    Code
      print(res$value)
    Output
      $zvalue
      [1] 2.828427
      
      $pvalue
      [1] 0.002339
      
      $lower
      [1] 0.5729798
      
      $upper
      [1] 0.8270202
      
    Code
      cat("expected_z:", z_expected, "\n")
    Output
      expected_z: 2.828427 
    Code
      cat("expected_p:", p_expected, "\n")
    Output
      expected_p: 0.002339 
    Code
      cat("expected_ci:", lower, upper, "\n")
    Output
      expected_ci: 0.5729798 0.8270202 
    Code
      cat("output:\n")
    Output
      output:
    Code
      cat(collapse_output(res$output), "\n")
    Output
      
      One Proportion z test
      
      Data: observed successes = 35, sample size = 50, sample proportion = 0.7
      
      Null hypothesis       : pi = 0.5 
      Alternative hypothesis: pi > 0.5 
      z-statistic: 2.828 
      p-value: 0.002339 
      95 % Confidence interval for pi: ( 0.5729798 ,  0.8270202 )  

# iscamtwopropztest matches two-sample z test calculations

    Code
      cat("value:\n")
    Output
      value:
    Code
      print(res$value)
    Output
      $zvalue
      [1] 0.8008724
      
      $pvalue
      [1] 0.2116
      
      $lower
      [1] -0.1124861
      
      $upper
      [1] 0.2680417
      
    Code
      cat("expected_z:", z_expected, "\n")
    Output
      expected_z: 0.8008724 
    Code
      cat("expected_p:", p_expected, "\n")
    Output
      expected_p: 0.2116 
    Code
      cat("expected_ci:", lower, upper, "\n")
    Output
      expected_ci: -0.1124861 0.2680417 
    Code
      cat("output:\n")
    Output
      output:
    Code
      cat(collapse_output(res$output), "\n")
    Output
      
      Two Proportion z test
      
      Group1: observed successes = 35, sample size = 50, sample proportion = 0.7
      
      Group2: observed successes = 28, sample size = 45, sample proportion = 0.6222
      
      Null hypothesis       : pi1-pi2 = 0 
      Alternative hypothesis: pi1-pi2 > 0 
      z-statistic: 0.8009 
      95 % Confidence interval for pi1-pi2: ( -0.1124861 ,  0.2680417 ) 
      p-value: 0.2116  

