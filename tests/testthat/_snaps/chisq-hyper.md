# iscamchisqprob returns formatted upper-tail probability

    Code
      cat("value:\n")
    Output
      value:
    Code
      print(res$value)
    Output
      [1] "0.1718"
    Code
      cat("expected:", pchisq(5, 3, lower.tail = FALSE), "\n")
    Output
      expected: 0.1717971 
    Code
      cat("output:\n")
    Output
      output:
    Code
      cat(collapse_output(res$output), "\n")
    Output
      probability: 0.1718  

# iscamhyperprob matches hypergeometric tails

    Code
      cat("lower_value:\n")
    Output
      lower_value:
    Code
      print(res_lower$value)
    Output
      [1] 0.7038184
    Code
      cat("lower_expected:", phyper(2, 5, fail, 8), "\n")
    Output
      lower_expected: 0.7038184 
    Code
      cat("lower_output:\n")
    Output
      lower_output:
    Code
      cat(collapse_output(res_lower$output), "\n")
    Output
      Probability 2 and below = 0.7038184  
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
      [1] 0.2961816
    Code
      cat("upper_expected:", 1 - phyper(2, 5, fail, 8), "\n")
    Output
      upper_expected: 0.2961816 
    Code
      cat("upper_output:\n")
    Output
      upper_output:
    Code
      cat(collapse_output(res_upper$output), "\n")
    Output
      Probability 3 and above = 0.2961816  

# iscamhypernorm reports tail probabilities and normal approximations

    Code
      cat("lower_value:\n")
    Output
      lower_value:
    Code
      print(res_lower$value)
    Output
      NULL
    Code
      cat("lower_output:\n")
    Output
      lower_output:
    Code
      cat(collapse_output(res_lower$output), "\n")
    Output
       hypergeometric: 0.7038 
       normal approx: 0.5 
       normal approx with continuity: 0.6963  
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
      NULL
    Code
      cat("upper_output:\n")
    Output
      upper_output:
    Code
      cat(collapse_output(res_upper$output), "\n")
    Output
       hypergeometric: 0.2962 
       normal approx: 0.1521 
       normal approx with continuity: 0.3037  

