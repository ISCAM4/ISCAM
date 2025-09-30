# iscamnormprob returns formatted probabilities

    Code
      cat("above_value:\n")
    Output
      above_value:
    Code
      print(res_above$value)
    Output
      [1] "0.025"
    Code
      cat("above_expected:", pnorm(1.96, lower.tail = FALSE), "\n")
    Output
      above_expected: 0.0249979 
    Code
      cat("above_output:\n")
    Output
      above_output:
    Code
      cat(collapse_output(res_above$output), "\n")
    Output
      probability: 0.025  
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
      [1] "0.6827"
    Code
      cat("between_expected:", pnorm(1) - pnorm(-1), "\n")
    Output
      between_expected: 0.6826895 
    Code
      cat("between_output:\n")
    Output
      between_output:
    Code
      cat(collapse_output(res_between$output), "\n")
    Output
      probability: 0.6827  

# iscamnormpower reports null and alternative rejection rates

    Code
      cat("rr:", rr, "\n")
    Output
      rr: 0.5919501 
    Code
      cat("null_prob:", null_prob, "\n")
    Output
      null_prob: 0.05 
    Code
      cat("alt_prob:", alt_prob, "\n")
    Output
      alt_prob: 0.2253625 
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
      Null: Probability 0.592 and above = 0.05 
      Alt: Probability 0.592 and above = 0.2253625  

# iscaminvnorm reports requested quantiles

    Code
      cat("below_value:\n")
    Output
      below_value:
    Code
      print(res_below$value)
    Output
      NULL
    Code
      cat("below_output:\n")
    Output
      below_output:
    Code
      cat(collapse_output(res_below$output), "\n")
    Output
      The observation with 0.05 probability below is -1.645  
    Code
      cat("---\n")
    Output
      ---
    Code
      cat("outside_value:\n")
    Output
      outside_value:
    Code
      print(res_outside$value)
    Output
      NULL
    Code
      cat("outside_output:\n")
    Output
      outside_output:
    Code
      cat(collapse_output(res_outside$output), "\n")
    Output
      There is 0.1 probability outside -1.645 and 1.645  

