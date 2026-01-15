# iscamchisqprob returns formatted upper-tail probability

    Code
      res$output
    Output
      [1] "probability: 0.1718 "

# iscamhyperprob matches hypergeometric tails

    Code
      res_lower$output
    Output
      [1] "Probability 2 and below = 0.7038184 "

---

    Code
      res_upper$output
    Output
      [1] "Probability 3 and above = 0.2961816 "

# iscamhypernorm reports tail probabilities and normal approximations

    Code
      res_lower$output
    Output
      [1] " hypergeometric: 0.7038 "               
      [2] " normal approx: 0.5 "                   
      [3] " normal approx with continuity: 0.6963 "

---

    Code
      res_upper$output
    Output
      [1] " hypergeometric: 0.2962 "               
      [2] " normal approx: 0.1521 "                
      [3] " normal approx with continuity: 0.3037 "

# iscamchisqprob prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "Chi-Square Probability\n\nDescription:\n\n     'chisqrprob' returns the upper tail probability for the given\n     chi-square statistic and degrees of freedom.\n\nUsage:\n\n     iscamchisqprob(xval, df, verbose = TRUE)\n\nArguments:"

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     xval: the value of the chi-square statistic.\n\n     df: the degrees of freedom.\n\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages\n"

# iscamhypernorm prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "Hypergeometric p-value and Distribution Overlaid with Normal\nDistribution\n\nDescription:\n\n     Hypergeometric p-value and Distribution Overlaid with Normal\n     Distribution\n\nUsage:\n\n     iscamhypernorm(k, total, succ, n, lower.tail, verbose = TRUE)\n"

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     k: Number of successes of interest or difference in conditional\n     proportions\n\n     total: Total number of observations in the study\n\n     succ: Overall number of successes\n\n     n: Number of observations in group A\n\n     lower.tail: Boolean for finding the probability above (FALSE) or below\n     (TRUE) the inputted value (inclusive)\n\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages\n"

# iscamhyperprob prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "Hypergeometric p-value and Distribution\n\nDescription:\n\n     Hypergeometric p-value and Distribution\n\nUsage:\n\n     iscamhyperprob(k, total, succ, n, lower.tail, verbose = TRUE)\n\nArguments:\n"

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     k: Number of successes of interest or difference in conditional\n     proportions\n\n     total: Total number of observations in the study\n\n     succ: Overall number of successes\n\n     n: Number of observations in group A\n\n     lower.tail: Boolean for finding the probability above (FALSE) or below\n     (TRUE) the inputted value (inclusive)\n\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages\n"

# iscamhypernorm converts fractional k inputs

    Code
      res$output
    Output
      [1] " hypergeometric: 0.9973 "               
      [2] " normal approx: 0.9901 "                
      [3] " normal approx with continuity: 0.9967 "

# iscamhyperprob matches fractional inputs for both tails

    Code
      res_lower$output
    Output
      [1] "Probability 6 and below = 0.9758352 "

---

    Code
      res_upper$output
    Output
      [1] "Probability 6 and above = 0.1182716 "

