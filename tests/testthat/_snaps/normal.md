# iscamnormprob returns formatted probabilities

    Code
      res_above$output
    Output
      [1] "probability: 0.025 "

---

    Code
      res_between$output
    Output
      [1] "probability: 0.6827 "

# iscamnormprob handles below and outside directions

    Code
      res_below$output
    Output
      [1] "probability: 0.3085 "

---

    Code
      res_outside$output
    Output
      [1] "probability: 0.3935 "

# iscamnormpower reports null and alternative rejection rates

    Code
      res$output
    Output
      [1] "Null: Probability 0.592 and above = 0.05 "    
      [2] "Alt: Probability 0.592 and above = 0.2253625 "

# iscaminvnorm reports requested quantiles

    Code
      res_below$output
    Output
      [1] "The observation with 0.05 probability below is -1.645 "

---

    Code
      res_outside$output
    Output
      [1] "There is 0.1 probability outside -1.645 and 1.645 "

# iscamnormpower handles less and two-sided alternatives

    Code
      res_less$output
    Output
      [1] "Null: Probability 0.3677 and below = 0.1 "     
      [2] "Alt: Probability 0.3677 and below = 0.3047272 "

---

    Code
      res_two$output
    Output
      [1] "Null: Probability in rejection region 0.05 " 
      [2] "Alt: Probability in rejection region 0.1549 "

# iscaminvnorm supports legacy Sd values

    Code
      res_above$output
    Output
      [1] "The observation with 0.2 probability above is 3.262 "

---

    Code
      res_between$output
    Output
      [1] "There is 0.9 probability between -0.3159 and 2.3159 "

# iscamnormprob prints help for question mark

    Code
      collapse_output(help_lines)
    Output
      [1] "Arguments:\n\n     xval: observed value.\n\n     mean: mean of normal distribution.\n\n     sd: standard deviation of normal distribution.\n\n     direction: direction for probability calculation, \"above\" or \"below\";\n     if \"outside\" or \"between\" are used, a second larger\n     observation, 'xval2' must be specified\n\n     label: horizontal axis label.\n\n     xval2: second observation value.\n\n     digits: number of digits to display.\n\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages\n"

# iscaminvnorm prints help for question mark

    Code
      collapse_output(help_lines)
    Output
      [1] "Arguments:\n\n     prob1: probability to find normal quantile of.\n\n     mean: mean of normal distribution.\n\n     sd: standard deviation of normal distribution.\n\n     Sd: deprecated-available for backwards compatibility.\n\n     direction: direction for probability calculation: \"above\", \"below\",\n     \"outside\", \"between\".\n\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages\n"

# iscamnormpower prints help for question mark

    Code
      collapse_output(help_lines)
    Output
      [1] "Arguments:\n\n     LOS: A numeric value representing the level of significance; 0 <\n     'LOS'< 1\n\n     n: A numeric value representing the sample size\n\n     prob1: A numeric value representing the first probability\n\n     alternative: \"less\", \"greater\", or \"two.sided\"\n\n     prob2: A numeric value representing the second probability\n\n     verbose: Logical, defaults to 'TRUE'. Set to 'FALSE' to suppress\n     messages\n"

