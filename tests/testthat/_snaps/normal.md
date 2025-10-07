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

