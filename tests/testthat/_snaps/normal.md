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

