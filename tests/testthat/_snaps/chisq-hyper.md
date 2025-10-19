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

