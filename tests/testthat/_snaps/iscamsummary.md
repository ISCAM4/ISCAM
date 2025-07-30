# summary without explanatory works

    Code
      iscamsummary(fake_data)
    Output
        Missing  n   Min     Q1 Median    Q3   Max  Mean    SD Skewness
      1       0 30 -1.54 -0.621 -0.031 0.487 2.405 0.022 0.914    0.441

---

    Code
      iscamsummary(fake_data, digits = 5)
    Output
        Missing  n      Min       Q1   Median      Q3     Max    Mean      SD
      1       0 30 -1.53995 -0.62109 -0.03144 0.48663 2.40465 0.02195 0.91414
        Skewness
      1  0.44133

# summary with explanatory works

    Code
      iscamsummary(fake_data, groups)
    Output
             Missing  n    Min     Q1 Median    Q3   Max   Mean    SD Skewness
      group1       0 13 -1.238 -0.326 -0.057 0.436 1.263 -0.020 0.655    0.210
      group2       0 17 -1.540 -0.892  0.133 0.764 2.405  0.054 1.091    0.384

---

    Code
      iscamsummary(fake_data, groups)
    Output
             Missing  n    Min     Q1 Median    Q3   Max   Mean    SD Skewness
      group1       0 10 -1.540 -0.298 -0.148 0.676 2.405  0.173 1.103    0.582
      group2       0 10 -1.285 -1.093 -0.089 0.360 1.330 -0.207 0.919    0.217
      group3       0 10 -0.892 -0.390  0.098 0.472 1.272  0.101 0.736    0.216

