# iscamsummary prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "Some Summary Statistics\n\nDescription:\n\n     'summary' calculates the five number summary, mean, and standard\n     deviation of the quantitative variable 'x'. An optional second,\n     categorical variable can be specified and values will be\n     calculated separately for each group. The number of digits in\n     output can also be specified. Skewness is sample skewness: g_1 :=\n     \\frac{m_3}{m_2^{3/2}}, where m_2 := \\frac{1}{n}sum_{i=1}^{n}(x_i -\n     \\bar{x})^2 and m_3 := \\frac{1}{n}sum_{i=1}^{n}(x_i - \\bar{x})^3\n     are the second and third central sample moments."

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     x: data to summarize.\n\n     explanatory: optional explanatory variable to group by.\n\n     digits: number of digits to round to.\n"

