# iscamboxplot works correctly with one variable

    Code
      res$output
    Output
      character(0)

# iscamboxplot works correctly with two variables

    Code
      res$output
    Output
      character(0)

# iscamdotplot works correctly with one variable

    Code
      res$output
    Output
      character(0)

# iscamdotplot works correctly with two variables

    Code
      res$output
    Output
      character(0)

# iscamboxplot prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "A box plot\n\nDescription:\n\n     'boxplot' plots the given data in a box plot. If a second\n     categorical variable is given, the data is grouped by this\n     variable.\n\nUsage:\n\n     iscamboxplot(\n       response,"

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     response: Vector of numeric values to plot.\n\n     explanatory: Optional second categorical variable to group by.\n\n     main: Optional title for the plot\n\n     xlab: Optional x-axis label for the plot\n\n     ylab: Optional y-axis label for the plot. Only displayed when\n     'explanatory' is provided.\n"

# iscamdotplot prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "A dot plot\n\nDescription:\n\n     'dotplot' creates a horizontal dot plot. If a second categorical\n     variable is given, the data is grouped by this variable. Use\n     'names' & 'mytitle' to specify the labels and title.\n\nUsage:\n\n     iscamdotplot(\n       response,"

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     response: Vector of numeric values to plot.\n\n     explanatory: Optional second categorical variable to group by.\n\n     main: Optional title for the plot\n\n     xlab: Optional x-axis label for the plot\n\n     ylab: Optional y-axis label for the plot. Only displayed when\n     'explanatory' is provided.\n"

