# iscamaddexp creates an exponential plot

    Code
      res$output
    Output
      character(0)

# iscamaddlnorm creates a log-normal plot

    Code
      res$output
    Output
      character(0)

# iscamaddnorm creates a normal plot

    Code
      res$output
    Output
      character(0)

# iscamaddt creates a t-distribution plot

    Code
      res$output
    Output
      character(0)

# iscamaddtnorm creates a t and normal plot

    Code
      res$output
    Output
      character(0)

# Plotting with custom parameters works

    Code
      res$output
    Output
      character(0)

# iscamaddexp prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "Overlay an Exponential Density Function on Histogram\n\nDescription:\n\n     'addexp' creates a histogram of 'x' and overlays an exponential\n     density function with lambda = \\frac{1}{mean}.\n\nUsage:\n\n     iscamaddexp(\n       x,\n       main = \"Histogram with exponential curve\","

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     x: A numeric vector representing the data to be plotted.\n\n     main: Optional title for the plot\n\n     xlab: Optional x-axis label for the plot\n\n     bins: Optional number of bins for the histogram.\n"

# iscamaddlnorm prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "Overlay a Log Normal Density Function on Histogram\n\nDescription:\n\n     'addlnorm' creates a histogram of 'x' and overlays a log normal\n     density function.\n\nUsage:\n\n     iscamaddlnorm(\n       x,\n       main = \"Histogram with log-normal curve\","

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     x: A numeric vector representing the data to be plotted.\n\n     main: Optional title for the plot\n\n     xlab: Optional x-axis label for the plot\n\n     bins: Optional number of bins for the histogram.\n"

# iscamaddnorm prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "Overlay a Normal Density Function on Histogram\n\nDescription:\n\n     'addnorm' creates a histogram of 'x' and overlays a normal density\n     function.\n\nUsage:\n\n     iscamaddnorm(\n       x,\n       main = \"Histogram with normal curve\","

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     x: A numeric vector representing the data to be plotted.\n\n     main: Optional title for the plot\n\n     xlab: Optional x-axis label for the plot\n\n     bins: Optional number of bins for the histogram.\n"

# iscamaddt prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "Overlay a t Density Function on Histogram\n\nDescription:\n\n     Overlay a t Density Function on Histogram\n\nUsage:\n\n     iscamaddt(\n       x,\n       df,\n       main = \"Histogram with t curve\","

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     x: A numeric vector representing the data to be plotted.\n\n     df: A numeric value representing the degrees of freedom of 'x'.\n\n     main: Optional title for the plot\n\n     xlab: Optional x-axis label for the plot\n\n     bins: Optional number of bins for the histogram.\n"

# iscamaddtnorm prints help for question mark

    Code
      collapse_output(head(help_lines, 12))
    Output
      [1] "Overlay a t Density Function and a Normal Density Function on Histogram\n\nDescription:\n\n     Overlay a t Density Function and a Normal Density Function on\n     Histogram\n\nUsage:\n\n     iscamaddtnorm(\n       x,\n       df,"

---

    Code
      collapse_output(extract_help_section(help_lines, "Arguments"))
    Output
      [1] "Arguments:\n\n     x: A numeric vector representing the data to be plotted.\n\n     df: A numeric value representing the degrees of freedom of 'x'.\n\n     main: Optional title for the plot\n\n     xlab: Optional x-axis label for the plot\n\n     bins: Optional number of bins for the histogram.\n"

