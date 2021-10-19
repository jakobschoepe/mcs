create.table <- function(x, caption, label, footnote, theta) {
  thetas <- list(name = paste0("$\\theta", sprintf("_%01d", 0:16), "$"), theta = theta)
  
  cat(paste0("\\begin{sidewaystable}\n", 
             "\\small\n", 
             "\\caption[", caption, "]{", caption, ".}\n", 
             "\\begin{tabularx}{\\textwidth}{lcccccccc}\n", 
             "\\toprule\n", 
             "& \\textbf{True value} & \\textbf{Accuracy} & \\textbf{Precision} & \\textbf{MSE} & \\multicolumn{4}{c}{\\textbf{Coverage probability}}\\\\\n", 
             "& & Bias & Empirical SE & & Standard CI & Percentile CI & Smoothed CI & Smoothed CI\\\\\n",
             "& & & & & & & & (bias-corrected)\\\\\n",
             "\\midrule"))
  for (i in seq_len(nrow(x))) {
    cat(paste0("\n", thetas[[1]][i], " & ", thetas[[2]][i]))
    cat(gsub("NA", "$\\\\times$", paste0(" & ", format(round(x[i,1], digits = 3), nsmall = 3), ifelse(is.na(x[i,2]), "", paste0(" (", format(round(x[i,2], digits = 3), nsmall = 3), ")")), 
               " & ", format(round(x[i,3], digits = 3), nsmall = 3), ifelse(is.na(x[i,4]), "", paste0(" (", format(round(x[i,4], digits = 3), nsmall = 3), ")")),
               " & ", format(round(x[i,5], digits = 3), nsmall = 3), ifelse(is.na(x[i,6]), "", paste0(" (", format(round(x[i,6], digits = 3), nsmall = 3), ")")),
               " & ", format(round(x[i,7], digits = 3), nsmall = 3), ifelse(is.na(x[i,8]), "", paste0(" (", format(round(x[i,8], digits = 3), nsmall = 3), ")")),
               " & ", format(round(x[i,9], digits = 3), nsmall = 3), ifelse(is.na(x[i,10]), "", paste0(" (", format(round(x[i,10], digits = 3), nsmall = 3), ")")),
               " & ", format(round(x[i,11], digits = 3), nsmall = 3), ifelse(is.na(x[i,12]), "", paste0(" (", format(round(x[i,12], digits = 3), nsmall = 3), ")")),
               " & ", format(round(x[i,13], digits = 3), nsmall = 3), ifelse(is.na(x[i,14]), "", paste0(" (", format(round(x[i,14], digits = 3), nsmall = 3), ")")))))
    cat(paste0("\\\\"))
  }
  cat(paste0("\n\\bottomrule\n", 
             "\\end{tabularx}\n",
             "\\label{", label, "}\n\n", 
             "\\begin{footnotesize}\n",
             "\\textbf{\\textit{Footnote:}} ", footnote, "\n",
             "\\end{footnotesize}\n", 
             "\\end{sidewaystable}"))
}
