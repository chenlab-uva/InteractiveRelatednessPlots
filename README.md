# InteractivePlot_KING
This repository is for the interactive plots from KING



## Interactive plots for ROH.

Run KING to get the required three files. They are *.roh, *.rohseg.gz and *allsegs.txt

```{bash}
king -b example.bed --roh --prefix example
```

Run the following R code in R to get interactive plots. Package 'shiny' and 'ggplot2' are required. Related R files are saved at ROH folder. <br/> 

```{bash}
library(shiny)
runGitHub("InteractivePlots", "chenlab-uva", ref = "main", subdir = "ROH")
```
Example output
<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/ROH/roh_example.png" width="854" height="480">
