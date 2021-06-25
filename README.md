# InteractivePlot_KING
This repository is for the interactive plots from KING



## Interactive plots for ROH

Run KING to get the required three files. They are *.roh, *.rohseg.gz and *allsegs.txt

```{bash}
king -b hapmap.bed --roh --prefix hapmap
```

Run the following R code in R to get interactive plots. Package 'shiny' and 'ggplot2' are required. Related R files are saved at ROH folder. <br/> 

```{bash}
library(shiny)
runGitHub("InteractivePlots", "chenlab-uva", ref = "main", subdir = "ROH")
```
Example output

<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/ROH/ROH_1.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/ROH/ROH_2.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/ROH/ROH_3.png" width="854" height="480">


## Interactive plots for IBD Segments

Run KING to get the required three files. They are *.seg, *.segments.gz and *allsegs.txt

```{bash}
king -b hapmap.bed --ibdseg --prefix hapmap
```

Run the following R code in R to get interactive plots. Package 'shiny', 'data.table' and 'ggplot2' are required. Related R files are saved at IBDSeg folder. <br/> 

```{bash}
library(shiny)
runGitHub("InteractivePlots", "chenlab-uva", ref = "main", subdir = "IBDSeg")
```
Example output

<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/IBDSeg/viewIBD_1.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/IBDSeg/viewIBD_2.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/IBDSeg/viewIBD_3.png" width="854" height="480">


## Visualization of Families

```{bash}
library(shiny)
runGitHub("InteractivePlots", "chenlab-uva", ref = "main", subdir = "FamilyVisualization")
```

<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/FamilyVisualization/viewFamily1.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/FamilyVisualization/viewFamily2.png" width="854" height="480">
