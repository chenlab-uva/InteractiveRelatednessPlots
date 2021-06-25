# Interactive Visualization of Relatedness from KING
This repository is for the interactive plots from KING.<br/> We use HapMap (https://www.dropbox.com/sh/gc14nids6s7bmsz/AAAzADx-_kJEAwKpwPsJnCtja?dl=0) as the example dataset. Related KING output files are saved at HapMap folder.

## R packages required for these shiny apps.
| Name  | Required R packages |
| ------------- | ------------- |
| viewROH  | 'shiny', 'ggplot2'  |
| viewIBD  | 'shiny', 'ggplot2' and 'data.table'|
| viewFamily  | 'shiny', 'ggplot2', 'data.table', 'kinship2' and 'igraph' |


## Interactive plots for ROH

Run KING to get the required three files. They are *.roh, *.rohseg.gz and *allsegs.txt

```{bash}
king -b hapmap.bed --roh --prefix hapmap
```

Run the following R code in R to get interactive plots. Package 'shiny' and 'ggplot2' are required. Related R files are saved at ROH folder. <br/> 

```{bash}
library(shiny)
runGitHub("InteractivePlots", "chenlab-uva", ref = "main", subdir = "viewROH")
```
Example output

<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/viewROH/ROH_1.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/viewROH/ROH_2.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/viewROH/ROH_3.png" width="854" height="480">


## Interactive plots for IBD Segments

Run KING to get the required three files. They are *.seg, *.segments.gz and *allsegs.txt

```{bash}
king -b hapmap.bed --ibdseg --prefix hapmap
```

Run the following R code in R to get interactive plots. Package 'shiny', 'data.table' and 'ggplot2' are required. Related R files are saved at IBDSeg folder. <br/> 

```{bash}
library(shiny)
runGitHub("InteractivePlots", "chenlab-uva", ref = "main", subdir = "viewIBD")
```
Example output

<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/viewIBD/viewIBD_1.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/viewIBD/viewIBD_2.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/viewIBD/viewIBD_3.png" width="854" height="480">


## Visualization of Families

Run KING to get the required four files. They are *.seg, *.segments.gz, *allsegs.txt and *splitped.txt.

```{bash}
king -b hapmap.bed --ibdseg --prefix hapmap
```

Package 'shiny', 'kinship2', 'igraph', 'data.table' and 'ggplot2' are required.

```{bash}
library(shiny)
runGitHub("InteractivePlots", "chenlab-uva", ref = "main", subdir = "viewFamily")
```

<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/viewFamily/viewFamily1.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractivePlots/blob/main/viewFamily/viewFamily2.png" width="854" height="480">
