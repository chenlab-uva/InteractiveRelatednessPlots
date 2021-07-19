# Interactive Visualization of Relatedness from KING
This repository is for the interactive plots from KING. We use HapMap (https://www.dropbox.com/s/3dgjlmdc9b1tigq/HapMap.zip?dl=0) as the example dataset.<br/>

Use curl to download the example dataset.
```{bash}
curl https://www.dropbox.com/s/3dgjlmdc9b1tigq/HapMap.zip?dl=0 -O -J -L
```
Then use unzip to unzip the zipped file.
```{bash}
unzip HapMap.zip 
```
Or we can click the Dropbox link, and then click the download button at the top right corner to download files. And then use unzip to unzip the zipped file.
```{bash}
unzip HapMap.zip 
```

Related KING output files are saved at HapMap folder.

## R packages required for these shiny apps.
| Name  | Required R packages |
| ------------- | ------------- |
| viewROH  | 'shiny', 'ggplot2'  |
| viewIBD  | 'shiny', 'ggplot2' and 'data.table', 'R.utils'|
| viewFamily  | 'shiny', 'ggplot2', 'data.table', 'kinship2' and 'igraph', 'R.utils' |


## Interactive plots for ROH

Run KING to get the required three files. They are *.roh, *.rohseg.gz and *allsegs.txt

```{bash}
king -b hapmap.bed --roh --prefix hapmap
```

Run the following R code in R to get interactive plots. Package 'shiny' and 'ggplot2' are required. Related R files are saved at viewROH folder. <br/> 

```{bash}
library(shiny)
runGitHub("InteractiveRelatednessPlots", "chenlab-uva", ref = "main", subdir = "viewROH")
```
Example output

<img src="https://github.com/chenlab-uva/InteractiveRelatednessPlots/blob/main/viewROH/viewROH_1.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractiveRelatednessPlots/blob/main/viewROH/viewROH_2.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractiveRelatednessPlots/blob/main/viewROH/viewROH_3.png" width="854" height="480">



## Interactive plots for IBD Segments

Run KING to get the required three files. They are *.seg, *.segments.gz and *allsegs.txt

```{bash}
king -b hapmap.bed --ibdseg --prefix hapmap
```

Run the following R code in R to get interactive plots. Package 'shiny', 'data.table' and 'ggplot2' are required. Related R files are saved at viewIBD folder. <br/> 

```{bash}
library(shiny)
runGitHub("InteractiveRelatednessPlots", "chenlab-uva", ref = "main", subdir = "viewIBD")
```
Example output

<img src="https://github.com/chenlab-uva/InteractiveRelatednessPlots/blob/main/viewIBD/viewIBD_1.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractiveRelatednessPlots/blob/main/viewIBD/viewIBD_2.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractiveRelatednessPlots/blob/main/viewIBD/viewIBD_3.png" width="854" height="480">




## Visualization of Families

Run KING to get the required four files. They are *.seg, *.segments.gz, *allsegs.txt and *splitped.txt. Related R files are saved at viewFamily folder. <br/> 

```{bash}
king -b hapmap.bed --ibdseg --prefix hapmap
```

Package 'shiny', 'kinship2', 'igraph', 'data.table' and 'ggplot2' are required.

```{bash}
library(shiny)
runGitHub("InteractiveRelatednessPlots", "chenlab-uva", ref = "main", subdir = "viewFamily")
```

<img src="https://github.com/chenlab-uva/InteractiveRelatednessPlots/blob/main/viewFamily/viewfamily_1.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractiveRelatednessPlots/blob/main/viewFamily/viewfamily_2.png" width="854" height="480">
<img src="https://github.com/chenlab-uva/InteractiveRelatednessPlots/blob/main/viewFamily/viewfamily_3.png" width="854" height="480">

