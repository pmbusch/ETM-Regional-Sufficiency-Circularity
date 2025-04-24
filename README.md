# The Omitted Role of Trade in Critical Mineral Sufficiency and Circularity Potential

Replication materials for Busch *et al.* (*Submitted*). The Omitted Role of Trade in Critical Mineral Sufficiency and Circularity Potential.

The following code and data allows for the reproduction of all the tables, figures and calculations made in the article, both in the main body and supplementary information.

If you identify any error in the source code or have any further suggestions please contact Pablo Busch at pmbusch@ucdavis.edu.


# Organization

* **Inputs**: Data inputs used in the analysis. 
* **Figures**: Figures included in the analysis. 
* **Scripts**: All code to process the data, run models and create figures. Each script starts with a description of the file purpose. Through the file there are several explanatory  comments.  
* **Results**: Aggregated results stored to recreate tables and figures.

# Instructions

The repository is ~160Mb fully unzipped, please make sure to manually unzip files in the Results folder. Downloading and unzipping everything should take less than 5 minutes on a normal computer.

Users can run all the code for replication using the "ETM-Regional-Sufficiency-Circularity.Rproj" file, or by setting their own working directory and running scripts independently.

This GitHub contains organization notes in each folder describing the, and each scripts is properly docummented.

## Runtime

Users can either used the uploaded model results to replicate figures, or run new instances to generate results for the demand or supply model. Some results are uploaded in the GitHub in a compressed format, so please make sure to unzip them. 

Please note that each demand scenarion calculation takes around 3 minutes to run.

# Software required

The script code was developed with **R** software version 4.4.1. 

The R code requires the following packages: *tidyverse*, *readr*,*readxl*,*ggplot2*,*data.table*,*dplyr*,*gridExtra*,*reshape2*,*scales*,*RColorBrewer*,*sf*,*ggrepel*. All libraries can be installed with the following command: 
```
install.packages(c("tidyverse","readr","readxl","ggplot2","data.table","dplyr","gridExtra","reshape2","scales","RColorBrewer","sf","ggrepel"), dependencies = T)
```

The model has only been tested using OS Windows 10 and 11, but it should work on Mac and Linux as well using **R** and **julia**

# License
This project is covered under the **MIT License**