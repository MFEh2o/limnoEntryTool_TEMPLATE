# limnoEntryTool-2020

#### Basic sketch of the workflow for using this tool
1. Download the folder from GitHub by clicking the green "code" download button.
2. Right now, the scripts assume that you have your database and dbUtils.R script two folders above the limnoEntryTool-2020 folder.
3. You have paper limno data sheets. Open the folder called **sampleTemplate/**. The Excel sheet inside this folder has the same format as the paper data sheets. 
4. Fill out the Excel sheet with the data from your first data sheet. When you finish the data sheet, save the Excel sheet **with a unique name** into the **sampleSheets2020/** folder: e.g. "limno2019_WL_2020-06-04.csv". Repeat for each of your paper data sheets, saving each one into **sampleSheets2020/**. 
5. Once you have translated all of your paper data sheets into csv's, open "limnoEntryTool-2020.Rproj". This will open a new, fresh session of RStudio, whose home/top-level directory is set to the folder containing the .Rproj file, **limnoEntryTool-2020/** (You can read more about RStudio "projects" [here](https://r4ds.had.co.nz/workflow-projects.html) and [here](https://www.tidyverse.org/blog/2017/12/workflow-vs-script/), if you're interested).
6. In the "Files" window (usually located at the bottom right corner of your RStudio window, but it depends on your settings), click on the **code/** folder, and then open the **updatingLogs.R** script. This is the script you will use to run the entry tool and enter your data.
7. Make sure the directories at the top of the script are correctly set to refer to the MFE database (see step 2--this is something that we may work on standardizing in the future). 
8. Run the script

(more explanation to come of the process beyond that)
