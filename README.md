# limnoEntryTool_TEMPLATE
This is a template repository, created by Kaija Gahm on 14 May 2021. It's designed to be duplicated for each year's data entry. Don't use this repo itself to enter and store data––make a copy first!

Not sure how to do that? Follow the instructions below.

### How to make a copy of this repository
Basically, we're going to be following the instructions [here](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-repository-from-a-template), so feel free to refer to that resource instead/in addition to this README.

**Section 1: GitHub**
**Step 1**
Go to the main page of this repository. If you're reading this README, you're very likely already on the main page. You should see a list of files at the top with their most recent commits.

**Step 2**
Click the big green **Use this template** button.

<img width="911" alt="Using this repository as a template" src="https://user-images.githubusercontent.com/37053323/118301989-ae374180-b4b1-11eb-9dc9-859191740b8a.png">

**Step 3**
In the dropdown menu, select the GitHub account that you would like to have as the owner of this repository. You can leave that as MFEh2o, or you can use your own GitHub account. I need to look more into what the repercussions of each of those choices will be.

<img width="430" alt="Choosing an owner for the repository" src="https://user-images.githubusercontent.com/37053323/118302217-f191b000-b4b1-11eb-9fc3-29c60de56e8e.png">

**Step 4**
In the field next to the owner, set a name for the new repository. I recommend **limnoEntryTool_2021** (or whichever year you're entering data for).

**Step 5**
By default, the repository will be private (i.e. viewable only by members of the MFEh2o "organization" on GitHub). You should probably leave it as private.

**Step 6**
Don't bother selecting "Include all branches"--you don't need that.

**Step 7**
Don't bother selecting any Marketplace apps--that's not relevant here.

**Step 8**
Click **Create repository from template.**

**Section 2: RStudio**
Great, now you've created a new GitHub repository. The next step is to download the files onto whichever computer you're going to be using to enter the data.

**Step 1**



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
