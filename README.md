# limnoEntryTool_TEMPLATE
This is a template repository, created by Kaija Gahm on 14 May 2021. It's designed to be duplicated for each year's data entry. Don't use this repo itself to enter and store data––make a copy first!

Not sure how to do that? Follow the instructions below.

## Setup instructions
Basically, we're going to be following the instructions [here](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-repository-from-a-template), so feel free to refer to that resource instead/in addition to this README.

### Section 1: GitHub

1. Go to the main page of this repository. If you're reading this README, you're very likely already on the main page. You should see a list of files at the top with their most recent commits.

2. Click the big green **Use this template** button.

<img width="911" alt="Using this repository as a template" src="https://user-images.githubusercontent.com/37053323/118301989-ae374180-b4b1-11eb-9dc9-859191740b8a.png">

3. In the dropdown menu, select the GitHub account that you would like to have as the owner of this repository. You can leave that as MFEh2o, or you can use your own GitHub account. I need to look more into what the repercussions of each of those choices will be.

<img width="430" alt="Choosing an owner for the repository" src="https://user-images.githubusercontent.com/37053323/118302217-f191b000-b4b1-11eb-9fc3-29c60de56e8e.png">

4. In the field next to the owner, set a name for the new repository. I recommend **limnoEntryTool_2021** (or whichever year you're entering data for).

5. By default, the repository will be private (i.e. viewable only by members of the MFEh2o "organization" on GitHub). I'm not sure about private vs. public--maybe leave it private for now and we can discuss later?

6. Don't bother selecting "Include all branches"--you don't need that.

7. Don't bother selecting any Marketplace apps--that's not relevant here.

8. Click **Create repository from template.**

### Section 2: RStudio

Great, now you've created a new GitHub repository. The next step is to download the files onto whichever computer you're going to be using to enter the data.

1. Make sure you have **R** and **RStudio** installed on the computer. If you don't, you can install them [here](https://www.r-project.org/) and [here](https://www.rstudio.com/products/rstudio/download/). 

2. Make sure you have **git** installed. If you don't, you can follow the instructions [here](https://happygitwithr.com/install-git.html) to install it.

3. If you haven't already on the computer you're using, [introduce yourself to Git](https://happygitwithr.com/hello-git.html) by setting your username and email (should be the same as the username/email associated with your GitHub account). The easiest way to do this is through RStudio.

```
# install.packages("usethis") # install the usethis package--makes it easy to do complicated and finnicky things in R
library(usethis)
use_git_config(user.name = "Jane Doe", user.email = "jane@example.org") # enter your user name and email here.
```

4. Connect RStudio to GitHub, by following [this tutorial](https://happygitwithr.com/rstudio-git-github.html). The rest of the sections on that website will be helpful if you run into any trouble.

5. Okay, it's finally time to download your new template repository. In **RStudio**, go to File > New Project. In the window that pops up, choose "Version Control" and then "Git".

Now you should see something like this:

<img width="480" alt="Screen Shot 2021-05-14 at 1 45 07 PM" src="https://user-images.githubusercontent.com/37053323/118308957-9a440d80-b4ba-11eb-9793-101f058cfdd3.png">

For the repository URL, navigate to the main page of the repository that you've just cloned from this template and copy the URL from the address bar. For example, it might look something like **https://github.com/MFEh2o/limnoEntryTool_2021**.

When you paste in that URL, the project directory name should auto-populate. If it doesn't, just call it something logical like "limnoEntryTool_2021".

Then, you have the option of deciding where to put the new project you're about to create. This is going to be a new folder on your computer, so put it wherever makes sense (Desktop, etc).

Click "Create Project". Your project will get created, and a new session of RStudio will initialize, with the home directory set to the new folder you've just created.

### Section 3: Add Files

1. Add the database file. This should be the most recent version of the database, before adding any data from the current season. Download it from the [MFE Box](https://app.box.com/folder/136303669282). *Note: for clarity and reproducibility, please download the version that includes a date in the file name. So, 'MFEdb_20210423.db', not 'MFEdb.db'.* Put that into the main, top-level 'limnoEntryTool_2021' (or whatever you ended up calling it) folder.

2. Add your sample sheets. You can use the **limno2020_ID_YYYY-MM-DD (DO NOT SAVE).xlsx** file as a template for creating new sample sheets. Just make sure to copy it before filling it out; leave the **DO NOT SAVE** version as a template.

## Running the limno entry tool

1. There are several files in the 'code/' directory. The only one you should need to work with directly is 'updatingLogs.R'. Open up that file. Fill in the name of the database at the top, on the line that looks like this:

```
db <- "CHANGE THIS" # name of the database file you're using. Try to use one with a specific date to make the workflow clear. For example, "MFEdb_20200530.db"
```

So, to be clear, when you're done that line should look like this (with the name of the database version you're using).

```
db <- "MFEdb_20200530.db" # name of the database file you're using. Try to use one with a specific date to make the workflow clear. For example, "MFEdb_20200530.db"
```

2. Run the script! Pay attention to warnings and errors that come up in the console. If you get errors related to e.g. trying to add a new lake or site that isn't already in the database, you can re-run the script after adding parameters like `force_lakeID = T` or `force_siteID = T` to the `updateLimno()` function call.

## Saving your changes to GitHub

**Important**: before following these instructions, make *sure* that you have copied the template repository as instructed above. It would really not be ideal if you started committing/pushing data and code specific to one year to the template repository on GitHub.

Assuming that you're working in an RStudio project that you've copied from the template (should be called something like "limnoEntryTool_2021"), then you can proceed.

In your RStudio window, you should see a "Git" tab, in the same pane as "Environment", "History", and "Connections" (by default, it's at the top right out of the four panes, but if you've modified your pane layout, it might be somewhere else). Click on that Git tab. You should now see a small window with some button options at the top: a blue downward-pointing arrow ("Pull"), a green upward-pointing arrow ("Push"), a gear icon, a refresh button, the name "master" or "main" (shows the current branch you're on), etc.

Below those button options, you should see three columns: Staged, Status, and Path. If you have added, removed, or modified files since setting up this repo, you will see some files listed there. In the 'Staged' column, there will be a checkbox; in the 'Status' column, there will be a small colored square with a letter in it (A for added, or yellow question marks if you've added the file but it has never been committed before, D for deleted, R for renamed, M for modified).

So, to be more specific, let's imagine that you've run the limno entry tool on a couple of sample sheets. You probably made the following changes:

1. You put the sample sheets into 'sampleSheets/', as instructed above.
2. You added the most recent database version into the root directory, as instructed above.
3. You modified the 'CHANGE THIS' line in limnoEntry.R to point to the named database file
4. Maybe you made some other changes to the `updateLimno()` function call in limnoEntry.R, such as setting a few of the `force` parameters to `TRUE` to deal with adding e.g. new sites.
(You may have made other changes too, but I'm going to start with the above for example).

If you did that, here's the corresponding information you should see in the Git pane:
1. Each of the sample sheets should show up with yellow question marks that turn to A's when you click the checkbox.
2. The database file should **not** show up at all in the panel, because we have a line in our .gitignore file that says to ignore files ending in '.db'. That's because the database file is too large to push and pull to GitHub--that's why it wasn't included in the template repository originally. If you DO see the database .db file showing up in the git panel, that's not good. You might need to open the .gitignore file and add `*.db` somewhere in it to make sure the database file gets ignored. 
3. limnoEntry.R will show up in the git pane with a blue M next to it because you modified at least one line.
4. Same as 3.
5. Once you run the limno entry tool, a bunch of other files get modified and created. The IS files will get created in logFiles/, and the existing LogFile.csv files will get modified. So each of those will also show up in the git window with an M or question marks/an A depending on whether they were modified or added. Similarly, the labels/ excel sheets will show up as newly created.

Having these files show up in the Git pane means that Git is aware of your changes. The next step for saving your changes to GitHub is to "stage" these files. The concept of staging is a little confusing; you can read more about it [here](https://softwareengineering.stackexchange.com/questions/119782/what-does-stage-mean-in-git#:~:text=Staging%20is%20a%20step%20before,from%20staging%2C%20etc.). But basically, staging a file just means that you're getting ready to commit it to GitHub.

So, you can go ahead and check the checkboxes for all those files (just make sure NOT to check the box on your database file if for some reason it's still showing up!)

Now that the boxes are checked, click the "Commit" button. A new window will pop up that shows your staged files again, along with an overview of your changes (insertions in green, deletions in red) at the bottom (you can see the changes for each file by clicking on its name in the top left panel). At the top right, you need to enter a "Commit mesasage"--a brief note about what change(s) you made. This is mandatory. Try to keep it under 50 characters (the maximum that will display on GitHub) and keep it short and sweet.

Something like '2021 setup and processing first 3 sample sheets' could work as a good first commit message.

Then click the "Commit" button to save your changes to the Git version control system.

Finally, you can "Push" your changes to GitHub by clicking the green "Push" arrow. This will cause the changes to actually show up if you go look at the repository on GitHub, which means they can be seen by others with access to the repo and accessed from other computers.

It might seem a little redundant to commit and then immediately push. Typically, you can make a bunch of commits before pushing, but that's more relevant when you're actively developing a piece of software. When you use the entry tool, you're more likely to run it each time with minimal changes, generating new output files and modifying log files each time, so it makes sense to push each time you commit. But it's not critical--if you forget to push, you can just commit the next day's datasheets and then push then. No worries.

I wrote this whole example assuming that you'll make your first commit after both setting up the entry tool *and* running the script for the first time. That's a fine way to do it, but if you prefer, you can also make one commit after just the setup part (after adding the database file and sample sheets but before running the tool) and another one after running the tool for the first time. Or, you can make one commit per file modified, if you'd like to leave a different commit message for each file. It's totally up to you! Again, because your changes will usually be relatively straightforward, the details of when and how you commit are not super important.

## Questions?

I hope that's a helpful introduction. If you want more information about committing, pushing, and pulling (which I didn't explicitly cover because you usually won't have to do it for this tool), you can check out [Happy Git with R](https://happygitwithr.com/git-basics.html), the [RStudio tutorial on version control](https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN), and [this tutorial on committing/pushing/pulling](https://cfss.uchicago.edu/setup/git-with-rstudio/#step-3-make-local-changes-save-commit).

Contact Kaija (gahmk@caryinstitute.org) with questions for troubleshooting help. If you're reading this while Kaija still works at Cary, definitely contact her! This is literally her job. If you're reading this after Kaija has left to go to grad school, try the above resources first, or talk to Chris, Stuart, or Randi.



