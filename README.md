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

5. By default, the repository will be private (i.e. viewable only by members of the MFEh2o "organization" on GitHub). You should probably leave it as private.

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

#### Section 3: Add Files

Now, add your sample sheets. You can use the **limno2020_ID_YYYY-MM-DD (DO NOT SAVE).xlsx** file as a template for creating new sample sheets. Just make sure to copy it before filling it out; leave the **DO NOT SAVE** version as a template.

## Running the limno entry tool



