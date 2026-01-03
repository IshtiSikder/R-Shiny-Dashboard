# The Levenson Index Project

## About

This repository holds the code base for an interactive dashboard, outlining the data and ideas presented in the paper titled **Effects of study design parameters on estimates of bee abundance and richness in agroecosystems: a meta-analysis**. The research work was conducted by Hannah K. Levenson, Bradley N. Metz, and David R.Tarpy.<br>



## Content
| Script Name  | Content |
| ------------- | ------------- |
| app.R  | run this script to launch app  |
| collectionMethods.R  | boxplot for collection methods  |
| data_preprocessing_prediction_algos.R  | cleans and prepare data frame for model |
| data_preprocessing_prediction_plot.R  | cleans and prepare data frame for plot |
| durationTrend.R  | graph for study duration trend  |
| exploratory_data.csv.R  | data frame for exploratory part of app  |
| global.R  | packages, libraries and sources  |
| identificationMethods.R  | pie chart for identification methods  |
| model_coefficients.R  | defines model coefficients for abundance and richness  |
| prediction_algos.R  | model and exponential function to predict abundance and richness  |
| prediction_plots.R  | creates prediction plot |
| samplingTrips.R  | histogram for sampling trips  |
| server.R  | background operations, data loading, button functions, reactive values or prediction, output for graphs |
| siteCounts.R  | histogram for site counts  |
| ui.R  | design theme  |
| ui_components.R  | page definitions, titles, buttons, tabs, and panels  |
| usaStateMap.R  | USA map with study count per state  |
| worldMap.R  | world map with study count per country  |


## Getting Started: 

### About Posit Cloud and shinyapps.io

1. Posit Cloud is a cloud IDE for developing Python/R coded interactive dashboards. Kindly open a free account through [this link](https://posit.co/products/cloud/cloud/). <br>
2. For publishing the dashboard, we will connect our Posit Cloud project with the shinyapps.io platform. Visit [this link](https://www.shinyapps.io/) and scroll down the page to check the different plans available. We would recommend opening a free acount first (25 active hours every month), and upgrading to a paid plan (100+ active hours per month) later on if needed (i.e. high number of site visits).

### Branching Strategy

1. **Develop** is the default branch that gets updated on this repository.
2. Approved version of the **Develop** branch gets merged with the **Main** branch, which is then deployed.

### Importing Github repository to your cloud environment (Posit Cloud)

#### For Client(s)

- Click on **Fork** on [this link](https://github.ncsu.edu/NC-State-Libraries-Data-Visualization/R-Shiny-Bees). It will create a new branch for this repo under your current Github account.
- Click on **Code** and copy the **HTTPS** link.
- Open **Posit Cloud** and run `git clone <your HTTPS link>`
- Jump to the **Publishing the dashboard** section and follow from there.

#### For Admin(s)

##### Initial setup: Has to be completed only ONCE when a new Posit Cloud project is set up. 
###### (Do NOT follow if you have already followed instructions for Clients)

- Create a new project under Posit Cloud first. In the project, go to **Terminal**.

 - Generate an SSH key in that project by copying and pasting the following on your **Terminal** then hitting **Enter**.

> `ssh-keygen -t rsa -b 4096`

- Press **Enter** to accept the default file location

- Press **Enter** twice for empty passphrase (or add one if you prefer)

- View your public key with:

> `cat ~/.ssh/id_rsa.pub`

- Copy this key and under your GitHub repo go to **Settings → Deploy Keys → Add deploy key**. Paste under **Key** and provide a name under **Title**. Make sure **Allow write access** is checked before clicking on **Add key**.

- Back in your Posit Cloud terminal, provide your Github account's username and mail id with following commands:

> `git config --global user.name "Your Name"`

> `git config --global user.email "your.email@example.com"`


- Then clone your repository under your Posit Cloud project:

> Copy/paste your repository's SSH URL (i.e. `git@github.com:username/repo-name.git`) from **Code → SSH** under Github

- Use Git commands in the Posit Cloud terminal to clone:

> `git clone git@github.com:username/repo-name.git`

- The following propt will pop up. Type `yes` when it does:

> `Are you sure you want to continue connecting (yes/no/[fingerprint])?`

-  Under **Files** in Posit Cloud, a folder can now be found with the cloned repository's name. Make sure your current directory is under this folder. Use `cd R-Shiny-Bees` to change directory to your cloned repository

- Check links between your Posit Cloud project and Github Repository using `git remote -v`. Usually there are two **Origin** links (both with same address tied to your repo) for **fetch** and **push**

- Click **File → Save All** in Posit Cloud

##### After initial setup: Has to be completed EACH time Posit Cloud project is opened.

- Make sure you are on the **Develop** branch. Check by running `git branch`. If not on **Develop** branch, switch by running `git checkout develop`.

- Run `git pull` in Posit Cloud terminal to import last updated repository.

### Updating + Testing Github repository from your cloud environment (Posit Cloud)

#### Update the code base

- You can now make edits to the code scripts from this repository in your Posit cloud environment. Once done, push recent updates as outlined below.
  
- Add/commit scripts individually, or
> `git add script1.R`

> `git commit -m "Updated something in script1"`

- Add/commit scripts together
> `git add -A`

> `git commit -m "All new updates"`

- After making all necessary commits separately, push them all at once by running
> `git push`

- Upon approval, push updates from **Develop** to **Main** branch (either using git commands or directly from Github).

#### Test the code base

- Select the **app.R** script from **Files** and click on **Run App** to test the dashboard deployment through Posit Cloud. Make sure pop-up is not blocked (the dashboard will open up in a new window). Close the window once done testing (the free posit cloud account allows max. 25 hours of testing).

### Publishing the dashboard

- Switch to **Main** branch by running `git checkout main`
- Log into to your **shinyapps.io** account.
- Click on **Copy to clipboard** to copy your **shinyapps.io** token.
- Come back to your **Posit Cloud** project. Click on **Publish the Application or Document** (blue round icon) and then **Manage Accounts**.
- Paste the copied token on the window that opens up and click **Apply**.
- Come back to the **Posit Cloud** account and click on **Publish** again.
- The dashboard should be online now. Check Deployment status from the **Application** tab on your **shinyapps.io** account.
