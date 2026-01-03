# The Levenson Index Project

## Content

## About

1. General description about paper <br>
2. General outline about this repository

## Getting Started: 

### Importing repository to your cloud environment (Posit Cloud)

#### About Posit Cloud?

1. General info + Link for opening new account. <br>
2. Differences between free tier (for testing) and paid plans (for deployment)

#### Importing repository on Posit Cloud account (Free Tier):

- Create a new project first. In the project, go to **Tools → Terminal**

 - Generate an SSH key in that project using:

> `ssh-keygen -t rsa -b 4096`

- Press **Enter** to accept the default file location

- Press **Enter** twice for empty passphrase (or add one if you prefer)

- View your public key with:

> `cat ~/.ssh/id_rsa.pub`

- Copy this key and add it to GitHub **(Settings → SSH Keys → New SSH Key)**. Then clone your repository:

> Use the repository's SSH URL (i.e. git@github.com:username/repo-name.git)

- Use Git commands in the terminal to clone:

> `git clone git@github.com:username/repo-name.git`


#### Note:

You'll need to be repeat this process for each new project where you want to use Git. <br>
This is one of the limitations of the free tier <br>
If you need persistent SSH keys, consider upgrading to a paid account <br>
Alternatively, you can save your SSH key somewhere secure and reuse it by copying it into new projects when needed

   



