# Ways of using ColabNet app

## OPTION 1 - Local use through `colabNet` package

### 1. Install the latest version of the colabNet package

```r
devtools::install_github("pieterjanvc/colabNet", ref = "main")
```

### 2. Run the `colabNet()` function

```r
colabNet::colabNet()
```

_This will start the app and allow you to choose from any ColabNet database on
your local machine or start with a new one. Check the function documentation for
additional options_

## OPTION 2 - Deployment on a remote server

### 1. Generate the app production files

- Run the generateProdFolder() function to create a ColabNet folder which
contains all files needed to publish the app on a server

### 2. Reinstall the latest colabNet package from GitHib

In order for a Shiny server to accept the colabNet package, it has to be 
installed locally through GitHub first. This means if you installed the 
package by cloning the repo and using devtools (or in R Studio Build --> Install Package) you might 
get an error. Just replace the current package with the latest version from
GitHub
```r
remotes::install_github("https://github.com/pieterjanvc/colabNet")
```

### 3. Deploy the app

- Deploy the app to a Shiny / Posit Connect server
- If you want to protect local databases from being edited by others, create an
  environment variable called `adminPassword`. This will now protect the admin
  tab

## OPTION 3 - Development with Git project

- Clone the repo from [GitHub](https://github.com/pieterjanvc/colabNet)
- Open the [app.R](../inst/app.R) file in the inst folder
- Optional: set the `testDB` variable to a local database if you like to skip
  the database connection modal pop-up when running the app
- You can now edit / run the app

_Note the app has several modules which can be found under R/modules/_
