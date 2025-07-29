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

### 1. Create a new blank folder

- Copy the [app.R](../inst/app.R) file from the inst folder to this new folder
- If you have any local databases you would like to upload, create and
  additional `localDB` sub folder and put all databases in there

### 2. Make the following changes to the app.R file

- Set the `mode` variable to `mode <- "prod"`
- Adjust the `autoCleanTemp` variables as needed

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
