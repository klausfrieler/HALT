# Headphone and Loudspeaker Test (HALT)


The HALT is a test for detection whether online experiment takers use headphones or loudspeakers 


## Citation

We also advise mentioning the software versions you used,
in particular the versions of the `HALT` and `psychTestR` packages.
You can find these version numbers from R by running the following commands:

``` r
library(HALT)
library(psychTestR)
if (!require(devtools)) install.packages("devtools")
x <- devtools::session_info()
x$packages[x$packages$package %in% c("HALT", "psychTestR"), ]
```

## Installation instructions (local use)

1. If you don't have R installed, install it from here: https://cloud.r-project.org/

2. Open R.

3. Install the ‘devtools’ package with the following command:

`install.packages('devtools')`

4. Install the HALT:

`devtools::install_github('klausfrieler/HALT', ref = "main")`
(The ref = "main" is needed for older github installation, as github changed the default branch name from "master" to "main" recently.)

## Usage

### Quick demo 

You can demo the HALT at the R console, as follows:

``` r
# Load the HALT package
library(HALT)

```

### Testing a participant

The `HALT_standalone()` function is designed for real data collection.
In particular, the participant doesn't receive feedback during this version.

``` r
# Load the HALT package
library(HALT)

# Run the test as if for a participant, using default settings,
# saving data, and with a custom admin password
# You can specify a strategy how to use the test A, B  and C using a config object, which you can create using auto_config() and make_config()
HALT_standalone(config, admin_password = "put-your-password-here")
```

You will need to enter a participant ID for each participant.
This will be stored along with their results.

Each time you test a new participant,
rerun the `HALT_standalone()` function,
and a new participation session will begin.

You can retrieve your data by starting up a participation session,
entering the admin panel using your admin password,
and downloading your data.
For more details on the psychTestR interface, 
see http://psychtestr.com/.

The HALT currently supports English (`en`) and German (`de` [Du] and `de_f` [Sie]).
You can select one of these languages by passing a language code as 
an argument to `HALT_standalone()`, e.g. `HALT_standalone(languages = "de")`,
or alternatively by passing it as a URL parameter to the test browser,
eg. http://127.0.0.1:4412/?language=DE (note that the `p_id` argument must be empty).

## Installation instructions (Shiny Server)

1. Complete the installation instructions described under 'Local use'.
2. If not already installed, install Shiny Server Open Source:
https://www.rstudio.com/products/shiny/download-server/
3. Navigate to the Shiny Server app directory.

`cd /srv/shiny-server`

4. Make a folder to contain your new Shiny app.
The name of this folder will correspond to the URL.

`sudo mkdir HALT`

5. Make a text file in this folder called `app.R`
specifying the R code to run the app.

- To open the text editor: `sudo nano HALT/app.R`
- Write the following in the text file:

``` r
library(HALT)
HALT_standalone(admin_password = "put-your-password-here")
```

- Save the file (CTRL-O).

6. Change the permissions of your app directory so that `psychTestR`
can write its temporary files there.

`sudo chown -R shiny HALT`

where `shiny` is the username for the Shiny process user
(this is the usual default).

7. Navigate to your new shiny app, with a URL that looks like this:
`http://my-web-page.org:3838/HALT

## Usage notes

- The HALT runs in your web browser.
- By default, image files are hosted online on our servers.
The test therefore requires internet connectivity.
