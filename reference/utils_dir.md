# Functions Package Dir

This function returns the directory path where all the data files of the
application are stored.

## Usage

``` r
getDirApp(...)

getDirAppAll(...)

getDirAppAllData(...)

getDirAppSharepoint(path = "MSF", ...)

getDirAppTemp(...)

getDirAppUsers(...)

getDirApps(...)

getDirAssets(...)

getDirDatabase()

getDirDevelopment(...)

getDirOrganization(org = "epicentre", ...)

getDirPackage(...)

getDirSharepoint(path = "MSF")

getDirShiny(...)

getDirTemp(...)

getDirUser(...)

getDirUserData(...)

getDirUserDataLocal(...)

getDirUserDataRoaming(...)

getDirUserHome()

createDir(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`fs::path`](https://fs.r-lib.org/reference/path.html)

  `ext`

  :   An optional extension to append to the generated path.

- path:

  Character string indicating the directory inside the SharePoint folder
  (default is 'MSF')

- org:

  The name of the organization for which the directory is to be created.
  Default is 'epicentre'.

## Value

A character string representing the full path to the user's roaming data
directory.

## Functions

- `getDirApp()`: Get Directory Data

  This function returns the path to the user data directory for the
  package.

- `getDirAppAll()`: Get the directory path for all application
  directories.

  This function creates a directory with the name 'all' inside the main
  application directory and returns the full path to this new directory.

- `getDirAppAllData()`: Get the directory path for all data files in the
  application.

- `getDirAppSharepoint()`: Get the directory path in the SharePoint
  application

  This function returns the directory path in the SharePoint application
  folder.

- `getDirAppTemp()`: Get temporary directory under the app directory

  This function creates a temporary directory under the application
  directory and returns the path to that directory.

- `getDirAppUsers()`: Get the directory path for application users

  This function returns the absolute path of the directory for
  application users. The function utilizes the `createDir` function to
  create the users directory if it doesn't exist.

- `getDirApps()`: Get directory for storing applications

  This function creates a directory for storing applications within the
  organization directory.

- `getDirAssets()`: Chemin des ressource du package (image, icon, fonts,
  css, js, ...)

  Cette fonction renvoie le chemin du repertoire contenant les
  ressources du package, telles que les images, les icones, les polices,
  les fichiers CSS, les fichiers JS, etc.

- `getDirDatabase()`: Get the directory path of the database

  This function retrieves the directory path where the database is
  located.

- `getDirDevelopment()`: Get the path to the development directory.

  This function retrieves the path to the development directory, based
  on the operating system. For Windows, it first obtains the user
  library directory using the `Sys.getenv` function, and then applies
  the [`fs::path_dir`](https://fs.r-lib.org/reference/path_file.html)
  function multiple times to navigate to the development directory. For
  other operating systems, it also retrieves the user library directory
  using `Sys.getenv', and applies the `fs::path_dir\` function to
  navigate to the development directory. Finally, it constructs the full
  path by appending any additional subdirectories specified in the
  arguments.

- `getDirOrganization()`: Get Directory Organization

  This function creates a directory with the specified organization name
  and returns its path.

- `getDirPackage()`: Get directory path within the package

  This function returns the path of a directory within the package.

- `getDirSharepoint()`: Get the SharePoint directory path

  This function returns the path of the SharePoint directory.

- `getDirShiny()`: Get Shiny directory

  This function returns the directory where the Shiny application is
  hosted.

- `getDirTemp()`: Get Temporary Directory Path

  This function returns the absolute path to the temporary directory.

- `getDirUser()`: Directory of the current user

  This function returns the directory of the current user.

- `getDirUserData()`: Get the directory for user data

  This function returns the directory path for user data. It first
  checks the operating system. If it is Windows, it calls the function
  `getDirUserDataLocal()` from the `fs` package to get the local user
  data directory. Otherwise, it returns `NULL`.

- `getDirUserDataLocal()`: Get local user data directory

  This function returns the path to the local user data directory. It
  uses the
  [`rappdirs::user_data_dir`](https://rappdirs.r-lib.org/reference/user_data_dir.html)
  function to get the user data directory and appends any additional
  path provided.

- `getDirUserDataRoaming()`: Get the path to the user's roaming data
  directory.

- `getDirUserHome()`: Get user's home directory

  This function returns the path of the user's home directory.

- `createDir()`: Create Directory

  This function creates a directory at the specified path. If the
  directory already exists, it does nothing.

## See also

`getDirApp`, `createDir`

`createDir`, `getDirApp`

## Examples

``` r
getDirData()
#> Error in getDirData(): could not find function "getDirData"
getDirData('subdir')
#> Error in getDirData("subdir"): could not find function "getDirData"

getDirApps()
#> Error in getDirApps(): could not find function "getDirApps"
getDirApps('app1')
#> Error in getDirApps("app1"): could not find function "getDirApps"

getDirAppAllData()
#> Error in getDirAppAllData(): could not find function "getDirAppAllData"

getDirAppSharepoint()
#> Error in getDirAppSharepoint(): could not find function "getDirAppSharepoint"

getDirAppTemp()
#> Error in getDirAppTemp(): could not find function "getDirAppTemp"
getDirAppTemp('subdir')
#> Error in getDirAppTemp("subdir"): could not find function "getDirAppTemp"

getDirApps() # creates a directory called 'apps' within the organization directory
#> Error in getDirApps(): could not find function "getDirApps"
getDirApps('app1') # creates a directory called 'app1' within the organization directory
#> Error in getDirApps("app1"): could not find function "getDirApps"

getDirOrganization()
#> Error in getDirOrganization(): could not find function "getDirOrganization"
getDirOrganization(org = 'myOrg')
#> Error in getDirOrganization(org = "myOrg"): could not find function "getDirOrganization"

getDirPackage('data')
#> Error in getDirPackage("data"): could not find function "getDirPackage"
getDirPackage('inst', 'templates')
#> Error in getDirPackage("inst", "templates"): could not find function "getDirPackage"

getDirSharepoint()
#> Error in getDirSharepoint(): could not find function "getDirSharepoint"
# Returns: 'C:/Users/<Username>/OneDrive/Sharepoint/MSF'

getDirSharepoint('Documents')
#> Error in getDirSharepoint("Documents"): could not find function "getDirSharepoint"
# Returns: 'C:/Users/<Username>/OneDrive/Sharepoint/Documents'

getDirShiny()
#> Error in getDirShiny(): could not find function "getDirShiny"

getDirUser()
#> Error in getDirUser(): could not find function "getDirUser"

getDirUserDataLocal()
#> Error in getDirUserDataLocal(): could not find function "getDirUserDataLocal"
getDirUserDataLocal('myapp')
#> Error in getDirUserDataLocal("myapp"): could not find function "getDirUserDataLocal"

getDirUserDataRoaming('subdir1', 'subdir2')
#> Error in getDirUserDataRoaming("subdir1", "subdir2"): could not find function "getDirUserDataRoaming"
# Returns 'C:/Users/User/AppData/Roaming/subdir1/subdir2'

getDirUserDataRoaming('filename.txt')
#> Error in getDirUserDataRoaming("filename.txt"): could not find function "getDirUserDataRoaming"
# Returns 'C:/Users/User/AppData/Roaming/filename.txt'

createDir('path/to/directory')
#> Error in createDir("path/to/directory"): could not find function "createDir"
```
