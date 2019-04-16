# configure your ".Renviron" and ".Rprofile"
Ref: https://csgillespie.github.io/efficientR/3-3-r-startup.html#rprofile

#: ----1, .Rprofile----
# use this to save customized functions, treat this file as a pre-run R script before your R script.


#: ----2, .Renviron----
# use this to save your (cross-project) environmental variables, such for lib path, API and credentials.

#: ----3, three locations to put above .Rxxxx files----
Files in three folders are important in this process:

R_HOME, the directory in which R is installed. The etc sub-directory can contain start-up files read early on in the start-up process. Find out where your R_HOME is with the R.home() command.

HOME, the user’s home directory. Typically this is /home/username on Unix machines or C:\Users\username on Windows (since Windows 7). Ask R where your home directory with, path.expand("~") (note the use of the Unix-like tilde to represent the home directory).

R’s current working directory. This is reported by getwd().

It is important to know the location of the .Rprofile and .Renviron set-up files that are being used out of these three options. R only uses one  .Rprofile and one .Renviron in any session: if you have a .Rprofile file in your current project, R will ignore .Rprofile in R_HOME and  HOME. Likewise, .Rprofile in HOME overrides .Rprofile in R_HOME. The same applies to .Renviron: you should remember that adding project specific environment variables with .Renviron will de-activate other .Renviron files.

To create a project-specific start-up script, simply create a .Rprofile file in the project’s root directory and start adding R code, e.g. via  file.edit(".Rprofile"). Remember that this will make .Rprofile in the home directory be ignored. The following commands will open your  .Rprofile from within an R editor:

file.edit(file.path("~", ".Rprofile")) # edit .Rprofile in HOME
file.edit(".Rprofile") # edit project specific .Rprofile