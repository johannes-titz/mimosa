# v0.5.0

- use shinytest2 for tests
- allow to select examples from menu
- add popular2 as example data set
- add hox poular2 as test
- test loading of datasets without gui
- add github actions check and coverage with badges
- improve Dockerfile
- get rid of plyr as dependency
- some helper functions are now exported because they are useful even without the gui
- add port and host as parameters for run_app
- simplify link for school example in help
- remove safari check as it seems to work fine now
- clean up interface code

# v0.4.0

- upstream problem with p-values for unstandardized coefficients was fixed
- update tests for unstandardized coefficients (see last bullet point)
- add JOSS doi and citation to readme and shiny page

# v0.3.0
- this is the release for JOSS
- fix typos
- add Maria Reichert as contributor
- add acknowledgements
- add non-reactive mode
- add busy spinner and notifcation
- filter independent variables
- copy-edit paper

# v0.2.0

- finding the grouping variable now works more reliably (mimosa now also checks for the number of average levels, not only for the number of level-2 variables)
- documentation was added to non-exported functions, which should make extensions easier
- in Safari all file endings are shown, while in the other browsers only .sav and .csv are shown
- a help box at start of mimosa shows some useful links
- the readme now includes a proper introduction and example
- a summary paper was added
- several new tests were added: identifying the grouping variable, Exam data estimates from mlmRev vignette, formula display

# v0.1.1

- fix problem with interaction formula in model display
- fix problem with NA variable names in model display
- make R package structure runable at shinyapps.io
- add test for model display

# v0.1.0

First Release