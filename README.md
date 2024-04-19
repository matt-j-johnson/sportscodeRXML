This is a brief package with functions that serve to import and export SportsCode xml files. 

Install using devtools::install_github('matt-j-johnson/sportscodeRXML')

It is designed to handle the xml structure of SportsCode files, and import them into a wide format, whilst still retaining ID, start and end values for each code to then allow for future export. Each code becomes a single row, with each label-group a column. Each label in a group is the value. You can also import the colours for each instance to allow for consistent colouring on export. 
Exporting requires the R tibble/dataframe to be in a similar structure to how it was imported, with an option to include the colour data frame. 
