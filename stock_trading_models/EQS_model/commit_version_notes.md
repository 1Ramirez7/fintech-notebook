This is code for a shiny app done with r code in r studio. the app is a bloomberg EQS style model. 

**7-commit setting default starting version for github projects-7**

- This commit futher set this folder to host a more structure EQS folder. 
- I move some files the storage repository.
  
  
  **8-commit added version 1.5 for EQS model 8**
  
 - organize the code by section
 - added option to filter by any variable and filter can also be null.
 - I also added python_test.qmd to test python code and added code to gte sector and industry info for stocks
 
 
**9-commit added version 1.6 for EQS model 9**

- This model runs multiple rank loops base if filter column was selected (eg. sector industry etc).
- So if Sector is selected it will run individual ranks for each sector.
- This prevents ranking sectors who financial metrics are not comparable to each other.

