
Folder: sql/

- This folder has the tables for my future OLAP database. 
- Also has a few duplicates of the index file to not read entire list when downloading data from yahoo. 

Folder: data/ 

- Folder mostly has sample csv files used for testing purposes.
- as of 6-27-25, no needed file is in this folder
- fetch_data.qmd uses/makes this files. 


fetch_data.qmd

This qmd file has the code that builds tables for OLAP! Final version of some codes here are in my fetch_data.ipynb file. 

THIS FILE IS BEING USE AS A MASTER FOR MAKING ALL INITIAL TABLES FOR THE OLAP!!!!


- yahoo finance options
- sample code to fetch data from yahoo finance

OLAP_structure.word 

- file has structure of the OLAP database
- it hasnt been updated, so needs updating
- the snowflake.rnd kinda replaces this, but maybe I still need text structure. 

**r_code_test.qmd**

- This file is to test r code only. 
- code shoul not remain in this folder.

snowflake.mySQL

- structure to my data base
- This RND view has all tables in my database
- The data types in this file and relationships will be offical. 

**snowflake.mwb.bak**

- file for snowflake.mySQL

**stock_screening.qmd**

- This file has stock screening code
- it is my main file when using r code

has the following: load stock data, time series plot, top losers & winners, info about file loaded, test download prices, 


**useful_or_old_code.qmd**

- has old code use in previous files
- has some other code that be useful but not needed.