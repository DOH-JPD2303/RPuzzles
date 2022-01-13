library(tidyverse)

##All Possible Dice Rolls

all.possible <- 
  expand.grid(
    d4 = seq.int(1,4,by = 1),
    d6 = seq.int(1,6,by = 1),
    d8 = seq.int(1,8,by = 1)
  )

##Increasing Sequence
all.possible <- 
  all.possible %>%
  dplyr::mutate(
    win = 
      ifelse(d8 > d6 & d6 > d4,1,0)
  )

##Total Rows
sum(all.possible$win)
25

##Percentage
sum(all.possible$win) / nrow(all.possible)
0.25



# SQL Solution ------------------------------------------------------------

library(DBI)
library(glue)

DBI::dbExecute(
  conn = con,
  glue::glue_sql(
    "
      -- Drop table if it exists

    IF OBJECT_ID('PMP_Staging.dbo.D4', 'u') IS NOT NULL
  DROP TABLE PMP_Staging.dbo.D4;

IF OBJECT_ID('PMP_Staging.dbo.D6', 'u') IS NOT NULL
  DROP TABLE PMP_Staging.dbo.D6;

IF OBJECT_ID('PMP_Staging.dbo.D8', 'u') IS NOT NULL
  DROP TABLE PMP_Staging.dbo.D8;
    
  --create tables for dices   
    
    CREATE TABLE PMP_Staging.dbo.D4 
      (
      d4 int
      );
      
    CREATE TABLE PMP_Staging.dbo.D6 
      (
      d6 int
      );
      
    CREATE TABLE PMP_Staging.dbo.D8 
      (
      d8 int
      );
      
  --insert faces as integers
    INSERT PMP_Staging.dbo.D4 (d4)   
    VALUES (1) ;  
     INSERT PMP_Staging.dbo.D4 (d4)   
    VALUES (2) ;  
     INSERT PMP_Staging.dbo.D4 (d4)   
    VALUES (3) ;  
     INSERT PMP_Staging.dbo.D4 (d4)   
    VALUES (4) ; 

     INSERT PMP_Staging.dbo.D6 (d6)   
    VALUES (1) ;  
     INSERT PMP_Staging.dbo.D6 (d6)   
    VALUES (2) ;  
     INSERT PMP_Staging.dbo.D6 (d6)   
    VALUES (3) ;  
     INSERT PMP_Staging.dbo.D6 (d6)   
    VALUES (4) ; 
    INSERT PMP_Staging.dbo.D6 (d6)   
    VALUES (5) ;  
     INSERT PMP_Staging.dbo.D6 (d6)   
    VALUES (6) ; 
    
     INSERT PMP_Staging.dbo.D8 (d8)   
    VALUES (1) ;  
     INSERT PMP_Staging.dbo.D8 (d8)   
    VALUES (2) ;  
     INSERT PMP_Staging.dbo.D8 (d8)   
    VALUES (3) ;  
     INSERT PMP_Staging.dbo.D8 (d8)   
    VALUES (4) ; 
    INSERT PMP_Staging.dbo.D8 (d8)   
    VALUES (5) ;  
     INSERT PMP_Staging.dbo.D8 (d8)   
    VALUES (6) ; 
    INSERT PMP_Staging.dbo.D8 (d8)   
    VALUES (7) ;  
     INSERT PMP_Staging.dbo.D8 (d8)   
    VALUES (8) ; 
    
    "
  )
)

## join all possible combinations and keep just increasing orders 

results <- 
  DBI::dbGetQuery(
    conn = con,
    glue::glue_sql(
      "
    SELECT 
    COUNT(*) as wins
    FROM 
       PMP_Staging.dbo.D4 
        JOIN 
          PMP_Staging.dbo.D6 on 1 = 1
        JOIN 
          PMP_Staging.dbo.D8 on 1 = 1
    WHERE d8 > d6 AND d6 > d4
   ;

    "
    )
  )

DBI::dbExecute(
  conn = con,
  glue::glue_sql(
    "
      -- Drop table if it exists

    IF OBJECT_ID('PMP_Staging.dbo.D4', 'u') IS NOT NULL
  DROP TABLE PMP_Staging.dbo.D4;

IF OBJECT_ID('PMP_Staging.dbo.D6', 'u') IS NOT NULL
  DROP TABLE PMP_Staging.dbo.D6;

IF OBJECT_ID('PMP_Staging.dbo.D8', 'u') IS NOT NULL
  DROP TABLE PMP_Staging.dbo.D8;

    "
  )
)

