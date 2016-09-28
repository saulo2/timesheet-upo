CREATE TABLE uw_TimeSheet_projectTable(uw_id int8 NOT NULL, uw_nm text NOT NULL,
 PRIMARY KEY (uw_id),
  CONSTRAINT uw_TimeSheet_projectTable_NM_IS_UNIQUE UNIQUE (uw_nm)
 );
 
 CREATE TABLE uw_TimeSheet_taskTable(uw_id int8 NOT NULL, uw_nm text NOT NULL,
  PRIMARY KEY (uw_id),
   CONSTRAINT uw_TimeSheet_taskTable_NM_IS_UNIQUE UNIQUE (uw_nm)
  );
  
  CREATE TABLE uw_TimeSheet_projectTaskTable(uw_projectid int8 NOT NULL, 
                                              uw_taskid int8 NOT NULL,
   PRIMARY KEY (uw_taskId, uw_projectId),
    CONSTRAINT uw_TimeSheet_projectTaskTable_PROJECT_ID_IS_FOREIGN_KEY
     FOREIGN KEY (uw_projectId) REFERENCES uw_TimeSheet_projectTable (uw_id),
                                                                             
     CONSTRAINT uw_TimeSheet_projectTaskTable_TASK_ID_IS_FOREIGN_KEY
      FOREIGN KEY (uw_taskId) REFERENCES uw_TimeSheet_taskTable (uw_id)
   );
   
   CREATE TABLE uw_TimeSheet_entryTable(uw_projectid int8 NOT NULL, 
                                         uw_taskid int8 NOT NULL, 
                                         uw_date timestamp NOT NULL, 
                                         uw_time float8 NOT NULL,
    PRIMARY KEY (uw_date, uw_projectId, uw_taskId),
     CONSTRAINT uw_TimeSheet_entryTable_PROJECT_ID_IS_FOREIGN_KEY
      FOREIGN KEY (uw_projectId) REFERENCES uw_TimeSheet_projectTable (uw_id),
                                                                              
      CONSTRAINT uw_TimeSheet_entryTable_TASK_ID_IS_FOREIGN_KEY
       FOREIGN KEY (uw_taskId) REFERENCES uw_TimeSheet_taskTable (uw_id)
    );
    
    