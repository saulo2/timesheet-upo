delete from uw_Sample_projectTaskTable;
delete from uw_Sample_taskTable;
delete from uw_Sample_projectTable;
delete from uw_Sample_userTable;

insert into uw_Sample_userTable (uw_id, uw_nm) values (1, 'Saulo Medeiros de Araujo');

insert into uw_Sample_projectTable (uw_id, uw_nm, uw_ds, uw_userid) values (1, 'Project 01', 'Project 01', 1);
insert into uw_Sample_projectTable (uw_id, uw_nm, uw_ds, uw_userid) values (2, 'Project 02', 'Project 02', 1);
insert into uw_Sample_projectTable (uw_id, uw_nm, uw_ds, uw_userid) values (3, 'Project 03', 'Project 03', 1);
insert into uw_Sample_projectTable (uw_id, uw_nm, uw_ds, uw_userid) values (4, 'Project 04', 'Project 04', 1);
insert into uw_Sample_projectTable (uw_id, uw_nm, uw_ds, uw_userid) values (5, 'Project 05', 'Project 05', 1);
insert into uw_Sample_projectTable (uw_id, uw_nm, uw_ds, uw_userid) values (6, 'Project 06', 'Project 06', 1);
insert into uw_Sample_projectTable (uw_id, uw_nm, uw_ds, uw_userid) values (7, 'Project 07', 'Project 07', 1);
insert into uw_Sample_projectTable (uw_id, uw_nm, uw_ds, uw_userid) values (8, 'Project 08', 'Project 08', 1);
insert into uw_Sample_projectTable (uw_id, uw_nm, uw_ds, uw_userid) values (9, 'Project 09', 'Project 09', 1);
insert into uw_Sample_projectTable (uw_id, uw_nm, uw_ds, uw_userid) values (10, 'Project 10', 'Project 10', 1);

insert into uw_Sample_taskTable (uw_id, uw_nm, uw_ds, uw_userid) values (1, 'Task 01', 'Task 01', 1);
insert into uw_Sample_taskTable (uw_id, uw_nm, uw_ds, uw_userid) values (2, 'Task 02', 'Task 02', 1);
insert into uw_Sample_taskTable (uw_id, uw_nm, uw_ds, uw_userid) values (3, 'Task 03', 'Task 03', 1);
insert into uw_Sample_taskTable (uw_id, uw_nm, uw_ds, uw_userid) values (4, 'Task 04', 'Task 04', 1);
insert into uw_Sample_taskTable (uw_id, uw_nm, uw_ds, uw_userid) values (5, 'Task 05', 'Task 05', 1);
insert into uw_Sample_taskTable (uw_id, uw_nm, uw_ds, uw_userid) values (6, 'Task 06', 'Task 06', 1);
insert into uw_Sample_taskTable (uw_id, uw_nm, uw_ds, uw_userid) values (7, 'Task 07', 'Task 07', 1);
insert into uw_Sample_taskTable (uw_id, uw_nm, uw_ds, uw_userid) values (8, 'Task 08', 'Task 08', 1);
insert into uw_Sample_taskTable (uw_id, uw_nm, uw_ds, uw_userid) values (9, 'Task 09', 'Task 09', 1);
insert into uw_Sample_taskTable (uw_id, uw_nm, uw_ds, uw_userid) values (10, 'Task 10', 'Task 10', 1);

insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (1, 1);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (1, 2);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (1, 3);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (1, 4);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (1, 5);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (1, 6);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (1, 7);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (1, 8);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (1, 9);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (1, 10);

insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (2, 1);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (2, 2);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (2, 3);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (2, 4);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (2, 5);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (2, 6);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (2, 7);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (2, 8);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (2, 9);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (2, 10);

insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (3, 1);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (3, 2);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (3, 3);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (3, 4);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (3, 5);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (3, 6);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (3, 7);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (3, 8);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (3, 9);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (3, 10);

insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (4, 1);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (4, 2);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (4, 3);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (4, 4);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (4, 5);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (4, 6);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (4, 7);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (4, 8);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (4, 9);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (4, 10);

insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (5, 1);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (5, 2);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (5, 3);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (5, 4);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (5, 5);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (5, 6);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (5, 7);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (5, 8);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (5, 9);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (5, 10);

insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (6, 1);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (6, 2);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (6, 3);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (6, 4);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (6, 5);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (6, 6);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (6, 7);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (6, 8);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (6, 9);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (6, 10);

insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (7, 1);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (7, 2);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (7, 3);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (7, 4);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (7, 5);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (7, 6);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (7, 7);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (7, 8);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (7, 9);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (7, 10);

insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (8, 1);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (8, 2);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (8, 3);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (8, 4);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (8, 5);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (8, 6);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (8, 7);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (8, 8);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (8, 9);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (8, 10);

insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (9, 1);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (9, 2);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (9, 3);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (9, 4);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (9, 5);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (9, 6);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (9, 7);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (9, 8);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (9, 9);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (9, 10);

insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (10, 1);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (10, 2);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (10, 3);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (10, 4);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (10, 5);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (10, 6);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (10, 7);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (10, 8);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (10, 9);
insert into uw_Sample_projectTaskTable (uw_projectid, uw_taskid) values (10, 10);