(*
signature TIMESHEET = sig
    type content

    type cell = {Time : time, Content : option content, Save : option content -> transaction unit}
    type row = {Label : string, Cells : list cell}
    type group = {Label : string, Rows : list row}
    type timesheet = {Header : list time, Groups : list group}

    val loadTimesheet : time -> int -> transaction timesheet
end

functor Timesheet (M : sig
		       con cellContentType :: {Type}
		   end) : TIMESHEET = struct
    type content = $(M.cellContentType)

    type cell = {Time : time, Content : option content, Save : option content -> transaction unit}

    type row = {Label : string, Cells : list cell}

    type group = {Label : string, Rows : list row}

    type timesheet = {Header : list time, Groups : list group}

    fun loadTimesheet (start : time) (count : int): transaction timesheet =
	return {Header = [], Groups = []}
end
*)

functor Make (M : sig
		  con groupTablePrimaryKeyColumnName :: Name
		  con groupTablePrimaryKeyColumnType :: Type
		  con groupTableOtherColumns :: {Type}
		  constraint [groupTablePrimaryKeyColumnName] ~ groupTableOtherColumns
		  con groupTableOtherConstraints :: {{Unit}}
		  constraint [Pkey = [groupTablePrimaryKeyColumnName]] ~ groupTableOtherConstraints
		  val groupTable : sql_table ([groupTablePrimaryKeyColumnName = groupTablePrimaryKeyColumnType] ++ groupTableOtherColumns)
					     ([Pkey = [groupTablePrimaryKeyColumnName]] ++ groupTableOtherConstraints)

		  con rowTablePrimaryKeyColumnName :: Name
		  con rowTablePrimaryKeyColumnType :: Type
		  con rowTableOtherColumns :: {Type}
		  constraint [rowTablePrimaryKeyColumnName] ~ rowTableOtherColumns
		  con rowTableOtherConstraints :: {{Unit}}
		  constraint [Pkey = [rowTablePrimaryKeyColumnName]] ~ rowTableOtherConstraints
		  val rowTable : sql_table ([rowTablePrimaryKeyColumnName = rowTablePrimaryKeyColumnType] ++ rowTableOtherColumns)
					   ([Pkey = [rowTablePrimaryKeyColumnName]] ++ rowTableOtherConstraints)

		  con groupRowTableGroupForeignKeyName :: Name
		  con groupRowTableRowForeignKeyName :: Name
		  constraint [groupRowTableGroupForeignKeyName] ~ [groupRowTableRowForeignKeyName]
		  con groupRowTableOtherColumns :: {Type}
		  constraint [groupRowTableGroupForeignKeyName, groupRowTableRowForeignKeyName] ~ groupRowTableOtherColumns
		  con groupRowTableOtherConstraints :: {{Unit}}
		  constraint [Pkey = [groupRowTableGroupForeignKeyName, groupRowTableRowForeignKeyName]] ~ groupRowTableOtherConstraints
		  val groupRowTable : sql_table ([groupRowTableGroupForeignKeyName = groupTablePrimaryKeyColumnType,
						  groupRowTableRowForeignKeyName = rowTablePrimaryKeyColumnType] ++ groupRowTableOtherColumns)
						([Pkey = [groupRowTableGroupForeignKeyName,
							  groupRowTableRowForeignKeyName]] ++ groupRowTableOtherConstraints)

		  con cellTableGroupForeignKeyName :: Name
		  con cellTableRowForeignKeyName :: Name
		  con cellTableDateColumnName :: Name
		  constraint [cellTableGroupForeignKeyName] ~ [cellTableRowForeignKeyName]
		  constraint [cellTableGroupForeignKeyName] ~ [cellTableDateColumnName]
		  constraint [cellTableRowForeignKeyName] ~ [cellTableDateColumnName]
		  con cellTableOtherColumns :: {Type}
		  constraint [cellTableGroupForeignKeyName, cellTableRowForeignKeyName, cellTableDateColumnName] ~ cellTableOtherColumns
		  con cellTableOtherConstraints :: {{Unit}}
		  constraint [Pkey = [cellTableGroupForeignKeyName, cellTableRowForeignKeyName]] ~ cellTableOtherConstraints
		  val cellTable : sql_table ([cellTableGroupForeignKeyName = groupTablePrimaryKeyColumnType,
					      cellTableRowForeignKeyName = rowTablePrimaryKeyColumnType,
					      cellTableDateColumnName = time] ++ cellTableOtherColumns)
					    ([Pkey = [cellTableGroupForeignKeyName,
						      cellTableRowForeignKeyName,
						      cellTableDateColumnName]] ++ cellTableOtherConstraints)
              end) = struct
    open M

    fun loadGroups () =
	gs <- queryL (SELECT
			GR.{groupRowTableGroupForeignKeyName},
			R.*
		      FROM groupRowTable AS GR
			INNER JOIN rowTable AS R ON GR.{groupRowTableRowForeignKeyName} = R.{rowTablePrimaryKeyColumnName}
		     );
	let val t : int = gs in
	    return t
	end
end

table projectTable : {Id : int, Nm : string} PRIMARY KEY Id,
      CONSTRAINT NM_IS_UNIQUE UNIQUE Nm      

table taskTable : {Id : int, Nm : string} PRIMARY KEY Id
      CONSTRAINT NM_IS_UNIQUE UNIQUE Nm

table projectTaskTable : {ProjectId : int, TaskId : int} PRIMARY KEY (ProjectId, TaskId),
      CONSTRAINT PROJECT_ID_IS_FOREIGN_KEY FOREIGN KEY ProjectId REFERENCES projectTable (Id),
      CONSTRAINT TASK_ID_IS_FOREIGN_KEY FOREIGN KEY TaskId REFERENCES taskTable (Id)

table entryTable : {ProjectId : int, TaskId : int, Date : time, Time : float} PRIMARY KEY (ProjectId, TaskId, Date),
      CONSTRAINT PROJECT_ID_IS_FOREIGN_KEY FOREIGN KEY ProjectId REFERENCES projectTable (Id),
      CONSTRAINT TASK_ID_IS_FOREIGN_KEY FOREIGN KEY TaskId REFERENCES taskTable (Id)

structure M1 = Make (struct
			 val groupTable = projectTable
			 val rowTable = taskTable
			 val groupRowTable = projectTaskTable
			 val cellTable = entryTable
		     end)
	       
structure M2 = Make (struct
			 val groupTable = taskTable
			 val rowTable = projectTable
			 val groupRowTable = projectTaskTable
			 val cellTable = entryTable
		     end)
