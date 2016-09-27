signature SCHEMA = sig
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
		   
    con groupRowTableGroupForeignKeyColumnName :: Name
    con groupRowTableRowForeignKeyColumnName :: Name
    constraint [groupRowTableGroupForeignKeyColumnName] ~ [groupRowTableRowForeignKeyColumnName]
    con groupRowTableOtherColumns :: {Type}
    constraint [groupRowTableGroupForeignKeyColumnName, groupRowTableRowForeignKeyColumnName] ~ groupRowTableOtherColumns
    con groupRowTableOtherConstraints :: {{Unit}}
    constraint [Pkey = [groupRowTableGroupForeignKeyColumnName, groupRowTableRowForeignKeyColumnName]] ~ groupRowTableOtherConstraints
    val groupRowTable : sql_table ([groupRowTableGroupForeignKeyColumnName = groupTablePrimaryKeyColumnType,
				    groupRowTableRowForeignKeyColumnName = rowTablePrimaryKeyColumnType] ++ groupRowTableOtherColumns)
				  ([Pkey = [groupRowTableGroupForeignKeyColumnName,
					    groupRowTableRowForeignKeyColumnName]] ++ groupRowTableOtherConstraints)

    con cellTableGroupForeignKeyColumnName :: Name
    con cellTableRowForeignKeyColumnName :: Name
    con cellTableDateColumnName :: Name
    constraint [cellTableGroupForeignKeyColumnName] ~ [cellTableRowForeignKeyColumnName]
    constraint [cellTableGroupForeignKeyColumnName] ~ [cellTableDateColumnName]
    constraint [cellTableRowForeignKeyColumnName] ~ [cellTableDateColumnName]
    con cellTableOtherColumns :: {Type}
    constraint [cellTableGroupForeignKeyColumnName, cellTableRowForeignKeyColumnName, cellTableDateColumnName] ~ cellTableOtherColumns
    con cellTableOtherConstraints :: {{Unit}}
    constraint [Pkey = [cellTableGroupForeignKeyColumnName, cellTableRowForeignKeyColumnName]] ~ cellTableOtherConstraints
    val cellTable : sql_table ([cellTableGroupForeignKeyColumnName = groupTablePrimaryKeyColumnType,
				cellTableRowForeignKeyColumnName = rowTablePrimaryKeyColumnType,
				cellTableDateColumnName = time] ++ cellTableOtherColumns)
			      ([Pkey = [cellTableGroupForeignKeyColumnName,
					cellTableRowForeignKeyColumnName,
					cellTableDateColumnName]] ++ cellTableOtherConstraints)
end

signature SERVICE = sig
    type content

    type cell = {Content : option content, Save : option content -> transaction unit}
    type row = {Label : string, Cells : list cell}
    type group = {Label : string, Rows : list row}
    type sheet = {Header : list time, Groups : list group}

    val loadSheet : time -> int -> transaction sheet
end

functor Service (Schema : SCHEMA) : SERVICE = struct
    open Schema
    
    type content = $(cellTableOtherColumns)

    type cell = {Content : option content, Save : option content -> transaction unit}
    type row = {Label : string, Cells : list cell}
    type group = {Label : string, Rows : list row}
    type sheet = {Header : list time, Groups : list group}

    fun midnight (time: time): time =
	let val t = Datetime.fromTime time in
	    Datetime.toTime {Year = t.Year, Month = t.Month, Day = t.Day, Hour = 0, Minute = 0, Second = 0}
	end

    fun sum (time: time) (days: int): time =
	Datetime.toTime (Datetime.addDays days (Datetime.fromTime time))

    fun intRange (i: int) (j: int): list int =
	if i > j
	then []
	else i :: (intRange (i + 1) j)

    fun timeRange (start: time) (count: int): list time =
	List.mp (fn days => sum start days) (intRange 0 count)

    fun loadSheet (start : time) (count : int) : transaction sheet =
	let val startTime = midnight start
	    val endTime = sum startTime count
	    val header = timeRange start (count - 1)
	in
	    cellRecords <- queryL (SELECT
				     C.{cellTableGroupForeignKeyColumnName} AS GroupId,
				     C.{cellTableRowForeignKeyColumnName} AS RowId,
				     C.{{cellTableOtherColumns}}
				   FROM groupTable AS G
				     INNER JOIN groupRowTable AS GR ON GR.{groupRowTableGroupForeignKeyColumnName} = G.{groupTablePrimaryKeyColumnName}
				     INNER JOIN rowTable AS R ON R.{rowTablePrimaryKeyColumnName} = GR.{groupRowTableRowForeignKeyColumnName}
				     INNER JOIN cellTable AS C ON C.{cellTableGroupForeignKeyColumnName} = G.{groupTablePrimaryKeyColumnName}
				     AND C.{cellTableRowForeignKeyColumnName} = R.{rowTablePrimaryKeyColumnName}
				   WHERE
				     {[startTime]} <= C.{cellTableDateColumnName}
				     AND C.{cellTableDateColumnName} <= {[endTime]});
	    
	    groupIdRowsPairs <- query (SELECT
					 G.{}
				       FROM groupTable AS G
					 INNER JOIN groupRowTable AS GR ON GR.{groupRowTableGroupForeignKeyColumnName} = G.{groupTablePrimaryKeyColumnName}
					 INNER JOIN rowTable AS R ON R.{rowTablePrimaryKeyColumnName} = GR.{groupRowTableRowForeignKeyColumnName})
				      (fn r projectIdTaskRowPairs =>
					  let val entryCells = List.mp (fn date => case List.find (fn entry => entry.PROJECT_ID = r.PROJECT_ID &&
														 entry.TASK_ID = r.ID &&
													       entry.DATE = date) entries of
										       None => (date, None)
										     | Some entry => (date, Some entry.TIME)) dates
					      val taskRow = (r.ID, r.NAME, r.VISIBLE, entryCells)
					      val projectIdTaskRowPair = (r.PROJECT_ID, taskRow)
					    in
					      return (projectIdTaskRowPair :: projectIdTaskRowPairs)
					  end) []
	    
	    let val t = cellRecords in
		return {Header = header, Groups = []}
	    end
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

structure Service1 = Service (struct
				  val groupTable = projectTable
				  val rowTable = taskTable
				  val groupRowTable = projectTaskTable
				  val cellTable = entryTable
			      end)

structure Service2 = Service (struct
				  val groupTable = taskTable
				  val rowTable = projectTable
				  val groupRowTable = projectTaskTable
				  val cellTable = entryTable
			      end)
