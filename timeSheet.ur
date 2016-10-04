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



signature SERVICE = sig
    type cellContent
    type rowContent	 
    type groupContent

    type cell = {ContentOption : option cellContent,
		 Save : option cellContent -> transaction unit}
		
    type row = {Content : rowContent,
		Cells : list cell}
	       
    type group = {Content : groupContent,
		  Rows : list row}
		 
    type sheet = {Dates : list time,
		  Groups : list group}

    val loadSheet : time -> int -> transaction sheet
end

signature MAKE_SERVICE_ARGUMENTS = sig
    con groupTablePrimaryKeyColumnName :: Name
    con groupTablePrimaryKeyColumnType :: Type
    val groupTablePrimaryKeyColumnTypeEq : eq groupTablePrimaryKeyColumnType
    val groupTablePrimaryKeyColumnTypeSqlInjectable : sql_injectable groupTablePrimaryKeyColumnType
    con groupTableOtherColumns :: {Type}
    constraint [groupTablePrimaryKeyColumnName] ~ groupTableOtherColumns
    con groupTableOtherConstraints :: {{Unit}}
    constraint [Pkey = [groupTablePrimaryKeyColumnName]] ~ groupTableOtherConstraints
    val groupTable : sql_table ([groupTablePrimaryKeyColumnName = groupTablePrimaryKeyColumnType] ++ groupTableOtherColumns)
			       ([Pkey = [groupTablePrimaryKeyColumnName]] ++ groupTableOtherConstraints)		     

    con rowTablePrimaryKeyColumnName :: Name
    con rowTablePrimaryKeyColumnType :: Type
    val rowTablePrimaryKeyColumnTypeEq : eq rowTablePrimaryKeyColumnType
    val rowTablePrimaryKeyColumnTypeSqlInjectable : sql_injectable rowTablePrimaryKeyColumnType
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
    val	cellTableOtherColumnsInjectable : $(map sql_injectable cellTableOtherColumns)
    val	cellTableOtherColumnsFolder : folder cellTableOtherColumns
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

functor MakeService (A : MAKE_SERVICE_ARGUMENTS) : SERVICE where type cellContent = $(A.cellTableOtherColumns)
                                                           where type rowContent = $(A.rowTableOtherColumns)
                                                           where type groupContent = $(A.groupTableOtherColumns) = struct
    open A
										     
    type cellContent = $(cellTableOtherColumns)
    type rowContent = $(rowTableOtherColumns)		      
    type groupContent = $(groupTableOtherColumns)

    type cell = {ContentOption : option cellContent,
		 Save : option cellContent -> transaction unit}
		
    type row = {Content : rowContent,
		Cells : list cell}
	       
    type group = {Content : groupContent,
		  Rows : list row}
		 
    type sheet = {Dates : list time,
		  Groups : list group}

    fun save (groupId : groupTablePrimaryKeyColumnType)
	     (rowId : rowTablePrimaryKeyColumnType)
	     (date : time)
	     (content : option cellContent) : transaction unit =
	case content of
	    None => return ()
	  | Some content => @Sql.easy_insertOrUpdate
			     [[cellTableGroupForeignKeyColumnName = _, cellTableRowForeignKeyColumnName = _, cellTableDateColumnName = _]]
			     ! _ cellTableOtherColumnsInjectable _ cellTableOtherColumnsFolder cellTable
			     ({cellTableGroupForeignKeyColumnName = groupId, cellTableRowForeignKeyColumnName = rowId, cellTableDateColumnName = date} ++ content)
	
    fun loadSheet (start : time) (count : int) : transaction sheet =
	let val startTime = midnight start
	    val endTime = sum startTime count
	    val dates = timeRange start (count - 1)
	in
	    cells <- queryL (SELECT
			       G.{groupTablePrimaryKeyColumnName} AS GroupId,
			       R.{rowTablePrimaryKeyColumnName} AS RowId,
			       C.{cellTableDateColumnName} AS Date,
			       C.{{cellTableOtherColumns}}
			     FROM groupTable AS G
			       INNER JOIN groupRowTable AS GR ON GR.{groupRowTableGroupForeignKeyColumnName} = G.{groupTablePrimaryKeyColumnName}
			       INNER JOIN rowTable AS R ON R.{rowTablePrimaryKeyColumnName} = GR.{groupRowTableRowForeignKeyColumnName}
			       INNER JOIN cellTable AS C ON C.{cellTableGroupForeignKeyColumnName} = G.{groupTablePrimaryKeyColumnName}
			       AND C.{cellTableRowForeignKeyColumnName} = R.{rowTablePrimaryKeyColumnName}
			     WHERE
			       {[startTime]} <= C.{cellTableDateColumnName}
			       AND C.{cellTableDateColumnName} <= {[endTime]});

	    groupIdRowPairs <- query (SELECT
					G.{groupTablePrimaryKeyColumnName} AS GroupId,
					R.{rowTablePrimaryKeyColumnName} AS RowId,
					R.{{rowTableOtherColumns}}
				      FROM groupTable AS G
					INNER JOIN groupRowTable AS GR ON GR.{groupRowTableGroupForeignKeyColumnName} = G.{groupTablePrimaryKeyColumnName}
					INNER JOIN rowTable AS R ON R.{rowTablePrimaryKeyColumnName} = GR.{groupRowTableRowForeignKeyColumnName})
				     (fn r groupIdRowPairs =>
					 let val cells = List.mp (fn date => {ContentOption = case List.find (fn cell => cell.GroupId = r.GroupId &&
															 cell.RowId = r.RowId &&
															 cell.Date = date)
													     cells of
												  None => None
												| Some cell => Some cell.C,
									      Save = save r.GroupId r.RowId date})
								 dates
					     val row = {Content = r.R, Cells = cells}
					     val groupIdRowPair = (r.GroupId, row)
					 in
					     return (groupIdRowPair :: groupIdRowPairs)
					 end)
				     [];
	    
	    groups <- query (SELECT
			       G.{groupTablePrimaryKeyColumnName} AS Id,
			       G.{{groupTableOtherColumns}}
			     FROM groupTable AS G)
			    (fn r groups =>
				let val groupIdRowPairs = List.filter (fn (groupId, _) => groupId = r.Id) groupIdRowPairs
				    val rows = List.mp (fn (_, row) => row) groupIdRowPairs
				    val group = {Content = r.G, Rows = rows}
				in
				    return (group :: groups)
				end)
			    [];

	    return {Dates = dates, Groups = groups}
	end
end



signature MODEL = sig
    type cellModelContent
    type rowModelContent
    type groupModelContent

    type cellModel = {Content : cellModelContent,
		      Save : cellModelContent -> transaction unit}

    type rowModel  = {Content : rowModelContent,
		      Cells : list cellModel}

    type groupModel  = {Content : groupModelContent,
			Rows : list rowModel}

    type sheetModel  = {DatesAndGroupsSource : source {Dates : list time,
						       Groups : list groupModel},
			Previous : transaction unit,
			Next : transaction unit,
			Minus : transaction unit,
			Plus : transaction unit}

    val loadSheetModel : time -> int -> transaction sheetModel
end

signature MAKE_MODEL_ARGUMENTS = sig
    structure Service : SERVICE

    type cellModelContent
    type rowModelContent
    type groupModelContent

    val convertCellContent : option Service.cellContent -> transaction cellModelContent
    val convertRowContent : Service.rowContent -> transaction rowModelContent
    val convertGroupContent : Service.groupContent -> transaction groupModelContent

    val convertCellModelContent : cellModelContent -> transaction (option Service.cellContent)
end
		  
functor MakeModel (A : MAKE_MODEL_ARGUMENTS) : MODEL where type cellModelContent = A.cellModelContent
                                                     where type rowModelContent = A.rowModelContent
                                                     where type groupModelContent = A.groupModelContent = struct
    open A
									       
    type cellModel = {Content : cellModelContent,
		      Save : cellModelContent -> transaction unit}

		
    type rowModel = {Content : rowModelContent,
		     Cells : list cellModel}

    type groupModel = {Content : groupModelContent,
		       Rows : list rowModel}

    type sheetModel = {DatesAndGroupsSource : source {Dates : list time,
						      Groups : list groupModel},
		       Previous : transaction unit,
		       Next : transaction unit,
		       Minus : transaction unit,
		       Plus : transaction unit}

    fun loadSheetModel (start : time) (count : int) : transaction sheetModel =
	datesAndGroupsSource <- source {Dates = [], Groups = []};

	let fun load (start : time) (count : int) : transaction unit =
		sheet <- rpc (Service.loadSheet start count);
		groups <- List.mapM (fn group =>
					content <- convertGroupContent group.Content;
					rows <- List.mapM (fn row =>
							      content <- convertRowContent row.Content;
							      cells <- List.mapM (fn (cell : Service.cell) =>
										     content <- convertCellContent cell.ContentOption;
										     let fun save (content : cellModelContent) : transaction unit =
											     contentOption <- convertCellModelContent content;
											     rpc (cell.Save contentOption)
										     in
											 return {Content = content, Save = save}
										     end) row.Cells;
							      return {Content = content, Cells = cells}) group.Rows;
					return {Content = content, Rows = rows}) sheet.Groups;
		set datesAndGroupsSource {Dates = sheet.Dates, Groups = groups}

	    val previous =		
		datesAndGroups <- get datesAndGroupsSource;
		case datesAndGroups.Dates of
		    start :: _ => let val count = List.length datesAndGroups.Dates
				      val start = sum start (0 - count)
				  in
				      load start count
				  end
		  | [] => return ()

	    val next =
		datesAndGroups <- get datesAndGroupsSource;
		case datesAndGroups.Dates of
		    start :: _ => let val count = List.length datesAndGroups.Dates
				      val start = sum start count
				  in
				      load start count
				  end
		  | [] => return ()				  			  
			  
	    val minus =
		datesAndGroups <- get datesAndGroupsSource;
		case datesAndGroups.Dates of
		    start :: _ :: _ => let val count = List.length datesAndGroups.Dates in
					   load start (count - 1)
				       end
		  | _ => return ()

	    val plus =
		datesAndGroups <- get datesAndGroupsSource;
		case datesAndGroups.Dates of
		    start :: _ :: _ => let val count = List.length datesAndGroups.Dates in
					   load start (count + 1)
				       end
		  | _ => return ()
		
	in
	    return {DatesAndGroupsSource  = datesAndGroupsSource,
		    Previous = previous,
		    Next  = next,
		    Minus = minus,
		    Plus = plus}
	end
end



signature VIEW = sig
    structure Model : MODEL
										      
    val sheetView : Model.sheetModel -> xbody
end

signature MAKE_VIEW_ARGUMENTS = sig
    structure Model : MODEL

    val cellView : Model.cellModelContent -> xbody
    val rowHeaderView : Model.rowModelContent -> xbody
    val groupHeaderView : Model.groupModelContent -> xbody
end

functor MakeView (A : MAKE_VIEW_ARGUMENTS) : VIEW = struct
    open A

    open Bootstrap3

    fun sheetView (sheetModel : Model.sheetModel) : xbody =
	let val s = datesAndGroups <- signal sheetModel.DatesAndGroupsSource;
		let val count = List.length datesAndGroups.Dates in
		    return <xml>
		      <table class="bs3_table table_bordered table_condensed table_responsive table_striped">
			<thead>
			  <tr>
			    <th rowspan=2>Project</th>
			    <th rowspan=2>Task</th>
			    <th colspan={count}>
			      <a class="glyphicon glyphicon_chevron_left" onclick={fn _ => sheetModel.Previous}/>
			      Date
			      <a class="glyphicon glyphicon_chevron_right" onclick={fn _ => sheetModel.Next}/>
			      <span class="pull_right">
				{if count > 1
				 then <xml><a class="glyphicon glyphicon_minus_sign" onclick={fn _ => sheetModel.Minus}/></xml>
				 else <xml></xml>}
				
				<a class="glyphicon glyphicon_plus_sign" onclick={fn _ => sheetModel.Plus}/>
			      </span>
			    </th>
			  </tr>
			  <tr>
			    {List.mapX (fn date =>
					   <xml>
					     <th>{[timef "%D" date]}</th>
					   </xml>) datesAndGroups.Dates}
			  </tr>
			</thead>
		      </table>
		    </xml>
		end
	in
	    <xml>
	      <dyn signal={s}/>
	    </xml>
	end
end

table projectTable : {Id : int, Nm : string, Ds : string} PRIMARY KEY Id,
      CONSTRAINT NM_IS_UNIQUE UNIQUE Nm      

table taskTable : {Id : int, Nm : string, Ds : string} PRIMARY KEY Id
      CONSTRAINT NM_IS_UNIQUE UNIQUE Nm

table projectTaskTable : {ProjectId : int, TaskId : int} PRIMARY KEY (ProjectId, TaskId),
      CONSTRAINT PROJECT_ID_IS_FOREIGN_KEY FOREIGN KEY ProjectId REFERENCES projectTable (Id),
      CONSTRAINT TASK_ID_IS_FOREIGN_KEY FOREIGN KEY TaskId REFERENCES taskTable (Id)

table entryTable : {ProjectId : int, TaskId : int, Date : time, Time : float} PRIMARY KEY (ProjectId, TaskId, Date),
      CONSTRAINT PROJECT_ID_IS_FOREIGN_KEY FOREIGN KEY ProjectId REFERENCES projectTable (Id),
      CONSTRAINT TASK_ID_IS_FOREIGN_KEY FOREIGN KEY TaskId REFERENCES taskTable (Id)

      
      
structure View = MakeView (struct
			       structure Model = MakeModel (struct
								structure Service = MakeService (struct
												     val groupTable = projectTable
												     val rowTable = taskTable
												     val groupRowTable = projectTaskTable
												     val cellTable = entryTable
												 end)
										    
								fun convertCellContent (contentOption : option {Time : float}) : transaction (source string) =
								    source (case contentOption of
										Some content => show content.Time
									      | None => "")
								    
								fun convertCellModelContent (contentSource : source string) : transaction (option {Time : float}) =
								    content <- get contentSource;
								    return (if content = ""
									    then None
									    else Some {Time = readError content})
								    
								fun convertRowContent (content : {Nm : string, Ds : string}) : transaction string =
								    return content.Nm
								    
								fun convertGroupContent (content : {Nm : string, Ds : string}) : transaction string =
								    return content.Nm
							    end)
						 
			       fun cellView (content : source string) : xbody = <xml>
				 <ctextbox source={content}/>
			       </xml>
										
			       fun rowHeaderView (content : string) : xbody = <xml>
				 {[content]}
			       </xml>
									      
			       fun groupHeaderView (content : string) : xbody = <xml>
				 {[content]}
			       </xml>
			   end)

fun application () : transaction page =
    start <- now;
    sheetModel <- View.Model.loadSheetModel start 7;
    return <xml>
	<body>
	  {View.sheetView sheetModel}
	</body>
    </xml>
