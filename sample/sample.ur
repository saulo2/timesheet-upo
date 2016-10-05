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

ffi blur: id -> transaction unit
				 
structure S0 = TimeSheet.MakeS0 (
	       TimeSheet.MakeView (
	       struct
		   structure Model = TimeSheet.MakeModel (
				     struct
					 structure Service = TimeSheet.MakeService (
							     struct
								 val groupTable = projectTable
								 val rowTable = taskTable
								 val groupRowTable = projectTaskTable
								 val cellTable = entryTable
							     end)
															
					 fun convertCellContent (contentOption : option {Time : float}) : transaction ({Id : id, ValueSource : source string}) =
					     id <- fresh;
					     valueSource <- source (case contentOption of
									Some content => show content.Time
								      | None => "");
					     return {Id = id,
						     ValueSource = valueSource}
					     
					 fun convertCellModelContent (content : {Id : id, ValueSource : source string}) : transaction (option {Time : float}) =
					     content <- get content.ValueSource;
					     return (if content = ""
						     then None
						     else Some {Time = readError content})
					     
					 fun convertRowContent (content : {Nm : string, Ds : string}) : transaction string =
					     return content.Nm
					     
					 fun convertGroupContent (content : {Nm : string, Ds : string}) : transaction string =
					     return content.Nm
				     end)
				     
		   fun cellView (id : Model.cellModelId) (content : {Id : id, ValueSource : source string}) : xbody = <xml>
		     <ctextbox source={content.ValueSource} onkeyup={fn event => if event.KeyCode = 13 then
										     _ <- Model.saveCellModel id content;
										     blur content.Id
										 else
										     return ()}/>
		     </xml>
														      
		   fun rowHeaderView (content : string) : xbody = <xml>
		     {[content]}
		   </xml>
								  
		   fun groupHeaderView (content : string) : xbody = <xml>
		     {[content]}
		   </xml>
	       end))
	       
structure Theme = Ui.Make(
		  struct
		      val css = {Bootstrap = bless "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css",
				 Sample = bless "/Sample/sample.css",
				 Upo = bless "/style.css"}
		      val icon = None
		  end)

val application = Theme.simple "TimeSheet" S0.ui

(*		  
open Bootstrap3

fun application () : transaction page =
    sheetModel <- View.Model.makeSheetModel;
    return <xml>
      <head>		  
	<link rel="stylesheet" type="text/css" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"/>
	<link rel="stylesheet" type="text/css" href="/TimeSheet/timeSheet.css"/>

      </head>
      <body style="padding-top: 50px" onload={start <- now;
					      sheetModel.Load start 7}>
	<div class="container">
	  <div class="row">
	    <div class="col-sm-12">
	      {View.sheetView sheetModel}
	    </div>
	  </div>
	</div>
      </body>
    </xml>
*)		  
