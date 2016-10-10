table userTable : {Id : int, Nm : string} PRIMARY KEY Id,
      CONSTRAINT NM_IS_UNIQUE UNIQUE Nm

table projectTable : {Id : int, Nm : string, Ds : string, UserId : int} PRIMARY KEY Id,
      CONSTRAINT NM_IS_UNIQUE UNIQUE Nm,
      CONSTRAINT USER_ID_IS_FOREIGN_KEY FOREIGN KEY UserId REFERENCES userTable (Id)

table taskTable : {Id : int, Nm : string, Ds : string, UserId : int} PRIMARY KEY Id,
      CONSTRAINT NM_IS_UNIQUE UNIQUE Nm,
      CONSTRAINT USER_ID_IS_FOREIGN_KEY FOREIGN KEY UserId REFERENCES userTable (Id)      

table projectTaskTable : {ProjectId : int, TaskId : int} PRIMARY KEY (ProjectId, TaskId),
      CONSTRAINT PROJECT_ID_IS_FOREIGN_KEY FOREIGN KEY ProjectId REFERENCES projectTable (Id),
      CONSTRAINT TASK_ID_IS_FOREIGN_KEY FOREIGN KEY TaskId REFERENCES taskTable (Id)

table entryTable : {ProjectId : int, TaskId : int, Date : time, Time : float} PRIMARY KEY (ProjectId, TaskId, Date),
      CONSTRAINT PROJECT_ID_IS_FOREIGN_KEY FOREIGN KEY ProjectId REFERENCES projectTable (Id),
      CONSTRAINT TASK_ID_IS_FOREIGN_KEY FOREIGN KEY TaskId REFERENCES taskTable (Id)

structure Service = TimeSheet.MakeService (struct
					       val groupTable = projectTable
					       val rowTable = taskTable
					       val groupRowTable = projectTaskTable
					       val cellTable = entryTable
					       val groupQueryPredicate = (WHERE G.UserId = 1)
					       val rowQueryPredicate = (WHERE G.UserId = 1 AND R.UserId = 1)
					       val cellQueryPredicate = (WHERE G.UserId = 1 AND R.UserId = 1)
					   end)

structure Model = TimeSheet.MakeModel (struct
					   structure Service = Service
							       
					   fun convertCellContent (contentOption : option {Time : float}) : transaction ({Id : id, ValueSource : source string}) =
					       id <- fresh;
					       valueSource <- source (case contentOption of
									  Some content => show content.Time
									| None => "");
					       return {Id = id, ValueSource = valueSource}
					       
					   fun convertCellModelContent (content : {Id : id, ValueSource : source string}) : transaction (option {Time : float}) =
					       content <- get content.ValueSource;
					       return (if content = ""
						       then None
						       else Some {Time = readError content})
					       
					   fun convertRowContent (content : [[Nm] ~ [Ds, UserId]] => {Nm : string, Ds : string, UserId : int}) : transaction string =
					       return content.Nm
					       
					   fun convertGroupContent (content : [[Nm] ~ [Ds, UserId]] => {Nm : string, Ds : string, UserId : int}) : transaction string =
					       return content.Nm
				       end)

ffi blur: id -> transaction unit

structure View = TimeSheet.MakeView (struct
					 structure Model = Model
					 
					 fun cellView (id : Model.cellModelId) (content : {Id : id, ValueSource : source string}) : xbody = <xml>
					   <ctextbox id={content.Id} source={content.ValueSource} onkeyup={fn event => if event.KeyCode = 13 then
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
				     end)

structure S0 = TimeSheet.MakeS0 (View)
	       
structure Theme = Ui.Make(struct
			      val css = {Bootstrap = bless "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css",
					 Sample = bless "/Sample/sample.css",
					 Upo = bless "/style.css"}
			      val icon = None
			  end)

val application = Theme.simple "Time Sheet" S0.ui
