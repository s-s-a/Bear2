*:********************************************************************
*:
*:	Procedure file: bear2m.prg
*:	Contents:
*:		Class		Bear AS Session
*:		Method:	Init(lcFileName)
*:		Method:	Destroy
*:		Method:	LoadFiles
*:		Method:	LoadStrings(lcFileName)
*:		Method:	Process
*:		Method:	CheckWord(lcWord,lcTemplate,lnWordLen)
*:		Method:	Put(lcStr)
*:		Method:	PutH(lcStr)
*:		Method:	PutE(lcStr)
*:		Method:	Save
*:		Method:	FileHeader
*:		Method:	FileFooter
*:		Method:	ClassHeader(lcClassName,
*:		Method:	ProcHeader(lcName,lnType)
*:		Method:	MethodHeader(lcName)
*:		Method:	InLine(lcStr)
*:		Method:	RightTrim(m.cString)
*:		Method:	AdjustString(m.cString)
*:		Method:	Beautify(m.source)
*:		Method:	MakeOptions
*:		Method:	B2C(m.Val)
*:		Method:	FindFile(lcFile)
*:
*: Piva BEAR v.200 format 23.04.2006 16:06:45
*:********************************************************************
*
* ����� Bear
* ���������� ������ Beautify ��� �������������� ������ � ����� ��� ������������ ��-�����
* ����� �������� ������ ������ Beautify.APP
* ��. ����� OptionXXXXXXXX
*
* �������� �.�. 2006, piva@acmetelecom.ru
* 13:00, �������, 18 ����� 2006 �.
*

#Define CR					Chr(13)
#Define LF				  Chr(10)												&& ssa &&
#Define CRLF				Chr(13)+Chr(10)

#Define WORD_BREAK			" ()[]"+Chr(9)
#Define NOTE_MARK			"*:"
#Define INLINE_COMMENT		Chr(38)+Chr(38)
#Define Tab					Chr(9)

#Define ENV_FILENAME	1
#Define ENV_SIZE		2
#Define ENV_SELSTART	17
#Define ENV_SELEND		18
#Define ENV_TABWIDTH	21
#Define ENV_KIND		25

#Define MAX_INSERT 2^19

#Define c_Message	.T.
#Define CONTENTS	.F.

Lparameters  m.InFile, m.options

Local o As Bear Of bear2.prg
o=Createobject("Bear", m.InFile)
If Vartype(o)='O'
	With o
		If Pcount()=2 And Vartype(m.options)='C' And Len(m.options)=36
			.BeautifyMode=.T.
			.options=m.options
		Endif
		.Process()
		If .BeautifyMode
			Strtofile(.Source,.OutFile)
			Return .OutFile
		Else
			If Not Empty(.OutFile) And File(.OutFile)
				Erase (.OutFile)
			Endif
		Endif
	Endwith
Endif

*:********************************************************************
*:
*:	Class:	Bear	based on Session
*:
*:********************************************************************
Define Class Bear As Session
	Version="v.200"
	Title='Bear 2'
	FoxTools=""																			&& ���� FoxTools.FLL
	Keywords=""																			&& ���� FDKeyWrd
	FD3FLL=""																				&& ���� FD3.DLL
	
	DetachFoxTools=.T.															&& ��������� FoxTools ����� ������
	
	TabWidth=4																			&& ������ ���������
	TabStop=50																			&& ������ ��� ����������� ��������� �����������
	
	FileName=""																			&& ��� �������������� �����
	
	WHandle=0																				&& ���� ����
	Position=0																			&& ������� ������� � ���������
	FileSize=0																			&& ������ �����
	SelStart=0																			&& ������� ���������
	SelEnd=0
	Kind=0																					&& ��� ��������� ���� - �������� ��� Snippet
	
	LinesCount=0																		&& ��������� ���������� ���������� ���������
	HeadersCount=0
	
	Source=""																				&& ��� ������ ����������� �����
	Header=""																				&& ��������� �����
	Footer=""
	
	Level=0																					&& ������ ������ �����������
	NextLevel=0																			&& ���������
	BaseLevel=0																			&& ������� (������ �������)
	CurrentClass=""																	&& ��� �������� ������
	IsComment=.F.																		&& �����������
	
	LineSize=68																			&& ������ ������ ������������ ��������
	
	BeautifyMode=.F.																&& ����� �� ���� ��� Beautify.APP
	options=""																			&& ����� ������������ Beautify
	InFile=""																				&& ��������� ���� ��� beautify
	OutFile=""																			&& �������� ����
	
	UseBeautify=.T.																	&& ������������ Beautfy ��� �������� ������
	FormatProcedures=.T.														&& ��������� ��������� �������� � �������
	FormatClasses=.T.																&& ����������� ����������� ������
	FormatFile=.T.																	&& ���������� ��������� �����
	
	MyIndent=.T.																		&& ��� ������� ��������
	* - ����� ��������� - ���� ����������� ������ ����� Beautify
	
	* ����� Beautify.APP
	* ������������ ��� �������� ������
	
	* 1 - UpperCase
	* 2 - LowerCase
	* 3 - MixedCase
	* 4 - NoChange
	
	OptionKeywords=3
	
	* 1 - UpperCase
	* 2 - LowerCase
	* 3 - Mach 1-st occetence
	* 4 - NoChange
	
	OptionSymbols=3
	
	* 1 - Tab
	* 2 - Spaces
	* 3 - NoChange
	
	OptionIndent = 1
	
	OptionSpaces = 1 && 4
	
	OptionExpandKeywords = .T.
	
	OptionCommentIndent=.F.
	OptionLineIndent=.F.
	OptionExtraProcedures=.F.
	OptionExtraDoCase=.T.
	
	Dimension aString[1], aHeader[1]
	
	*:********************************************************************
	*:
	*:	Method:	Init(lcFileName) of class Bear
	*:
	*:********************************************************************
	Procedure Init(lcFileName)
	With This
		Sys(3056)
		If Not .LoadFiles() Or Not .LoadStrings(lcFileName)	&& ssa && ��� ������� � ���������� �����������
			Return .F.
		Endif
		*ssa*			If Not .LoadStrings(lcFileName)
		*ssa*				Return .F.
		*ssa*			Endif
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	Destroy of class Bear
	*:
	*:********************************************************************
	Procedure Destroy
	With This
		If .DetachFoxTools
			Release Library (.FoxTools)
		Endif
		Use In Select("Keywords")
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	LoadFiles of class Bear
	*:
	*:********************************************************************
	Procedure LoadFiles
	*ssa*		With This
	*ssa*			.FoxTools=.FindFile("FoxTools.FLL")
	*ssa*			If Empty(.FoxTools)
	*ssa*				Return .F.
	*ssa*			Endif
	*ssa*			.DetachFoxTools=Not "FOXTOOLS.FLL" $ Set('library')
	*ssa*			If .DetachFoxTools
	*ssa*				Set Library To (.FoxTools) Additive
	*ssa*			Endif
	*ssa*		Endwith
	Set Library To Sys(2004)+'FoxTools' Additive
	If "FOXTOOLS.FLL" $ Set('library')
		.DetachFoxTools = .T.
		.Foxtools = Sys(2004)+'FOXTOOLS.FLL'
	Endif
	Return .DetachFoxTools
	
	*:********************************************************************
	*:
	*:	Method:	LoadStrings(lcFileName) of class Bear
	*:
	*:********************************************************************
	Procedure LoadStrings(lcFileName)
	With This
		Local lcSource
		If Empty(lcFileName)
			.WHandle=_WOnTop()
			If .WHandle < 1
				Return .F.
			Endif
			Local laFileInfo[25]
			
			_EdGetEnv(.WHandle,@laFileInfo)
			.Kind=laFileInfo[ENV_KIND]
			
			If .Kind<1
				Messagebox("������� ���� �� �������� ����� ��������� FoxPro",16,.Title)
				Return .F.
			Endif
			
			.Position=_EdGetPos(.WHandle)
			
			.SelStart	=laFileInfo[ENV_SELSTART]
			.SelEnd		=laFileInfo[ENV_SELEND]
			.FileName	=laFileInfo[ENV_FILENAME]
			.FileSize	=laFileInfo[ENV_SIZE]
			.TabWidth	=laFileInfo[ENV_TABWIDTH]
			
			* ���� ������� ��� �� ����� ��� ��������� ���� ����� �����
			lcSource=_EdGetStr(.WHandle,0,.FileSize)
		Else
			If Not File(lcFileName)
				Error 1,lcFileName
				Return .F.
			Endif
			.FileName=lcFileName
			lcSource=Filetostr(lcFileName)
		Endif
		If .BeautifyMode Or .UseBeautify
			If Not .Beautify(lcSource)
				Return .F.
			Endif
		Else
			.LinesCount=Alines(.aString,lcSource,.T.)
		Endif
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	Process of class Bear
	*:
	*:********************************************************************
	Procedure Process
	With This
		Local lnCount, lcStr, lcClassName, lnLevel
		Local laWords[1], lnWords
		
		Dimension .aHeader[1]
		.HeadersCount=0
		
		.Source=""
		
		lcPreText=""
		.Level=0
		.NextLevel=0
		.BaseLevel=0
		
		For lnCount=1 To .LinesCount
			lnLevel=0
			lcStr=.aString[lnCount]
			If Empty(lcStr)
				.Put()
				Loop
			Endif
			* �������� ��������� TAB'�
			*ssa*				Do While Left(Alltrim(lcStr),1)=Tab
			*ssa*					lcStr=Substr(lcStr,2)
			*ssa*				Enddo
			*ssa*				* � �������� ����
			*ssa*				Do While Right(Alltrim(lcStr),1)=Tab
			*ssa*					lcStr=Substr(lcStr,1,Len(lcStr)-1)
			*ssa*				Enddo
			lcStr = Alltrim(lcStr, Tab)									&& ssa  && ���� ���� � ������� ������, �� �������� ������ �������� ' ' (�� ���� ������ :) )
			
			*ssa*				Dimension laWord[GetWordCount(lcStr,WORD_BREAK)]
			*ssa*				For lnWord=1 To Alen(laWord)
			*ssa*					laWord[lnWord]=Getwordnum(lcStr,lnWord,WORD_BREAK)
			*ssa*				Next
			Alines(laWord, lcStr, 1+8, ' ')							&&WORD_BREAK ����� ������� � ��� �������?
			
																									&& ssa &&  � ����� ��������� ���� FDKEYWRD.DBF?
			
			If 	.FormatClasses ;
				and .CheckWord(laWord[1],'DEFINE',4) ;
				and .CheckWord(laWord[2],'CLASS',4)
				
				.CurrentClass=laWord[3]
				
				.Level=0
				.NextLevel=0
				.BaseLevel=0
				lnLevel=1
				
				.ClassHeader(laWord[3],laWord[5])
				
			Endif
			
			If .FormatProcedures ;
				and .CheckWord(laWord[1],"PROTECTED",4) ;
				or .CheckWord(laWord[1],"HIDDEN",4)
				
				If .CheckWord(laWord[2],"PROCEDURE",4) ;
					or .CheckWord(laWord[2],"FUNCTION",4) ;
					
					.Level=0
					.ProcHeader(laWord[3],Atc(laWord[3],"FUNCTION"))
					.NextLevel=0
					
				Endif
			Endif
			
			If .FormatProcedures ;
				and .CheckWord(laWord[1],"PROCEDURE",4) ;
				or .CheckWord(laWord[1],"FUNCTION",4) ;
				
				.Level=0
				*ssa*					.ProcHeader(laWord[2],Atc(laWord[2],"Function"))
				.ProcHeader(laWord[2], Atc("FUNCTION", laWord[1]))
				.NextLevel=0
				
			Endif
			
			If (.FormatProcedures ;
				or .FormatClasses ;
				or .FormatFile ) ;
				and laWord[1]=NOTE_MARK
				Loop
			Endif
			
			If Not .MyIndent
				*ssa*					=Put(.aString[lnCount])
				.Put(.aString[lnCount])										&& ssa && ����� �����, ��� ��� .MyIndent=.f. ������ �� ���� :)
				Loop
			Endif
			
			* ���������� ��� ����������� �����������
			If .IsComment Or Left(laWord[1],1)='*' Or (Upper(laWord[1])="NOTE" And Len(laWord[1])=4)
				.Put(lcStr)
				.IsComment=Right(Getwordnum(lcStr,Getwordcount(lcStr,WORD_BREAK),WORD_BREAK),1)=";"
				Loop
			Endif
			
			If 	.CheckWord(laWord[1],'ENDDEFINE',4)
				.CurrentClass=""
				.BaseLevel=0
				.Level=0
				.NextLevel=0
			Endif
			
			If .CheckWord(laWord[1],'IF'		,2) ;
				or .CheckWord(laWord[1],'FOR'		,3) ;
				or .CheckWord(laWord[1],'SCAN'		,4) ;
				or .CheckWord(laWord[1],'WITH'		,4) ;
				or .CheckWord(laWord[1],'TRY'		,3) ;
				or .CheckWord(laWord[1],'CATCH'		,4) ;
				or .CheckWord(laWord[1],'PRINTJOB'	,4) ;
				or .CheckWord(laWord[1],'#IF'		,3) ;
				or .CheckWord(laWord[1],'#IFDEF'	,4) ;
				or (.CheckWord(laWord[1],'DO',2) ;
				and (.CheckWord(laWord[2],'WHILE',4) ;
				or .CheckWord(laWord[2],'CASE',4)))
				
				.NextLevel=.NextLevel+1
				
			Endif
			
			
			If .CheckWord(laWord[1],'ELSE'		,4) ;
				or .CheckWord(laWord[1],'CASE'		,4) ;
				or .CheckWord(laWord[1],'CATCH'		,4) ;
				or .CheckWord(laWord[1],'FINALLY'	,4) ;
				or .CheckWord(laWord[1],'#ELIF'		,4) ;
				or .CheckWord(laWord[1],'#ELSE'		,4) ;
				
				.Level=.Level-1
				*				.NextLevel=.NextLevel+1
				
			Endif
			
			If .CheckWord(laWord[1],'ENDIF'		,4) ;
				or .CheckWord(laWord[1],'ENDCASE'	,4) ;
				or .CheckWord(laWord[1],'ENDTRY'	,4) ;
				or .CheckWord(laWord[1],'ENDDO'		,4) ;
				or .CheckWord(laWord[1],'ENDFOR'	,4) ;
				or .CheckWord(laWord[1],'ENDWITH'	,4) ;
				or .CheckWord(laWord[1],'NEXT'		,4) ;
				or .CheckWord(laWord[1],'#ENDIF'	,4) ;
				or .CheckWord(laWord[1],'ENDSCAN'	,4)			&& ssa && + ������ ������� ������ :)
				
				.Level=.Level-1
				.NextLevel=.NextLevel-1
				
			Endif
			
			If .CheckWord(laWord[1],'ENDPROC'	,4) ;
				or .CheckWord(laWord[1],'ENDFUNC'	,4) ;
				
				.Level=0
				.NextLevel=0
				
			Endif
			
			If Atc(INLINE_COMMENT,lcStr)>0
				lcStr=Replicate(Tab,Max(.BaseLevel+.Level,0))+lcStr
				lcStr=.InLine(lcStr)
				.Source=.Source+lcStr+CRLF
			Else
				.Put(lcStr)
			Endif
			
			.Level=.NextLevel
			
			If Not Empty(lnLevel)
				.BaseLevel=lnLevel
			Endif
			
		Next
		If .FormatFile
			.FileHeader()
			.FileFooter()
		Endif
		
		.Source=.Header+.Source+.Footer
		.Save()
		
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	CheckWord(lcWord,lcTemplate,lnWordLen) of class Bear
	*:
	*:********************************************************************
	Procedure CheckWord(lcWord,lcTemplate,lnWordLen)
	*ssa*		With This
	*ssa*			Return Atc(lcWord,lcTemplate)=1 And Len(lcWord)>=lnWordLen
	Return lcTemplate=Upper(lcWord)									&& ssa && Atc(lcTemplate, lcWord)=1 And Len(lcWord)>=lnWordLen
	*ssa*		Endwith
	
	
	*:********************************************************************
	*:
	*:	Method:	Put(lcStr) of class Bear
	*:
	*:********************************************************************
	Procedure Put(lcStr)
	With This
		.Source=.Source+Replicate(Tab,Max(.BaseLevel+.Level, 0))+Iif(Empty(lcStr),"",lcStr)+CRLF
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	PutH(lcStr) of class Bear
	*:
	*:********************************************************************
	Procedure PutH(lcStr)
	With This
		.Header=.Header+Iif(Empty(lcStr),"",lcStr)+CRLF
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	PutE(lcStr) of class Bear
	*:
	*:********************************************************************
	Procedure PutE(lcStr)
	With This
		.Footer=.Footer+Iif(Empty(lcStr),"",lcStr)+CRLF
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	Save of class Bear
	*:
	*:********************************************************************
	Procedure Save
	With This
		If .BeautifyMode
			Strtofile(.Source,.OutFile)
			Return .T.
		Endif
		
		* ���� ������ ���� ���������
		If Not Empty(.WHandle)
			_EdUndoOn(.WHandle,.T.)											&& �������� ����� UNDO ��� 1 �������� �����
			_EdSelect(.WHandle,0,.FileSize)							&& ������� ����� �����
			_EdDelete(.WHandle)													&& ��� �������
*ssa*	  ��� ��������� ������� ������ ������� ����������� �� ������ ������������ �����.
*ssa*	  �������� ������ ������� �������
			If Len(.Source) > MAX_INSERT
				For i=1 to Int(Len(.Source)/MAX_INSERT)
					_EdInsert(.WHandle, Substr(.Source, (i-1)*MAX_INSERT+1, MAX_INSERT), MAX_INSERT)
				Next
				_EdInsert(.WHandle,Right(.Source,Len(.Source)%MAX_INSERT), Len(.Source)%MAX_INSERT)
			Else 
				_EdInsert(.WHandle,.Source,Len(.Source))		&& ������� ����������������� �����
			EndIf
			_EdUndoOn(.WHandle,.F.)											&& ��������� UNDO
			_EdSetPos(.WHandle,.Position)								&& ������ �� �������
			_EdStoPos(.WHandle,.Position,.T.)						&& ����������� ��������� �� ��� �������
		Else
			* ���� ���������� ��� ����� - �� ��� � ������������
			Strtofile(.Source,.FileName,0)
		Endif
	Endwith
		
	*:********************************************************************
	*:
	*:	Method:	FileHeader of class Bear
	*:
	*:********************************************************************
	Procedure FileHeader
	With This
		.PutH(NOTE_MARK+Replicate("*",.LineSize))
		.PutH(NOTE_MARK)
		
		If .Kind=1
			.PutH(NOTE_MARK+Tab+"Procedure file: "+.FileName)
		Endif
		If .HeadersCount>0
			.PutH(NOTE_MARK+Tab+"Contents:")
			For i=1 To .HeadersCount
				.PutH(NOTE_MARK+Tab+Tab+.aHeader[i])
			Next
		Endif
		.PutH(NOTE_MARK)
		.PutH(NOTE_MARK+" Piva BEAR "+.Version+" format "+Transform(Datetime()))
		.PutH(NOTE_MARK+Replicate("*",.LineSize))
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	FileFooter of class Bear
	*:
	*:********************************************************************
	Procedure FileFooter
	With This
		If .Kind=1
			.PutE(NOTE_MARK+" End of : "+.FileName)
			*ssa*			Else
			
		Endif
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	ClassHeader(lcClassName, of class Bear
	*:
	*:********************************************************************
	Procedure ClassHeader(lcClassName, lcBaseClass)
	With This
		.HeadersCount = .HeadersCount + 1
		Dimension .aHeader[.HeadersCount]
		.aHeader[.HeadersCount]="Class"+Tab+Tab+lcClassName+" AS "+lcBaseClass
		
		.Put(NOTE_MARK+Replicate("*",.LineSize))
		.Put(NOTE_MARK)
		.Put(NOTE_MARK+Tab+"Class:"+Tab+lcClassName+Tab+"based on "+lcBaseClass)
		.Put(NOTE_MARK)
		.Put(NOTE_MARK+Replicate("*",.LineSize))
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	ProcHeader(lcName,lnType) of class Bear
	*:
	*:********************************************************************
	Procedure ProcHeader(lcName,lnType)
	With This
		If Not Empty(.CurrentClass)
			Return .MethodHeader(lcName)
		Endif
		
		.HeadersCount = .HeadersCount + 1
		Dimension .aHeader[.HeadersCount]
		.aHeader[.HeadersCount]=Iif(Bittest(lnType,0),"Function:","Procedure:")+Tab+lcName
		
		.Put(NOTE_MARK+Replicate("*",.LineSize))
		.Put(NOTE_MARK)
		.Put(NOTE_MARK+Tab+Iif(Bittest(lnType,0),"Function:","Procedure:")+Tab+lcName)
		.Put(NOTE_MARK)
		.Put(NOTE_MARK+Replicate("*",.LineSize))
		
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	MethodHeader(lcName) of class Bear
	*:
	*:********************************************************************
	Procedure MethodHeader(lcName)
	With This
		.HeadersCount = .HeadersCount + 1
		Dimension .aHeader[.HeadersCount]
		.aHeader[.HeadersCount]="Method:"+Tab+lcName
		
		.Put(NOTE_MARK+Replicate("*",.LineSize))
		.Put(NOTE_MARK)
		.Put(NOTE_MARK+Tab+"Method:"+Tab+lcName+" of class "+.CurrentClass)
		.Put(NOTE_MARK)
		.Put(NOTE_MARK+Replicate("*",.LineSize))
		
	Endwith
	
	
	*:********************************************************************
	*:
	*:	Method:	InLine(lcStr) of class Bear
	*:
	*:********************************************************************
	Procedure InLine(lcStr)
	With This
		* ���� ����������� ��� ��������
		If INLINE_COMMENT $ lcStr										&& ssa  && ? ������� ���� #Define INLINE_COMMENT		Chr(38)+Chr(38)
			Local lnCommentPos, lcSuffix
			lnCommentPos = At(INLINE_COMMENT,lcStr)				&& ssa &&  ��� �����, ��� ����� �� ���������� :)
			lcSuffix = .RightTrim(Substr(lcStr,lnCommentPos))
			lcStr = .RightTrim(Left(lcStr, lnCommentPos-1))	&& ssa &&  Substr(lcStr,1,lnCommentPos-1)) �� left()
			If Not Empty(lcSuffix)
				lcStr = .AdjustString(lcStr)+lcSuffix
			Endif
		Endif
		Return lcStr
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	RightTrim(m.cString) of class Bear
	*:
	*:********************************************************************
	Procedure RightTrim(m.cString)
	* Remove all trailing spaces and tabs and whitespace
	*ssa*		Local i,c
	*ssa*		c=m.cString
	*ssa*		For i=Len(m.cString) To 1 Step -1
	*ssa*			If Inlist(Asc(Substr(m.c,i-1,1)),32,9,160)
	*ssa*				m.c=Substr(m.c,1,i-1)
	*ssa*			Else
	*ssa*				Exit
	*ssa*			Endif
	*ssa*		Next
	*ssa*		m.c=Strtran(m.c,Chr(13),"")
	*ssa*		m.c=Strtran(m.c,Chr(10),"")
	*ssa*		Return m.c
	Return Rtrim(m.cString, CR, LF, Chr(32), Tab, Chr(160))	&&  ssa &&  ��� ����� �������� DEFINE ��� ��������� ��������
	
	*:********************************************************************
	*:
	*:	Method:	AdjustString(m.cString) of class Bear
	*:
	*:********************************************************************
	Procedure AdjustString(m.cString)
	* ������������ ������ �� ������� ������� �����������
	Local i, m.pos
	m.pos = 0
	For i=1 To Len(m.cString)
		*ssa*			If Substr(m.cString,i,1)=Tab
		*ssa*				m.pos=Int((m.pos-1+.TabWidth)/.TabWidth)*.TabWidth+1
		*ssa*			Else
		*ssa*				m.pos = m.pos + 1
		*ssa*			EndIf
		m.pos = Iif(Substr(m.cString,i,1)=Tab, Int((m.pos-1+.TabWidth)/.TabWidth)*.TabWidth+1, m.pos + 1)	&& ssa && ������ if �� iif
	Next
	If m.pos < .TabStop
		* ������ ������ ����������
		i = Len(m.cString)
		Do While m.pos < .TabStop-.TabWidth
			m.cString = m.cString+Tab
			* ��� ������ ����� -1 � ��� � �� ����� :))
			m.pos = (Int((m.pos-1+.TabWidth)/.TabWidth)*.TabWidth)+1
		Enddo
	Else
		* �������� 1 ���������
		m.cString = m.cString+Tab
	Endif
	Return m.cString
	
	*:********************************************************************
	*:
	*:	Method:	Beautify(m.source) of class Bear
	*:
	*:********************************************************************
	Procedure Beautify(m.source)
	* ��� ���������� Beautify
	With This
		m.Keywords=.FindFile("FDKEYWRD.DBF")
		m.FD3FLL=.FindFile("FD3.FLL")
		
		Local fsuccess, m.OutFile, m.InFile, xrefname, m.options
		Local M.errlogfile, moldlogerrors
		If File(m.Keywords) And File(m.FD3FLL)
			m.options=.MakeOptions()
			Use (m.Keywords) In 0  Again Alias fdkeywrd Order token
			Select fdkeywrd
			Set Library To (m.FD3FLL) Additive
			m.InFile = Sys(2023)+"\"+Substr(Sys(2015), 3, 10)+".TMP"
			Strtofile(m.source,m.InFile)
			m.OutFile = Sys(2023)+"\"+Substr(Sys(2015), 3, 10)+".TMP"
			
			If (Substr(m.options, 1, 1)=Chr(3))
				m.xrefname = "FDXREF"
				
				Create Cursor (m.xrefname) ;
				( symbol 		c(65);
				, procname 		c(40);
				, Flag 			c(1);
				, Lineno 		N(5);
				, sniprecno 	N(5);
				, snipfld 		c(10);
				, sniplineno 	N(5);
				, Adjust 		N(5);
				, FileName 		c(161))
				
				Index On Flag Tag Flag
				Index On Upper(symbol)+Flag Tag symbol
			Endif
			m.fsuccess = Beautify(m.InFile,m.OutFile,m.options)
			Release Library (m.FD3FLL)
			If (Substr(m.options, 1, 1)=Chr(3))
				Use In Select("fdxref")
			Endif
			Use In Select("fdkeywrd")
			.LinesCount = Alines(.aString,Filetostr(m.OutFile),.T.)
			
			.InFile = m.InFile
			.OutFile = m.OutFile
			
			Erase (m.InFile)
		Endif
		Return m.fsuccess
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	MakeOptions of class Bear
	*:
	*:********************************************************************
	Procedure MakeOptions
	* �������� ����� �� �������� �����
	With This
		If .BeautifyMode
			Return .options
		Endif
		Return  ;
		.B2C(.OptionSymbols)+;
		.B2C(.OptionKeywords)+;
		.B2C(.OptionSpaces)+;
		.B2C(.OptionIndent)+;
		.B2C(.OptionExpandKeywords)+;
		.B2C(.OptionCommentIndent)+;
		.B2C(.OptionLineIndent)+;
		.B2C(.OptionExtraProcedures)+;
		.B2C(.OptionExtraDoCase)
	Endwith
	
	*:********************************************************************
	*:
	*:	Method:	B2C(m.Val) of class Bear
	*:
	*:********************************************************************
	Procedure B2C(m.Val)
	*ssa*		Local m.Ret
	m.Val=Iif(Vartype(m.Val)='L',Iif(m.Val,1,0),m.Val)
	*ssa*		m.Ret=""
	*ssa*		Do While m.Val # 0
	*ssa*			m.Ret=m.Ret+Chr(Mod(m.Val,256))
	*ssa*			m.Val=Bitrshift(m.Val,8)
	*ssa*		Enddo
	*ssa*		Return Padr(m.Ret,4,Chr(0))
	Return BinToC(m.Val, '4rs')
	
	*:********************************************************************
	*:
	*:	Method:	FindFile(lcFile) of class Bear
	*:
	*:********************************************************************
	Procedure FindFile(lcFile)
	With This
		If File(lcFile)
			Return lcFile
		Endif
		If File(Home()+lcFile)
			Return Home()+lcFile
		Endif
		If File(Home()+"wizards\"+lcFile)
			Return Home()+"wizards\"+lcFile
		Endif
		Return ""
	Endwith
	
Enddefine
*: End of : bear2m.prg
