* Причесыватель кода
* Автор: Пирожков Вадим
* Источник: FoxPro Club
* Описание:
* Причесыватель кода, может использоваться как простая замена
* beautify.app или работать с ним в связке, сначала beautify.app наводит
* порядок, а потом эта утилька придает ему тот вид к которму я больше
* привых или можно поменять на корпоративный стиль оформления кода.
* Первое упоминание про нее я нашел у себя датированное 17 октября 1997 года.
* Маненько переделано под VFP8
*:********************************************************************
*:
*: Procedure file :bear.prg
*:
*: (c) Piva, BEAR v.42 on 25.04.2003 09:38
*:********************************************************************
#DEFINE BEAR_VERSION	"v.45"
#DEFINE BEA_BAK 	"BEA_BAK.BEA"
#DEFINE prg_step	0.05
#DEFINE CR			CHR(13)
#DEFINE CRLF		CHR(13)+CHR(10)
#DEFINE WORD_BREAK	" ()[]"+CHR(9)
#DEFINE NOTE_MARK	"*:"

#DEFINE c_Message	.T.
#DEFINE CONTENTS	.F.

* Using FoxTools.FLL for internal editor access

PARAMETER m.PrgFile
IF NOT _WINDOWS
	WAIT WINDOW NOWAIT "Windows only"
	RETURN .F.
ENDIF
IF EMPTY(WONTOP())
	RETURN .F.
ENDIF

IF NOT "FOXTOOLS.FLL" $ SET('libr')
	SET LIBR TO (SYS(2004)+'\FoxTools.fll') ADDITIVE
ENDIF
PRIVATE ALL

_VISUAL="VISUAL" $ UPPER(VERS()) &&  ATC("VISUAL",VERS())>0

wh=_WONTOP()

IF wh < 1
	RETURN .F.
ENDIF
DECLARE aa[25], PR[1]
PR=""
m.PrCount=0

*=_EDGetEnv(wh,@aa)
*m.PRGFILE=aa[1]

=_EDGetEnv(wh,@aa)
m.PrgFile=aa[1]
m.Kind=aa[25]
m.Size=aa[2]


IF m.Kind < 1
	=MsgBox("Invalid Edit Window","",48)
	RETURN .F.
ENDIF

m.old=SELECT()
SELECT 0

#IF NOT c_Message
	=Progress(0,"Обработка файла"+CHR(13)+m.PrgFile ,"Bear")
#ENDIF

ib=0
ID=0
_P=""
w=""
w2=""
w3=""

clsn=""
clsb=""
cmnt=.F.

prv=0

_CLIPTEXT=""

=_EdUndoOn(wh,.T.)		&& Включили UNDO
m.CurPos=_EdGetPos(wh)	&& Запомнили позицию
=_EdSetPos(wh,0)		&& Прыг в начало
m.Start=0				&& Позиция начала строки
m.End=0					&& Позиция конца строки

IF _VISUAL
	_SCREEN.MOUSEPOINTER=11
ENDIF

DO WHILE .T.
	m.End=_EdSkipLin(wh, m.Start, 1)	&& Позиция конца - начало следующей строки
	IF m.End=m.Start					&& Если они одинаковые - конец файла
		EXIT
	ENDIF
	s=ALLTRIM(_EdGetStr(wh, m.Start, m.End))	&& Абтримили
	IF AT(CR,s) > 0						&& Обкусали CR
		s=LEFT(s,AT(CR,s)-1)
	ENDIF
	IF m.End/m.Size >= prv				&& Расчет показывалки
		prv=m.End/SIZE+prg_step
		#IF c_Message
			SET MESSAGE TO m.PrgFile+" "+TRAN(m.End/m.Size*100,"999% completed.")
		#ELSE
			=Progress(m.End/SIZE)
		#ENDIF
	ENDIF
	DO WHILE LEFT(ALLTRIM(s),1)=CHR(9)	&& Обкусали все TAB'ы
		s=IIF(LEN(s)>1,SUBSTR(s,2),"")
	ENDDO
	w=UPPER(wordnum(s,1,WORD_BREAK))	&& Первое слово
	w2=wordnum(s,2,WORD_BREAK)	&& Дороже второго
	l=LEN(w)
	l2=LEN(w2)

	IF NOT cmnt AND LEFT(s,2)=NOTE_MARK
		m.Start=m.End
		LOOP
	ENDIF

	ID=IIF(ID<0,0,ID)
*? id,ib,s

	DO CASE
		CASE ATC(w,'PROCEDURE')=1 AND l>=4
			=ProcHeader()
		CASE ATC(w,'FUNCTION')=1 AND l>=4
			=FuncHeader()
		CASE ATC(w,'DEFINE')=1 AND l>=4
			DO CASE
				CASE ATC(w2,'CLASS')=1 AND l2>=4
					clsn=wordnum(s,3)
					=ClassHeader()
			ENDCASE
		CASE ATC(w,'PROTECTED')=1 AND l>=4 AND ATC(w2,'PROCEDURE')=1 AND l2 >= 4
			=ProcHeader()
		CASE ATC(w,'PROTECTED')=1 AND l>=4 AND ATC(w2,'FUNCTION')=1 AND l2 >= 4
			=FuncHeader()
		CASE ATC(w,'HIDDEN')=1 AND l>=4 AND ATC(w2,'PROCEDURE')=1 AND l2 >= 4
			=ProcHeader()
		CASE ATC(w,'HIDDEN')=1 AND l>=4 AND ATC(w2,'FUNCTION')=1 AND l2 >= 4
			=FuncHeader()
	ENDCASE

	IF l >=4 AND ;
			(ATC(w,'ENDIF')=1 OR ;
			ATC(w,'#ENDIF')=1 OR ;
			ATC(w,'ENDDO')=1 OR ;
			ATC(w,'ENDCASE')=1 OR ;
			ATC(w,'ENDSCAN')=1 OR ;
			ATC(w,'ENDFOR')=1 OR ;
			ATC(w,'NEXT')=1 OR ;
			ATC(w,'ENDWITH')=1 OR ;
			ATC(w,'ENDPRINTJOB')=1 OR ;
			ATC(w,'ENDTRY')=1)


*			 ATC(w,'ENDFUNCTION')=1)
*			 or ATC(w,'ENDPROCEDURE')=1 )

* Decrement indention
		ID=ID-1

	ENDIF

	IF l >=4 AND ATC(w,'ENDDEFINE')=1
		clsn=""
* Decrement Indent behind
		ib=0
		ID=0
	ENDIF

	IF l >=4 AND ;
			(ATC(w,'ELSE')=1 OR ;
			ATC(w,'CASE')=1 OR ;
			ATC(w,'#ELSE')=1 OR ;
			ATC(w,'#ELIF')=1 OR ;
			ATC(w,'OTHERWISE')=1 OR ;
			ATC(w,'CATCH')=1 OR ;
			ATC(w,'FINALLY')=1 OR ;
			ATC(w,'THROW')=1)

		ID=ID-1
	ENDIF

	IF (ib+ID+IIF(cmnt,1,0)) > 0
		_P=REPL(CHR(9),ib+ID+IIF(cmnt,1,0))
	ELSE
		_P=""
	ENDIF

	=Put(_P+s)

	IF RIGHT(s,1)=';'
		cmnt=.T.
	ELSE
		IF cmnt
			cmnt=.F.
		ENDIF
	ENDIF

	IF l >=4 AND ;
			(ATC(w,'ELSE')=1 OR ;
			ATC(w,'CASE')=1 OR ;
			ATC(w,'#ELSE')=1 OR ;
			ATC(w,'#ELIF')=1 OR ;
			ATC(w,'OTHERWISE')=1 OR ;
			ATC(w,'CATCH')=1 OR ;
			ATC(w,'FINALLY')=1 OR ;
			ATC(w,'THROW')=1)

		ID=ID+1
	ENDIF

	DO CASE
		CASE ATC(w,'PROCEDURE')=1 AND l>=4
			ID=0
		CASE ATC(w,'FUNCTION')=1 AND l>=4
			ID=0
		CASE ATC(w,'DO')=1 AND l=2
			DO CASE
				CASE ATC(w2,'WHILE')=1 AND l2>=4
					ID=ID+1
				CASE ATC(w2,'CASE')=1 AND l2=4
					ID=ID+1
			ENDCASE
		CASE ATC(w,'FOR')=1 AND l=3
			ID=ID+1
		CASE ATC(w,'IF')=1 AND l=2
			ID=ID+1
		CASE ATC(w,'#IF')=1 AND l=3
			ID=ID+1
		CASE ATC(w,'DEFINE')=1 AND l>=4
			DO CASE
				CASE ATC(w2,'CLASS')=1 AND l2>=4
					w2=wordnum(s,3)
					ib=ib+1
			ENDCASE
		CASE ATC(w,'WITH')=1 AND l=4
			ID=ID+1
		CASE ATC(w,'SCAN')=1 AND l=4
			ID=ID+1
		CASE ATC(w,'TEXT')=1 AND l=4
			ID=ID+1
		CASE ATC(w,'PRINTJOB')=1 AND l>=4
			ID=ID+1
		CASE ATC(w,'TRY')=1 AND l=3
			ID=ID+1
	ENDCASE
	m.Start=m.End
ENDDO

#IF c_Message
	SET MESSAGE TO m.PrgFile+" "+TRAN(100,"999% completed.")
#ELSE
	=Progress(1)
#ENDIF

=Put("*: End of "+JUSTFNAME(m.PrgFile))

=fileheader()

=_EdSelect(wh,0,m.Size)
=_EdDelete(wh)
=_EdPaste(wh)

=_EdSetPos(wh,m.CurPos)
=_EdStoPos(wh,m.CurPos,.T.)

=_EdUndoOn(wh,.F.)

SELECT (m.old)

#IF c_Message
	SET MESSAGE TO
#ELSE
	=Progress()
#ENDIF

IF _VISUAL
	_SCREEN.MOUSEPOINTER=0
ENDIF


*:********************************************************************
*:
*: Procedure : fileheader
*:
*:********************************************************************
PROCEDURE fileheader
PRIVATE i, v
v=""
v=v+'*:'+REPL('*',68)+CRLF
IF m.Kind=1
	v=v+'*:'+CRLF
	v=v+'*: Procedure file :'+m.PrgFile+CRLF
ENDIF
v=v+'*:'+CRLF

IF m.PrCount > 0
	v=v+"*: Contents :"+CRLF
	FOR i=1 TO m.PrCount
		IF PR[i]='Class  '
*			v=v+'*: '+CRLF
		ENDIF
		v=v+"*: "+CHR(9)+CHR(9)+PR[i]+CRLF
	NEXT
	v=v+'*:'+CRLF
ENDIF
v=v+'*: (c) Piva, BEAR '+BEAR_VERSION+' on '+DTOC(DATE())+' '+SUBSTR(TIME(),1,5)+CRLF
v=v+'*:'+REPL('*',68)+CRLF

_CLIPTEXT=v+_CLIPTEXT

*:********************************************************************
*:
*: Procedure : ProcHeader
*:
*:********************************************************************
PROCEDURE ProcHeader
IF ATC(w,'PROTECTED')=1 OR ATC(w,'HIDDEN')=1
	w2=wordnum(s,3,WORD_BREAK)
ENDIF

IF CONTENTS
	m.PrCount=m.PrCount+1
	IF m.PrCount > ALEN(PR,1)
		DECLARE PR[m.PrCount]
	ENDIF
	PR[m.PrCount]="Procedure "+clsn+IIF(NOT EMPTY(clsn),".","")+w2
ENDIF

IF NOT EMPTY(clsn)
	=classmethodheader()
ELSE
	=Put(REPL(CHR(9),ib+ID)+'*:'+REPL('*',68))
	=Put(REPL(CHR(9),ib+ID)+'*:')
	=Put(REPL(CHR(9),ib+ID)+'*: Procedure : '+w2)
	=Put(REPL(CHR(9),ib+ID)+'*:')
	=Put(REPL(CHR(9),ib+ID)+'*:'+REPL('*',68))
ENDIF

*:********************************************************************
*:
*: Procedure : FuncHeader
*:
*:********************************************************************
PROCEDURE FuncHeader
IF ATC(w,'PROTECTED')=1 OR ATC(w,'HIDDEN')=1
	w2=wordnum(s,3,WORD_BREAK)
*	PR[m.PrCount]="Function  "+clsn+iif(not empty(clsn),".","")+w2+' '+upper(w)
*else
ENDIF

IF CONTENTS
	m.PrCount=m.PrCount+1
	IF m.PrCount > ALEN(PR,1)
		DECLARE PR[m.PrCount]
	ENDIF
	PR[m.PrCount]="Function  "+clsn+IIF(NOT EMPTY(clsn),".","")+w2
ENDIF


IF NOT EMPTY(clsn)
	=classmethodheader()
ELSE
	=Put(REPL(CHR(9),ib+ID)+'*:'+REPL('*',68))
	=Put(REPL(CHR(9),ib+ID)+'*:')
	=Put(REPL(CHR(9),ib+ID)+'*: Function : '+w2)
	=Put(REPL(CHR(9),ib+ID)+'*:')
	=Put(REPL(CHR(9),ib+ID)+'*:'+REPL('*',68))
ENDIF

*:********************************************************************
*:
*: Procedure : ClassHeader
*:
*:********************************************************************
PROCEDURE ClassHeader
PRIVATE _s, k
_s=s
k=ATC("AS ",_s)
IF k=0
	k=ATC("AS"+CHR(9), _s)
ENDIF
IF k > 0
	clsb=wordnum(SUBSTR(_s,k+3), 1)
ELSE
	clsb=""
ENDIF

m.PrCount=m.PrCount+IIF(CONTENTS,2,1)
IF m.PrCount > ALEN(PR,1)
	DECLARE PR[m.PrCount]
ENDIF
IF CONTENTS
	PR[m.PrCount-1]=""
ENDIF
PR[m.PrCount]="Class     "+clsn+" AS "+clsb

=Put(REPL(CHR(9),ib)+'*:'+REPL('*',68))
=Put(REPL(CHR(9),ib)+'*:')
=Put(REPL(CHR(9),ib)+'*:    Class : '+clsn+IIF(NOT EMPTY( clsb )," based on "+clsb,""))
=Put(REPL(CHR(9),ib)+'*:')
=Put(REPL(CHR(9),ib)+'*:'+REPL('*',68))

*:********************************************************************
*:
*: Procedure : ClassMethodHeader
*:
*:********************************************************************
PROCEDURE classmethodheader
PRIVATE m.w_
m.w_=""

IF ATC(w,'PROTECTED')=1 OR ATC(w,'HIDDEN')=1
	w2=wordnum(s,3,WORD_BREAK)
	PR[m.PrCount]=PR[m.PrCount]+" "+w
	m.w_=w
ENDIF

=Put(REPL(CHR(9),ib+ID)+'*:'+REPL('*',68))
=Put(REPL(CHR(9),ib+ID)+'*:')
=Put(REPL(CHR(9),ib+ID)+'*: Method : '+w2+' of class '+clsn+IIF(!EMPTY(m.w_)," - "+m.w_,""))
=Put(REPL(CHR(9),ib+ID)+'*:')
=Put(REPL(CHR(9),ib+ID)+'*:'+REPL('*',68))

*:********************************************************************
*:
*: Procedure : MSG
*:
*:********************************************************************
PROCEDURE MSG
PARAMETER m.msgtext
IF _VISUAL
	MESSAGEBOX(m.msgtext,16,'Bea')
ELSE
	=MsgBox(m.msgtext,"Bea",16)
ENDIF
RETURN .F.


*:********************************************************************
*:
*: Procedure : Put
*:
*:********************************************************************
PROCEDURE Put
PARAMETER m.What
_CLIPTEXT=_CLIPTEXT+m.What+CRLF

*: End of bear.prg
