BNFC:=${HOME}/BNFC_2.2/bnfc.exe

AbsContextFree.hs LexContextFree.x ParContextFree.y : ContextFree.cf
	${BNFC} ContextFree.cf

LexContextFree.hs : LexContextFree.x
	alex LexContextFree.x

ParContextFree.hs : ParContextFree.y
	happy ParContextFree.y
