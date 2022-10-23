package types

type TToken int

const (
	// reserved words
	ARRAY TToken = iota
	BOOLEAN
	BREAK
	CHAR
	CONTINUE
	DO
	ELSE
	FALSE
	FUNCTION
	IF
	INTEGER
	OF
	STRING
	STRUCT
	TRUE
	TYPE
	VAR
	WHILE
	// symbols
	COLON
	SEMI_COLON
	COMMA
	EQUALS
	LEFT_SQUARE
	RIGHT_SQUARE
	LEFT_BRACES
	RIGHT_BRACES
	LEFT_PARENTHESIS
	RIGHT_PARENTHESIS
	AND
	OR
	LESS_THAN
	GREATER_THAN
	LESS_OR_EQUAL
	GREATER_OR_EQUAL
	NOT_EQUAL
	EQUAL_EQUAL
	PLUS
	PLUS_PLUS
	MINUS
	MINUS_MINUS
	TIMES
	DIVIDE
	DOT
	NOT
	// regular tokens
	CHARACTER
	NUMERAL
	STRINGVAL
	ID
	// unknown token
	UNKNOWN
	EOF
)

type TNonT int

const (
	B_ TNonT = iota
	LDV_
	LS_
	CHR_
	DC_
	LI_
	T_
	DE_
	DF_
	DT_
	IDD_
	LP_
	NUM_
	DV_
	E_
	L_
	F_
	LV_
	IDU_
	TRUE_
	FALSE_
	STR_
	R_
	LDE_
	LE_
	S_
	P_
	Y_
	ID_
	NF_
	MC_
	MT_
	ME_
	MW_
)

type TRule int

const (
	START TRule = iota
	B_0
	CHR_0
	DC_0
	DC_1
	DE_0
	DE_1
	DF_0
	DT_0
	DT_1
	DT_2
	DV_0
	E_0
	E_1
	E_2
	F_0
	F_1
	F_2
	F_3
	F_4
	F_5
	F_6
	F_7
	F_8
	F_9
	F_10
	F_11
	F_12
	F_13
	FALSE_0
	ID_0
	IDD_0
	IDU_0
	L_0
	L_1
	L_2
	L_3
	L_4
	L_5
	L_6
	LDE_0
	LDE_1
	LDV_0
	LDV_1
	LE_0
	LE_1
	LI_0
	LI_1
	LP_0
	LP_1
	LS_0
	LS_1
	LV_0
	LV_1
	LV_2
	MC_0
	ME_0
	MF_0
	MT_0
	MW_0
	NB_0
	NF_0
	NUM_0
	P_0
	R_0
	R_1
	R_2
	S_0
	S_1
	S_2
	S_3
	S_4
	S_5
	S_6
	S_7
	STR_0
	T_0
	T_1
	T_2
	T_3
	T_4
	TRUE_0
	Y_0
	Y_1
	Y_2
)

type TConst struct {
	ConstType byte // 0-char, 1-int, 2-string
	CVal      byte
	NVal      int
	SVal      string
}

type TObject struct {
	NName             int
	PNext             *TObject
	Kind              TKind
	Var, Param, Field struct {
		PType  *TObject
		NIndex int
		NSize  int
	}
	Function struct {
		PParams  *TObject
		PRetType *TObject
		NIndex   int
		NParams  int
		NVars    int
	}
	Array struct {
		PElemType *TObject
		NNumElems int
		NSize     int
	}
	Struct struct {
		PFields *TObject
		NSize   int
	}
	Alias, Type struct {
		PBaseType *TObject
		NSize     int
	}
}

type TAttrib struct {
	TypeNonTerminal TNonT
	NSize           int
	ID              struct {
		Obj  *TObject
		Name int
	}
	T, E, L, R, Y, F, LV struct {
		Type *TObject
	}
	MC struct {
		Type  *TObject
		Param *TObject
		Err   bool
	}
	MT, ME, MW, MA struct {
		Label int
	}
	LE struct {
		Type  *TObject
		Param *TObject
		Err   bool
		N     int
	}
	LI, DC, LP struct {
		List *TObject
	}

	TRUE, FALSE struct {
		Type *TObject
		Val  bool
	}
	CHR struct {
		Type *TObject
		Val  *byte
		Pos  int
	}
	STR struct {
		Type *TObject
		Val  string
		Pos  int
	}
	NUM struct {
		Type *TObject
		Val  int
		Pos  int
	}
}

type TKind int

const (
	NO_KIND_DEF TKind = iota - 1
	VAR_
	PARAM_
	FUNCTION_
	FIELD_
	ARRAY_TYPE_
	STRUCT_TYPE_
	ALIAS_TYPE_
	SCALAR_TYPE_
	UNIVERSAL_
)
