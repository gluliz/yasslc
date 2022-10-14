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
)

type TConst struct {
	ConstType byte // 0-char, 1-int, 2-string
	CVal      byte
	NVal      int
	SVal      string
}

type Stack []int

// IsEmpty: check if stack is empty
func (s *Stack) IsEmpty() bool {
	return len(*s) == 0
}

// Push a new value onto the stack
func (s *Stack) Push(i int) {
	*s = append(*s, i) // Simply append the new value to the end of the stack
}

// Remove and return top element of stack. Return false if stack is empty.
func (s *Stack) Pop() (int, bool) {
	if s.IsEmpty() {
		return 0, false
	} else {
		index := len(*s) - 1   // Get the index of the top most element.
		element := (*s)[index] // Index into the slice and obtain the element.
		*s = (*s)[:index]      // Remove it from the stack by slicing it off.
		return element, true
	}
}

// Get the top most element
func (s *Stack) Top() int {
	return (*s)[len(*s)-1]
}
