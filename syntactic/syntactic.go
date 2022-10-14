package syntactic

import (
	"encoding/csv"
	"errors"
	"log"
	"os"
	"strconv"
	"yasslc/lexical"
	"yasslc/semantic"
	"yasslc/types"
)

func IsShift(p int) bool {
	return p > 0
}
func IsReduction(p int) bool {
	return p < 0
}
func Rule(p int) int {
	return -p
}

type Syntactic struct {
	Lexical lexical.Data
}

var stack types.Stack

func ReadCsvFile(filePath string) [][]string {
	f, err := os.Open(filePath)
	if err != nil {
		log.Println("Unable to read input file "+filePath, err)
	}
	defer f.Close()

	csvReader := csv.NewReader(f)
	records, err := csvReader.ReadAll()
	if err != nil {
		log.Println("Unable to parse file as CSV for "+filePath, err)
	}

	return records
}

// There are 49 tokens defined
func ConvertToToken(nonTerminal string, actionTable *[][]string) int {
	for i, v := range (*actionTable)[0] {
		if v == nonTerminal {
			return i
		}
	}
	return -1
}

var actionHeader = []types.TToken{types.NOT, types.NOT_EQUAL, types.AND, types.LEFT_PARENTHESIS, types.RIGHT_PARENTHESIS, types.TIMES, types.PLUS, types.PLUS_PLUS, types.COMMA, types.MINUS, types.MINUS_MINUS, types.DOT, types.DIVIDE, types.COLON, types.SEMI_COLON, types.LESS_THAN, types.LESS_OR_EQUAL, types.EQUALS, types.EQUAL_EQUAL, types.GREATER_THAN, types.GREATER_OR_EQUAL, types.LEFT_SQUARE, types.RIGHT_SQUARE, types.ARRAY, types.BOOLEAN, types.BREAK, types.CHARACTER, types.CHAR, types.CONTINUE, types.DO, types.ELSE, types.FALSE, types.FUNCTION, types.ID, types.IF, types.INTEGER, types.NUMERAL, types.OF, types.STRINGVAL, types.STRING, types.STRUCT, types.TRUE, types.TYPE, types.VAR, types.WHILE, types.LEFT_BRACES, types.OR, types.RIGHT_BRACES, types.EOF}

func ConvertTokenToPosition(token types.TToken) int {
	for i, v := range actionHeader {
		if token == v {
			return i + 1
		}
	}
	return -1
}

func (synt Syntactic) Semantics(ruleNumber int) {

}

func (synt Syntactic) SyntacticAnalysis() error {
	action := ReadCsvFile("./syntactic/action_table.csv")
	lenLeft := ReadCsvFile("./syntactic/len_left_table.csv")
	final := 5 // for this specific grammar; TODO: read this from action table
	q := 0
	stack.Push(0)
	a := synt.Lexical.NextToken()
	for q != final {
		col := ConvertTokenToPosition(a)
		p, _ := strconv.Atoi(action[q+1][col])
		if IsShift(p) {
			stack.Push(p)
			a = synt.Lexical.NextToken()
		} else if IsReduction(p) {
			r := Rule(p)
			ruleLen, _ := strconv.Atoi(lenLeft[r][0])
			// TODO: implement pop for more than one item
			for i := 0; i < ruleLen; i++ {
				stack.Pop()
			}
			// It only works for non terminals
			leftSideToken := ConvertToToken(lenLeft[r][1], &action)
			nextItem, _ := strconv.Atoi(action[stack.Top().(int)+1][leftSideToken])
			stack.Push(nextItem)
			semantic.Semantics(types.TRule(r))
		} else {
			return errors.New("Syntax error")
		}
		q = stack.Top().(int)
	}
	return nil
}
