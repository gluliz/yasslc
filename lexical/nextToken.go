package lexical

import (
	"os"
	"strconv"
	"unicode"
	"yasslc/types"
)

type Data struct {
	F               *os.File
	ReservedWords   []string
	Tokens          *[]types.TToken
	SecundaryTokens map[string]int
	NNumSecTokens   *int
	NNumConsts      *int
	VConsts         *[]types.TConst
}

func (data Data) readChar() rune {
	b := make([]byte, 1)
	return (rune)(b[0])
}

var nextChar rune = ' '
var token types.TToken
var secundaryToken int

func (data Data) NextToken() types.TToken {
	for unicode.IsSpace(nextChar) {
		nextChar = data.readChar()
	}
	if unicode.IsLetter(nextChar) {
		text := ""
		for unicode.IsLetter(nextChar) || unicode.IsDigit(nextChar) || nextChar == '_' {
			text += (string)(nextChar)
			nextChar = data.readChar()
		}
		token = data.SearchKeyWord(text)
		if token == types.ID {
			secundaryToken = data.SearchName(text)
		}
	} else if unicode.IsDigit(nextChar) {
		numeral := ""
		for unicode.IsDigit(nextChar) {
			numeral += (string)(nextChar)
			nextChar = data.readChar()
		}
		token = types.NUMERAL
		num, _ := strconv.Atoi(numeral)
		secundaryToken = data.AddIntConst(num)
	} else if nextChar == '"' {
		text := ""
		nextChar = data.readChar()
		for nextChar != '"' {
			text += (string)(nextChar)
			nextChar = data.readChar()
		}
		nextChar = data.readChar()
		token = types.STRINGVAL
		secundaryToken = data.AddStringConst(text)
	} else {
		switch nextChar {
		case '\'':
			nextChar = data.readChar()
			token = types.CHARACTER
			secundaryToken = data.AddCharConst((byte)(nextChar))
			nextChar = data.readChar()
			nextChar = data.readChar()
		case ':':
			nextChar = data.readChar()
			token = types.COLON
		case ';':
			nextChar = data.readChar()
			token = types.SEMI_COLON
		case ',':
			nextChar = data.readChar()
			token = types.COMMA
		case '(':
			nextChar = data.readChar()
			token = types.LEFT_PARENTHESIS
		case ')':
			nextChar = data.readChar()
			token = types.RIGHT_PARENTHESIS
		case '[':
			nextChar = data.readChar()
			token = types.LEFT_SQUARE
		case ']':
			nextChar = data.readChar()
			token = types.RIGHT_SQUARE
		case '{':
			nextChar = data.readChar()
			token = types.LEFT_BRACES
		case '}':
			nextChar = data.readChar()
			token = types.RIGHT_BRACES
		case '*':
			nextChar = data.readChar()
			token = types.TIMES
		case '/':
			nextChar = data.readChar()
			token = types.DIVIDE
		case '.':
			nextChar = data.readChar()
			token = types.DOT
		case '=':
			nextChar = data.readChar()
			if nextChar == '=' {
				token = types.EQUAL_EQUAL
				nextChar = data.readChar()
			} else {
				token = types.EQUALS
			}
		case '+':
			nextChar = data.readChar()
			if nextChar == '+' {
				token = types.PLUS_PLUS
				nextChar = data.readChar()
			} else {
				token = types.PLUS
			}
		case '-':
			nextChar = data.readChar()
			if nextChar == '-' {
				token = types.MINUS_MINUS
				nextChar = data.readChar()
			} else {
				token = types.MINUS
			}
		case '&':
			nextChar = data.readChar()
			if nextChar == '&' {
				token = types.AND
				nextChar = data.readChar()
			} else {
				token = types.UNKNOWN
			}
		case '|':
			nextChar = data.readChar()
			if nextChar == '|' {
				token = types.OR
				nextChar = data.readChar()
			} else {
				token = types.UNKNOWN
			}
		case '<':
			nextChar = data.readChar()
			if nextChar == '=' {
				token = types.LESS_OR_EQUAL
				nextChar = data.readChar()
			} else {
				token = types.LESS_THAN
			}
		case '>':
			nextChar = data.readChar()
			if nextChar == '=' {
				token = types.GREATER_OR_EQUAL
				nextChar = data.readChar()
			} else {
				token = types.GREATER_THAN
			}
		case '!':
			nextChar = data.readChar()
			if nextChar == '=' {
				token = types.NOT_EQUAL
				nextChar = data.readChar()
			} else {
				token = types.NOT
			}
		default:
			token = types.UNKNOWN
		}
	}
	return token
}
