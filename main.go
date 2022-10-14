package main

import (
	"fmt"
	"log"
	"os"
	"yasslc/lexical"
	"yasslc/syntactic"
	"yasslc/types"
)

var ReservedWords = []string{"array", "boolean", "break", "char", "continue", "do", "else", "false", "function", "if", "integer", "of", "string", "struct", "true", "type", "var", "while"}

var SecundaryTokens = make(map[string]int)
var NNumSecTokens = 0

var VConsts []types.TConst
var NNumConsts = 0
var Tokens []types.TToken

func main() {
	f, err := os.Open("input.ss")
	if err != nil {
		log.Println(err)
	}
	defer f.Close()
	data := lexical.Data{F: f, ReservedWords: ReservedWords, SecundaryTokens: SecundaryTokens, NNumSecTokens: &NNumSecTokens, NNumConsts: &NNumConsts, VConsts: &VConsts, Tokens: &Tokens}
	syntactic := syntactic.Syntactic{Lexical: data}
	err = syntactic.SyntacticAnalysis()
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println("Everything is fine :)")

}
