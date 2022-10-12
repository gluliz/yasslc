package syntactic

import (
	"encoding/csv"
	"log"
	"os"
	"yasslc/lexical"
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

func readCsvFile(filePath string) [][]string {
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

func (synt Syntactic) SyntacticAnalysis() error {
	action := readCsvFile("./action_table.csv")
	lenLeft := readCsvFile("./len_left_table.csv")
	final := 5 //for this specific grammar
	q := 0
	stack.Push(0)
	a := synt.Lexical.NextToken()
	for q != final {

	}
	return nil
}
