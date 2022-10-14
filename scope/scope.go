package scope

import "yasslc/types"

const MaxNestLevel = 256

var Int_ = types.TObject{NName: -1, PNext: nil, Kind: types.SCALAR_TYPE_}
var PInt = &Int_

var Char_ = types.TObject{NName: -1, PNext: nil, Kind: types.SCALAR_TYPE_}
var PChar = &Char_

var Bool_ = types.TObject{NName: -1, PNext: nil, Kind: types.SCALAR_TYPE_}
var PBool = &Bool_

var String_ = types.TObject{NName: -1, PNext: nil, Kind: types.SCALAR_TYPE_}
var PString = &String_

var Univesal_ = types.TObject{NName: -1, PNext: nil, Kind: types.SCALAR_TYPE_}
var PUniversal = &Univesal_

var (
	symbolTable     = new([MaxNestLevel]*types.TObject)
	symbolTableLast = new([MaxNestLevel]*types.TObject)
	nCurrentLevel   = 0
)

func NewBlock() int {
	nCurrentLevel++
	symbolTable[nCurrentLevel] = nil
	symbolTableLast[nCurrentLevel] = nil
	return nCurrentLevel
}

func EndBlock() int {
	nCurrentLevel--
	return nCurrentLevel
}

func Define(aName int) *types.TObject {
	Obj := new(types.TObject)
	Obj.NName = aName
	Obj.PNext = nil

	if symbolTable[nCurrentLevel] == nil {
		symbolTable[nCurrentLevel] = Obj
		symbolTableLast[nCurrentLevel] = Obj
	} else {
		symbolTableLast[nCurrentLevel].PNext = Obj
		symbolTableLast[nCurrentLevel] = Obj
	}
	return Obj
}

func Search(aName int) *types.TObject {
	Obj := symbolTable[nCurrentLevel]
	for Obj != nil {
		if Obj.NName == aName {
			break
		} else {
			Obj = Obj.PNext
		}
	}
	return Obj
}

func Find(aName int) *types.TObject {
	var Obj *types.TObject = nil
	for i := nCurrentLevel; i >= 0; i-- {
		Obj = symbolTable[i]
		for Obj != nil {
			if Obj.NName == aName {
				break
			} else {
				Obj = Obj.PNext
			}
		}
		if Obj != nil {
			break
		}
	}
	return Obj
}

func IsTypeKind(k types.TKind) bool {
	return types.ARRAY_TYPE_ == k || types.STRUCT_TYPE_ == k || types.ALIAS_TYPE_ == k || types.SCALAR_TYPE_ == k
}
