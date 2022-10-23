package semantic

import (
	"errors"
	"fmt"
	"os"
	"yasslc/lexical"
	"yasslc/scope"
	"yasslc/types"
)

var stack types.Stack

type Semantic struct {
	LexicalData *lexical.Data
}

var (
	Data            Semantic
	CurrentFunction *types.TObject
	NFunc           = 0
	FunctionVarPos  = 0
	LabelNumber     = 0
)

func NewLabel() int {
	LabelNumber++
	return LabelNumber
}

func CheckTypes(t1 *types.TObject, t2 *types.TObject) bool {
	if t1 == t2 {
		return true
	}
	if t1 == scope.PUniversal || t2 == scope.PUniversal {
		return true
	}
	if t1.Kind == types.UNIVERSAL_ || t2.Kind == types.UNIVERSAL_ {
		return true
	}
	if t1.Kind == types.ALIAS_TYPE_ && t2.Kind != types.ALIAS_TYPE_ {
		return CheckTypes(t1.Alias.PBaseType, t2)
	}
	if t2.Kind == types.ALIAS_TYPE_ && t1.Kind != types.ALIAS_TYPE_ {
		return CheckTypes(t2.Alias.PBaseType, t1)
	}
	if t1.Kind == t2.Kind {
		if t1.Kind == types.ALIAS_TYPE_ {
			return CheckTypes(t1.Alias.PBaseType, t2.Alias.PBaseType)
		} else if t1.Kind == types.ARRAY_TYPE_ {
			if t1.Array.NNumElems == t2.Array.NNumElems {
				return CheckTypes(t1.Array.PElemType, t2.Array.PElemType)
			}
		} else if t1.Kind == types.STRUCT_TYPE_ {
			f1 := t1.Struct.PFields
			f2 := t2.Struct.PFields
			for f1 != nil && f2 != nil {
				if !CheckTypes(f1.Field.PType, f2.Field.PType) {
					return false
				}
			}
			return (f1 == nil && f2 == nil)
		}
	}
	return false
}

func Semantics(rule types.TRule) error {
	var (
		IDD, IDU, ID, T, LI, LI0, LI1, TRUE, FALSE, CHR, E1, STR, NUM, DC, DC0, DC1, LP, LP0, LP1, E, E0, L, L0, L1, R, R0, R1, Y, Y0, Y1, F, F0, F1, LV, LV0, LV1, MC, LE, LE0, LE1, MT, ME, MW types.TAttrib
	)
	//
	//MT, ME, MW, NB

	switch rule {
	case types.LDE_0:
	case types.LDE_1:
	case types.DE_0:
	case types.DE_1:
		return nil
	case types.T_0:
		T.T.Type = scope.PInt
		T.TypeNonTerminal = types.T_
		T.NSize = 1
		stack.Push(T)
	case types.T_1:
		T.T.Type = scope.PChar
		T.TypeNonTerminal = types.T_
		T.NSize = 1
		stack.Push(T)
	case types.T_2:
		T.T.Type = scope.PBool
		T.TypeNonTerminal = types.T_
		T.NSize = 1
		stack.Push(T)
	case types.T_3:
		T.T.Type = scope.PString
		T.TypeNonTerminal = types.T_
		T.NSize = 1
		stack.Push(T)
	case types.T_4:
		IDU = stack.Top().(types.TAttrib)
		p := IDU.ID.Obj
		stack.Pop()
		if scope.IsTypeKind(p.Kind) || p.Kind == types.UNIVERSAL_ {
			T.T.Type = p
			if p.Kind == types.ALIAS_TYPE_ {
				T.NSize = p.Alias.NSize
			} else if p.Kind == types.ARRAY_TYPE_ {
				T.NSize = p.Array.NSize
			} else if p.Kind == types.STRUCT_TYPE_ {
				T.NSize = p.Struct.NSize
			}
		} else {
			T.T.Type = scope.PUniversal
			T.NSize = 0
			//Error
		}
		T.TypeNonTerminal = types.T_
		stack.Push(T)
	case types.DT_0:
		T = stack.Top().(types.TAttrib)
		stack.Pop()
		NUM = stack.Top().(types.TAttrib)
		stack.Pop()
		IDD = stack.Top().(types.TAttrib)
		stack.Pop()

		p := IDD.ID.Obj
		n := NUM.NUM.Val
		t := T.T.Type

		p.Kind = types.ARRAY_TYPE_
		p.Array.NNumElems = n
		p.Array.PElemType = t
		p.Array.NSize = n * T.NSize
	case types.DT_1:
		DC = stack.Top().(types.TAttrib)
		stack.Pop()
		IDD = stack.Top().(types.TAttrib)
		stack.Pop()

		p := IDD.ID.Obj
		p.Kind = types.STRUCT_TYPE_
		p.Struct.PFields = DC.DC.List
		p.Struct.NSize = DC.NSize
		scope.EndBlock()
	case types.DT_2:
		T = stack.Top().(types.TAttrib)
		stack.Pop()
		IDD = stack.Top().(types.TAttrib)
		stack.Pop()

		p := IDD.ID.Obj
		t := T.T.Type

		p.Kind = types.ALIAS_TYPE_
		p.Alias.PBaseType = t
		p.Alias.NSize = T.NSize

	case types.DC_0:
		T = stack.Top().(types.TAttrib)
		stack.Pop()
		LI = stack.Top().(types.TAttrib)
		stack.Pop()
		DC1 = stack.Top().(types.TAttrib)
		stack.Pop()

		p := LI.LI.List
		t := T.T.Type
		n := DC1.NSize

		for p != nil && p.Kind == types.NO_KIND_DEF {
			p.Kind = types.FIELD_
			p.Field.PType = t
			p.Field.NIndex = n
			p.Field.NSize = T.NSize
			n = n + T.NSize
			p = p.PNext
		}

		DC0.DC.List = DC1.DC.List
		DC0.NSize = n
		DC0.TypeNonTerminal = types.DC_
		stack.Push(DC0)
	case types.DC_1:
		T = stack.Top().(types.TAttrib)
		stack.Pop()
		LI = stack.Top().(types.TAttrib)
		stack.Pop()

		p := LI.LI.List
		t := T.T.Type
		n := 0

		for p != nil && p.Kind == types.NO_KIND_DEF {
			p.Kind = types.FIELD_
			p.Field.PType = t
			p.Field.NIndex = n
			p.Field.NSize = T.NSize
			n = n + T.NSize
			p = p.PNext
		}

		DC.DC.List = LI.DC.List
		DC.NSize = n
		DC.TypeNonTerminal = types.DC_
		stack.Push(DC)
	case types.DF_0:
		scope.EndBlock()
	case types.LP_0:
		T = stack.Top().(types.TAttrib)
		stack.Pop()
		IDD = stack.Top().(types.TAttrib)
		stack.Pop()
		LP1 = stack.Top().(types.TAttrib)
		stack.Pop()

		p := IDD.ID.Obj
		t := T.T.Type
		n := LP1.NSize

		p.Kind = types.PARAM_
		p.Param.PType = t
		p.Param.NIndex = n
		p.Param.NSize = T.NSize

		LP0.LP.List = LP1.LP.List
		LP0.NSize = n + T.NSize
		LP0.TypeNonTerminal = types.LP_
		stack.Push(LP0)

	case types.LP_1:
		T = stack.Top().(types.TAttrib)
		stack.Pop()
		IDD = stack.Top().(types.TAttrib)
		stack.Pop()

		p := IDD.ID.Obj
		t := T.T.Type

		p.Kind = types.PARAM_
		p.Param.PType = t
		p.Param.NIndex = 0
		p.Param.NSize = T.NSize

		LP.LP.List = p
		LP.NSize = T.NSize
		LP.TypeNonTerminal = types.LP_
		stack.Push(LP)
	case types.B_0:
		fmt.Fprint(os.Stdout, "END_FUNC\n")
		//offset, _ := Data.LexicalData.FOut.Seek(0, io.SeekCurrent)
		//Data.LexicalData.FOut.Seek(int64(FunctionVarPos), io.SeekStart)
		//fmt.Fprint(os.Stdout, "%02d\n", CurrentFunction.Function.NVars)
		//Data.LexicalData.FOut.Seek(offset, io.SeekStart)

	case types.LDV_0:
	case types.LS_0:
	case types.LS_1:
		return nil
	case types.DV_0:
		T = stack.Top().(types.TAttrib)
		t := T.T.Type
		stack.Pop()
		LI = stack.Top().(types.TAttrib)
		stack.Pop()
		p := LI.LI.List
		n := CurrentFunction.Function.NVars
		for p != nil && p.Kind == types.NO_KIND_DEF {
			p.Kind = types.VAR_
			p.Var.PType = t
			p.Var.NSize = T.NSize
			p.Var.NIndex = n

			n += T.NSize
			p = p.PNext
		}
		CurrentFunction.Function.NVars = n
	case types.LI_0:
		IDD = stack.Top().(types.TAttrib)
		stack.Pop()
		LI1 = stack.Top().(types.TAttrib)
		stack.Pop()
		LI0.LI.List = LI1.LI.List
		LI0.TypeNonTerminal = types.LI_
		stack.Push(LI0)
	case types.LI_1:
		IDD = stack.Top().(types.TAttrib)
		LI.LI.List = IDD.ID.Obj
		LI.TypeNonTerminal = types.LI_
		stack.Pop()
		stack.Push(LI)
	case types.MW_0:
		l := NewLabel()
		MW.MW.Label = l
		stack.Push(MW)
		fmt.Fprintf(os.Stdout, "L%d\n", l)
	case types.ME_0:
		MT = stack.Top().(types.TAttrib)
		l1 := MT.MT.Label
		l2 := NewLabel()
		ME.ME.Label = l2
		ME.TypeNonTerminal = types.ME_
		stack.Push(ME)
		fmt.Fprintf(os.Stdout, "\tJMP_FW L%d:\nL%d\n", l2, l1)
	case types.S_0:
		MT = stack.Top().(types.TAttrib)
		stack.Pop()
		E = stack.Top().(types.TAttrib)
		stack.Pop()
		t := E.E.Type
		if !CheckTypes(t, scope.PBool) {
			return errors.New("Bool type expected")
		}
		fmt.Fprintf(os.Stdout, "L%d\n", MT.MT.Label)
	case types.S_1:
		ME = stack.Top().(types.TAttrib)
		stack.Pop()
		MT = stack.Top().(types.TAttrib)
		stack.Pop()
		E = stack.Top().(types.TAttrib)
		stack.Pop()

		l := ME.ME.Label
		t := E.E.Type
		if !CheckTypes(t, scope.PBool) {
			return errors.New("Bool expected")
		}
		fmt.Fprintf(os.Stdout, "L%d:\n", l)
	case types.S_2:
		MT = stack.Top().(types.TAttrib)
		stack.Pop()
		E = stack.Top().(types.TAttrib)
		stack.Pop()
		MW = stack.Top().(types.TAttrib)
		stack.Pop()

		l1 := MW.MW.Label
		l2 := MT.MT.Label
		if !CheckTypes(E.E.Type, scope.PBool) {
			return errors.New("Bool expected")
		}
		fmt.Fprintf(os.Stdout, "\tJMP_BW L%d\nL%d\n", l1, l2)
	case types.S_3:
		E = stack.Top().(types.TAttrib)
		stack.Pop()
		MW = stack.Top().(types.TAttrib)
		stack.Pop()

		l := MW.MW.Label
		t := E.E.Type
		if !CheckTypes(t, scope.PBool) {
			return errors.New("Bool expected")
		}
		fmt.Fprintf(os.Stdout, "\tNOT\n\tTJMP_BW L%d\n", l)
	case types.S_4:
		scope.EndBlock()
	case types.S_5:
		E = stack.Top().(types.TAttrib)
		stack.Pop()
		LV = stack.Top().(types.TAttrib)
		stack.Pop()
		t1 := LV.LV.Type
		t2 := E.E.Type

		if !CheckTypes(t1, t2) {
			return errors.New("Types mismatch")
		}
		fmt.Fprintf(os.Stdout, "\tSTORE_REF %d\n", t1.Type.NSize)
	case types.S_6:
	case types.S_7:
		return nil
	case types.E_0:
		L = stack.Top().(types.TAttrib)
		stack.Pop()
		E1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(L.L.Type, scope.PBool) || !CheckTypes(E1.E.Type, scope.PBool) {
			return errors.New("Bool type expected")
		}
		E0.E.Type = scope.PBool
		E0.TypeNonTerminal = types.E_
		stack.Push(E0)
		fmt.Fprintf(os.Stdout, "\tAND\n")
	case types.E_1:
		L = stack.Top().(types.TAttrib)
		stack.Pop()
		E1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(L.L.Type, scope.PBool) || !CheckTypes(E1.E.Type, scope.PBool) {
			return errors.New("Bool type expected")
		}
		E0.E.Type = scope.PBool
		E0.TypeNonTerminal = types.E_
		stack.Push(E1)
		fmt.Fprintf(os.Stdout, "\tOR\n")
	case types.E_2:
		L = stack.Top().(types.TAttrib)
		stack.Pop()
		E.E.Type = L.L.Type
		E.TypeNonTerminal = types.E_
		stack.Push(E)
	case types.L_0:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(R.R.Type, L1.L.Type) {
			return errors.New("Types mismatch")
		}
		L0.L.Type = scope.PBool
		L0.TypeNonTerminal = types.L_
		stack.Push(L0)
		fmt.Fprintf(os.Stdout, "\tLT\n")
	case types.L_1:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(R.R.Type, L1.L.Type) {
			return errors.New("Types mismatch")
		}
		L0.L.Type = scope.PBool
		L0.TypeNonTerminal = types.L_
		stack.Push(L0)
		fmt.Fprintf(os.Stdout, "\tGT\n")
	case types.L_2:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(R.R.Type, L1.L.Type) {
			return errors.New("Types mismatch")
		}
		L0.L.Type = scope.PBool
		L0.TypeNonTerminal = types.L_
		stack.Push(L0)
		fmt.Fprintf(os.Stdout, "\tLE\n")
	case types.L_3:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(R.R.Type, L1.L.Type) {
			return errors.New("Types mismatch")
		}
		L0.L.Type = scope.PBool
		L0.TypeNonTerminal = types.L_
		stack.Push(L0)
		fmt.Fprintf(os.Stdout, "\tGE\n")
	case types.L_4:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(R.R.Type, L1.L.Type) {
			return errors.New("Types mismatch")
		}
		L0.L.Type = scope.PBool
		L0.TypeNonTerminal = types.L_
		stack.Push(L0)
		fmt.Fprintf(os.Stdout, "\tEQ\n")
	case types.L_5:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(R.R.Type, L1.L.Type) {
			return errors.New("Types mismatch")
		}
		L0.L.Type = scope.PBool
		L0.TypeNonTerminal = types.L_
		stack.Push(L0)
		fmt.Fprintf(os.Stdout, "\tNE\n")
	case types.L_6:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L.L.Type = R.R.Type
		L.TypeNonTerminal = types.L_
		stack.Push(L)
	case types.MT_0:
		l := NewLabel()
		MT.MT.Label = 1
		MT.TypeNonTerminal = types.MT_
		fmt.Fprintf(os.Stdout, "\tTJMP_FW L%d\n", l)
		stack.Push(MT)
	case types.R_0:
		Y = stack.Top().(types.TAttrib)
		stack.Pop()
		R1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(R1.R.Type, Y.Y.Type) {
			return errors.New("Types mismatch")
		}
		if !CheckTypes(R1.R.Type, scope.PInt) && !CheckTypes(R1.R.Type, scope.PString) {
			return errors.New("Invalid type")
		}
		R0.R.Type = R1.R.Type
		R0.TypeNonTerminal = types.R_
		stack.Push(R0)
		fmt.Fprintf(os.Stdout, "\tADD\n")
	case types.R_1:
		Y = stack.Top().(types.TAttrib)
		stack.Pop()
		R1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(R1.R.Type, Y.Y.Type) {
			return errors.New("Types mismatch")
		}
		if !CheckTypes(R1.R.Type, scope.PInt) {
			return errors.New("Invalid type")
		}
		R0.R.Type = R1.R.Type
		R0.TypeNonTerminal = types.R_
		stack.Push(R0)
		fmt.Fprintf(os.Stdout, "\tSUB\n")
	case types.R_2:
		Y = stack.Top().(types.TAttrib)
		stack.Pop()

		R.R.Type = Y.Y.Type
		R.TypeNonTerminal = types.R_
		stack.Push(R)
	case types.Y_0:
		F = stack.Top().(types.TAttrib)
		stack.Pop()
		Y1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(F.F.Type, Y1.Y.Type) {
			return errors.New("Types mismatch")
		}
		if !CheckTypes(F.F.Type, scope.PInt) {
			return errors.New("Invalid type")
		}
		Y0.Y.Type = Y1.Y.Type
		Y0.TypeNonTerminal = types.Y_
		stack.Push(Y0)
		fmt.Fprintf(os.Stdout, "\tMUL\n")
	case types.Y_1:
		F = stack.Top().(types.TAttrib)
		stack.Pop()
		Y1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(F.F.Type, Y1.Y.Type) {
			return errors.New("Types mismatch")
		}
		if !CheckTypes(F.F.Type, scope.PInt) {
			return errors.New("Invalid type")
		}
		Y0.Y.Type = Y1.Y.Type
		Y0.TypeNonTerminal = types.Y_
		stack.Push(Y0)
		fmt.Fprintf(os.Stdout, "\tDIV\n")
	case types.Y_2:
		F = stack.Top().(types.TAttrib)
		stack.Pop()
		Y.Y.Type = F.F.Type
		Y.TypeNonTerminal = types.Y_
		stack.Push(Y)
	case types.F_0:
		LV = stack.Top().(types.TAttrib)
		stack.Pop()

		n := LV.LV.Type.Type.NSize
		F.F.Type = LV.LV.Type
		F.TypeNonTerminal = types.F_
		stack.Push(F)
		fmt.Fprintf(os.Stdout, "\tDE_REF %d\n", n)
	case types.F_1:
		LV = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(LV.LV.Type, scope.PInt) {
			return errors.New("Invalid type")
		}
		F.F.Type = scope.PInt
		F.TypeNonTerminal = types.F_
		stack.Push(F)
		fmt.Fprintf(os.Stdout, "\tDUP\n\tDUP\n\tDE_REF 1\n")
		fmt.Fprintf(os.Stdout, "\tINC\n\tSTORE_REF 1\n\tDE_REF 1\n")
	case types.F_2:
		LV = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(LV.LV.Type, scope.PInt) {
			return errors.New("Invalid type")
		}
		F.F.Type = LV.LV.Type
		F.TypeNonTerminal = types.F_
		stack.Push(F)
		fmt.Fprintf(os.Stdout, "\tDUP\n\tDUP\n\tDE_REF 1\n")
		fmt.Fprintf(os.Stdout, "\tDEC\n\tSTORE_REF 1\n\tDE_REF 1\n")
	case types.F_3:
		LV = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(LV.LV.Type, scope.PInt) {
			return errors.New("Invalid type")
		}
		F.F.Type = LV.LV.Type
		F.TypeNonTerminal = types.F_
		stack.Push(F)
		fmt.Fprintf(os.Stdout, "\tDUP\n\tDUP\n\tDE_REF 1\n")
		fmt.Fprintf(os.Stdout, "\tINC\n\tSTORE_REF 1\n\tDE_REF 1\n")
		fmt.Fprintf(os.Stdout, "\tDEC\n")
	case types.F_4:
		LV = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(LV.LV.Type, scope.PInt) {
			return errors.New("Invalid type")
		}
		F.F.Type = LV.LV.Type
		F.TypeNonTerminal = types.F_
		stack.Push(F)
		fmt.Fprintf(os.Stdout, "\tDUP\n\tDUP\n\tDE_REF 1\n")
		fmt.Fprintf(os.Stdout, "\tDEC\n\tSTORE_REF 1\n\tDE_REF 1\n")
		fmt.Fprintf(os.Stdout, "\tINC\n")
	case types.F_5:
		E = stack.Top().(types.TAttrib)
		stack.Pop()

		F.F.Type = E.E.Type
		F.TypeNonTerminal = types.F_
		stack.Push(F)
	case types.F_6:
		LE = stack.Top().(types.TAttrib)
		stack.Pop()
		MC = stack.Top().(types.TAttrib)
		stack.Pop()
		IDU = stack.Top().(types.TAttrib)
		stack.Pop()
		if !MC.MC.Err {
			if LE.LE.Param != nil {
				return errors.New("Too few args")
			}
		}
		f := IDU.ID.Obj
		F.F.Type = MC.MC.Type
		F.TypeNonTerminal = types.F_
		stack.Push(F)
		fmt.Fprintf(os.Stdout, "\tCALL %d\n", f.Function.NIndex)
	case types.F_7:
		F1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(F1.F.Type, scope.PInt) {
			return errors.New("Invalid type")
		}
		F0.F.Type = F1.F.Type
		F0.TypeNonTerminal = types.F_
		stack.Push(F0)
		fmt.Fprintf(os.Stdout, "\tNEG\n")
	case types.F_8:
		F1 = stack.Top().(types.TAttrib)
		stack.Pop()
		if !CheckTypes(F1.F.Type, scope.PBool) {
			return errors.New("Invalid type")
		}
		F0.F.Type = F1.F.Type
		F0.TypeNonTerminal = types.F_
		stack.Push(F0)
		fmt.Fprintf(os.Stdout, "\tNOT\n")
	case types.F_9:
		TRUE = stack.Top().(types.TAttrib)
		stack.Pop()

		F.F.Type = scope.PBool
		F.TypeNonTerminal = types.F_
		stack.Push(F)
		fmt.Fprintf(os.Stdout, "\tLOAD_CONST %d\n", lexical.SecundaryToken)
	case types.F_10:
		FALSE = stack.Top().(types.TAttrib)
		stack.Pop()

		F.F.Type = scope.PBool
		F.TypeNonTerminal = types.F_
		stack.Push(F)
		fmt.Fprintf(os.Stdout, "\tLOAD_CONST %d\n", lexical.SecundaryToken)
	case types.F_11:
		CHR = stack.Top().(types.TAttrib)
		stack.Pop()

		F.F.Type = scope.PChar
		F.TypeNonTerminal = types.F_
		stack.Push(F)
		fmt.Fprintf(os.Stdout, "\tLOAD_CONST %d\n", lexical.SecundaryToken)
	case types.F_12:
		STR = stack.Top().(types.TAttrib)
		stack.Pop()

		F.F.Type = scope.PString
		F.TypeNonTerminal = types.F_
		stack.Push(F)
		fmt.Fprintf(os.Stdout, "\tLOAD_CONST %d\n", lexical.SecundaryToken)
	case types.F_13:
		NUM = stack.Top().(types.TAttrib)
		stack.Pop()

		F.F.Type = scope.PInt
		F.TypeNonTerminal = types.F_
		stack.Push(F)
		fmt.Fprintf(os.Stdout, "\tLOAD_CONST %d\n", lexical.SecundaryToken)
	case types.LE_1:
		E = stack.Top().(types.TAttrib)
		stack.Pop()
		MC = stack.Top().(types.TAttrib)

		LE.LE.Param = nil
		LE.LE.Err = MC.MC.Err
		n := 1
		if !MC.MC.Err {
			p := MC.MC.Param
			if p == nil {
				LE.LE.Err = true
				return errors.New("Too many args")
			} else {
				if !CheckTypes(p.Param.PType, E.E.Type) {
					return errors.New("Wrong param type")
				}
				LE.LE.Param = p.PNext
				LE.LE.N = n + 1
			}
		}
		LE.TypeNonTerminal = types.LE_
		stack.Push(LE)
	case types.LE_0:
		E = stack.Top().(types.TAttrib)
		stack.Pop()
		LE1 = stack.Top().(types.TAttrib)
		stack.Pop()
		LE0.LE.Param = nil
		LE0.LE.Err = L1.LE.Err
		n := LE1.LE.N
		if !LE1.LE.Err {
			p := LE1.LE.Param
			if p == nil {
				LE0.LE.Err = true
				return errors.New("Too many args")
			} else {
				if !CheckTypes(p.Param.PType, E.E.Type) {
					return errors.New("Wrong param type")
				}
				LE0.LE.Param = p.PNext
				LE0.LE.N = n + 1
			}
		}
		LE0.TypeNonTerminal = types.LE_
		stack.Push(LE0)
	case types.LV_0:
		ID = stack.Top().(types.TAttrib)
		stack.Pop()
		LV1 = stack.Top().(types.TAttrib)
		stack.Pop()

		t := LV1.LV.Type
		if t.Kind != types.STRUCT_TYPE_ {
			if t.Kind != types.UNIVERSAL_ {
				return errors.New("Kind not struct")
			}
			LV0.LV.Type = scope.PUniversal
		} else {
			p := t.Struct.PFields
			for p != nil {
				if p.NName == ID.ID.Name {
					break
				}
				p = p.PNext
			}
			if p == nil {
				LV0.LV.Type = scope.PUniversal
			} else {
				LV0.LV.Type = p.Field.PType
				LV0.LV.Type.Type.NSize = p.Field.NSize
				fmt.Fprintf(os.Stdout, "\tADD %d\n", p.Field.NIndex)
			}
		}

		LV0.TypeNonTerminal = types.LV_
		stack.Push(LV0)

	case types.LV_1:
		E = stack.Top().(types.TAttrib)
		stack.Pop()
		LV1 = stack.Top().(types.TAttrib)
		stack.Pop()

		t := LV1.LV.Type
		if CheckTypes(t, scope.PString) {
			LV0.LV.Type = scope.PChar
		}
		if t.Kind != types.ARRAY_TYPE_ {
			if t.Kind != types.UNIVERSAL_ {
				return errors.New("Kind not array")
			}
			LV0.LV.Type = scope.PUniversal
		} else {
			LV0.LV.Type = t.Array.PElemType
			n := t.Array.PElemType.Type.NSize
			fmt.Fprintf(os.Stdout, "\tMUL %d\n\tADD\n", n)
		}
		if !CheckTypes(E.E.Type, scope.PInt) {
			return errors.New("Invalid index type")
		}
		LV0.TypeNonTerminal = types.LV_
		stack.Push(LV0)

	case types.LV_2: //LV -> IDU
		IDU = stack.Top().(types.TAttrib)
		stack.Pop()

		p := IDU.ID.Obj
		if p.Kind != types.VAR_ && p.Kind != types.PARAM_ {
			if p.Kind != types.UNIVERSAL_ {
				return errors.New("Kind not var")
			}
			LV.LV.Type = scope.PUniversal
		} else {
			LV.LV.Type = p.Var.PType
			LV.LV.Type.Type.NSize = p.Var.NSize
			fmt.Fprintf(os.Stdout, "\tLOAD_REF %d\n", p.Var.NIndex)
		}
		LV.TypeNonTerminal = types.LV_

		//t := LV.LV.Type
		stack.Push(LV)

	case types.NB_0:
		scope.NewBlock()

	case types.NF_0:
		IDD = stack.Top().(types.TAttrib)
		f := IDD.ID.Obj
		f.Kind = types.FUNCTION_
		f.Function.NParams = 0
		f.Function.NVars = 0
		NFunc++
		f.Function.NIndex = NFunc
		scope.NewBlock()

	case types.MF_0:
		T = stack.Top().(types.TAttrib)
		stack.Pop()
		LP = stack.Top().(types.TAttrib)
		stack.Pop()
		IDD = stack.Top().(types.TAttrib)
		stack.Pop()

		f := IDD.ID.Obj
		f.Kind = types.FUNCTION_
		f.Function.PRetType = T.T.Type
		f.Function.PParams = LP.LP.List
		f.Function.NParams = LP.NSize
		f.Function.NVars = 0
		CurrentFunction = f
		fmt.Fprintf(os.Stdout, "BEGIN_FUNC %d, %d, %d\n", NFunc, f.Function.NParams, 0)
		//offset, _ := Data.LexicalData.FOut.Seek(0, io.SeekCurrent)
		//FunctionVarPos = int(offset) - 3
	case types.MC_0:
		IDU = stack.Top().(types.TAttrib)
		f := IDU.ID.Obj
		if f.Kind != types.FUNCTION_ {
			MC.MC.Type = scope.PUniversal
			MC.MC.Param = nil
			MC.MC.Err = true
		} else {
			MC.MC.Type = f.Function.PRetType
			MC.MC.Param = f.Function.PParams
			MC.MC.Err = false
		}
		MC.TypeNonTerminal = types.MC_
		stack.Push(MC)

	case types.IDD_0:
		name := lexical.SecundaryToken
		IDD.ID.Name = name
		p := scope.Search(name)
		if (p) != nil {
			return errors.New("Variable already declared")
		} else {
			p = scope.Define(name)
		}
		p.Kind = types.NO_KIND_DEF
		IDD.ID.Obj = p
		stack.Push(IDD)

	case types.IDU_0:
		name := lexical.SecundaryToken
		IDU.ID.Name = name
		p := scope.Find(name)
		if (p) == nil {
			p = scope.Define(name)
			return errors.New("Variable not declared")
		}
		IDU.ID.Obj = p
		stack.Push(IDU)
	case types.ID_0:
		name := lexical.SecundaryToken
		ID.ID.Name = name
		ID.ID.Obj = nil
		stack.Push(ID)

	case types.TRUE_0:
		TRUE.TypeNonTerminal = types.TRUE_
		TRUE.TRUE.Val = true
		TRUE.TRUE.Type = scope.PBool
		stack.Push(TRUE)

	case types.FALSE_0:
		FALSE.TypeNonTerminal = types.FALSE_
		FALSE.FALSE.Type = scope.PBool
		FALSE.FALSE.Val = true
		stack.Push(FALSE)

	case types.CHR_0:
		CHR.CHR.Pos = lexical.SecundaryToken
		CHR.CHR.Val = &(*Data.LexicalData.VConsts)[lexical.SecundaryToken].CVal
		stack.Push(STR)

	case types.STR_0:
		STR.STR.Pos = lexical.SecundaryToken
		STR.STR.Val = (*Data.LexicalData.VConsts)[lexical.SecundaryToken].SVal
		stack.Push(NUM)

	case types.NUM_0:
		NUM.NUM.Pos = lexical.SecundaryToken
		NUM.NUM.Val = (*Data.LexicalData.VConsts)[lexical.SecundaryToken].NVal
		stack.Push(NUM)
	}
	return nil
}
