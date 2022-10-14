package semantic

import (
	"errors"
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
)

func Semantics(rule types.TRule) error {
	var (
		IDD, IDU, ID, T, LI, LI0, LI1, TRUE, FALSE, STR, CHR, NUM, DC, DC0, DC1, LP, LP0, LP1, E, E0, E1, L, L0, L1, R, R0, R1, Y, Y0, Y1, F, F0, F1, LV, LV0, LV1, MC, LE, LE0, LE1, MT, ME, MW, NB types.TAttrib
	)

	switch rule {
	case types.LDE_0:
	case types.LDE_1:
	case types.DE_0:
	case types.DE_1:
		return nil
	case types.T_0:
		T.T.Type = scope.PInt
		T.TNonTerminal = types.T_
		T.NSize = 1
		stack.Push(T)
	case types.T_1:
		T.T.Type = scope.PChar
		T.TNonTerminal = types.T_
		T.NSize = 1
		stack.Push(T)
	case types.T_2:
		T.T.Type = scope.PBool
		T.TNonTerminal = types.T_
		T.NSize = 1
		stack.Push(T)
	case types.T_3:
		T.T.Type = scope.PString
		T.TNonTerminal = types.T_
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
		T.TNonTerminal = types.T_
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
		n := LP1.NSize

		p.Kind = types.PARAM_
		p.Param.PType = t
		p.Param.NIndex = n
		p.Param.NSize = T.NSize

		LP0.LP.List = LP1.LP.List
		LP0.NSize = n + T.NSize
		LP0.TypeNonTerminal = types.LP_
		stack.Push(LP0)
	case types.LV_0:
		ID = stack.Top().(types.TAttrib)
		stack.Pop()
		LV1 = stack.Top().(types.TAttrib)
		stack.Pop()

		t := LV1.LV.Type
		if t.Kind != types.STRUCT_TYPE_ {
			if t.Kind != types.UNIVERSAL_ {
				// TODO Error( ERR_KIND_NOT_STRUCT);
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
		// if (CheckTypes(t, scope.PString)) {

		// }
		if t.Kind != types.ARRAY_TYPE_ {
			if t.Kind != types.UNIVERSAL_ {
				// TODO
			}
			LV0.LV.Type = scope.PUniversal
		} else {
			LV0.LV.Type = t.Array.PElemType
			//n := t.Array.PElemType.Type.NSize
		}
		// if( !check_types(E_static.E.type,pInt)){
		// 	Error(ERR_INVALID_INDEX_TYPE);
		// }
		LV0.TypeNonTerminal = types.LV_
		stack.Push(LV0)

	case types.LV_2: //LV -> IDU
		IDU = stack.Top().(types.TAttrib)
		stack.Pop()

		p = IDU.ID.Obj
		if p.Kind != types.VAR_ && p.Kind != types.PARAM_ {
			if p.Kind != types.UNIVERSAL_ {
				//Error(ERR_KIND_NOT_VAR);
			}
			LV.LV.Type = scope.PUniversal
		} else {
			LV.LV.Type = p.Var.PType
			LV.LV.Type.Type.NSize = p.Var.NSize
			fprintf(out, "\tLOAD_REF %d\n", p.Var.NIndex)
		}
		LV.Type = LV

		t = LV.LV.Type
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

	case types.MC_0:
		//IDU := stack.Top().(types.TAttrib)
		//f = IDU.ID.Obj
		stack.Push(MC)

	case types.IDD_0:
		name := lexical.SecundaryToken
		IDD.ID.Name = name
		p := scope.Search(name)
		if (p) != nil {
			errors.New("Variable already declared")
		} else {
			p = scope.Define(name)
		}
		IDD.ID.Obj = p
	case types.IDU_0:
		name := lexical.SecundaryToken
		IDU.ID.Name = name
		p := scope.Find(name)
		if (p) == nil {
			errors.New("Variable not declared")
			p = scope.Define(name)
		}
		IDU.ID.Obj = p
	case types.ID_0:
		name := lexical.SecundaryToken
		ID.ID.Name = name
		ID.ID.Obj = nil
		stack.Push(ID)

	case types.TRUE_0:
		// TODO: change this to boolean true
		TRUE.TRUE.Val = 0
		stack.Push(TRUE)

	case types.FALSE_0:
		// TODO: change this to boolean false
		FALSE.FALSE.Val = 0
		stack.Push(FALSE)

	case types.CHR_0:
		STR.STR.Pos = lexical.SecundaryToken
		//STR.STR.Val = *(Data.LexicalData).VConsts[lexical.SecundaryToken]
		stack.Push(STR)

	case types.STR_0:
		STR.STR.Pos = lexical.SecundaryToken
		//STR.STR.Val = *(Data.LexicalData).VConsts[lexical.SecundaryToken]
		stack.Push(NUM)

	case types.NUM_0:
		NUM.NUM.Pos = lexical.SecundaryToken
		//NUM.NUM.Val = *(Semantic.LexicalData).NNumConsts[lexical.SecundaryToken]
		stack.Push(NUM)
	}
	return nil
}
