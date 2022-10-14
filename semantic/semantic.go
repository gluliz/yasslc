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
		IDD, IDU, ID, T, LI, LI0, LI1, TRUE, FALSE, CHR, E1, STR, NUM, DC, DC0, DC1, LP, LP0, LP1, E, E0, L, L0, L1, R, R0, R1, Y, Y0, Y1, F, F0, F1, LV, LV0, LV1, MC, LE, LE0, LE1, MF types.TAttrib
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
	case types.LDV_0:
	case types.LS_0:
	case types.LS_1:
		return nil
	case types.DV_0:
		// TODO
		T = stack.Top().(types.TAttrib)
		//t := T.T.Type
		stack.Pop()
		LI = stack.Top().(types.TAttrib)
		stack.Pop()
		//p := LI.LI.List

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
	case types.S_0:
		// TODO: replace _ by MT for code generation purposes
		_ = stack.Top().(types.TAttrib)
		stack.Pop()
		E = stack.Top().(types.TAttrib)
		stack.Pop()
	case types.S_4:
		scope.EndBlock()
	case types.S_5:
		E = stack.Top().(types.TAttrib)
		stack.Pop()
		LV = stack.Top().(types.TAttrib)
		stack.Pop()
		//t := LV.LV.Type
	case types.S_6:
	case types.S_7:
		return nil
	case types.E_0:
		L = stack.Top().(types.TAttrib)
		stack.Pop()
		E1 = stack.Top().(types.TAttrib)
		stack.Pop()
		E0.E.Type = scope.PBool
		E0.TypeNonTerminal = types.E_
		stack.Push(E0)
	case types.E_1:
		L = stack.Top().(types.TAttrib)
		stack.Pop()
		E1 = stack.Top().(types.TAttrib)
		stack.Pop()
		E0.E.Type = scope.PBool
		E0.TypeNonTerminal = types.E_
		stack.Push(E1)
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
		L0.L.Type = scope.PBool
		L0.TypeNonTerminal = types.L_
		stack.Push(L0)
	case types.L_1:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L1 = stack.Top().(types.TAttrib)
		stack.Pop()
		L0.L.Type = scope.PBool
		L0.TypeNonTerminal = types.L_
		stack.Push(L0)
	case types.L_2:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L1 = stack.Top().(types.TAttrib)
		stack.Pop()
		L0.L.Type = scope.PBool
		L0.TypeNonTerminal = types.L_
		stack.Push(L0)
	case types.L_3:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L1 = stack.Top().(types.TAttrib)
		stack.Pop()
		L0.L.Type = scope.PBool
		L0.TypeNonTerminal = types.L_
		stack.Push(L0)
	case types.L_4:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L1 = stack.Top().(types.TAttrib)
		stack.Pop()
		L0.L.Type = scope.PBool
		L0.TypeNonTerminal = types.L_
		stack.Push(L0)
	case types.L_5:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L1 = stack.Top().(types.TAttrib)
		stack.Pop()
		L0.L.Type = scope.PBool
		L0.TypeNonTerminal = types.L_
		stack.Push(L0)
	case types.L_6:
		R = stack.Top().(types.TAttrib)
		stack.Pop()
		L.L.Type = R.R.Type
		L.TypeNonTerminal = types.L_
		stack.Push(L)
	case types.R_0:
		Y = stack.Top().(types.TAttrib)
		stack.Pop()
		R1 = stack.Top().(types.TAttrib)
		stack.Pop()

		R0.R.Type = R1.R.Type
		R0.TypeNonTerminal = types.R_
		stack.Push(R0)
	case types.R_1:
		Y = stack.Top().(types.TAttrib)
		stack.Pop()
		R1 = stack.Top().(types.TAttrib)
		stack.Pop()

		R0.R.Type = R1.R.Type
		R0.TypeNonTerminal = types.R_
		stack.Push(R0)
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

		Y0.Y.Type = Y1.Y.Type
		Y0.TypeNonTerminal = types.Y_
		stack.Push(Y0)
	case types.Y_1:
		F = stack.Top().(types.TAttrib)
		stack.Pop()
		Y1 = stack.Top().(types.TAttrib)
		stack.Pop()

		Y0.Y.Type = Y1.Y.Type
		Y0.TypeNonTerminal = types.Y_
		stack.Push(Y0)
	case types.Y_2:
		F = stack.Top().(types.TAttrib)
		stack.Pop()
		Y.Y.Type = F.F.Type
		Y.TypeNonTerminal = types.Y_
		stack.Push(Y)
	case types.F_0:
		LV = stack.Top().(types.TAttrib)
		stack.Pop()

		//n := LV.LV.Type.Type.NSize
		F.F.Type = LV.LV.Type
		F.TypeNonTerminal = types.F_
		stack.Push(F)
	case types.F_1:
		LV = stack.Top().(types.TAttrib)
		stack.Pop()
		//t := LV.LV.Type
		F.F.Type = scope.PInt
		F.TypeNonTerminal = types.F_
		stack.Push(F)
	case types.F_2:
		LV = stack.Top().(types.TAttrib)
		stack.Pop()
		//t := LV.LV.Type
		F.F.Type = LV.LV.Type
		F.TypeNonTerminal = types.F_
		stack.Push(F)
	case types.F_3:
		LV = stack.Top().(types.TAttrib)
		stack.Pop()
		//t := LV.LV.Type
		F.F.Type = LV.LV.Type
		F.TypeNonTerminal = types.F_
		stack.Push(F)
	case types.F_4:
		LV = stack.Top().(types.TAttrib)
		stack.Pop()
		t := LV.LV.Type
		F.F.Type = t
		F.TypeNonTerminal = types.F_
		stack.Push(F)
	case types.F_5:
		E = stack.Top().(types.TAttrib)
		stack.Top()

		F.F.Type = E.E.Type
		F.TypeNonTerminal = types.F_
		stack.Push(F)
	case types.F_6:
		LE = stack.Top().(types.TAttrib)
		stack.Top()
		MC = stack.Top().(types.TAttrib)
		stack.Top()
		IDU = stack.Top().(types.TAttrib)
		stack.Top()

		//f := IDU.ID.Obj
		F.F.Type = MC.MC.Type
		F.TypeNonTerminal = types.F_
		stack.Push(F)
	case types.F_7:
		F1 = stack.Top().(types.TAttrib)
		stack.Top()

		F0.F.Type = F1.F.Type
		F0.TypeNonTerminal = types.F_
		stack.Push(F0)
	case types.F_8:
		F1 = stack.Top().(types.TAttrib)
		stack.Top()

		F0.F.Type = F1.F.Type
		F0.TypeNonTerminal = types.F_
		stack.Push(F0)
	case types.F_9:
		TRUE = stack.Top().(types.TAttrib)
		stack.Top()

		F.F.Type = scope.PBool
		F.TypeNonTerminal = types.F_
		stack.Push(F)
	case types.F_10:
		FALSE = stack.Top().(types.TAttrib)
		stack.Top()

		F.F.Type = scope.PBool
		F.TypeNonTerminal = types.F_
		stack.Push(F)
	case types.F_11:
		CHR = stack.Top().(types.TAttrib)
		stack.Top()

		F.F.Type = scope.PChar
		F.TypeNonTerminal = types.F_
		stack.Push(F)
	case types.F_12:
		STR = stack.Top().(types.TAttrib)
		stack.Top()

		F.F.Type = scope.PString
		F.TypeNonTerminal = types.F_
		stack.Push(F)
	case types.F_13:
		NUM = stack.Top().(types.TAttrib)
		stack.Top()

		F.F.Type = scope.PInt
		F.TypeNonTerminal = types.F_
		stack.Push(F)
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
			} else {
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
			} else {
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

		p := IDU.ID.Obj
		if p.Kind != types.VAR_ && p.Kind != types.PARAM_ {
			if p.Kind != types.UNIVERSAL_ {
				//Error(ERR_KIND_NOT_VAR);
			}
			LV.LV.Type = scope.PUniversal
		} else {
			LV.LV.Type = p.Var.PType
			LV.LV.Type.Type.NSize = p.Var.NSize
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
		stack.Push(MF)

	case types.MC_0:
		//IDU := stack.Top().(types.TAttrib)
		//f = IDU.ID.Obj
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
		// TODO: change this to boolean true
		TRUE.TRUE.Val = 0
		stack.Push(TRUE)

	case types.FALSE_0:
		// TODO: change this to boolean false
		FALSE.FALSE.Val = 0
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
