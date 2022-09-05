package lexical

import (
	"sort"
	"strings"
	"yasslc/types"
)

func (data Data) SearchKeyWord(name string) types.TToken {
	token := (types.TToken)(sort.Search(len(data.ReservedWords), func(i int) bool { return strings.Compare(data.ReservedWords[i], name) > -1 }))
	if (int)(token) == len(data.ReservedWords) {
		return types.ID
	} else if data.ReservedWords[token] != name {
		return types.ID
	}
	return token
}

func (data Data) SearchName(name string) int {
	if val, ok := data.SecundaryTokens[name]; ok {
		return val
	}
	data.SecundaryTokens[name] = *data.NNumSecTokens
	*data.NNumSecTokens++
	return *data.NNumSecTokens - 1
}

func (data Data) AddCharConst(c byte) int {
	to_add := types.TConst{ConstType: 0, CVal: c}
	*data.VConsts = append(*data.VConsts, to_add)
	return len(*data.VConsts) - 1
}
func (data Data) AddIntConst(n int) int {
	to_add := types.TConst{ConstType: 1, NVal: n}
	*data.VConsts = append(*data.VConsts, to_add)
	return len(*data.VConsts) - 1
}
func (data Data) AddStringConst(s string) int {
	to_add := types.TConst{ConstType: 2, SVal: s}
	*data.VConsts = append(*data.VConsts, to_add)
	return len(*data.VConsts) - 1
}

func (data Data) GetCharConst(n int) byte {
	return (*data.VConsts)[n].CVal
}
func (data Data) GetIntConst(n int) int {
	return (*data.VConsts)[n].NVal
}
func (data Data) GetStringConst(n int) string {
	return (*data.VConsts)[n].SVal
}
