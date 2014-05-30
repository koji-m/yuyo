package yuyocore

import "fmt"

func Deb(message string) {
	fmt.Println("[DEBUG]" + message)
}

func ShowSymTable() {
	fmt.Println("<< SYMBOL TABLE >>")
	for str, yobj := range symbolTable {
		fmt.Println("[", str, "]=", symbolVal(yobj))
		fmt.Println("[", str, "]=", typeOf(yobj))
	}
}

func showStack(m *vm) {
	fmt.Println("<<<<  showStack  >>>>")
	for i := 0; i < 12; i++ {
		fmt.Println("[", i, "] =", int64(uintptr((*yCell)(m.stk[i]).car)))
	}
	fmt.Println("[", 12, "] =", intnumVal(*(*yObj)(m.stk[12])))
}
