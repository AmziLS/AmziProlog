if {[package vcompare [package provide Tcl] 8.6] != 0} { return }
package ifneeded Tk 8.6 [list load [file join $dir .. .. bin tk86t.dll] Tk]
