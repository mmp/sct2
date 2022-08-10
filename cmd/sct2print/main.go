/*
sct2print prints the contents of SCT2 files to standard output.

Usage:

	sct2print [filenames...]
*/
package main

import (
	"fmt"
	"os"
	"time"

	"github.com/mmp/sct2"
)

func main() {
	for _, fn := range os.Args[1:] {
		contents, err := os.ReadFile(fn)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %s", fn, err)
			continue
		}

		start := time.Now()
		sf, err := sct2.Parse(contents, fn, func(err string) {
			fmt.Fprint(os.Stderr, err)
		})
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %s", fn, err)
		} else {
			elapsed := time.Since(start)
			fmt.Printf("Parsed file in %s\n", elapsed)
			sf.Write(os.Stdout)
		}
	}
}
