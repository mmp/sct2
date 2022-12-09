/*
sct2print prints the contents of SCT2 files to standard output.

Usage:

	sct2print [filenames...]
*/
package main

import (
	"flag"
	"fmt"
	"os"
	"runtime/pprof"
	"time"

	"github.com/mmp/sct2"
)

var (
	profile = flag.Bool("profile", false, "write cpu.prof file")
)

func main() {
	flag.Parse()
	if *profile {
		if f, err := os.Create("cpu.prof"); err != nil {
			fmt.Printf("%s: unable to create CPU profile file: %v", "cpu.prof", err)
		} else {
			if err = pprof.StartCPUProfile(f); err != nil {
				fmt.Printf("unable to start CPU profile: %v", err)
			} else {
				defer pprof.StopCPUProfile()
			}
		}
	}

	for _, fn := range flag.Args() {
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
			if !*profile {
				sf.Write(os.Stdout)
			}
		}
	}
}
