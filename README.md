
Simple golang implementation of a parser for the SCT2 file format as used
in VATSIM.  Based on the specification in the VRC documentation
([1](https://vrc.rosscarlson.dev/docs/doc.php?page=appendix_g),
[2](https://vrc.rosscarlson.dev/docs/doc.php?page=appendix_f]).

Basic usage:
```go
	contents, err := os.ReadFile(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s", filename, err)
	}
	errorCallback := func(err string) {
		fmt.Fprint(os.Stderr, err)
	}
	sectorFile, err := sct2.Parse(contents, filename, errorCallback)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s", filename, err)
	} else {
		sectorFile.Write(os.Stdout)
	}
```

