// sct2 file parser
// Copyright(c) 2022 Matt Pharr, Apache License

// Package sct2 provides a parser for the SCT2 file format, as used in
// VATSIM to describe airports, runways, navagation aids, and other items
// that remain in fixed locations in the world.
package sct2

import (
	"fmt"
	"io"
	"math"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"
)

// SectorFile is a structure that wraps up all of the items processed by
// the parser.
type SectorFile struct {
	Id                string // Sector file identifier
	DefaultCallsign   string
	DefaultAirport    string
	Center            LatLong // Default center scope position
	NmPerLatitude     float64 // Nautical miles per degree latitude (should always be 60)
	NmPerLongitude    float64 // Nautical miles per degree longitude (varies according to latitude)
	MagneticVariation float64 // Degrees of magnetic variation in this region of the world
	Scale             float64

	Colors      []NamedColor // Colors defined via #define in the sector file
	VORs        []NamedLatLong
	NDBs        []NamedLatLong
	Airports    []NamedLatLong
	Fixes       []NamedLatLong
	ARTCC       []ARTCC // ARTCC boundary
	ARTCCLow    []ARTCC // Low boundary of ARTCC
	ARTCCHigh   []ARTCC // High boundary of ARTCC
	LowAirways  []Airway
	HighAirways []Airway
	Geo         []Geo
	SIDs        []SidStar
	STARs       []SidStar
	Regions     []NamedPoints
	Labels      []Label
	Runways     []Runway
}

type RGB struct {
	R, G, B float32
}

type NamedColor struct {
	RGB
	Name string
}

// LatLong encodes a position using latitude-longitude.
type LatLong struct {
	Latitude, Longitude float64
}

type NamedLatLong struct {
	LatLong
	Name string
}

// Segment represents a line segment between two positions.
type Segment struct {
	P [2]LatLong
}

type Geo struct {
	Segment
	Color string
}

type ARTCC struct {
	Name string
	Segs []Segment
}

type Airway struct {
	Name string
	Segs []Segment
}

type ColoredSegment struct {
	Segment
	Color string
}

// NamedPoints represents a sequence of positions with an associated name.
type NamedPoints struct {
	Name string
	P    []LatLong
}

type SidStar struct {
	Name string
	Segs []ColoredSegment
}

type Label struct {
	Name  string
	P     LatLong
	Color string
}

type Runway struct {
	Number  [2]string  // Runway number at each end
	Heading [2]float64 // Actual magnetic heading at each end
	P       [2]LatLong // Positions of the endpoints
	Airport string     // Name of the runway's airport.
}

func int24ToRGB(i int) RGB {
	return RGB{R: float32(i&0xff) / 255.,
		G: float32((i>>8)&0xff) / 255.,
		B: float32((i>>16)&0xff) / 255.}
}

// Writes a text representation of the sector file to the provided writer.
func (sf SectorFile) Write(w io.Writer) {
	fmt.Fprintf(w, "INFO:\n\tId: %s\n\tCallsign: %s\n\tAirport: %s\n\tCenter: %s\n",
		sf.Id, sf.DefaultCallsign, sf.DefaultAirport, sf.Center)
	fmt.Fprintf(w, "\tNmPerLatitude: %f\n\tNmPerLongitude: %f\n\tMagneticVariation: %f\n",
		sf.NmPerLatitude, sf.NmPerLongitude, sf.MagneticVariation)
	fmt.Fprintf(w, "\tScale: %f\n\n", sf.Scale)

	fmt.Fprintf(w, "Named Colors:\n")
	for _, color := range sf.Colors {
		fmt.Fprintf(w, "\t%s: (%f, %f, %f)\n", color.Name, color.R, color.G, color.B)
	}

	fmt.Fprintf(w, "\nVORs:\n")
	for _, vor := range sf.VORs {
		fmt.Fprintf(w, "\t%s %s\n", vor.Name, vor)
	}

	fmt.Fprintf(w, "\nNDBs:\n")
	for _, ndb := range sf.NDBs {
		fmt.Fprintf(w, "\t%s %s\n", ndb.Name, ndb)
	}

	fmt.Fprintf(w, "\nAirports:\n")
	for _, ap := range sf.Airports {
		fmt.Fprintf(w, "\t%s %s\n", ap.Name, ap)
	}

	fmt.Fprintf(w, "\nFixes:\n")
	for _, fix := range sf.Fixes {
		fmt.Fprintf(w, "\t%s %s\n", fix.Name, fix)
	}

	fmt.Fprintf(w, "\nRunways:\n")
	for _, r := range sf.Runways {
		fmt.Fprintf(w, "\t%s: %s/%s (%.1f/%.1f) %s - %s\n", r.Airport,
			r.Number[0], r.Number[1], r.Heading[0], r.Heading[1], r.P[0], r.P[1])
	}

	printSegs := func(segs []Segment) {
		for _, s := range segs {
			fmt.Fprintf(w, "\t\t%s - %s\n", s.P[0], s.P[1])
		}
	}

	fmt.Fprintf(w, "\nARTCC:\n")
	for _, artcc := range sf.ARTCC {
		fmt.Fprintf(w, "\t%s\n", artcc.Name)
		printSegs(artcc.Segs)
	}

	fmt.Fprintf(w, "\nARTCC Low:\n")
	for _, artcc := range sf.ARTCCLow {
		fmt.Fprintf(w, "\t%s\n", artcc.Name)
		printSegs(artcc.Segs)
	}

	fmt.Fprintf(w, "\nARTCC High:\n")
	for _, artcc := range sf.ARTCCHigh {
		fmt.Fprintf(w, "\t%s\n", artcc.Name)
		printSegs(artcc.Segs)
	}

	fmt.Fprintf(w, "\nLow Airways:\n")
	for _, aw := range sf.LowAirways {
		fmt.Fprintf(w, "\t%s\n", aw.Name)
		printSegs(aw.Segs)
	}

	fmt.Fprintf(w, "\nHigh Airways:\n")
	for _, aw := range sf.HighAirways {
		fmt.Fprintf(w, "\t%s\n", aw.Name)
		printSegs(aw.Segs)
	}

	fmt.Fprintf(w, "\nGeo:\n")
	for _, g := range sf.Geo {
		fmt.Fprintf(w, "\t%s %s (%s)\n", g.P[0], g.P[1], g.Color)
	}

	fmt.Fprintf(w, "\nSIDs:\n")
	for _, sid := range sf.SIDs {
		fmt.Fprintf(w, "\t%s:\n", sid.Name)
		for _, seg := range sid.Segs {
			fmt.Fprintf(w, "\t\t%s %s (%s)\n", seg.P[0], seg.P[1], seg.Color)
		}
	}

	fmt.Fprintf(w, "\nSTARs:\n")
	for _, star := range sf.STARs {
		fmt.Fprintf(w, "\t%s:\n", star.Name)
		for _, seg := range star.Segs {
			fmt.Fprintf(w, "\t\t%s %s (%s)\n", seg.P[0], seg.P[1], seg.Color)
		}
	}

	fmt.Fprintf(w, "\nRegions:\n")
	for _, region := range sf.Regions {
		fmt.Fprintf(w, "\t%s:\n", region.Name)
		for _, pt := range region.P {
			fmt.Fprintf(w, "\t\t%s\n", pt)
		}
	}

	fmt.Fprintf(w, "\nLabels:\n")
	for _, l := range sf.Labels {
		fmt.Fprintf(w, "\t\"%s\" %s %s\n", l.Name, l.P, l.Color)
	}

}

// Returns the latitude-longitude of the point in the format
// "N040.37.58.400 W073.46.17.000"
func (gp LatLong) String() string {
	llstr := func(l float64) string {
		s := fmt.Sprintf("%03d.", int(l))
		l -= math.Floor(l)
		l *= 60.
		s += fmt.Sprintf("%02d.", int(l))
		l -= math.Floor(l)
		l *= 60.
		s += fmt.Sprintf("%02d.", int(l))
		l -= math.Floor(l)
		l *= 1000.
		s += fmt.Sprintf("%03d", int(l))
		return s
	}

	var s string
	if gp.Latitude < 0 {
		s += "S" + llstr(-gp.Latitude)
	} else {
		s += "N" + llstr(gp.Latitude)
	}
	s += " "
	if gp.Longitude < 0 {
		s += "W" + llstr(-gp.Longitude)
	} else {
		s += "E" + llstr(gp.Longitude)
	}

	return s
}

func (s Segment) String() string {
	return fmt.Sprintf("%v %v", s.P[0], s.P[1])
}

///////////////////////////////////////////////////////////////////////////
// sectorFileParser

type sectorFileParser struct {
	file                   []byte
	filename               string
	offset                 int
	lineno                 int
	line                   []byte
	vorMap, ndbMap, fixMap map[string]LatLong
	errorCallback          func(string)
}

func (p *sectorFileParser) GetLine() ([]byte, error) {
	if p.offset == len(p.file) {
		return nil, io.EOF
	}

	// Scan forward until the start of a comment or end of line.
	p.lineno++
	end := p.offset
	for end < len(p.file) {
		c := p.file[end]
		// FIXME: Note that the test for comments will inadvertently
		// consider a label "foo; bar" as having a comment starting after
		// "foo"
		if c == ';' || c == '\r' || c == '\n' {
			break
		}
		end++
	}
	// Grab the good stuff
	p.line = p.file[p.offset:end]

	// Scan past detritus at EOL
	for end < len(p.file) && p.file[end] != '\n' {
		end++
	}
	if end < len(p.file) {
		end++ // skip newline
	}
	p.offset = end

	return p.line, nil
}

func (p *sectorFileParser) SyntaxError(f string, args ...interface{}) {
	err := fmt.Sprintf(fmt.Sprintf("%s:%d: syntax error: %s\n", p.filename, p.lineno, f), args...)
	err += fmt.Sprintf("\t%s\n", p.line)
	p.errorCallback(err)
	os.Exit(1)
}

func isSectionSeparator(s []byte) bool {
	return len(s) > 0 && s[0] == '[' && strings.Index(string(s), "]") != -1
}

func (p *sectorFileParser) parseLatLong(l []byte) float64 {
	if l[0] != 'N' && l[0] != 'S' && l[0] != 'E' && l[0] != 'W' {
		p.SyntaxError("Malformed latitude/longitude: %s", l)
	}

	// skip NSEW
	start := 1

	// get end at the next ".".
	end := start
	for l[end] != '.' {
		end += 1
		if end == len(l) {
			p.SyntaxError("Malformed latitude/longitude: %s", l)
		}
	}
	ll := float64(p.atof(l[start:end]))

	start = end + 1
	end = start
	for l[end] != '.' {
		end += 1
		if end == len(l) {
			p.SyntaxError("Malformed latitude/longitude: %s", l)
		}
	}
	ll += float64(p.atof(l[start:end])) / 60.

	ll += float64(p.atof(l[end+1:])) / 3600.

	if l[0] == 'S' || l[0] == 'W' {
		ll = -ll
	}

	return ll
}

func isSpace(c byte) bool {
	return c == ' ' || c == '\t'
}

func fields(s []byte) [][]byte {
	var f [][]byte
	start := 0
	for start < len(s) {
		if isSpace(s[start]) {
			start++
			continue
		}

		end := start
		for end < len(s) && !isSpace(s[end]) {
			end++
		}
		f = append(f, s[start:end])

		start = end
	}

	return f
}

func (p *sectorFileParser) atof(s []byte) float64 {
	if v, err := strconv.ParseFloat(strings.TrimSpace(string(s)), 64); err != nil {
		p.SyntaxError("%s: %s", err, string(s))
		return 0.
	} else {
		return v
	}
}

func (p *sectorFileParser) atoi(s []byte) int {
	if v, err := strconv.ParseInt(strings.TrimSpace(string(s)), 10, 32); err != nil {
		p.SyntaxError("%s: %s", err, string(s))
		return 0
	} else {
		return int(v)
	}
}

// Parses the provided contents, which are assumed to be a SCT2 format
// sector file.  The implementation assumes that the sector file is ASCII
// text.  The provided filename is used only when generating error
// messages.  If a syntax error is encountered during parsing, the syntax
// callback is called with an error message.  Note that the parser does not
// attempt to recover and continue after encountering a parsing error.
func Parse(contents []byte, filename string, syntax func(string)) (*SectorFile, error) {
	sectorFile := &SectorFile{}

	p := &sectorFileParser{file: contents,
		filename:      filename,
		errorCallback: syntax,
		vorMap:        make(map[string]LatLong),
		ndbMap:        make(map[string]LatLong),
		fixMap:        make(map[string]LatLong)}

	// Process #defines
	var section []byte
	for {
		line, err := p.GetLine()
		if err == io.EOF {
			p.SyntaxError("Premature EOF in #defines")
		}
		if len(line) == 0 {
			continue
		}
		if isSectionSeparator(line) {
			// we're on to the regular sections
			section = line
			break
		}

		f := fields(line)
		if len(f) != 3 {
			p.SyntaxError("Expected 3 fields: %+v", f)
		}
		if string(f[0]) != "#define" {
			p.SyntaxError("Expected #define for first token")
		}

		nc := NamedColor{int24ToRGB(p.atoi(f[2])), string(f[1])}
		sectorFile.Colors = append(sectorFile.Colors, nc)
	}

	// Separate out the sections so they can be parsed in parallel.
	// Precondition: |section| is set above to the initial one
	sectionLines := make(map[string][][]byte)
	var currentSectionLines [][]byte
	for {
		line, err := p.GetLine()
		if err == io.EOF {
			break
		}
		if len(line) == 0 {
			continue
		}
		if isSectionSeparator(line) {
			// finish the current one
			sectionLines[string(section)] = currentSectionLines
			currentSectionLines = nil
			section = line
		} else {
			// Don't bother if the line is entirely whitespace.
			s := 0
			for s < len(line) && isSpace(line[s]) {
				s++
			}
			if s < len(line) {
				currentSectionLines = append(currentSectionLines, line)
			}
		}
	}

	// Slightly tricky: we need to parse VORs, NDBs, and fixes first, since
	// their names may be referred to to define locations in other sections.
	var wg sync.WaitGroup
	for _, section := range []string{"[VOR]", "[NDB]", "[FIXES]"} {
		if lines, ok := sectionLines[section]; ok {
			wg.Add(1)
			go func(section string, lines [][]byte) {
				parseSection(section, lines, p, sectorFile)
				wg.Done()
			}(section, lines)
		}
	}
	wg.Wait()

	for section, lines := range sectionLines {
		if section == "[VOR]" || section == "[NDB]" || section == "[FIXES]" {
			continue
		}
		wg.Add(1)
		go func(section string, lines [][]byte) {
			parseSection(section, lines, p, sectorFile)
			wg.Done()
		}(section, lines)
	}
	wg.Wait()

	return sectorFile, nil
}

func parseSection(section string, lines [][]byte, p *sectorFileParser, sectorFile *SectorFile) {
	vnfPos := func(t []byte) LatLong {
		token := string(t)
		if loc, ok := p.vorMap[token]; ok {
			return loc
		} else if loc, ok := p.ndbMap[token]; ok {
			return loc
		} else if loc, ok := p.fixMap[token]; ok {
			return loc
		} else {
			p.SyntaxError("%s: named VOR/NDB/fix not found", token)
			return LatLong{}
		}
	}

	// handle a VOR, NDB, fix, or an actual numeric position...
	parseLatitude := func(token []byte) float64 {
		if len(token) <= 5 {
			// it should be a VOR, NDB, or fix.
			return vnfPos(token).Latitude
		} else {
			return p.parseLatLong(token)
		}
	}
	parseLongitude := func(token []byte) float64 {
		if len(token) <= 5 {
			// it should be a VOR, NDB, or fix.
			return vnfPos(token).Longitude
		} else {
			return p.parseLatLong(token)
		}
	}

	parseloc := func(tokens [][]byte) LatLong {
		return LatLong{Latitude: parseLatitude(tokens[0]),
			Longitude: parseLongitude(tokens[1])}
	}
	parseseg := func(tokens [][]byte) Segment {
		var s Segment
		s.P[0] = parseloc(tokens[0:2])
		s.P[1] = parseloc(tokens[2:4])
		return s
	}

	parseNamedSegments := func() map[string]*[]Segment {
		m := make(map[string]*[]Segment)

		for _, line := range lines {
			f := fields(line)
			if len(f) != 5 {
				p.SyntaxError("Expected 5 fields for named segment")
			}

			name := string(f[0])
			seg := parseseg(f[1:])

			segs, ok := m[name]
			if !ok {
				segs = &[]Segment{}
				m[name] = segs
			}
			*segs = append(*segs, seg)
		}

		return m
	}

	parseARTCC := func() []ARTCC {
		m := parseNamedSegments()

		var artcc []ARTCC
		for name, segs := range m {
			artcc = append(artcc, ARTCC{name, *segs})
		}
		return artcc
	}
	parseAirway := func() []Airway {
		m := parseNamedSegments()

		var aw []Airway
		for name, segs := range m {
			aw = append(aw, Airway{name, *segs})
		}
		return aw
	}

	// [SID], [STAR]
	parseSidStar := func() []SidStar {
		var ns []SidStar

		for _, line := range lines {
			if isSpace(line[0]) {
				if len(ns) == 0 {
					p.SyntaxError("Unexpected whitespace at start of line in SIDs/STARs")
				}
			} else {
				// First 26 characters, always (whitespace padded as needed)
				ns = append(ns, SidStar{Name: strings.TrimSpace(string(line[0:26]))})
				line = line[26:]
			}

			f := fields(line)
			if len(f) != 4 && len(f) != 5 {
				p.SyntaxError("Expected 4 or 5 fields")
			}

			var cs ColoredSegment
			cs.P[0] = parseloc(f[0:2])
			cs.P[1] = parseloc(f[2:4])
			if len(f) == 5 {
				cs.Color = string(f[4])
			}
			ns[len(ns)-1].Segs = append(ns[len(ns)-1].Segs, cs)
		}

		return ns
	}

	// Process a section [FOO]
	switch string(section) {
	case "[INFO]":
		// [INFO] is special since it's a fixed number of lines in
		// sequence, each with a fixed meaning.
		sectorFile.Id = string(lines[0])
		sectorFile.DefaultCallsign = string(lines[1])
		sectorFile.DefaultAirport = string(lines[2])
		sectorFile.Center.Latitude = parseLatitude(lines[3])
		sectorFile.Center.Longitude = parseLongitude(lines[4])
		sectorFile.NmPerLatitude = p.atof(lines[5])
		sectorFile.NmPerLongitude = p.atof(lines[6])
		sectorFile.MagneticVariation = p.atof(lines[7])
		sectorFile.Scale = p.atof(lines[8])

	case "[VOR]":
		// Format: name freq lat long
		for _, line := range lines {
			f := fields(line)
			if len(f) != 4 {
				p.SyntaxError("Expected 4 fields")
			}
			name := string(f[0])
			pos := parseloc(f[2:4])
			sectorFile.VORs = append(sectorFile.VORs, NamedLatLong{pos, name})
			p.vorMap[name] = pos
		}

	case "[NDB]":
		// Format: name freq lat long
		for _, line := range lines {
			f := fields(line)
			if len(f) != 4 {
				p.SyntaxError("Expected 4 fields")
			}
			name := string(f[0])
			pos := parseloc(f[2:4])
			sectorFile.NDBs = append(sectorFile.NDBs, NamedLatLong{pos, name})
			p.ndbMap[name] = pos
		}

	case "[AIRPORT]":
		// Format: name freq lat long airspace
		for _, line := range lines {
			f := fields(line)
			if len(f) != 5 {
				p.SyntaxError("Expected 5 fields")
			}
			name := string(f[0])
			pos := parseloc(f[2:4])
			sectorFile.Airports = append(sectorFile.Airports, NamedLatLong{pos, name})
		}

	case "[RUNWAY]":
		for _, line := range lines {
			f := fields(line)
			if len(f) < 9 {
				p.SyntaxError("Expected at least 9 fields")
			}

			var r Runway
			r.Number[0] = string(f[0])
			r.Number[1] = string(f[1])
			r.Heading[0] = p.atof(f[2])
			r.Heading[1] = p.atof(f[3])
			r.P[0] = parseloc(f[4:6])
			r.P[1] = parseloc(f[6:8])
			r.Airport = string(f[8])

			sectorFile.Runways = append(sectorFile.Runways, r)
		}

	case "[FIXES]":
		for _, line := range lines {
			f := fields(line)
			if len(f) != 3 {
				p.SyntaxError("Expected 3 fields")
			}
			name := string(f[0])
			pos := parseloc(f[1:3])
			sectorFile.Fixes = append(sectorFile.Fixes, NamedLatLong{pos, name})
			p.fixMap[name] = pos
		}

	case "[ARTCC]":
		sectorFile.ARTCC = parseARTCC()

	case "[ARTCC LOW]":
		sectorFile.ARTCCLow = parseARTCC()

	case "[ARTCC HIGH]":
		sectorFile.ARTCCHigh = parseARTCC()

	case "[SID]":
		sectorFile.SIDs = parseSidStar()

	case "[STAR]":
		sectorFile.STARs = parseSidStar()

	case "[LOW AIRWAY]":
		sectorFile.LowAirways = parseAirway()

	case "[HIGH AIRWAY]":
		sectorFile.HighAirways = parseAirway()

	case "[GEO]":
		for _, line := range lines {
			f := fields(line)
			if len(f) != 5 {
				// WAR for error (I think) in ZNY sct file.
				fmt.Fprintf(os.Stderr, "Skipping malformed GEO line: %s\n", line)
				return
			}

			seg := parseseg(f[0:4])
			sectorFile.Geo = append(sectorFile.Geo, Geo{seg, string(f[4])})
		}

	case "[REGIONS]":
		for _, line := range lines {
			if !isSpace(line[0]) {
				// Start a new region
				f := fields(line)
				sectorFile.Regions = append(sectorFile.Regions, NamedPoints{Name: string(f[0])})
				line = line[len(f[0]):]
			}

			f := fields(line)
			if len(f) != 2 {
				p.SyntaxError("Expected 2 fields")
			}

			pos := parseloc(f[:2])
			sectorFile.Regions[len(sectorFile.Regions)-1].P =
				append(sectorFile.Regions[len(sectorFile.Regions)-1].P, pos)
		}

	case "[LABELS]":
		for _, line := range lines {
			if line[0] != '"' {
				p.SyntaxError("Missing opening quote for label")
			}
			line = line[1:]

			label, rest, found := strings.Cut(string(line), "\"")
			if !found {
				p.SyntaxError("Unable to find closing quote for label")
			}

			f := fields([]byte(rest))
			if len(f) != 3 {
				p.SyntaxError("Expected 3 fields; got %+v", rest, fields)
			}

			p := parseloc(f[:2])
			l := Label{Name: label, P: p, Color: string(f[2])}
			sectorFile.Labels = append(sectorFile.Labels, l)
		}

	default:
		p.SyntaxError("Unexpected section name: %s", string(section))
	}
}
