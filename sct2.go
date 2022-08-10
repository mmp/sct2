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

	Colors      map[string]RGB // Colors defined via #define in the sector file
	VORs        map[string]LatLong
	NDBs        map[string]LatLong
	Airports    map[string]LatLong
	Fixes       map[string]LatLong
	ARTCC       map[string][]Segment // ARTCC boundary
	ARTCCLow    map[string][]Segment // Low boundary of ARTCC
	ARTCCHigh   map[string][]Segment // High boundary of ARTCC
	LowAirways  map[string][]Segment
	HighAirways map[string][]Segment
	Geo         []ColoredSegment
	SIDs        []NamedSegments
	STARs       []NamedSegments
	Regions     []NamedPoints
	Labels      []Label
	Runways     []Runway
}

type RGB struct {
	R, G, B float32
}

// LatLong encodes a position using latitude-longitude.
type LatLong struct {
	Latitude, Longitude float64
}

// Segment represents a line segment between two positions.
type Segment struct {
	P [2]LatLong
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

// NamedPoints represents a sequence of segments (e.g., an airway) with an
// associated name.
type NamedSegments struct {
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
	for name, color := range sf.Colors {
		fmt.Fprintf(w, "\t%s: (%f, %f, %f)\n", name, color.R, color.G, color.B)
	}

	fmt.Fprintf(w, "\nVORs:\n")
	for name, vor := range sf.VORs {
		fmt.Fprintf(w, "\t%s %s\n", name, vor)
	}

	fmt.Fprintf(w, "\nNDBs:\n")
	for name, ndb := range sf.NDBs {
		fmt.Fprintf(w, "\t%s %s\n", name, ndb)
	}

	fmt.Fprintf(w, "\nAirports:\n")
	for name, ap := range sf.Airports {
		fmt.Fprintf(w, "\t%s %s\n", name, ap)
	}

	fmt.Fprintf(w, "\nFixes:\n")
	for name, fix := range sf.Fixes {
		fmt.Fprintf(w, "\t%s %s\n", name, fix)
	}

	fmt.Fprintf(w, "\nRunways:\n")
	for _, r := range sf.Runways {
		fmt.Fprintf(w, "\t%s: %s/%s (%.1f/%.1f) %s - %s\n", r.Airport,
			r.Number[0], r.Number[1], r.Heading[0], r.Heading[1], r.P[0], r.P[1])
	}

	printNamedSegs := func(m map[string][]Segment) {
		for name, segs := range m {
			fmt.Fprintf(w, "\t%s:\n", name)
			for _, s := range segs {
				fmt.Fprintf(w, "\t\t%s - %s\n", s.P[0], s.P[1])
			}
		}
	}

	fmt.Fprintf(w, "\nARTCC:\n")
	printNamedSegs(sf.ARTCC)

	fmt.Fprintf(w, "\nARTCC Low:\n")
	printNamedSegs(sf.ARTCCLow)

	fmt.Fprintf(w, "\nARTCC High:\n")
	printNamedSegs(sf.ARTCCHigh)

	fmt.Fprintf(w, "\nLow Airways:\n")
	printNamedSegs(sf.LowAirways)

	fmt.Fprintf(w, "\nHigh Airways:\n")
	printNamedSegs(sf.HighAirways)

	fmt.Fprintf(w, "\nGeo:\n")
	for _, g := range sf.Geo {
		fmt.Fprintf(w, "\t%s %s (%s)\n", g.P[0], g.P[1], g.Color)
	}

	fmt.Fprintf(w, "\nSIDs:\n")
	for _, sid := range sf.SIDs {
		fmt.Fprintf(w, "\t%s:\n", sid.Name)
		for _, seg := range sid.Segs {
			fmt.Fprintf(w, "\t\t%s %s (%f, %f, %f)\n", seg.P[0], seg.P[1], seg.Color)
		}
	}

	fmt.Fprintf(w, "\nSTARs:\n")
	for _, sid := range sf.STARs {
		fmt.Fprintf(w, "\t%s:\n", sid.Name)
		for _, seg := range sid.Segs {
			fmt.Fprintf(w, "\t\t%s %s (%f, %f, %f)\n", seg.P[0], seg.P[1], seg.Color)
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
	file          []byte
	filename      string
	offset        int
	lineno        int
	line          []byte
	errorCallback func(string)
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

func newSectorFile() *SectorFile {
	var sf SectorFile
	sf.Colors = make(map[string]RGB)
	sf.VORs = make(map[string]LatLong)
	sf.NDBs = make(map[string]LatLong)
	sf.Airports = make(map[string]LatLong)
	sf.Fixes = make(map[string]LatLong)
	sf.ARTCC = make(map[string][]Segment)
	sf.ARTCCLow = make(map[string][]Segment)
	sf.ARTCCHigh = make(map[string][]Segment)
	sf.LowAirways = make(map[string][]Segment)
	sf.HighAirways = make(map[string][]Segment)
	return &sf
}

func isGroupSeparator(s []byte) bool {
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
	sectorFile := newSectorFile()

	p := &sectorFileParser{file: contents, filename: filename, errorCallback: syntax}

	// Process #defines
	var group []byte
	for {
		line, err := p.GetLine()
		if err == io.EOF {
			p.SyntaxError("Premature EOF in #defines")
		}
		if len(line) == 0 {
			continue
		}
		if isGroupSeparator(line) {
			// we're on to the regular sections
			group = line
			break
		}

		f := fields(line)
		if len(f) != 3 {
			p.SyntaxError("Expected 3 fields: %+v", f)
		}
		if string(f[0]) != "#define" {
			p.SyntaxError("Expected #define for first token")
		}

		sectorFile.Colors[string(f[1])] = int24ToRGB(p.atoi(f[2]))
	}

	vnfPos := func(t []byte) LatLong {
		token := string(t)
		if loc, ok := sectorFile.VORs[token]; ok {
			return loc
		} else if loc, ok := sectorFile.NDBs[token]; ok {
			return loc
		} else if loc, ok := sectorFile.Fixes[token]; ok {
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

	// For a group [FOO] in a sector file, call the provided callback with
	// every line of text in the group definition.
	parsegroup := func(cb func([]byte)) []byte {
		for {
			line, err := p.GetLine()
			if err == io.EOF {
				return nil
			} else if isGroupSeparator(line) {
				return line
			}

			// Don't bother with the callback if the line is entirely
			// whitespace.
			s := 0
			for s < len(line) && isSpace(line[s]) {
				s++
			}
			if s < len(line) {
				cb(line)
			}
		}
	}

	parseARTCC := func(m map[string][]Segment) []byte {
		group := parsegroup(func(line []byte) {
			f := fields(line)
			if len(f) != 5 {
				p.SyntaxError("Expected 5 fields for ARTCC")
			}

			name := string(f[0])
			seg := parseseg(f[1:])
			m[name] = append(m[name], seg)
		})
		return group
	}

	// [SID], [STAR]
	parseNamedSegments := func() ([]NamedSegments, []byte) {
		var ns []NamedSegments

		group := parsegroup(func(line []byte) {
			if isSpace(line[0]) {
				if len(ns) == 0 {
					p.SyntaxError("Unexpected whitespace at start of line in SIDs/STARs")
				}
			} else {
				// First 26 characters, always (whitespace padded as needed)
				ns = append(ns, NamedSegments{Name: strings.TrimSpace(string(line[0:26]))})
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
		})

		return ns, group
	}

	// Process all of the [FOO] groups.
	for group != nil {
		switch string(group) {
		case "[INFO]":
			// [INFO] is special since it's a fixed number of lines in
			// sequence, each with a fixed meaning.
			next := func() []byte {
				l, err := p.GetLine()
				if err == io.EOF {
					p.SyntaxError("Premature end of file in [INFO] block")
				}
				return l
			}

			sectorFile.Id = string(next())
			sectorFile.DefaultCallsign = string(next())
			sectorFile.DefaultAirport = string(next())
			sectorFile.Center.Latitude = parseLatitude(next())
			sectorFile.Center.Longitude = parseLongitude(next())
			sectorFile.NmPerLatitude = p.atof(next())
			sectorFile.NmPerLongitude = p.atof(next())
			sectorFile.MagneticVariation = p.atof(next())
			sectorFile.Scale = p.atof(next())

			// advance to the next group
			for {
				line, err := p.GetLine()
				if err == io.EOF {
					group = nil
					break
				} else if isGroupSeparator(line) {
					group = line
					break
				}
			}

		case "[VOR]":
			// Format: name freq lat long
			group = parsegroup(func(line []byte) {
				f := fields(line)
				if len(f) != 4 {
					p.SyntaxError("Expected 4 fields")
				}
				name := string(f[0])
				pos := parseloc(f[2:4])
				sectorFile.VORs[name] = pos
			})

		case "[NDB]":
			// Format: name freq lat long
			group = parsegroup(func(line []byte) {
				f := fields(line)
				if len(f) != 4 {
					p.SyntaxError("Expected 4 fields")
				}
				name := string(f[0])
				pos := parseloc(f[2:4])
				sectorFile.NDBs[name] = pos
			})

		case "[AIRPORT]":
			// Format: name freq lat long airspace
			group = parsegroup(func(line []byte) {
				f := fields(line)
				if len(f) != 5 {
					p.SyntaxError("Expected 5 fields")
				}
				name := string(f[0])
				pos := parseloc(f[2:4])
				sectorFile.Airports[name] = pos
			})

		case "[RUNWAY]":
			group = parsegroup(func(line []byte) {
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
			})

		case "[FIXES]":
			group = parsegroup(func(line []byte) {
				f := fields(line)
				if len(f) != 3 {
					p.SyntaxError("Expected 3 fields")
				}
				name := string(f[0])
				pos := parseloc(f[1:3])
				sectorFile.Fixes[name] = pos
			})

		case "[ARTCC]":
			group = parseARTCC(sectorFile.ARTCC)

		case "[ARTCC LOW]":
			group = parseARTCC(sectorFile.ARTCCLow)

		case "[ARTCC HIGH]":
			group = parseARTCC(sectorFile.ARTCCHigh)

		case "[SID]":
			sectorFile.SIDs, group = parseNamedSegments()

		case "[STAR]":
			sectorFile.STARs, group = parseNamedSegments()

		case "[LOW AIRWAY]":
			group = parsegroup(func(line []byte) {
				f := fields(line)
				if len(f) != 5 {
					p.SyntaxError("Expected 5 fields")
				}
				name := string(f[0])
				seg := parseseg(f[1:5])
				sectorFile.LowAirways[name] = append(sectorFile.LowAirways[name], seg)
			})

		case "[HIGH AIRWAY]":
			group = parsegroup(func(line []byte) {
				f := fields(line)
				if len(f) != 5 {
					p.SyntaxError("Expected 5 fields")
				}
				name := string(f[0])
				seg := parseseg(f[1:5])
				sectorFile.HighAirways[name] = append(sectorFile.HighAirways[name], seg)
			})

		case "[GEO]":
			group = parsegroup(func(line []byte) {
				f := fields(line)
				if len(f) != 5 {
					// WAR for error (I think) in ZNY sct file.
					fmt.Fprintf(os.Stderr, "Skipping malformed GEO line: %s\n", line)
					return
				}

				seg := parseseg(f[0:4])
				sectorFile.Geo = append(sectorFile.Geo, ColoredSegment{seg, string(f[4])})
			})

		case "[REGIONS]":
			group = parsegroup(func(line []byte) {
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
			})

		case "[LABELS]":
			group = parsegroup(func(line []byte) {
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
			})

		default:
			p.SyntaxError("Unexpected group name: %s", string(group))
		}
	}

	return sectorFile, nil
}
