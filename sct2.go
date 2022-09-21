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
	"strconv"
	"strings"
	"sync"
)

/* Open issues:
- EDGG_1512a.sct does things like this in the ARTCC section, both having a color at the end
  and also an apparent implicit assumption that the name can be skipped after the first time.

EDGGDFA07H_FL115_FL135    N000.00.00.000 E000.00.00.000 N000.00.00.000 E000.00.00.000 AppSector
                          N049.52.14.000 E008.32.50.000 N049.52.59.000 E008.29.12.000 AppSector
                          N049.52.59.000 E008.29.12.000 N049.54.12.000 E008.23.15.000 AppSector

*/

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
	Name     string
	Segments []Segment
	Colors   []string
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
		fmt.Fprintf(w, "\t%s\n", g.Name)
		for i := range g.Segments {
			fmt.Fprintf(w, "\t\t%s - %s (%s)\n", g.Segments[i].P[0], g.Segments[i].P[1], g.Colors[i])
		}
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

type sctLine struct {
	text   string
	lineno int
}

type sectorFileParser struct {
	file                               []byte
	filename                           string
	offset                             int
	lineno                             int
	lines                              []sctLine
	vorMap, ndbMap, fixMap, airportMap map[string]LatLong
	mapMutex                           sync.Mutex
	errorCallback                      func(string)
}

func (p *sectorFileParser) GetLine() (sctLine, error) {
	if p.offset == len(p.file) {
		return sctLine{}, io.EOF
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
	contents := p.file[p.offset:end]

	// Scan past detritus at EOL
	for end < len(p.file) && p.file[end] != '\n' {
		end++
	}
	if end < len(p.file) {
		end++ // skip newline
	}
	p.offset = end

	return sctLine{text: string(contents), lineno: p.lineno}, nil
}

func (p *sectorFileParser) SyntaxError(l sctLine, f string, args ...interface{}) {
	err := fmt.Sprintf(fmt.Sprintf("%s:%d: syntax error: %s\n", p.filename, l.lineno, f), args...)
	err += fmt.Sprintf("\t%s\n", l.text)
	p.errorCallback(err)
}

func isSectionSeparator(s string) bool {
	return len(s) > 0 && s[0] == '[' && strings.Index(string(s), "]") != -1
}

func parseLatLong(l string, isLatitude bool) (float64, error) {
	if isLatitude && l[0] != 'N' && l[0] != 'S' {
		return 0, fmt.Errorf("Malformed latitude: %s", l)
	} else if !isLatitude && l[0] != 'E' && l[0] != 'W' {
		return 0, fmt.Errorf("Malformed longitude: %s", l)
	}

	ll := 0.
	div := []float64{1, 60, 60, 1000}
	d := 1.
	for i, num := range strings.Split(l[1:], ".") {
		if i > len(div) {
			return 0, fmt.Errorf("More dotted entries than expected in latlong value")
		} else if val, err := atof(num); err != nil {
			return 0, err
		} else {
			d *= div[i]
			ll += val / d
		}
	}

	if l[0] == 'S' || l[0] == 'W' {
		ll = -ll
	}

	return ll, nil
}

func isSpace(c byte) bool {
	return c == ' ' || c == '\t'
}

func atof(s string) (float64, error) {
	return strconv.ParseFloat(strings.TrimSpace(s), 64)
}

func atoi(s string) (int, error) {
	return strconv.Atoi(strings.TrimSpace(s))
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
		fixMap:        make(map[string]LatLong),
		airportMap:    make(map[string]LatLong)}

	// Process #defines
	var section sctLine
	for {
		line, err := p.GetLine()
		if err == io.EOF {
			p.SyntaxError(line, "Premature EOF in #defines")
			break
		}
		if len(line.text) == 0 {
			continue
		}
		if isSectionSeparator(line.text) {
			// we're on to the regular sections
			section = line
			break
		}

		f := strings.Fields(line.text)
		if len(f) != 3 {
			p.SyntaxError(line, "Expected 3 fields: %+v", f)
			continue
		}
		if f[0] != "#define" {
			p.SyntaxError(line, "Expected #define for first token")
			continue
		}

		if irgb, err := atoi(f[2]); err != nil {
			p.SyntaxError(line, err.Error())
		} else {
			nc := NamedColor{int24ToRGB(irgb), f[1]}
			sectorFile.Colors = append(sectorFile.Colors, nc)
		}
	}

	// Separate out the sections so they can be parsed in parallel.
	// Precondition: |section| is set above to the initial section and the
	// parser is ready to give its first line.
	sectionLines := make(map[string][]sctLine)
	var currentSectionLines []sctLine
	for {
		line, err := p.GetLine()
		if err == io.EOF {
			if len(currentSectionLines) > 0 {
				sectionLines[strings.TrimSpace(section.text)] = currentSectionLines
			}
			break
		}
		if isSectionSeparator(line.text) {
			// finish the current one
			sectionLines[strings.TrimSpace(section.text)] = currentSectionLines
			currentSectionLines = nil
			section = line
		} else {
			// Don't bother if the line is entirely whitespace.
			if strings.TrimSpace(line.text) != "" {
				currentSectionLines = append(currentSectionLines, line)
			}
		}
	}

	// Slightly tricky: we need to parse VORs, NDBs, airports, and fixes
	// first, since their names may be referred to to define locations in
	// other sections.
	var wg sync.WaitGroup
	for _, section := range []string{"[VOR]", "[NDB]", "[FIXES]", "[AIRPORT]"} {
		if lines, ok := sectionLines[section]; ok {
			wg.Add(1)
			go func(section string, lines []sctLine) {
				parseSection(section, lines, p, sectorFile)
				wg.Done()
			}(section, lines)
		}
	}
	wg.Wait()

	for section, lines := range sectionLines {
		if section == "[VOR]" || section == "[NDB]" || section == "[FIXES]" || section == "[AIRPORT]" {
			continue
		}
		wg.Add(1)
		go func(section string, lines []sctLine) {
			parseSection(section, lines, p, sectorFile)
			wg.Done()
		}(section, lines)
	}
	wg.Wait()

	return sectorFile, nil
}

func parseSection(section string, lines []sctLine, p *sectorFileParser, sectorFile *SectorFile) {
	vnfPos := func(token string) (LatLong, error) {
		p.mapMutex.Lock()
		defer p.mapMutex.Unlock()

		if loc, ok := p.vorMap[token]; ok {
			return loc, nil
		} else if loc, ok := p.ndbMap[token]; ok {
			return loc, nil
		} else if loc, ok := p.fixMap[token]; ok {
			return loc, nil
		} else if loc, ok := p.airportMap[token]; ok {
			return loc, nil
		} else {
			return LatLong{}, fmt.Errorf("%s: named VOR/NDB/fix not found", token)
		}
	}

	// handle a VOR, NDB, fix, or an actual numeric position...
	parseLatitude := func(token string) (float64, error) {
		if pos, err := vnfPos(token); err == nil {
			return pos.Latitude, nil
		}
		return parseLatLong(token, true)
	}
	parseLongitude := func(token string) (float64, error) {
		if pos, err := vnfPos(token); err == nil {
			return pos.Longitude, nil
		}
		return parseLatLong(token, false)
	}

	parseloc := func(tokens []string) (LatLong, error) {
		if lat, err := parseLatitude(tokens[0]); err != nil {
			return LatLong{}, err
		} else if long, err := parseLongitude(tokens[1]); err != nil {
			return LatLong{}, err
		} else {
			return LatLong{Latitude: lat, Longitude: long}, nil
		}
	}

	parseseg := func(tokens []string) (Segment, error) {
		if p0, err := parseloc(tokens[0:2]); err != nil {
			return Segment{}, err
		} else if p1, err := parseloc(tokens[2:4]); err != nil {
			return Segment{}, err
		} else {
			return Segment{P: [2]LatLong{p0, p1}}, nil
		}
	}

	parseNamedSegments := func() map[string]*[]Segment {
		m := make(map[string]*[]Segment)

		for _, line := range lines {
			f := strings.Fields(line.text)
			if len(f) < 5 {
				p.SyntaxError(line, "Expected 5+ fields for named segment: \"%s\"", line.text)
				continue
			}

			// We need to deal with things like:
			// EGNS Isle of Man CTA N053.58.00.066 W004.18.42.359 N053.58.04.867 W004.18.20.829
			// so we'll take the last four as the segment and whatever is
			// left at the start as its name.
			name := strings.Join(f[0:len(f)-4], " ")
			if seg, err := parseseg(f[len(f)-4:]); err != nil {
				p.SyntaxError(line, err.Error())
			} else {
				segs, ok := m[name]
				if !ok {
					segs = &[]Segment{}
					m[name] = segs
				}
				*segs = append(*segs, seg)
			}
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
		nsIndex := -1

		for _, line := range lines {
			text := line.text

			f := strings.Fields(text)
			if isSpace(text[0]) {
				if len(ns) == 0 {
					p.SyntaxError(line, "Unexpected whitespace at start of line in SIDs/STARs")
					continue
				}
			} else {
				// The spec says that the name is in the first 26
				// characters, though lots of SCT files seem to play fast
				// and loose with that. So we'll try that first (since some
				// go right up and then go into the segment w/o any space).
				name := ""
				if len(text) > 30 {
					f2 := strings.Fields(text[26:])
					if len(f2) >= 4 {
						if _, err := parseseg(f2[:4]); err == nil {
							name = strings.TrimSpace(text[:26])
							f = f2
						}
					}
				}

				// If that didn't't work, we'll basically do what we do for
				// Geo and poke around until we find a full valid segment.
				if name == "" {
					if len(f) < 5 {
						p.SyntaxError(line, "Expected at least 5 fields")
						continue
					}
					i := 1
					for ; i+4 <= len(f); i++ {
						if _, err := parseseg(f[i : i+4]); err == nil {
							break
						}
					}
					if i+4 > len(f) {
						p.SyntaxError(line, "Did not find valid segment")
						continue
					}

					name = strings.Join(f[0:i], " ")
					f = f[i:]
				}

				// Does this name already exist? If so, we'll add to it.
				nsIndex = -1
				for i, ss := range ns {
					if ss.Name == name {
						nsIndex = i
						break
					}
				}
				if nsIndex == -1 {
					nsIndex = len(ns)
					ns = append(ns, SidStar{Name: name})
				}
			}

			var cs ColoredSegment
			var err error
			if cs.P[0], err = parseloc(f[0:2]); err != nil {
				p.SyntaxError(line, err.Error())
			} else if cs.P[1], err = parseloc(f[2:4]); err != nil {
				p.SyntaxError(line, err.Error())
			} else {
				if len(f) == 5 {
					cs.Color = string(f[4])
				}
				ns[nsIndex].Segs = append(ns[nsIndex].Segs, cs)
			}
		}

		return ns
	}

	// Process a section [FOO]
	switch section {
	case "[INFO]":
		// [INFO] is special since it's a fixed number of lines in
		// sequence, each with a fixed meaning.
		var err error
		sectorFile.Id = lines[0].text
		sectorFile.DefaultCallsign = lines[1].text
		sectorFile.DefaultAirport = lines[2].text
		if sectorFile.Center.Latitude, err = parseLatitude(lines[3].text); err != nil {
			p.SyntaxError(lines[3], err.Error())
		}
		if sectorFile.Center.Longitude, err = parseLongitude(lines[4].text); err != nil {
			p.SyntaxError(lines[4], err.Error())
		}
		if sectorFile.NmPerLatitude, err = atof(lines[5].text); err != nil {
			p.SyntaxError(lines[5], err.Error())
		}
		if sectorFile.NmPerLongitude, err = atof(lines[6].text); err != nil {
			p.SyntaxError(lines[6], err.Error())
		}
		if sectorFile.MagneticVariation, err = atof(lines[7].text); err != nil {
			p.SyntaxError(lines[7], err.Error())
		}
		if len(lines) >= 9 {
			if sectorFile.Scale, err = atof(lines[8].text); err != nil {
				p.SyntaxError(lines[8], err.Error())
			}
		} else {
			sectorFile.Scale = 1
		}

	case "[VOR]":
		// Format: name freq lat long
		for _, line := range lines {
			f := strings.Fields(line.text)
			if len(f) != 4 {
				p.SyntaxError(line, "Expected 4 fields")
				continue
			}
			name := string(f[0])
			if pos, err := parseloc(f[2:4]); err != nil {
				p.SyntaxError(line, err.Error())
			} else {
				sectorFile.VORs = append(sectorFile.VORs, NamedLatLong{pos, name})

				p.mapMutex.Lock()
				p.vorMap[name] = pos
				p.mapMutex.Unlock()
			}
		}

	case "[NDB]":
		// Format: name freq lat long
		for _, line := range lines {
			f := strings.Fields(line.text)
			if len(f) != 4 {
				p.SyntaxError(line, "Expected 4 fields")
				continue
			}
			name := f[0]
			if pos, err := parseloc(f[2:4]); err != nil {
				p.SyntaxError(line, err.Error())
			} else {
				sectorFile.NDBs = append(sectorFile.NDBs, NamedLatLong{pos, name})
				p.mapMutex.Lock()
				p.ndbMap[name] = pos
				p.mapMutex.Unlock()
			}
		}

	case "[AIRPORT]":
		// Format: name freq lat long airspace
		for _, line := range lines {
			f := strings.Fields(line.text)
			if len(f) < 4 {
				p.SyntaxError(line, "Expected at least 4 fields")
				continue
			}
			name := f[0]
			// f[1] is tower frequency
			if pos, err := parseloc(f[2:4]); err != nil {
				p.SyntaxError(line, err.Error())
			} else {
				// f[4], which is not always present, is the airspace
				sectorFile.Airports = append(sectorFile.Airports, NamedLatLong{pos, name})

				if name != "" {
					p.mapMutex.Lock()
					p.airportMap[name] = pos
					p.mapMutex.Unlock()
				}
			}
		}

	case "[RUNWAY]":
		for _, line := range lines {
			f := strings.Fields(line.text)
			if len(f) < 8 {
				p.SyntaxError(line, "Expected at least 8 fields")
				continue
			}

			var r Runway
			var err error
			r.Number[0] = f[0]
			r.Number[1] = f[1]
			if r.Heading[0], err = atof(f[2]); err != nil {
				p.SyntaxError(line, err.Error())
				continue
			}
			if r.Heading[1], err = atof(f[3]); err != nil {
				p.SyntaxError(line, err.Error())
				continue
			}
			if r.P[0], err = parseloc(f[4:6]); err != nil {
				p.SyntaxError(line, err.Error())
				continue
			}
			if r.P[1], err = parseloc(f[6:8]); err != nil {
				p.SyntaxError(line, err.Error())
				continue
			}
			if len(f) == 9 { // sometimes it's commented out...
				r.Airport = f[8]
			}

			sectorFile.Runways = append(sectorFile.Runways, r)
		}

	case "[FIXES]":
		for _, line := range lines {
			f := strings.Fields(line.text)
			if len(f) != 3 {
				p.SyntaxError(line, "Expected 3 fields")
				continue
			}
			name := f[0]
			if pos, err := parseloc(f[1:3]); err != nil {
				p.SyntaxError(line, err.Error())
				continue
			} else {
				sectorFile.Fixes = append(sectorFile.Fixes, NamedLatLong{pos, name})
				p.mapMutex.Lock()
				p.fixMap[name] = pos
				p.mapMutex.Unlock()
			}
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
		geoIdx := -1
		for _, line := range lines {
			f := strings.Fields(line.text)
			if len(f) < 4 {
				p.SyntaxError(line, "Skipping malformed GEO line")
				continue
			}

			if _, err := parseseg(f[0:4]); err != nil {
				// start of a new section; there may be spaces in the name,
				// so keep going until we find a latitude
				name := ""
				var i int
				for i = 0; i+4 <= len(f); i++ {
					if _, err := parseseg(f[i : i+4]); err == nil {
						break
					}
					if i > 0 {
						name = name + " "
					}
					name = name + f[i]
				}

				if i+4 > len(f) {
					p.SyntaxError(line, "Segment not found after name \"%s\"?", name)
					continue
				}

				if seg, err := parseseg(f[i : i+4]); err != nil {
					p.SyntaxError(line, err.Error())
				} else {
					// Do we already have an entry for this name?
					geoIdx = -1
					for i, g := range sectorFile.Geo {
						if g.Name == name {
							geoIdx = i
							break
						}
					}
					if geoIdx == -1 {
						geoIdx = len(sectorFile.Geo)
						sectorFile.Geo = append(sectorFile.Geo, Geo{Name: name})
					}

					sectorFile.Geo[geoIdx].Segments = append(sectorFile.Geo[geoIdx].Segments, seg)
					// Sometimes this is omitted for the name line, which
					// usually has bogus points anyway...
					color := ""
					if i+4 < len(f) {
						color = f[i+4]
					}
					sectorFile.Geo[geoIdx].Colors = append(sectorFile.Geo[geoIdx].Colors, color)
				}
			} else {
				// another segment for the current section
				if len(sectorFile.Geo) == 0 {
					// Special case: if the first line goes straight into
					// segments, then have an initial default section.
					sectorFile.Geo = append(sectorFile.Geo, Geo{Name: "default"})
					geoIdx = 0
				}

				if seg, err := parseseg(f[0:4]); err != nil {
					p.SyntaxError(line, err.Error())
				} else {
					sectorFile.Geo[geoIdx].Segments = append(sectorFile.Geo[geoIdx].Segments, seg)
					if len(f) >= 5 {
						sectorFile.Geo[geoIdx].Colors = append(sectorFile.Geo[geoIdx].Colors, f[4])
					} else {
						sectorFile.Geo[geoIdx].Colors = append(sectorFile.Geo[geoIdx].Colors, "")
					}
				}
			}
		}

	case "[REGIONS]":
		for _, line := range lines {
			text := line.text
			if !isSpace(text[0]) {
				// Start a new region
				f := strings.Fields(line.text)
				sectorFile.Regions = append(sectorFile.Regions, NamedPoints{Name: string(f[0])})
				text = text[len(f[0]):]
			}

			f := strings.Fields(text)
			if len(f) != 2 {
				p.SyntaxError(line, "Expected 2 fields")
				continue
			}

			if pos, err := parseloc(f[:2]); err != nil {
				p.SyntaxError(line, err.Error())
			} else {
				sectorFile.Regions[len(sectorFile.Regions)-1].P =
					append(sectorFile.Regions[len(sectorFile.Regions)-1].P, pos)
			}
		}

	case "[LABELS]":
		for _, line := range lines {
			if line.text[0] != '"' {
				p.SyntaxError(line, "Missing opening quote for label")
				continue
			}
			text := line.text[1:]

			label, rest, found := strings.Cut(text, "\"")
			if !found {
				p.SyntaxError(line, "Unable to find closing quote for label")
				continue
			}

			f := strings.Fields(rest)
			if len(f) != 3 {
				p.SyntaxError(line, "Expected 3 fields; got %+v", rest, f)
				continue
			}

			if pos, err := parseloc(f[:2]); err != nil {
				p.SyntaxError(line, err.Error())
			} else {
				l := Label{Name: label, P: pos, Color: f[2]}
				sectorFile.Labels = append(sectorFile.Labels, l)
			}
		}

	default:
		p.SyntaxError(sctLine{}, "Unexpected section name: \"%s\"", section)
	}
}
