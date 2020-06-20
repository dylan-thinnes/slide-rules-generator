%
% OCR A for MetaType1
% Translated by Matthew Skala from METAFONT code, starting March 2011
%
% I, Matthew Skala, release my work on this to the public domain.  However,
% the copyright notice below, from the METAFONT code I used as a source,
% may be applicable.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% METAFONT-84 Definition for OCR-A Optical Character Recognition Font
% (C) Copyright 1988, 1989 Richard B. Wales.  All Rights Reserved.
%
% Coded by:
%
%	Richard B. Wales
%	Computer Science Department
%	University of California, Los Angeles (UCLA)
%	Los Angeles, California 90024-1596 (USA)
%	+1 (213) 825-5683
%	wales@CS.UCLA.EDU
%
% Source of information:
%
%	ANSI Standard X3.17-1977 (approved January 20, 1977, by the
%	American National Standards Institute, Inc.)
%
% Based on an earlier version by:
%
%	Tor Lillqvist
%	Computing Services
%	Technical Research Centre of Finland
%	TML@FINGATE.BITNET; mcvax!santra!tml
%
% Terms of copyright:
%
%	You may do anything you wish with this font definition, as
%	long as you retain the original copyright notice and credit
%	information intact in the source, and do not charge any fee
%	for distribution of either the source or any fonts derived
%	from it beyond a reasonable copying or communications charge.
%
% Disclaimer:
%
%	While I have made every reasonable effort to make this font
%	definition correspond to the specifications in ANSI Standard
%	X3.17-1977, I do not guarantee that it conforms fully to all
%	official requirements for the OCR-A font, nor that the char-
%	acters produced from this definition will be usable by any
%	particular OCR hardware and/or software.  It is the respon-
%	sibility of the user to determine the usability of this font
%	definition for his application.  Use of this font definition
%	is totally at the user's own risk and without any warranty
%	whatsoever, express or implied.  Neither the author nor his
%	employer will be responsible for any damages, including
%	secondary or consequential damages, arising out of the use of
%	this font definition.
%
%	The four ASCII characters "Tilde", "Underline", "Grave Accent",
%	and "Vertical Bar" do not appear as part of ANSI X3.17-1977.
%	In order to make a complete ASCII character set, I supplemented
%	the standard with my own designs for these four characters.
%	Since these characters are not part of any standard, they will
%	presumably *NOT* be readable by any existing OCR-A equipment.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

beginfont;

string extra_beginchar,extra_endchar;
extra_beginchar="";
extra_endchar="";

pf_info_quad 1000;
pf_info_space 723, 0, 0;
pf_info_familyname "OCRA";
pf_info_fixedpitch true;
pf_info_capheight 800;
pf_info_xheight 585;
pf_info_ascender 800;
pf_info_descender 200;

def ocrdraw(expr dp) =
  begingroup
    save g;
    path g;
    pen_stroke()(dp)(g);
    if cycle dp:
      dangerousFill g.r;
      dangerousFill g.l;
    else:
      dangerousFill g;
    fi;
  endgroup;
enddef;

def ocrdrawdot(expr pt) =
  dangerousFill (default_nib shifted pt);
enddef;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Much of the following material is copied essentially intact from
% Lillqvist's work; specifically, the "join", "fill_corners", and
% "angle_fill_corner" routines.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Miscellaneous stuff.
%
% mode_setup;
% letter_fit# := 1.25T#;
letter_fit# := 0;
define_whole_ps_units (W, H, L, T, Y, R, letter_fit);    % round off
theta := (2*94/55)*(W/H);	% 2-degree angle for some small letters
def adjust_fit =
    l := -letter_fit;
    interim xoffset := -l;
    charwd := charwd + 2letter_fit#;
    r := l + hround (charwd*hppp);
    w := r - letter_fit;
enddef;
extra_beginchar := extra_beginchar & "adjust_fit;";
extra_endchar := extra_endchar & "penlabels (range 1 thru 20);";


%
% Curvature radii defined in ANSI X3.17 for various characters.
%
r1# = H#/16 *     ((7H#)/(6W#) + sqrt(((7H#)/(6W#))**2 + 1));
r2# = W#/8  *     ((6W#)/(7H#) + sqrt(((6W#)/(7H#))**2 + 1));
r3# = W#/8  * (1 - H#/(2W#)    + sqrt((H#/(2W#))**2    + 1));
r4# = H#/16 * (1 - (4W#)/(3H#) + sqrt(((4W#)/(3H#))**2 + 1));
r5# = H#/16 * (1 - (5H#)/(6W#) + sqrt(((5H#)/(6W#))**2 + 1));
r6# = W#/8  * (1 - (6W#)/(5H#) + sqrt(((6W#)/(5H#))**2 + 1));
define_ps_units (r1, r2, r3, r4, r5, r6);


%
% ocrpen:
%	A circular pen of diameter T; used for virtually all characters.
%
default_nib:=fix_nib(T,T,0);
path ocrpen;
ocrpen := default_nib;


%
% "ocrchar" macro:
%	Used at the start of almost every character definition.  Note
%	that the point "z0" is the lower left-hand corner of most char-
%	acters.  The height of the bounding box is equal to the full
%	height of the character (including stroke thickness), and the
%	characters are all centered left-to-right within the box.  This
%	requires all character point definitions to be stated relative
%	to the point "z0".
%
def ocrchar (expr name,code) =
    message name;
    encode(name) (code); standard_introduce(name);
    beginglyph(name);    
    default_nib:=ocrpen;
    join_radius := T/2;
    z0 = ((R-W)/2, T/2);
enddef;

def endchar =
    fix_hsbw(R,0,0);
    endglyph;
enddef;

%
% "join" operator:
%	Joins two strokes so that the pen center curvature radius
%	is "join_radius".  ("join_radius" must already be defined.)
%
tertiarydef p join q =
    begingroup
	% "ipq_" is the point where the paths "p",112 and "q",113 would cross.
	pair ipq_;
	ipq_ := p intersectionpoint q;

	% "dirv0" is the angular direction at the end of "p",112;
	% "dirv1" is the angular direction at the start of "q",113.
	pair dirv[];
	dirv0 = dir angle direction (length p) of p;
	dirv1 = dir angle direction 0 of q;

	% If the directions of "p",112 and "q",113 are close enough,
	% just connect the paths without any fancy stuff.
	if length (dirv1 - dirv0) < 1/10:
	    p .. q

	% Otherwise, compute a joining of the required curvature.
	else:
	    path par[];
	    k_ := if ypart (dirv1 rotated -angle dirv0) > 0:
		     90 else: -90 fi;
	    forsuffixes $=0, 1:
		par$ := ((ipq_ - (20join_radius*dirv$))--
			 (ipq_ + (20join_radius*dirv$)))
			shifted ((join_radius*dirv$) rotated k_);
	    endfor;
	    numeric pt_, qt_;
	    (pt_, qt_) = par0 intersectiontimes par1;
	    if pt_ < 0:
		errmessage "No intersection curvature center found";
	    fi
	    pair icc_; icc_ = point pt_ of par0;
	    numeric cp_, cpp_, cq_, cqp_;
	    (cp_, cpp_) = p intersectiontimes
		(icc_--icc_+(2join_radius*dirv0 rotated -k_));
	    (cq_, cqp_) = q intersectiontimes
		(icc_--icc_+(2join_radius*dirv1 rotated -k_));
	    subpath (0, cp_) of p ..
		(icc_ + join_radius*unitvector(dirv0-dirv1)) ..
		subpath (cq_, length q) of q
	fi
    endgroup
enddef;


%
% "fill_corners" macro:
%	Fills in one or more corners to be sharp right-angle bends.
%
def fill_corners (text t) =
    begingroup
	forsuffixes $=t:
	    dangerousFill (unitsquare shifted (-.5, -.5) scaled T shifted z$);
	endfor
    endgroup
enddef;

%
% fill_corner_clipped:
%	Brand new macro by Matthew Skala:  do "fill_corners" on just one
%	corner, clipped to a path.  Many undocumented restrictions apply.
%	This substitutes for the "unfill" that MetaType1+Fontforge can't
%	do gracefully.
def fill_corner_clipped (suffix x)(expr y) =
  begingroup
    save csq,ta,tb;
    path csq;
    csq:=(unitsquare shifted (-.5, -.5) scaled T shifted z[x]);
    ta:=xpart (csq intersectiontimes y);
    tb:=(length csq)-xpart ((reverse csq) intersectiontimes y);
    if (tb-ta)>(ta-tb+length csq):
      dangerousFill (subpath (ta,tb) of csq)--cycle;
    else:
      dangerousFill ((subpath (0,ta) of csq)
        --(subpath (tb,length csq) of csq))&cycle;
    fi;
  endgroup;
enddef;

%
% "angle_fill_corners" macro:
%	Fills in a corner to be a sharp bend, but not a right angle.
%	The parameters are three points that define the angle, with
%	the corner specified second.
%
def angle_fill_corner (suffix a, b, c) =
    begingroup
	save p;
	path p.q, p.w, p.e, p.r;
	p.q = (z.a--z.b+(z.b-z.a))
	      shifted (T/2*unitvector (z.b-z.a) rotated 90);
	p.w = (z.a--z.b+(z.b-z.a))
	      shifted (T/2*unitvector (z.b-z.a) rotated -90);
	p.e = (z.b-(z.c-z.b)--z.c)
	      shifted (T/2*unitvector (z.c-z.b) rotated 90);
	p.r = (z.b-(z.c-z.b)--z.c)
	      shifted (T/2*unitvector (z.c-z.b) rotated -90);

	save q;
	pair q.u, q.l;
	q.u = p.q intersectionpoint p.e;
	q.l = p.w intersectionpoint p.r;

	dangerousFill reverse (point 0 of p.q -- q.u -- point 1 of p.e --
	     point 1 of p.r -- q.l -- point 0 of p.w -- cycle);
    endgroup
enddef;


%
% "to_mid" macro:
%	Defines a path starting at the first point and continuing to
%	a point between the two points (slightly past the middle).
%
def to_mid (expr a, b) =
    (a -- .6[a,b])
enddef;


%
% "from_mid" macro:
%	Defines a path starting between two points (slightly before the
%	middle) and continuing to the second point.
%
def from_mid (expr a, b) =
    (.4[a,b] -- b)
enddef;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%								       %
%			  CHARACTER DEFINITIONS			       %
%								       %
%   For debugging purposes, you may wish to remove the initial "%%%"   %
%   from the "def" and "enddef" lines below.  This will allow you to   %
%   invoke the character definitions one at a time (e.g., to inspect   %
%   them on a bit-mapped display).                                     %
%								       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% space added by MS
ocrchar ("space",32); "Space";
endchar;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Digits.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% def draw_zero =
    ocrchar ("zero",48); "Number ZERO";
	z1 = (x0, y0); z2 = (x0+W, y1);
	z3 = (x2, y0+H); z4 = (x1, y3);

	ocrdraw(from_mid (z1, z2)
	     join (z2 -- z3)
	     join to_mid (z3, z4));
	ocrdraw(from_mid (z3, z4)
	     join (z4 -- z1)
	     join to_mid (z1, z2));
    endchar;
%%% enddef;


%%% def draw_one =
    ocrchar ("one",49); "Number ONE";
	z1 = (x0, y0); z2 = (x0+W, y1); z3 = (x2, y0+3/8H);
	z4 = .5[z1,z2]; z5 = (x4, y0+H); z6 = (x1, y5);

	ocrdraw(z1 -- z2 -- z3);
	ocrdraw(z4 -- z5 -- z6);
	fill_corners (5);
    endchar;
%%% enddef;


%%% def draw_two =
    ocrchar ("two",50); "Number TWO";
	z1 = (x0+W, y0); z2 = (x0, y1); z3 = (x2, y0+1/2H);
	z4 = (x1, y3); z5 = (x1, y0+H); z6 = (x2, y5);

	ocrdraw((z1 -- z2 -- z3)
	     join (z3 -- z4)
	     join (z4 -- z5)
	     join (z5 -- z6));
	fill_corners (2);
    endchar;
%%% enddef;


%%% def draw_three =
    ocrchar ("three",51); "Number THREE";
	z1 = (x0, y0); z2 = (x0+W, y1); z3 = (x2, y0+1/2H);
	z4 = (x0+1/4W, y3); z5 = (x0+W, y0+H); z6 = (x0, y5);

	ocrdraw((z1 -- z2)
	     join to_mid (z2, z3));
	ocrdraw(from_mid (z3, z5)
	     join (z5 -- z6));
	join_radius := T;
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z4));
	ocrdraw((z4 -- z3)
	     join to_mid (z3,z5));
    endchar;
%%% enddef;


%%% def draw_four =
    ocrchar ("four",52); "Number FOUR";
	z1 = (x0+13/16W, y0); z2 = (x1, y0+7/8H);
	z3 = (x0+15/16W, y0+3/8H); z4 = (x0+1/16W, y3); z5 = (x4, y0+H);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4 -- z5);
	fill_corners (4);
    endchar;
%%% enddef;


%%% def draw_five =
    ocrchar ("five",53); "Number FIVE";
	z1 = (x0+W, y0+H); z2 = (x0+1/4W, y1); z3 = (x2, y0+1/2H);
	z4 = (x1, y3); z5 = (x1, y0);
	z6 = (x0+1/4W, y5); z7 = (x0, y0+1/16H);

	ocrdraw(z1 -- z2 -- to_mid (z3, z4));
	ocrdraw(from_mid (z3, z4)
	     join (z4 -- z5)
	     join to_mid (z5, z6));
	join_radius := T;
	ocrdraw(from_mid (z5, z6)
	     join (z6 -- z7));
	fill_corners (2, 3);
    endchar;
%%% enddef;


%%% def draw_six =
    ocrchar ("six",54); "Number SIX";
	z1 = (x0+1/8W, y0+H); z2 = (x0, y0+H); z3 = (x0, y0);
	z4 = (x0+W, y0); z5 = (x0+W, y0+3/8H); z6 = (x0, y5);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5 -- z6);
    endchar;
%%% enddef;


%%% def draw_seven =
    ocrchar ("seven",55); "Number SEVEN";
	z1 = (x0, y0+15/16H); z2 = (x0, y0+H); z3 = (x0+W, y0+H);
	z4 = (x0+W, y0+5/8H); z5 = (x0+1/2W, y0+3/8H); z6 = (x5, y0);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5 -- z6);
	fill_corners (3);
    endchar;
%%% enddef;


%%% def draw_eight =
    ocrchar ("eight",56); "Number EIGHT";
	z1 = (x0+1/4W, y0+1/2H); z2 = (x1, y0+H);
	z3 = (x0+3/4W, y0+H); z4 = (x3, y1);
	z5 = (x0, y0+1/2H); z6 = (x0+W, y5);
	z7 = (x0+W, y0); z8 = (x0, y0);

	ocrdraw(z1 -- z2 -- z3 -- z4);
	ocrdraw(from_mid (z5, z6)
	     join (z6 -- z7)
	     join to_mid (z7, z8));
	ocrdraw(from_mid (z7,z8)
	     join (z8 -- z5)
	     join to_mid (z5, z6));
    endchar;
%%% enddef;


%%% def draw_nine =
    ocrchar ("nine",57); "Number NINE";
	z1 = (x0+7/8W, y0); z2 = (x0+W, y0); z3 = (x0+W, y0+H);
	z4 = (x0, y0+H); z5 = (x0, y0+5/8H); z6 = (x0+W, y5);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5 -- z6);
    endchar;
%%% enddef;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Upper-case letters.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% def draw_a =
    ocrchar ("A",65); "Letter A";
	z1 = (x0, y0); z2 = (x0+1/2W, y0+H); z3 = (x0+W, y0);
	z4 = 1/4[z1,z2]; z5 = 1/4[z3,z2];

	ocrdraw(z1 -- z2 -- z3);
	ocrdraw(z4 -- z5);
    endchar;
%%% enddef;


%%% def draw_b =
    ocrchar ("B",66); "Letter B";
	z1 = (x0, y0); z2 = (x0+W, y0); z3 = (x0+W, y0+1/2H);
	z4 = (x0, y3); z5 = (x0+W, y0+H); z6 = (x0, y0+H);

	join_radius := 3/2T;
	ocrdraw((z1 -- z2)
	     join (z2 -- z3)
	     join (z3 -- z4));
	ocrdraw((z6 -- z5)
	     join (z5 -- z3)
	     join (z3 -- z4));
	ocrdraw(z1 -- z6);
	fill_corners (1, 6);
    endchar;
%%% enddef;


%%% def draw_c =
    ocrchar ("C",67); "Letter C";
	z1 = (x0+W, y0); z2 = (x0+3/8W, y0); z3 = (x0, y0+7/16H);
	z4 = (x1, y0+H); z5 = (x2, y0+H); z6 = (x3, y0+9/16H);

	join_radius := T;
	ocrdraw((z1 -- z2)
	     join to_mid (z2, z3));
	ocrdraw((z4 -- z5)
	     join to_mid (z5, z6));
	join_radius := r1;
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z6)
	     join to_mid (z6, z5));
    endchar;
%%% enddef;


%%% def draw_d =
    ocrchar ("D",68); "Letter D";
	z1 = (x0, y0); z2 = (x0+5/8W, y0); z3 = (x0+W, y0+7/16H);
	z4 = (x1, y0+H); z5 = (x2, y0+H); z6 = (x3, y0+9/16H);
	z7 = (x0+1/4W, y0); z8 = (x7, y0+H);

	join_radius := T;
	ocrdraw((z1 -- z2)
	     join to_mid (z2, z3));
	ocrdraw((z4 -- z5)
	     join to_mid (z5, z6));
	join_radius := r1;
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z6)
	     join to_mid (z6, z5));
	ocrdraw(z7 -- z8);
    endchar;
%%% enddef;


%%% def draw_e =
    ocrchar ("E",69); "Letter E";
	z1 = (x0+W, y0); z2 = (x0, y0);
	z3 = (x0, y0+H); z4 = (x0+W, y0+H);
	z5 = (x0, y0+1/2H); z6 = (x0+1/2W, y5);

	ocrdraw(z1 -- z2 -- z3 -- z4);
	ocrdraw(z5 -- z6);
	fill_corners (2, 3);
    endchar;
%%% enddef;

%%% def draw_f =
    ocrchar ("F",70); "Letter F";
	z1 = (x0, y0); z2 = (x0, y0+H); z3 = (x0+W, y0+H);
	z4 = (x0, y0+5/8H); z5 = (x0+3/4W, y4);

	ocrdraw(z1 -- z2 -- z3);
	ocrdraw(z4 -- z5);
	fill_corners (2);
    endchar;
%%% enddef;


%%% def draw_g =
    ocrchar ("G",71); "Letter G";
	z1 = (x0+W, y0+H); z2 = (x0+1/2W, y0+H); z3 = (x0, y0+5/8H);
	z4 = (x0, y0); z5 = (x0+W, y0);
	z6 = (x0+W, y0+3/8H); z7 = (x0+5/8W, y6);

	join_radius := T;
	ocrdraw((z1 -- z2)
	     join to_mid (z2, z3));
	ocrdraw(from_mid (z3, z4)
	     join (z4 -- z5)
	     join (z5 -- z6)
	     -- z7);
	join_radius := 3/2T;
	ocrdraw(from_mid (z2, z3)
	     join to_mid (z3, z4));
	fill_corners (6);
    endchar;
%%% enddef;


%%% def draw_h =
    ocrchar ("H",72); "Letter H";
	z1 = (x0, y0); z2 = (x0, y0+H);
	z3 = (x0+W, y0); z4 = (x0+W, y0+H);
	z5 = (x0, y0+1/2H); z6 = (x0+W, y5);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4);
	ocrdraw(z5 -- z6);
    endchar;
%%% enddef;


%%% def draw_i =
    ocrchar ("I",73); "Letter I";
	z1 = (x0+1/2W, y0); z2 = (x1, y0+H);
	z3 = (x0, y0); z4 = (x0+W, y0);
	z5 = (x0, y0+H); z6 = (x0+W, y0+H);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4);
	ocrdraw(z5 -- z6);
    endchar;
%%% enddef;


%%% def draw_j =
    ocrchar ("J",74); "Letter J";
	z1 = (x0+7/8W, y0+H); z2 = (x1, y0);
	z3 = (x0+1/8W, y0); z4 = (x3, y0+3/8H);

	join_radius := T;
	ocrdraw((z1 -- z2)
	     join (z2 -- z3)
	     join (z3 -- z4));
    endchar;
%%% enddef;


%%% def draw_k =
    ocrchar ("K",75); "Letter K";
	z1 = (x0, y0+H); z2 = (x0, y0);
	z3 = (x0+W, y0+H); z4 = (x0, y0+1/2H); z5 = (x0+W, y0);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4 -- z5);
    endchar;
%%% enddef;


%%% def draw_l =
    ocrchar ("L",76); "Letter L";
	z1 = (x0, y0+H); z2 = (x0, y0); z3 = (x0+W, y0);

	ocrdraw(z1 -- z2 -- z3);
	fill_corners (2);
    endchar;
%%% enddef;


%%% def draw_m =
    ocrchar ("M",77); "Letter M";
	z1 = (x0, y0); z2 = (x0, y0+H); z3 = (x0+1/8W, y0+H);
	z4 = (x0+7/8W, y0+H); z5 = (x0+W, y0+H); z6 = (x0+W, y0);
	z7 = (x0+1/2W, y0+11/16H); z8 = (x7, y0+5/8H);

	ocrdraw(z1 -- z2 -- z3 -- z7 -- z4 -- z5 -- z6);
	ocrdraw(z7 -- z8);
	fill_corners (2, 5);
	angle_fill_corner (2, 3, 7);
	angle_fill_corner (7, 4, 5);
    endchar;
%%% enddef;


%%% def draw_n =
    ocrchar ("N",78); "Letter N";
	z1 = (x0, y0); z2 = (x0, y0+H); z3 = (x0+1/8W, y0+H);
	z4 = (x0+7/8W, y0); z5 = (x0+W, y0); z6 = (x0+W, y0+H);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5 -- z6);
	fill_corners (2, 5);
	angle_fill_corner (2, 3, 4);
	angle_fill_corner (3, 4, 5);
    endchar;
%%% enddef;


%%% def draw_o =
    ocrchar ("O",79); "Letter O";
	z1 = (x0, y0+7/16H); z2 = (x1, y0+9/16H);
	z3 = (x0+3/8W, y0+H); z4 = (x0+5/8W, y3);
	z5 = (x0+W, y2); z6 = (x5, y1);
	z7 = (x4, y0); z8 = (x3, y7);

	join_radius := r1;
	ocrdraw(from_mid (z8, z1)
	     join (z1 -- z2)
	     join to_mid (z2, z3));
	ocrdraw(from_mid (z4, z5)
	     join (z5 -- z6)
	     join to_mid (z6, z7));
	join_radius := r2;
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z4)
	     join to_mid (z4, z5));
	ocrdraw(from_mid (z6, z7)
	     join (z7 -- z8)
	     join to_mid (z8, z1));
    endchar;
%%% enddef;


%%% def draw_p =
    ocrchar ("P",80); "Letter P";
	z1 = (x0, y0); z2 = (x0, y0+H);
	z3 = (x0+W, y2); z4 = (x3, y0+7/16H);
	z5 = (x0, y4);

	join_radius := T;
	ocrdraw(z1 -- to_mid (z2, z3));
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z4)
	     join (z4 -- z5));
	fill_corners (2);
    endchar;
%%% enddef;


%%% def draw_q =
    ocrchar ("Q",81); "Letter Q";
	z1 = (x0+1/4W, y0); z1a = (x0, y0); z2 = (x0, y0+5/8H);
	z3 = (x0+3/4W, y0+H); z3a = (x0+W, y3); z4 = (x3a, y0+3/8H);
	z5 = (x0+1/2W, y4); z6 = (x3, y0); z7 = (x4, y0);

	join_radius := 3/2T;
	ocrdraw(from_mid (z1a, z2)
	     join to_mid (z2, z3));
	ocrdraw(from_mid (z3a, z4)
	     join to_mid (z4, z1));
	join_radius := r3;
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z3a)
	     join to_mid (z3a, z4));
	ocrdraw(from_mid (z4, z1)
	     join (z1 -- z1a)
	     join to_mid (z1a, z2));
	ocrdraw(z5 -- z6 -- z7);
	angle_fill_corner (5, 6, 7);
    endchar;
%%% enddef;


%%% def draw_r =
    ocrchar ("R",82); "Letter R";
	z1 = (x0, y0); z2 = (x0, y0+H);
	z3 = (x0+W, y2); z4 = (x3, y0+5/8H); z5 = (x0, y4);
	z6 = (x0+3/8W, y4); z7 = (x3, y1);

	join_radius := T;
	ocrdraw(z1 -- to_mid (z2, z3));
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z4)
	     join (z4 -- z5));
	ocrdraw(z6 -- z7);
	fill_corners (2);
    endchar;
%%% enddef;


%%% def draw_s =
    ocrchar ("S",83); "Letter S";
	z1 = (x0, y0+1/8H); z1a = (x1, y0);
	z1b = (x0+W, y1a); z2 = (x1b, y1);
	z3 = (x1b, y0+7/8H); z3a = (x3, y0+H);
	z3b = (x0, y3a); z4 = (x3b, y3);

	join_radius := 1/8H;
	ocrdraw((z1 -- z1a)
	     join to_mid (z1a, z1b));
	ocrdraw((z3 -- z3a)
	     join to_mid (z3a, z3b));
	join_radius := r4;
	ocrdraw(from_mid (z1a, z1b)
	     join (z1b -- z2)
	     join (z2 -- z4)
	     join (z4 -- z3b)
	     join to_mid (z3b, z3a));
    endchar;
%%% enddef;


%%% def draw_t =
    ocrchar ("T",84); "Letter T";
	z1 = (x0, y0+7/8H); z2 = (x1, y0+H);
	z3 = (x0+W, y2); z4 = (x3, y1);
	z5 = (x0+1/2W, y2); z6 = (x5, y0);

	ocrdraw(z1 -- z2 -- z3 -- z4);
	ocrdraw(z5 -- z6);
	fill_corners (2, 3);
    endchar;
%%% enddef;


%%% def draw_u =
    ocrchar ("U",85); "Letter U";
	z1 = (x0, y0+H); z2 = (x0, y0);
	z3 = (x0+W, y0); z4 = (x3, y1);

	join_radius := T;
	ocrdraw((z1 -- z2)
	     join (z2 -- z3)
	     join (z3 -- z4));
    endchar;
%%% enddef;


%%% def draw_v =
    ocrchar ("V",86); "Letter V";
	z1 = (x0, y0+H); z2 = (x1, y0+3/4H); z3 = (x0+1/2W, y0);
	z4 = (x0+W, y2); z5 = (x4, y1);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5);
	angle_fill_corner (1, 2, 3);
	angle_fill_corner (3, 4, 5);
    endchar;
%%% enddef;


%%% def draw_w =
    ocrchar ("W",87); "Letter W";
	z1 = (x0, y0+H); z2 = (x0, y0+1/8H); z3 = (x0+1/8W, y0);
	z4 = (x0+3/8W, y0); z5 = (x0+1/2W, y2); z6 = (x0+5/8W, y0);
	z7 = (x0+7/8W, y0); z8 = (x0+W, y2); z9 = (x8, y1);
	z10 = (x5, y0+5/8H);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5 -- z6 -- z7 -- z8 -- z9);
	ocrdraw(z5 -- z10);
	angle_fill_corner (1, 2, 3);
	angle_fill_corner (7, 8, 9);
    endchar;
%%% enddef;


%%% def draw_x =
    ocrchar ("X",88); "Letter X";
	z1 = (x0, y0+H); z2 = (x0+W, y0);
	z3 = (x1, y2); z4 = (x2, y1);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4);
    endchar;
%%% enddef;


%%% def draw_y =
    ocrchar ("Y",89); "Letter Y";
	z1 = (x0, y0+H); z2 = (x1, y0+7/8H); z3 = (x0+1/2W, y0+1/2H);
	z4 = (x0+W, y2); z5 = (x4, y1); z6 = (x3, y0);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5);
	ocrdraw(z3 -- z6);
	angle_fill_corner (1, 2, 3);
	angle_fill_corner (3, 4, 5);
    endchar;
%%% enddef;


%%% def draw_z =
    ocrchar ("Z",90); "Letter Z";
	z1 = (x0, y0+H); z2 = (x0+W, y1);
	z3 = (x0, y0); z4 = (x2, y0);

	ocrdraw(z1 -- z2 -- z3 -- z4);
	fill_corner_clipped(3)(unitsquare
	       shifted (-1.5,-0.5) scaled T
	       rotated (angle (z2-z3) - 90) shifted z3);
	fill_corner_clipped(2)(unitsquare
	       shifted (0.5,-0.5) scaled T
	       rotated (angle (z2-z3) - 90) shifted z2);
    endchar;
%%% enddef;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ASCII punctuation symbols.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Note that there is also a new "alternate" period shape.
%
%%% def draw_period =
    ocrchar ("period",46); "Period";
	z1 = (x0+3/8W, y0+3/16H); z2 = (x0+5/8W, y1);
	z3 = (x2, y0+5/16H); z4 = (x1, y3);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- cycle);
	dangerousFill z1 -- z2 -- z3 -- z4 -- cycle;
    endchar;
%%% enddef;


%
% Note that there is also a new "alternate" comma shape.
%
%%% def draw_comma =
    ocrchar ("comma",44); "Comma";
	z1 = (x0+1/4W, y0+3/8H); z2 = (x0+3/4W, y1); z3 = (x2, y0);

	ocrdraw(z1 -- z2 -- z3);
    endchar;
%%% enddef;


%%% def draw_colon =
    ocrchar ("colon",58); "Colon";
	z1 = (x0+3/8W, y0+3/16H); z2 = (x0+5/8W, y1);
	z3 = (x2, y0+5/16H); z4 = (x1, y3);
	z5 = (x1, y1+1/2H); z6 = (x2, y2+1/2H);
	z7 = (x3, y3+1/2H); z8 = (x4, y4+1/2H);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- cycle);
	ocrdraw(z5 -- z6 -- z7 -- z8 -- cycle);
	dangerousFill z1 -- z2 -- z3 -- z4 -- cycle;
	dangerousFill z5 -- z6 -- z7 -- z8 -- cycle;
    endchar;
%%% enddef;


%%% def draw_semicolon =
    ocrchar ("semicolon",59); "Semicolon";
	z1 = (x0+1/4W, y0+3/8H); z2 = (x0+3/4W, y1); z3 = (x2, y0);
	z4 = (x1, y0+3/4H); z5 = (x0+1/2W, y4);
	z6 = (x5, y0+7/8H); z7 = (x4, y6);

	ocrdraw(z1 -- z2 -- z3);
	ocrdraw(z4 -- z5 -- z6 -- z7 -- cycle);
	dangerousFill z4 -- z5 -- z6 -- z7 -- cycle;
    endchar;
%%% enddef;


%%% def draw_equals_sign =
    ocrchar ("equal",61); "Equals Sign";
	z1 = (x0, y0+3/8H); z2 = (x0+W, y1);
	z3 = (x1, y0+11/16H); z4 = (x2, y3);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4);
    endchar;
%%% enddef;

%%% def draw_plus_sign =
    ocrchar ("plus",43); "Plus Sign";
	z1 = (x0, y0+1/2H); z2 = (x0+W, y1);
	z3 = (x0+1/2W, y1+1/2W); z4 = (x3, y3-W);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4);
    endchar;
%%% enddef;


%%% def draw_slant =
    ocrchar ("slash",47); "Slant";
	z1 = (x0, y0); z2 = (x0+W, y0+H);

	ocrdraw(z1 -- z2);
    endchar;
%%% enddef;


%%% def draw_asterisk =
    ocrchar ("asterisk",42); "Asterisk";
	z1 = (x0+1/2W, y0+1/8H); z2 = (x1, y0+7/8H);
	z3 = (x0, y0+1/4H); z4 = (x0+W, y0+3/4H);
	z5 = (x3, y4); z6 = (x4, y3);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4);
	ocrdraw(z5 -- z6);
    endchar;
%%% enddef;


%%% def draw_quotation_mark =
    ocrchar ("quotedbl",34); "Quotation Mark";
	z1 = (x0+1/4W, y0+H); z2 = (x0+1/8W, y0+5/8H);
	z3 = (x0, y1); z4 = (x0+W, y1);
	z5 = (x0+7/8W, y2); z6 = (x0+3/4W, y1);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5 -- z6);
	dangerousFill reverse (z1 -- z2 -- z3 -- cycle);
	dangerousFill reverse (z4 -- z5 -- z6 -- cycle);
	fill_corner_clipped(3)(unitsquare
	       shifted (-1.5,-0.5) scaled T
	       rotated (angle (z3-z2) - 90) shifted z3);
	fill_corner_clipped(4)(unitsquare
	       shifted (1.5,-0.5) scaled T
	       rotated (90 - angle (z6-z5)) shifted z6);
    endchar;
%%% enddef;


%%% def draw_opening_brace =
    ocrchar ("braceleft",123); "Opening Brace";
	z1 = (x0+W, y0); z2 = (x0+1/2W, y0);
	z3 = (x2, y0+1/2H); z4 = (x0, y3);
	z5 = (x2, y0+H); z6 = (x1, y5);

	ocrdraw((z1 -- z2)
	     join to_mid (z2, z3));
	ocrdraw((z6 -- z5)
	     join to_mid (z5, z3));
	join_radius := T;
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z4));
	ocrdraw(from_mid (z5, z3)
	     join (z3 -- z4));
    endchar;
%%% enddef;


%%% def draw_closing_brace =
    ocrchar ("braceright",125); "Closing Brace";
	z1 = (x0, y0); z2 = (x0+1/2W, y0);
	z3 = (x2, y0+1/2H); z4 = (x0+W, y3);
	z5 = (x2, y0+H); z6 = (x1, y5);

	ocrdraw((z1 -- z2)
	     join to_mid (z2, z3));
	ocrdraw((z6 -- z5)
	     join to_mid (z5, z3));
	join_radius := T;
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z4));
	ocrdraw(from_mid (z5, z3)
	     join (z3 -- (z4+3*left))); % MS - overlap bug correction
    endchar;
%%% enddef;


%%% def draw_percent_sign =
    ocrchar ("percent",37); "Percent Sign";
	z1 = (x0+3/4W, y0); z2 = (x0+W, y1);
	z3 = (x2, y0+1/8H); z4 = (x1, y3);
	z5 = (x0, y0+7/8H); z6 = (x0+1/4W, y5);
	z7 = (x6, y0+H); z8 = (x5, y7);
	z9 = (x0, y3); z10 = (x0+W, y5);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- cycle);
	ocrdraw(z5 -- z6 -- z7 -- z8 -- cycle);
	ocrdraw(z9 -- z10);
	dangerousFill(z1 -- z2 -- z3 -- z4 -- cycle);
	dangerousFill(z5 -- z6 -- z7 -- z8 -- cycle);
    endchar;
%%% enddef;


%
% Note that there is also a new "alternate" question mark shape.
%
%%% def draw_question_mark =
    ocrchar ("question",63); "Question Mark";
	z1 = (x0, y0+5/8H); z2 = (x0+3/4W, y0+H);
	z3 = (x0+W, y0+7/8H); z4 = (x3, y0+3/4H);
	z5 = (x0+1/4W, y0+3/8H); z6 = (x5, y0+1/4H);
	z7 = (x5, y0);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5 -- z6);
	ocrdrawdot(z7);
    endchar;
%%% enddef;


%%% def draw_ampersand =
    ocrchar ("ampersand",38); "Ampersand";
	z1 = (x0+W, y0); z2 = (x0, y0+5/8H);
	z3 = (x0, y0+7/8H); z4 = (x0+3/4W, y3);
	z5 = (x4, y2); z6 = (x0, y0+1/4H);
	z7 = (x0, y0); z8 = (x0+1/2W, y0); z9 = (x1, y6);

	join_radius := T;
	ocrdraw(z1 -- 
	  ((z2 -- z3) join (z3 -- z4) join (z4 -- z5))
	  -- ((z6 -- z7) join (z7 -- z8))
	  -- z9);
    endchar;
%%% enddef;


%
% This is a new apostrophe shape.  The older shape has been made
% into an "alternate" form.
%
%%% def draw_apostrophe =
    ocrchar ("quotesingle",39); "Apostrophe";
	z1 = (x0+3/8W, y0+H); z2 = (x1, y0+1/2H); z3 = (x0+5/8W, y1);

	ocrdraw(z1 -- z2 -- z3 -- cycle);
	dangerousFill(z1 -- z2 -- z3 -- cycle);
    endchar;
%%% enddef;


%
% This is a new hyphen shape.  The older shape has been made
% into an "alternate" form.
%
%%% def draw_hyphen =
    ocrchar ("hyphen",45); "Hyphen";
	z1 = (x0, y0+1/2H); z2 = (x0+W, y1);
	z3 = (x2, y0+7/16H); z4 = (x1, y3);

	ocrdraw((0.4[z1,z2]) -- z2 -- z3 -- z4 -- z1 -- (0.6[z1,z2]));
	dangerousFill reverse (z1 -- z2 -- z3 -- z4 -- cycle);
    endchar;
%%% enddef;


%%% def draw_dollar_sign =
    ocrchar ("dollar",36); "Dollar Sign";
	z1 = (x0, y0+1/4H); z2 = (x0+W, y1); z3 = (x2, y0+1/2H);
	z4 = (x1, y3); z5 = (x4, y0+3/4H); z6 = (x2, y5);
	z7 = (x0+1/2W, y0); z8 = .5[z1,z2];
	z9 = (x7, y0+H); z10 = .5[z5,z6];

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5 -- z6);
	ocrdraw(z7 -- z8);
	ocrdraw(z9 -- z10);
    endchar;
%%% enddef;


%%% def draw_upward_arrowhead =
    ocrchar ("circumflex",94); "Upward Arrowhead";
	z1 = (x0, y0+3/16H); z2 = (x0+1/2W, y0+5/8H);
	z3 = (x0+W, y1); z4 = (x2, y0+3/4H);
	
	begingroup % weirdness!
	  save tmppath;
	  path tmppath;
	  tmppath=z1 -- z2 -- z3 -- z4 -- cycle;
	  dangerousFill(tmppath);
	  ocrdraw(tmppath);
	endgroup;
    endchar;
%%% enddef;


%%% def draw_opening_bracket =
    ocrchar ("bracketleft",91); "Opening Bracket";
	z1 = (x0+W, y0); z2 = (x0, y0);
	z3 = (x2, y0+H); z4 = (x1, y3);
	z5 = (5/8[x1,x2], y1); z6 = (x5, y3);

	ocrdraw(z1 -- z2 -- z3 -- z4);
	ocrdraw(z5 -- z6);
    endchar;
%%% enddef;


%%% def draw_closing_bracket =
    ocrchar ("bracketright",93); "Closing Bracket";
	z1 = (x0, y0); z2 = (x0+W, y0);
	z3 = (x2, y0+H); z4 = (x1, y3);
	z5 = (5/8[x1,x2], y1); z6 = (x5, y3);

	ocrdraw(z1 -- z2 -- z3 -- z4);
	ocrdraw(z5 -- z6);
    endchar;
%%% enddef;


%%% def draw_less_than_sign =
    ocrchar ("less",60); "Less Than Sign";
	z1 = (x0+W, y0+1/8H); z2 = (x0, y0+15/32H);
	z3 = (x2, y0+17/32H); z4 = (x1, y0+7/8H);

	ocrdraw(z1 -- z2 -- z3 -- z4);
    endchar;
%%% enddef;


%%% def draw_greater_than_sign =
    ocrchar ("greater",62); "Greater Than Sign";
	z1 = (x0, y0+1/8H); z2 = (x0+W, y0+15/32H);
	z3 = (x2, y0+17/32H); z4 = (x1, y0+7/8H);

	ocrdraw(z1 -- z2 -- z3 -- z4);
    endchar;
%%% enddef;


%%% def draw_opening_parenthesis =
    ocrchar ("parenleft",40); "Opening Parenthesis";
	z1 = (x0+3/4W, y0); z2 = (x0+3/8W, y0+9/32H);
	z3 = (x2, y0+23/32H); z4 = (x1, y0+H);

	join_radius := 3/2T;
	ocrdraw((z1 -- z2)
	     join (z2 -- z3)
	     join (z3 -- z4));
    endchar;
%%% enddef;


%%% def draw_closing_parenthesis =
    ocrchar ("parenright",41); "Closing Parenthesis";
	z1 = (x0+1/4W, y0); z2 = (x0+5/8W, y0+9/32H);
	z3 = (x2, y0+23/32H); z4 = (x1, y0+H);

	join_radius := 3/2T;
	ocrdraw((z1 -- z2)
	     join (z2 -- z3)
	     join (z3 -- z4));
    endchar;
%%% enddef;


%%% def draw_exclamation_point =
    ocrchar ("exclam",33); "Exclamation Point";
	z1 = (x0+7/16W, y0); z2 = (x0+9/16W, y1);
	z3 = (x2, y0+1/16H); z4 = (x1, y3);
	z5 = (x0+1/2W, y0+7/16H); z6 = (x1, y0+H); z7 = (x2, y6);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- cycle);
	ocrdraw(z5 -- z6 -- z7 -- cycle);
	dangerousFill(z1 -- z2 -- z3 -- z4 -- cycle);
	dangerousFill(z5 -- z6 -- z7 -- cycle);
    endchar;
%%% enddef;


%%% def draw_number_sign =
    ocrchar ("numbersign",35); "Number Sign";
	z1 = (x0+1/4W, y0+3/16H); z2 = (x1, y0+H);
	z3 = (x0+3/4W, y1); z4 = (x3, y2);
	z5 = (x0, y0+7/16H); z6 = (x0+W, y5);
	z7 = (x5, y0+3/4H); z8 = (x6, y7);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4);
	ocrdraw(z5 -- z6);
	ocrdraw(z7 -- z8);
    endchar;
%%% enddef;


%%% def draw_commercial_at =
    ocrchar ("at",64); "Commercial At";
	z1 = (x0, y0+7/8H); z2 = (x0, y0+H); z3 = (x0+W, y2);
	z4 = (x3, y0); z5 = (x0+1/2W, y0); z6 = (x5, y0+9/16H);
	z7 = (x0, y6); z8 = (x0, y0);

	join_radius := 3/16H;
	ocrdraw((z1 -- z2)
	     join (z2 -- z3)
	     join to_mid (z3, z4));
	join_radius := 1/4W;
	ocrdraw(from_mid (z3, z4)
	     join (z4 -- z5)
	     join (z5 -- z6));
	ocrdraw((z6 -- z7)
	     join (z7 -- z8)
	     join (z8 -- z5)
	     join to_mid (z5, z6));
    endchar;
%%% enddef;


%%% def draw_reverse_slant =
    ocrchar ("backslash",92); "Reverse Slant";
	z1 = (x0, y0+H); z2 = (x0+W, y0);

	ocrdraw(z1 -- z2);
    endchar;
%%% enddef;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Lower-case letters.
% Note that ANSI X3.17-1977 defines these characters as part of OCR-A
% only for the smallest size (Size I).  Also, if lower-case letters are
% used, the standard requires a minimum vertical line spacing of five
% lines per inch (instead of the normal six lines per inch).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% def draw_small_a =
    ocrchar ("a",97); "Small Letter A";
	z1 = (x0+W, y0);
	z2 = (0, H) rotated theta shifted (z1);
	z3 = ((x0, y0+1/8H) -- (x0+W, y0+1/8H))
		 intersectionpoint (z1 -- z2);
	z4 = (x0+5/8W, y0);
	z5 = (x0, y0);
	z6 = (x0, y0+3/8H);
	z7 = (z6 -- (x1, y6)) intersectionpoint (z1 -- z2);
	z8 = (x0+1/4W, y0+11/16H);
	z9 = (z8 -- (x1, y8)) intersectionpoint (z1 -- z2);

	join_radius := 1/4W;
	ocrdraw(z3 -- to_mid (z4, z5));
	ocrdraw(from_mid (z4, z5)
	     join (z5 -- z6)
	     join (z6 -- z7)
	     join (z7 -- z3));
	ocrdraw((z8 -- z9)
	     join (z9 -- z1));
    endchar;
%%% enddef;


%%% def draw_small_b =
    ocrchar ("b",98); "Small Letter B";
	z1 = (x0, y0); z2 = (x1, y0+H);
	z3 = (x1, y0+3/16H); z4 = (x0+3/8W, y0);
	z5 = (x0+3/4W, y4); z6 = (x0+W, y0+1/8H);
	z7 = (x6, y0+9/16H); z8 = (x5, y0+11/16H);
	z9 = (x4, y8); z10 = (x3, y0+1/2H);

	join_radius := 1/4W;
	ocrdraw(z1 -- z2);
	ocrdraw(from_mid (z3, z4)
	     join (z4 -- z5)
	     join (z5 -- z6)
	     join (z6 -- z7)
	     join to_mid (z7, z8));
	ocrdraw(from_mid (z7, z8)
	     join (z8 -- z9)
	     join (z9 -- z10)
	     join (z10 -- z3)
	     join to_mid (z3, z4));
    endchar;
%%% enddef;


%%% def draw_small_c =
    ocrchar ("c",99); "Small Letter C";
	z1 = (x0+W, y0); z2 = (x0+3/8W, y1);
	z3 = (x0, y0+3/16H); z4 = (x3, y0+1/2H);
	z5 = (x2, y0+11/16H); z6 = (x1, y5);

	join_radius := 1/4W;
	ocrdraw((z1 -- z2)
	     join (z2 -- z3)
	     join (z3 -- z4)
	     join (z4 -- z5)
	     join (z5 -- z6));
    endchar;
%%% enddef;


%%% def draw_small_d =
    ocrchar ("d",100); "Small Letter D";
	z1 = (x0+W, y0); z2 = (x1, y0+H);
	z3 = (x2, y0+3/16H); z4 = (x0+5/8W, y0);
	z5 = (x0+1/4W, y4); z6 = (x0, y0+1/8H);
	z7 = (x6, y0+9/16H); z8 = (x5, y0+11/16H);
	z9 = (x4, y8); z10 = (x3, y0+1/2H);

	join_radius := 1/4W;
	ocrdraw(z1 -- z2);
	ocrdraw(from_mid (z3, z4)
	     join (z4 -- z5)
	     join (z5 -- z6)
	     join (z6 -- z7)
	     join to_mid (z7, z8));
	ocrdraw(from_mid (z7, z8)
	     join (z8 -- z9)
	     join (z9 -- z10)
	     join (z10 -- z3)
	     join to_mid (z3, z4));
    endchar;
%%% enddef;


%%% def draw_small_e =
    ocrchar ("e",101); "Small Letter E";
	z1 = (x0+W, y0); z2 = (x0+1/4W, y1);
	z3 = (x0, y0+1/8H); z4 = (x3, y0+9/16H);
	z5 = (x2, y0+11/16H); z6 = (x0+3/4W, y5);
	z7 = (x1, y4); z8 = (x7, y0+5/16H); z9 = (x3, y8);

	join_radius := 1/4W;
	ocrdraw((z1 -- z2)
	     join (z2 -- z3)
	     join (z3 -- z4)
	     join (z4 -- z5)
	     join (z5 -- z6)
	     join (z6 -- z7)
	     join (z7 -- z8)
	     -- z9);
    endchar;
%%% enddef;


%%% def draw_small_f =
    ocrchar ("f",102); "Small Letter F";
	z1 = (x0+3/8W-1/4T, y0);
	z2 = (x1, y0+H); z3 = (x0+W-1/4T, y2);
	z4 = (x0+1/8W-1/4T, y0+11/16H);
	z5 = (x0+3/4W-1/4T, y4);

	join_radius := .32W;
	ocrdraw((z1 -- z2)
	     join (z2 -- z3));
	ocrdraw(z4 -- z5);
    endchar;
%%% enddef;


%%% def draw_small_g =
    ocrchar ("g",103); "Small Letter G";
	z1 = (x0+W, y8); z2 = (x1, y0-3/16H);
	z2a = (x0+3/4W, y0-5/16H); z2b = (x0+1/8W, y2a);
	z3 = (x2, y0+1/4H); z4 = (x0+5/8W, y0+1/16H);
	z5 = (x0+1/4W, y4); z6 = (x0, y0+3/16H);
	z7 = (x6, y0+9/16H); z8 = (x5, y0+11/16H);
	z9 = (x4, y8); z10 = (x3, y0+1/2H);

	join_radius := 1/4W;
	ocrdraw((z1 -- z2)
	     join (z2 -- z2a)
	     join (z2a -- z2b));
	ocrdraw(from_mid (z3, z4)
	     join (z4 -- z5)
	     join (z5 -- z6)
	     join (z6 -- z7)
	     join to_mid (z7, z8));
	ocrdraw(from_mid (z7, z8)
	     join (z8 -- z9)
	     join (z9 -- z10)
	     join (z10 -- z3)
	     join to_mid (z3, z4));
    endchar;
%%% enddef;


%%% def draw_small_h =
    ocrchar ("h",104); "Small Letter H";
	z1 = (x0+W, y0);
	z2 = (0, H) rotated theta shifted (z1);
	z3 = (x0, y0); z4 = (x0, y0+H);
	z5 = (x3, y0+1/2H); z6 = (x0+1/2W, y0+11/16H);
	z7 = (z6 -- (x1, y6)) intersectionpoint (z1 -- z2);

	join_radius := 1/4W;
	ocrdraw(z3 -- z4);
	ocrdraw((z5 -- z6)
	     join (z6 -- z7)
	     join (z7 -- z1));
    endchar;
%%% enddef;


%%% def draw_small_i =
    ocrchar ("i",105); "Small Letter I";
	z1 = (x0+1/2W, y0); z2 = (x1, y0+11/16H); z3 = (x0+1/8W, y2);
	z4 = (x3, y0); z5 = (x0+7/8W, y4);
	z6 = (x1, y0+H); z7 = (x6, y0+17/16H);
	z8 = (x0+3/8W, y7); z9 = (x8, y6);

	ocrdraw(z1 -- z2 -- z3);
	ocrdraw(z4 -- z5);
	ocrdraw(z6 -- z7 -- z8 -- z9 -- cycle);
	dangerousFill(z6 -- z7 -- z8 -- z9 -- cycle);
    endchar;
%%% enddef;


%%% def draw_small_j =
    ocrchar ("j",106); "Small Letter J";
	z1 = (x0+3/8W, y0+11/16H); z2 = (x0+7/8W, y1);
	z3 = (x2, y0-5/16H); z4 = (x0+1/8W, y3); z5 = (x4, y0-3/16H);
	z6 = (x2, y0+H); z7 = (x6, y0+17/16H);
	z8 = (x0+3/4W, y7); z9 = (x8, y6);

	join_radius := 1/4W;
	ocrdraw((z5 -- z4)
	     join (z4 -- z3)
	     join (z3 -- z2)
	     -- z1);
	ocrdraw(z6 -- z7 -- z8 -- z9 -- cycle);
	dangerousFill(z6 -- z7 -- z8 -- z9 -- cycle);
    endchar;
%%% enddef;


%%% def draw_small_k =
    ocrchar ("k",107); "Small Letter K";
	z1 = (x0, y0); z2 = (x1, y0+H);
	z3 = (x1, y0+1/4H); z4 = (x0+7/8W, y0+11/16H);
	z5 = (x0+1/4W, y0+3/8H); z6 = (x0+W, y0);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4);
	ocrdraw(z5 -- z6);
    endchar;
%%% enddef;


%%% def draw_small_l =
    ocrchar ("l",108); "Small Letter L";
	z1 = (x0+1/2W, y0); z2 = (x1, y0+H); z3 = (x0+1/8W, y2);
	z4 = (x3, y0); z5 = (x0+7/8W, y4);

	ocrdraw(z1 -- z2 -- z3);
	ocrdraw(z4 -- z5);
    endchar;
%%% enddef;


%%% def draw_small_m =
    ocrchar ("m",109); "Small Letter M";
	z1 = (x0-.02W, y0+11/16H); z2 = (x1, y0);
	z3 = (x1, y0+.55H); z4 = (x0+1/4W, y1);
	z5 = (x0+1/2W, y1); z6 = (x5, y0);
	z7 = (x5, y0+9/16H); z8 = (x0+3/4W, y1);
	z10 = (x0+1.02W, y0);
	z10a = (0, H) rotated theta shifted (z10);
	z9 = (z8 -- (x10, y8)) intersectionpoint (z10 -- z10a);
	z9b = (z7 -- (x10, y7)) intersectionpoint (z10 -- z10a);

	join_radius := 0.181W;
	ocrdraw(z1 -- z2);
	ocrdraw((z3 -- z4)
	     join (z4 -- z5)
	     join (z5 -- z7)
	     -- z6);
	join_radius := 0.176W;
	ocrdraw((z7 -- z8)
	     join (z8 -- z9)
	     join (z9 -- z9b)
	     -- z10);
    endchar;
%%% enddef;


%%% def draw_small_n =
    ocrchar ("n",110); "Small Letter N";
	z1 = (x0+W, y0);
	z2 = (0, H) rotated theta shifted (z1);
	z3 = (x0, y0); z4 = (x0, y6);
	z5 = (x3, y0+1/2H); z6 = (x0+1/2W, y0+11/16H);
	z7 = (z6 -- (x1, y6)) intersectionpoint (z1 -- z2);

	join_radius := 1/4W;
	ocrdraw(z3 -- z4);
	ocrdraw((z5 -- z6)
	     join (z6 -- z7)
	     join (z7 -- z1));
    endchar;
%%% enddef;


%%% def draw_small_o =
    ocrchar ("o",111); "Small Letter O";
	z1 = (x0, y0+1/8H); z2 = (x0+1/4W, y0);
	z3 = (x0+3/4W, y2); z4 = (x0+W, y1);
	z5 = (x4, y0+9/16H); z6 = (x3, y0+11/16H);
	z7 = (x2, y6); z8 = (x1, y5);

	join_radius := 1/4W;
	ocrdraw(from_mid (z8, z1)
	     join (z1 -- z2)
	     join to_mid (z2, z3));
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z4)
	     join to_mid (z4, z5));
	ocrdraw(from_mid (z4, z5)
	     join (z5 -- z6)
	     join to_mid (z6, z7));
	ocrdraw(from_mid (z6, z7)
	     join (z7 -- z8)
	     join to_mid (z8, z1));
    endchar;
%%% enddef;


%%% def draw_small_p =
    ocrchar ("p",112); "Small Letter P";
	z1 = (x0, y0+23/32H); z2 = (x1, y0-5/16H);
	z3 = (x1, y0+3/16H); z4 = (x0+3/8W, y0-1/32H);
	z5 = (x0+5/8W, y4); z6 = (x0+W, y0+.157H);
	z7 = (x6, y0+.529H); z8 = (x5, y0+23/32H);
	z9 = (x4, y8); z10 = (x3, y0+1/2H);

	join_radius := 1/4W;
	ocrdraw(z1 -- z2);
	ocrdraw(from_mid (z3, z4)
	     join (z4 -- z5)
	     join (z5 -- z6)
	     join (z6 -- z7)
	     join to_mid (z7, z8));
	ocrdraw(from_mid (z7, z8)
	     join (z8 -- z9)
	     join (z9 -- z10)
	     join (z10 -- z3)
	     join to_mid (z3, z4));
    endchar;
%%% enddef;


%%% def draw_small_q =
    ocrchar ("q",113); "Small Letter Q";
	z1 = (x0+W, y0+11/16H); z2 = (x1, y0-5/16H);
	z3 = (x2, y0+3/16H); z4 = (x0+5/8W, y0);
	z5 = (x0+1/4W, y4); z6 = (x0, y0+1/8H);
	z7 = (x6, y0+9/16H); z8 = (x5, y0+11/16H);
	z9 = (x4, y8); z10 = (x3, y0+1/2H);

	join_radius := 1/4W;
	ocrdraw(z1 -- z2);
	ocrdraw(from_mid (z3, z4)
	     join (z4 -- z5)
	     join (z5 -- z6)
	     join (z6 -- z7)
	     join to_mid (z7, z8));
	ocrdraw(from_mid (z7, z8)
	     join (z8 -- z9)
	     join (z9 -- z10)
	     join (z10 -- z3)
	     join to_mid (z3, z4));
    endchar;
%%% enddef;


%%% def draw_small_r =
    ocrchar ("r",114); "Small Letter R";
	z1 = (x0, y0+11/16H); z2 = (x1, y0);
	z3 = (x1, y0+7/16H); z4 = (x0+1/2W, y1);
	z5 = (x0+W, y4); z6 = (x5, y0+1/2H);

	join_radius := 1/4W;
	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4);
	ocrdraw((z6 -- z5)
	     join (z5 -- z4));
    endchar;
%%% enddef;


%%% def draw_small_s =
    ocrchar ("s",115); "Small Letter S";
	z1 = (x0, y0+1/16H); z1a = (x0+.07W, y0+.03H);
	z2 = (x0+.20W, y0); z3 = (x0+W, y2); z4 = (x3, y0+1/4H);
	z5 = (x0+.027W, y0+1/2H); z6 = (x5, y0+11/16H);
	z6a = (x0+1/2W, y6); z7 = (x0+.92W, y6);
	z8 = (x0+.973W, y0+5/8H);

	join_radius := .32W;
	ocrdraw((z1 -- z1a)
	     join (z1a -- z2)
	     join to_mid (z2, z3));
	join_radius := .26W;
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z4)
	     join to_mid (z4, z5));
	join_radius := .20W;
	ocrdraw(from_mid (z4, z5)
	     join (z5 -- z6)
	     join (z6 -- z6a));
	join_radius := .25W;
	ocrdraw((z8 -- z7)
	     join (z7 -- z6a));
    endchar;
%%% enddef;


%%% def draw_small_t =
    ocrchar ("t",116); "Small Letter T";
	z1 = (x0+W, y0+1/8H); z2 = (x1, y0);
	z3 = (x0+1/4W, y2); z4 = (x3, y0+15/16H);
	z5 = (x0, y0+11/16H); z6 = (x0+7/8W, y5);

	join_radius := 1/4W;
	ocrdraw((z1 -- z2)
	     join (z2 -- z3)
	     join (z3 -- z4));
	ocrdraw(z5 -- z6);
    endchar;
%%% enddef;


%%% def draw_small_u =
    ocrchar ("u",117); "Small Letter U";
	z1 = (x0, y0+11/16H);
	z2 = (0, -H) rotated theta shifted (z1);
	z3 = (x0+W, y1); z4 = (x3, y0);
	z5 = (x3, y0+3/16H); z6 = (x0+1/2W, y0);
	z7 = (z6 -- (x1, y6)) intersectionpoint (z1 -- z2);

	join_radius := 1/4W;
	ocrdraw(z3 -- z4);
	ocrdraw((z5 -- z6)
	     join (z6 -- z7)
	     join (z7 -- z1));
    endchar;
%%% enddef;


%%% def draw_small_v =
    ocrchar ("v",118); "Small Letter V";
	z1 = (x0, y0+11/16H); z2 = (x1, y0+9/16H);
	z3 = (x0+.421W, y0); z4 = (x0+.579W, y0);
	z5 = (x0+W, y2); z6 = (x5, y1);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5 -- z6);
    endchar;
%%% enddef;


%%% def draw_small_w =
    ocrchar ("w",119); "Small Letter W";
	z1 = (x0, y0+1/4H); z2 = (x0+1/8W, y0); z3 = (x0+1/4W, y0);
	z4 = (x0+1/2W, y1); z5 = (x4, y0+7/16H);
	z6 = (x0+3/4W, y0); z7 = (x0+7/8W, y0); z8 = (x0+W, y1);
	z9a = (0, H) rotated theta shifted (z1);
	z9b = (0, H) rotated -theta shifted (z8);
	z10a = (x0-1/4W, y0+11/16H); z10b = (x0+5/4W, y10a);
	z11a = (z10a -- z10b) intersectionpoint (z1 -- z9a);
	z11b = (z10a -- z10b) intersectionpoint (z8 -- z9b);

	ocrdraw(z11a -- z1 -- z2 -- z3 -- z4 -- z6 -- z7 -- z8 -- z11b);
	ocrdraw(z4 -- z5);
    endchar;
%%% enddef;


%%% def draw_small_x =
    ocrchar ("x",120); "Small Letter X";
	z1 = (x0, y0); z2 = (x0+.973W, y0+11/16H);
	z3 = (x0+W, y0); z4 = (x0+.027W, y0+11/16H);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4);
    endchar;
%%% enddef;


%%% def draw_small_y =
    ocrchar ("y",121); "Small Letter Y";
	z1 = (x0, y0+11/16H); z2 = (x1, y0+9/16H);
	z3 = (x0+3/8W, y0+1/16H); z4 = (x0+5/8W, y3);
	z5 = (x0+W, y1); z6 = (x5, y2);
	z8 = (x0, y0-5/16H);
	z7 = (z6 -- z4+(z4-z6))
		 intersectionpoint (z8 -- (x8+W, y8));

	ocrdraw(z1 -- z2 -- z3 -- z4);
	ocrdraw(z5 -- z6 -- z7 -- z8);
    endchar;
%%% enddef;


%%% def draw_small_z =
    ocrchar ("z",122); "Small Letter Z";
	z1 = (x0+.061W, y0+11/16H); z2 = (x0+W, y1);
	z3 = (x2, y0+5/8H); z4 = (x0, y0+1/16H);
	z5 = (x0, y0); z6 = (x2, y5);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5 -- z6);
    endchar;
%%% enddef;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Alternate symbol forms.
% These are all new shapes, except for Apostrophe and Hyphen, where
% the "alternate" shapes are the older versions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%  NOTE from MS: For purposes of MetaType1's 8-bit output, these are
%  being encoded into their ASCII positions +128.  The hope is that
%  later preprocessing can give them better Unicode points.
%

%
% Alternate period was introduced because the standard character causes
% paper punch-through on some printers.
%
%%% def draw_period_alt =
    ocrchar ("period.alt",142); "Period (Alternate)";
	z1 = (x0+5/16W, y0); z2 = (x0+11/16W, y1);
	z3 = (x2, y0+3/8W); z4 = (x1, y3);
	% NOTE:  The use of "W",87 in the Y-coordinate of "z3"
	% is as given in the spec, to make a square period.

	ocrdraw(z1 -- z2 -- z3 -- z4 -- cycle);
	dangerousFill(z1 -- z2 -- z3 -- z4 -- cycle);
    endchar;
%%% enddef;


%
% Alternate question mark was introduced because the small dot at the
% bottom of the standard character is difficult or impossible to form
% on some embossed plates/cards, and also causes paper punch-through
% on some printers.
%
%%% def draw_question_mark_alt =
    ocrchar ("question.alt",159); "Question Mark (Alternate)";
	z1 = (x0, y0+5/8H); z2 = (x0+3/4W, y0+H);
	z3 = (x0+W, y0+7/8H); z4 = (x3, y0+3/4H);
	z5 = (x0+1/4W, y0+3/8H); z6 = (x5, y0);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5 -- z6);
    endchar;
%%% enddef;


%
% A new apostrophe was introduced (and the older character changed to
% alternate status) because the older character causes paper punch-
% through on some printers and cannot be properly recognized by some
% readers which do not store vertical-position data.
%
%%% def draw_apostrophe_alt =
    ocrchar ("quotesingle.alt",167); "Apostrophe (Alternate)";
	z1 = (x0+3/8W, y0+H); z2 = (x0+1/2W, y0+5/8H);
	z3 = (x0+5/8W, y1);

	ocrdraw(z1 -- z2 -- z3 -- cycle);
	dangerousFill(z1 -- z2 -- z3 -- cycle);
    endchar;
%%% enddef;


%
% Alternate comma was introduced because the standard character causes
% paper punch-through on some printers.
%
%%% def draw_comma_alt =
    ocrchar ("comma.alt",172); "Comma (Alternate)";
	z1 = (x0+1/4W, y0+5/16H); z2 = (x1, y0+3/8H);
	z3 = (x0+3/4W, y2); z4 = (x3, y0); z5 = (x0+5/8W, y4);
	z6 = (x5, y1);

	ocrdraw((0.4[z6,z1]) -- z1 -- z2 -- z3 -- z4 -- z5 -- z6
	  -- (0.6[z6,z1]));
	dangerousFill reverse (z1 -- z2 -- z3 -- z4 -- z5 -- z6 -- cycle);
    endchar;
%%% enddef;


%
% A new hyphen was introduced (and the older character changed to
% alternate status) because the older character causes paper punch-
% through on some printers.
%
%%% def draw_hyphen_alt =
    ocrchar ("hyphen.alt",173); "Hyphen (Alternate)";
	z1 = (x0, y0+1/2H); z2 = (x0+W, y1);

	ocrdraw(z1 -- z2);
    endchar;
%%% enddef;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Special OCR characters.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% NOTE from MS: for MetaType1 purposes these are encoded arbitrarily
% starting from 0xB0 (high partners of ASCII digits)
%


%
% The "hook" symbol (looks sort of like a squared-off S) is defined in
% ANSI X3.17-1977 to go into the "tilde" spot in the ASCII table.
%
%%% def draw_symbol_hook =
    ocrchar ("uni2440",176); "Symbol HOOK";
	z1 = (x0, y0+3/8H); z2 = (x0, y0); z3 = (x0+1/2W, y0);
	z4 = (x3, y0+H); z5 = (x0+W, y0+H); z6 = (x0+W, y0+5/8H);

	ocrdraw(z1 -- z2 -- z3 -- z4 -- z5 -- z6);
	fill_corners (2, 3, 4, 5);
    endchar;
%%% enddef;


%
% The "chair" symbol (looks sort of like a squared-off, backwards lower-
% case H) is defined in ANSI X3.17-1977 to go into the "grave accent"
% spot in the ASCII table.
%
%%% def draw_symbol_chair =
    ocrchar ("uni2441",177); "Symbol CHAIR";
	z1 = (x0+W, y0+H); z2 = (x1, y0);
	z3 = .5[z1,z2]; z4 = (x0, y3); z5 = (x0, y0);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4 -- z5);
	fill_corners (4);
    endchar;
%%% enddef;


%
% The "fork" symbol (looks sort of like a squared-off Y) is defined in
% ANSI X3.17-1977 to go into the "underline" spot in the ASCII table.
% However, a note in the standard says that this assignment should not
% preclude use of the Underline in applications not needing the "fork".
%
%%% def draw_symbol_fork =
    ocrchar ("uni2442",178); "Symbol FORK";
	z1 = (x0, y0+H); z2 = (x0, y0+1/2H);
	z3 = (x0+W, y2); z4 = (x0+W, y0+H);
	z5 = (x0+1/2W, y2); z6 = (x5, y0);

	ocrdraw(z1 -- z2 -- z3 -- z4);
	ocrdraw(z5 -- z6);
	fill_corners (2, 3);
    endchar;
%%% enddef;


% NOTE from MS:  Unicode doesn't seem to have a code for this specific to
% OCR so I am encoding it to the Unicode vertical line drawing line, and with
% the MetaType1 number 251, making it '|' + 0x80.

%
% The "Long Vertical Mark" is not really suitable as a representation
% of the ASCII "vertical bar" character.  The "Long Vertical Mark" is
% intended as a field separator and is significantly taller than other
% characters in the font.
%
%%% def draw_long_vertical_mark =
    ocrchar ("uni2502",251); "Long Vertical Mark";
	z1 = (x0+1/2W, y0+L-T/2); z2 = (x1, y0-Y+T/2);

	ocrdraw(z1 -- z2);
	fill_corners (1, 2);
    endchar;
%%% enddef;


% NOTE from MS: as above, this corresponds to a character not specifically
% encoded for OCR purposes in Unicode.  Choosing "black block" as best
% match, and 127 (ASCII "delete").

%
% "Character Erase" (a large rectangular black box) is defined in
% ANSI X3.17-1977 only for the smallest character size (Size I).
%
%%% def draw_character_erase =
    ocrchar ("uni25A0",127); "Character Erase";
	z1 = (x0-3/4T, y0-3/4T); z2 = (x0+W+3/4T, y1);
	z3 = (x2, y0+H+3/4T); z4 = (x1, y3);

	dangerousFill(z1 -- z2 -- z3 -- z4 -- cycle);
    endchar;
%%% enddef;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Miscellaneous symbols.
% These are not an official part of ANSI OCR-A, but were included in an
% appendix to X3.17-1977.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% def draw_aa =
    ocrchar ("Aring",197); "Letter \AA";
	z1 = (x0, y0); z2 = (x0+3/8W, y0+3/4H);
	z3 = (x0+5/8W, y2); z4 = (x0+W, y1);
	z5 = (x0+1/8W, y2); z6 = (x0+7/8W, y2);
	z7 = (x6, y0+H); z8 = (x5, y7);
	z9 = (x5, y0+1/4H); z10 = (x6, y9);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4);
	ocrdraw(z9 -- z10);
	ocrdraw(from_mid (z5, z6)
	     join (z6 -- z7)
	     join to_mid (z7, z8));
	ocrdraw(from_mid (z7, z8)
	     join (z8 -- z5)
	     join to_mid (z5, z6));
    endchar;
%%% enddef;


%%% def draw_a_umlaut =
    ocrchar ("Adieresis",196); "Letter "&ditto&"A";
	z1 = (x0, y0); z2 = (x0+1/2W, y0+3/4H); z3 = (x0+W, y0);
	z4 = (x0+1/8W, y0+3/16H); z5 = (x0+7/8W, y4);
	z6 = (x1, y0+H); z7 = (x6, y0+7/8H);
	z8 = (x3, y6); z9 = (x8, y7);

	ocrdraw(z1 -- z2 -- z3);
	ocrdraw(z4 -- z5);
	ocrdraw(z6 -- z7);
	ocrdraw(z8 -- z9);
    endchar;
%%% enddef;


%%% def draw_ae =
    ocrchar ("AE",198); "Letter \AE";
	z1 = (x0, y0); z2 = (x0+5/8W, y0+H); z3 = (x0+W, y2);
	z4 = (x2, y0+1/2H); z5 = (x3, y4);
	z6 = (x2, y1); z7 = (x3, y1);
	z8 = 1/4[z1,z2]; z9 = 1/4[z6,z2];

	ocrdraw(z1 -- z2 -- z3);
	ocrdraw(z2 -- z6 -- z7);
	ocrdraw(z4 -- z5);
	ocrdraw(z8 -- z9);
    endchar;
%%% enddef;


%%% def draw_n_tilde =
    ocrchar ("Ntilde",209); "Letter \~N";
	z1 = (x0, y0); z2 = (x1, y0+5/8H);
	z3 = (x0+W, y1); z4 = (x3, y2);
	z5 = (x1, y0+15/16H); z6 = (x5, y0+H);
	z7 = (x3, y5); z8 = (x3, y6);

	ocrdraw(z1 -- z2 -- z3 -- z4);
	ocrdraw(z5 -- z6 -- z7 -- z8);
    endchar;
%%% enddef;


%%% def draw_o_slant =
    ocrchar ("Oslash",216); "Letter \O";
	z1 = (x0, y0+7/16H); z2 = (x1, y0+9/16H);
	z3 = (x0+3/8W, y0+H); z4 = (x0+5/8W, y3);
	z5 = (x0+W, y2); z6 = (x5, y1);
	z7 = (x4, y0); z8 = (x3, y7);
	z9 = (x0, y0); z10 = (x0+W, y0+H);

	join_radius := r1;
	ocrdraw(from_mid (z8, z1)
	     join (z1 -- z2)
	     join to_mid (z2, z3));
	ocrdraw(from_mid (z4, z5)
	     join (z5 -- z6)
	     join to_mid (z6, z7));
	join_radius := r2;
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z4)
	     join to_mid (z4, z5));
	ocrdraw(from_mid (z6, z7)
	     join (z7 -- z8)
	     join to_mid (z8, z1));
	ocrdraw(z9 -- z10);
    endchar;
%%% enddef;


%%% def draw_o_umlaut =
    ocrchar ("Odieresis",214); "Letter \"&ditto&"O";
	z1 = (x0, y0+5/16H); z2 = (x1, y0+7/16H);
	z3 = (x0+3/8W, y0+3/4H); z4 = (x0+5/8W, y3);
	z5 = (x0+W, y2); z6 = (x5, y1);
	z7 = (x4, y0); z8 = (x3, y7);
	z9 = (x1, y0+H); z10 = (x1, y0+7/8H);
	z11 = (x5, y9); z12 = (x5, y10);

	join_radius := r5;
	ocrdraw(from_mid (z8, z1)
	     join (z1 -- z2)
	     join to_mid (z2, z3));
	ocrdraw(from_mid (z4, z5)
	     join (z5 -- z6)
	     join to_mid (z6, z7));
	join_radius := r6;
	ocrdraw(from_mid (z2, z3)
	     join (z3 -- z4)
	     join to_mid (z4, z5));
	ocrdraw(from_mid (z6, z7)
	     join (z7 -- z8)
	     join to_mid (z8, z1));
	ocrdraw(z9 -- z10);
	ocrdraw(z11 -- z12);
    endchar;
%%% enddef;


%%% def draw_u_umlaut =
    ocrchar ("Udieresis",220); "Letter \"&ditto&"U";
	z1 = (x0, y0+3/4H); z2 = (x0, y0);
	z3 = (x0+W, y0); z4 = (x3, y1);
	z5 = (x0+1/4W, y0+H); z6 = (x5, y0+7/8H);
	z7 = (x0+3/4W, y5); z8 = (x7, y6);

	join_radius := T;
	ocrdraw((z1 -- z2)
	     join (z2 -- z3)
	     join (z3 -- z4));
	ocrdraw(z5 -- z6);
	ocrdraw(z7 -- z8);
    endchar;
%%% enddef;


%%% def draw_pound_sign =
    ocrchar ("sterling",163); "Pound Sign";
	z1 = (x0, y0); z2 = (x0+W, y1);
	z3 = (x0+1/4W, y1); z4 = (x0+3/4W, y0+H); z5 = (x2, y4);
	z6 = (x1, y0+1/2H); z7 = (x2, y6);

	join_radius := T;
	ocrdraw(z1 -- z2);
	ocrdraw((z3 -- z4)
	     join (z4 -- z5));
	ocrdraw(z6 -- z7);
    endchar;
%%% enddef;


%%% def draw_yen_sign =
    ocrchar ("yen",165); "Yen Sign";
	z1 = (x0, y0+H); z2 = (x0+1/2W, y0+1/2H); z3 = (x0+W, y1);
	z4 = (x2, y0); z5 = (x1, y0+3/8H); z6 = (x3, y5);

	ocrdraw(z1 -- z2 -- z3);
	ocrdraw(z2 -- z4);
	ocrdraw(z5 -- z6);
    endchar;
%%% enddef;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Non-standard ASCII characters.
% I designed these characters myself in order to make a complete ASCII
% character set.  Note that these characters are *NOT* part of ANSI
% X3.17-1977 or any other OCR-A standard, and will almost certainly
% *NOT* be understood by any OCR-A scanner currently in existence.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% def draw_tilde =
    ocrchar ("tilde",126); "Tilde";
	z1 = (x0, y0+11/16H); z2 = (x1, y0+H);
	z3 = (x0+1/2W, y2); z4 = (x3, y0+5/8H);
	z5 = (x0+W, y4); z6 = (x5, y0+15/16H);

	join_radius := T;
	ocrdraw((z1 -- z2)
	     join (z2 -- z3)
	     join to_mid (z3, z4));
	ocrdraw((z6 -- z5)
	     join (z5 -- z4)
	     join to_mid (z4, z3));
    endchar;
%%% enddef;


%%% def draw_underline =
    ocrchar ("underscore",95); "Underline";
	z1 = (x0, y0-T); z2 = (x0+W, y1);

	ocrdraw(z1 -- z2);
    endchar;
%%% enddef;


%%% def draw_grave_accent =
    ocrchar ("grave",96); "Grave Accent";
	z1 = (x0+1/8W, y0+H); z2 = (x0+7/8W, y0+5/8H);
	z3 = (x0+3/8W, y1);

	ocrdraw(z1 -- z2 -- z3 -- cycle);
	dangerousFill(z1 -- z2 -- z3 -- cycle);
    endchar;
%%% enddef;


%%% def draw_vertical_bar =
    ocrchar ("bar",124); "Vertical Bar";
	z1 = (x0+1/2W, y0+H); z2 = (x1, y0+1/2H+T);
	z3 = (x1, y0+1/2H-T); z4 = (x1, y0);

	ocrdraw(z1 -- z2);
	ocrdraw(z3 -- z4);
	fill_corners (2, 3);
    endchar;
%%% enddef;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%								       %
%			    Font parameters			       %
%								       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


designsize := L#;
font_slant := 0;
font_normal_space := R#;
font_normal_stretch := 0;	% mustn't be more than .8R#, per spec
font_normal_shrink := 0;	% mustn't be more than .1R#, per spec
font_x_height := H#;
font_quad := 2W#;
font_extra_space := 0;

end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%								       %
%		   END OF OCR-A METAFONT DEFINITION		       %
%								       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
