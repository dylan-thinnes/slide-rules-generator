%
% Tsukurimashou "font base" macros
%
% THIS FILE IS PUBLIC DOMAIN NOTWITHSTANDING THE COPYRIGHT ON THE
% OVERALL TSUKURIMASHOU PACKAGE
%
% This file is based on the files "fontbase.mp" and "plain_ex.mp" from the
% METATYPE1 package version 0.55.  Those files contain no copyright-related
% notices of their own, but the README for METATYPE1 version 0.55 contains
% the following notices (in English and Polish; the slashes are verbatim
% from the original and presumably are some convention for expressing
% non-ASCII Polish letters in the ASCII file):
%
%     This is METATYPE1 package -- a tool for creating Type 1 fonts using
%     METAPOST. The package belongs to public domain (no copyrights,
%     copylefts, copyups, copydowns, etc.).
%     Version: 0.55 (16.09.2009; a tentative version, released along with
%              the sources of the Latin Modern fonts ver. 2.003)
%     Author: JNS team <JNSteam@gust.org.pl>
%
%     To jest pakiet METATYPE1 -- narz/edzie do tworzenia font/ow Type 1
%     za pomoc/a systemu METAPOST. Pakiet stanowi dobro wsp/olne
%     (/zadnych copyright/ow, copyleft/ow, copyup/ow, copydown/ow, etc.).
%     Wersja: 0.55 (16.09.2009 -- wersja opublikowana wraz z wersj/a
%             /xr/od/low/a 2.003 pakietu font/ow Latin Modern)
%     Autorstwo: JNS team <JNSteam@gust.org.pl>
%
% Although I assert my general right to claim copyright on work of my own
% that draws from public domain source materials, I nonetheless am releasing
% this file to the public domain in an effort to maintain the spirit of the
% JNS team's release above.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%
% Matthew Skala
% mskala@ansuz.sooke.bc.ca
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% GENERAL LIBRARY FUNCTIONS

% inclusion lock written explicitly so as not to depend on preintro.mp
if known already_included.fntbase:
  endinput;
fi;
boolean already_included.fntbase;
already_included.fntbase:=true;

% gobble a text argument
def killtext text t = enddef; % absent from older versions of plain.mf

% Knuthian tradition unit definitions
mm#=2.84528; pt#=1; dd#=1.07001; bp#=1.00375; cm#=28.45276; pc#=12;
cc#=12.84010; in#=72.27;

% numeric functions
vardef tand primary a = sind(a)/cosd(a) enddef;
vardef cotd primary a = cosd(a)/sind(a) enddef;
vardef signum primary x = if x>0: 1 elseif x<0: -1 else: 0 fi enddef;
primarydef w dotnorm z =
 begingroup
  save w_, z_, lw_, lz_; pair w_, z_;
  lw_=abs(w); w_:=w if lw_>0: /lw_ fi;
  lz_=abs(z); z_:=z if lz_>0: /lz_ fi;
  (xpart w_ * xpart z_ + ypart w_ * ypart z_)
 endgroup
enddef;

% expand "decimal" to cover some other data types
let ori_decimal=decimal;
def decimal primary n = 
 (
  if path n: 
   for i_=0 upto length(n)-1: if i_>0: & " " & fi
    decimal(point i_ of n) & " " & decimal(postcontrol i_ of n)  & " " & 
     decimal(precontrol i_+1 of n)  & " " & decimal(point i_+1 of n) 
   endfor
  elseif color n: ori_decimal(redpart(n)) & " " &
   ori_decimal(greenpart(n)) & " " & ori_decimal(bluepart(n)) 
  elseif pair n: ori_decimal(xpart(n)) & " " & ori_decimal(ypart(n)) 
  else: ori_decimal(n) fi
 )
enddef;

% The definition of |postdir| and |predir| given below is
% based on the following observation, being the consequence
% of l'H\^opital's rule: consider a~B\'ezier segment
% |a .. controls b and c .. d|; normally, the vector $\vec{ab}$
% determines the ``post'' direction at node~$a$; if $b$
% coincides with $a$, then the vector $\vec{ac}$ determines
% the direction; if also $c$ coincides coincides with~$a$,
% then the last resort is the vector $\vec{ad}$; if even $d$
% coincides with~$a$, the B\'ezier segment is degenerated,
% and can be removed (a~similar argumentation can be provided
% for the ``pre'' direction at node~$d$).

% Previous, insufficiently robust definitions:
% |vardef predir expr t of p = (point t of p)-(precontrol t of p) enddef;|
% |vardef postdir expr t of p = (postcontrol t of p)-(point t of p) enddef;|
% |vardef udir expr t of p = unitvector(direction t of p) enddef;|

% New, more general definitions:
vardef gendir expr t of p =
 predir t of p + postdir t of p % |direction|-compatible definition
enddef;
vardef predir expr t of p =
 save a_,b_,c_,d_,s_,t_; pair a_,b_,c_,d_; path s_; t_:=t;
 if not cycle p: if t<0: t_:=0; elseif t>length(p): t_:=length(p); fi fi
  s_=subpath (ceiling t_-1,t_) of p;
  a_=point 0 of s_;
  b_=postcontrol 0 of s_; % |b_<>postcontrol t-1 of p| for |t=0|
  c_=precontrol 1 of s_;
  d_=point 1 of s_;
  if d_<>c_:     d_-c_
  elseif d_<>b_: d_-b_
  elseif d_<>a_: d_-a_
  else:       (0,0)
 fi
enddef;

vardef postdir expr t of p =
 save a_,b_,c_,d_,s_,t_; pair a_,b_,c_,d_; path s_; t_:=t;
 if not cycle p: if t<0: t_:=0; elseif t>length(p): t_:=length(p); fi fi
  s_=subpath (t_,floor t_+1) of p;
  a_=point 0 of s_;
  b_=postcontrol 0 of s_;
  c_=precontrol 1 of s_; % |c_<>precontrol t+1 of p| for |t=length p|
  d_=point 1 of s_;
  if a_<>b_:     b_-a_
  elseif a_<>c_: c_-a_
  elseif a_<>d_: d_-a_
  else:       (0,0)
 fi
enddef;

% Definitions related to ``pre-'' and ``post-''
vardef udir expr t of p = unitvector(gendir t of p) enddef;
vardef upredir expr t of p = unitvector(predir t of p) enddef;
vardef upostdir expr t of p = unitvector(postdir t of p) enddef;
vardef pos_subpath expr z of p =
 if not cycle p: subpath z of p else:
  if xpart(z)<=ypart(z): subpath z of p
  else: subpath (xpart(z),ypart(z)+length(p)) of p fi
 fi
enddef;

vardef posttension expr t of p = % ``The \MF{}book'', ex. 14.15
 save q_; path q_;
 q_=point t of p {direction t of p} .. {direction t+1 of p} point t+1 of p;
 length(postcontrol 0 of q_ - point 0 of q_)/
 length(postcontrol t of p - point t of p)% doesn't work for ``straight lines''
enddef;
vardef pretension expr t of p = % ditto
 save q_; path q_;
 q_=point t-1 of p {direction t-1 of p} .. {direction t of p} point t of p;
 length(precontrol 1 of q_ - point 1 of q_)/
 length(precontrol t of p - point t of p)% doesn't work for ``straight lines''
enddef;

% The two macros below, |path_eq| and |inside| macros, might have been
% primitives. The macro |path_eq| is obvious; |a inside b| returns true
% if the bounding box of |a| is inside the bounding box of |b|, which
% may be misleading; think, for example of:
% |fullcircle inside unitsquare shifted (-1/2,-1/2) scaled .9 rotated 45|.
% For most curves occuring in fonts, however, one can safely infere
% that if |a inside b| holds, then |a| is inside |b|.
vardef path_eq(expr a,b)=
 save i_,l_,r_; boolean r_;
 r_:=(length(a)=length(b)) and (cycle a= cycle b);
 if r_:
  i_:=0; l_:=length(a) if cycle a: -1 fi;
  forever:
   r_:=(point i_ of a = point i_ of b); exitif not r_;
   r_:=(precontrol i_ of a = precontrol i_ of b); exitif not r_;
   r_:=(postcontrol i_ of a = postcontrol i_ of b); exitif not r_;
   exitif incr i_>l_;
 endfor fi
 r_
enddef;

tertiarydef a inside b =
 if path a: % |and path b|
  (xpart llcorner b < xpart llcorner a) and
  (xpart urcorner b > xpart urcorner a) and
  (ypart llcorner b < ypart llcorner a) and
  (ypart urcorner b > ypart urcorner a)
 else: % |numeric a and pair b|
  (a>=xpart b) and (a<=ypart b)
 fi
enddef;

% The macro |&&| is to be used instead of the |&| operator if the respective
% ends of paths coincide only approximately; using |..| instead would add
% unwanted tiny B\'ezier segments. The macro is somewhat ``left-handed,''
% i.e., it does not consider the expression that follow the macro, therefore,
% it can be used before the `|cycle|' command; if the argument |p| of the
% macro |amp_amp_| is a |pair|, it is just ignored which may be
% considered hardly intuitive.
def && = amp_amp_ whatever enddef;
tertiarydef p amp_amp_ q =
 if not pair p:
  (subpath(0,length(p)-1) of p) .. controls (postcontrol length(p)-1 of p)
   and (precontrol length(p) of p) ..
 fi
enddef;

vardef extrapolate expr t of b = % |t| pair, |b| B\'ezier segment
 clearxy;
 Casteljau(xpart(t)) = point 0 of b;
 Casteljau(1/3[xpart(t),ypart(t)]) = point 1/3 of b;
 Casteljau(2/3[xpart(t),ypart(t)]) = point 2/3 of b;
 Casteljau(ypart(t)) = point 1 of b;
 z0 .. controls z1 and z2 .. z3
enddef;

def Casteljau(expr t) =
 t[t[t[z0,z1], t[z1,z2] ], t[t[z1,z2], t[z2,z3] ] ]
enddef;

vardef elongation_to_times(expr ea,eb) =
 % negative parameter values are admissible; they are meant for |pen_stroke|
 (if ea<0: - fi 1/(abs(ea)+1), eb/(abs(eb)+1))
enddef;

% A~numerical function `|point_line_dist|' takes as a~parameter
% three |pair| expressions and returns a~(signed) value of the distance
% of the first parameter from the line defined by the other two.
% It is referred to in the `|is_line|' function.

vardef point_line_dist(expr a,b,c) =
 clearxy; save d_; d_=sqrt(length(b-c));
 z0=a/d_; z1=b/d_; z2=c/d_;
 (x2-x1)*(y1-y0)-(x1-x0)*(y2-y1)
enddef;

% The idea of calculation of a turning angle
% between two vectors, employed in the definition of the function
% `turn_ang,' is based on the following observation:
vardef turn_ang(expr za,zb) =
 if (abs(za)>=1/1000) and (abs(zb)>=1/1000): % |eps| may be not enough
  angle(unitvector(za) zscaled (unitvector(zb) reflectedabout (origin,right)))
 else: whatever fi
enddef;

% A~Boolean function `|is_line|' checks whether a~given B\'ezier segment
% is a~straight line. For large segments (fonts) it makes sense to specify
% a~numerical parameter |is_line_off>=0|; it defines a~maximal acceptable
% distance of the control points of a~B\'ezier arc from its secant
% (which corresponds to the distance between the arc and the secant
% circa |3/4is_line_off| for a symmetric, inflexionless arcs).
vardef is_line(expr B) =
 save r_; boolean r_;
 if known is_line_off:
  save a_;
  a_=length((point 1 of B)-(point 0 of B));
  r_=(-a_+arclength(B))<=(a_/infinity);
  if r_:
   r_:=(is_line_off>=abs(point_line_dist(
         postcontrol 0 of B, point 0 of B, point 1 of B))) and
       (is_line_off>=abs(point_line_dist(
         precontrol 1 of B, point 0 of B, point 1 of B)));
  fi
 else: % backward compatibility
  save a_,b_,c_,d_;
  a_=length((point 1 of B)-(point 0 of B));
  b_=length((postcontrol 0 of B)-(point 0 of B));
  c_=length((precontrol 1 of B)-(postcontrol 0 of B));
  d_=length((point 1 of B)-(precontrol 1 of B));
  r_=(-a_+b_+c_+d_ <= a_/infinity);
 fi
 r_
enddef;

% Abbreviations for a few simple yet useful phrases
def xyscaled primary p = xscaled xpart(p) yscaled ypart(p) enddef;
def yxscaled primary p = yscaled xpart(p) xscaled ypart(p) enddef;

% The macro |insert_nodes| inserts additional nodes at given non-integer
% non-repeating times~|t| into a given path |p|.
% The code would be a bit longer without `|arclength|' and `|arctime|.'
% The macro can be useful in some cases in the context of finding
% the envelopes of pen-stroked paths (avoiding inflection
% points---see below).
vardef insert_nodes(expr p)(text t) =
 save j_, p_, s_, t_; path p_; p_:=p;
 t_:=0;
 for i_:=t:
  if round(i_)<>i_: % ignore integer times
   t_[incr t_]=arclength(subpath(0,i_ mod length(p_)) of p_);
  fi
 endfor
 for i_:=1 upto t_:
  s_:=arctime t_[i_] of p_;
  if abs(round(s_)-s_)>eps: % ignore repeating times; is |eps| OK?
   p_:=(subpath (0, s_) of p_) && (subpath (s_,length p_) of p_)
    if cycle p_: & cycle fi;
  fi
 endfor;
 p_
enddef;

% get rid of degeneracies
def regenerate(expr p) =
  begingroup
    save q;
    path q;
    for t=1 step 1 until length p:
      if abs((point t of p)-(point (t-1) of p))>3:
        if unknown q:
          q:=subpath (t-1,t) of p;
        elseif length(q)=1:
          q:=(point 0 of q)..
              controls (postcontrol 0 of q) and (precontrol 1 of q)..
            (0.5[point 1 of q,point t-1 of p])..
              controls (postcontrol t-1 of p) and (precontrol t of p)..
            (point t of p);
        else:
          q:=(subpath (0,length(q)-1) of q)..
              controls (postcontrol length(q)-1 of q)
                and (precontrol length(q) of q)..
            (0.5[point length(q) of q,point t-1 of p])..
              controls (postcontrol t-1 of p) and (precontrol t of p)..
            (point t of p);
        fi;
      fi;
    endfor;
    if cycle p:
      q:=subpath (0,length(q)-1) of q..
           controls (postcontrol length(q)-1 of q)
             and (precontrol length(q) of q)..
         cycle;
    fi;
    q
  endgroup
enddef;

% like Fill, but doesn't complain about turning number
def dangerousFill text glist =
 begingroup
  save h_; path h_;
  for g_:=glist:
   h_:=g_ start.default; % JMN's suggestion
   if glyph_usage div store = 1: % storing
    glyph_stored.glyph_name[incr glyph_stored.glyph_name.num]=h_;
   fi
   glyph_list[incr glyph_list.num]:=round_node_values(h_ italicized);
   update_glyph_bb(glyph_list[glyph_list.num]);
  endfor;
 endgroup
enddef;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PREFIX AND SUFFIX HANDLING

% A method, entangled a bit and not particularly robust, of testing whether
% a parameter is a {\it string\/} expression or a {\it suffix}.
% (Remark: |is_suffix((a))| or |is_suffix(a+b)| returns |true|;
% |is_suffix(((a)))| causes \MP{} to report an error).
vardef is_suffix(text suffix_or_not_suffix) =
 save the_suffix_; string the_suffix_; is_suffix_ suffix_or_not_suffix;
 the_suffix_<>""
enddef;
def is_suffix_ suffix $ = the_suffix_:= str $; killtext enddef;

% suffix munging

def store_prec_obj = store_prec_obj_ whatever enddef;
primarydef a store_prec_obj_ b = hide(def prec_obj = a enddef) enddef;

% primarydef a sub b =
%  if path a: (pos_subpath b of a) elseif string a: (substring b of a) fi
% enddef;

def node = store_prec_obj node_ enddef;
vardef node_@# primary a =
 if str @#="x": xpart(point a of prec_obj)
 elseif str @#="y": ypart(point a of prec_obj)
 elseif str @#="": point a of prec_obj
 else:
  errhelp "The operator `node' works only with `x', `y' or an empty suffixes.";
  errmessage "PX: improper usage of `node'";
 fi
enddef;

def first suffix $ =
 if str $="at": % moves the first point of a path to a specified location
  store_prec_obj prec_obj shifted -(point 0 of prec_obj) shifted
 else: node$(0) fi
enddef;
def last suffix $ =
 if str $="at": % moves the last point of a path to a specified location
  store_prec_obj prec_obj shifted
   -(point if cycle prec_obj: 0 else: infinity fi of prec_obj) shifted
 else: node$(if cycle prec_obj: 0 else: infinity fi) fi
enddef;
 
% Neat macros excerpted from John D. Hobby's boxes.mp macro package

% Find the length of the prefix of string |s| for which |cond| is true for
% each character |c| of the prefix
vardef genericize_prefix(expr s)(text cond) =
 save i_, c_; string c_;
 i_ = 0;
 forever:
  c_ := substring (i_,i_+1) of s;
  exitunless cond; exitif incr i_=length s;
 endfor
 i_
enddef;

% Take a string returned by the |str| operator and return the same string
% with explicit numeric subscripts replaced by generic subscript symbols []:
vardef genericize(expr s) =
 save res_, s_, l_; string res_, s_;
 res_=""; % result so far
 s_ =s; % left to process
 forever: exitif s_="";
  l_:=genericize_prefix(s_, (c_<>"[") and ((c_<"0") or (c_>"9")));
  res_:=res_ & substring (0,l_) of s_;
  s_:=substring (l_,infinity) of s_;
  if s_<>"":
   res_ := res_ & "[]";
   l_ :=if s_>="[": 1+genericize_prefix(s_, c_<>"]")
    else: genericize_prefix(s_, (c_=".") or ("0"<=c_) and (c_<="9")) fi;
   s_:=substring(l_,infinity) of s_;
  fi
 endfor
 res_
enddef;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A MODULE THAT FINDS AN ENVELOPE OF A PATH DRAWN WITH A PEN

% The following macros approximate the envelope of an elliptical or a razor
% pen. The exact solution is impossible---in general, the envelope is not
% a B\'ezier curve, therefore some heuristics is, in general, unavoidable.
% We assumed that the backbone of a figure is such that
% the envelope does not form loops at smoothly joined nodes. Moreover,
% all B\'ezier segments appearing in the process {\bf should not}
% contain inflection points (the reason for this limitation is the
% method of finding an approximation of a pen envelope). If the latter
% condition is not fulfilled, one may expect weird results (see the usage
% of the |...| operator in the code of |pen_stroke_edge|).

% We assume that slanting should not distort a pen. Therefore, if
% a glyph is to be slanted {\it after\/} expanding a stroke, which
% usually is the case, the envelope should be constructed with
% an {\it unslanted pen}. Macros |slant_stroke|, |unslant_stroke|,
% and |unslant_angle| are devised to facilitate handling this
% situation. These macros refer to the variable |slant_stroke_val|;
% it should be assigned a definite value prior to expanding stroke.
def slant_stroke =
 if known slant_stroke_val: slanted slant_stroke_val fi
enddef;
def unslant_stroke =
 if known slant_stroke_val: slanted -slant_stroke_val fi
enddef;
vardef unslant_angle(expr a) = angle(dir(a) unslant_stroke) enddef;

% Macro |fix_nib| returns a path. If |y_diam| parameter
% is~0, a ``razor'' pen (a segment) is returned, otherwise it is
% an approximation of an ellipse. We do our best to avoid unnecessary
% nodes, hence the approximation is somewhat complicated; another reason
% for the complication is that interpolation and affine transformations
% do not commute, therefore the appropriate nodes are found for
% the untransformed pen, and only then the pen is transformed.
% {\it Note\/}: So far, there is no explicit relation between a built-in
% \MP{} pen mechanism and the |fix_nib| operation, in particular,
% |beginfig| does not alter the setting of |default_nib|. Needs rethinking.

vardef fix_nib(expr x_diam, y_diam, rot_angle) =
 if (x_diam<>0) and (y_diam<>0): fix_elliptic_nib(x_diam, y_diam, rot_angle)
 elseif (x_diam<>0) and (y_diam=0): fix_razor_nib(x_diam, rot_angle)
 elseif (x_diam=0) and (y_diam<>0): fix_razor_nib(y_diam, rot_angle+90)
 else:
  errhelp "I'll use the default pen, but I'd suggest to cancel the job.";
  errmessage "PX: the null pen is not alowed";
  default_nib
 fi
enddef;

vardef fix_razor_nib(expr x_diam, rot_angle) =
 ((-1/2x_diam,0)--(1/2x_diam,0)) rotated rot_angle unslant_stroke
enddef;

vardef fix_elliptic_nib(expr x_diam, y_diam, rot_angle) =
 save p_; path p_;
 % construct a temporary ellipse:
 p_:=fullcircle
  xscaled x_diam yscaled y_diam rotated rot_angle unslant_stroke;
 % construct an elliptic pen path having
 % 4 or, if necessary (heuristic), 6 nodes:
 (for d=up unslant_stroke, left,
  if (y_diam/x_diam<1/2) and (abs(rot_angle mod 90)>5):
    left rotated rot_angle unslant_stroke,
  fi
  down unslant_stroke, right,
  if (y_diam/x_diam<1/2) and (abs(rot_angle mod 90)>5):
    right rotated rot_angle unslant_stroke
  fi:
  (point(directiontime d of p_) of fullcircle)
   {direction (directiontime d of p_) of fullcircle}...
 endfor cycle) xscaled x_diam yscaled y_diam rotated rot_angle unslant_stroke
enddef;

% Arcs of a pen shorter than |ignore_nib_limit| will be joined together
% to form larger ones. Remember to adjust the parameter |ignore_nib_limit|
% if the size of |default_nib| is significantly changed.
newinternal ignore_nib_limit; ignore_nib_limit:=5;

path default_nib;
default_nib:=fix_nib(50,50,0); % hundred times as large as a default plain pen

newinternal default_elongation, default_join, default_cap;
default_elongation:=1/2;
default_join:=1;
  % 0 -- tip, default elongation used
  % 1 -- pen join, default elongation ignored
  % 2 -- tip, default elongation ignored, elongation=0 used
default_cap:=1;
  % 0 -- cut 90 rel
  % 1 -- pen end

% |tangent_point|, |pen_join|, |pen_stroke_edge_|, and |pen_stroke_edge|
% are auxiliary macros, exploited by the main macro, i.e., |pen_stroke|.
vardef tangent_point(expr d,nib) = % |d| -- direction of pen movement
 save a_;
 point if cycle nib: (directiontime d of nib) else:
  hide (a_:=turn_ang(d,(point 1 of nib)-(point 0 of nib)))
  if abs(a_ mod 180)<.1: 1/2 % emergency
  elseif a_<0: 0 else: 1 fi
 fi of nib
enddef;

vardef pen_join(expr a,b,c,nib)=
 % deleting superfluous nodes is based on the |arclength| operation
 % which, obviously, is not preserved after slanting,  but let's hope
 % it does not matter (too much)
 save t_, m_, m__, ta_, tb_, p_; path p_;
 m_:=infinity; % will be the minimal length of |nib|'s segment
 for t_:=0 upto 1/2length(nib)-1:
  m__:=arclength(subpath(t_,t_+1) of nib);
  if m__<m_: m_:=m__; fi
 endfor
 if m_<ignore_nib_limit:
  message "PX: the shortest nib segment < ignore_nib_limit (" &
    decimal(m__) & " < " & decimal(ignore_nib_limit) & ")";
 fi
 p_=nib shifted c;
 if cycle nib:
  ta_=directiontime a of p_; tb_=directiontime b of p_;
  p_:=pos_subpath(ta_,tb_) of p_;
  if arclength(p_)>ignore_nib_limit:
   for i_:=0,0:
    p_:=reverse p_; % short segments may appear at both ends
    if length(p_)>1: % optimization
     if arclength(subpath (0,1) of p_)<1/4ignore_nib_limit: 
      % cf. the comment concerning |1/4ignore_nib_limit| in
      % |pen_stroke_edge| below
      p_:=(point 0 of p_) .. controls (postcontrol 1 of p_) and 
        (precontrol 2 of p_) .. subpath (2,infinity) of p_;
    fi
   fi
   endfor
  else:
   p_:=(point 0 of p_){a}...{b}(point length(p_) of p_);
  fi
 else: % razor nib
  p_:=tangent_point(a,p_)--tangent_point(b,p_);
 fi
 p_
enddef;

% The finding of a~pen envelope for a~given B\'ezier segment,
% defined by nodes |a|, |b|, |c|, and |d|, begins with
% the placing the pen at the ends of the B\'ezier segment
% (i.e., at the points |a|, |d|) and finding the corresponding points
% |a'| and |d'| where the pen outline is parrallel to the direction
% of the original path at these points. Then, the outline is constructed.
% For |pen_stroke_method=0| (default), the envelope segment is constructed
% by setting the beginning and final directions (optionally, the direction
% at a given node can be ignored); for |pen_stroke_method=1| or~2
% an alternative (more elaborate) procedure is involved which explicitly
% computes control nodes |b'| and |c'| of the resulting path basing on
% a~heuristic assumption that
% |length(b'-a')/length(b-a)|~$\approx$
% |length(c'-d')/length(c-d)|~$\approx$
% |length(a'-d')/length(a-d)|.\break
% The default method never produce concave edges because the operator |...|
% is used always; the alternative methods employs the operator
% |force_convex_edge| instead; for |pen_stroke_method=1| the convex edges
% are forced (i.e, inflexion points are being removed),
% for |pen_stroke_method=2| no forcing of convex edges takes place.
vardef extrapoline expr t of B = % the result may be not a single segment
 save l_, t_;
 (t_.a,t_.b)=t; % |0<=ta_<tb_<=1|!
 l_=arclength(B)/(t_.b-t_.a); l_.a=l_*t_.a; l_.b=l_*(1-t_.b);
 if t_.a>0: ((point 0 of B) - l_.a*(upostdir 0 of B))-- fi
  B
 if t_.b<1:  -- ((point 1 of B) + l_.b*(upredir 1 of B)) fi
enddef;

vardef force_convex_edge(expr za, zb, zc, zd) =
 save a_, b_, c_, d_, z_;
 a_:=length(zd-za); b_:=length(zb-za); c_:=length(zc-zb); d_:=length(zd-zc);
 if (-a_+b_+c_+d_ > a_/infinity):
  if pen_stroke_method=2:
   za .. controls zb and zc .. zd
  else:
   if (a_>0.01) and (b_>0.01) and (c_>0.01) and (d_>0.01): % no degeneration...
    a_:=signum((za-zd) rotated -90 dotnorm (zb-za));
    b_:=signum((zb-za) rotated -90 dotnorm (zc-zb));
    c_:=signum((zc-zb) rotated -90 dotnorm (zd-zc));
    d_:=signum((zd-zc) rotated -90 dotnorm (za-zd));
    if ((a_<>b_) or (b_<>c_)) and (a_=d_):
     numeric b_, c_; pair z_;
     z_=b_[za,zb]=c_[zd,zc];
     za .. controls
      if b_<1: z_ else: zb fi and if c_<1: z_ else: zc fi
     .. zd
    else:
     za .. controls zb and zc .. zd
    fi
   else:
    za .. controls zb and zc .. zd
   fi
  fi
 else:
  za -- zd
 fi
enddef;

vardef pen_stroke_edge_(expr b,b_nib,e_nib) = % |b| -- B\'ezier segment
 save pa_,pb_,qa_,qb_,ra_,rb_,sa_,sb_;
 pair pa_,pb_,qa_,qb_,ra_,rb_,sa_,sb_;
 pa_=point 0 of b; ra_=(postcontrol 0 of b)-pa_; sa_=postdir 0 of b;
 pb_=point 1 of b; rb_=(precontrol 1 of b)-pb_;  sb_=predir 1 of b;
 qa_=pa_ + tangent_point(sa_, b_nib);
 qb_=pb_ + tangent_point(sb_, e_nib);
 if pen_stroke_method=0:
  qa_ {sa_} ... {sb_} qb_
 elseif (pen_stroke_method=1) or (pen_stroke_method=2):
  save lp_,lq_; lp_=length(pb_-pa_); lq_=length(qb_-qa_);
  if 2lp_<lq_: % heuresis -- too close nodes
   qa_ {sa_} ... {sb_} qb_
  else:
   force_convex_edge(qa_, qa_+lq_/lp_*ra_, qb_+lq_/lp_*rb_, qb_)
  fi
 else:
  errhelp "Only the values 0,1 and 2 for `pen_stroke_method' are admissible. " &
   "Better stop now.";
  errmessage "PX: unknown pen stroke method (" &
   decimal(pen_stroke_method) & ")";
 fi
enddef;

vardef pen_stroke_edge@#(expr p) =
 save e_,l_,i_,i__; path e_[\\];
 l_:=length(p);
 for i_:=0 upto l_-1:
  e_[i_]=pen_stroke_edge_(subpath (i_,i_+1) of p,
   % |local_nib_@#(i_),local_nib_@#(i_+1));| % a nasty bug removed 20.08.2009
   local_nib_@#(i_),local_nib_@#((i_+1) if cycle p: mod l_ fi)); 
 endfor
 for i_:=0 upto l_ if cycle p: -1 else: -2 fi:
  i__:=(i_+1) mod l_;
  save t_;
  t_:=turn_ang(predir 1 of e_[i_], postdir 0 of e_[i__]);
  if if known t_: abs(t_)>1 else: false fi:
   save t_; (t_.a,t_.b)=e_[i_] intersectiontimes e_[i__];
   if t_.a>0:
    e_[i_]:=subpath (0,t_.a) of e_[i_];
    e_[i__]:=subpath (t_.b,1) of e_[i__];
   elseif known local_tip_@#(i__):
    save tx_, ty_, b_, b__, ei_, ei__; path ei_, ei__, ei_[], ei__[];
    (tx_,ty_)=local_tip_@#(i__);
    ei_:=if is_line(e_[i_]):
     (point 0 of e_[i_])--
     (1/abs(tx_))[point 0 of e_[i_], point 1 of e_[i_] ]
     elseif tx_<0: hide(b_:=1) extrapoline (0,abs(tx_)) of e_[i_]
     else: extrapolate (0,abs(tx_)) of e_[i_] fi;
    ei__:=if is_line(e_[i__]):
     (1/(1-abs(ty_)))[point 1 of e_[i__], point 0 of e_[i__] ] --
     point 1 of e_[i__]
    elseif ty_<0: hide(b__:=1) extrapoline (abs(ty_),1) of e_[i__]
    else: extrapolate (abs(ty_),1) of e_[i__] fi;
% clumsy HEURESIS (choosing an optimal intersection point, if there are
% more intersections):
    save t_; (t_.a1,length(ei__)-t_.b1)=ei_ intersectiontimes reverse ei__;
    if t_.a1>0:
     ei_1:=if (known b_)  and (t_.a1>1):
       force_convex_edge(point 0 of e_[i_], postcontrol 0 of e_[i_],
        precontrol 1 of e_[i_],  point t_.a1 of ei_)
      else: subpath (0,t_.a1) of ei_ fi;
     ei__1:=if (known b__) and (t_.b1<1):
      force_convex_edge(point t_.b1 of ei__, postcontrol 0 of e_[i__],
       precontrol 1 of e_[i__], point 1 of e_[i__])
      else: subpath (t_.b1,infinity) of ei__ fi;
     (length(ei_)-t_.a2,t_.b2)=reverse ei_ intersectiontimes ei__;
     if length((t_.a1,t_.b1)-(t_.a2,t_.b2))>eps:
      ei_2:=if (known b_) and (t_.a2>1):
       force_convex_edge(point 0 of e_[i_], postcontrol 0 of e_[i_],
        precontrol 1 of e_[i_],  point t_.a2 of ei_)
       else: subpath (0,t_.a2) of ei_ fi;
      ei__2:=if (known b__) and (t_.b2<1):
      force_convex_edge(point t_.b2 of ei__, postcontrol 0 of e_[i__],
       precontrol 1 of e_[i__], point 1 of e_[i__])
       else: subpath (t_.b2,infinity) of ei__ fi;
      if arclength(ei_1)+arclength(ei__1) > arclength(ei_2)+arclength(ei__2):
       ei_1:=ei_2; ei__1:=ei__2;
      fi
     fi
     e_[i_]:=ei_1; e_[i__]:=ei__1;
    fi
   fi
  fi
 endfor
 for i_:=0 upto l_-1:
  hide(i__:=(i_-1) mod l_)
  if cycle p or (i_>0):
   if length((point 1 of e_[i__])-(point 0 of e_[i_]))>1/4ignore_nib_limit:
   % the constant |1/4ignore_nib_limit| plays a similar role
   % to that of the |SNAP_TO_NODE| variable in pf2mt1.awk
    (point 1 of e_[i__])
    if known local_tip_@#(i_): -- else:
     && pen_join(predir 1 of e_[i__],postdir 0 of e_[i_],point i_ of p,
      local_nib_@#(i_)) &&
    fi
   fi
  fi
  % reconstruct |e_[i_]| (possibly ignoring direction(s)):
  (point 0 of e_[i_])
   if is_line(e_[i_]):
   % the using of |--| circumvents \MF{}//\MP{} instable behaviour:
   % the operator |...| may cause that a control point and a node
   % (nearly) coincide (note that this is feature, not a bug);
   % thus, it is advisable for |pen_stroke_method=0|; supposedly,
   % it is also adequate for |pen_stroke_method=1|:
    --
   else:
    if pen_stroke_method=0:
     if not ignore_dir_(i_): {postdir 0 of e_[i_]} fi ...
     if not ignore_dir_(i_+1): {predir 1 of e_[i_]} fi
    elseif (pen_stroke_method=1) or (pen_stroke_method=2):
     .. controls (postcontrol 0 of e_[i_]) and (precontrol 1 of e_[i_]) ..
    fi
   fi
 endfor
 if cycle p: cycle else: (point 1 of e_[l_-1]) fi
enddef;
newinternal pen_stroke_method;

% Macro |pen_stroke| performs an operation known as ``expanding stroke'';
% we'll call the result of the operation a ``pen envelope'' (for
% a given path). The macro has one optional parameter, |opts| (|text|),
% and two obligatory ones: input path~|p| (|expr|)
% and a |result| (|suffix|). A user has an access to subpaths of the
% envelope, namely: |result.r| is the right edge of the envelope,
% |result.l|---its left edge, |result.b|---is a fragment of the pen outline
% joining left and right edge of the envelope at the beginning
% node of the path, |result.e|---is a similar fragment at the ending
% node of the path (see the picture below). If the path~|p|
% is cyclic, then |result.e| and |result.b| are undefined,
% otherwise the variable |result| contains additionally the complete
% expanded stroke.

% For finding an envelope, a default path (|default_nib|, returned
% by |fix_nib|) is used except nodes for which the parameter |opts|
% sets another pen. Mastering the usage of the parameter |opts| allows
% a user to achieve nontrivial effects. The parameter |opts| is a list
% (space-separated or semicolon-separated) of the following
% operators: (1)~|nib|, (2)~|cut|, (3)~|tip|, and (4)~|ignore_directions|.

% Ad 1. The macro |nib| has two parameters:
% |nib|(pen)(list_of_nodes), where ``pen'' is a path returned by
% macro |fix_nib|, and ``list_of_nodes'' contains comma-separated numbers
% (times) of the nodes of the path~|p| at which a given pen is to be
% used. If needed, the outline is complemented at corner nodes
% with a fragment of a pen path. Such a join corresponds to the setting
% |linejoin:=rounded| in \MP{}. If the path~|p| is non-cyclic,
% its ends are also complemented with appropriate fragments of a pen path
% (the setting |linecap:=rounded|). Such a method of joining is also applied
% by |pen_stroke| to nodes not mentioned in the parameter |opts|.
% The result of the following statement
% \LINE{\descriptioncomments
% |pen_stroke(nib(default_nib xyscaled (1,2))(infinity))(p)(q)|
% \unskip}
% \descriptioncomments
% that changes the pen at the last node of the path,
% is shown in the following picture:
% \LINE{\epsfbox{\illusname.110}}

% Ad 2. The call of the macro |cut| has the form: |cut|(angle,
% pen)(list_of_nodes) or |cut|(pen,~angle)(list_of_nodes),
% where ``pen'' and ``list_of_nodes'' are defined as
% previously. The pen parameter can be omitted which means using a default
% pen (|default_nib|). The macro replaces a default pen with a special
% ``razor'' pen at specified nodes. More precisely, it is a projection of a
% given pen in the direction of the path~|p| at a given node onto a
% straight line going through this node under the angle specified in the
% respective parameter of the macro. Uf\/f\/f\dots\ The angle of the straight
% line can be defined either absolutly (with respect to the axis~|x|)
% or---by adding a prefix `|rel|'---relatively to the direction of the path
% at a given node. From the point of view of a user, the result of the
% macro |cut| is ``cutting'' the expanded stroke with a straight
% line. This operation is particularly useful at the ends of a path and
% corresponds to the setting |linecap:=butt| in \MP{}, except that in \MP{}
% one cannot specify angles. The result of the statement
% \LINE{\descriptioncomments
% |pen_stroke(cut(45)(0)|
% |cut(default_nib xyscaled (1,2), rel 90)(infinity))(p)(q)|
% \unskip}
% \descriptioncomments
% that cuts both ends and, moreover, changes a pen
% at the ending node is shown in the figure below
% (at the beginning node, the absolute angle of 45 degrees is specified,
% at the ending one---the relative angle of 90~degrees):

% Ad 3. The call of the macro |tip| has the form |tip|(pen,
% pre_elongate, post_elongate)(list_of_nodes), where ``pen''
% and ``list_of_nodes'' have the same meaning as previously.
% In particular, a pen can be omitted. At corner nodes
% specified in the list of nodes, the consecutive elements of the outline
% are not joined with an appropriate subpath of a pen; instead, they
% are elongated (extrapolated) until they intersect. This process corresponds
% (roughly) to the \MP{} setting |linejoin:=mitered|:

% The illustration above is the result of the following call
% of the macro |pen_stroke| (the macro |tip| is invoked with default
% settings, only the number of a node is specified):
% |pen_stroke(tip()(3))(p)(q); draw q;|
% The optional parameters |pre_elongation| and |post_elongation| define how
% far the consecutive edges (segments) should be elongated in order to make
% them intersect each other (the measure is the time). If one parameter is
% omitted, both will receive the same value; if both are omitted, a~default
% value, |(0.5,0.5)| (it corresponds to elongation by circa 50\%), will be
% used. The precise meaning of the pre- and post-elongation is defined as
% follows: for a~given pre-edge |e1|, post-edge |e2|, pre-elongation |v1|
% and post-elongation |v2|, the paths
%  |extrapolate (0, 1/(1+v1) of e1| and
%  |extrapolate (v2/(1+v2), 1) of e2| are computed
% (i.e., for the default elongation: |extrapolate (0, 2/3) of s1|
% and |extrapolate (1/3, 1) of s2|, respectively).
% If elongated curves do not intersect, the terminal nodes
% of the consecutive segments are joined with a~straight line. The latter
% property can be used to obtain a~result corresponding to the \MP{} setting
% |linejoin:=beveled|: it suffices to apply a~null elongation, i.e.,
% |tip|(0)(list_of_nodes). Changing the first (empty) parameter
% of the |tip| macro in the previous example would yield the following
% result:

% Ad 4. The macro |ignore_directions| has a different character. It is
% invoked with one parameter being a comma-separated list of nodes:
% |ignore_directions|(list_of_nodes). The numbers {\it must be\/} followed
% by sufixes |l| or |r|. The macro causes that, at specified nodes,
% the direction of the outline is not forced to be parallel to the direction
% of the path~|p| (which is the default); instead, the direction is
% calculated by \MP{}. Suffixes determine whether the direction
% is not to be forced at the right (|r|) or the left~(|l|) edge (with
% respect to the direction of the path~|p|). This heuristic
% trick can be used to improve the appearence of the outline
% if the ``inner'' part of the envelope has too tight arcs.
%% The examples of the usage of this macro can be found in the \MP{} version
%% of D.~E.~Knuth's `logo' font (letters `P'~and~,S').

vardef pen_stroke(text opts)(expr p)(suffix result) =
 forsuffixes $=,r,l,b,e:
  if not path result$: scantokens("path " & genericize(str result$)); fi
 endfor
 save a_, a__, d_, i_, k_, n_, p_, z_, norm_, norml_, normr_, normlr_,
  fix_opts_, ignore_dir_, ignore_dir__, local_nib_, local_nib__,
  local_tip_, default_tip_, local_tip__, % internal
  all, rel, last, nib, cut, tip, ignore_directions, current_node; % exported
 numeric ignore_dir__[\\]; pair default_tip_, local_tip__[\\];
 path local_nib__[\\];
 pair a_, d_, z_[\\]; path p_;
%% xpart norm_ norml_ normr_ normlr_
 vardef norm_ primary n =
  if cycle p: n mod last else: if n<0: 0 elseif n>last: last else: n fi fi
 enddef;
 vardef norml_ primary n = -norm_ n -1 enddef;
 vardef normr_ primary n = norm_ n +1 enddef;
 vardef normlr_@# primary i= if str @#="l": -norm_(last-i)-1 else: i+1 fi enddef;
 last=length(p);
 vardef rel primary a =
  angle((gendir current_node of p) slant_stroke)+a
 enddef;
 def all =
  hide(% locally we use the prefix rather than postfix noitation;
       % a trick due to the |suffix| parameter of the |allcont_| macro
   vardef l primary n = (norml_ n,0) enddef;
   vardef r primary n = (normr_ n,0) enddef) allcont_
 enddef;
 def allcont_ suffix $ =
  $0 for i_:=1 upto last if cycle p: -1 fi: , $i_ endfor
 enddef;
 vardef fixopts_(suffix optname)(text nodes) text val =
%% intersectiontimes lcont_ rcont_
  save l, r, lcont_, rcont_;
  def l = lcont_ whatever enddef; primarydef a lcont_ b = (norml_ a,0) enddef;
  def r = rcont_ whatever enddef; primarydef a rcont_ b = (normr_ a,0) enddef;
  for n_:=nodes:
   if numeric n_:
    current_node:=norm_ n_;
    optname[norml_ n_]:=optname[normr_ n_]
   else:
    current_node:=abs(xpart n_)-1; % the inverse of both |norml_| and |normr_|
    optname[xpart(n_)]
   fi :=val; % |val| may depend on |current_node|
  endfor
 enddef;
 def nib(text nib_)(text nodes) = % nib and node list
  fixopts_(local_nib__)(nodes)
   begingroup
    p_:=default_nib; for k_:=nib_: p_:=k_; endfor \\ p_
   endgroup;
 enddef;
 def cut(text nib_and_ang)(text nodes) = % angle, nib and node list
  fixopts_(local_nib__)(nodes)
   begingroup
    p_:=default_nib;
    for k_:=nib_and_ang:
     if numeric k_: a_:=dir(unslant_angle(k_)); else: p_:=k_; fi
    endfor
    d_:=gendir current_node of p;
    z_1:=whatever*a_=tangent_point(d_,p_)+whatever*d_;
    z_2:=whatever*a_=tangent_point(-d_,p_)+whatever*d_;
    z_1--z_2
   endgroup;
 enddef;
 def tip(text nib_and_lim)(text nodes)= % limit(s) and node list
  i_:=0; for n_:=nib_and_lim: if numeric n_: i_[incr i_]:=n_; fi endfor
  fixopts_(local_tip__)(nodes)
   elongation_to_times(if i_=0: default_elongation, default_elongation
    elseif i_=1: i_1, i_1 else: i_1, i_2 fi);
  fixopts_(local_nib__)(nodes)
   begingroup
    p_:=default_nib; for k_:=nib_and_lim: if path k_: p_:=k_; fi endfor \\ p_
   endgroup;
 enddef;
 def ignore_directions(text nodes) = % node list
  fixopts_(ignore_dir__)(nodes) 1;
 enddef;
 if default_cap=0:
  if not cycle p: cut(rel 90)(0,last); fi
 elseif default_cap=1: % do nothing
 else:
  errhelp "Admissible values are 0, 1; continue, I'll use the value 1.";
  errmessage "PX: improper `default_cap' value ("&decimal(default_cap)&")";
 fi
 opts;

 if default_join=0:
  default_tip_:=elongation_to_times(default_elongation, default_elongation);
 elseif default_join=1: % no tip setting, do nothing
 elseif default_join=2:
  default_tip_:=(1,0); % |(1,0)=elongation_to_times(0,0)|
 else:
  errhelp "Admissible values are 0, 1, 2; continue, I'll use the value 1.";
  errmessage "PX: improper `default_join' value ("&decimal(default_join)&")";
 fi
 vardef ignore_dir_@#(expr i) = known ignore_dir__[normlr_@# i] enddef;
 vardef local_tip_@#(expr i) = if known local_tip__[normlr_@# i]:
  local_tip__[normlr_@# i] else: default_tip_ fi enddef;
 vardef local_nib_@#(expr i) = if known local_nib__[normlr_@# i]:
  local_nib__[normlr_@# i] else: default_nib fi enddef;
 result.r:=pen_stroke_edge.r(p);
 result.l:=pen_stroke_edge.l(reverse p);
 if not cycle p:
  result.b:=pen_cap(predir infinity of result.l,postdir 0 of result.r,
   -postdir 0 of p,point 0 of p,local_nib_.l(last),local_nib_.r(0));
  result.e:=pen_cap(predir infinity of result.r,postdir 0 of result.l,
   predir last of p,point last of p,local_nib_.r(last), local_nib_.l(0));
  result:=result.r && result.e && result.l && result.b && cycle;
 fi
enddef;

vardef pen_cap(expr a,b,c,p,niba,nibb)=
 if path_eq(niba,nibb): pen_join(a,b,p,niba)
 else: pen_join(a,c rotated 90,p,niba)--pen_join(c rotated 90,b,p,nibb)
 fi
enddef;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% POSTSCRIPT FONT GENERATION

% Note that this has been stripped down a lot from the METATYPE1
% original code; most of the stuff for hinting, ligature tables,
% METAFONT-style proof generation, and so on has been removed
% because it's irrelevant to Tsukurimashou.

vardef pfi_file = jobname & ".pfi" enddef;
vardef pic_file = "piclist" enddef;
vardef dim_file = jobname & ".dim" enddef;

errorstopmode; warningcheck:=-1;
ignore:=whatever; process:=0; utilize:=1; store:=2; % constants for introducing
let semicolon_=; ; % stores original meaning of a semicolon
newinternal tracingdimens; % if |tracingdimens>0| then |dim_file| is generated

def write_special = % additional info to be processed by AWK
 special "%GLYNFO: " &
enddef;
vardef mtone_glyph_pfx = "MT1: glyph " & str glyph_name & ": " enddef;
def mtone_message = message mtone_glyph_pfx & enddef;

% Macro write_tex provides contact with the
% outer world. The macro contains the information about EPSes that is
% used for proofing and assembling the font; must be consistent with
% the definitions contained in the files `mpform.sty' and `mp2pf.awk'.
vardef write_tex(expr name, num) =
 write "\EPSNAMEandNUMBER{" & name & "}{" & decimal(num) & "}"
  to pic_file & ".tex"
enddef;

% The following macros are related to the operation of slanting.
% In particular, they enable to keep a fixed width of a stem
% after slanting.
vardef slant_ang = % should be rather called ``local_slant_angle''
 slang \\ if known glyph_slanting.glyph_name: * glyph_slanting.glyph_name fi
enddef;
vardef slant_val = tand(slant_ang) enddef;
vardef slant_preadjust(expr slope, slang) =
% |if sind(angle(slope))=0: 1 else:|
% | abs(sind(angle(slope))/sind(angle(cotd(angle(slope))+tand(slang),1)))|
% |fi|
% Correction of stem size taking into account its slope and a slant angle;
% nice formula, isn't it? Much simpler than the previous one, yet equivalent:
 length(unitvector(slope) slanted tand(slang))
enddef;
vardef slant_stroke_val = slant_val enddef; % compatibility with plain_ex.mp

vardef stem_corr (expr slope) = slant_preadjust(slope, slant_ang) enddef;

def italicized = % fairly complex operation
 if slang<>0:
  if known glyph_slanting.glyph_name:
   if glyph_slanting.glyph_name=0: shifted (math_axis*tand(slang),0) fi
  fi
  shifted (italic_shift*tand(slang),0) % re-positioning
  slanted slant_val % and slanting
 fi
enddef;

primarydef b || c =
 whatever*b + c*stem_corr(b)*unitvector(b rotated 90)
enddef;

primarydef c /\ b =
% A variant of the |leg| procedure that iteratively counteracts slant
% deformation; as with |leg|, given: |c| -- hypotenuse (vector) of
% a right-angled triangle, |b| -- the length of one of its legs;
% result: the other leg of the triangle (vector),
 if slant_ang=0: (c leg b)
 else:
  begingroup save b_, b__, n_; b_:=b__:=b; n_:=10;
   forever:
    b_:=b*stem_corr(c leg b_);
    exitif (abs(b_-b__)<.01) or (n_<=0);
    b__:=b_; n_:=n_-1;
   endfor
   if (abs(b_-b__)>=.01):
    errhelp "The result is likely to be weird.";
    errmessage mtone_glyph_pfx & "iteration hasn't converged";
   fi
   c leg b_
  endgroup
 fi
enddef;

% Obsolete?
vardef rib(expr t,p,r) text u = % |u| is either empty or a vector
 save k_; pair k_; for i_:=u: k_:=u; endfor
 if unknown k_: k_=((udir t of p) rotated 90); fi
 (point t of p) + r * k_ * stem_corr(k_ rotated 90)
enddef;

% The operation {\it compose_path\/} is useful in \MP{} programs
% automatically generated from PFB sources (pf2mt1 utility). Suffixes
% $a$ and $b$ of control nodes stand for `after' and `before', respectively;
% The operation {\it compose_path\/} makes use of the operation
% {\it compose_segment\/} that serves for constructing non-cyclic
% paths. Undefined nodes are ignored.
vardef compose_segment@#(expr m,n) = % |m<=n|, not checked
 if unknown inside_compose_path_: save idx_, n_; n_:=-1; fi
 save n__; n__=n_+1;
 for i_:=m upto n: if known @#[i_]: idx_[incr(n_)]=i_; fi endfor
 for i_:=n__ upto n_-1:
   @#[idx_[i_]] .. controls
         @#[idx_[i_]]   if known @#[idx_[i_]]a: a fi
     and @#[idx_[i_+1]] if known @#[idx_[i_+1]]b: b fi
     ..
 endfor
 @#[idx_[n_]]
enddef;
vardef compose_path@#(expr n) =
 save inside_compose_path_, idx_, n_; n_:=-1; inside_compose_path_:=1;
 compose_segment@#(0,n)
  if @#[idx_[0]]=@#[idx_[n_]]: & else: -- fi \\ cycle
enddef;

% Basic macros for building character glyphs:
vardef round_node_values(expr p) =
 save d_; % candidates for Flex -- no checking for ``straightlinessness''
 for t_=0 upto length(p)-1:
  if round(point t_ of p)=round(point t_+1 of p):
   hide(mtone_message "degenerated bezier " & ", length=" &
    decimal(length(p)) & " " & ", time=" & decimal(t_) & " ";
   show p)
  else:
   round(point t_ of p)..
    if if known d_[t_] or known d_[t_+1]: false else:
     is_line(subpath (t_,t_+1) of p) fi:
    controls round(point t_ of p) and round(point t_+1 of p)
   else:
    controls round(postcontrol t_ of p) and round(precontrol t_+1 of p)
   fi
   ..
  fi
 endfor
 round(point length(p) of p) \\ if cycle p: & cycle fi
enddef;

primarydef a start b =
 if cycle a:
  if b=default: default_start_(a)
  else: ((subpath (b,length(a)+b) of a) & cycle) fi
 else: a fi
enddef;

newinternal default; default:=infinity;
vardef default_start_(expr p) =
 save i_,j_,pi_,pj_; pair pi_,pj_;
 j_:=0; pj_:=point j_ of p;
 for i_=1 upto length(p):
  pi_:=point i_ of p;
  if (xpart(pi_)>xpart(pj_)) or
   (xpart(pi_)=xpart(pj_)) and (ypart(pi_)<ypart(pj_)):
   j_:=i_; pj_:=point j_ of p;
  fi
 endfor
 (subpath (j_, length(p)+j_) of p) & cycle
enddef;

def Fill text glist =
 begingroup
  save h_; path h_;
  for g_:=glist:
   h_:=g_ start.default; % JMN's suggestion
   if turningnumber h_<>1:
    errhelp "The result is likely to be weird.";
    errmessage mtone_glyph_pfx & "strange turning number in Fill, " &
     decimal(turningnumber h_);
   fi
   if glyph_usage div store = 1: % storing
    glyph_stored.glyph_name[incr glyph_stored.glyph_name.num]=h_;
   fi
   glyph_list[incr glyph_list.num]:=round_node_values(h_ italicized);
   update_glyph_bb(glyph_list[glyph_list.num]);
  endfor;
 endgroup
enddef;

def unFill text glist =
 begingroup
  save h_; path h_;
  for g_:=glist:
   h_:=g_ start.default; % JMN's suggestion
   if turningnumber h_<>-1:
    errhelp "The result is likely to be weird.";
    errmessage mtone_glyph_pfx & "strange turning number in unFill, " &
     decimal(turningnumber h_);
   fi
   if glyph_usage div store = 1: % storing
    glyph_stored.glyph_name[incr glyph_stored.glyph_name.num]=h_;
   fi
   glyph_list[incr glyph_list.num]:=round_node_values(h_ italicized);
  endfor;
 endgroup
enddef;

def fix_hsbw (expr xr,ml,mr) =
 glyph_shift:=round(ml); % shift = left margin
 glyph_width:=round(xr+ml+mr); % declared width plus margins
 if glyph_usage div store = 1: % storing
  glyph_shift.glyph_name:=glyph_shift; glyph_width.glyph_name:=glyph_width;
 fi
enddef;

def fix_exact_hsbw(expr xr,ml,mr) =
 glyph_shift:=round(ml); % shift = left margin
 glyph_width:=xr+ml+mr; % declared width plus margins
 if glyph_usage div store = 1: % storing
  glyph_shift.glyph_name:=glyph_shift; glyph_width.glyph_name:=glyph_width;
 fi
enddef;

% Macros below set PostScript and \TeX{} units; a trick with `\#'
% in {\it tfm\_units\/} proves useful in achieving compatibility
% with the Knuthian fonts (e.g., it is employed in {\it logo\/} font).
% Old versions of {\it tfm\_units\/} and {\it ps\_units\/} are less
% accurate, but are kept because of backward compatibility reasons.
vardef tfm_units(text x) =
 save #; if known (x#): x# else: x/(1000/designsize) fi
enddef;
vardef old_tfm_units(text x) =
 save #; if known (x#): x# else: x/1000*designsize fi
enddef;

vardef ps_units(expr x) = x*(1000/designsize) enddef;
vardef old_ps_units(expr x) = x/designsize*1000 enddef;

def define_ps_units(text t) =
 forsuffixes $:=t: $:=ps_units($.#); endfor
enddef;
def define_whole_ps_units(text t) =
 forsuffixes $:=t: $:=round(ps_units($.#)); endfor
enddef;
def define_even_ps_units(text t) =
 forsuffixes $:=t: $:=2round(1/2ps_units($.#)); endfor
enddef;

% In general, all objects are supposed to be drawn by the
% {\bf endglyph} macro, i.e., all drawing operations are deferred.
% The same concerns labelling, which necessitates redefinition
% of labelling macros.

def label_(suffix pos)(expr s,z, dot_or_not) =
% should be more complex if overlapping labels are to be avoided
enddef;
string label_defaultfont; label_defaultfont:="cmr10";
newinternal label_defaultscale; label_defaultscale:=magstep 5;

% If the {\it project\/} variable is assigned value greater than~0,
% proofing mode is assumed; the following macros display then
% the details of the construction of glyphs for proofing purposes.
% The larger value of the variable {\it project}, the more details
% are visualised.
def local_drawoptions (text t) = % to be used within a group, see below
% \begingroup \def\\#1{{\it#1}}% local: no underscore hacks
 save _op_; drawoptions(t);
% \endgroup
enddef;

def update_glyph_bb(expr p) =
 if unknown glyph_llx:
  glyph_llx:=xpart(llcorner(p)); glyph_lly:=ypart(llcorner(p));
  glyph_urx:=xpart(urcorner(p)); glyph_ury:=ypart(urcorner(p));
 else:
  if xpart(llcorner(p))<glyph_llx: glyph_llx:=xpart(llcorner(p)); fi
  if ypart(llcorner(p))<glyph_lly: glyph_lly:=ypart(llcorner(p)); fi
  if xpart(urcorner(p))>glyph_urx: glyph_urx:=xpart(urcorner(p)); fi
  if ypart(urcorner(p))>glyph_ury: glyph_ury:=ypart(urcorner(p)); fi
 fi
enddef;
string stencil_dir;
def ship_glyphs =
 begingroup
  local_drawoptions();
  for g_:=1 upto glyph_list.num:
   if turningnumber glyph_list[g_]>0: fill else: unfill fi
    glyph_list[g_] shifted (glyph_shift,0);
  endfor
 endgroup
enddef;
newinternal show_stroke_size; show_stroke_size:=1.5;
color show_stroke_color; show_stroke_color:=red;

color label_dot_color, label_text_color;
label_dot_color:=.8white; label_text_color:=black;
newinternal label_dot_size; label_dot_size:=3bp;

% Begin and end of the definitions of a character glyph:
def begin_skip =
 let endglyph = fi;
 let ; = end_skip semicolon_
 if false:
enddef;
def end_skip =
 let ; = semicolon_ semicolon_
 let endglyph = endglyph_;
enddef;

def uni_name(text name) = % name is either a suffix or a string expression
 if is_suffix(name): name else: scantokens("_" & name) fi
enddef;

def glyph_name_ext = enddef;
def beginglyph(text name) =
 %
 def glyph_name = uni_name(name) glyph_name_ext enddef; % to use in |endglyph|
 numeric glyph_usage; glyph_usage:=glyph_usage.glyph_name;
 if unknown glyph_usage: expandafter begin_skip fi
 string ps_name; ps_name:=glyph_ps_name.glyph_name;
 if unknown ps_name:
  errhelp "Use macro `introduce' or `assign_name' prior to `beginglyph.'";
  errmessage "MT1: PS name not assigned to " & str glyph_name;
 fi
 if name_used(glyph_name):
  errhelp "Proceed if you wish, I'll use the second glyph description.";
  errmessage "MT1: double output: name " & (str glyph_name);
 fi
 if glyph_usage mod store = 1: % utilizing
  mark_name_used(glyph_name);
 fi
 numeric glyph_code, glyph_num; glyph_code:=name_to_code(glyph_name);
  if glyph_code<0: glyph_num:=500-decr(min_glyph_code); else:
   glyph_num:=100+glyph_code;
   if glyph_code>max_glyph_code: max_glyph_code:=glyph_code; fi
  fi
 %
 beginfig(glyph_num)
 if glyph_usage mod store = 1: % utilizing
  write_special "NAME " & ps_name & " " & decimal(glyph_code);
   % mpform.sty and mp2pf.awk interface
%  |write_tex(glyph_name, glyph_num);|
   write_tex(ps_name, glyph_num);
 fi;
 glyph_list.num:=label_list.num:=0;
 path glyph_list[\\];
 picture label_list[\\]; pair label_list.dot[\\];
 numeric glyph_llx, glyph_lly, glyph_urx, glyph_ury;
 numeric bitmap_scale; pair bitmap_offset;
 numeric glyph_shift, glyph_width, glyph_axis;
 save glyph;
 hstem_list.num:=vstem_list.num:=hstem_list.cov:=vstem_list.cov:=0;
 pair hstem_list[\\], vstem_list[\\];
 path hstem_list_segms[\\], vstem_list_segms[\\];
 numeric old_hinting_scheme, new_hinting_scheme;
 if glyph_usage div store = 1: % storing
  if not path glyph_stored.glyph_name[0]: % glyph_name may contain digits
   scantokens("path " & genericize(str glyph_stored.glyph_name) & "[]");
   scantokens("pair " & genericize(str hstem_stored.glyph_name) & "[]");
   scantokens("path " & genericize(str hstem_stored_segms.glyph_name) & "[]");
   scantokens("pair " & genericize(str vstem_stored.glyph_name) & "[]");
   scantokens("path " & genericize(str vstem_stored_segms.glyph_name) & "[]");
  fi
  glyph_stored.glyph_name.num:=0;
  hstem_stored.glyph_name.num:=0; vstem_stored.glyph_name.num:=0;
 fi
 scantokens extra_beginglyph;
enddef;

picture endglyph_picture;
def endglyph =
 scantokens extra_endglyph;
 % usually, |currentpicture=nullpicture|, but if not (i.e., some
 % extra objects have been drawn), the picture must be shifted:
 endglyph_picture:=currentpicture shifted (glyph_shift,0);
 currentpicture:=nullpicture;
 if known glyph_axis: % actually, used only with stored chars
  glyph_axis.glyph_name:=glyph_axis;
 fi
% fix char dimensions and write them to TFM and/or |dim_file|
% independently of |glyph_usage| (|dim_file|)
% fix_tfm_data(glyph_urx+glyph_shift, glyph_ury);
 if glyph_usage mod store = 1: % utilizing
    write_special "HSBW * " & decimal(glyph_width);
    write_special "BEGINCHAR";
    ship_glyphs;
  endfig;
 else:
  endgroup; % ends figure without shipping it out
 fi
enddef;
let endglyph_=endglyph;
string extra_beginglyph, extra_endglyph; extra_beginglyph=extra_endglyph="";

% Additional macros
vardef fix_name_list text t =
 string name_list[]; numeric name_list.num; name_list.num:=0;
 save , ; let , = fix_name_list_; fix_name_list_ t
enddef;
def fix_name_list_ suffix name =
 ; % important semicolon!
 if str name<>"": fix_name_list_s_ name else: fix_name_list_e_ "_" & fi
enddef;
def fix_name_list_s_ suffix s_name = fix_name_list_e_ (str s_name) enddef;
def fix_name_list_e_ expr e_name = % name is expected to be of the string type
 name_list[incr name_list.num]=e_name
enddef;

def introduce suffix name =
 if str name="": introduce_
 elseif (substring (0,1) of str name)<>"_": introduce_ name
 else: introduce__ name fi
enddef;
def introduce_ expr name = % name is expected to be a string expression
 introduce__ scantokens("_" & name)
enddef;
vardef introduce__@#(expr usage, slanting)(text stencil) =
 if (unknown process_selected or known process_selected@#)
  and known usage and unknown ignore_selected@#:
  glyph_usage@#:=usage; % |ignore=whatever|, |process=0|, |utilize=1|, |store=2|
  if unknown glyph_ps_name@#: % set default:
   assign_name @# (substring (1,infinity) of (str @#));
  fi
  glyph_slanting@#:=slanting; % ignore |slant_ang| if |0|; use |slant_ang| otherwise
  % |stencil| can be either string (recommended) or suffix (with default
  % extension |".eps"| -- obsolete), hence some trickery below
  save r_; string r_;
  for i_:=stencil: if string i_: r_:=i_; fi endfor
  if unknown r_:
   forsuffixes i_:=stencil: r_:= str i_; endfor
   if r_<>"": r_:=r_ & ".eps"; fi
  fi
  if r_<>"":
   if not string glyph_stencil@#:
    scantokens("string " & genericize(str glyph_stencil@#));
   fi
   glyph_stencil@# = r_;
  fi
 fi
enddef;

vardef assign_name@#(expr ps_name) =
 if not string glyph_ps_name @#:
  scantokens("string " & genericize(str glyph_ps_name@#));
 fi
 glyph_ps_name@#:=ps_name;
enddef;

def standard_introduce(expr name) =
 introduce name (utilize+store)(1)();
enddef;

vardef name_to_code(text name) =
 save res_, name_; string name_;
 name_:=substring (1,infinity) of str uni_name(name); res_=-1;
 for i:=0 upto 255: % 1-to-1 coding presumed
  if known code_to_name_[i]: if code_to_name_[i]=name_: res_:=i; fi fi
  exitif res_>-1;
 endfor
 res_
enddef;
def encode(text name)(expr glyph_code)=
 if (glyph_code<0) or (glyph_code>255):
  errhelp "The code must be within the range 0..255.";
  errmessage "MT1: improper code " & decimal(glyph_code) &
   " (`encode' ignored)";
 elseif known code_to_name_[glyph_code]:
  errhelp "A given code can be assigned only to one name (obviously).";
  errmessage "MT1: repeated code for " & code_to_name_[glyph_code] &
    " (" & decimal(glyph_code) & "; `encode' ignored)";
 else:
  code_to_name_[glyph_code]:=substring (1,infinity) of str uni_name(name);
 fi
enddef;
string code_to_name_[\\];

vardef name_used(text name) =
 save res_, name_; boolean res_; string name_;
 name_:=substring (1,infinity) of str uni_name(name); res_:=false;
 for i:=1 upto max_name_used: res_:=(name_used_[i]=name_); exitif res_; endfor
 res_
enddef;
def mark_name_used(text name)=
 name_used_[incr max_name_used]:=substring (1,infinity) of str uni_name(name);
enddef;
string name_used_[\\]; newinternal max_name_used;

vardef string_date =
 if day<10: "0" & fi decimal(day) & "." &
 if month<10: "0" & fi decimal(month) & "." &
 decimal(year)
enddef;

def set_pfi (suffix kind) (expr val) =
 if known val:
  if (numeric val) or (string val) or (boolean val):
   if (numeric val) and (not numeric pf_info_set.kind):
    scantokens ("numeric " & genericize(str pf_info_set.kind));
   elseif (string val) and (not string pf_info_set.kind):
    scantokens ("string " & genericize(str pf_info_set.kind));
   elseif (boolean val) and (not boolean pf_info_set.kind):
    scantokens ("boolean " & genericize(str pf_info_set.kind));
   fi
   pf_info_set.kind:=val;
    write str kind & " : " &
     if string val: val
     elseif numeric val: decimal(val)
     elseif boolean val: if val: "true" else: "false" fi
     fi
     to pfi_file;
  else:
   errhelp "Proceed, I'll just ignore the setting.";
   errmessage "MT1: pf_info keys can only be of numeric, string " &
    "and boolean type";
  fi
 fi
enddef;

def pf_info_version expr v = set_pfi(VERSION,v); enddef;

def pf_info_creationdate text t =
 begingroup
  save k_; k_:=0;
  for t_:=t: k_:=k_+1; set_pfi(CREATION_DATE, t_); exitif k_=1; endfor
  if k_=0: set_pfi(CREATION_DATE, string_date); fi
 endgroup
enddef;

def pf_info_fontname text t =
 begingroup
  save k_; k_:=0;
  for t_:=t: k_:=k_+1;
   if k_=1: set_pfi(FONT_NAME, t_); fi
   if k_=2: set_pfi(FULL_NAME, t_); fi
   exitif k_=2;
  endfor
  if k_=1: set_pfi(FULL_NAME, pf_info_set.FONT_NAME); fi
 endgroup
enddef;

def pf_info_author expr v = set_pfi(AUTHOR,v); enddef;
% There is `much ado about nothing,' i.e., about the sign of descender:
% in a PFB file in an `ADL' comment, descender is positive, while in an AFM
% in a `Descender' comment -- negative; we will distinguish between
% the two, the more so as `ADL' comment is not mentioned in
% in the Adobe documentation {\it Adobe Type~1 Font Format}.

def pf_info_ascender expr v = ascender:=v; set_pfi(ASCENDER,v); enddef;
def pf_info_descender expr v = descender:=v; set_pfi(DESCENDER,v); enddef;

def pf_info_adl text t =
 begingroup
  save k_; k_:=0;
  for t_:=t: k_:=k_+1;
   if (k_=1) and known t_: adl_ascender:=t_; set_pfi(ADL_ASCENDER,t_); fi
   if (k_=2) and known t_: adl_descender:=t_; set_pfi(ADL_DESCENDER,t_); fi
   if (k_=3) and known t_: adl_lineskip:=t_; set_pfi(ADL_LINESKIP,t_); fi
   exitif k_=3;
  endfor
 endgroup
enddef;

def pf_info_underline text t =
 begingroup
  save k_; k_:=0;
  for t_:=t: k_:=k_+1;
   if k_=1: set_pfi(UNDERLINE_POSITION,t_); fi
   if k_=2: set_pfi(UNDERLINE_THICKNESS,t_); fi
   exitif k_=2;
  endfor
 endgroup
enddef;

def pf_info_pfm text t =
% parameters: name, bold (0 or 1), italic (0 or 1), char set;
% each of them can be either known or unknown or "*" (which means unknown);
% the last parameter can be either numeric or string representation of
% a valid Perl numeric value (e.g., "0xFF" means 255).
 begingroup
  save k_; k_:=0;
  for t_:=t: k_:=k_+1;
   if (k_=1) and known t_: set_pfi(PFM_NAME,t_); fi
   if (k_=2) and known t_: set_pfi(PFM_BOLD,t_); fi
   if (k_=3) and known t_: set_pfi(PFM_ITALIC,t_); fi
   if (k_=4) and known t_: set_pfi(PFM_CHARSET,t_); fi
   exitif k_=4;
  endfor
 endgroup
enddef;

def pf_info_fixedpitch expr v = set_pfi(FIXED_PITCH,v); enddef;
def pf_info_capheight expr v = uc_height:=v; set_pfi(CAPHEIGHT,v); enddef;
def pf_info_weight expr v = set_pfi(WEIGHT,v); enddef;
def pf_info_stdvstem expr v = set_pfi(STDVW,v); enddef;
def pf_info_stdhstem expr v = set_pfi(STDHW,v); enddef;
def pf_info_forcebold expr v = set_pfi(FORCE_BOLD,v); enddef;

% TeX-related font info (fontdimens and headerbytes):
def pf_info_fontdimen text t = % exceptionally, TFM units expected
 begingroup
  save i_, k_, b_; boolean b_;
  k_:=0;
  if true for t_:=t: hide(k_:=k_+1) and known t_ endfor and (k_<=3):
   k_:=0;
   for t_:=t: k_:=k_+1;
    if k_=1:
     i_:=t_;
     % |b| means ``we are ready to override (possibly) the previous value
     % of a font parameter unless we are inside |complete_tfm_info| and
     % then we want to set only a `virgin' value.''
     b_:=unknown completing_tfm_info or unknown pf_info_set.FONT_DIMEN[i_];
    fi
    if b_ and (k_=2): set_pfi(FONT_DIMEN[i_],t_); fontdimen i_: t_; fi
    if b_ and (k_=3): set_pfi(DIMEN_NAME[i_],t_); fi
   endfor
   if b_ and (k_=2): set_pfi(DIMEN_NAME[i_],"(unknown fontdimen name)"); fi
  else:
   errhelp "Proceed, I'll just ignore TFM fontdimen settings.";
   errmessage "MT1: invalid TFM fontdimen data";
  fi
 endgroup
enddef;
def pf_info_headerbyte text t =
 begingroup
  save i_, k_; k_:=0;
  if true for t_:=t: hide(k_:=k_+1) and known t_ endfor and (k_=2):
   k_:=0;
   for t_:=t: k_:=k_+1;
    if k_=1: i_:=t_; fi
    if k_=2:
     set_pfi(HEADER_BYTE[i_],if numeric t_: decimal(t_) else: t_ fi);
     if i_=9: % encoding scheme, e.g., |"TEX TEXT"|
      headerbyte 9: BCPL_string(t_,40); fi
     if i_=49: % font family, e.g., |"CMR"|
      headerbyte 49: BCPL_string(t_,20); fi
     if i_=72: % family member number, which should be between 0 and 255
      headerbyte 72: t_; fi
    fi
   endfor
  else:
   errhelp "Proceed, I'll just ignore TFM headerbyte settings.";
   errmessage "MT1: invalid TFM headerbyte data";
  fi
 endgroup
enddef;
def pf_info_designsize expr v = % |designsize| is special
 designsize:=v; set_pfi(DESIGN_SIZE,decimal(v) & " (in points)");
enddef;
def pf_info_italicangle expr v =
 begingroup
  save tfm_units; vardef tfm_units(text x) = c enddef;
  slang:=v; set_pfi(ITALIC_ANGLE,-v);
  pf_info_fontdimen 1, if known slant: slant else: tand(slang) fi, "(slant)";
 endgroup
enddef;
def pf_info_space text t = % three in one
 begingroup
  save k_; k_:=0;
  for t_:=t: k_:=k_+1;
   if (designsize<>0) and known t_:
    if k_=1:
     space:=t_; pf_info_fontdimen 2, tfm_units(space), "(space)";
    elseif k_=2:
     space_stretch:=t_; pf_info_fontdimen 3, tfm_units(space_stretch),
      "(space stretch)";
    elseif k_=3:
     space_shrink:=t_; pf_info_fontdimen 4, tfm_units(space_shrink),
       "(space shrink)";
    fi
   fi
   exitif k_=3;
  endfor
 endgroup
enddef;
def pf_info_normal_space text t =
 begingroup
  save k_; k_:=0;
  if true for t_:=t: hide(k_:=k_+1) endfor and (k_<=2):
   k_:=0;
   for t_:=t: k_:=k_+1;
    if (k_=1) and known t_: space:=t_; fi
    if (k_=2) and known t_: % |t_| is expected to be in TFM units
     pf_info_fontdimen 2, t_, "(space)";
    fi
   endfor
   if (k_=1) and (designsize<>0) and known space:
    pf_info_fontdimen 2, tfm_units(space), "(space)";
   fi
  fi
 endgroup
enddef;
def pf_info_space_stretch text t =
 begingroup
  save k_; k_:=0;
  if true for t_:=t: hide(k_:=k_+1) endfor and (k_<=2):
   k_:=0;
   for t_:=t: k_:=k_+1;
    if (k_=1) and known t_: space_stretch:=t_; fi
    if (k_=2) and known t_: % |t_| is expected to be in TFM units
     pf_info_fontdimen 3, t_, "(space stretch)";
    fi
   endfor
   if (k_=1) and (designsize<>0) and known space_stretch:
    pf_info_fontdimen 3, tfm_units(space_stretch), "(space stretch)";
   fi
  fi
 endgroup
enddef;
def pf_info_space_shrink text t =
 begingroup
  save k_; k_:=0;
  if true for t_:=t: hide(k_:=k_+1) endfor and (k_<=2):
   k_:=0;
   for t_:=t: k_:=k_+1;
    if (k_=1) and known t_: space_shrink:=t_; fi
    if (k_=2) and known t_: % |t_| is expected to be in TFM units
     pf_info_fontdimen 4, t_, "(space shrink)";
    fi
   endfor
   if (k_=1) and (designsize<>0) and known space_shrink:
    pf_info_fontdimen 4, tfm_units(space_shrink), "(space shrink)";
   fi
  fi
 endgroup
enddef;
def pf_info_xheight text t =
 begingroup
  save k_; k_:=0;
  if true for t_:=t: hide(k_:=k_+1) endfor and (k_<=2):
   k_:=0;
   for t_:=t: k_:=k_+1;
    if (k_=1) and known t_: lc_height:=t_; set_pfi(XHEIGHT, t_); fi
    if (k_=2) and known t_: % |t_| is expected to be in TFM units
     pf_info_fontdimen 5, t_, "(xheight)";
    fi
   endfor
   if (k_=1) and (designsize<>0) and known lc_height:
    pf_info_fontdimen 5, tfm_units(lc_height), "(xheight)";
   fi
  fi
 endgroup
enddef;
def pf_info_quad text t =
 begingroup
  save k_; k_:=0;
  if true for t_:=t: hide(k_:=k_+1) endfor and (k_<=2):
   k_:=0;
   for t_:=t: k_:=k_+1;
    if (k_=1) and known t_: quad:=t_; fi
    if (k_=2) and known t_: % |t_| is expected to be in TFM units
     pf_info_fontdimen 6, t_, "(quad)";
    fi
   endfor
   if (k_=1) and (designsize<>0) and known quad:
    pf_info_fontdimen 6, tfm_units(quad), "(quad)";
   fi
  fi
 endgroup
enddef;
def pf_info_extra_space text t =
 begingroup
  save k_; k_:=0;
  if true for t_:=t: hide(k_:=k_+1) endfor and (k_<=2):
   k_:=0;
   for t_:=t: k_:=k_+1;
    if (k_=1) and known t_: extra_space:=t_; fi
    if (k_=2) and known t_: % |t_| is expected to be in TFM units
     pf_info_fontdimen 7, t_, "(extra space)";
    fi
   endfor
   if (k_=1) and (designsize<>0) and known extra_space:
    pf_info_fontdimen 7, tfm_units(extra_space), "(extra space)";
   fi
  fi
 endgroup
enddef;
def pf_info_encoding text t =
 begingroup
  save k_; k_:=0;
  for t_:=t: k_:=k_+1;
   if (k_=1) and known t_: if t_<>"": set_pfi(ENCODING_SCHEME, t_); fi fi
   if (k_=2) and known t_: if t_<>"": pf_info_headerbyte 9, t_; fi fi
   if (k_=3) and known t_: if t_<>"": set_pfi(ENCODING_NAME, t_); fi fi
   exitif k_=3;
  endfor
  if (k_=1) and known pf_info_set.ENCODING_SCHEME % upward compatibility
   and unknown pf_info_set.HEADER_BYTE9:
   pf_info_headerbyte 9, pf_info_set.ENCODING_SCHEME;
  fi
 endgroup
enddef;
def pf_info_familyname text t =
 begingroup
  save k_; k_:=0;
  for t_:=t: k_:=k_+1;
   if k_=1: set_pfi(FAMILY_NAME, t_); fi
   if k_=2: pf_info_headerbyte 49, t_; fi
   exitif k_=2;
  endfor
  if k_=1: pf_info_headerbyte 49, pf_info_set.FAMILY_NAME; fi
 endgroup
enddef;

% bluezz forever...
newinternal blue_fuzz, blue_scale, blue_shift;
blue_fuzz:=0; % Adobe Type 1 Font Format, p. 41
blue_scale:=0.0454545;
blue_shift:=7;

% it is advisable to avoid typso whenever possible:
def show_compose  expr x = show_compose_  :=x; enddef;
def show_fills    expr x = show_fills_    :=x; enddef;
def show_strokes  expr x = show_strokes_  :=x; enddef;
def show_paths    expr x = show_paths_    :=x; enddef;
def show_labels   expr x = show_labels_   :=x; enddef;
def show_boxes    expr x = show_boxes_    :=x; enddef;
def show_stems    expr x = show_stems_    :=x; enddef;
def show_stencils expr x = show_stencils_ :=x; enddef;

string extra_beginfont, extra_endfont; extra_beginfont=extra_endfont="";

def beginfont =
 min_glyph_code=max_glyph_code=0;
 complete_param_setting;
 scantokens extra_beginfont;
enddef;

def complete_param_setting =
 if designsize=0: designsize:=10; fi
 if unknown space: space:=333; fi
 if unknown space_stretch: space_stretch:=round(1/2space); fi
 if unknown space_shrink: space_shrink:=round(1/3space); fi
 if unknown extra_space: extra_space:=round(1/3space); fi
 if unknown quad: quad:=1000; fi
 if unknown slang:
  if known slant: % compatibility with the Old Tradition...
   slang:=angle(1, slant);
  else: slang:=0; fi
 fi
 if unknown uc_height: uc_height:=750; fi
 if unknown lc_height: lc_height:=400; fi
 if unknown italic_shift: italic_shift:=-40; fi % used to be |-100|
 if unknown depth: depth:=-250; fi
 if unknown ascender: ascender:=uc_height; fi
 if unknown descender: descender:=depth; fi
 if unknown adl_ascender: adl_ascender:=uc_height; fi
 if unknown adl_descender: adl_descender:=-depth; fi
 if unknown adl_lineskip: adl_lineskip:=0; fi
 if unknown top_line: top_line:=adl_ascender+1/2adl_lineskip; fi
 if unknown bot_line: bot_line:=-(adl_descender+1/2adl_lineskip); fi
 if unknown math_axis: math_axis:=250; fi
 if unknown math_rule: math_rule:=40; fi
 begingroup
  save rth_, pt_, subs_, desc_depth_, fig_height_, asc_height_;
  rth_:=math_rule; pt_:=100;
  % math symbol font parameters (defaults excerpted from cmsy10)
  subs_:=7/10;
  desc_depth_:=70/36pt_; fig_height_:=232/36pt_; asc_height_:=250/36pt_;
  if unknown num_one:
   num_one:=math_axis+3.51rth_+54/36pt_+subs_*desc_depth_; fi
  if unknown num_two:   num_two:=math_axis+1.51rth_+30/36pt_; fi
  if unknown num_three: num_three:=math_axis+1.51rth_+48/36pt_; fi
  if unknown denom_one:
   denom_one:=3.51rth_+subs_*fig_height_+124/36pt_-math_axis; fi
  if unknown denom_two:
   denom_two:=1.51rth_+subs_*fig_height_+30/36pt_-math_axis; fi
  if unknown sup_one:   sup_one:=8.99pt_-subs_*asc_height_; fi
  if unknown sup_two:   sup_two:=8.49pt_-subs_*asc_height_; fi
  if unknown sup_three: sup_three:=104/36pt_; fi
  if unknown sub_one:   sub_one:=54/36pt_; fi
  if unknown sub_two:   sub_two:=-8.49pt_+2subs_*asc_height_+3.1rth_; fi
  if unknown sup_drop:  sup_drop:=subs_*asc_height_-36/36pt_; fi
  if unknown sub_drop:  sub_drop:=18/36pt_; fi
  if unknown delim_one: delim_one:=23.9pt_; fi
  if unknown delim_two: delim_two:=10.1pt_; fi
  % math extension font parameters (defaults excerpted from cmex10)
  if unknown big_op_spacing_one:   big_op_spacing_one:=40/36pt_; fi;
  if unknown big_op_spacing_two:   big_op_spacing_two:=60/36pt_; fi;
  if unknown big_op_spacing_three: big_op_spacing_three:=72/36pt_; fi;
  if unknown big_op_spacing_four:  big_op_spacing_four:=216/36pt_; fi;
  if unknown big_op_spacing_five:  big_op_spacing_five:=36/36pt_; fi;
 endgroup;
enddef;

def endfont =
 scantokens extra_endfont;
 complete_pf_info;
 complete_tfm_info;
 scantokens "end";
enddef;

def complete_pf_info =
 if unknown pf_info_set.DESIGN_SIZE: pf_info_designsize designsize; fi
 if unknown pf_info_set.VERSION: pf_info_version "0.000"; fi
 if unknown pf_info_set.AUTHOR: pf_info_author "Unknown"; fi
 if unknown pf_info_set.CREATION_DATE: pf_info_creationdate; fi
 if unknown pf_info_set.FAMILY_NAME: pf_info_familyname "Untitled"; fi
 if unknown pf_info_set.FONT_NAME: pf_info_fontname "Untitled"; fi
 if unknown pf_info_set.ASCENDER: pf_info_ascender ascender; fi
 if unknown pf_info_set.DESCENDER: pf_info_descender descender; fi
 if unknown pf_info_set.ADL_ASCENDER:
  pf_info_adl adl_ascender, whatever, whatever;
 fi
 if unknown pf_info_set.ADL_DESCENDER:
  pf_info_adl whatever, adl_descender, whatever;
 fi
 if unknown pf_info_set.ADL_LINESKIP:
  pf_info_adl whatever, whatever, adl_lineskip;
 fi
 if unknown pf_info_set.UNDERLINE_POSITION: pf_info_underline -200, whatever; fi
 if unknown pf_info_set.UNDERLINE_THICKNESS: pf_info_underline whatever, math_rule; fi
 if unknown pf_info_set.ITALIC_ANGLE: pf_info_italicangle slang; fi
 if unknown pf_info_set.FIXED_PITCH: pf_info_fixedpitch false; fi
 if unknown pf_info_set.CAPHEIGHT: pf_info_capheight uc_height; fi
 if unknown pf_info_set.XHEIGHT: pf_info_xheight lc_height; fi
 if unknown pf_info_set.WEIGHT: pf_info_weight "Normal"; fi
 if unknown pf_info_set.STDVW: fi % just ignore
 if unknown pf_info_set.STDHW: fi % just ignore
 if unknown pf_info_set.FORCE_BOLD: pf_info_forcebold false; fi
 if unknown pf_info_set.ENCODING_SCHEME:
  pf_info_encoding "FontSpecific", whatever;
 fi
 if unknown pf_info_set.HEADER_BYTE9:
  pf_info_encoding whatever, "UNSPECIFIED";
 fi
 if unknown pf_info_set.BLUE_VALUES: set_pfi(BLUE_VALUES, ""); fi
 if unknown pf_info_set.OTHER_BLUES: fi % just ignore
 if unknown pf_info_set.BLUE_FUZZ: set_pfi(BLUE_FUZZ, blue_fuzz); fi
 if unknown pf_info_set.BLUE_SCALE: set_pfi(BLUE_SCALE, blue_scale); fi
 if unknown pf_info_set.BLUE_SHIFT: set_pfi(BLUE_SHIFT, blue_shift); fi
 % for those who like smart (implicit) systems:
 if unknown no_implicit_spaces:
  if not name_used("space"):
   if unknown glyph_usage._space: introduce _space (utilize)(0)(); fi;
   if (name_to_code("space")<0) and (unknown code_to_name_32):
    encode("space") (32);
   fi
   beginglyph(_space) fix_hsbw(space,0,0); endglyph;
  fi
  if not name_used("nbspace"):
   if unknown glyph_usage._nbspace: introduce _nbspace (utilize)(0)(); fi;
   %
   beginglyph(_nbspace) fix_hsbw(space,0,0); endglyph; % normal space width
  fi
 fi
enddef;

def complete_tfm_info =
 % complete fontdimen info:
 % |designsize| is expected to be known
 % |slant| dimen has already been set; |xheight| dimen -- not necessarily,
 % but |pf_info_set.XHEIGHT| is known:
 completing_tfm_info:=1;
 pf_info_xheight whatever,
   if known lc_height#: lc_height# else: tfm_units(pf_info_set.XHEIGHT) fi;
 pf_info_normal_space space if known space#: , space# fi;
 pf_info_space_stretch space_stretch
  if known space_stretch#: , space_stretch# fi;
 pf_info_space_shrink space_shrink if known space_shrink#: , space_shrink# fi;
 pf_info_quad quad if known quad#: , quad# fi;
 pf_info_extra_space extra_space if known extra_space#: , extra_space# fi;
  font_math_rule math_rule;
  font_math_axis math_axis;
 % complete header info:
 pf_info_headerbyte 72, max(0, 254 - round 2designsize);
 completing_tfm_info:=whatever;
enddef;

def BCPL_string(expr s,n)= % string |s| becomes an |n|-byte BCPL string
 for l:=if length(s)>=n: n-1 else: length(s) fi: l
  for k:=1 upto l: , substring (k-1,k) of s endfor
  for k:=l+2 upto n: , 0 endfor endfor
enddef;

% The Old Tradition...
def font_size expr x = designsize:=x enddef;
def font_slant expr x = fontdimen 1: x enddef;
def font_normal_space expr x = fontdimen 2: x enddef;
def font_normal_stretch expr x = fontdimen 3: x enddef;
def font_normal_shrink expr x = fontdimen 4: x enddef;
def font_x_height expr x = fontdimen 5: x enddef;
def font_quad expr x = fontdimen 6: x enddef;
def font_extra_space expr x = fontdimen 7: x enddef;

% A New Tradition...
def def_font_param (suffix param_name)(expr param_num, param_desc) =
 def param_name text x =
  begingroup save #; % cf. the definition of |tfm_units|
   if (known x#) or ((designsize<>0) and known x):
    pf_info_fontdimen param_num, tfm_units(x), "(" & param_desc & ")";
   fi
  endgroup
 enddef;
enddef;

def_font_param (font_math_rule, 8, "math rule");
def_font_param (font_math_axis, 22, "math axis");
% symbol fonts
def_font_param (font_num_one,    8, "num1");
def_font_param (font_num_two,    9, "num2");
def_font_param (font_num_three, 10, "num3");
def_font_param (font_denom_one, 11, "denom1");
def_font_param (font_denom_two, 12, "denom2");
def_font_param (font_sup_one,   13, "sup1");
def_font_param (font_sup_two,   14, "sup2");
def_font_param (font_sup_three, 15, "sup3");
def_font_param (font_sub_one,   16, "sub1");
def_font_param (font_sub_two,   17, "sub2");
def_font_param (font_sup_drop,  18, "sup_drop");
def_font_param (font_sub_drop,  19, "sub_drop");
def_font_param (font_delim_one, 20, "delim1");
def_font_param (font_delim_two, 21, "delim2");
% extension fonts
def_font_param (font_big_op_spacing_one,    9, "big_op_spacing1");
def_font_param (font_big_op_spacing_two,   10, "big_op_spacing2");
def_font_param (font_big_op_spacing_three, 11, "big_op_spacing3");
def_font_param (font_big_op_spacing_four,  12, "big_op_spacing4");
def_font_param (font_big_op_spacing_five,  13, "big_op_spacing5");

endinput
