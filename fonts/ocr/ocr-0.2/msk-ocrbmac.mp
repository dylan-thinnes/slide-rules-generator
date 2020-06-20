%
% Modified by Matthew Skala, March 2011, to make it MetaType1 compatible
%

%%%%%%%%%%%%%%%%% Endehandlung nach Zeichenbestimmung %%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 1995, 2010 Norbert Schwarz
% You may freely use, modify, and/or distribute this file, without limitation.
%
%  Dies wird am Ende jeden Zeichens  v o r  "endchar ausgefuehrt.
%
%  Dort koennen globale Design-Entscheidungen durch evtl.
%  Mehrfachausgaben realisiert wdrden.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the next several routines are added by Matthew Skala

beginfont;

proofing:=0;

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
    save g,h,ng,mh,oldnib;
    path g,h,ng,oldnib;
    oldnib:=default_nib;
    mh:=if lightfactor>0: lightfactor else: 1 fi;
    default_nib:=default_nib scaled mh;
    if cycle dp:
      if sharpedges=0:
        ng:=(subpath (0.4,length(dp)) of dp)--(subpath (0,0.6) of dp);
        pen_stroke()(ng)(g);
        dangerousFill g;
      else:
        g:=sharp_stroke_cycle(dp,ocrb.penwidth*scalefak*mh/2);
        if (xpart ((urcorner g)-(llcorner g))>50)
          and (ypart ((urcorner g)-(llcorner g))>50):
          dangerousFill g;
        fi;
        g:=sharp_stroke_cycle(reverse dp,ocrb.penwidth*scalefak*mh/2);
        if (xpart ((urcorner g)-(llcorner g))>50)
          and (ypart ((urcorner g)-(llcorner g))>50):
          dangerousFill g;
        fi;
      fi;
    else:
      if sharpedges=0:
        pen_stroke()(dp)(g);
      else:
        g:=sharp_stroke_path(dp,ocrb.penwidth*scalefak*mh/2);
      fi;
      dangerousFill g;
    fi;
    default_nib:=oldnib;
  endgroup;
enddef;

def ocrdrawdot(expr pt) =
  dangerousFill (default_nib shifted pt);
enddef;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Okay, I just can't get pen_stroke working to my satisfaction in the
% "sharp corners" case.  Here's an all-new replacement for it.

vardef sharp_stroke_path(expr pathtostroke,strokeradius) =
  begingroup
    % set up variables
    save x,y,rval,sf;
    path rval;
    boolean sf;
    % compute first points
    numeric x[],y[];
    z0=point 0 of pathtostroke;
    z1=postcontrol 0 of pathtostroke;
    z2=strokeradius*(z1-z0)/abs(z0-z1);
    z3=z0-z2+(z2 rotated 90);
    z4=z0-z2+(z2 rotated -90);
    rval=z4--z3;
    % compute other points
    sf:=true;
    for i=0 upto length pathtostroke:
      numeric x[],y[];
      z0=point i of pathtostroke;
      if i=0: z0=0.5[z1,z2]; else:
        z1=precontrol i of pathtostroke; fi;
      if i=length pathtostroke: z0=0.5[z1,z2]; else:
        z2=postcontrol i of pathtostroke; fi;
      z3=(z0-z1)/abs(z0-z1);
      z4=(z2-z0)/abs(z2-z0);
      if (z3 dotprod z4)>0.95:
        % curve
        z5=strokeradius*(z3+z4)/2;
        z6=z0+(z5 rotated -90);
        z7=z0+(z5 rotated 90);
        rval:=z6{-z5}..if sf: {curl 1}rval{curl 1} else: rval fi..{z5}z7;
        sf:=false;
      else:
        % corner
        z6=z0+strokeradius*(z3 rotated -90)+whatever*z3;
        z6=z0+strokeradius*(z4 rotated -90)+whatever*z4;
        z7=z0+strokeradius*(z3 rotated 90)+whatever*z3;
        z7=z0+strokeradius*(z4 rotated 90)+whatever*z4;
        if (z3 dotprod z4)<-0.6:
          if (z3 dotprod (z4 rotated 90))<0:
            % cut off, turning to left
            z8=z0+strokeradius*(z3 rotated -90);
            z9=z0+strokeradius*(z4 rotated -90);
            if sf:
              rval:=z9--z8--rval;
            else:
              rval:=z9--z8..rval;
            fi;
            if (z3 dotprod z4)>-0.9:
              rval:=rval--z7;
            fi;
          else:
            % cut off, turning to right
            z8=z0+strokeradius*(z3 rotated 90);
            z9=z0+strokeradius*(z4 rotated 90);
            if (z3 dotprod z4)>-0.9:
              rval:=z6--rval;
            fi;
            if sf:
              rval:=rval--z8--z9;
            else:
              rval:=rval..z8--z9;
            fi;
          fi;
        else:
          rval:=z6{curl 1}..
            if sf: {curl 1}rval{curl 1} else: rval fi..
            {curl 1}z7;
        fi;
        sf:=true;
      fi;
    endfor;
    % compute last points
    numeric x[],y[];
    z3=point infinity of pathtostroke;
    z4=precontrol infinity of pathtostroke;
    z5=strokeradius*(z4-z3)/abs(z4-z3);
    z1=z3-z5+(z5 rotated 90);
    z2=z3-z5+(z5 rotated -90);
    rval:=z1{curl 1}..
      if sf: {curl 1}rval{curl 1} else: rval fi..
      {curl 1}z2--cycle;
    % return the result
    reverse rval
  endgroup
enddef;

vardef sharp_stroke_cycle(expr pathtostroke,strokeradius) =
  begingroup
    % set up variables
    save x,y,rval,sf,ssf;
    path rval;
    boolean sf,ssf;
    % figure out whether last point is sharp
    numeric x[],y[];
    z0=point 0 of pathtostroke;
    z1=postcontrol 0 of pathtostroke;
    z2=precontrol 0 of pathtostroke;
    z3=(z1-z0)/abs(z1-z0);
    z4=(z0-z2)/abs(z0-z2);
    sf:=(z3 dotprod z4)<0.95;
    ssf:=sf;
    % compute other points
    for i=0 upto (length pathtostroke)-1:
      numeric x[],y[];
      z0=point i of pathtostroke;
      z1=precontrol i of pathtostroke;
      z2=postcontrol i of pathtostroke;
      z3=(z0-z1)/abs(z0-z1);
      z4=(z2-z0)/abs(z2-z0);
      if (z3 dotprod z4)>0.95:
        % curve
        z5=strokeradius*(z3+z4)/2;
        z6=z0+(z5 rotated -90);
        z7=z0+(z5 rotated 90);
        if (i>0):
          rval:=if sf: rval{curl 1} else: rval fi..{z5}z7;
        else:
          rval:=z7;
        fi;
        sf:=false;
      else:
        % corner
        z6=z0+strokeradius*(z3 rotated -90)+whatever*z3;
        z6=z0+strokeradius*(z4 rotated -90)+whatever*z4;
        z7=z0+strokeradius*(z3 rotated 90)+whatever*z3;
        z7=z0+strokeradius*(z4 rotated 90)+whatever*z4;
        if (z3 dotprod z4)<-0.6:
          if (z3 dotprod (z4 rotated 90))<0:
            % cut off, turning to left
            z8=z0+strokeradius*(z3 rotated -90);
            z9=z0+strokeradius*(z4 rotated -90);
            if (z3 dotprod z4)>-0.9:
              if (i>0):
                rval:=if sf: rval{curl 1} else: rval fi..{curl 1}z7;
              else:
                rval:=z7;
              fi;
            fi;
          else:
            % cut off, turning to right
            z8=z0+strokeradius*(z3 rotated 90);
            z9=z0+strokeradius*(z4 rotated 90);
            if (i>0):
              rval:=if sf: rval{curl 1} else: rval fi..{curl 1}z8--z9;
            else:
              rval:=z8--z9;
            fi;
          fi;
        else:
          if (i>0):
            rval:=if sf: rval{curl 1} else: rval fi..{curl 1}z7;
          else:
            rval:=z7;
          fi;
        fi;
        sf:=true;
      fi;
    endfor;
    % compute last points
    rval:=rval if sf: {curl 1} fi .. if ssf: {curl 1} fi cycle;
    if ssf:
      rval:=(point 0 of rval)--(subpath (1,length(rval)-1) of rval)--cycle;
    else:
      rval:=(point 0 of rval)..(subpath (1,length(rval)-1) of rval)..cycle;
    fi;
    % return the result
    reverse rval
  endgroup
enddef;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% beginning of code by Norbert Schwarz (though some modifications were
% applied to this too)

path penxy;
picture bild; picture bildb;
path negpath;

def dofinishing =
%    prule;                  % Rule in xheight-- Hoehe
    for j:=1 step 1 until subchar:
      firstpath:= firstpathcount[j];
      lastpath := lastpathcount[j];
%     message "path "&decimal j ;
%     message "   from "&decimal firstpath;
%     message "   to "&decimal lastpath ;
      if j > 1 :
         penxy := default_nib;
         if known penrand : default_nib := default_nib scaled penrand; fi
         cullit;   undrawallpath;
%         if sharpedges>0: cullit; shutedges -1; fi
         default_nib :=penxy;
         cullit;
      fi
      drawallpath;
%     if globalslant>0 : "with slant"; fi
      if sharpedges>0  :
%          "with sharp edges";
%          shutedges 1;
      fi
%      if hohl>0 :
%         "'hohl'";
%          cullit;
%          default_nib:=fix_nib(hohl,hohl,0);
%          undrawallpath;
%          cullit;
%          if sharpedges>0  : shutedges -1 ; fi
%          cullit;
%      fi
      if extraline>0 :
%       " with extraline";
        default_nib:=fix_nib(extraline,extraline,0);
        drawallpath;
%        if sharpedges>0   : shutedges 1; fi
    fi
    endfor
    if negativ > 0:
%       cullit;
       bild:=currentpicture;
       clearit;
    negpath:=  (-0.5*SYMBWIDTH/scalefak ,-0.35maxy)--
               ( 0.5*SYMBWIDTH/scalefak ,-0.35maxy)--
               ( 0.5*SYMBWIDTH/scalefak , 1.2maxy)--
               (-0.5*SYMBWIDTH/scalefak , 1.2maxy)--cycle;
%              (-0.5*SYMBWIDTH/scalefak ,-0.35maxy)..cycle;
        negpath:=negpath shifted (shiftfak,0) scaled scalefak;
        if known glyph_llx:
          negpath:=(min(glyph_llx-10,xpart llcorner negpath),
                    min(glyph_lly-10,ypart llcorner negpath))--
                   (max(glyph_urx+10,xpart urcorner negpath),
                    min(glyph_lly-10,ypart llcorner negpath))--
                   (max(glyph_urx+10,xpart urcorner negpath),
                    max(glyph_ury+10,ypart urcorner negpath))--
                   (min(glyph_llx-10,xpart llcorner negpath),
                    max(glyph_ury+10,ypart urcorner negpath))--cycle;
        fi;
        dangerousFill negpath;
%       cullit;
       bildb := currentpicture;
       clearit;
%       currentpicture := bildb-bild;
%       cullit;
%      showit;
     fi
    clearallpath;
    enddef;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%def comment text x=
%      for k:=0 step 29 until (length x)+ 28 :
%       special " 9"&substring(k,k+29) of(x&"                              ") ;
%       numspecial 0;numspecial 0; endfor  enddef;

def corrwidth expr x=  charwd:=x/pt;
                        w:=x;
                        h:=w;
                        enddef;
def prule =
       proofrule((0,0.5capheight),(capheight,0.5capheight));
       proofrule((0,0.25capheight),(capheight,0.25capheight));
       proofrule((0,0.75capheight),(capheight,0.75capheight));
       proofrule((0.25capheight,0),(0.25capheight,capheight));
       proofrule((0.75capheight,0),(0.75capheight,capheight));
       proofrule((  0.5capheight,0),(0.5capheight,capheight));enddef;



sheight := 0.75 xheight;

vardef make@#=
   if (length str@#) = 1: charcode:=ASCII str @#; else: charcode:=0; fi
   "Generierung: "&str@#;
   encode(str@#) (charcode); standard_introduce(str@#);
   beginglyph(str@#);
     char@#;
   fix_hsbw(10pt#,0,0);
   endglyph;
   enddef;



path allpath[];

def clearallpath =
    subchar := 1;
    firstpathcount[subchar]:=0;
    lastpathcount[subchar]:=-1;
    pathcount:=-1;  enddef;

def nextsubchar =
    subchar := subchar+1;
    firstpathcount[subchar]:=pathcount+1;
    lastpathcount[subchar]:=pathcount;
    enddef;


clearallpath;
path shutpen;
path unitpath;



def shutedges expr www  =
    unitpath := (-0.5,-0.5)--(0.5,-0.5)--(0.5,0.5)--(-0.5,0.5)..cycle;
    shutwidth := abs pen_lft + pen_rt;
    for i:=firstpath step 1 until lastpath:
        if cycle allpath[i]:
           else:
             shutpen:= unitpath scaled shutwidth
                       rotated (angle(direction 0 of allpath[i]))
                       slanted  globalslant ;
             dangerousFill (shutpen scaled www
                shifted ((point 0 of allpath[i]) slanted globalslant));
             shutpen:= unitpath scaled shutwidth
                       rotated (angle (direction infinity of allpath[i]))
                       slanted  globalslant ;
             dangerousFill (shutpen scaled www
                shifted ((point infinity of allpath[i]) slanted globalslant));
           fi
     endfor
     enddef;

def savepath text p =   pathcount:=pathcount+1;
                        lastpathcount[subchar]:=pathcount;
                        allpath[pathcount]:=p;  enddef;

def drawallpath =
    if globalslant=0:
      for i:=firstpath step 1 until lastpath: ocrdraw(allpath[i]); endfor
      else:
      for i:=firstpath step 1 until lastpath:
              ocrdraw (allpath[i] slanted globalslant);
          endfor
      fi
  enddef;

def undrawallpath =
    if globalslant=0:
      for i:=firstpath step 1 until lastpath: ocrdraw(allpath[i]); endfor
      else:
      for i:=firstpath step 1 until lastpath:
              ocrdraw (allpath[i] slanted globalslant);
          endfor
      fi
  enddef;


%=====================================================================


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%$%%%%%
%%%%%%%%%%%%%% Makros und Konstanten zum Font  O C R B  %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% ==========

% mm := (1000u/10pt) * (72.27pt/1in) * (1in/25.4mm) * 1mm;
% small correction to make widths exactly 500 units
mm := (7227/25.4)*1.004*(10pt#/designsize);

designfaktor := designsize*0.1;

capheight := 12.275*mm*designfaktor;        % Hoehe von "E" unskaliert
xheight   := 0.5*capheight * designfaktor;  %
SYMBWIDTH := 2.54mm*designfaktor;           % 10 Zeichen je inch

scalefak  := mm*2.28/11.4*designfaktor;

% ================================

path o.s[][];

ocrb.penwidth     := 1.75;        % Strichstaerke
ocrb.width        := 7 + ocrb.penwidth;

path my_defaultpen;
my_defaultpen :=fullcircle scaled (ocrb.penwidth*scalefak);
maxy := 12.6 + 0.5ocrb.penwidth;

%shiftfak := 0.5maxy;
shiftfak := (0.5SYMBWIDTH)/scalefak;
path xpath;

%------- font dimensionen
 ptscale := 1/pt;

 font_x_height       (8.5+0.5ocrb.penwidth)*scalefak / pt * designfaktor;
 font_slant          globalslant ;
 font_normal_space   1.0 * SYMBWIDTH    * ptscale;
 font_normal_stretch 0.0 * SYMBWIDTH    * ptscale;
 font_normal_shrink  0.0 * SYMBWIDTH    * ptscale;
 font_quad           2.0 * SYMBWIDTH    * ptscale;
 font_extra_space    1.0 * SYMBWIDTH    * ptscale;

%============= Ligaturtabelle  fuer Umlaute ====================

ligtable "a" : oct"042" =: oct"200";
ligtable "o" : oct"042" =: oct"201";
ligtable "u" : oct"042" =: oct"202";
ligtable "A" : oct"042" =: oct"203";
ligtable "O" : oct"042" =: oct"204";
ligtable "U" : oct"042" =: oct"205";

%===============================================================


def makechar expr charnumber =
 if known o.i[charnumber] :
 encode("char"&decimal charnumber) (charnumber);
 standard_introduce("char"&decimal charnumber);
 beginglyph("char"&decimal charnumber);
 default_nib:=my_defaultpen;
   kx := 0;
   for i:=1 step 1 until o.i[charnumber]:
     if known o.s[charnumber][i] :
          xpath :=(o.s[charnumber][i] shifted (shiftfak,0) ) scaled scalefak;
          savepath xpath;
          for k:=0 step 1 until (length xpath):
              kx:=kx+1;
              z[kx] = point k of xpath;
          endfor
     fi
  endfor
 if proofing>0 :
   labels(0,1,2,3,4,5,6,7,8,9);
   labels(10,11,12,13,14,15,16,17,18,19);
   labels(20,21,22,23,24,25,26,27,28,29);
   labels(30,31,32,33,34,35,36,37,38,39);
 fi
 dofinishing;
%corrwidth capheight;
 corrwidth SYMBWIDTH;
% Tiefe korrigieren
 if known o.m[charnumber]:
   if (o.m[charnumber]-(0.5*ocrb.penwidth))< 0:
     chardp := ((o.m[charnumber]-(0.5*ocrb.penwidth)) * scalefak ) / pt;
     d :=round (chardp*pt);
   fi
 fi
   fix_hsbw(723,0,0);
   endglyph;
 else:
   message "unknown character referenced in makechar";
 fi
enddef;

body_height#:=11.5*scalefak;
asc_height#:= 11.5*scalefak;
cap_height#:= 11.5*scalefak;
fig_height#:= 12.6*scalefak;
x_height#:=    8.5*scalefak;
math_axis#:=   8.5*scalefak;

hohl := lightfactor * ocrb.penwidth * scalefak;   %   0<= lightfactor < 1
