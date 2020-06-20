%
% Modified by Matthew Skala, March 2011, to make it MetaType1 compatible
%

% This is OCRB10x_mf   (negativ normal)
% Copyright 1995, 2010 Norbert Schwarz
% You may freely use, modify and/or distribute this file, without limitation.

input fntbase.mp;
% font_identifier:="OCRB10X";
font_size:=10pt#;
designsize:=10pt#;

%%%%%%%%%%%%%%%%%%%%%%%%% font parameters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sharpedges  := 0;          %  0 -> rounded edges   |  1 -> sharp edges
globalslant := 0;          %  slant factor (typical zero or 0.2)
lightfactor := 0;          %  (typical 0.6 for outlined characters)
negativ     := 0;          %  0 -> normal          |  1 -> white characters
                           %                               on dark background
extraline   := 0;          %  0 -> normal
%%%%%%% do the job %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

input msk-ocrbmac.mp;
input msk-ocrbdef.mp;

for kkk:=0 step 1 until 144 : makechar kkk; endfor

end.
