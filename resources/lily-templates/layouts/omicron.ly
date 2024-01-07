\version "2.24.0"


%%%
halflongarrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(1.24 . 1.3) \overlay {
      \draw-line #'(0.01 . -3)
     % \draw-line #'(-0.3 . -0.7)
      \draw-line #'(0.3 . -0.7)
    }


longarrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(1.24 . 1.3) \overlay {
      \draw-line #'(0.01 . -3)
      \draw-line #'(-0.3 . -0.7)
      \draw-line #'(0.3 . -0.7)
    }

fulllongarrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(1.24 . 1.3) \overlay {
      \draw-line #'(0.01 . -3)
      %\draw-line #'(-0.3 . -0.7)
      %\draw-line #'(0.3 . -0.7)
      \arrow-head #Y #UP ##t	
    }
%%%

halfcarrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(0.32 . 2.3) \overlay {
      \draw-line #'(0.01 . -2)
     % \draw-line #'(-0.3 . -0.7)
      \draw-line #'(0.3 . -0.7)
    }

carrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(0.32 . 2.3) \overlay {
      \draw-line #'(0.01 . -2)
      \draw-line #'(-0.3 . -0.7)
      \draw-line #'(0.3 . -0.7)
    }
    
fullcarrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(0.32 . 2.3) \overlay {
      \draw-line #'(0.01 . -2)
      %\draw-line #'(-0.3 . -0.7)
      %\draw-line #'(0.3 . -0.7)
       \arrow-head #Y #UP ##t	
    }
%%%

halfsharparrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(0.78 . 2.5) \overlay {
      \draw-line #'(0.01 . -2)
     % \draw-line #'(-0.3 . -0.7)
      \draw-line #'(0.3 . -0.7)
    }

sharparrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(0.78 . 2.5) \overlay {
      \draw-line #'(0.01 . -2)
      \draw-line #'(-0.3 . -0.7)
      \draw-line #'(0.3 . -0.7)
    }

fullsharparrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(0.78 . 2.5) \overlay {
      \draw-line #'(0.01 . -2)
     % \draw-line #'(-0.3 . -0.7)
     % \draw-line #'(0.3 . -0.7)
        \arrow-head #Y #UP ##t		   
    }
%%%

halfrarrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(1.24 . 2.5) \overlay {
      \draw-line #'(0.01 . -2)
     % \draw-line #'(-0.3 . -0.7)
      \draw-line #'(0.3 . -0.7)
    }

rarrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(1.24 . 2.5) \overlay {
      \draw-line #'(0.01 . -2)
      \draw-line #'(-0.3 . -0.7)
      \draw-line #'(0.3 . -0.7)
    }

fullrarrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(1.24 . 2.5) \overlay {
      \draw-line #'(0.01 . -2)
      %\draw-line #'(-0.3 . -0.7)
      %\draw-line #'(0.3 . -0.7)
     \arrow-head #Y #UP ##t		  
    }
%%%

flarrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(0.8 . 2.5) \overlay {
      \draw-line #'(0.01 . -2)
 %     \draw-line #'(-0.3 . -0.7)
 %     \draw-line #'(0.3 . -0.7)
       \arrow-head #Y #UP ##t
    }


fcarrow =
  \markup \override #'(thickness . 1.45)
    \translate #'(0.32 . 2.3) \overlay {
      \draw-line #'(0.01 . -2)
 %     \draw-line #'(-0.3 . -0.7)
 %     \draw-line #'(0.3 . -0.7)
      \arrow-head #Y #UP ##t 
    }






% adapted from http://lsr.di.unimi.it/LSR/Item?id=784

% Define the alterations as fraction of the equal-tempered whole tone.
#(define-public SEVEN-E-SHARP  7/8)
#(define-public SHARP-RAISE    5/8)
#(define-public SHARP-LOWER    3/8)
#(define-public NATURAL-RAISE  1/8)
#(define-public NATURAL-LOWER -1/8)
#(define-public FLAT-RAISE    -3/8)
#(define-public FLAT-LOWER    -5/8)
#(define-public SEVEN-E-FLAT  -7/8)

#(define-public 1-16-RAISE  1/16)
#(define-public 3-16-RAISE  3/16)
#(define-public 5-16-RAISE  5/16)
#(define-public 7-16-RAISE  7/16)
#(define-public 9-16-RAISE  9/16)
#(define-public 11-16-RAISE 11/16)
#(define-public 13-16-RAISE 13/16)
#(define-public 15-16-RAISE 15/16)

% Note names can now be defined to represent these pitches in our
% Lilypond input.  We extend the list of Dutch note names:
arrowedPitchNames =  #`(
                   (ceses . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
                   (cesqq . ,(ly:make-pitch -1 0 SEVEN-E-FLAT))
                   (ceseh . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
                   (ceseq . ,(ly:make-pitch -1 0 FLAT-LOWER))
                   (ces   . ,(ly:make-pitch -1 0 FLAT))
                   (cesiq . ,(ly:make-pitch -1 0 FLAT-RAISE))
                   (ceh   . ,(ly:make-pitch -1 0 SEMI-FLAT))
                   (ceq   . ,(ly:make-pitch -1 0 NATURAL-LOWER))
                   (c     . ,(ly:make-pitch -1 0 NATURAL))
                   (cik   . ,(ly:make-pitch -1 0 1-16-RAISE))		
                   (ciq   . ,(ly:make-pitch -1 0 NATURAL-RAISE))
                   (ciqk  . ,(ly:make-pitch -1 0 3-16-RAISE))	
                   (cih   . ,(ly:make-pitch -1 0 SEMI-SHARP))
                   (cihk . ,(ly:make-pitch -1 0 5-16-RAISE))
                   (ciseq . ,(ly:make-pitch -1 0 SHARP-LOWER))
                   (ciseqk . ,(ly:make-pitch -1 0 7-16-RAISE))
                   (cis   . ,(ly:make-pitch -1 0 SHARP))
                   (cisik . ,(ly:make-pitch -1 0 9-16-RAISE))
                   (cisiq . ,(ly:make-pitch -1 0 SHARP-RAISE))
                   (cisiqk . ,(ly:make-pitch -1 0 11-16-RAISE))                   
                   (cisih . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
                   (cisihk . ,(ly:make-pitch -1 0 13-16-RAISE)) 
                   (cisqq . ,(ly:make-pitch -1 0 SEVEN-E-SHARP))
                   (cisiqqk . ,(ly:make-pitch -1 0 15-16-RAISE))                    
                   (cisis . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

                   (deses . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
                   (desqq . ,(ly:make-pitch -1 1 SEVEN-E-FLAT))
                   (deseh . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
                   (deseq . ,(ly:make-pitch -1 1 FLAT-LOWER))
                   (des   . ,(ly:make-pitch -1 1 FLAT))
                   (desiq . ,(ly:make-pitch -1 1 FLAT-RAISE))
                   (deh   . ,(ly:make-pitch -1 1 SEMI-FLAT))
                   (deq   . ,(ly:make-pitch -1 1 NATURAL-LOWER))
                   (d     . ,(ly:make-pitch -1 1 NATURAL))
                   (diq   . ,(ly:make-pitch -1 1 NATURAL-RAISE))
                   (dih   . ,(ly:make-pitch -1 1 SEMI-SHARP))
                   (diseq . ,(ly:make-pitch -1 1 SHARP-LOWER))
                   (dis   . ,(ly:make-pitch -1 1 SHARP))
                   (disiq . ,(ly:make-pitch -1 1 SHARP-RAISE))
                   (disih . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
                   (disqq . ,(ly:make-pitch -1 1 SEVEN-E-SHARP))
                   (disis . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

                   (eeses . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
                   (eesqq . ,(ly:make-pitch -1 2 SEVEN-E-FLAT))
                   (eeseh . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
                   (eeseq . ,(ly:make-pitch -1 2 FLAT-LOWER))
                   (ees   . ,(ly:make-pitch -1 2 FLAT))
                   (eesiq . ,(ly:make-pitch -1 2 FLAT-RAISE))
                   (eeh   . ,(ly:make-pitch -1 2 SEMI-FLAT))
                   (eeq   . ,(ly:make-pitch -1 2 NATURAL-LOWER))
                   (e     . ,(ly:make-pitch -1 2 NATURAL))
                   (eiq   . ,(ly:make-pitch -1 2 NATURAL-RAISE))
                   (eih   . ,(ly:make-pitch -1 2 SEMI-SHARP))
                   (eiseq . ,(ly:make-pitch -1 2 SHARP-LOWER))
                   (eis   . ,(ly:make-pitch -1 2 SHARP))
                   (eisiq . ,(ly:make-pitch -1 2 SHARP-RAISE))
                   (eisih . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
                   (eisqq . ,(ly:make-pitch -1 2 SEVEN-E-SHARP))
                   (eisis . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

                   (feses . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
                   (fesqq . ,(ly:make-pitch -1 3 SEVEN-E-FLAT))
                   (feseh . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
                   (feseq . ,(ly:make-pitch -1 3 FLAT-LOWER))
                   (fes   . ,(ly:make-pitch -1 3 FLAT))
                   (fesiq . ,(ly:make-pitch -1 3 FLAT-RAISE))
                   (feh   . ,(ly:make-pitch -1 3 SEMI-FLAT))
                   (feq   . ,(ly:make-pitch -1 3 NATURAL-LOWER))
                   (f     . ,(ly:make-pitch -1 3 NATURAL))
                   (fiq   . ,(ly:make-pitch -1 3 NATURAL-RAISE))
                   (fih   . ,(ly:make-pitch -1 3 SEMI-SHARP))
                   (fiseq . ,(ly:make-pitch -1 3 SHARP-LOWER))
                   (fis   . ,(ly:make-pitch -1 3 SHARP))
                   (fisiq . ,(ly:make-pitch -1 3 SHARP-RAISE))
                   (fisih . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
                   (fisqq . ,(ly:make-pitch -1 3 SEVEN-E-SHARP))
                   (fisis . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

                   (geses . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
                   (gesqq . ,(ly:make-pitch -1 4 SEVEN-E-FLAT))
                   (geseh . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
                   (geseq . ,(ly:make-pitch -1 4 FLAT-LOWER))
                   (ges   . ,(ly:make-pitch -1 4 FLAT))
                   (gesiq . ,(ly:make-pitch -1 4 FLAT-RAISE))
                   (geh   . ,(ly:make-pitch -1 4 SEMI-FLAT))
                   (geq   . ,(ly:make-pitch -1 4 NATURAL-LOWER))
                   (g     . ,(ly:make-pitch -1 4 NATURAL))
                   (giq   . ,(ly:make-pitch -1 4 NATURAL-RAISE))
                   (gih   . ,(ly:make-pitch -1 4 SEMI-SHARP))
                   (giseq . ,(ly:make-pitch -1 4 SHARP-LOWER))
                   (gis   . ,(ly:make-pitch -1 4 SHARP))
                   (gisiq . ,(ly:make-pitch -1 4 SHARP-RAISE))
                   (gisih . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
                   (gisqq . ,(ly:make-pitch -1 4 SEVEN-E-SHARP))
                   (gisis . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

                   (aeses . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
                   (aesqq . ,(ly:make-pitch -1 5 SEVEN-E-FLAT))
                   (aeseh . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
                   (aeseq . ,(ly:make-pitch -1 5 FLAT-LOWER))
                   (aes   . ,(ly:make-pitch -1 5 FLAT))
                   (aesiq . ,(ly:make-pitch -1 5 FLAT-RAISE))
                   (aeh   . ,(ly:make-pitch -1 5 SEMI-FLAT))
                   (aeq   . ,(ly:make-pitch -1 5 NATURAL-LOWER))
                   (a     . ,(ly:make-pitch -1 5 NATURAL))
                   (aiq   . ,(ly:make-pitch -1 5 NATURAL-RAISE))
                   (aih   . ,(ly:make-pitch -1 5 SEMI-SHARP))
                   (aiseq . ,(ly:make-pitch -1 5 SHARP-LOWER))
                   (ais   . ,(ly:make-pitch -1 5 SHARP))
                   (aisiq . ,(ly:make-pitch -1 5 SHARP-RAISE))
                   (aisih . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
                   (aisqq . ,(ly:make-pitch -1 5 SEVEN-E-SHARP))
                   (aisis . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

                   (beses . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
                   (besqq . ,(ly:make-pitch -1 6 SEVEN-E-FLAT))
                   (beseh . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
                   (beseq . ,(ly:make-pitch -1 6 FLAT-LOWER))
                   (bes   . ,(ly:make-pitch -1 6 FLAT))
                   (besiq . ,(ly:make-pitch -1 6 FLAT-RAISE))
                   (beh   . ,(ly:make-pitch -1 6 SEMI-FLAT))
                   (beq   . ,(ly:make-pitch -1 6 NATURAL-LOWER))
                   (b     . ,(ly:make-pitch -1 6 NATURAL))
                   (biq   . ,(ly:make-pitch -1 6 NATURAL-RAISE))
                   (bih   . ,(ly:make-pitch -1 6 SEMI-SHARP))
                   (biseq . ,(ly:make-pitch -1 6 SHARP-LOWER))
                   (bis   . ,(ly:make-pitch -1 6 SHARP))
                   (bisiq . ,(ly:make-pitch -1 6 SHARP-RAISE))
                   (bisih . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
                   (bisqq . ,(ly:make-pitch -1 6 SEVEN-E-SHARP))
                   (bisis . ,(ly:make-pitch -1 6 DOUBLE-SHARP)))
pitchnames = \arrowedPitchNames
#(ly:parser-set-note-names pitchnames)




% The symbols for each alteration
arrowGlyphs = #`(
        ( 1                     . "accidentals.doublesharp")
        (,15-16-RAISE		. "accidentals.sharp.slashslash.stemstemstem")
        (,SEVEN-E-SHARP         . "accidentals.sharp.slashslash.stemstemstem" ) 
        (,13-16-RAISE		. "accidentals.sharp.slashslash.stemstemstem")
        ( 3/4                   . "accidentals.sharp.slashslash.stemstemstem")
        (,11-16-RAISE		. "accidentals.sharp")
        (,SHARP-RAISE           . "accidentals.sharp")
        (,9-16-RAISE		. "accidentals.sharp")
        ( 1/2                   . "accidentals.sharp")
        (,7-16-RAISE		. "accidentals.sharp.slashslash.stem")
        (,SHARP-LOWER           . "accidentals.sharp.slashslash.stem")
        (,5-16-RAISE		. "accidentals.sharp.slashslash.stem")
        ( 1/4                   . "accidentals.sharp.slashslash.stem")
        (,3-16-RAISE		. "accidentals.natural")
        (,NATURAL-RAISE         . "accidentals.natural.arrowup")
        (,1-16-RAISE		. "accidentals.natural")	
        ( 0                     . "accidentals.natural")
        (,NATURAL-LOWER         . "accidentals.natural.arrowdown")
        (-1/4                   . "accidentals.mirroredflat")
        (,FLAT-RAISE            . "accidentals.flat.arrowup")
        (-1/2                   . "accidentals.flat")
        (,FLAT-LOWER            . "accidentals.flat.arrowdown")
        (-3/4                   . "accidentals.mirroredflat.flat")
        (,SEVEN-E-FLAT          . "accidentals.flatflat.slash")
        (-1                     . "accidentals.flatflat")
)




% The glyph-list needs to be loaded into each object that
%  draws accidentals.
\layout {
  \context {
    \Score
    
    \override Accidental.stencil =
      #(grob-transformer
        'stencil
        (lambda (grob orig)
          (cond 
           ((eqv? 1/16 (ly:grob-property grob 'alteration))
	    (ly:stencil-add (grob-interpret-markup grob halflongarrow)))
	    ((eqv? 3/16 (ly:grob-property grob 'alteration))
	    (ly:stencil-add (grob-interpret-markup grob fulllongarrow)))
	   ((eqv? 5/16 (ly:grob-property grob 'alteration))
	    (ly:stencil-add orig (grob-interpret-markup grob halfcarrow)))
	   ((eqv? 7/16 (ly:grob-property grob 'alteration))
	    (ly:stencil-add orig (grob-interpret-markup grob fullcarrow)))
	   ((eqv? 9/16 (ly:grob-property grob 'alteration))
	    (ly:stencil-add orig (grob-interpret-markup grob halfsharparrow)))
	   ((eqv? 11/16 (ly:grob-property grob 'alteration))
	    (ly:stencil-add orig (grob-interpret-markup grob fullsharparrow)))	
	   ((eqv? 13/16 (ly:grob-property grob 'alteration))
	    (ly:stencil-add orig (grob-interpret-markup grob halfrarrow)))	
	   ((eqv? 15/16 (ly:grob-property grob 'alteration))
	    (ly:stencil-add orig (grob-interpret-markup grob fullrarrow)))			
	   ((eqv? 7/8 (ly:grob-property grob 'alteration))
	    (ly:stencil-add orig (grob-interpret-markup grob rarrow)))		
	   ((eqv? 1/8 (ly:grob-property grob 'alteration))
	    (ly:stencil-add (grob-interpret-markup grob longarrow)))
	   ((eqv? 3/8 (ly:grob-property grob 'alteration))
	    (ly:stencil-add orig (grob-interpret-markup grob carrow)))
	   ((eqv? 5/8 (ly:grob-property grob 'alteration))
	    (ly:stencil-add orig (grob-interpret-markup grob sharparrow)))

	   (else orig))))	
    
    \override KeySignature.alteration-glyph-name-alist = \arrowGlyphs
    \override Accidental.alteration-glyph-name-alist = \arrowGlyphs
    \override AccidentalCautionary.alteration-glyph-name-alist = \arrowGlyphs
    \override TrillPitchAccidental.alteration-glyph-name-alist = \arrowGlyphs
    \override AmbitusAccidental.alteration-glyph-name-alist = \arrowGlyphs
  }
  \context {
    \Staff
    extraNatural = ##f % this is a workaround for bug #1701
  }
}
