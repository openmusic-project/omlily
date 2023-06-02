%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%omlily template%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%\midi {}
\layout {
	ragged-last = ##f
indent = 0.0

\context {\Voice
     \remove "Forbid_line_break_engraver"
     % allowBeamBreak = ##t
          }
\context {\Score          
	  %%Uncomment to remove empty-staves from parts.
	  %\override VerticalAxisGroup.remove-first = ##t
          %% FOR FORCING BREAKS
                \override Beam.breakable = ##t
                \override TieColumn.breakable = ##t
                \override TupletBracket.breakable = ##t
          %%   
		
          %% This is if you we use mensural type bars (uncomment next line) :
	  %% \override BarLine.transparent = ##t 
	  %% (So use StaffGroup instead  of ChoirStaff 
	  %% WARNING: this doesnt work with polyblockmeas)
	  %%
	  %% \override SeparationItem.padding = #2.5 %%ne marche pas avec les line breaks !!!!!
        \override TupletBracket.direction = #1
        \override TupletBracket.bracket-visibility = ##t
        \override TupletBracket.edge-text = 
	  #(cons 
	    (markup #:arrow-head X LEFT #f)
	    (markup #:arrow-head X RIGHT #f)) 
          %tupletFullLength = ##f
	  
	\override TupletBracket.full-length-to-extent = ##t
         %% \override Stem.stemlet-length = #0.75
         measureBarType=#""

\override TupletNumber.text = #tuplet-number::calc-fraction-text

          \override MetronomeMark.padding = #2.5
          
	   %\override SpacingSpanner #'uniform-stretching = ##t
	    %proportionalNotationDuration = #(ly:make-moment 1 7)
	   %\override SpacingSpanner #'strict-note-spacing = ##t
	  
          %% \override SpacingSpanner #'uniform-stretching = ##t
	  %% \override SpacingSpanner #'strict-note-spacing = ##t
	 
	  %% proportionalNotationDuration = #(ly:make-moment 1 64)
	  \override TimeSignature.break-visibility = #end-of-line-invisible
	  \override Beam.break-overshoot = #'(-0.5 . 1.0)
	  \override TupletBracket.break-overshoot = #'(-0.5 . 1.0)
	  \override TupletBracket.staff-padding = #1.5
          \override TupletBracket.padding = #2 
	  \override PaperColumn.used = ##t 
          \override DynamicLineSpanner.staff-padding = #3  %%add this by default Should I ??
  	  %%Moved to Staff context for poly(tempo)marking	
          \remove Mark_engraver 
          \remove Text_mark_engraver
	  \remove Staff_collecting_engraver
          
%\override Flag.stencil = #modern-straight-flag
%%For multitempi
\override RehearsalMark.self-alignment-X = #LEFT 


         }
\context {\Staff
          %%Uncomment to remove empty-staves from parts.
	  %\RemoveEmptyStaves 
	  %\override TupletNumber.font-size= #0
	  %\override TupletNumber.font-name=#"Nepomuk Bold Italic" 
          \override VerticalAxisGroup.minimum-Y-extent = #'(-8 . 8)
          \override TimeSignature.style = #'()
          \override NoteHead.style = #'baroque
	   \override DynamicLineSpanner.staff-padding = #3  %%add this by default Should I ??
	   \consists "Mark_engraver" %%%for the fermata on barline
	  \override Flag.stencil = #modern-straight-flag
           \override  VerticalAxisGroup.staff-staff-spacing =
#'((basic-distance . 15) (minimum-distance . 10) (padding . 5) (stretchability . 5)) 
     %     \consists "Timing_translator"
%	  \consists "Default_bar_line_engraver"
          \numericTimeSignature
          \consists Mark_engraver
	  \consists Text_mark_engraver
	  \consists Staff_collecting_engraver

}
}
