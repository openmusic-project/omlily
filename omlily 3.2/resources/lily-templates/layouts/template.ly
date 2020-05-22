%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%omlily template%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





\layout {
	ragged-last = ##f
indent = 0.0
\context {\Score 
         \override TupletBracket #'staff-padding = #1.5
         \override TupletBracket #'direction = #1
         \override TupletBracket #'bracket-visibility = ##t
          \override Stem #'stemlet-length = #0.75
	  \remove "Mark_engraver" %%%for the fermata on barline 
	   \override MetronomeMark #'padding = #2.5
	  %\remove "Timing_translator"
	  %\remove "Default_bar_line_engraver"

         }
\context {\Staff
         \numericTimeSignature
          \override NoteHead #'style = #'baroque
	   \override DynamicLineSpanner #'staff-padding = #3  %%add this by default Should I ??
\override Flag #'stencil = #modern-straight-flag
	   \consists "Mark_engraver" %%%for the fermata on barline
       %   \consists "Timing_translator"
	%  \consists "Default_bar_line_engraver"

}
}
