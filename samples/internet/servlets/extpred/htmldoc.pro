%----------------------------------------------------------------------
% HTMLDOC - build an HTML document fragment
%

htmldoc(DOC) :-
   user_info(NAME, OS, DIR),
   stringlist_concat([$<p>Name: $, NAME, $</p><p>Operating System: $, OS, $</p><p>Directory: $, DIR, $</p>$], DOC).
