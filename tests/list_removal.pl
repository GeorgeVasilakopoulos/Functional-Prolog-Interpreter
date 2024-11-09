
remove_element(empty,Element,empty).
remove_element(l(Element,Tail),Element,Tail).
remove_element(l(Head,Tail),Element,l(Head,NewTail)) :- remove_element(Tail,Element,NewTail).

