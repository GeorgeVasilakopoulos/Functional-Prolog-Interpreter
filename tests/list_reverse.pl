
reverse(List, Tsil) :- reverse_helper(List,empty,Tsil).

reverse_helper(empty,Accumulator,Accumulator).
reverse_helper(l(Head,Tail),Accumulator,Tsil) :- reverse_helper(Tail,l(Head,Accumulator),Tsil).

