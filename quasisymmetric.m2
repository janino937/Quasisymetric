

subsets toList (1..2)

s   

strongCompositions = method ()
strongCompositions(ZZ):= n -> (
    sets = subsets toList(1..(n-1));
    apply(sets, s -> setToComposition(s,n))
    )

setToComposition = method()
setToComposition(List,ZZ) := (s,n)-> (
    s = prepend(0,s);
    s = append(s,n);
    apply(#s-1,i-> s#(i+1)-s#i)    
)

compositionToSet = method()
compositionToSet(List) := (comp)

compositionsBelow(List,ZZ) := (comp,n)
