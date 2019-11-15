
newPackage(
        "quasisymmetric",
        Version => "1.0", 
        Date => "October 25, 2019",
        Authors => {{Name => "Jonathan Niño", 
                  Email => "ja.nino937@uniandes.edu.co", 
                  HomePage => "http://www.uniandes.edu.co"}},
        Headline => "Methods for calculating the supercovariant algebra and related objects.",
        DebuggingMode => false
        )


export{"strongCompositions"}
export{"setToComposition"}
export{"compositionToSet"}
export{"compositionsBelow"}


strongCompositions = method ()
strongCompositions(ZZ):= n -> (
    sets := subsets toList(1..(n-1));
    apply(sets, s -> setToComposition(s,n))
    )

setToComposition = method()
setToComposition(List,ZZ) := (s,n)-> (
    s = prepend(0,s);
    s = append(s,n);
    apply(#s-1,i-> s#(i+1)-s#i)    
)

setToComposition(Set,ZZ) := (s,n)-> (
     setToComposition (sort toList s,n)
)

compositionToSet = method()
compositionToSet(List) := (comp) -> (
    n := sum(comp);
      s := 0;
      compSet := for i to #comp-2 list s+comp#i  do (
	  s = s+comp#i
	  );
      compSet
      
    )

compositionsBelow = method()
compositionsBelow(List) := (comp) -> (
    n:= sum(comp);
    universe := set(toList (1..(n-1)));
    compSet := compositionToSet(comp);
    apply( subsets (universe - compSet),s -> ( setToComposition(universe-s,n)))
    )





beginDocumentation()


