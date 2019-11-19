
newPackage(
        "quasisymmetric",
        Version => "1.0", 
        Date => "October 25, 2019",
        Authors => {{Name => "Jonathan Niño", 
                  Email => "ja.nino937@uniandes.edu.co", 
                  HomePage => "http://www.uniandes.edu.co"}},
        Headline => "Methods for calculating the supercovariant algebra and related objects.",
        DebuggingMode => true
        )


export{"strongCompositions"}
export{"setToComposition"}
export{"compositionToSet"}
export{"compositionsBelow"}
export{"generateMonomial"}
export{"monomialQuasisymmetricPolynomial"}
export{"monomialQuasisymmetricPolynomials"}
export{"fundamentalQuasisymmetricPolynomial"}
export{"fundamentalQuasisymmetricPolynomials"}

export{"isDyckPath"}
export{"trimZeros"}
export{"transdiagonalPolynomial"}
export{"transdiagonalPolynomials"}

strongCompositions = method ()
strongCompositions(ZZ):= n -> (
    sets := {{}};
    if(n != 1) then sets = subsets toList(1..(n-1));
    apply(sets, s -> setToComposition(s,n))
    )

setToComposition = method()
setToComposition(List,ZZ) := (s,n)-> (
    ans:={};
    if(n !=0 ) then (
    	s = prepend(0,s);
    	s = append(s,n);
    	ans = apply(#s-1,i-> s#(i+1)-s#i)
	);
    ans
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

generateMonomial = method()
generateMonomial(List,List,Ring) := (ind,exp,R) -> (
    if (#ind != #exp ) then error "The number of indices and exponents provided does not match";
    p := product(0..(#ind-1), i-> R_(ind#i)^(exp#i) );
    if (p==1 ) then p = 1_R;
    p
    )


monomialQuasisymmetricPolynomial = method()
monomialQuasisymmetricPolynomial(List,PolynomialRing):= (comp,R)->
(
    indices := subsets(toList (0..numgens R -1),#comp);
    sum apply (indices,ind->(generateMonomial(ind,comp,R)))        
)


monomialQuasisymmetricPolynomials = method()
monomialQuasisymmetricPolynomials(ZZ,PolynomialRing):= (d,R)->
(
    comps := strongCompositions(d);
    hashTable apply (comps, comp-> comp => monomialQuasisymmetricPolynomial(comp,R))       
)

fundamentalQuasisymmetricPolynomial = method()
fundamentalQuasisymmetricPolynomial(List,PolynomialRing):= (comp,R) -> (
    compsBelow := compositionsBelow(comp);
    sum apply(compsBelow,compB-> monomialQuasisymmetricPolynomial(compB,R))
    )

fundamentalQuasisymmetricPolynomials = method()
fundamentalQuasisymmetricPolynomials(ZZ,PolynomialRing):= (d,R) -> (
    comps := strongCompositions(d);
    hashTable apply (comps, comp-> comp => fundamentalQuasisymmetricPolynomial(comp,R))
    )

isDyckPath = method()
isDyckPath(List) := (vect)-> (
    
    s := 0;
    i := 0;
    while i<#vect and s+vect#i <= i do (
	s = s+vect#i;
	i = i+1;
	);
    i == #vect
    )

trimZeros = method()
trimZeros List := vect -> (
    i:= #vect-1;
    while (i>=0) and vect#i == 0 do (
	vect = drop(vect,-1);
	i = i-1;
	);
    vect
    )

transdiagonalPolynomial = method()
transdiagonalPolynomial (List,PolynomialRing) := (vect,R) -> (
    ans := 0_R;
    vectOr := vect;
    if not isDyckPath vect then (
	vect = trimZeros vect;
	i := position(vect, j-> j == 0, Reverse => true);
	if(i === null) then(
	    ans=fundamentalQuasisymmetricPolynomial(vect,R);
	    print(vect,ans);
	    )
	else (
	    vect=drop(vect,{i,i});
	    ans = transdiagonalPolynomial(vect,R);
	    a := vect#i;
	    vect = drop(vect,{i,i});
	    vect = insert(i,a-1,vect);
	    
	    ans = ans-R_i*transdiagonalPolynomial(vect,R);
	    print(vect,i,R_i);
	    print(vectOr,ans);
	    ); 
	);
    ans 
    )


transdiagonalPolynomialBasis = method()
transdiagonalPolynomialBasis (List,PolynomialRing) := (vect,R) -> (
    ans := 0_R;
    vectOr := vect;
    if not isDyckPath vect then (
	vect = trimZeros vect;
	i := position(vect, j-> j == 0, Reverse => true);
	if(i === null) then(
	    ans=fundamentalQuasisymmetricPolynomial(vect,R);
	    print(vect,ans);
	    )
	else (
	    vect=drop(vect,{i,i});
	    ans = transdiagonalPolynomial(vect,R);
	    a := vect#i;
	    vect = drop(vect,{i,i});
	    vect = insert(i,a-1,vect);
	    
	    ans = ans-R_i*transdiagonalPolynomial(vect,R);
	    print(vect,i,R_i);
	    print(vectOr,ans);
	    ); 
	);
    ans 
    )

beginDocumentation()

end
loadPackage("quasisymmetric",Reload=> true)
R = QQ[x_1..x_4,MonomialOrder => Lex]
comp = {2,1}
monomialQuasisymmetricPolynomial(comp,R)
i = 3
monomialQuasisymmetricPolynomials(i,R)
fundamentalQuasisymmetricPolynomials(i,R)
transdiagonalPolynomial({1,1,0,1},R)

