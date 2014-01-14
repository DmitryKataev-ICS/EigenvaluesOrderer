namespace EigenvaluesOrderer

type Mode(ReIm : float * float, V : float array * float array, refB : float * float, refC : float * float) =
    static member closeEnough = 1.0E-10
    member x.Re = fst(ReIm)
    member x.Im = snd(ReIm)
    member x.V = V
    member x.refB = refB
    member x.refC = refC
    member x.isComplex = x.Im > Mode.closeEnough
    static member isConjugate (a : Mode) (b : Mode) = a.Re = b.Re && (a.Im + b.Im < Mode.closeEnough)
    //member x.buildPair = ((x.Re, x.Im), (x.Re, -x.Im))

type LDSSnapshot
    (
        reV : float array array,
        imV : float array array,
        reD : float array,
        imD : float array,
        reB : float array,
        imB : float array,
        reC : float array,
        imC : float array,
        states : string array) =
    let _V = Array.zip reV imV
    let _D = Array.zip reD imD
    let _B = Array.zip reB imB
    let _C = Array.zip reC imC
    let _states = states
    let _modes = 
        Array.map2
            (fun (riv, rid) (rib, ric) -> Mode(rid, riv, rib, ric))
            (Array.zip _V _D)
            (Array.zip _B _C)
    member x.States with get() = _states


    
