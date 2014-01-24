namespace EigenvaluesOrderer

type EigenValue(ReIm : float * float, V : float array * float array, refB : float * float, refC : float * float) =
    static member closeEnough = 1.0E-10
    member x.Re = fst(ReIm)
    member x.Im = snd(ReIm)
    member x.V = V
    member x.refB = refB
    member x.refC = refC
    member x.isComplex = (abs x.Im) > EigenValue.closeEnough
    static member isConjugate (a : EigenValue) (b : EigenValue) = a.Re = b.Re && (a.Im + b.Im < EigenValue.closeEnough)

type Mode(a : EigenValue, b : EigenValue option) =
    let _a = a
    let _b = b
    member x.isPair = _a.isComplex
    member x.Re = _a.Re
    member x.ImAbs = abs _a.Im
    member x.V = Array.zip (fst _a.V) (snd _a.V)
    member x.GetEV () = if _a.isComplex then [_a; _b.Value] else [_a]
    member x.V_diff (a : Mode) = 
            Array.map2 (fun (r1,i1) (r2,i2) -> (abs (r1-r2)) + (abs (i1-i2))) x.V a.V |> Array.sum
    override x.Equals o = 
        match o with
            | :? Mode as b -> x.Re = b.Re && x.ImAbs = b.ImAbs
            | _ -> false
    override x.GetHashCode () = x.Re - x.ImAbs |> int
