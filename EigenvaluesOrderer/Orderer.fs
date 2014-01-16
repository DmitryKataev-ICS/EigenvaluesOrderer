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
    let _ModesList =
        let _EV = 
            Array.map2
                (fun (riv, rid) (rib, ric) -> EigenValue(rid, riv, rib, ric))
                (Array.zip _V _D)
                (Array.zip _B _C)
            |> Array.toList
        let rec fold2modes (eax : Mode list) (src : EigenValue list) =
            if src.IsEmpty then
                eax |> List.rev
            else
                if src.Head.isComplex then
                    //assuming MATLAB keeps complex-conjugate eigenvalues together (it does)
                    fold2modes (Mode(src.Head, Some src.Tail.Head) :: eax) src.Tail.Tail
                else
                    fold2modes (Mode(src.Head, None) :: eax) src.Tail
        fold2modes [] _EV
    static member unfold2primitives(modeslist : Mode list) =
        let rec unfold2ev (eax : EigenValue list) (src : Mode list) =
            if src.IsEmpty then
                eax |> List.rev
            else
                unfold2ev ((src.Head.GetEV()) @ eax) src.Tail
        let args =
            modeslist
            |> unfold2ev []
            |> List.fold
                (
                    fun (Vs, other) (ev : EigenValue) ->
                        (
                            [(fst ev.V) :: Vs.[0]; (snd ev.V) :: Vs.[1]],
                            [
                                ev.Re :: other.[0]; 
                                ev.Im :: other.[1]; 
                                (fst ev.refB) :: other.[2];
                                (snd ev.refB) :: other.[3];
                                (fst ev.refC) :: other.[4];
                                (snd ev.refC) :: other.[5]] ))
                ([ [[||]]; [[||]] ], [for i in 0..5 -> []])
        let Vs = args |> fst |> List.map (List.rev>>List.toArray)
        let other = args |> snd |> List.map (List.rev>>List.toArray)
        (Vs, other)

    new (modeslist : Mode list, stateslist) =
        let (Vs, other) = LDSSnapshot.unfold2primitives modeslist
        LDSSnapshot
            (
                Vs.[0],
                Vs.[1],
                other.[0],
                other.[1],
                other.[2],
                other.[3],
                other.[4],
                other.[5],
                stateslist)

    member x.States with get() = _states
    member x.ModesList with get() = _ModesList
    member x.ReorderWith (init : LDSSnapshot) =
        let pairs2swap =
            [
                for i in 0..(x.ModesList.Length - 1) ->
                    (
                        i,
                        List.findIndex
                            ((=) 
                                (List.minBy 
                                    (fun a -> x.ModesList.[i].V_diff a) 
                                    init.ModesList))
                            init.ModesList)]
        LDSSnapshot([for i in 0..(_ModesList.Length - 1) -> _ModesList.[pairs2swap.[i] |> snd] ], _states)

    type MATLABInterface(init : LDSSnapshot) =
        let _init = init
        let _snapshots : LDSSnapshot list ref = ref []
        member x.AddSnapshot ns =
            _snapshots := ns :: !_snapshots
        member x.Reorder() =
            !_snapshots
            |> List.rev
            |> List.map
                (fun s -> s.ReorderWith _init)
            |> (:=) _snapshots
        member x.Get id =
            LDSSnapshot.unfold2primitives (!_snapshots).[id].ModesList