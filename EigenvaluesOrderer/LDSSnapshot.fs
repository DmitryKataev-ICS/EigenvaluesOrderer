namespace EigenvaluesOrderer

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
    let vdims = [reV.Length; reV.[0].Length; imV.Length; imV.[0].Length]
    do 
        if not(vdims.[0] = vdims.[2] && vdims.[1] = vdims.[3]) then 
            failwithf "V dimensions error: %ix%i & %ix%i" vdims.[0] vdims.[2] vdims.[1] vdims.[3]
    let _V = Array.zip reV imV
    let ddims = (reD.Length, imD.Length)
    do
        if not(fst ddims = snd ddims) then
            failwithf "D dimension error: %i & %i" (fst ddims) (snd ddims)
    let _D = Array.zip reD imD
    let bdims = (reB.Length, imB.Length)
    do
        if not(fst bdims = snd bdims) then
            failwithf "B dimension error: %i & %i" (fst bdims) (snd bdims)
    let _B = Array.zip reB imB
    let cdims = (reC.Length, imC.Length)
    do
        if not(fst cdims = snd cdims) then
            failwithf "C dimension error: %i & %i" (fst cdims) (snd cdims)
    let _C = Array.zip reC imC
    let _states = states
    let _ModesList =
        let _EV = 
            let vdd = (_V.Length, _D.Length)
            if not(fst vdd = snd vdd) then failwithf "V & D have different lengths: %i & %i" (fst vdd) (snd vdd)
            let bcd = (_B.Length, _C.Length)
            if not(fst bcd = snd bcd) then failwithf "B & C have different lengths: %i & %i" (fst bcd) (snd bcd)
            if not(fst vdd = fst bcd) then failwithf "V (or D) & B (or C) have different lengths: %i & %i" (fst vdd) (fst bcd)
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
        ModesList(fold2modes [] _EV)
    
    new (modeslist : ModesList, stateslist) =
        let (Vs, other) = modeslist.unfold2primitives()
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
        try
            LDSSnapshot([for i in 0..(_ModesList.Length - 1) -> _ModesList.[pairs2swap.[i] |> snd] ], _states)
        with
            | _ -> 
                failwith 
                    ("Failed to swap: init.length = " + init.ModesList.Length.ToString() + "; cur.length = " + _ModesList.Length.ToString() + "\n" +
                        (
                            pairs2swap 
                            |> List.map (fun ((a, b) : int*int) -> "("+a.ToString()+"; "+b.ToString()+")\n" )
                            |> List.reduce (+)))
