namespace EigenvaluesOrderer

type Snapshot
    (
        reV : float array array,
        imV : float array array,
        reD : float array,
        imD : float array,
        reB : float array,
        imB : float array,
        reC : float array,
        imC : float array,
        states : string array,
        keys : string list,
        keys_full : (float * float) array list,
        keys_ev : (float * float) array) =
    //let vdims = [reV.Length; reV.[0].Length; imV.Length; imV.[0].Length]
    let _V = Array.zip reV imV
    //let ddims = (reD.Length, imD.Length)
    let _D = Array.zip reD imD
    //let bdims = (reB.Length, imB.Length)
    let _B = Array.zip reB imB
    //let cdims = (reC.Length, imC.Length)
    let _C = Array.zip reC imC
    let _states = states
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
    let _eigen_dict = EigenDict(keys, keys_full, keys_ev _EV)
    member x.Keys with get() = _eigen_dict.Keys
    member x.KeysFull with get() = _eigen_dict.KeysFull
    member x.Log with get() = _eigen_dict.Log
    member x.AllDistances with get() = _eigen_dict.AllDistaces
    member x.EigenValues 
        with get() = 
            _eigen_dict.Keys
            |> List.map (fun (a : string) -> _eigen_dict.EigenValues.[a]) 
            |> List.map (fun (a : EigenValue) -> (a.Re, a.Im))
            |> List.toArray
    member x.Unfold2Primitives() =
        let _all_ev = List.map (fun (a : string) -> _eigen_dict.EigenValues.[a]) _eigen_dict.Keys
        let rec unfold2ev (eax : EigenValue list) (src : Mode list) =
            if src.IsEmpty then
                eax |> List.rev
            else
                unfold2ev ((src.Head.GetEV()) @ eax) src.Tail
        let args =
            _all_ev
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


