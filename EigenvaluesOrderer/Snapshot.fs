namespace EigenvaluesOrderer

type Snaphot
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


