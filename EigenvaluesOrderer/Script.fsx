// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

//#load "BasicTypes.fs"
//#load "ModesDictionary.fs"
//#load "Snapshot.fs"
//#load "Orderer.fs"
//open EigenvaluesOrderer

type EigenValue(ReIm : float * float, V : float array * float array, refB : float * float, refC : float * float) =
    static member closeEnough = 1.0E-10
    member x.Re = fst(ReIm)
    member x.Im = snd(ReIm)
    member x.V = V
    member x.refB = refB
    member x.refC = refC
    member x.isComplex = (abs x.Im) > EigenValue.closeEnough
    member x.getStringHash() = 
        let round2snd : float -> float = ((*) 100.0) >> System.Math.Round
        Array.map (fun (a,b) -> (round2snd a).ToString() + (round2snd a).ToString()) (Array.zip (fst V) (snd V))
        |> Array.reduce (+)
        |> (+) ((sign x.Im).ToString())
    static member isConjugate (a : EigenValue) (b : EigenValue) = a.Re = b.Re && (a.Im + b.Im < EigenValue.closeEnough)

type TmpCell(fullkey : (float * float) array) =
    let _keys_full = fullkey
    let _ev : EigenValue option ref = ref None
    member x.FullKey with get() = _keys_full
    //member x.IsEmpty = ref true
    member x.EV with get() = !_ev and set(newval) = _ev := newval

type EigenDict (keys : string list, keys_full : (float * float) array list, ev : EigenValue list) =
    do printfn "%i%i%i" keys.Length keys_full.Length ev.Length
    let fullkey_dist (a : (float * float) array) (b : (float * float) array) =
        Array.map2 (fun (r1,i1) (r2,i2) -> (abs (r1-r2)) + (abs (i1-i2))) a b |> Array.sum
    // cur - index and value of currently chosen mode for given key; src mode distances list
    let rec min_index (cur : int*float) (src : float list) (available : bool ref array) i =
        match src with
            | head :: tail -> 
                if head <= (snd cur) && (available.[i].Value) then 
                    min_index (i, head) tail available (i+1) 
                else min_index cur tail available (i+1)
            | [] -> fst cur
    let (_dict, _keys, _keys_full) =
        if keys.IsEmpty then
            printfn "empty keys dict"
            let loc_keys = List.map (fun (a : EigenValue) -> a.getStringHash()) ev
            (
                List.zip loc_keys ev |> dict,
                loc_keys,
                List.map (fun (a : EigenValue) -> a.V) ev)

        else
            printfn "some other dict"
            //let wip = List.zip keys (List.map (fun a -> TmpCell(a)) keys_full) |> List.toSeq |> dict
            let eax = List.map (fun a -> TmpCell(a)) keys_full
            let are_available = [|for i in 0..(eax.Length - 1) -> ref true|]
            let all_distances = // all_distaces.[i].[j] :> i - eax index, j - ev index
                List.map
                    (fun (tmpcell : TmpCell) -> 
                        List.map (fun (eig : EigenValue) -> fullkey_dist tmpcell.FullKey (Array.zip (fst eig.V) (snd eig.V))) ev)
                    eax
            List.iteri
                (
                    fun index (dist_vector : float list) -> // distances from current TmpCell to all subject eigenvalues
                        let id = Array.findIndex (fun a -> !a) are_available
                        //let emptiness_map = List.map (fun (a : TmpCell) -> a.IsEmpty) eax
                        let closest_id = min_index (id, dist_vector.[id]) dist_vector are_available 0
                        are_available.[closest_id]  := false
                        match eax.[index].EV with
                            | None -> printfn "eax.[%i].EV.Value is None" index
                            | _ -> printfn "eax.[%i].EV.Value is not None O_O" index
                        eax.[index].EV <- Some (ev.[closest_id])
                        printfn "put %i into %i" closest_id index
                        match eax.[index].EV with
                            | None -> printfn "eax.[%i].EV.Value is None" index
                            | Some(a) -> printfn "eax.[%i].EV.Value is not None" index)
                all_distances
            List.iteri
                (
                    fun id (a : TmpCell) ->
                        match a.EV with
                            | None -> printfn "%i is None" id
                            | Some(b) -> printfn "%i is Some" id) 
                eax
            (
                List.zip keys (List.map (fun (a : TmpCell) -> printfn "mapping..."; a.EV.Value) eax) |> dict,
                keys,
                List.map Array.unzip keys_full)
            
    member x.EigenValues with get() = _dict
    member x.Keys with get() = _keys
    member x.KeysFull with get() = _keys_full

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
        keys_full : (float * float) array list) =
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
    let _eigen_dict = EigenDict(keys, keys_full, _EV)
    member x.Keys with get() = _eigen_dict.Keys
    member x.KeysFull with get() = _eigen_dict.KeysFull
    member x.Unfold2Primitives() =
        let _all_ev = List.map (fun (a : string) -> _eigen_dict.EigenValues.[a]) _eigen_dict.Keys

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

type Orderer(length : int) =
    let _snapshots : Snapshot option ref array= [|for i in 1..length -> ref None|]
    let _next_index = ref 0
    let _keys : string list ref = ref []
    let _keys_full : (float * float) array list ref = ref []
    member x.AddSnapshot(in_reV : float[,], in_imV : float[,], reD, imD, reB, imB, reC, imC)= //, states) =
        let states : string[] = [||]
        let reV = [|for i in 0..(in_reV.GetLength(1) - 1) -> in_reV.[*, i]|]
        let imV = [|for i in 0..(in_imV.GetLength(1) - 1) -> in_imV.[*, i]|]
        if !_next_index = 0 then
            let snapshot = Snapshot(reV, imV, reD, imD, reB, imB, reC, imC, states, [], [])
            _keys := snapshot.Keys
            _keys_full := List.map (fun (a, b) -> Array.zip a b) snapshot.KeysFull
            _snapshots.[!_next_index] := Some(snapshot)
            _next_index := 1
        else
            match (!_keys, !_keys_full) with
                | (a, b) when a.Length = 0 && b.Length = 0 -> failwith "_keys and _keys_full are not set!11 =(((( \n"
                | (a, _) when a.Length = 0 -> failwith "_keys are not set! =(\n"
                | (_, a) when a.Length = 0 -> failwith "_keys_full are not set! =( \n"
                | _ -> ()
            let snapshot = Snapshot(reV, imV, reD, imD, reB, imB, reC, imC, states, !_keys, !_keys_full)
            _snapshots.[!_next_index] := Some(snapshot)
            _next_index := !_next_index + 1
    member x.GetOrdered id =
        _snapshots.[id].Value.Value.Unfold2Primitives()
        |> snd |> List.toArray



let orderer = Orderer(2)
orderer.AddSnapshot(
    Array2D.init 2 2 (fun a b -> (a+1) + 10*(b+1) |> float),
    Array2D.create 2 2 0.0,
    [|-0.5; -1.5|],
    [| -1.0; 1.0|],
    [| 1.0; 1.0|],
    [| 1.0; 1.0|],
    [| 1.0; 1.0|],
    [| 1.0; 1.0|])
orderer.AddSnapshot(
    Array2D.init 2 2 (fun a b -> (a+1) + 10*(b+1) |> float),
    Array2D.create 2 2 0.0,
    [|-0.5; -1.5|],
    [| -1.0; 1.0|],
    [| 1.0; 1.0|],
    [| 1.0; 1.0|],
    [| 1.0; 1.0|],
    [| 1.0; 1.0|])

// Define your library scripting code here

