namespace EigenvaluesOrderer

//type MATLABInterface(init : LDSSnapshot) =
//    let _init = init
//    let _snapshots : LDSSnapshot list ref = ref []
//    member x.AddSnapshot ns =
//        _snapshots := ns :: !_snapshots
//    member x.Reorder() =
//        !_snapshots
//        |> List.rev
//        |> List.map
//            (fun s -> s.ReorderWith _init)
//        |> (:=) _snapshots
//    member x.Get id =
//        //LDSSnapshot.unfold2primitives (!_snapshots).[id].ModesList |> snd |> List.toArray
//        () |> _snapshots.Value.[id].ModesList.unfold2primitives |> snd |> List.toArray

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
//            match (!_keys, !_keys_full) with
//                | (a, b) when a.Length = 0 && b.Length = 0 -> failwith "_keys and _keys_full are not set!11 =(((( \n"
//                | (a, _) when a.Length = 0 -> failwith "_keys are not set! =(\n"
//                | (_, a) when a.Length = 0 -> failwith "_keys_full are not set! =( \n"
//                | _ -> ()
            let snapshot = Snapshot(reV, imV, reD, imD, reB, imB, reC, imC, states, !_keys, !_keys_full)
            _snapshots.[!_next_index] := Some(snapshot)
            _next_index := !_next_index + 1
    member x.GetOrdered id =
        _snapshots.[id].Value.Value.Unfold2Primitives()
        |> snd |> List.toArray
    member x.GetLog id = _snapshots.[id].Value.Value.Log