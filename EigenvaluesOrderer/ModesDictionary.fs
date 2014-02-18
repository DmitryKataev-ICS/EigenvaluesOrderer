﻿namespace EigenvaluesOrderer

type TmpCell(fullkey : (float * float) array) =
    member x.FullKey = fullkey
    member x.IsEmpty = ref true
    member x.EV : EigenValue option ref = ref None

type EigenDict (keys : string list, keys_full : (float * float) array list, ev : EigenValue list) =
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
    let _dict =
        if keys.IsEmpty then
            List.zip (List.map (fun (a : EigenValue) -> a.getStringHash()) ev) ev
            |> dict
        else
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
                        eax.[index].EV := Some ev.[closest_id])
                all_distances
            List.zip keys (List.map (fun (a : TmpCell) -> a.EV.Value.Value) eax)
            |> dict
            
    member x.EigenValues with get() = _dict