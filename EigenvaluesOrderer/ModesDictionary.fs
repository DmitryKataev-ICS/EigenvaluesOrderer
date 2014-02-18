namespace EigenvaluesOrderer

type TmpCell(fullkey : (float * float) array) =
    member x.FullKey = fullkey
    member x.IsEmpty = ref true
    member x.EV : EigenValue option ref = ref None

type EigenDict (keys : string list, keys_full : (float * float) array list, ev : EigenValue list) =
    let fullkey_dist (a : (float * float) array) (b : (float * float) array) =
        Array.map2 (fun (r1,i1) (r2,i2) -> (abs (r1-r2)) + (abs (i1-i2))) a b |> Array.sum
    let rec min_index (cur : int*float) (src : (float * bool) list) i =
        match src with
            | head :: tail -> 
                if (fst head) <= (snd cur) && (snd head) then 
                    min_index (i, fst head) tail (i+1) 
                else min_index cur tail (i+1)
            | [] -> fst cur
    let _dict =
        if keys.IsEmpty then
            List.zip (List.map (fun (a : EigenValue) -> a.getStringHash()) ev) ev
            |> dict
        else
            //let wip = List.zip keys (List.map (fun a -> TmpCell(a)) keys_full) |> List.toSeq |> dict
            let eax = List.map (fun a -> TmpCell(a)) keys_full
            let all_distances = // all_distaces.[i].[j] :> i - eax index, j - ev index
                List.map
                    (fun (tmpcell : TmpCell) -> 
                        List.map (fun (eig : EigenValue) -> fullkey_dist tmpcell.FullKey (Array.zip (fst eig.V) (snd eig.V))) ev)
                    eax
            List.iter
                (
                    fun (dist_vector : float list) -> // distances from current TmpCell to all subject eigenvalues
                        let id = List.findIndex (fun (a : TmpCell) -> a.IsEmpty) eax
                        let emptiness_map = List.map (fun (a : TmpCell) -> a.IsEmpty) eax
                        let closest_id = min_index (id, eax.[id]) 
                all_distances
            
    member x.EigenValues with get() = _dict
