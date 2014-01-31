namespace EigenvaluesOrderer

type ModesList(modes : Mode list) =
    let _all_modes = modes
    member x.OscillatoryCount = (List.filter (fun (x : Mode) -> x.isPair) modes).Length
    member x.AperiodicCount = _all_modes.Length - x.OscillatoryCount
    member x.Count = x.OscillatoryCount + x.AperiodicCount
    member x.Modes with get() = _all_modes
    member x.ReorderWith (init : ModesList) =
        let swap_list (order : Mode list) (subj : Mode list) : (int*int) list * int list =
            let (nearest_order_modes, nearest_distances) = 
                List.map
                    (fun (m : Mode) -> (List.minBy (fun a -> m.V_diff a) order, List.map (fun a -> m.V_diff a) order))
                    subj
                |> List.unzip
            let nearest_order_modes_ids = List.map (fun m -> List.findIndex ((=) m) order) nearest_order_modes
            match order.Length - subj.Length with
                | 0 ->
                    (List.zip [0..(subj.Length - 1)] nearest_order_modes_ids, [])
                | diff when diff < 0 -> //order is shorter than subj
                    let rec separate outcasts remaining items2cast =
                        match items2cast with
                            | left when left <= 0 -> (remaining, outcasts)
                            | left when left > 0 -> 
                                let outcast_id = List.findIndex ((=) (List.max nearest_distances)) nearest_distances
                                let remaining_ids = List.filter (((=) outcast_id) >> not) nearest_order_modes_ids
                                separate (outcast_id :: outcasts) remaining_ids (items2cast - 1)
                            | _ -> failwith "failed to separate modes"
                    let (what2keep, what2cast) = separate [] [0..(subj.Length)] -diff
                    (
                        List.zip what2keep (List.map (fun id -> nearest_order_modes_ids.[id]) what2keep),
                        what2cast)
                | diff when diff > 0 -> //order is longer than subj
                    (
                        List.zip [0..(subj.Length - 1)] nearest_order_modes_ids, 
                        List.filter (fun id -> not (List.exists ((=) id) nearest_order_modes_ids)) [0..(order.Length - 1)])
                | _ -> failwith "failed to compare modes"
        if (x.AperiodicCount = init.AperiodicCount && x.OscillatoryCount = init.OscillatoryCount) then // this might be redundant
            let (pairs2swap, _) = swap_list init.Modes _all_modes
            ModesList([for i in 0..(x.Count - 1) -> _all_modes.[pairs2swap.[i] |> snd] ])
        else
            let (order_osci, order_aper) = List.partition (fun (m : Mode) -> m.isPair) init.Modes
            let (subj_osci, subj_aper) = List.partition (fun (m : Mode) -> m.isPair) _all_modes // nooooooooooo!11;
            let (osci, osci2cast) = swap_list order_osci subj_osci
            let (aper, aper2cast) = swap_list order_aper subj_aper
            let res_osci = [for i in 0..(osci.Length - 1) -> subj_osci.[osci.[i] |> snd] ]
            let res_aper = [for i in 0..(aper.Length - 1) -> subj_aper.[aper.[i] |> snd] ]
            let cast = 
                [for i in 0..(osci2cast.Length - 1) -> subj_osci.[osci2cast.[i]]] @
                [for i in 0..(aper2cast.Length - 1) -> subj_aper.[aper2cast.[i]]]
            ModesList(res_osci @ res_aper @ cast)
    member x.unfold2primitives() =
        let rec unfold2ev (eax : EigenValue list) (src : Mode list) =
            if src.IsEmpty then
                eax |> List.rev
            else
                unfold2ev ((src.Head.GetEV()) @ eax) src.Tail
        let args =
            _all_modes
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

