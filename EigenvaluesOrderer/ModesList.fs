namespace EigenvaluesOrderer

type ModesList(modes : Mode list) =
    let _all_modes = modes
    member x.OscillatoryCount = (List.filter (fun (x : Mode) -> x.isPair) modes).Length
    member x.AperiodicCount = _all_modes.Length - x.OscillatoryCount
    member x.Count = x.OscillatoryCount + x.AperiodicCount
    member x.Modes with get() = _all_modes
    member x.ReorderWith (init : ModesList) =
        if (x.AperiodicCount = init.AperiodicCount && x.OscillatoryCount = init.OscillatoryCount) then // this might be redundant
            let pairs2swap =
                [
                    for i in 0..(x.Count - 1) ->
                        (
                            i,
                            List.findIndex
                                ((=) 
                                    (List.minBy 
                                        (fun a -> _all_modes.[i].V_diff a) 
                                        init.Modes))
                                init.Modes)]
            ModesList([for i in 0..(x.Count - 1) -> _all_modes.[pairs2swap.[i] |> snd] ])
        else
            //match x-min oscillatory modes
            let os2match_count = min x.OscillatoryCount init.OscillatoryCount
            //match x-min aperiodic modes
            let ap2match_count = min x.AperiodicCount init.AperiodicCount
            //handle the reset
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

