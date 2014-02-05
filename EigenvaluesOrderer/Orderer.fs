namespace EigenvaluesOrderer

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
        //LDSSnapshot.unfold2primitives (!_snapshots).[id].ModesList |> snd |> List.toArray
        () |> _snapshots.Value.[id].ModesList.unfold2primitives |> snd |> List.toArray