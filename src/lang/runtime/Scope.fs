module Nosh.lang.runtime.Scope

type Scope(_variables: Map<string, obj>, parent: Scope option) =

    let mutable _variables = _variables

    static member toMap (scope: Scope) : Map<string, obj> =
        let mutable map: Map<string, obj> = scope.variables

        match scope.parent with
        | Some(parent) ->
            let mutable parentMap = Scope.toMap parent

            for k in map.Keys do
                parentMap <- parentMap.Add (k, map[k])

            map <- parentMap

            ()
        | _ ->
            ()

        map

    member this.parent = parent
    member this.variables
        with get () = _variables
        and  set (value) = _variables <- value

    member this.get(key: string) =
        if parent.IsSome && not <| this.hasOwn key then
            parent.Value.get key
        else
            this.variables[key]

    member this.hasOwn(key: string) = this.variables.ContainsKey(key)

    member this.has(key: string) =
        if parent.IsSome && not <| this.hasOwn (key) then
            parent.Value.has (key)
        else
            this.hasOwn key

    member this.set (key: string) (value: obj) =
        if parent.IsSome && parent.Value.hasOwn key then
            parent.Value.set key value |> ignore
        else
            this.variables <- this.variables.Add(key, value)