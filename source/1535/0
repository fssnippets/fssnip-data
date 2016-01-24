type Turn = Left | Right | Straight

let grahamScan points=

    let findTurn (x1, y1) (x2, y2) (x3, y3) =
        let x = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)
        match sign(x) with
        | (-1) -> Right
        | 0 -> Straight
        | 1 -> Left

    let rec scan = function
        | (p1::p2::p3::points) ->
            let t = findTurn p1 p2 p3
            match t with
            | Right -> (p1::(scan (p3::points)))
            | _ -> (p1::(scan (p2::p3::points)))
        | points -> points

    let translate ((ox:double), (oy:double)) (x, y) = (x-ox, y-oy)
    let atan2' ((x:double), y) = atan2 y x
    let origin = List.minBy (fun (x, y) -> (y, x)) points
    scan (List.sortBy (translate(origin) >> atan2') points)