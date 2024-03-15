import ListUtils

-- Parameters
screen_width : Float
screen_height : Float
theta_spacing : Float
phi_spacing : Float
r1 : Float
r2 : Float
k1 : Float
k2 : Float

screen_width = 54.0
screen_height = 40.0
theta_spacing = 0.07
phi_spacing = 0.02
r1 = 1.0
r2 = 2.0
k2 = 5.0

k1 = screen_width *. k2 *. 3.0 /. (8.0 *. (r1 +. r2))


total_iterations : Int
total_iterations = round (2.0 *. pi /. theta_spacing) * round (2.0 *. pi /. phi_spacing)

luminance_chars : StringList
luminance_chars = SList "." (SList "," (SList "-" (SList "~" (SList ":" (SList ";" (SList "=" (SList "!" (SList "*" (SList "#" (SList "$" (SList "@" SNil)))))))))))

-- Apparently >. <. >=. <=. return a Float instead of a Int
greaterThan : Float -> Float -> Bool
greaterThan x y =
    let xi = truncate (x *. 10000.0) in
    let yi = truncate (y *. 10000.0) in
    (xi - yi) > 0

greaterEqThan : Float -> Float -> Bool
greaterEqThan x y =
    let xi = truncate (x *. 10000.0) in
    let yi = truncate (y *. 10000.0) in
    (xi - yi) >= 0

smallerThan : Float -> Float -> Bool
smallerThan x y =
    let xi = truncate (x *. 10000.0) in
    let yi = truncate (y *. 10000.0) in
    (xi - yi) < 0

smallerEqThan : Float -> Float -> Bool
smallerEqThan x y =
    let xi = truncate (x *. 10000.0) in
    let yi = truncate (y *. 10000.0) in
    (xi - yi) <= 0


-- for loop 2
for_loop_2 : Float -> Float -> Float -> Float -> Float -> Float -> StringMatrix -> FloatMatrix -> Float -> Float -> Int -> (StringMatrix, FloatMatrix, Int)
for_loop_2 cosA sinA cosB sinB costheta sintheta output zbuffer theta phi iter
    | greaterEqThan phi (2.0 *. pi) = (output, zbuffer, iter)
    | otherwise =
        smPrint @String "iter: " ; print @Int iter ;
        let cosphi = cos phi in
        let sinphi = sin phi in
        let circlex = r2 +. r1 *. costheta in
        let circley = r1 *. sintheta in
        let x = circlex *. (cosB *. cosphi +. sinA *. sinB *. sinphi) -. circley *. cosA *. cosB in
        let y = circlex *. (sinB *. cosphi -. sinA *. cosB *. sinphi) +. circley *. cosA *. cosB in
        let z = k2 +. cosA *. circlex *. sinphi +. circley *. sinA in
        let ooz = 1.0 /. z in
        let xp = truncate (screen_width /. 2.0 +. k1 *. ooz *. x) in
        let yp = truncate (screen_height /. 2.0 -. k1 *. ooz *. y) in
        let luminance = cosphi *. costheta *. sinB 
                        -. cosA *. costheta *. sinphi 
                        -. sinA *. sintheta 
                        +. cosB *. (cosA *. sintheta -. costheta *. sinA *. sinphi) in

        if (greaterThan luminance 0.0) && (greaterThan ooz (mfRead zbuffer (xp, yp))) then
            let zbuffer' = mfWrite zbuffer (xp, yp) ooz in
            let luminance_index = truncate (luminance *. 8.0) in
            let output' = msWrite output (xp, yp) (sElemAt luminance_chars luminance_index) in
            for_loop_2 cosA sinA cosB sinB costheta sintheta output' zbuffer' theta (phi +. phi_spacing) (iter + 1)
        else
            for_loop_2 cosA sinA cosB sinB costheta sintheta output zbuffer theta (phi +. phi_spacing) (iter + 1)

-- for loop 1
for_loop_1 : Float -> Float -> Float -> Float -> StringMatrix -> FloatMatrix -> Float -> Int -> (StringMatrix, Int)
for_loop_1 cosA sinA cosB sinB output zbuffer theta iter
    | greaterEqThan theta (2.0 *. pi) = (output, iter)
    | otherwise = 
        let costheta = cos theta in
        let sintheta = sin theta in
        let (output', zbuffer') = for_loop_2 cosA sinA cosB sinB costheta sintheta output zbuffer theta 0.0 iter in
        for_loop_1 cosA sinA cosB sinB output' zbuffer' (theta +. theta_spacing) iter

render_frame : Float -> Float -> (StringMatrix, Int)
render_frame a b =
    let cosA = cos a in
    let sinA = sin a in
    let cosB = cos b in
    let sinB = sin b in
    let output = msInitFill (round screen_width) (round screen_height) " " in
    let zbuffer = mfInit (round screen_width) (round screen_height) in
    for_loop_1 cosA sinA cosB sinB output zbuffer 0.0 0


main : ()
main = putStr "Expected iterations: " ; print @Int total_iterations ; let (output, n) = (render_frame 0.0 0.0) in sMatrixPrint output; print @String "\0"