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


luminance_chars : [Int]
--                  .   ,   -   ~    :   ;   =   !   *   #   $  @
luminance_chars = [46, 44, 45, 126, 58, 59, 61, 33, 42, 35, 36, 64]

-- Apparently >. <. >=. <=. don't work...
greaterThan : Float -> Float -> Bool
greaterThan x y =
    let xi = truncate (x *. 10000000000000000.0) in
    let yi = truncate (y *. 10000000000000000.0) in
    (xi - yi) > 0

greaterEqThan : Float -> Float -> Bool
greaterEqThan x y =
    let xi = truncate (x *. 10000000000000000.0) in
    let yi = truncate (y *. 10000000000000000.0) in
    (xi - yi) >= 0

smallerThan : Float -> Float -> Bool
smallerThan x y =
    let xi = truncate (x *. 10000000000000000.0) in
    let yi = truncate (y *. 10000000000000000.0) in
    (xi - yi) < 0

smallerEqThan : Float -> Float -> Bool
smallerEqThan x y =
    let xi = truncate (x *. 10000000000000000.0) in
    let yi = truncate (y *. 10000000000000000.0) in
    (xi - yi) <= 0


-- for loop 2
for_loop_2 : Float -> Float -> Float -> Float -> Float -> Float -> Matrix -> Matrix -> Float -> Float -> (Matrix, Matrix)
for_loop_2 cosA sinA cosB sinB costheta sintheta output zbuffer theta phi
    | greaterEqThan phi (2.0 *. pi) = (output, zbuffer)
    | otherwise =
        let cosphi = cos phi in
        let sinphi = sin phi in
        let circlex = r2 +. r1 *. costheta in
        let circley = r1 *. sintheta in
        let x = circlex *. (cosB *. cosphi +. sinA *. sinB *. sinphi) -. circley *. cosA *. cosB in
        let y = circlex *. (sinB *. cosphi -. sinA *. cosB *. sinphi) +. circley *. cosA *. cosB in
        let z = k2 +. cosA *. circlex *. sinphi +. circley *. sinA in
        let ooz = 1.0 /. z in
        let xp = round (screen_width /. 2.0 +. k1 *. ooz *. x) in
        let yp = round (screen_height /. 2.0 -. k1 *. ooz *. y) in
        let luminance = cosphi *. costheta *. sinB 
                        -. cosA *. costheta *. sinphi 
                        -. sinA *. sintheta 
                        +. cosB *. (cosA *. sintheta -. costheta *. sinA *. sinphi) in
        if (round luminance > 0 && (round (ooz*.100000000000.0)) > (mRead zbuffer (xp, yp))) then 
            let zbuffer' = mWrite zbuffer (xp, yp) (round (ooz*.100000000000.0)) in
            let luminance_index = round (luminance *. 8.0) in
            let output' = mWrite output (xp, yp) (elemAt luminance_chars luminance_index) in
            for_loop_2 cosA sinA cosB sinB costheta sintheta output' zbuffer' theta (phi +. phi_spacing)
        else
            for_loop_2 cosA sinA cosB sinB costheta sintheta output zbuffer theta (phi +. phi_spacing)

-- for loop 1
for_loop_1 : Float -> Float -> Float -> Float -> Matrix -> Matrix -> Float -> Matrix
for_loop_1 cosA sinA cosB sinB output zbuffer theta
    | greaterEqThan theta (2.0 *. pi) = output
    | otherwise = 
        let costheta = cos theta in
        let sintheta = sin theta in
        let (output', zbuffer') = for_loop_2 cosA sinA cosB sinB costheta sintheta output zbuffer theta 0.0 in
        for_loop_1 cosA sinA cosB sinB output' zbuffer' (theta +. theta_spacing)

render_frame : Float -> Float -> Matrix
render_frame a b =
    let cosA = cos a in
    let sinA = sin a in
    let cosB = cos b in
    let sinB = sin b in
    let output = mInitFill (round screen_width) (round screen_height) 32 in -- 32 is char ' '
    let zbuffer = mInit (round screen_width) (round screen_height) in
    for_loop_1 cosA sinA cosB sinB output zbuffer 0.0


main : ()
main = bufferPrint (render_frame 0.0 0.0); print @String "\0"