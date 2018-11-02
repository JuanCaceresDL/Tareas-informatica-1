module Main exposing (..)

import Canvas
import CanvasColor as Color exposing (Color)
import Html exposing (Html,div,button,text, input)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Browser

type Frac = Koch | Sierpinski | Nada

type alias Modelo = {fracta : Frac, reps : Int, px1 : Float, py1 : Float, px2 : Float, py2 : Float, px3 : Float, py3 : Float, re : String, col : Int, col1 : Int}


modeloInicial : Modelo
modeloInicial = {fracta = Nada , reps = 0, px1 = 0, py1 = 0, px2 = 0, py2 = 0, px3 = 0, py3 = 0, re = "no", col = 0, col1 = 255}

type Mensaje = Fracta Frac | Repe Int | Xx1 String | Yy1 String | Xx2 String | Yy2 String | Xx3 String | Yy3 String | Reiniciar



actualizador : Mensaje -> Modelo -> Modelo
actualizador mensaje modelo = case mensaje of 
    Fracta a -> {fracta = a, reps = 0, 
        px1 = 300,
        py1 = if a == Koch then 45 else if a == Sierpinski then 0 else 0,
        px2 = if a == Koch then 500 else if a == Sierpinski then 600 else 0,
        py2 = if a == Koch then 425 else if a == Sierpinski then 600 else 0,
        px3 = if a == Koch then 100 else if a == Sierpinski then 0 else 0,
        py3 = if a == Koch then 425 else if a == Sierpinski then 600 else 0, re = "no", col = modeloInicial.col, col1 = modeloInicial.col1}
    Repe a -> {fracta = modelo.fracta, reps = 
        if a == 1 then (modelo.reps) + 1 else 
        if modelo.reps == 0 then modelo.reps 
         else (modelo.reps) - 1, px1 = modelo.px1, py1 = modelo.py1, px2 = modelo.px2, py2 = modelo.py2, px3 = modelo.px3, py3 = modelo.py3, re = "no", 
         col = if a == 1 then (modelo.col) + 40 else if modelo.reps == 0 then modelo.col else (modelo.col) - 40,
         col1 = if a == 1 then 0 else if modelo.reps == 0 then modelo.col1 else 255} 
    Xx1 s -> {fracta = modelo.fracta, reps = modelo.reps, px1 = conv (String.toFloat s), py1 = modelo.py1, px2 = modelo.px2, py2 = modelo.py2, px3 = modelo.px3, py3 = modelo.py3, re = "no", col = modelo.col, col1 = modelo.col1}
    Yy1 s -> {fracta = modelo.fracta, reps = modelo.reps, px1 = modelo.px1, py1 = conv (String.toFloat s), px2 = modelo.px2, py2 = modelo.py2, px3 = modelo.px3, py3 = modelo.py3, re = "no", col = modelo.col, col1 = modelo.col1}
    Xx2 s -> {fracta = modelo.fracta, reps = modelo.reps, px1 = modelo.px1, py1 = modelo.py1, px2 = conv (String.toFloat s), py2 = modelo.py2, px3 = modelo.px3, py3 = modelo.py3, re = "no", col = modelo.col, col1 = modelo.col1}
    Yy2 s -> {fracta = modelo.fracta, reps = modelo.reps, px1 = modelo.px1, py1 = modelo.py1, px2 = modelo.px2, py2 = conv (String.toFloat s), px3 = modelo.px3, py3 = modelo.py3, re = "no", col = modelo.col, col1 = modelo.col1}
    Xx3 s -> {fracta = modelo.fracta, reps = modelo.reps, px1 = modelo.px1, py1 = modelo.py1, px2 = modelo.px2, py2 = modelo.py2, px3 = conv (String.toFloat s), py3 = modelo.py3, re = "no", col = modelo.col, col1 = modelo.col1}
    Yy3 s -> {fracta = modelo.fracta, reps = modelo.reps, px1 = modelo.px1, py1 = modelo.py1, px2 = modelo.px2, py2 = modelo.py2, px3 = modelo.px3, py3 = conv (String.toFloat s), re = "no", col = modelo.col, col1 = modelo.col1}
    Reiniciar -> {fracta = Nada, reps = 0, px1 = 0, py1 = 0, px2 = 0, py2 = 0, px3 = 0, py3 = 0, re = "si", col = modelo.col, col1 = modelo.col1}


conv : Maybe Float -> Float
conv n = case n of
    Just a -> a
    Nothing -> 0
-- Las coordenadas de cada una de las esquinas del
-- poligono que se dibujara
fractal : List (Float, Float)
fractal = [
    (300, 0),
    (600, 600),
    (0, 600)]

fractalp : List (Float, Float)
fractalp = [
    (300, 45),
    (500, 425),
    (100, 425),(300, 45),(300, 45)]

inicial = []
--sierpinski----------------------------------------------------------------------------------------------------------------------

pmedio : (Float, Float) -> (Float, Float) -> (Float, Float)
pmedio x y = case (x, y) of 
    ((x1, y1), (x2, y2)) -> (((x1 + x2) / 2), ((y1 + y2) / 2))

invtriangulo : List (Float, Float) -> List (Float, Float)
invtriangulo trian = case trian of 
    [] -> []
    x1::x2::x3::xs -> (pmedio x1 x2)::(pmedio x2 x3)::(pmedio x3 x1)::[]
    [_] -> []
    [_,_] -> []

t1 : List (Float, Float) -> List (Float, Float)
t1 l1 = case l1 of
    [] -> []
    x1::x2::x3::xs -> x1::(pmedio x1 x2)::(pmedio x3 x1)::[]
    [_] -> []
    [_,_] -> []

t2 : List (Float, Float) -> List (Float, Float)
t2 l1 = case l1 of
    [] -> []
    x1::x2::x3::xs -> x2::(pmedio x1 x2)::(pmedio x2 x3)::[]
    [_] -> []
    [_,_] -> []

t3 : List (Float, Float) -> List (Float, Float)
t3 l1 = case l1 of
    [] -> []
    x1::x2::x3::xs -> x3::(pmedio x3 x1)::(pmedio x2 x3)::[]
    [_] -> []
    [_,_] -> []

sierpinski : Int -> List (Float, Float) -> List (List (Float, Float))
sierpinski n l = case (n, l) of
    (0, []) -> []
    (0, x::xs) -> [x::xs]
    (ns, x::xs) -> (x::xs)::(invtriangulo (x::xs))::(sierpinski (ns-1) (t1 (x::xs))) ++ (sierpinski (ns-1) (t2 ((x::xs)))) ++ (sierpinski (ns-1) (t3 (x::xs)))
    (_, []) -> []

--sierpinski : Int -> List (List (Float, Float))
--sierpinski x = aux x fractal 


--snowflake------------------------------------------
p1 : (Float, Float) -> (Float, Float) -> (Float, Float)
p1 x y = case (x, y) of
    ((x1, y1), (x2, y2)) -> (((x1 + (1/2) * x2) / (1 + (1/2))), ((y1 + (1/2) * y2) / (1 + (1/2))))

p2 : (Float, Float) -> (Float, Float) -> (Float, Float)
p2 x y = case (x, y) of
    ((x1, y1), (x2, y2)) -> (((x1 + (2 * x2)) / (3)), ((y1 + (2 * y2)) / (3)))

matriz : (Float, Float) -> (Float, Float)
matriz x = case x of
    (x2, y2) -> (((x2 * (cos(degrees 60))) - y2 * (sin(degrees 60))), ((x2 * sin(degrees 60) + y2 * cos(degrees 60))))

sumar x y = case (x, y) of
    ((x1, y1), (x2, y2)) -> ((x1 + x2), (y1 + y2))

kochpm : (Float, Float) -> (Float, Float) -> (Float, Float)
kochpm x y = case (x, y) of
    ((x1, y1), (x2, y2)) -> sumar (matriz ((x1 - x2), (y1 - y2))) (x2, y2)

picos : List ( Float, Float ) -> List ( Float, Float )
picos lista = case lista of
    [] -> []
    x1::x2::xs -> x1::(p1 x1 x2)::(kochpm (p1 x1 x2) (p2 x1 x2))::(p2 x1 x2)::(picos (x2::xs))
    _ -> []

snowflake : Int -> List ( Float, Float ) -> List ( Float, Float )
snowflake n lista = if n == 0 then lista else snowflake (n - 1) (picos lista)

--snowflake : Int -> List ( Float, Float )
--snowflake n = kochaux n fractalp



-- Dada una lista de coordesnadas, esta funcion
-- genera los comandos necesarios para dibujar
-- las lineas que connectan dichas coordenadas
dibujar : List ( Float, Float ) -> Canvas.Commands -> Canvas.Commands
dibujar triangulo context =
    let
        acc (x,y) s = s |> Canvas.lineTo x y
    in
        case triangulo of
            (x0,y0)::xs ->
                List.foldl acc (context |> Canvas.moveTo x0 y0) ((x0,y0)::xs)
                |> Canvas.lineTo x0 y0
            _ -> context

dibujartriangulos : List (List ( Float, Float )) -> Canvas.Commands -> Canvas.Commands
dibujartriangulos triangulos context = case triangulos of
    [] -> context
    x::xs -> dibujar x (dibujartriangulos xs context)

-- Funcion que genera el html que corresponde al
-- poligono siendo dibujado

vista : Modelo -> Html Mensaje
vista modelo = div[style "background" "RGB(0,0,0)"]
    [div [] [text "NADA"],
    div[style "display" "flex", 
         style "justify-content" "center", 
         style "align-items" "center"]
    [
    let
        width = 600
        height = 600
        poligono = case (modelo.fracta) of 
            Koch -> dibujar (snowflake (modelo.reps) ([(modelo.px1, modelo.py1),(modelo.px2, modelo.py2),(modelo.px3, modelo.py3),(modelo.px1, modelo.py1),(modelo.px1, modelo.py1)]))
            Sierpinski -> dibujartriangulos (sierpinski (modelo.reps) ([(modelo.px1, modelo.py1),(modelo.px2, modelo.py2),(modelo.px3, modelo.py3)]))
            Nada -> dibujar inicial
        c1 = modelo.col
        c2 = modelo.col1
    in
        Canvas.element
            width
            height
            [ style "border" "5px solid red"]
            (
                Canvas.empty
                |> Canvas.beginPath
                |> Canvas.clearRect 0 0 width height
                |> Canvas.lineWidth 3
                |> Canvas.fillStyle (Color.rgba 0 0 0 0.6)
                |> Canvas.fillRect 0 0 width height
                |> Canvas.strokeStyle (Color.rgb 255 c2 c1)
                |> poligono
                |> Canvas.stroke
            )
   ], 
    div [style "display" "flex", style "justify-content" "Center", style "align-items" "Center"] [
        button [onClick (Fracta Koch), style "height" "90px" , style"width" "90px",style "background" "rgb(062,095,138)",style "color" "RGB(255,255,255)"] [text "KOCH"],
        button [onClick (Fracta Sierpinski), style "height" "90px" , style"width" "90px",style "background" "rgb(062,095,138)",style "color" "RGB(255,255,255)" ] [text "SIERPINSKI"],
        text ".",
        button [onClick (Reiniciar), style "height" "90px" , style"width" "90px",style "background" "rgb(062,095,138)",style "color" "RGB(255,255,255)" ] [text "REINCICIAR"],
        text "......",
        button [onClick (Repe 0), style "height" "90px" , style"width" "90px",style "background" "RGB(036,231,017)",style "color" "RGB(0,0,0)"] [Html.text "<-"],
        button [onClick (Repe 1),style "height" "90px" , style"width" "90px",style "background" "RGB(036,231,017 )",style "color" "RGB(0,0,0)"] [Html.text "->"]],
    div [] [text "NADA"],
    div [style "display" "flex", style "justify-content" "center", style "align-items" "center", style "color" "RGB(255,255,255)"] [ 
        text "PUNTO 1. . . .X . .",
        input [if modelo.re == "si" then value "" else onInput (\px1 ->if px1 == "" then if modelo.fracta == Koch then Xx1 "300" else if modelo.fracta == Sierpinski then Xx1 "300" else Xx1 "0" else Xx1 px1)] [], 
        text ". . Y . .", 
        input [if modelo.re == "si" then value "" else onInput (\py1 ->if py1 == "" then if modelo.fracta == Koch then Yy1 "45" else if modelo.fracta == Sierpinski then Yy1 "0" else Yy1 "0" else Yy1 py1)] []],
    div [style "display" "flex", style "justify-content" "center", style "align-items" "center", style "color" "RGB(255,255,255)"] [ 
        text "PUNTO 2. . . .X . .",
        input [if modelo.re == "si" then value "" else onInput (\px2 ->if px2 == "" then if modelo.fracta == Koch then Xx2 "500" else if modelo.fracta == Sierpinski then Xx2 "600" else Xx2 "0" else Xx2 px2)] [], 
        text ". . Y . .", 
        input [if modelo.re == "si" then value "" else onInput (\py2 ->if py2 == "" then if modelo.fracta == Koch then Yy2 "425" else if modelo.fracta == Sierpinski then Yy2 "600" else Yy2 "0" else Yy2 py2)] []],
    div [style "display" "flex", style "justify-content" "center", style "align-items" "center", style "color" "RGB(255,255,255)"] [ 
        text "PUNTO 3. . . .X . .",
        input [if modelo.re == "si" then value "" else onInput (\px3 ->if px3 == "" then if modelo.fracta == Koch then Xx3 "100" else if modelo.fracta == Sierpinski then Xx3 "0" else Xx3 "0" else Xx3 px3)] [], 
        text ". . Y . .", 
        input [if modelo.re == "si" then value "" else onInput (\py3 ->if py3 == "" then if modelo.fracta == Koch then Yy3 "425" else if modelo.fracta == Sierpinski then Yy3 "600" else Yy3 "0" else Yy3 py3)] []]

    ]

main =
    Browser.sandbox
        { init = modeloInicial
        , view = vista
        , update = actualizador
        }