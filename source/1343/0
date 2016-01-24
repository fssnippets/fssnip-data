open System
type matrix(m:float list list) =
    ///Numero de filas de la matriz
    member this.rows = m.Length
    ///Numero de columnas de la matriz
    member this.cols = m.Head.Length
    //Elementos de la matriz
    member this.items = m        
    //Obtiene el valor de un elemento de la matriz
    member this.item i j = m.[i].[j]
    //Convierte a string la matriz        
    override this.ToString() = "\
" + (this.items |> List.fold (fun r s -> r + "[ " + (s |> List.fold (fun x y -> x + (sprintf "%*.01f "4 y)) "") + "]\
") "")

    (******Metodos para poder realizar operaciones de suma, resta y productos de matrices******)
    ///Operador para suma de matrices donde x y son del mismo tamaño 
    static member (+) (x:matrix, y:matrix) =
        matrix (List.map2 (fun x y-> List.map2(fun x y-> x + y) x y) x.items y.items)
    ///Operador para resta de matrices donde x y son del mismo tamaño
    static member (-) (x:matrix, y:matrix) =
        matrix (List.map2 (fun x y -> List.map2(fun x y-> x - y) x y) x.items y.items)
    ///Operador para multiplicacion de una matriz por un numero n
    static member (*) (n:_, x:matrix) = 
        matrix ( x.items |> List.map (fun x-> x |> List.map(fun x -> x * (float)n)))
    ///Operador para multiplicacion de matrices
    static member (*) (x:matrix, y:matrix) = 
        matrix([for k in 0..x.rows - 1 do 
                yield [for i in 0..y.cols - 1 do
                       yield List.sum [for j in 0..x.cols - 1 do
                                       yield (x.item k j) * (y.item j i)]]])

    ///Obtiene la inversa de una matriz cuadrada
    member this.inv() =
        //Generamos la matriz identidad
        let id = matrix [for i = 0 to this.rows - 1 do
                            yield [for j = 0 to this.cols - 1 do
                                    yield if j = i then 1.0 else 0.0]]  
        //Agregamos la matriz identidad a la derecha de nuestra matriz                              
        let mi = ref (matrix (List.map2 (fun x y -> List.append x y) this.items id.items))
        //Hacemos los calculos necesarios para pasar la matriz identidad a la izquierda
        for i = 0 to mi.Value.rows - 1 do
            //Se hace 1 los elementos que estan en diagonal
            mi := matrix (mi.Value.items |> List.mapi (fun ix x -> x |> List.map (fun y -> if ix = i then y / mi.Value.item i i else y)))            
            //Hacer 0 las columna i de los otros renglones
            mi := matrix (mi.Value.items |> List.mapi (fun ix x -> x |> List.mapi (fun j y -> if ix <> i then (mi.Value.item i j * - mi.Value.item ix i) + y  else y )))                    
        matrix [for i = 0 to this.rows - 1 do
                    yield [for j = this.rows  to mi.Value.cols - 1 do
                            yield mi.Value.item i j]]


        
//Ejemplo 
//Generamos una matriz
let a = matrix [[ 7.0; 1.0; 0.0]
                [ 6.0; 4.0; 2.0]
                [ 1.0; 1.0; 5.0]]

//Obtenemos la inversa de la matriz
let i = a.inv()

//Validamos que la matriz inversa sea correcta
//  Multiplicamos la matriz inversa por la matriz original y el resultado 
//  debe ser la matriz identidad                                 
let x = a * i