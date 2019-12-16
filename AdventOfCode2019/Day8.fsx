open System.IO

type X = int
type Y = int
type Coordinate = X * Y

type Width = int
type Height = int
type Dimensions = Width * Height
type Color = int

type Layer = Map<Coordinate, Color>

type Image = {
    Dimension: Dimensions;
    Layers: Layer[];
}

type MaybeBuilder() =
    member this.Return(x) = Some x
    member this.Bind(m, f) = Option.bind f m
    member this.Zero() = None

let option = MaybeBuilder()

let parseInput (str:string) = 
    let result:int[] = Array.zeroCreate str.Length
    str |> Seq.map (
        function 
        | '0' -> Some 0 | '1' -> Some 1 | '2' -> Some 2 | '3' -> Some 3 | '4' -> Some 4 
        | '5' -> Some 5 | '6' -> Some 6 | '7' -> Some 7 | '8' -> Some 8 | '9' -> Some 9 
        | _ -> None
    )
    |> Seq.fold (fun acc it -> option {
        let! (idx, result:int[]) = acc
        let! pixel = it
        result.[idx] <- pixel
        return (idx + 1, result)
    }) (Some (0, result))
    |> Option.map snd

let countLayers dimensions inputLength = option {
    let (width,height) = dimensions
    let countPerLayer = width * height
    let layersCount = inputLength / countPerLayer
    let check = inputLength = countPerLayer * layersCount
    if check then 
        return (countPerLayer, layersCount)
}

let parseImage (dim:Dimensions) (pixels:string) : Image option =
    option {
    
        let! pixels = parseInput pixels

        let (width,height) = dim
    
        let! (perLayer, layersCount) = countLayers dim pixels.Length

        let layers = [|


            let coords = seq {
                for y in [0..height-1] do
                for x in [0..width-1] do
                    yield x,y
            }

            for layerIdx in [0..layersCount-1] do
                yield coords |> Seq.fold (fun layer (x,y) -> 
                    let offset = layerIdx * perLayer + width * y + x
                    layer |> Map.add (x,y) pixels.[offset]
                ) Map.empty
        |]
        
        return {
            Dimension = dim;
            Layers = layers
        }
    }

let solve dim data = option {
    let! image = parseImage dim data

    let layerWithLeastZeros = 
        image.Layers 
        |> Array.minBy (fun layer -> 
            layer 
            |> Map.toSeq 
            |> Seq.filter (fun (k,v) -> v = 0) 
            |> Seq.length
        )

    return
        layerWithLeastZeros 
        |> Map.toSeq
        |> Seq.fold (fun (ones, twos) (k,v) -> 
            if v = 1 then (ones + 1, twos)
            elif v = 2 then (ones, twos + 1)
            else (ones, twos)    
        ) (0,0)

}

let test1 = solve (3,2) "123456789012"
let input = File.ReadAllText(__SOURCE_DIRECTORY__  + "\Day8.txt")
let answer1' = solve (25,6) input |> Option.map (fun (a,b) -> a * b)

let pixelate (image:Image) = 

    let (width, height) = image.Dimension

    let stacked point = 
        [|
            for layer in image.Layers do
                yield layer |> Map.find point
        |]

    let Black = 0
    let White = 1
    let Transparent = 2

    let data = Array2D.init height width (fun y x -> 
        let colors = stacked (x,y)
        let finalColor = 
            colors 
            |> Seq.skipWhile (fun c -> c = Transparent) 
            |> Seq.tryHead 
            |> Option.defaultValue Black

        finalColor
    )

    [|
        for y in 0..height-1 do
            let (shot:string) = 
                [| for x in 0..width-1 do yield data.[y,x] |] 
                |> Array.map ( fun c -> if c = White then '#' else ' ' )
                |> System.String
            yield shot    
    |]
    |> fun lines -> System.String.Join ("\n", lines) 
    |> printfn "%s"

parseImage (25,6) input |> Option.map pixelate