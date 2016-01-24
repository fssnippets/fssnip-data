(*

We consider the movement of cells towards an area of ligand activity (chemotaxis) and the cells’ 
reactions to different ligands.  

For simplicity, we will consider only horizontal movement on an integer grid in the first two 
parts of this exercise. That is, the locations of the cells can only take integer values and 
the movement will only change the x-part of the cell location. 

The module Cells defines the object Cell and functions that can be applied to it in order to 
define the behaviour of the model.  
 
What you should observe are cells (circles) moving towards two different kinds of ligands, 
called Ras and Notch (indicated as areas of different colour). The cells start in areas 
with no ligand present and move towards the closest ligand or in a random direction if 
they are in the middle between two ligand areas. Once the cells have reached the area of 
ligand activity, they react to the specific ligand. Ras causes the cells to stop their 
movement and grow and Notch results in the cells to stop moving. 

*)
/// Defines the properties of cells and functions describing their behaviour
module Cells = 
    
    /// Creates a random number generator
    let randomGenerator = System.Random()
    
    /// Represents a point (x,y) in the model with coordinates on an integer grid
    type Point = {x:int;y:int}
   
    /// Build a point with coordinates x and y
    let Point(x,y) = {x=x;y=y}
    
    
    /// Represents ligand activity
    type Ligand = 
        /// Represents the presence of Notch protein
        | Notch 
        /// Represents the presence of Ras protein
        | Ras 
        /// Represents the absence of ligand activity
        | NoLigand
    
    ///Represents the state of a cell that defines how it will behave
    type CellMarker = 
        /// Represents a cell that is able to move
        | Move 
        /// Represents a cell that is able to grow
        | Grow 
        /// Represents a cell that stays in the place that it is in and does not do anything
        | Stopped 
        /// Represents a cell that has died
        | Dead 
        /// Represents a cell that is set to divide
        | Divide
    
    
    /// Defines the states and state modifications of a cell
    type Cell(x,y) =
        // Define the initial states of the cell. The keyword 'mutable' is used to indicate
        // that the state can change over time.
        let mutable location  = Point(x,y)
        let mutable size      = 0.5
        let mutable marker   = Move
        let mutable direction = 0
        
        // Expose the states of the cell to make them accessible from outside of the object
        member this.Location   with get() = location  and set(x) = location <- x
        member this.Marker     with get() = marker    and set(x) = marker <- x 
        member this.Direction  with get() = direction and set(x) = direction <- x
        member this.Size       with get() = size      and set(x) = size <- x
    
        // Modify states of the cell using functions that can be called outside of the object
        member this.Move()      = location <- Point(location.x + direction,location.y)
        member this.Grow(x)     = size <- size + x
       
        member this.RenderData() =         
            let color = 
                match marker with
                | Move -> "blue" 
                | Grow -> "magenta"
                | Dead -> "black"
                | Divide -> "red"
                | _ -> "green"     
            (location.x, location.y, size, color)  
    
    /// Computes the ligand activity in location p.
    let ligand (p:Point) =
        let x = p.x
        if x < 10 then Ras 
        elif x > 20 && x < 30 then Notch 
        elif x > 40 && x < 50 then Ras 
        elif x > 60 && x < 70 then Notch 
        elif x > 80 && x < 90 then Ras 
        else NoLigand

        
            
      
    /// This function is called by the simulation engine. It sets the marker of a cell according
    /// to the ligand activity at its location and in part B depending its neighbour and the ligand 
    /// activity at its next location.
    let setMarker (cells:Cell[]) (cell:Cell) =
        // These three definitions are needed in part B
        let nextX = cell.Location.x + cell.Direction
        let nextLoc = Point(nextX,cell.Location.y)
        let neighbour = Array.filter (fun (c:Cell) -> c.Location = nextLoc) cells 
        if ligand cell.Location = Ras then
            cell.Marker <- Grow
        elif ligand cell.Location = Notch then
            cell.Marker <- Stopped
    
    /// This function is called by the simulation engine. It performs the actions 
    /// described by the markers decided by the 'setMarkers' function in an earlier phase of
    /// the simulation on a cell.
    let react (step:float) (cell:Cell) =
        if cell.Marker = Move then
            cell.Move()
        elif cell.Marker = Grow  then
                cell.Grow(step)
    
    
    /// This function is called by the simulation engine. It defines the direction of a cell 
    /// according to its location relative to the ligands.
    let cellDirection (cell:Cell) =
        let x = cell.Location.x
        let y = cell.Location.y
        let right = Point(x+5,y)
        let left = Point(x-5,y)
        if ligand cell.Location = NoLigand && ligand right = NoLigand && ligand left = NoLigand then
        // If the cell is located with equal distance to the ligand on the left and the ligand on the right, the direction is chosen at random.
            let random = randomGenerator.Next(0,3)
            cell.Direction <- random - 1
        else if cell.Direction = 0 then
        // Else if the cell's direction hasn't been set yet, it will move towards the closest ligand.
            if ligand right = Notch || ligand right = Ras then
                cell.Direction <- 1
            else if ligand left = Notch || ligand left = Ras then
                cell.Direction <- -1
    
    /// This function is called by the simulation engine. It lets a cell divide if the location
    /// to its left or its right is free, it places the new daughter cell in the free space
    /// and it resets the division timer of a cell that has just divided. If the neighbouring 
    /// locations aren't free, the function does nothing so that the cell can try to divide 
    /// again at the next simulation step. The function returns the new cell.
    let cellDivision (cells:Cell[]) (cell:Cell) = [| |]
    
    /// This function is called by the simulation engine. It lets a cell die if would
    /// move from an area without ligand to an area with Ras activity, but the location 
    /// is occupied.
    let cellDeath (cell:Cell) = () 

// Now create an HTML5 Canvas element to visualize the scene in the HTML graphics pane
let width, height = 1000, 1000
TryFSharp.Canvas.Show()
TryFSharp.Canvas.RunJavaScript (sprintf @"
(function (self) {
    var canv = document.getElementById('canvas1');
    if (canv == undefined) {
        canv = document.createElement('canvas');
        if (canv.getContext) {
            canv.id = 'canvas1';
            canv.width = %A;
            canv.height = %A;
            document.body.appendChild(canv);
        } else {
            alert('Your browser does not seem to support HTML5 Canvas.');
        };
    };
    var ctx = canv.getContext('2d');
    ctx.scale(2.0,2.0); 
           
    self.render_wall = function() {
            var start,stop,range,color;
            ctx.clearRect(0,0,%A,%A); 
            start=0; stop=10; range=stop-start; color='pink';
            ctx.fillStyle = color; ctx.fillRect(start,0,range,50);
            start=20; stop=30; range=stop-start; color='brown';
            ctx.fillStyle = color; ctx.fillRect(start,0,range,50);
            start=40; stop=50; range=stop-start; color='pink';
            ctx.fillStyle = color; ctx.fillRect(start,0,range,50);
            start=60; stop=70; range=stop-start; color='brown';
            ctx.fillStyle = color; ctx.fillRect(start,0,range,50);
            start=80; stop=90; range=stop-start; color='pink';  
            ctx.fillStyle = color; ctx.fillRect(start,0,range,50);
            ctx.fill();             
        };
        
    self.render_cell = function(pos_x,pos_y,size,color) {
        ctx.beginPath();
        ctx.arc(pos_x,pos_y,size,0,2*Math.PI);
        ctx.fillStyle=color;
        ctx.fill();
        ctx.strokeStyle=color;
        ctx.stroke(); 
    };
    return self;
}(window.CellSim = window.CellSim || {}));
" width height width height)

// Export JS to F#
let r_cell = TryFSharp.Canvas.JavaScriptFunction("CellSim.render_cell")
let r_wall = TryFSharp.Canvas.JavaScriptFunction("CellSim.render_wall")


// Simulation step function. 
let timeDelta = 0.5

let step (cells : Cells.Cell[]) =     
    for cell in cells do
        Cells.cellDirection cell
    for cell in cells do
        Cells.setMarker cells cell
    for cell in cells do
        Cells.react timeDelta cell
    cells         
        
let step_and_draw cells =          
        // Run a simulation step.
        let cells' = step cells
        // Render just the wall.
        r_wall.Invoke() |> ignore
        // Render each cell on the wall.
        Array.iter
            (fun (c:Cells.Cell) -> 
                    let (x,y,size,clr) = c.RenderData()                
                    r_cell.Invoke(x,y,size,clr) |> ignore)
            cells' 
        cells' 

let rec run_steps num_of_steps step state =
    if num_of_steps = 0 then state
    else
        let state' = step state
        run_steps (num_of_steps - 1) step state'

// Initial simulation state, 
let mk_initial_cells _ = 
    let start_cells (x:int) = [| for y in 0 .. 9 -> Cells.Cell(x,2+4*y) |]
    [| yield! start_cells 15
       yield! start_cells 35
       yield! start_cells 55
       yield! start_cells 75 |]
       

/// Run the simulation 
// Make the cells 
let c0 = mk_initial_cells ()
// Render the activity wall. 
r_wall.Invoke() |> ignore
// Run 7 steps         
run_steps 7 step_and_draw c0         
    
    