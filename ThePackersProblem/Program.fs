open System
open System.IO

let inputFilePath =  Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
//let inputFilePath = Path.Combine(__SOURCE_DIRECTORY__, "input-fail.txt")
type Command =
    | Open of int
    | Close of int
    | Unknown of string

let lineToCommand (line: string) : Command =
    match line[0] with
    | 'O' -> Open(Int32.Parse(line[1..]))
    | 'C' -> Close(Int32.Parse(line[1..]))
    | other -> Unknown line

let lines = File.ReadLines(inputFilePath)
let commands = lines |> Seq.map lineToCommand

//Modelling the boxes
type BoxState =
    | Opened
    | Closed

type Box =
    { state: BoxState
      size: int
      openedChildren: Box list
      closedChildren: Box list }

let totalBoxSize (boxList: Box list) : int =
    boxList |> List.map (fun box -> box.size) |> List.sum

let isClosable (box: Box) : bool = box.openedChildren.IsEmpty

let sizeRemaining (box: Box) : int =
    box.size - (totalBoxSize box.openedChildren) - (totalBoxSize box.closedChildren)

let checkIfChildrenFit (box: Box) : bool = (sizeRemaining box) >= 0

let closeFirstOpenChild (box: Box) : Box =
    if (box.state = Closed) then
        box
    else
        match box.openedChildren with
        | head :: tail ->
            let newState = if tail.IsEmpty then Closed else Opened

            { box with
                state = newState
                openedChildren = tail
                closedChildren = { head with state = Closed } :: box.closedChildren }
        | other -> box

let boxSummary (box: Box) =
    ((if (box.state = Opened) then '0' else 'C'), box.size, sizeRemaining box)

//Modelling the command processing
type BoxStack = Box list

type ProcessStatus = { stack: BoxStack; maxDepth: int }

let initProcessStatus = { stack = []; maxDepth = 0 }

(* Close the box that is the head of the stack. That means:
  => If the box that is the head is closed, return the stack unchanged.
  => Remove that box that is the head of the stack (no need to actually close it).
  => If the box that is the new head has open children,
        close the first open child (which is the same as the old head)
        and make the second open child (if any) the  "new new" head.
*)

let closedStackHead (stack: BoxStack) : BoxStack =
    match stack with
    | [] -> [] //nothing to close
    | head :: tail ->
        if (head.state = Closed) then
            stack
        else //tail is the new stack, unless it is empty
            match tail with
            | [] ->
                if (head.state = Closed) then
                    stack
                else
                    [ { head with state = Closed } ] //always leave the last box on
                                                    //the stack, so we can see if
                                                    //it was closed
            | head2 :: tail2 -> //close first open child of new top of new stack
                match head2.openedChildren with
                | [] -> tail
                | openHead :: openTail ->
                    match openTail with
                    | [] ->
                        { head2 with
                            openedChildren = []
                            closedChildren =
                                {
                                    openHead with state = Closed
                                } :: head2.closedChildren }
                        :: tail2
                    | openHead2 :: openTail2 ->
                        openHead2
                        :: { head2 with
                               openedChildren = openTail
                               closedChildren =
                                   {
                                       openHead with state = Closed
                                   } :: head2.closedChildren }
                        :: tail2

let processCommand (status: ProcessStatus option) (command: Command) : ProcessStatus option =
    match status with
    | None -> None //command processing has failed
    | Some status ->
        printfn "stack: %A (max %d)" (status.stack |> Seq.map boxSummary) status.maxDepth
        printfn "command: %A" command
        let stack = status.stack
        match command with
        | Unknown line -> None

        | Close closeSize ->
            if (stack.Head.state = Closed)
            then None //Can't close a box that is already closed.
            else
                if (closeSize <> stack.Head.size)
                then None //trying to close the wrong size of a box
                else
                    match stack with
                    | [] -> None
                    | head :: tail ->
                        if (tail.IsEmpty)
                        then
                            let closedHead = {head with state = Closed}
                            Some {status with stack = [closedHead]} //always leave the last element on the stack
                        else
                            let newStack = closedStackHead stack
                            match newStack.Head.openedChildren with
                            | [] -> Some { status with stack = newStack}
                            | openHead :: openTail ->
                                Some { status with stack = openHead :: newStack }

        | Open openSize ->
            let stack = status.stack
            match stack with
            | [] ->
                let box = { state = Opened; size = openSize; openedChildren = []; closedChildren = [] }
                Some {stack = [box]; maxDepth = 1}
            | head :: tail ->
                if (openSize > sizeRemaining head)
                then  None //not enough room in the box for new box
                else
                    let box = { state = Opened; size = openSize; openedChildren = []; closedChildren = [] }
                    let newHead = { head with openedChildren = box :: head.openedChildren }
                    let newStack = box::newHead::tail
                    Some {stack = newStack; maxDepth = List.max [status.maxDepth; newStack.Length] }

let result = commands |> Seq.fold processCommand (Some initProcessStatus)

let finalResult =
    match result with
    | Some status ->
        if ((status.stack.Length = 1) && (status.stack.Head.state = Closed))
        then Some status
        else None
    | None -> None

match finalResult with
| Some status -> printfn "Well done!"
| None -> printfn "You are fired!"

if finalResult.IsSome
then
    printfn "Max depth is %d" finalResult.Value.maxDepth