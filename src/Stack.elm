module Stack exposing (initialise, push, pop, Stack)

{-| This library implements a stack data structure in Elm, allowing you to worry more about your business logic and less about implementing common adts.

# Definition
@docs Stack

# Common Helpers
@docs initialise, pop, push

-}

import Array exposing (..)


{-|
        The Stack type uses an array to represent the stack.
-}
type Stack a
    = Stack
        { data : Array a
        , top : Int
        , default : a
        }


defaultFromStack : Stack (Maybe a) -> Maybe a
defaultFromStack (Stack { default }) =
    default


{-|
        Initialises a stack given an initialisation value and a stack size.

-}
initialise : a -> Int -> Stack a
initialise t size =
    Stack
        { data = Array.initialize size (always t)
        , top = 0
        , default = t
        }


{-|
        Pushes an item onto the stack and returns the new stack. The item must be of the same type as the stack.
-}
push : a -> Stack (Maybe a) -> Stack (Maybe a)
push item ((Stack { top, data }) as stack) =
    if Array.length data /= top then
        Stack { data = Array.set top (Just item) data, top = top + 1, default = defaultFromStack stack }
    else
        stack


{-|
        Removes the item at the top of the stack and returns it as the first item of a tuple.
-}
pop : Stack (Maybe a) -> ( Maybe a, Stack (Maybe a) )
pop ((Stack { data, top }) as stack) =
    let
        item =
            Array.get (top - 1) data
    in
        case item of
            Nothing ->
                ( Nothing, stack )

            Just item ->
                ( item, Stack { data = Array.set (top - 1) Nothing data, top = (top - 1), default = defaultFromStack stack } )
