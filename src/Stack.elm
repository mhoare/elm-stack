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
type alias Stack a =
    { data : Array a
    , top : Int
    , default : a
    }


{-|
        Initialises a stack given an initialisation value and a stack size.

-}
initialise : a -> Int -> Stack a
initialise t size =
    { data = Array.initialize size (always t)
    , top = 0
    , default = t
    }


{-|
        Pushes an item onto the stack and returns the new stack. The item must be of the same type as the stack.
-}
push : a -> Stack (Maybe a) -> Stack (Maybe a)
push item stack =
    if Array.length stack.data /= stack.top then
        { stack | data = Array.set stack.top (Just item) stack.data, top = stack.top + 1 }
    else
        stack


{-|
        Removes the item at the top of the stack and returns it as the first item of a tuple.
-}
pop : Stack (Maybe a) -> ( Maybe a, Stack (Maybe a) )
pop stack =
    let
        item =
            Array.get (stack.top - 1) stack.data
    in
        case item of
            Nothing ->
                ( Nothing, stack )

            Just item ->
                ( item, { stack | data = Array.set (stack.top - 1) Nothing stack.data, top = (stack.top - 1) } )
